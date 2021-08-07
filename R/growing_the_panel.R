# Introduction ------------------------------------------------------------

# In this script I will organize our current accessions and divide them 
# into multiple waves for planting over the course of the experiment.

library(tidyverse)
library(googlesheets4)


# Importing the data and fixing rows --------------------------------------

# This table was created in the script 
# building_diversity_panel_and_germplasm_resources.R. It contains metadata 
# from Alonge et al. 2020 Cell (long read sequenced lines) and Razifard et 
# al. 2020 Molecular Biology and Evolution (the Varitome).

accessions <- read.table(file = file.path(getwd(), 
                                          "output_tables", 
                                          "accessions.txt"),
                         header = TRUE,
                         sep = '\t')

# Removing an accession that wasn't sent (note: have to check
# for NAs in the expression or it returns NAs too)
accessions <- accessions[is.na(accessions$TGRC) | accessions$TGRC != "LA4791", ]
accessions <- accessions[accessions$accession != "Voyage", ]

# Adding a column for downstream species organization (the different sources 
# have species in different formats, I need a consistent one for naming)
accessions$short_species <- NA

# Adding accessions that weren't in the original table, mostly these
# are the PGRP GTTR lines
to_add <- data.frame(accession = c("LA0490",
                                   "LA1994",
                                   "LA2375",
                                   "LA2661",
                                   "LA2662",
                                   "LA3120",
                                   "LA3320",
                                   "LA4345",
                                   "LA4355",
                                   "LA4715",
                                   "LA4790",
                                   "LA4792",
                                   "LA4793"),
  Assigned_population = c(NA),
  Country = c(NA),
  Province = c(NA),
  Collecting_locality = c(NA),
  Local_name = c("VF-36",
                 "Tamaulipas",
                 "San Marzano",
                 "Nagcarlang",
                 "Saladette",
                 "Malintka",
                 "Hotset",
                 "Heinz",
                 "Gold Nugget",
                 "unknown",
                 "unknown",
                 "Tegucigalpa",
                 "Voyage"),
  Notes_from_original_collections = c(NA),
  Latitude = c(NA),
  Longitude = c(NA),
  part_of_varitome = c(FALSE),
  Species = c(NA),
  Taxon = c(NA),
  TGRC = c("LA0490",
           "LA1994",
           "LA2375",
           "LA2661",
           "LA2662",
           "LA3120",
           "LA3320",
           "LA4345",
           "LA4355",
           "LA4715",
           "LA4790",
           "LA4792",
           "LA4793"),
  CC = c(NA),
  EA = c(NA),
  PI = c(NA),
  TS = c(NA),
  SV.Count = c(NA),
  SV.Collector.Rank = c(NA),
  SRA.ID = c(NA),
  Yield..bp. = c(NA),
  part_of_alonge = c(NA),
  available_usda = c(NA),
  available_tgrc = c(NA),
  have_accession = c(NA),
  have_from_matthias = c(NA),
  have_from_tgrc = c(NA),
  varitome_ordered_from_tgrc = c(NA),
  short_species = c("lycopersicum",
                    "lycopersicum",
                    "lycopersicum",
                    "lycopersicum",
                    "lycopersicum",
                    "lycopersicum",
                    "lycopersicum",
                    "lycopersicum",
                    "lycopersicum",
                    "pimpinellifolium",
                    "lycopersicum",
                    "lycopersicum",
                    "lycopersicum"))

accessions <- rbind(accessions, to_add)

# This is the manual record of seed packets that we have.
gs4_deauth()
packet_list <- read_sheet("1RMDwiJy14HxPH7kY3pwske3iXLqL8N3WLHIl2o0WQW8")
# length(unique(packet_list$Accession_ID))


# Investigating duplicated packets ----------------------------------------

# I need to combine the packet list with the metadata and figure out which
# accessions are present, as well as make sure there aren't any typos, etc.

matching_accessions_rows <- accessions[accessions$accession %in% packet_list$Accession_ID |
  accessions$TGRC %in% packet_list$Accession_ID |
  accessions$PI %in% packet_list$Accession_ID |
  accessions$CC %in% packet_list$Accession_ID |
  accessions$TS %in% packet_list$Accession_ID |
  accessions$EA %in% packet_list$Accession_ID |
  accessions$Local_name %in% packet_list$Accession_ID, ]

# unique(packet_list$Species)

# Checking for typos
unmatched_accessions_rows <- accessions[! accessions$accession %in% matching_accessions_rows$accession, ]

# Looking at seed packets we have that don't match records in the metadata
unmatched_packets <- packet_list[! packet_list$Accession_ID %in% accessions$accession &
                                 ! packet_list$Accession_ID %in% accessions$TGRC &
                                 ! packet_list$Accession_ID %in% accessions$PI &
                                 ! packet_list$Accession_ID %in% accessions$CC &
                                 ! packet_list$Accession_ID %in% accessions$TS &
                                 ! packet_list$Accession_ID %in% accessions$EA &
                                 ! packet_list$Accession_ID %in% accessions$Local_name, ]

# There are more matched entries in the packet list than there are rows in the
# accessions list, so there must be packets that are duplicated, for example 
# one packet with a TGRC id and one packet with a common name for the same 
# accession.
matched_packets <- packet_list[packet_list$Accession_ID %in% accessions$accession |
                               packet_list$Accession_ID %in% accessions$TGRC |
                               packet_list$Accession_ID %in% accessions$PI |
                               packet_list$Accession_ID %in% accessions$CC |
                               packet_list$Accession_ID %in% accessions$TS |
                               packet_list$Accession_ID %in% accessions$EA |
                               packet_list$Accession_ID %in% accessions$Local_name, ]

# Making a dataframe that shows which id's the packet_list entries match
packet_id_matches <- data.frame(packet_list_id = packet_list$Accession_ID,
  accession = packet_list$Accession_ID %in% accessions$accession,
  TGRC = packet_list$Accession_ID %in% accessions$TGRC,
  PI = packet_list$Accession_ID %in% accessions$PI,
  CC = packet_list$Accession_ID %in% accessions$CC,
  TS = packet_list$Accession_ID %in% accessions$TS,
  EA = packet_list$Accession_ID %in% accessions$EA,
  Local_name = packet_list$Accession_ID %in% accessions$Local_name)

# Trying to count the TRUEs to narrow it down a little bit. This doesn't
# really help though, because a packet only needs one match in one column 
# for it to break.
packet_id_matches <- packet_id_matches %>% 
  rowwise() %>% 
  mutate(SUM_RQ = sum(c_across(2:8)))

# I should really be counting how many packet_list rows each accession row 
# matches. This method counts how many rows of packet_list each row of 
# accessions matches. I know appending vectors is slow, but it's ok here.
match_sum_df <- data.frame()
for (n in seq(1, nrow(accessions))){
  loop_row <- accessions[n, ]
  match_sum <- nrow(packet_list[packet_list$Accession_ID %in% loop_row$accession |
                    packet_list$Accession_ID %in% loop_row$TGRC |
                    packet_list$Accession_ID %in% loop_row$PI |
                    packet_list$Accession_ID %in% loop_row$CC |
                    packet_list$Accession_ID %in% loop_row$TS |
                    packet_list$Accession_ID %in% loop_row$EA |
                    packet_list$Accession_ID %in% loop_row$Local_name, ])
  append_row <- data.frame(accessions = loop_row$accession, 
                           TGRC = loop_row$TGRC,
                           match_sum = match_sum)
  match_sum_df <- rbind(match_sum_df, append_row)
}

# Showing the ones with duplications
multiple_packets <- match_sum_df[match_sum_df$match_sum > 1, ]

# Marking the duplicated seed packets
packet_list$is_duplicated <- FALSE
packet_list$is_duplicated[packet_list$Accession_ID %in% multiple_packets$accessions |
                          packet_list$Accession_ID %in% multiple_packets$TGRC] <- TRUE

sum(packet_list$is_duplicated) # 21 duplicated packets (42 rows that are dups)


# Adding new identifiers for this experiment ------------------------------

# Having 5 different sets of identifiers is not practical for the experiment.
# I will add new IDs using this convention:
# CW0000 – CW0999: S. lycopersicum
# CW1000 – CW1999: S. pimpinellifolium
# CW2000 – CW2999: S. cheesmaniae
# CW3000 – CW3999: S. galapagense
# 
# Beyond the species ranges, the order of the numbering is arbitrary. 

# Starting with all the accessions that have matching packets, then adding 
# species to the ones that are missing it in the short_species column. 
accessions <- matching_accessions_rows

accessions$short_species[grepl("SP", accessions$Assigned_population)] <- "pimpinellifolium"
accessions$short_species[grepl("SLL", accessions$Assigned_population) | 
                         grepl("SLC", accessions$Assigned_population)] <- "lycopersicum"
accessions$short_species[grepl("SP", accessions$Species)] <- "pimpinellifolium"
accessions$short_species[grepl("SLL", accessions$Species)] <- "lycopersicum"
accessions$short_species[grepl("SLC", accessions$Species)] <- "lycopersicum"
accessions$short_species[grepl("GAL", accessions$Species)] <- "galapagense"
accessions$short_species[grepl("CHE", accessions$Species)] <- "cheesmaniae"
accessions$short_species[accessions$TGRC == "LA4768"] <- "lycopersicum" # species from TGRC
accessions$short_species[accessions$accession == "BGV006148"] <- "pimpinellifolium" # species from TGRC, intermediate
accessions$short_species[accessions$accession == "LA0767"] <- "lycopersicum" # species from TGRC

# Put this packet aside: CATIE-11106-1
accessions <- accessions[accessions$accession != "CATIE-11106-1", ]

problems <- accessions[is.na(accessions$short_species), ]


# Checking for duplicates in the accessions table -------------------------

# I'm still not 100% confident that there aren't duplicated rows in the 
# accessions data frame.

# Making a simplified version of the table that should only include the 
# essential data for seeing if there are duplicates.
simple_accessions <- accessions[ , c(1, 13)]
simple_accessions$TGRC[is.na(simple_accessions$TGRC)] <- "no_value"

# There don't seem to be any problems with the TGRC identifiers at least
match_sum_df <- data.frame()
for (n in seq(1, nrow(simple_accessions))){
  loop_row <- simple_accessions[n, ]
  match_sum <- nrow(simple_accessions[simple_accessions$accession %in% loop_row$TGRC, ])
  append_row <- data.frame(accessions = loop_row$accession, 
                           TGRC = loop_row$TGRC,
                           match_sum = match_sum)
  match_sum_df <- rbind(match_sum_df, append_row)
}
sum(match_sum_df$match_sum)

# Writing out the table to do some manual checks
write.table(accessions, 
            file = file.path(getwd(), 
                             "output_tables", 
                             "accessions_names.txt"),
            row.names = FALSE,
            sep = '\t')

# Getting the relative frequencies
accessions %>% 
  group_by(short_species) %>% 
  tally() %>% 
  mutate(freq = n / sum(n),
         wave_porportion = freq * 25)

# 19 lyc, 4 pim, 1 che, 1 gal

# Grabbing some lines for the first round
sampled_accessions <- accessions %>% 
  filter(short_species == "pimpinellifolium") %>%
  sample_n(5)
            
sampled_accessions <- accessions %>% 
  filter(short_species == "galapagense")
            
            
            