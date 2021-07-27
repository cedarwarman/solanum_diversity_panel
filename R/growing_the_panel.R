# Introduction ------------------------------------------------------------

# In this script I will organize our current accessions and divide them 
# into multiple waves for planting over the course of the experiment.

library(tidyverse)
library(googlesheets4)


# Importing the data ------------------------------------------------------

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


# This is the manual record of seed packets that we have.
gs4_deauth()
packet_list <- read_sheet("1RMDwiJy14HxPH7kY3pwske3iXLqL8N3WLHIl2o0WQW8")
# length(unique(packet_list$Accession_ID))


# Combining the packet list with the metadata -----------------------------
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
match_sum_df <- data.frame(accessions = c(), match_sum = c())
for (n in seq(1, nrow(accessions))){
  loop_row <- accessions[n, ]
  match_sum <- nrow(packet_list[packet_list$Accession_ID %in% loop_row$accession |
                    packet_list$Accession_ID %in% loop_row$TGRC |
                    packet_list$Accession_ID %in% loop_row$PI |
                    packet_list$Accession_ID %in% loop_row$CC |
                    packet_list$Accession_ID %in% loop_row$TS |
                    packet_list$Accession_ID %in% loop_row$EA |
                    packet_list$Accession_ID %in% loop_row$Local_name, ])
  append_row <- data.frame(accessions = loop_row$accession, match_sum = match_sum)
  match_sum_df <- rbind(match_sum_df, append_row)
}

# Showing the ones with duplications
multiple_packets <- match_sum_df[match_sum_df$match_sum > 1, ]







