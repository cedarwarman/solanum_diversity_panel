# Introduction ------------------------------------------------------------

# I feel like I got lost in the weeds a little bit with the other metadata 
# scripts, so I'll make a clean one here. My goal is to combine primary 
# spreadsheets from the literature and the TGRC and make a definitive list of 
# all the accessions I have along with their associated metadata. I'll then 
# split them into waves and give them all CW numbers.

library(tidyverse)
library(readxl)
library(googlesheets4)
library(fuzzyjoin)

# Reproducibility
set.seed(6)


# Loading the data --------------------------------------------------------

# Razifard et al. 2020 Molecular Biology and Evolution (Varitome)
# This is just the line they sequenced (166), not the total number of lines
# that they used for their phylogeny (295).
razifard <- read_excel(file.path(getwd(), "data", "Razifard_2020_table_s1.xlsx"),
                       skip = 1)

# Fixes some of the colnames, still a bunch of slashes and parentheses so 
# quotes will be required for those
colnames(razifard) <- gsub("\\s","_",colnames(razifard))
colnames(razifard)[1] <- "accession"

# Keeping track of which sets accessions belong to
razifard$part_of_razifard <- TRUE

# Alonge et al. 2020 Cell
alonge <- read_excel(file.path(getwd(), "data", "Alonge_2020_table_s1.xlsx"),
                     sheet = "S1B",
                     skip = 2)
colnames(alonge)[1] <- "accession"

# Keeping track of which sets accessions belong to
alonge$part_of_alonge <- TRUE

# TGRC Varitome data (from Roger Chetelat)
tgrc_varitome <- read_excel(file.path(getwd(), "data", "TGRC_Varitome.xlsx"))

# Packet list
# Load packet list from growing_the_panel

# Organizing Alonge and Razifard ------------------------------------------
# I'll take a different strategy this time, working off the packet list to 
# only include rows from razifard, alonge, and the tgrc_varitome that are 
# present in the packet list.

# OR really combine alonge and razifard properly, then keep rows that match 
# the accessions, while at the same time adding the packet iD (maybe two columns 
# for dups).

# OR make Alonge, Razifard, and tgrc_varitome have the same columns for all 
# the different IDs, separated. That might make the series of joins a lot 
# neater and easier to track.

# Trying the last version, making common columns for all the names. Columns:
# 
# name_TGRC
# name_BGV
# name_CC
# name_EA
# name_PI
# name_TS
# name_FLA
# name_LYC
# name_TR
# name_common

# Starting with alonge and seeing what comes out. It already has TGRC, CC, 
# EA, PI, TS. 
alonge$name_BGV <- NA
alonge$name_FLA <- NA
alonge$name_LYC <- NA
alonge$name_TR <- NA
alonge$name_other <- NA
names(alonge)[1] <- "name_original_alonge"
names(alonge)[4] <- "name_TGRC"
names(alonge)[5] <- "name_CC"
names(alonge)[6] <- "name_EA"
names(alonge)[7] <- "name_PI"
names(alonge)[8] <- "name_TS"
alonge <- alonge[ , c(4:8, 14:18, 1:3, 9:13)]

# Moving names from "accessions" to the right columns
for (n in seq(1, nrow(alonge))) {
  loop_accession <- alonge$name_original_alonge[n]
  if (str_detect(loop_accession, "^LA")) {
    alonge$name_TGRC[n] <- loop_accession
    print(paste("added LA: ", loop_accession))
  } else if (str_detect(loop_accession, "^CC")){
    alonge$name_CC[n] <- loop_accession
    print(paste("added CC: ", loop_accession))
  } else if (str_detect(loop_accession, "^EA")){
    alonge$name_EA[n] <- loop_accession
    print(paste("added EA: ", loop_accession))
  } else if (str_detect(loop_accession, "^PI")){
    alonge$name_PI[n] <- loop_accession
    print(paste("added PI: ", loop_accession))
  } else if (str_detect(loop_accession, "^TS")){
    alonge$name_TS[n] <- loop_accession
    print(paste("added TS: ", loop_accession))
  } else if (str_detect(loop_accession, "^BGV")){
    alonge$name_BGV[n] <- loop_accession
    print(paste("added BGV: ", as.character(loop_accession)))
  } else if (str_detect(loop_accession, "^Fla")){
    alonge$name_FLA[n] <- loop_accession
    print(paste("added Fla: ", loop_accession))
  } else if (str_detect(loop_accession, "^LYC")){
    alonge$name_LYC[n] <- loop_accession
    print(paste("added LYC: ", loop_accession))
  } else if (str_detect(loop_accession, "^TR")){
    alonge$name_TR[n] <- loop_accession
    print(paste("added TR: ", loop_accession))
  } else {
    alonge$name_other[n] <- loop_accession
    print(paste("added other: ", loop_accession))
  }
}

# Doing the same thing for Razifard
names(razifard)[1] <- "name_original_razifard"
razifard$name_TGRC <- NA
razifard$name_CC <- NA
razifard$name_EA <- NA
razifard$name_PI <- NA
razifard$name_TS <- NA
razifard$name_BGV <- NA
razifard$name_FLA <- NA
razifard$name_LYC <- NA
razifard$name_TR <- NA
razifard$name_other <- NA
razifard <- razifard[ , c(24:33, 1:23)]

for (n in seq(1, nrow(razifard))) {
  loop_accession <- razifard$name_original_razifard[n]
  if (str_detect(loop_accession, "^LA")) {
    razifard$name_TGRC[n] <- loop_accession
    print(paste("added LA: ", loop_accession))
  } else if (str_detect(loop_accession, "^CC")){
    razifard$name_CC[n] <- loop_accession
    print(paste("added CC: ", loop_accession))
  } else if (str_detect(loop_accession, "^EA")){
    razifard$name_EA[n] <- loop_accession
    print(paste("added EA: ", loop_accession))
  } else if (str_detect(loop_accession, "^PI")){
    razifard$name_PI[n] <- loop_accession
    print(paste("added PI: ", loop_accession))
  } else if (str_detect(loop_accession, "^TS")){
    razifard$name_TS[n] <- loop_accession
    print(paste("added TS: ", loop_accession))
  } else if (str_detect(loop_accession, "^BGV")){
    razifard$name_BGV[n] <- loop_accession
    print(paste("added BGV: ", as.character(loop_accession)))
  } else if (str_detect(loop_accession, "^Fla")){
    razifard$name_FLA[n] <- loop_accession
    print(paste("added Fla: ", loop_accession))
  } else if (str_detect(loop_accession, "^LYC")){
    razifard$name_LYC[n] <- loop_accession
    print(paste("added LYC: ", loop_accession))
  } else if (str_detect(loop_accession, "^TR")){
    razifard$name_TR[n] <- loop_accession
    print(paste("added TR: ", loop_accession))
  } else {
    razifard$name_other[n] <- loop_accession
    print(paste("added other: ", loop_accession))
  }
}


# Adding in the TGRC Varitome data ----------------------------------------
# This is all well and good, but adding the other names in from the 
# tgrc_varitome df to Razifard might reveal some of the dups.
tgrc_varitome_key <- data.frame(name_TGRC = tgrc_varitome$AccessionNum,
                                name_BGV = tgrc_varitome$OtherID)

# To deal with later: CATIE-11106/1, PAS014479
tgrc_varitome_key_extras <- tgrc_varitome_key[c(135,136), ]

# Cleaning up the key for the BGV names
tgrc_varitome_key <- tgrc_varitome_key[complete.cases(tgrc_varitome_key), ]
tgrc_varitome_key <- tgrc_varitome_key[str_detect(tgrc_varitome_key$name_BGV, "^BGV"), ]

# Adding the BGV names
razifard <- razifard %>% 
  left_join( tgrc_varitome_key, by = c('name_BGV')) %>%
  mutate(name_TGRC = coalesce(name_TGRC.x, name_TGRC.y), .keep = 'unused') %>%
  relocate(name_TGRC)

# Manually adding the other four names
razifard$name_TGRC[razifard$name_other == "CATIE-11106-1"] <- "LA4790"
razifard$name_TGRC[razifard$name_other == "PAS014479"] <- "LA4791"
razifard$name_TGRC[razifard$name_other == "Tegucigalpa"] <- "LA4792"
razifard$name_TGRC[razifard$name_other == "Voyage"] <- "LA4793"


# Joining -----------------------------------------------------------------
# I think a series of left joins with each name column (removing all the other ones) is the solution 
# Starting with Alonge because Razifard has the fewest names to add.

# Only add TGRC, PI, BGV, and other
# unique(razifard$name_TGRC)
# unique(razifard$name_CC) #skip
# unique(razifard$name_EA) #skip
# unique(razifard$name_PI)
# unique(razifard$name_TS) #skip
# unique(razifard$name_BGV)
# unique(razifard$name_FLA) #skip
# unique(razifard$name_LYC) #skip
# unique(razifard$name_TR) #skip
# unique(razifard$name_other)

# Making sub dataframes with only 1 names column
razifard_name_tgrc <- razifard[ , c(-(2:10))]
razifard_name_pi <- razifard[ , c(-(1:3), -(5:10))]
razifard_name_bgv <- razifard[ , c(-(1:5), -(7:10))]
razifard_name_other <- razifard[ , c(-(1:9))]

# Doing the sequential joins
# joined_tgrc <- inner_join(alonge, razifard_name_tgrc, by = "name_TGRC", na_matches = "never") # none
joined_pi <- inner_join(alonge, razifard_name_pi, by = "name_PI", na_matches = "never")
joined_bgv <- inner_join(alonge, razifard_name_bgv, by = "name_BGV", na_matches = "never")
joined_other <- inner_join(alonge, razifard_name_other, by = "name_other", na_matches = "never")

# Fixing the names that got dropped from the joined df that were present in 
# Razifard (just doing the bgv's, hopefully this isn't a problem elsewhere)
razifard_bgv_la_key <- razifard[razifard$name_BGV %in% c("BGV006175", "BGV006232", "BGV006336", "BGV006370",
                                                         "BGV006454", "BGV006767", "BGV006768", "BGV006775",
                                                         "BGV006852", "BGV006865", "BGV006906", "BGV007109",
                                                         "BGV007152", "BGV007198", "BGV007931", "BGV007981",
                                                         "BGV007989", "BGV007992", "BGV008042", "BGV008108",
                                                         "BGV008189", "BGV008225", "BGV012615", "BGV012626",
                                                         "BGV013161"), ]
razifard_bgv_la_key <- razifard_bgv_la_key[ , c(1, 6)]
# razifard_bgv_la_key <- razifard_bgv_la_key[complete.cases(razifard_bgv_la_key), ]
joined_bgv <- joined_bgv[ , c(2:41)]
joined_bgv <- inner_join(joined_bgv, razifard_bgv_la_key)
joined_bgv <- joined_bgv %>% 
  relocate(c(name_TGRC))

# Testing just joining by accession (they match)
# test_join <- inner_join(alonge, razifard, by = c("name_original_alonge" = "name_original_razifard"))

# Adding the rows that are not shared between the two data frames
joined <- rbind(joined_pi, joined_bgv, joined_other)
bind_df <- razifard[! razifard$name_original_razifard %in% joined$name_original_razifard, ]
joined <- bind_rows(joined, bind_df)

bind_df <- alonge[! alonge$name_original_alonge %in% joined$name_original_alonge, ]
joined <- bind_rows(joined, bind_df)

# Cleaning it up (before adding tgrc_varitome: 236)
joined <- joined[ , c(1:11, 19, 41, 18, 12:17, 20:40)]
joined$part_of_alonge[is.na(joined$part_of_alonge)] <- FALSE
joined$part_of_razifard[is.na(joined$part_of_razifard)] <- FALSE

# Still 236 after adding the TGRC names from tgrc_varitome
# Checking for duplicates
# length(joined$name_TGRC[! is.na(joined$name_TGRC)])
# length(unique(joined$name_TGRC[! is.na(joined$name_TGRC)]))
# length(joined$name_BGV[! is.na(joined$name_BGV)])
# length(unique(joined$name_BGV[! is.na(joined$name_BGV)]))
# length(joined$name_other[! is.na(joined$name_other)])
# length(unique(joined$name_other[! is.na(joined$name_other)]))
# No dups in name_TGRC, name_BGV, name_other

# Final Alonge / Razifard dataframe with (hopefully) no dups, all metadata
accessions <- joined


# Manually adding non-long-read/Varitome accessions -----------------------
# These are accessions that I ordered outside of the main set, mostly things
# that are from the larger PGRP thermotolerance project.
to_add <- data.frame("name_TGRC" = c("LA0490",
                                   "LA1994",
                                   "LA2375",
                                   "LA2661",
                                   "LA2662",
                                   "LA3120",
                                   "LA3320",
                                   "LA4345",
                                   "LA4355",
                                   "LA4715"),
                     "name_CC" = c(NA),
                     "name_EA" = c(NA),
                     "name_PI" = c(NA),
                     "name_TS" = c(NA),
                     "name_BGV" = c(NA),
                     "name_FLA" = c(NA),
                     "name_LYC" = c(NA),
                     "name_TR" = c(NA),
                     "name_other" = c("VF-36",
                                      "Tamaulipas",
                                      "San Marzano",
                                      "Nagcarlang",
                                      "Saladette",
                                      "Malintka",
                                      "Hotset",
                                      "Heinz",
                                      "Gold Nugget",
                                      "unknown"),
                     "name_original_alonge" = c(NA),
                     "name_original_razifard" = c(NA),
                     "part_of_razifard" = c(FALSE),
                     "part_of_alonge" = c(FALSE),
                     "Species" = c("SLL",
                                   "SLL",
                                   "SLL",
                                   "SLL",
                                   "SLL",
                                   "SLL",
                                   "SLL",
                                   "SLL",
                                   "SLL",
                                   "SP"),
                     "Taxon" = c(NA),
                     "SV Count" = c(NA),
                     "SV Collector Rank" = c(NA),
                     "SRA ID" = c(NA),
                     "Yield (bp)" = c(NA),
                     "Assigned_population" = c(NA),
                     "Country" = c(NA),
                     "Province" = c(NA),
                     "Collecting_locality" = c(NA),
                     "Local_name" = c(NA),
                     "Notes_from_original_collections" = c(NA),
                     "Latitude" = c(NA),
                     "Longitude" = c(NA),
                     "fresh_weight_(g)" = c(NA),
                     "dry_weight_(%)" = c(NA),
                     "locule_number_(count)" = c(NA),
                     "pericarp_thickness_(mm)" = c(NA),
                     "soluble_solids_(Brix)" = c(NA),
                     "beta-carotene_(mg/g_tissue)" = c(NA),
                     "lycopene_(mg/g_tissue)" = c(NA),
                     "citric_acid_(mg/g_tissue)" = c(NA),
                     "malic_acid_(mg/g_tissue)" = c(NA),
                     "glucose_(mg/g_tissue)" = c(NA),
                     "fructose_(mg/g_tissue)" = c(NA),
                     "alpha-tomatine_(mg/g_tissue)" = c(NA),
                     "dehydrotomatine_(mg/g_tissue)" = c(NA))

# The slashes and parentheses don't make it to colnames, but this seems to 
# fix it:
colnames(to_add) <- colnames(accessions)

# Combining them with the larger set:
accessions <- rbind(accessions, to_add)


# Adding consistent species metadata --------------------------------------
# I want to be able to sort by species, so I'll add a column and make the 
# names consistent.
colnames(accessions)[15] <- "alonge_species" # "species" was taken
accessions$species <- NA
accessions <- accessions[ , c(1:14, 42, 15:41)]# fixing order

accessions$species[grepl("SP", accessions$Assigned_population)] <- "pimpinellifolium"
accessions$species[grepl("SLL", accessions$Assigned_population) | 
                           grepl("SLC", accessions$Assigned_population)] <- "lycopersicum"
accessions$species[grepl("SP",accessions$alonge_species)] <- "pimpinellifolium"
accessions$species[grepl("SLL", accessions$alonge_species)] <- "lycopersicum"
accessions$species[grepl("SLC", accessions$alonge_species)] <- "lycopersicum"
accessions$species[grepl("GAL", accessions$alonge_species)] <- "galapagense"
accessions$species[grepl("CHE", accessions$alonge_species)] <- "cheesmaniae"
accessions$species[accessions$name_TGRC == "LA4768"] <- "lycopersicum" # species from TGRC
accessions$species[accessions$name_BGV == "BGV006148"] <- "pimpinellifolium" # species from TGRC, intermediate
accessions$species[accessions$name_TGRC == "LA0767"] <- "lycopersicum" # species from TGRC
accessions$species[accessions$name_TGRC == "LA4790"] <- "lycopersicum" # species from TGRC 
accessions$species[accessions$name_TGRC == "LA4793"] <- "lycopersicum" # species from TGRC 

# Put this packet aside: CATIE-11106-1
# accessions <- accessions[accessions$accession != "CATIE-11106-1", ]

# Removing an accession that wasn't sent (actually it was)
# accessions <- accessions[is.na(accessions$name_other) | accessions$name_other != "Voyage", ]

# CATIE. What happened with CATIE again?? Removing 
# CATIE for now, but check.
# accessions <- accessions[is.na(accessions$name_other) | accessions$name_other != "CATIE-11106-1", ]

# Problems
nrow(accessions[is.na(accessions$species), ]) # Should be none


# Adding seed packet information ------------------------------------------
# Sara put together a list of seed packets. I will add this information to 
# the table of accessions from Alonge and Razifard.

gs4_deauth()
packet_list <- read_sheet("1RMDwiJy14HxPH7kY3pwske3iXLqL8N3WLHIl2o0WQW8") # not actually a list, technically
packet_list <- packet_list[ , 1]
colnames(packet_list) <- c("packet_name")

# Go through packet names, check all the name columns, if match add the 
# packet name to a new packet name column (make two), if first is full 
# then add to the second one, print error if the second on is full too. 
# at end, mark dups by checking packet name columns.

# Adding packet columns
accessions <- accessions %>% 
  mutate(packet_name_1 = NA,
         packet_name_2 = NA) %>%
  relocate(c(packet_name_1, packet_name_2))


# Doing a loop to match packets to accessions rows. Allows more than once 
# packet to match the same accessions row, because of duplicates.
for (n in seq(1, nrow(packet_list))){
  # Grabbing a row of packet_list
  packet <- packet_list$packet_name[n]
  # print(packet)
  
  row_counter = 0
  
  # Checking for matches in the name columns of each row
  for (x in seq(1, nrow(accessions))){
    accessions_row <- accessions[x, ]
    if (packet %in% accessions_row$name_TGRC[1] | 
        packet %in% accessions_row$name_CC[1] | 
        packet %in% accessions_row$name_EA[1] | 
        packet %in% accessions_row$name_PI[1] | 
        packet %in% accessions_row$name_TS[1] | 
        packet %in% accessions_row$name_BGV[1] | 
        packet %in% accessions_row$name_FLA[1] | 
        packet %in% accessions_row$name_LYC[1] | 
        packet %in% accessions_row$name_TR[1] | 
        packet %in% accessions_row$name_other[1]){
      
      print(paste0("found packet in row ", x))
      
      # Checking to see if packet_name_1 is available
      if (is.na(accessions_row$packet_name_1)){
        print("Adding packet to packet_name_1")
        accessions[x, "packet_name_1"] <- packet
      } else if (is.na(accessions_row$packet_name_2)){
        print("Adding packet to packet_name_2")
        accessions[x, "packet_name_2"] <- packet
      } else {
        print("RAN OUT OF PACKET COLUMNS")
      }

      row_counter = row_counter + 1
      # var = 1
    }
  }
  
  # Troubleshooting
  # if (row_counter > 1){
  #   # print(paste0("DID NOT FIND PACKET: ", packet))
  #   print(paste0(packet, " MATCHED ", row_counter, " ROWS"))
  # }
}


# Adding existing CW names ------------------------------------------------
# I added CW names to the first round by hand, so I'll update the accessions 
# data frame with those here.

# Getting the names I already assigned
gs4_deauth()
cw_list <- read_sheet("1g-Qo17ZPSpwDR89bSBLHQcYLyfxcO8u9ehQ_M8W3DCw")
cw_list <- cw_list[ , c(1:2)]
colnames(cw_list) <- c("name_CW", "packet_name")

# Removing the accessions that I don't have (and keeping a table of the 
# missing ones in case I need it in the future)
missing_accessions <- accessions[is.na(accessions$packet_name_1) & is.na(accessions$packet_name_2), ]
accessions <- accessions[!(is.na(accessions$packet_name_1) & is.na(accessions$packet_name_2)), ]

# Making a new column for the fuzzy join
accessions <- accessions %>%
  unite(united_names, packet_name_1:name_original_razifard, 
        remove = FALSE,
        na.rm = TRUE) %>%
  relocate(united_names)

# Fuzzy joining
accessions <- accessions %>% 
  regex_left_join(cw_list, by = c(united_names = "packet_name")) %>%
  relocate(name_CW) %>%
  select(!c(united_names, packet_name))


# Adding new CW names and wave numbers ------------------------------------
# Now I'll add the rest of the CW names. I want them to just be random at 
# this point (the first set wasn't random). I will add the wave numbers here 
# as well. The new IDs will use this convention:
# 
# CW0000 – CW0999: S. lycopersicum
# CW1000 – CW1999: S. pimpinellifolium
# CW2000 – CW2999: S. cheesmaniae
# CW3000 – CW3999: S. galapagense

# Function to add the new CW names
add_cw_names <- function(input_df, input_species) {
  if (input_species == "lycopersicum") {
    cw_prefix = "CW0"
  } else if (input_species == "pimpinellifolium") {
    cw_prefix = "CW1"
  } else if (input_species == "cheesmaniae") {
    cw_prefix = "CW2"
  } else if (input_species == "galapagense") {
    cw_prefix = "CW3"
  } else {
    stop("Species error")
  }
  
  df <- input_df[input_df$species == input_species, ]
  df <- df[order(df$name_CW), ]
  
  df_old <- df[!is.na(df$name_CW), ] # Getting accessions with CW name
  df_old <- df_old %>% # Putting in the wave
    mutate(wave = 1) %>%
    relocate(wave)
    
  df_new <- df[is.na(df$name_CW), ] # Getting accessions without CW name
  df_new <- df_new[sample(1:nrow(df_new)), ] # Randomizing 
  
  # Assigns new CW names, starting where the existing names left off and 
  # ending at the total number of accessions for this species
  df_new$name_CW <- paste0(cw_prefix, sprintf('%0.3d', nrow(df_old):(nrow(df) - 1)))
  
  # Adding an empty column for the wave for the new names
  df_new <- df_new %>%
    mutate(wave = NA) %>%
    relocate(wave)
  
  # Combining the old and new back together
  df <- rbind(df_old, df_new)
  
  return(df)
}

# Adding the new CW names one species at a time
accessions_lyc <- add_cw_names(accessions, "lycopersicum")
accessions_pim <- add_cw_names(accessions, "pimpinellifolium")
accessions_che <- add_cw_names(accessions, "cheesmaniae")
accessions_gal <- add_cw_names(accessions, "galapagense")

# Combining back together
accessions <- rbind(accessions_lyc,
                    accessions_pim,
                    accessions_che,
                    accessions_gal)

# Adding wave 2 info
accession_ids_lyc <- paste0("CW0", sprintf('%0.3d', 19:37)) # lycopersicum
accession_ids_pim <- paste0("CW1", sprintf('%0.3d', 4:7)) # pimpinellifolium
accession_ids_che <- "CW2001" # cheesmaniae
accession_ids_gal <- "CW3001" # galapagense

accession_ids <- c(accession_ids_lyc,
                   accession_ids_pim,
                   accession_ids_che,
                   accession_ids_gal)

accessions$wave[accessions$name_CW %in% accession_ids] <- 2

# Adding wave 3 info (previous wave + 1 + 18 for lyc)
accession_ids_lyc <- paste0("CW0", sprintf('%0.3d', 38:56)) # lycopersicum
accession_ids_pim <- paste0("CW1", sprintf('%0.3d', 8:11)) # pimpinellifolium
accession_ids_che <- "CW2002" # cheesmaniae
accession_ids_gal <- "CW3002" # galapagense

accession_ids <- c(accession_ids_lyc,
                   accession_ids_pim,
                   accession_ids_che,
                   accession_ids_gal)

accessions$wave[accessions$name_CW %in% accession_ids] <- 3

# Adding wave 4 info (previous wave + 1 + 18 for lyc)
accession_ids_lyc <- paste0("CW0", sprintf('%0.3d', 57:75)) # lycopersicum
accession_ids_pim <- paste0("CW1", sprintf('%0.3d', 12:15)) # pimpinellifolium
accession_ids_che <- "CW2003" # cheesmaniae
accession_ids_gal <- "CW3003" # galapagense

accession_ids <- c(accession_ids_lyc,
                   accession_ids_pim,
                   accession_ids_che,
                   accession_ids_gal)

accessions$wave[accessions$name_CW %in% accession_ids] <- 4

# Adding wave 5 info (previous wave + 1 + 17 for lyc, then add CW0000 (Heinz) manually)
accession_ids_lyc <- paste0("CW0", sprintf('%0.3d', 76:93)) # lycopersicum
accession_ids_pim <- paste0("CW1", sprintf('%0.3d', 16:19)) # pimpinellifolium
accession_ids_che <- "CW2004" # cheesmaniae
accession_ids_gal <- "CW3004" # galapagense

accession_ids <- c(accession_ids_lyc,
                   accession_ids_pim,
                   accession_ids_che,
                   accession_ids_gal)

accessions$wave[accessions$name_CW %in% accession_ids] <- 5

# Adding wave 6 info (previous wave + 1 + 18 for lyc, then add CW0000 (Heinz) manually)
# lyc increases here because cheesmaniae are now finished
accession_ids_lyc <- paste0("CW0", sprintf('%0.3d', 94:112)) # lycopersicum
accession_ids_pim <- paste0("CW1", sprintf('%0.3d', 20:23)) # pimpinellifolium
accession_ids_che <- "CW2005" # cheesmaniae # None so skip next time
accession_ids_gal <- "CW3005" # galapagense

accession_ids <- c(accession_ids_lyc,
                   accession_ids_pim,
                   accession_ids_che,
                   accession_ids_gal)

accessions$wave[accessions$name_CW %in% accession_ids] <- 6

# Adding wave 7 info (previous wave + 1 + 19 for lyc, then add CW0000 (Heinz) manually)
# lyc increases here because cheesmaniae are now finished
accession_ids_lyc <- paste0("CW0", sprintf('%0.3d', 113:132)) # lycopersicum
accession_ids_pim <- paste0("CW1", sprintf('%0.3d', 24:27)) # pimpinellifolium
accession_ids_che <- "" # cheesmaniae # None so skip next time
accession_ids_gal <- "" # galapagense # None

accession_ids <- c(accession_ids_lyc,
                   accession_ids_pim,
                   accession_ids_che,
                   accession_ids_gal)

accessions$wave[accessions$name_CW %in% accession_ids] <- 7

# Uploading to a google sheet for planting --------------------------------
# I'll upload a simplified version of the accessions data frame to a Google 
# sheet here.
simple_accessions <- accessions %>%
  select(c(wave:packet_name_2, species))

# Writes to the sheet.
gs4_auth()

# Make it check first and not overwrite the old ones (for the highlighting)
# for (loop_wave in unique(simple_accessions$wave[!is.na(simple_accessions$wave)])) {
# #   simple_accessions_subset <- simple_accessions[simple_accessions$wave %in% loop_wave, ]
#   write_sheet(simple_accessions_subset,
#               ss = "1MxPu5Mf_2YxfMeR74Bc80K2b_xUm1mrRbAEZFXfH1ec",
#               sheet = paste0("wave_", loop_wave))
# }

# Version for just one wave
simple_accessions_subset <- simple_accessions[simple_accessions$wave %in% 7, ]
write_sheet(simple_accessions_subset,
            ss = "1MxPu5Mf_2YxfMeR74Bc80K2b_xUm1mrRbAEZFXfH1ec",
            sheet = "wave_7")

# Looking at the duplicate packets ----------------------------------------
# I need to figure out which packets are duplicated and which of these are 
# from Esther. Ideally I'll not use her seeds for most of them, then test 
# the germination of the ones without dups and ask her to send more. 






