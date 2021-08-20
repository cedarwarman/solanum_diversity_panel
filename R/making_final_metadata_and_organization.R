# Introduction ------------------------------------------------------------

# I feel like I got lost in the weeds a little bit with the other metadata 
# scripts, so I'll make a clean one here. My goal is to combine primary 
# spreadsheets from the literature and the TGRC and make a definitive list of 
# all the accessions I have along with their associated metadata. I'll then 
# save this list to a Google sheet, then load it in "growing_the_panel.R" for 
# further processing into separate waves for planting.

library(tidyverse)
library(readxl)
library(googlesheets4)


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

# Organizing alonge and razifard ------------------------------------------
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


# Adding in the tgrc_varitome data ----------------------------------------
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

# Manually adding the other two names
razifard$name_TGRC[razifard$name_other == "CATIE-11106-1"] <- "LA4790"
razifard$name_TGRC[razifard$name_other == "PAS014479"] <- "LA4791"


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
                                   "LA4715",
                                   "LA4790",
                                   "LA4792",
                                   "LA4793"),
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
                                      "unknown",
                                      "unknown",
                                      "Tegucigalpa",
                                      "Voyage"),
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
                                   "SP",
                                   "SLL",
                                   "SLL",
                                   "SLL"),
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

# Put this packet aside: CATIE-11106-1
# accessions <- accessions[accessions$accession != "CATIE-11106-1", ]

# Removing an accession that wasn't sent 
accessions <- accessions[is.na(accessions$name_other) | accessions$name_other != "Voyage", ]

# CATIE and Voyage are problems. What happened with CATIE again?? Removing 
# CATIE for now, but check.
accessions <- accessions[is.na(accessions$name_other) | accessions$name_other != "CATIE-11106-1", ]

# Problems
nrow(accessions[is.na(accessions$species), ]) # Should be none


# Adding seed packet information ------------------------------------------
# Sara put together a list of seed packets. I will add this information to 
# the table of accessions from Alonge and Razifard.

gs4_deauth()
packet_list <- read_sheet("1RMDwiJy14HxPH7kY3pwske3iXLqL8N3WLHIl2o0WQW8")
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









