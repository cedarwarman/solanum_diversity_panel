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
# tgrc_varitome df might reveal some of the dups.


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
