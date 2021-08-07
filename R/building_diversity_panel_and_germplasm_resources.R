# Introduction ------------------------------------------------------------

# In this script I'll gather together metadata from the Varitome and Alonge
# et al. long read accessions to build my diversity panel. I'll also include 
# germplasm availability, and upload the results to a Google sheet.

library(tidyverse)
library(readxl)
library(googlesheets4)


# Importing the data ------------------------------------------------------

# Razifard et al. 2020 Molecular Biology and Evolution (Varitome)
# This is just the line they sequenced (166), not the total number of lines
# that they used for their phylogeny (295).
razifard <- read_excel(file.path(getwd(), "data", "Razifard_2020_table_s1.xlsx"),
                       skip = 1)

# Fixes some of the colnames, still a bunch of slashes and parentheses so 
# quotes will be required for those
colnames(razifard) <- gsub("\\s","_",colnames(razifard))
colnames(razifard)[1] <- "accession"

# Alonge et al. 2020 Cell
alonge <- read_excel(file.path(getwd(), "data", "Alonge_2020_table_s1.xlsx"),
                     sheet = "S1B",
                     skip = 2)
colnames(alonge)[1] <- "accession"

# TGRC Varitome data (from Roger Chetelat)
tgrc_varitome <- read_excel(file.path(getwd(), "data", "TGRC_Varitome.xlsx"))


# Building the accession table --------------------------------------------

# Starting with the varitome accessions
accessions <- razifard

# Removing some extra metadata (can add back later if it becomes relevant)
accessions <- accessions[ , 1:9]

# Adding that these are from the Varitome set
accessions$part_of_varitome <- TRUE

# Adding the Alonge accessions. 30 are already part of the Varitome, so the 
# full join just adds their metadata columns. In the future I should look to 
# see if there are any conflicts with the subpopulation designations.
alonge$part_of_alonge <- TRUE
accessions <- full_join(accessions, alonge, by = "accession") # is this where the dups come from??
accessions$part_of_varitome[is.na(accessions$part_of_varitome)] <- FALSE
accessions$part_of_alonge[is.na(accessions$part_of_alonge)] <- FALSE

# Adding columns for whether I already have it or not and the source
gs4_deauth()
current_accessions <- read_sheet("1WkWedrEj3j77UFgwjV8TkLfIFzY7Ct_kDEAWztCxPkE")

# Checking to make sure I did the TRUE and FALSE right
# sum(current_accessions$available_usda) # 5
# sum(current_accessions$have_accession) # 83
# sum(current_accessions$have_from_matthias) + sum(current_accessions$have_from_tgrc) # 83

# Combining
accessions <- full_join(accessions, current_accessions, by = "accession")
accessions$have_accession[is.na(accessions$have_accession)] <- FALSE

# Getting some numbers for presentation

# Part of Varitome: 
sum(accessions$part_of_varitome) # 166

# Part of Along:
sum(accessions$part_of_alonge) # 100

# Part of both:
nrow(accessions[accessions$part_of_alonge & accessions$part_of_varitome, ]) # 30

# Adding TGRC numbers to the Varitome lines
tgrc_varitome_key <- tgrc_varitome[ , c(1,4)]
colnames(tgrc_varitome_key) <- c("TGRC_varitome", "accession")
accessions_subset <- accessions[ , c("accession", "TGRC")]

merge_table <- left_join(accessions_subset, tgrc_varitome_key) # why are there extras in a full join?

merge_table <- merge_table %>% 
  mutate("merged" = if_else(is.na(TGRC_varitome), TGRC, TGRC_varitome)) %>%
  select(accession, merged) %>%
  rename(TGRC = merged)

# Checking to make sure the columns are the same
all(accessions$accession == merge_table$accession)

# Replacing the column
accessions$TGRC <- merge_table$TGRC

# Check some of these to make sure they're right
test_df <- accessions[ , c("accession", "TGRC")]


# Making list of accessions I need from TGRC Varitome collection ----------
# These are accessions from the 138 Varitome accessions that the TGRC has 
# that I don't yet have (I probably have some already, through the long 
# read order).

# First getting list of accessions I already have
accession_list <- accessions$accession[accessions$have_accession == TRUE]

# It looks like I already have 20 of the tgrc_varitome accessions
tgrc_varitome_order <- tgrc_varitome[!(tgrc_varitome$AccessionNum %in% accession_list |
                                     tgrc_varitome$OtherID %in% accession_list), ]
tgrc_varitome_order <- tgrc_varitome_order[tgrc_varitome_order$Status == "Active", ]

write.table(tgrc_varitome_order[, 1], 
            file = file.path(getwd(), "output_tables", "tgrc_varitome_order.txt"),
            row.names = F,
            quote = F)


# Making list of accessions I need from Esther ----------------------------
# Razifard entries that are not part of tgrc_varitome_order and not part of 
# accessions$have_accession. Double check the final tables though to make sure
# you didn't fuck anything up.

# Adding a column to "accessions" for Varitome accessions that I ordered from 
# the TGRC.
accessions$varitome_ordered_from_tgrc <- if_else(
  (accessions$accession %in% tgrc_varitome_order$AccessionNum |
     accessions$accession %in% tgrc_varitome_order$OtherID),
  TRUE,
  FALSE
)

# Checking accessions that I didn't order from the TGRC but were part of the
# Varitome:
ask_esther_for_these <- accessions[accessions$part_of_varitome & 
                                   !accessions$varitome_ordered_from_tgrc &
                                   !accessions$have_accession, ]

write.table(ask_esther_for_these[, 1], 
            file = file.path(getwd(), "output_tables", "esther_order.txt"),
            row.names = F,
            quote = F)


# Making a list of USDA accessions ----------------------------------------
# I still need to order a few accessions from the USDA, so I'll do so here.
accessions$available_usda[is.na(accessions$available_usda)] <- FALSE
usda_order <- accessions[accessions$available_usda, ]

write.table(usda_order, 
            file = file.path(getwd(), "output_tables", "usda_order.txt"),
            row.names = F,
            quote = F,
            sep = '\t')


# Output accessions table -------------------------------------------------
# Fixing whitespace
accessions$Notes_from_original_collections <- gsub("\t", "", accessions$Notes_from_original_collections)
accessions$Notes_from_original_collections <- gsub("\n", "", accessions$Notes_from_original_collections)
accessions$Notes_from_original_collections <- gsub("\r", "", accessions$Notes_from_original_collections)
accessions$Collecting_locality <- gsub("\t", "", accessions$Collecting_locality)
accessions$Collecting_locality <- gsub("\n", "", accessions$Collecting_locality)
accessions$Collecting_locality <- gsub("\r", "", accessions$Collecting_locality)
accessions$Local_name <- gsub("\t", "", accessions$Local_name)
accessions$Local_name <- gsub("\n", "", accessions$Local_name)
accessions$Local_name <- gsub("\r", "", accessions$Local_name)

# Fixing other whitespace
accessions$Notes_from_original_collections <- str_trim(accessions$Notes_from_original_collections, side = "both")
accessions$Collecting_locality <- str_trim(accessions$Collecting_locality, side = "both")
accessions$Local_name <- str_trim(accessions$Local_name, side = "both")

write.table(accessions, 
            file = file.path(getwd(), "output_tables", "accessions.txt"),
            row.names = T,
            quote = T,
            sep = '\t')





