library(tidyverse)
library(readxl)
library(dplyr)
library(googlesheets4)

# Loading the data --------------------------------------------------------

# Razifard et al. 2020 Molecular Biology and Evolution
# This is just the line they sequenced (166), not the total number of lines
# that they used for their phylogeny (295).
razifard <- read_excel(file.path(getwd(), "data", "Razifard_2020_table_s1.xlsx"),
                       skip = 1)

# Fixes some of the colnames, still a bunch of slashes and parentheses so 
# quotes will be required for those
colnames(razifard) <- gsub("\\s","_",colnames(razifard))

# The full set
razifard_full <- read_excel(file.path(getwd(), "data", "Razifard_2020_table_s3.xlsx"),
                            skip = 1)

# Alonge et al. 2020 Cell
alonge <- read_excel(file.path(getwd(), "data", "Alonge_2020_table_s1.xlsx"),
                     sheet = "S1B",
                     skip = 2)


# Figuring out common lines -----------------------------------------------

# Just comparing the first column (Alonge has other columns with other IDs).
# These should be the Alonge et al accessions that are part of the Razifard 
# collection (30 accessions)
alonge_accessions_in_razifard <- alonge[alonge$Accession %in% razifard$Accession_code_or_cultivar_name, ]

# Checking the other Alonge identifier columns:
alonge[alonge$TGRC %in% razifard$Accession_code_or_cultivar_name, ] # 0
alonge[alonge$CC %in% razifard$Accession_code_or_cultivar_name, ]   # 0 
alonge[alonge$EA %in% razifard$Accession_code_or_cultivar_name, ]   # 0
alonge[alonge$PI %in% razifard$Accession_code_or_cultivar_name, ]   # 4, but already included
alonge[alonge$TS %in% razifard$Accession_code_or_cultivar_name, ]   # 0

# In looks like there are 30 accessions that are in both sets.

# Now comparing the full Razifard set of all 295 lines (47 accessions)
alonge_accessions_in_razifard_full <- alonge[alonge$Accession %in% razifard_full$accession, ]

# Looking at just the Alonge lines that are not in the Razifard sequenced set
setdiff(alonge_accessions_in_razifard_full$Accession, alonge_accessions_in_razifard$Accession)

# How many of the these two sets (30 and 47 accessions) do I already have?
gs4_deauth()
current_accessions <- read_sheet("1WkWedrEj3j77UFgwjV8TkLfIFzY7Ct_kDEAWztCxPkE")
current_accessions <- current_accessions[current_accessions$have_accession |
                                           current_accessions$available_usda, ]
current_accessions <- current_accessions[current_accessions$available_usda, ]


# Number I have in Razifard sequenced set? 25
sum(current_accessions$accession %in% alonge_accessions_in_razifard$Accession)

# Number I have in Razifard full set? 40
sum(current_accessions$accession %in% alonge_accessions_in_razifard_full$Accession)

