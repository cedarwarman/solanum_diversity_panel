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

# This is the manual record of seed packets that we have.
gs4_deauth()
packet_list <- read_sheet("1RMDwiJy14HxPH7kY3pwske3iXLqL8N3WLHIl2o0WQW8")
length(unique(packet_list$Accession_ID))


# Combining the packet list with the metadata -----------------------------
# I need to combine the packet list with the metadata and figure out which
# accessions are present, as well as make sure there aren't any typos, etc.

matching_rows <- accessions[accessions$accession %in% packet_list$Accession_ID |
                            accessions$TGRC %in% packet_list$Accession_ID |
                            accessions$PI %in% packet_list$Accession_ID |
                            accessions$CC %in% packet_list$Accession_ID |
                            accessions$TS %in% packet_list$Accession_ID |
                            accessions$EA %in% packet_list$Accession_ID |
                            accessions$Local_name %in% packet_list$Accession_ID, ]






