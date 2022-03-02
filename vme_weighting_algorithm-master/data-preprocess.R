## Preprocess data, write TAF data tables

## Before: VMEdb_Extraction.csv
## After: VMEdb_Extraction_formatted.csv"

library(icesTAF)

# read in and format VME database extraction
VMEdb <- read.taf("data/VMEdb_Extraction.csv", stringsAsFactors = FALSE)

# Attributing "Large Sponges" and "Generic Sponges" indicator category to the "Sponge" group
# (note that i have found genus = Null but geodia in species, implying an incomplete/incorrect database)
LargeSponges <- c("Chonelasma", "Geodia", "Pheronema", "Polymastia",
                  "Stryphnus", "Tetilla", "Thenea", "Vazella")
VMEdb$VME_Indicator[VMEdb$Genus %in% LargeSponges] <- "Large Sponge"
VMEdb$VME_Indicator[VMEdb$VME_Indicator == "Sponge"] <- "Generic Sponge"

# replacing all missing values and "0" by NA
VMEdb[VMEdb %in% c("NULL", "", " ", "0")] <- NA

# extracting "Year" from ObsDate to obtain a separate "Year" column
VMEdb$Year <- VMEdb$intyear

# substituting "1900" from Year by NA since 1900 implies no year available
VMEdb$Year[VMEdb$Year == "1900"] <- NA

# write out to file
write.taf(VMEdb, file = "data/VMEdb_Extraction_formatted.csv", quote = TRUE)
