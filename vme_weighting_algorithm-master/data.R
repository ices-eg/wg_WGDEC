## Preprocess data, write TAF data tables

## Before:
## After: VME_scores.csv,
##        VME_survey_method.csv,
##        VMEdb_Extraction.csv,
##        VMEdb_Extraction_formatted.csv"

library(icesTAF)

sourceTAF("data-extraction.R")
sourceTAF("data-preprocess.R")
