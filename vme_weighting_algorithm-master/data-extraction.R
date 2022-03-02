## Preprocess data, write TAF data tables

## Before:
## After: VME_scores.csv,
##        VME_survey_method.csv,
##        VMEdb_Extraction.csv


library(icesTAF)
library(RODBC)

# create directories
mkdir("data")

# DB settings
dbConnection <- 'Driver={SQL Server};Server=SQL06;Database=VME;Trusted_Connection=yes'

# download and save VME scores locally
conn <- odbcDriverConnect(connection = dbConnection)
out <- sqlQuery(conn, "select * FROM dbo.tblVMEScores")
odbcClose(conn)
write.taf(out, file = "data/VME_scores.csv")

# download and save VME survey method locally
conn <- odbcDriverConnect(connection = dbConnection)
out <- sqlQuery(conn, "select * FROM dbo.tblVMESurveyMethod")
odbcClose(conn)
write.taf(out, file = "data/VME_survey_method.csv")

# download VME database file
conn <- odbcDriverConnect(connection = dbConnection)
#VMEdb <- sqlQuery(conn, "SELECT * FROM dbo.vw_VMEDataset")
# use the pre calculated view in the table below:
VMEdb <- sqlQuery(conn, "SELECT * FROM dbo.tblGetVMEDataset WHERE VME_Indicator is not null or HabitatType is not null")
odbcClose(conn)
#VMEdb <- read.csv("d:/projects/VME/RScript Weighting system WGDEC/RScript Weighting system/Files for R script/VMEdb_Extration2March2016.csv", sep=";", header=TRUE)
# write out to file
write.taf(VMEdb, file = "data/VMEdb_Extraction.csv", quote = TRUE)
