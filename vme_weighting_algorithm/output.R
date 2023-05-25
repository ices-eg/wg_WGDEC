## Extract results of interest, write TAF output tables

## Before: Score_confidence.csv (model)
## After: Score_confidence.csv (output)
##        Colin_TEST_TEST_TEST (VME database)


library(icesTAF)
library(DBI)
library(odbc)
library(RODBC)
# SOFTWARE.bib installs:
# remotes::install_github("rstats-db/odbc@SQLTable")

# copy over model output - no need to format
mkdir("output")
cp("model/final_vme_score.csv", "output")

# and update the database
results <- read.taf("output/final_vme_score.csv")
# add empty columns for later - don't know why but we get errros when running the stored procedure below:
# 42S22 207 [Microsoft][ODBC SQL Server Driver][SQL Server]Invalid column name 'Uncertainty_Class'.
# [RODBC] ERROR: Could not SQLExecDirect 'EXEC dbo.up_updateWeighting_algorithm_classes')
# And the same for VME_Class...
results$VME_Class <- NA_integer_
results$Uncertainty_Class <- NA_integer_
results$VME_Class_Name <- NA_character_
results$Conf_Name <- NA_character_

msg("Creating / updating: dbo.tbl_VMEWeightingAlgorithm")
conn <- dbConnect(odbc::odbc(),
                 driver="{SQL Server Native Client 11.0}",
                 server="SQL10",
                 Trusted_Connection = "yes",
                 database = "VME")
tbl <- Id(schema = "dbo", table = "tbl_VMEWeightingAlgorithm")
#> <SQL> "dbo"."tbl_VMEWeightingAlgorithm"
dbWriteTable(conn, tbl, results, overwrite = TRUE)
dbDisconnect(conn)


# run procedure
msg("Running update SQL procedure up_updateWeighting_algorithm_classes...")
# DB settings
dbConnection <- 'Driver={SQL Server};Server=SQL10;Database=VME;Trusted_Connection=yes'
# execute stored procedure
conn <- odbcDriverConnect(connection = dbConnection)
ret <- sqlQuery(conn, "EXEC dbo.up_updateWeighting_algorithm_classes")
odbcClose(conn)
msg("return value: ", ret)

# done
