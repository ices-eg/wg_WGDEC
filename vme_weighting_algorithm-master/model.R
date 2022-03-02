## Run analysis, write model results

## Before: VME_scores.csv (data)
##         VME_survey_method.csv (data)
##         VMEdb_Extraction_formatted.csv (data)
## After: Score_confidence.csv (model)

library(icesTAF)
library(data.table)
library(dplyr)

mkdir("model")

# Read VME indicator list with score; as agreed in the ICES WGDEC 2016
VMEscores <- read.taf("data/VME_Scores.csv")

# Read VME survey method; as agreed in the ICES WGDEC 2016
VMEsurveymeth <- read.taf("data/VME_survey_method.csv")

# Read VME database file
VMEdb <- read.taf("data/VMEdb_Extraction_formatted.csv")


#    -----------------      model settings   -------------------

# Determine VME index weighting values for VME scores and Abundance threshold
# (abundance will only accounts for 10% of the final scores, 90% to taxonomy)
wVMEscore <- 0.90
wABUNDthr <- 0.10

# Determine weight (kg) thresholds to be used in score calculation
SpongeThr1 <- 200
SpongeThr2 <- 60
CoralThr1 <- 30
CoralThr2 <- 1


# Determine abdundance score based on weight values
# (scores related to above thresholds, note that abundance only accounts for 10% of the final scores)
Score_below_thr <- 1 * wABUNDthr
Score_betw_thr <- 3 * wABUNDthr
Score_abov_thr <- 5 * wABUNDthr

# Define threshold on how old the last survey was
LowMyears <- 10
HighMyears <- 30

# Define confidence score for how old the last survey was
LowMaxYScore <- 1
MidMaxYScore <- 0.5
HighMaxYScore <- 0

# Define threshold for the range of years covered in the cell
LowRange <- 5
HighRange <- 10

# Define confidence score for the range of years
LowRangeScore <- 0
MidRangeScore <- 0.5
HighRangeScore <- 1

# Define the thresholds for number of surveys per CSquare
LowNsurveythr <- 3
HighNsurveythr <- 5

# Define uncertainty score for number of surveys per CSquare
LowNsurveyScore <- 0
MidNsurveyScore <- 0.5
HighNsurveyScore <- 1

# Confidence Score for type of Survey Method (K=known, Infer=infered, WK = well-known)
WK <- 1
Infer <- 0
K <- 0.5

# calculating the score of each the VME indicator group in the VMEscores data frame
VMEscores$indicator_score <-
  sqrt(rowMeans(
    VMEscores[,c("Rarity", "Functionality", "Fragility", "Life history", "Structural complexity")]^2
  ))

VMEscores <-
  within(VMEscores, {
    # calculating the score of each VME indicator when no abundance data is available in the VMEscores data frame
    indicator_score_noabund <- indicator_score * wVMEscore

    # calculating the score of each VME indicator when below 60kg of sponges or 1kg for corals in the VMEscores data frame
    indicator_score_below <- indicator_score_noabund + Score_below_thr

    # calculating the score of each VME indicator when between 60kg and 200 kg of sponges (1kg and 30kg of corals) in the VMEscores data frame
    indicator_score_between <- indicator_score_noabund + Score_betw_thr

    # calculating the score of each VME indicator when above 200 kg of sponges and 30kg of corals in the VMEscores data frame
    indicator_score_above <- indicator_score_noabund + Score_abov_thr
  })


# ADDING the VME indicator score when no abundance data exists in the VMEdb(i.e. vlookup)
VMEdb$noabundscore <- left_join(VMEdb, VMEscores, by = c("VME_Indicator" = "VME indicator"))$indicator_score_noabund

# ADDING the SurveyMethod score to each Method in the VMEdb
VMEdb$SurveyMethodcode <- left_join(VMEdb, VMEsurveymeth, by = "SurveyMethod")$description

VMEdb$SurveyMethodscore <-
  ifelse(VMEdb$SurveyMethodcode == "WK" | !is.na(VMEdb$HabitatType),
         WK,
         ifelse(is.na(VMEdb$SurveyMethodcode),
                Infer,
                ifelse(VMEdb$SurveyMethodcode == "Infer", Infer, K)))



# VMEScores not used below this line

#    -----------------      VME SCORE CALCULATION   -------------------

VMEdb$Score <-
  ifelse(is.na(VMEdb$VME_Indicator) & is.na(VMEdb$HabitatType),
         1,
    ifelse(!is.na(VMEdb$HabitatType),
         5,
      VMEdb$noabundscore +
        ifelse(is.na(VMEdb$Weight_kg),
           0,
          ifelse(VMEdb$VME_Indicator == "Large Sponge" | VMEdb$VME_Indicator == "Generic Sponge",
              ifelse(VMEdb$Weight_kg < SpongeThr2, Score_below_thr,
                ifelse(VMEdb$Weight_kg < SpongeThr1, Score_betw_thr, Score_abov_thr)
              ),
              ifelse(VMEdb$Weight_kg < CoralThr2, Score_below_thr,
                ifelse(VMEdb$Weight_kg < CoralThr1, Score_betw_thr, Score_abov_thr))
          )
        )
    )
  )


# ----------------------           CONFIDENCE (based on MAX scores of each cell)  ----------------

# Subset of VMEdb having all the records with max scores for each CSquares
VMEdb1 <- VMEdb[!is.na(VMEdb$Score),]
dt <- data.table(VMEdb1)
dt.max <- dt[,.SD[which(Score == max(Score))],by = CSquare]
df.max <- data.frame(dt.max)

VMEdb$SkeyMeth <- paste (VMEdb$SurveyKey,VMEdb$SurveyMethod, sep = "---")
dt_meth <- data.table(VMEdb)


# EXTRACTING average score per CSquare of SurveyMethod in the VMEdb

## new way where take average value of survey methods associated with unique surveys within that cell

dt_meth1 <- data.table(df.max)
dt.methmax1 <- dt_meth[,unique(SkeyMeth, na.action = NULL, na.rm = TRUE), by = CSquare]
df.methmax1 <- data.frame(dt.methmax1)

df.methmax1 <- transform(df.methmax1, test = do.call(rbind, strsplit(V1, "---", fixed = TRUE)), stringsAsFactors = FALSE)
colnames(df.methmax1) <- c("CSquare", "MethSurv", "surveykey", "method")
df.methmax1$method[which(df.methmax1$method == "NA")] <- NA

df.methmax1$SurveyMethodcode <- left_join(df.methmax1, VMEsurveymeth, by = c("method" = "SurveyMethod"))$description

df.methmax1$SurveyMethodscore <-
  ifelse(df.methmax1$SurveyMethodcode == "WK" ,
         WK,
         ifelse(is.na(df.methmax1$SurveyMethodcode),
                Infer,
                ifelse(df.methmax1$SurveyMethodcode == "Infer",
                       Infer,
                       K)))

# Calcuylate the average “survey method” score for each individual csquares
# (of all the individual records within a csquare).
MeanMethScore_csq2_new <- unique(df.methmax1["CSquare"])
rownames(MeanMethScore_csq2_new) <- MeanMethScore_csq2_new$CSquare
tbl.methodScore <- tapply(df.methmax1$SurveyMethodscore, df.methmax1$CSquare, mean)
MeanMethScore_csq2_new$MeanMethErr_new <- tbl.methodScore[MeanMethScore_csq2_new$CSquare]

# EXTRACTING the highest year in each CSquare in the df.max
# WARNINGS
MaxYear_csq2 <- aggregate(df.max$Year ~ df.max$CSquare, df.max, max, na.action = NULL, na.rm = TRUE)
colnames(MaxYear_csq2) <- c("CSquare", "MaxYear")

# EXTRACTING the lowest year in each CSquare  in the df.max
## WARNINGS
MinYear_csq2 <- aggregate(df.max$Year ~ df.max$CSquare, df.max, min, na.action = NULL, na.rm = TRUE)
colnames(MinYear_csq2) <- c("CSquare", "MinYear")

# MERGING MIN AND MAX YEARS IN ONE DATAFRAME
Years_csq2 <- merge(MaxYear_csq2,MinYear_csq2, by = "CSquare")

# CALCULATING the range of years in each CSquare
Years_csq2$Range <- (Years_csq2$MaxYear - Years_csq2$MinYear) + 1

# CALCULATING the number of years since 2016 in each CSquare
Years_csq2$Myears <- (2016 - Years_csq2$MaxYear)

# SUBSTITUTING THE -INF BY NA
Years_csq2[Years_csq2 == "-Inf" ] <- NA
Years_csq2[Years_csq2 == "Inf" ] <- NA

# EXTRACTING the max VME score in each CSquare in the df.max
MaxScore_csq2 <- aggregate(df.max$Score ~ df.max$CSquare, df.max, max)
colnames(MaxScore_csq2) <- c("CSquare", "MaxScore")

# EXTRACTING the number of different surveys in each CSquare
# 1 define the function then pass it into aggregate
count_unq <- function(x) {
    if(all(is.na(x)) == TRUE) return(NA)
    length(unique(x))
}

# 2 pass the function into aggregate
Nsurvey_csq2 <- aggregate(SurveyKey ~ CSquare, data = df.max, na.action= NULL, count_unq)
colnames(Nsurvey_csq2) <- c("CSquare", "Nsurvey")


# ---------   CALCULATING CONFIDENCE ASSOCIATED WITH NSURVEYS --------
Nsurvey_csq2$NsurveyErr <-
  ifelse(Nsurvey_csq2$Nsurvey < LowNsurveythr | is.na(Nsurvey_csq2$Nsurvey),
         LowNsurveyScore,
         ifelse(Nsurvey_csq2$Nsurvey > HighNsurveythr,
                HighNsurveyScore,
                MidNsurveyScore))

# ---------   CALCULATING CONFIDENCE ASSOCIATED WITH TIMES SPAN (RANGE) --------
Years_csq2$timespanErr <-
  ifelse(Years_csq2$Range < LowRange | is.na(Years_csq2$Range),
         LowRangeScore,
         ifelse(Years_csq2$Range > HighRange,
                HighRangeScore,
                MidRangeScore))

# ---------    CALCULATING CONFIDENCE ASSOCIATED WITH AGE OF YOUNGEST RECORDS  --------
Years_csq2$MyearsErr <-
  ifelse(Years_csq2$Myears > HighMyears| is.na(Years_csq2$Myears),
         HighMaxYScore,
         ifelse(Years_csq2$Myears < LowMyears,
                LowMaxYScore,
                MidMaxYScore))

# CREATING NEW DATAFRAME FOR calculating final uncertainty score

Final2 <- cbind(Years_csq2[, c("timespanErr", "MyearsErr")],
                MaxScore_csq2[, c("CSquare", "MaxScore")],
                MeanMethErr_new = MeanMethScore_csq2_new$MeanMethErr_new,
                NsurveysErr = Nsurvey_csq2$NsurveyErr)

Final2$Confidence_mean <-
  ifelse(Final2$MaxScore == 5,
         1,
         rowMeans(subset(Final2, select = c(timespanErr, MyearsErr, MeanMethErr_new, NsurveysErr)), na.rm = TRUE))

Final2$Uncertainty_score <-
  ifelse(Final2$MaxScore == 5,
         1,
         sqrt(rowMeans(Final2[,c("timespanErr","MyearsErr","NsurveysErr","Confidence_mean")]^2))
         )

# rename columns
Final2 <- dplyr::rename(Final2, VME_index = MaxScore)

# select only the columns we need
Final_score <- Final2[,c("CSquare","VME_index", "Uncertainty_score")]

# write output
write.taf(Final_score, file = "model/final_vme_score.csv")
