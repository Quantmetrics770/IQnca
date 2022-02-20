# -------------------------------------------------------------------------#
# Example with SD data TAU=24 (linlin) ----
# Non zero value at 0 .... handle via endogenous to allow it ...
# Inbuilt R dataset Theoph - compared to Winnonlin run on same data obtained from the web
# -------------------------------------------------------------------------#

library(IQnca)
rm(list=ls())

# -------------------------------------------------------------------------#
# Import data ----
# -------------------------------------------------------------------------#
data <- as.data.frame(Theoph)
data$Subject <- as.numeric(as.character(data$Subject))
names(data) <- c("USUBJID","WT0","DOSE","ATIME","ACONC")
data$DOSE <- 320 # Same dose as in reference used
data$STUDYID <- "Theop R dataset"
data$COMPOUND <- "Theop"
data$ANALYTE <- "Theop"
data$MATRIX <- "Plasma"
data$PROFILE <- "SD profile"
data$PROFTYPE <- "SD"
data$GROUP <- "320 mg SD"
data$GROUPN <- "320"
data$GROUPU <- "mg"
data$DAY <- 1
data$NTIME <- NA
data$TIMEUNIT <- "Hours"
data$CONCUNIT <- "ug/mL"
data$LLOQ <- 0
data$ADM <- "Extravascular"
data$DOSEUNIT <- "mg"
data$TAU <- 24

# derive NTIME
data$NTIME[data$ATIME==0] <- 0
data$NTIME[data$ATIME>0 & data$ATIME<=0.4] <- 0.25
data$NTIME[data$ATIME>0.4 & data$ATIME<=0.7] <- 0.5
data$NTIME[data$ATIME>0.7 & data$ATIME<=1.5] <- 1
data$NTIME[data$ATIME>1.5 & data$ATIME<=2.5] <- 2
data$NTIME[data$ATIME>2.5 & data$ATIME<=3.5] <- 3
data$NTIME[data$ATIME>3.5 & data$ATIME<=5.5] <- 5
data$NTIME[data$ATIME>5.5 & data$ATIME<=7.5] <- 7
data$NTIME[data$ATIME>7.5 & data$ATIME<=9.5] <- 9
data$NTIME[data$ATIME>9.5 & data$ATIME<=14] <- 12
data$NTIME[data$ATIME>14 & data$ATIME<=Inf] <- 24
unique(data$NTIME)

data <- dplyr::arrange(data,USUBJID)

# -------------------------------------------------------------------------#
# Create IQdataNCA dataset ----
# -------------------------------------------------------------------------#

cat("Theoph Linear\n")

dataNCA <- IQdataNCA(data = data,FLAGTIME = "actual",AUCMETHD = "Linear LinearInterpolation",FLAGignore = FALSE)

resNCA <- nca_IQdataNCA(dataNCA)

resNCA

