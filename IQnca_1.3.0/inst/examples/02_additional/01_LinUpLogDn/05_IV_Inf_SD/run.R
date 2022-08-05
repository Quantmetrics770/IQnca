rm(list=ls())
library(IQnca)
IQRinitCompliance("run.R")

# The hardest part is to get all the information out of always different winnonlin files :-)
# Good thing is that IQnca uses a standard data format which is easy to understand and easy to fill in.
dataOrig <- readxl::read_excel("IV_single_Dose_Infusion - Corrected.xls")
dataOrig <- dataOrig[-1,]
data <- data.frame(
  USUBJID = dataOrig$ID,
  STUDYID = "Example Study",
  COMPOUND = "Test Drug",
  ANALYTE = "Test Drug",
  MATRIX = "Plasma",
  PROFILE = "Single IV Infusion Dose (SD)",
  PROFTYPE = "SD",
  GROUP = paste0(dataOrig$Dose, "mg/m2"),
  GROUPN = as.numeric(dataOrig$Dose),
  GROUPU = "mg/m2",
  DAY = NA,
  ATIME = as.numeric(dataOrig$Actual_time),
  NTIME = as.numeric(dataOrig$Nominal_time),
  TIMEUNIT = "Hours",
  ACONC = {
    x <- dataOrig$Conc
    x[grepl("BLQ",x)] <- 0
    as.numeric(x)
  },
  CONCUNIT = "ug/mL",
  LLOQ = 5,
  ADM = "Infusion",
  DOSE = as.numeric(dataOrig$Dose_1),
  DOSEUNIT = "mg",
  NDUR = 2,
  ADUR = NA,
  stringsAsFactors = FALSE
)

# Manually add the infusion times. Wouldn't it be nice if Winnonlin would be able to store everything in a single file?
# Maybe in the next decade it will be modernized?
data$ADUR[data$USUBJID==1] <- 1.83
data$ADUR[data$USUBJID==2] <- 2.17
data$ADUR[data$USUBJID==3] <- 2.07
data$ADUR[data$USUBJID==4] <- 2.67
data$ADUR[data$USUBJID==5] <- 3.67
data$ADUR[data$USUBJID==6] <- 2.5


# Importing the data, setting the options
dataNCA <- IQdataNCA(data,FLAGTIME = "actual",AUCMETHD = "LinearUp LogDown",FLGBLQPO = "0")

# Exporting the data ... some col names certainly do not 100% match the ad requirements but that
# is something that can be fixed later. Currently at least colnames are not longer than 8 chars.
# A define file is not yet generated but will do so soon.
export_IQdataNCA(data = dataNCA,filename = "RESULTS/adnca",CSV = TRUE,ADNCA = TRUE)

# Just running the NCA ... no options provided here ... all options are stored in the input data.
# Essentially the input data contains all information to even automatically write out a full report ...
# with methods section etc.
result <- nca_IQdataNCA(data = dataNCA,CTLASTwinnonlinbehavior = TRUE)

# Export of results in pretty much the same format as the Winnonlin Excel files. We just use CSV instead
# of Excel format. The same function will be able to export adpp.xpt and a define file ... its on the list
# it is easy ... but time is limited. Default export is CSV.
export_IQnca(data = result,filename = "RESULTS/Results_pivoted")

