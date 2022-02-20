rm(list=ls())
library(IQnca)
IQRinitCompliance("run.R")

# The hardest part is to get all the information out of always different winnonlin files :-)
# Good thing is that IQnca uses a standard data format which is easy to understand and easy to fill in.
dataOrig <- readxl::read_excel("plasma_data.xls")
dataOrig <- dataOrig[-1,]
data <- data.frame(
  USUBJID = dataOrig$Subject,
  STUDYID = "Example Study",
  COMPOUND = "Test Drug",
  ANALYTE = "Test Drug",
  MATRIX = dataOrig$DCP_Description,
  PROFILE = "Single dose",
  PROFTYPE = "SD",
  GROUP = ifelse(dataOrig$Treatment=="Low Dose","30 mg SD","60 mg SD"),
  GROUPN = ifelse(dataOrig$Treatment=="Low Dose",30,60),
  GROUPU = "mg",
  DAY = 1,
  ATIME = as.numeric(dataOrig$Relative_Actual_Time),
  NTIME = dataOrig$Relative_Nominal_Time,
  TIMEUNIT = "Hours",
  ACONC = dataOrig$Conc,
  CONCUNIT = "ug/mL",
  LLOQ = 0, # Faking unknown LLOQ ... The winnonlin format is not really informative ...
  ADM = "Extravascular",
  DOSE = ifelse(dataOrig$Treatment=="Low Dose",30,60),
  DOSEUNIT = "mg",
  VISIT = "Undefined",
  VISITNUM = NA,
  PCTPT = dataOrig$Time_Label,
  EXSTDTC = NA,
  PERIOD = NA,
  AGE = dataOrig$Age,
  SEX = dataOrig$Sex,
  RACE = dataOrig$Race,
  stringsAsFactors = FALSE
)

# Importing the data, setting the options
dataNCA <- IQdataNCA(data,FLAGTIME = "actual",AUCMETHD = "LinearUp LogDown")

# Exporting the data ... some col names certainly do not 100% match the ad requirements but that
# is something that can be fixed later. Currently at least colnames are not longer than 8 chars.
# A define file is not yet generated but will do so soon.
export_IQdataNCA(data = dataNCA,filename = "RESULTS/adnca",CSV = TRUE,ADNCA = TRUE)

# Just running the NCA ... no options provided here ... all options are stored in the input data.
# Essentially the input data contains all information to even automatically write out a full report ...
# with methods section etc.
result <- nca_IQdataNCA(data = dataNCA)

# Export of results in pretty much the same format as the Winnonlin Excel files. We just use CSV instead
# of Excel format. The same function will be able to export adpp.xpt and a define file ... its on the list
# it is easy ... but time is limited. Default export is CSV.
export_IQnca(data = result,filename = "RESULTS/Results_pivoted")

