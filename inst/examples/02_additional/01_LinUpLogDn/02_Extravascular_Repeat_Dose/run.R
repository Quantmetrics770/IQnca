rm(list=ls())
library(IQnca)
IQRinitCompliance("run.R")

# The hardest part is to get all the information out of always different winnonlin files :-)
# Good thing is that IQnca uses a standard data format which is easy to understand and easy to fill in.
dataOrig <- readxl::read_excel("Steady_state_data.xls")
dataOrig <- dataOrig[-1,]
data <- data.frame(
  USUBJID = dataOrig$ID,
  STUDYID = "Example Study",
  COMPOUND = "Test Drug",
  ANALYTE = "Test Drug",
  MATRIX = "Plasma",
  PROFILE = "Repeated Dose (SS)",
  PROFTYPE = "SS",
  GROUP = "100 mg QD",
  GROUPN = 100,
  GROUPU = "mg",
  DAY = NA,
  ATIME = NA,
  NTIME = as.numeric(dataOrig$TIME),
  TIMEUNIT = "Hours",
  ACONC = as.numeric(dataOrig$CONC),
  CONCUNIT = "ng/mL",
  LLOQ = 0, # Faking unknown LLOQ ... The winnonlin format is not really informative ...
  ADM = "Extravascular",
  DOSE = as.numeric(dataOrig$DOSE),
  DOSEUNIT = "mg",
  TAU = 24,
  stringsAsFactors = FALSE
)

cat("LinearUp LogDown EV SS\n")

# Importing the data, setting the options
dataNCA <- IQdataNCA(data,FLAGTIME = "nominal",AUCMETHD = "LinearUp LogDown")

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


