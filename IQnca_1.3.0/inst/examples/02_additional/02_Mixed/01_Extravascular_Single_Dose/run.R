rm(list=ls())
library(IQnca)
IQRinitCompliance("run.R")

# The hardest part is to get all the information out of always different winnonlin files :-)
# Good thing is that IQnca uses a standard data format which is easy to understand and easy to fill in.
dataOrig <- readxl::read_excel("Extravascular_data.xls")
dataOrig <- dataOrig[-1,]
data <- data.frame(
  USUBJID = dataOrig$ID,
  STUDYID = "Example Study",
  COMPOUND = "Test Drug",
  ANALYTE = "Test Drug",
  MATRIX = "Plasma",
  PROFILE = "Single Dose (SD)",
  PROFTYPE = "SD",
  GROUP = "100 mg",
  GROUPN = 100,
  GROUPU = "mg",
  DAY = NA,
  ATIME = NA,
  NTIME = as.numeric(dataOrig$TIME),
  TIMEUNIT = "Hours",
  ACONC = {
    x <- dataOrig$CONC
    x[grepl("<",x)] <- 0
    as.numeric(x)
  },
  CONCUNIT = "ng/mL",
  LLOQ = 5,
  ADM = "Extravascular",
  DOSE = as.numeric(dataOrig$Dose),
  DOSEUNIT = "mg",
  stringsAsFactors = FALSE
)

# -------------------------------------------------------------------------#
# Linear Up Log Down - LLOQ/2 and 0 ----
# -------------------------------------------------------------------------#

dataNCA <- IQdataNCA(data,FLAGTIME = "nominal",AUCMETHD = "LinearUp LogDown",FLGBLQPO = "0")
result <- nca_IQdataNCA(data = dataNCA,CTLASTwinnonlinbehavior = TRUE)


# -------------------------------------------------------------------------#
# Linear Log - LLOQ/2 and 0----
# -------------------------------------------------------------------------#

dataNCA <- IQdataNCA(data,FLAGTIME = "nominal",AUCMETHD = "Linear Log",FLGBLQPO = "0")
result <- nca_IQdataNCA(data = dataNCA,CTLASTwinnonlinbehavior = TRUE)


# -------------------------------------------------------------------------#
# Linear Linear Interpolation with interval AUC ----
# -------------------------------------------------------------------------#

AUCINVAL <- "[0;6]"
dataNCA <- IQdataNCA(data,FLAGTIME = "nominal",AUCMETHD = "Linear LinearInterpolation",FLGBLQPO = "0",AUCINVAL = AUCINVAL)
result <- nca_IQdataNCA(data = dataNCA,CTLASTwinnonlinbehavior = TRUE)


# -------------------------------------------------------------------------#
# Linear LinearLog Interpolation with interval AUC ----
# -------------------------------------------------------------------------#

AUCINVAL <- "[0;6]"
dataNCA <- IQdataNCA(data,FLAGTIME = "nominal",AUCMETHD = "Linear LinearLogInterpolation",FLGBLQPO = "0",AUCINVAL = AUCINVAL)
result <- nca_IQdataNCA(data = dataNCA,CTLASTwinnonlinbehavior = TRUE)

# -------------------------------------------------------------------------#
# Linear Up Log Down - BLLOQ 0 ----
# -------------------------------------------------------------------------#

dataNCA <- IQdataNCA(data,FLAGTIME = "nominal",AUCMETHD = "LinearUp LogDown",FLGBLQP1 = "0",FLGBLQPO = "0")
result <- nca_IQdataNCA(data = dataNCA,CTLASTwinnonlinbehavior = TRUE)

