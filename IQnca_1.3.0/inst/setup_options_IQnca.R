# ==============================================================================
# Compliance mode
# ==============================================================================
# If TRUE then  exported PDFs, text files, etc. will have an associated log file
# that  records  time, data, file  location,   and  user. If FALSE,  then  these
# additional log files are not generated. The activated compliance mode requires
# the user to call IQRinitCompliance("ScriptName") at the start of each script.
.COMPLIANCE_MODE <- FALSE


# ==============================================================================
# RDS mode
# ==============================================================================
# If TRUE then each figure and table object that is exported with IQRoutput* functions
# also is exported as *.rds file along with the figure - allowing potential
# post-processing.
.RDS_FILES_OUTPUT <- FALSE


# ==============================================================================
# User settings file
# ==============================================================================
# Define if the user is allowed to override the options in  this file.  If it is
# allowed then the user can place a setup_options_IQnca.R file  in his or her
# home folder and define the variables that should be overriden  (with exception
# of the following variable):
.ALLOW_USER_SETTINGS_FILE <- TRUE
# In a production  environment with a global  installation of  IQRtools it makes
# sense to set this variable to FALSE.


# ==============================================================================
# Define data and parameter specification path
# If NULL then the default documents are used - which might be overwritten in the
# local version of this setup file.
# ==============================================================================
.dataSpecPath <- NULL
.paramSpecPath <- NULL


# ==============================================================================
# Define thresholds to assess if extrapolation of AUC is trustworthy
# Default values set to standard values
#
# If any of those values falls outside accepted range then the depending
# parameters will be flagged in PK parameter listings as not reliably calculated.
# In listings the calculated values will be shown. From summary tables and statistical
# inferences these values will be excluded.
# ==============================================================================

.SPAN_MIN <- 1.5      # Minimum accepted relative interval over which terminal elimination halflife calculated
.LAMZNPT_MIN <- 3     # Minimum accepted number of time points in the slope calculation
.R2ADJ_MIN <- 0.85    # Minimum accepted adjusted R-square value
.AUCEXTRAP_MAX <- 20  # Maximum accepted percentage of extrapolated AUC (both BOLUS IV from 0 to first datapoint and for all ADM types AUCLAST-AUCINF)

.footnoteChar_LAMZ_NA <- "+" # Terminal slope could not be determined
.footnoteChar_DOSE0_NA <- "o" # Dose was 0
.footnoteChar_ISSUE_UNCAUGHT_NA <- "x" # Issue in data - need to check manually - applies to all NA values for which no reason is determined automatically
.footnoteChar_IGNOREDSUBJECT_NA <- "§" # Subject was ignored in PK parameter calculation. Reason: "REASON"

.affectedparam_LAMZ_NA <- c("LAMZHL","AUCIFO","AUCIFP","AUCPEO","AUCPEP","AUMCIFO","AUMCIFP","AUMCPEO","AUMCPEP","AUCPBEO","AUCPBEP","VZFO","VZFP","CLFO","CLFP",
                            "MRTEVIFO","MRTEVIFP","VZFSS","MRTEVIFO","MRTEVIFP","VZO","VZP","CLO","CLP","MRTIVIFO","MRTIVIFP","MRTIVIFO","MRTIVIFP","VSSO",
                            "VSSP","VZSS","MRTIVIFO","MRTIVIFP","MRTIVIFO","MRTIVIFP","SPAN","AILAMZ","LAMZICPT","CORRXY","LAMZ","LAMZLL","LAMZUL","R2","R2ADJ",
                            "CLSTP","AUCTAU","AUMCTAU","CAVG","CTAU","FLUCP","FLUCPTAU","SWINGTAU","AUCIFOD","AUCIFPD","AUCTAU","AUCTAUD","CLSS","AUMCTAU","AUCPTAUE",
                            "AUCINT1","AUCINT2","AUCINT3","AUCINT4","AUCINT5","AUCINT6","AUCINT7","AUCINT8","AUCINT9",
                            "AUCINT1D","AUCINT2D","AUCINT3D","AUCINT4D","AUCINT5D","AUCINT6D","AUCINT7D","AUCINT8D","AUCINT9D")

.affectedparam_DOSE0_NA <- c("AUCLSTD","AUCIFOD","AUCIFPD","AUCTAUD","VZFO","VZFP","CLFO","CLFP","CLFSS","VZFSS","VZO","VZP","CLO","CLP",
                             "CLSS","VZSS","CMAXD","CMIND",
                             "AUCINT1D","AUCINT2D","AUCINT3D","AUCINT4D","AUCINT5D","AUCINT6D","AUCINT7D","AUCINT8D","AUCINT9D")

.footnoteChar_SPAN_LOW <- "a" # SPAN less than .SPAN_MIN
.footnoteChar_LAMZNPT_LOW <- "b" # Less than .LAMZNPT_MIN points in slope calculation
.footnoteChar_R2ADJ_LOW <- "c" # R2 adjusted lower than .R2ADJ_MIN
.footnoteChar_AUCOEXTR_HIGH <- "d" # AUC observed extrapolation > .AUCEXTRAP_MAX
.footnoteChar_AUCPEXTR_HIGH <- "e" # AUC predicted extrapolation > .AUCEXTRAP_MAX


.affectedparam_SPAN_LOW <- c("LAMZHL","AUCIFO","AUCIFP","AUCPEO","AUCPEP","AUMCIFO","AUMCIFP","AUMCPEO","AUMCPEP","AUCPBEO","AUCPBEP","VZFO","VZFP","CLFO","CLFP",
                             "MRTEVIFO","MRTEVIFP","VZFSS","MRTEVIFO","MRTEVIFP","VZO","VZP","CLO","CLP","MRTIVIFO","MRTIVIFP","MRTIVIFO","MRTIVIFP","VSSO",
                             "VSSP","VZSS","MRTIVIFO","MRTIVIFP","MRTIVIFO","MRTIVIFP","SPAN","AILAMZ","LAMZICPT","CORRXY","LAMZ","LAMZLL","LAMZUL","R2","R2ADJ",
                             "CLSTP","AUCTAU","AUMCTAU","CAVG","CTAU","FLUCP","FLUCPTAU","SWINGTAU","AUCIFOD","AUCIFPD","AUCTAU","AUCTAUD","CLSS","AUMCTAU","AUCPTAUE",
                             "AUCINT1","AUCINT2","AUCINT3","AUCINT4","AUCINT5","AUCINT6","AUCINT7","AUCINT8","AUCINT9",
                             "AUCINT1D","AUCINT2D","AUCINT3D","AUCINT4D","AUCINT5D","AUCINT6D","AUCINT7D","AUCINT8D","AUCINT9D")

.affectedparam_LAMZNPT_LOW <- .affectedparam_SPAN_LOW

.affectedparam_R2ADJ_LOW <- .affectedparam_SPAN_LOW

.affectedparam_AUCOEXTR_HIGH <- c("AUCIFO","AUMCIFO","AUCPEO","AUMCPEO","AUCIFOD","AUCPBEO","VZFO","CLFO","MRTEVIFO","VZO","CLO","MRTIVIFO","VSSO")

.affectedparam_AUCPEXTR_HIGH <- c("AUCIFP","AUMCIFP","AUCPEP","AUMCPEP","AUCIFPD","AUCPBEP","VZFP","CLFP","MRTEVIFP","VZP","CLP","MRTIVIFP","VSSP")


# ==============================================================================
# Set number of max major x ticks in plots
# ==============================================================================
.maxNxticks <- 10

# ==============================================================================
# Additional variables defining Figures, Listings, and Tables titles
# ==============================================================================

# Figure titles
.figure_individual_pkconc <- "Figure NRPLACEHOLDER-INDEXPLACEHOLDER Individual observed values vs. time curve"
.figure_spaghetti_pkconc <- "Figure NRPLACEHOLDER-INDEXPLACEHOLDER Spaghetti plot of individual observed values vs. time by dose group"
.figure_spaghetti_dosenormalized_pkconc <- "Figure NRPLACEHOLDER-INDEXPLACEHOLDER Spaghetti plot of dose-normalized individual observed values vs. time"
.figure_summary_mean_pkconc <- "Figure NRPLACEHOLDER-INDEXPLACEHOLDER Arithmetic mean (+SD) concentration vs. time"
.figure_summary_geommean_pkconc <- "Figure NRPLACEHOLDER-INDEXPLACEHOLDER Geometric mean concentration with upper 95% CI vs. time curve"
.figure_summary_mean_dosenorm_pkconc <- "Figure NRPLACEHOLDER-INDEXPLACEHOLDER Arithmetic mean (+SD) dose-normalized concentration vs. time"
.figure_summary_geommean_dosenorm_pkconc <- "Figure NRPLACEHOLDER-INDEXPLACEHOLDER Geometric mean dose-normalized concentration with upper 95% CI vs. time"

.figure_summary_pkconc <- "Figure NRPLACEHOLDER-INDEXPLACEHOLDER AVGPLACEHOLDER (VARPLACEHOLDER) concentration vs. time"

# Listing titles
.listing_excludedanalysis_pkconc <- "Listing NRPLACEHOLDER Subjects and records excluded from the analysis"
.listing_samplingtimes_pkconc <- "Listing NRPLACEHOLDER-INDEXPLACEHOLDER Sampling times for individual concentration data"
.listing_actualtime_pkconc <- "Listing NRPLACEHOLDER-INDEXPLACEHOLDER Actual Sampling time"
.listing_concdetailed_pkconc <- "Listing NRPLACEHOLDER-INDEXPLACEHOLDER Individual concentration data"
.listing_conc_pkconc <- "Listing NRPLACEHOLDER-INDEXPLACEHOLDER Individual concentration data"
.listing_pkparameter_pkconc <- "Listing NRPLACEHOLDER-INDEXPLACEHOLDER Individual pharmacokinetic parameters"

# Table titles
.table_summary_pkconc <- "Table NRPLACEHOLDER-INDEXPLACEHOLDER Summary statistics for PK concentrations"
.table_summary_pkparametersbygroup <- "Table NRPLACEHOLDER-INDEXPLACEHOLDER Summary statistics for PK parameters by dose group"
.table_summary_pkparameters <- "Table NRPLACEHOLDER-INDEXPLACEHOLDER Summary statistics for PK parameters across dose groups"


# ==============================================================================
# Path to IQReport for integrated reporting in Microsoft Word
# ==============================================================================
# The IQReport software can be obtained from http://iqreport.intiquan.com.
# Installation  needs  to be handled separately from IQRtools but here the paths
# to the executables need to be provided (for Windows and Linux separately):
if (Sys.info()[["sysname"]]=="Windows") {
  .PATH_IQRreport <- "C:/LOCAL/IQReport/IQReport.exe"
} else {
  .PATH_IQRreport <- "/opt/IQReport/IQReport.sh"
}


