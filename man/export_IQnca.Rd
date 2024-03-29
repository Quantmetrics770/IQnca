% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nca_classes_IQRdataNCA.R
\name{export_IQnca}
\alias{export_IQnca}
\title{Export an IQnca object}
\usage{
export_IQnca(
  data,
  filename = "nca_results",
  SIGNIF = 8,
  TXT = FALSE,
  CSV = TRUE,
  parameterReport = "standard"
)
}
\arguments{
\item{data}{IQnca object}

\item{filename}{Filename with path for export. No extension considered but added automatically.}

\item{SIGNIF}{Number of significant digits}

\item{TXT}{logical if TRUE then export as IQRoutputTable (pivoted).}

\item{CSV}{logical if TRUE then export as CSV file (pivoted).}

\item{parameterReport}{Character string defining the PK parameters to report in the listing.
By default these parameters will be selected based on the "Standard" parameters defined in the IQparamNCA_Specification.xlsx
document, subject to the profile and administration type. The user can select "All" or "Standard".
Alternatively, parameterReport can also be a vector or parameter names (based on PKPARAMCD in IQparamNCA_Specification.xlsx).
Instead of the standard or all ones then these will be considered in the listing (also subject to profile and
administration type). Note that interval AUC (if calculated is always added by default!)
If defined it overrides the default labels.}
}
\description{
Export the NCA results. Pivoted text and CSV format or long format as ADPP XPT dataset
}
\seealso{
Other NCA Parameter Export: 
\code{\link{exportCDISC_IQnca}()}
}
\concept{NCA Parameter Export}
