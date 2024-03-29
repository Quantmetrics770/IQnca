% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nca_classes_IQRdataNCA.R
\name{ignorerecord_IQdataNCA}
\alias{ignorerecord_IQdataNCA}
\title{Define records to be ignored with reason why}
\usage{
ignorerecord_IQdataNCA(data, REASON, USUBJID, PROFILE = NULL, IX)
}
\arguments{
\item{data}{IQdataNCA object in which to ignore selected records}

\item{REASON}{Reason for ignoring the record(s)}

\item{USUBJID}{USUBJID of the subject from which records are to be ignored. Can be character vector with multiple USUBJIDs}

\item{PROFILE}{Profile name (entry in PROFILE column) to which to apply the ignoring (if multiple profiles available in the IQdataNCA object).
If no PROFILE name defined then this is applied to the subject for all possible profiles. PROFILE can be character vector, allowing to define
multiple profiles to which the ignoring should be applied.}

\item{IX}{Index (IX column value) of the record(s) to be ignored with the same reason. Can be scalar or vector.}
}
\value{
Updated IQdataNCA object with adjusted ignore settings
}
\description{
Ignoring of records is done by entering a reason in the IGNORER column. A record that is ignored
is not considered at all in any step. This means it does not appear in the listings, tables, figures,
and neither is considered in the PK parameter determination.
After ignoring the slope is recalculated. IT DOES NOT EVEN APPEAR IN THE LISTINGS OF THE
individual PK concentrations! So ... handle with care!
}
\seealso{
Other NCA Data: 
\code{\link{IQdataNCA}()},
\code{\link{check_IQdataNCA}()},
\code{\link{dataSpec_IQdataNCA}()},
\code{\link{exportADNCA_IQdataNCA}()},
\code{\link{export_IQdataNCA}()},
\code{\link{ignoreNCA_IQdataNCA}()},
\code{\link{ignoreSUM_IQdataNCA}()},
\code{\link{ignoresubject_IQdataNCA}()},
\code{\link{load_IQdataNCA}()},
\code{\link{plot.IQdataNCA}()},
\code{\link{reportIgnoredSUMNCA_IQdataNCA}()}
}
\concept{NCA Data}
