% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nca_classes_IQRdataNCA.R
\name{ignoreSUM_IQdataNCA}
\alias{ignoreSUM_IQdataNCA}
\title{Define records to be excluded from summary and inferential procedures}
\usage{
ignoreSUM_IQdataNCA(data, REASON, USUBJID, PROFILE = NULL, IX)
}
\arguments{
\item{data}{IQdataNCA object in which to ignore selected records}

\item{REASON}{Reason for ignoring the record(s)}

\item{USUBJID}{USUBJID of the subject from which records are to be ignored in the summary procedures. Can be character vector with multiple USUBJIDs}

\item{PROFILE}{Profile name (entry in PROFILE column) to which to apply the ignoring (if multiple profiles available in the IQdataNCA object).
If no PROFILE name defined then this is applied to the subject for all possible profiles. PROFILE can be character vector, allowing to define
multiple profiles to which the ignoring should be applied. If only one profile present then PROFILE is not required to be defined.}

\item{IX}{Index (IX column value) of the record(s) to be ignored in summary with the same reason. Can be scalar or vector.}
}
\value{
Updated IQdataNCA object with adjusted ignore settings
}
\description{
Ignoring of records is done by entering a reason in the IGNORSUM column. A record that is ignored
for the summary and inferential procedures is not considered for summary tables of the observed
concentration data. It will still be listed in the individual concentration listings and might be considered
in the PK parameter determination.
}
\seealso{
Other NCA Data: 
\code{\link{IQdataNCA}()},
\code{\link{check_IQdataNCA}()},
\code{\link{dataSpec_IQdataNCA}()},
\code{\link{exportADNCA_IQdataNCA}()},
\code{\link{export_IQdataNCA}()},
\code{\link{ignoreNCA_IQdataNCA}()},
\code{\link{ignorerecord_IQdataNCA}()},
\code{\link{ignoresubject_IQdataNCA}()},
\code{\link{load_IQdataNCA}()},
\code{\link{plot.IQdataNCA}()},
\code{\link{reportIgnoredSUMNCA_IQdataNCA}()}
}
\concept{NCA Data}
