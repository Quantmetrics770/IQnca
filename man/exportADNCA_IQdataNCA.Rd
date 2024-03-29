% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nca_classes_IQRdataNCA.R
\name{exportADNCA_IQdataNCA}
\alias{exportADNCA_IQdataNCA}
\title{Export an IQdataNCA object to an ADNCA dataset}
\usage{
exportADNCA_IQdataNCA(data, pathname = ".", addColLabels = NULL)
}
\arguments{
\item{data}{IQdataNCA object}

\item{pathname}{Path where to store the adnca.xpt dataset}

\item{addColLabels}{Named list. Names define column names and elements define labels for these columns - only used in generation of ADNCA}
}
\description{
General ADPC format cannot contain all the information needed for reproducing an NCA analysis.
We instead use an "ADNCA" format that contains all information in the data that is needed
for the reproduction of the NCA analysis. Standard file name is "adnca.xpt". The path can be chosen.
In addition to the adnca.xpt dataset a define.pdf file is generated, documenting the dataset format.
}
\seealso{
Other NCA Data: 
\code{\link{IQdataNCA}()},
\code{\link{check_IQdataNCA}()},
\code{\link{dataSpec_IQdataNCA}()},
\code{\link{export_IQdataNCA}()},
\code{\link{ignoreNCA_IQdataNCA}()},
\code{\link{ignoreSUM_IQdataNCA}()},
\code{\link{ignorerecord_IQdataNCA}()},
\code{\link{ignoresubject_IQdataNCA}()},
\code{\link{load_IQdataNCA}()},
\code{\link{plot.IQdataNCA}()},
\code{\link{reportIgnoredSUMNCA_IQdataNCA}()}
}
\concept{NCA Data}
