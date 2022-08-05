#' Installs defined webinar module from the web
#'
#' Convenient function to install IntiQuan Webinar material directly from the IntiQuan Server
#'
#' @param module_number Module number
#' @export
#' @family Help & Documentation
install_MIDDmodule <- function (module_number) {
  if (.Platform$OS.type=="windows") {
    methodDownload = "wininet"
    installDir <- "c:/IntiQuan/Webinars" 
  } else {
    methodDownload = "libcurl"
    installDir <- "~/IntiQuan/Webinars"
  }
  url <- paste0("https://training.intiquan.com/MIDDmodules/M",module_number,".zip")
  if (!url_exists(url)) stopIQR("Requested webinar module does not exist on server.")
  aux_mkdir(installDir)
  setwd(installDir)
  utils::download.file(url = url,
                       destfile = "webinardownload.zip",
                       mode="wb",
                       method=methodDownload)
  utils::unzip(zipfile = "webinardownload.zip",overwrite = TRUE)
  unlink("webinardownload.zip",force = TRUE)
  message(paste0("The Webinar Module ",module_number," material is installed in '",installDir,"/",module_number,"'.\n\nEnjoy It!\n"))
}
#' Open support webpage
#'
#' Convenient function to open the support webpage
#'
#' @export
#' @family Help & Documentation
support <- function () {
  utils::browseURL("https://groups.google.com/a/intiquan.com/g/iq-nca-users-group")
}
#' Installs workshop material
#'
#' Convenient function to install workshop material directly from the IntiQuan Server
#'
#' @param workshop Workshop identifier
#' @export
#' @family Help & Documentation
install_workshop <- function (workshop) {
  if (.Platform$OS.type=="windows") {
    methodDownload = "wininet"
  } else {
    methodDownload = "libcurl"
  }
  setwd("~")
  utils::download.file(url = paste0("https://training.intiquan.com/",workshop,"/material.zip"),
                destfile = "IntiQuan.zip",
                mode="wb",
                method=methodDownload)
  utils::unzip(zipfile = "IntiQuan.zip",overwrite = TRUE)
  unlink("IntiQuan.zip",force = TRUE)
  message(paste0("The workshop material is installed in '~/IntiQuan/.\n\nHappy Workshopping!\n"))
}
#' Open IQnca documentation in browser
#'
#' @export
#' @family Help & Documentation
doc_IQnca <- function () {
  if (file.exists(system.file(package="IQnca",paste0("docs/book/index.html")))) {
    location <- system.file(package="IQnca",paste0("docs/book/index.html"))
  } else {
    location <- "https://iqnca.intiquan.com"
  }
  utils::browseURL(location)
}
#' Install examples to allow running examples from the book
#'
#' This function copies the examples in inst folder
#' to the default path (~/IntiQuan/IQnca/examples). In addition it sets the
#' path to the path where the material was copied.
#'
#' @param path Path to where to copy the book example files. Should not contain spaces
#' @export
#' @family Help & Documentation
examples_IQnca <- function(path="~/IntiQuan/IQnca"){
  if (grepl(" ",path)) {
    stopIQR("The path contains spaces. Choose a different path")
  }
  aux_mkdir(path)
  file.copy(from=system.file(package="IQnca","examples"), to=path,recursive=TRUE,overwrite = TRUE)
  setwd(path)
  message(paste0("IQnca documentation example material copied to: ",path))
  message(paste0("Working directory set to: ",path))
}
showStartupMessage <- function () {
  Rversion <- paste0(version$major,".",version$minor)
  message <- paste0("\n",aux_strrep(crayon::bold(crayon::green("IQ"),crayon::silver("nca"))," ",""),": ",
                    "Noncompartmental Analysis in R - No clicks required!\n\n           ",
                    crayon::bold("Version:",aux_version()),"\n\n","       Open Source (GNU AGPL-3) - Provided to You by IntiQuan\n\n")
  cat(message)
  loadSetupOptions_IQnca()
  if (.COMPLIANCE_MODE) {
    cat(crayon::bold(crayon::green("       Compliance mode is ON\n\n")))
  } else {
    cat(crayon::bold(crayon::red("       Compliance mode is OFF\n\n")))
  }
  cat(crayon::bold(crayon::cyan("       Support: https://support.intiquan.com/iqnca\n\n")))
  return(invisible(NULL))
}
#' Opening the setup_options_IQnca.R file for editing
#'
#' This function opens the setup_options_IQnca.R for editing IQnca options.
#' The user can choose to edit the general / global settings file in the packages
#' installation location or create/edit such a file in the users home folder.
#' On cluster installations a user might not be able to change the global file and
#' in such a case the use of a users file might be useful.
#' Note that the users setup_options_IQnca.R file does not need to include all
#' information present in the global setup_options_IQnca.R file. The user file
#' can be used to override one or more of the settings in the global file.
#' In the setup_options_IQnca.R file located in the package installation folder
#' it can be decided if such a user defined settings file is allowed or not.
#'
#' @param local Logical. If FALSE then the function will open the global file for
#'   editing. If TRUE then the function will open the local file for editing. If
#'   the local file is not present then it will be created. If the definition of
#'   a local file is disabled in the global settings file, the local=TRUE setting
#'   will lead to an error.
#'
#' @export
#' @family Installation
setup_IQnca <- function(local=FALSE){
  if (!local) {
    file.edit(system.file(package="IQnca","setup_options_IQnca.R"))
  } else {
    loadSetupOptions_IQnca()
    if (!.ALLOW_USER_SETTINGS_FILE) {
      stopIQR("The use of a local setup_options_IQnca.R file has been disabled")
    }
    home__ <- Sys.getenv("HOME")
    file__ <- paste0(home__,"/setup_options_IQnca.R")
    file.edit(file__)
  }
}
url_exists <- function(x, non_2xx_return_value = FALSE, quiet = TRUE,...) {
  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)
        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stopIQR("Terminated by user", call. = FALSE)
      }
    )
  }
  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }
  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)
  res <- sHEAD(x, ...)
  if (is.null(res$result) ||
      ((httr::status_code(res$result) %/% 200) != 1)) {
    res <- sGET(x, ...)
    if (is.null(res$result)) return(NA) 
    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warningIQR(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }
    return(TRUE)
  } else {
    return(TRUE)
  }
}
#' Exporting a value with label and potential description for reporting
#'
#' Single values or elements can be pushed to a table that can be used to
#' store values of interest during the analysis. Upon call of this
#' function the path (path argument) an rds file is generated with
#' the provided label, value, and description content. If compliance
#' mode is on also the compliance information is stored. In addition
#' all similar files are read in and a table is constructed and
#' exported in the text based format used by IQReport.
#' This table is then useful when reporting results inline in IQReport.
#' The path to the value table information can be chosen. A default is
#' provided. The name of the resulting table text file is fixed to
#' TAB01_ValueTable.txt as there can only be one such file per value
#' table folder.
#'
#' @param value A single value (numeric or string) to report in the table.
#' @param label A label allowing to reference the element. Only characters A-Z, a-z, 0-9 are allowed.
#' @param description More wordy description of the element. Default: "".
#' @param path Path to the folder in which to store the value table elements and where to generate
#'   the value table text file for reporting.
#' @export
#' @family Output & Compliance
IQRoutputValueTable <- function(value,label,description="",path="../Output/00_ValueTable") {
  if (length(value) > 1) stopIQR("Value has to be of length 1")
  if (nchar(gsub("\\W","",label)) != nchar(label)) stopIQR("label argument only allowed to contain characters: A-Z, a-z, 0-9")
  if (file.exists(path)) {
    vTpre <- load_ValueTable(path)
    if (any(grepl(label,vTpre$label))) stopIQR("Label argument is already present in value table elements")
  }
  vTelement <- data.frame(
    label = label,
    value = value,
    description = description,
    stringsAsFactors = FALSE
  )
  filename <- paste0(path,"/vT_",label,".rds")
  if (file.exists(filename)) stopIQR("Valuetable element with same filename already exists.")
  IQRoutputRDS(vTelement,filename)
  filenameVT <- paste0(path,"/TAB01_ValueTable.txt")
  tab <- load_ValueTable(path)
  IQRoutputTable(xtable = tab,xtitle = "Value table for lookup and reporting purposes",filename = filenameVT)
  return(invisible(NULL))
}
load_ValueTable <- function(path="../Output/00_ValueTable") {
  if (!dir.exists(path)) stopIQR("Value table folder does not exist")
  files <- list.files(path,pattern = "*.rds",full.names = TRUE)
  files <- files[!grepl(".rds.log",files)]
  do.call(rbind,lapply(files, function (file) {
    out__ <- readRDS(file)
    logInfo <- parseLogfile(paste0(file,".log"))
    if (!is.null(logInfo)) {
      out__$log <- paste0(logInfo$outputfile,"; ",logInfo$analysisfile,"; ",logInfo$date)
    } else {
      out__$log <- ""
    }
    out__
  }))
}
parseLogfile <- function (path) {
  if (!file.exists(path)) return(NULL)
  content <- aux_fileread(path,collapserows = FALSE)
  list(
    username = aux_strtrim(aux_explode(content[grepl("Username",content)],"\\|")[2]),
    analysisfile = aux_strtrim(aux_explode(content[grepl("Analysis file",content)],"\\|")[2]),
    date = aux_strtrim(aux_explode(content[grepl("Date of creation",content)],"\\|")[2]),
    outputfile = aux_strtrim(aux_explode(content[grepl("File (relative to calling function)",content,fixed = TRUE)],"\\|")[2])
  )
}
#' Checks if the compliance mode is enabled
#'
#' @return Returns TRUE is compliance mode is enabled. FALSE otherwise
#' @export
is_enabled_complianceMode <- function(){
  loadSetupOptions_IQnca()
  test <- globalenv()$IQRNCA_OVERRIDE_SETTING_COMPLIANCE_MODE
  test <- ifelse(is.null(test),FALSE,test)
  if (exists("IQRNCA_OVERRIDE_SETTING_COMPLIANCE_MODE")) return(IQRNCA_OVERRIDE_SETTING_COMPLIANCE_MODE)
  return(.COMPLIANCE_MODE)
}
#' Checks if the rds mode is enabled
#'
#' @return Returns TRUE is rds mode is enabled. FALSE otherwise
#' @export
is_enabled_rdsMode <- function(){
  loadSetupOptions_IQnca()
  return(.RDS_FILES_OUTPUT)
}
#' Initializes the compliance mode
#'
#' The compliance mode log file generation requires the global
#' variable COMPLIANCE_MODE_SCRIPT_NAME to be defined in the global
#' environment. The user needs to take care that this is the same as
#' the script file name from his/her analysis script, generating the outputs
#' of interest for compliance tracking.
#'
#' @param scriptname Name of analysis script, generating the outputs
#'   of interest for compliance tracking.
#' @export
#' @family Output & Compliance
IQRinitCompliance <- function(scriptname){
  e__ <- globalenv()
  e__$COMPLIANCE_MODE_SCRIPT_NAME <- scriptname
}
genComplianceLog <- function(outputfilename, FLAGshort = FALSE, FLAGsession = FALSE) {
  if (!is_enabled_complianceMode()) return()
  if (is.null(outputfilename)) return()
  e__ <- globalenv()
  if (!("COMPLIANCE_MODE_SCRIPT_NAME" %in% ls(e__)))
    stopIQR("Compliance mode is enabled but the COMPLIANCE_MODE_SCRIPT_NAME variable has not\nbeen defined in the global environment (by the user). You can use the function IQRinitCompliance to do so")
  COMPLIANCE_MODE_SCRIPT_NAME <- e__$COMPLIANCE_MODE_SCRIPT_NAME
  logt__ <- "<TT>   File generation log"
  logfilepath__ <- paste(outputfilename,'.log',sep='')
  outputfilename <- gsub('//','/',outputfilename)
  userrow__ <- Sys.info()[['user']]
  timerow__ <- Sys.time()
  short__ <- paste0(COMPLIANCE_MODE_SCRIPT_NAME, " | User: ", userrow__, " | Date: ", timerow__)
  callr__   <- paste('<TR>   Analysis file                       | ',COMPLIANCE_MODE_SCRIPT_NAME,sep='')
  pathrow__ <- paste('<TR>   File (relative to calling function) | ',outputfilename,sep='')
  userrow__ <- paste('<TR>   Username                            | ',userrow__,sep='')
  timerow__ <- paste('<TR>   Date of creation                    | ',timerow__,sep='')
  loglength__ <- max(1,max(nchar(callr__)),nchar(timerow__),nchar(userrow__),nchar(pathrow__))
  logsep__ <- paste(c("       ",rep("=",loglength__-7),"\n"),collapse="",sep='')
  content__ <- c(logt__,logsep__,pathrow__,userrow__,timerow__,callr__)
  if (FLAGshort) content__ <- short__
  if (FLAGsession) return(content__)
  write(content__,logfilepath__,append=FALSE)
}
#' Exporting a data.frame as CSV file
#'
#' The compliance log file will only be generated if .COMPLIANCE_MODE is set to TRUE.
#' The name of the generated compliance log file is the same name as the text file name
#' but with appended ".log". The extension for the exported text file will be kept
#' unchanged in the exported log file name. In order for the log file generation to
#' work the global variable "COMPLIANCE_MODE_SCRIPT_NAME" needs to be defined by the user
#' and in each R script set to the name of the script.
#'
#' @param data The data.frame or matrix to export
#' @param filename Character string with the filename (can include path) for export.
#'   Should include extension to the filename.
#' @param na Character to represent NA values
#' @param quote TRUE: use "" quotes, FALSE: do not (for NONMEM and MONOLIX)
#' @param row.names TRUE: exposrt row names FALSE: do not (typical use in IQnca)
#' @param FLAGattributes FALSE: do not export custom attributes as .atr file. TRUE: do (default)
#' @param replaceComma NULL: do not replace comma. Character: replace by user-provided character
#' @export
#' @family Output & Compliance
#' @family General Data I/O
#' @author Henning Schmidt, Daniel Kaschek, Daniel Lill, IntiQuan
IQRoutputCSV <- function(data,
                         filename,
                         na=".",
                         quote=FALSE,
                         row.names=FALSE,
                         FLAGattributes=TRUE,
                         replaceComma=NULL) {
  if (is.null(filename)){
    warningIQR("filename is NULL - no file written")
    return(data)
  }
  filename.csv__ <- paste0(aux_strrep(filename,".csv",""),".csv")
  IQRsaveCSVdata(data,
                 filename.csv__,
                 na=na,
                 quote=quote,
                 row.names=row.names,
                 FLAGattributes=FLAGattributes,
                 replaceComma=replaceComma)
  genComplianceLog(filename.csv__)
  if (FLAGattributes) {
    filenameATR <- gsub('\\.csv(.gz)?$','.atr', filename)
    if (file.exists(filenameATR)) {
      genComplianceLog(filenameATR)
    }
  }
}
#' Exporting a text file with added compliance information log file
#'
#' The compliance log file will only be generated if .COMPLIANCE_MODE is set to TRUE.
#' The name of the generated compliance log file is the same name as the text file name
#' but with appended ".log". The extension for the exported text file will be kept
#' unchanged in the exported log file name. In order for the log file generation to
#' work the global variable "COMPLIANCE_MODE_SCRIPT_NAME" needs to be defined by the user
#' and in each R script set to the name of the script.
#' If compliance model is enabled, the original graphics object will also be stored as
#' an RDS file, allowing subsequent further processing.
#'
#' @param text Character string to export in a file
#' @param filename Character string with the filename (can include path) for export.
#'   Should include extension to the filename.
#' @export
#' @family Output & Compliance
IQRoutputFile <- function(text,filename) {
  if (is.null(filename)) return()
  aux_mkdir(aux_fileparts(filename)$pathname)
  aux_filewrite(text,filename)
  genComplianceLog(filename)
}
#' Exporting an R object to RDS format with added compliance information log file
#'
#' The compliance log file will only be generated if .COMPLIANCE_MODE is set to TRUE.
#' The name of the generated compliance log file is the same name as the text file name
#' but with appended ".log". The extension for the exported text file will be kept
#' unchanged in the exported log file name. In order for the log file generation to
#' work the global variable "COMPLIANCE_MODE_SCRIPT_NAME" needs to be defined by the user
#' and in each R script set to the name of the script.
#'
#' @param object R object to export
#' @param filename Character string with the filename (can include path) for export.
#'   ".rds" extension will be added and ignored in provided filename.
#' @export
#' @family Output & Compliance
IQRoutputRDS <- function(object,filename) {
  if (is.null(filename)) return()
  filename <- paste0(aux_strrep(filename,".rds",""),".rds")
  aux_mkdir(aux_fileparts(filename)$pathname)
  saveRDS(object,filename)
  genComplianceLog(filename)
}
#' Printing a single ggplot object to png file with added compliance information log file
#'
#' The compliance log file will only be generated if .COMPLIANCE_MODE is set to TRUE.
#' The name of the generated compliance log file is the same name as the text file name
#' but with appended ".log". The extension for the exported text file will be kept
#' unchanged in the exported log file name. In order for the log file generation to
#' work the global variable "COMPLIANCE_MODE_SCRIPT_NAME" needs to be defined by the user
#' and in each R script set to the name of the script.
#' If compliance model is enabled, the original graphics object will also be stored as
#' an RDS file, allowing subsequent further processing.
#'
#' @param gr ggplot graph object to be printed to file
#' @param filename Character string with the filename (can include path) for export.
#'   Should include extension to the filename.
#' @param res Resolution in DPI
#' @param height height of figure in inch
#' @param width width of figure in inch
#' @param scale relative factor for scaling of both width and height
#' @param scaleWidth relative factor for scaling width
#' @param scaleHeight relative factor for scaling height
#' @param ... whatever Dx put there
#' @export
#' @family Output & Compliance
IQRoutputPNG <- function(gr, filename=NULL, res = 300, width = 21/2.54, height = 21/2.54*3/4, scale=1, scaleWidth=1, scaleHeight=1, ...) {
  on.exit({if (!is.null(filename)) aux_closePNGs()})
  if (!is.null(filename)) {
    aux_closePNGs()
    filename <- paste0(aux_strrep(filename,".png",""),".png")
    aux_mkdir(aux_fileparts(filename)$pathname)
    grDevices::png(filename, height = height*scale*scaleHeight, width = width*scale*scaleWidth, bg = "transparent", res = res, units = "in")
    if ("gtable" %in% class(gr))
    {
      print(graphics::plot(gr))
    } else {
      print(gr)
    }
    genComplianceLog(filename)
    aux_closePNGs()
    if (is_enabled_rdsMode()) {
      saveRDS(gr,file=paste0(filename,".rds"))
    }
  }
}
#' Printing ggplot object or list of objects to a PDF file with added compliance information log file
#'
#' Flexible function to allow single or multi-page PDFs. A single ggplot object can be passed
#' or a list of ggplot objects. If a list is passed the plots are arranged in a grid that
#' can be defined by arguments nrow and ncol. By default these are set to 1 each, plotting a
#' single graphics object on a single page in the PDF.
#' The compliance log file will only be generated if .COMPLIANCE_MODE is set to TRUE.
#' The name of the generated compliance log file is the same name as the text file name
#' but with appended ".log". The extension for the exported text file will be kept
#' unchanged in the exported log file name. In order for the log file generation to
#' work the global variable "COMPLIANCE_MODE_SCRIPT_NAME" needs to be defined by the user
#' and in each R script set to the name of the script - this is done using the
#' IQRinitCompliance function.
#' If compliance model is enabled, the original graphics object will also be stored as
#' an RDS file, allowing subsequent further processing.
#'
#' @param gr ggplot graph object to be printed to file
#' @param filename Character string with the filename (can include path) for export.
#'   Should include extension to the filename (.pdf).
#' @param width width of figure in inch
#' @param height height of figure in inch
#' @param scale relative factor for scaling of both width and height
#' @param scaleWidth relative factor for scaling width
#' @param scaleHeight relative factor for scaling height
#' @param nrow Number of rows for grid plotting if graphics object is a list
#' @param ncol Number of columns for grid plotting if graphics object is a list
#' @param ... Additional arguments passed to \code{\link[grDevices]{pdf}()}
#' @export
#' @family Output & Compliance
IQRoutputPDF <- function(gr,
                         filename=NULL,
                         width = 21/2.54,
                         height = 21/2.54*3/4,
                         scale=1,
                         scaleWidth=1,
                         scaleHeight=1,
                         nrow = 1, ncol = 1,
                         ...) {
  on.exit({if (!is.null(filename)) aux_closePDFs()})
  if (!is.null(filename)) {
    aux_closePDFs()
    filename <- paste0(aux_strrep(filename,".pdf",""),".pdf")
    aux_mkdir(aux_fileparts(filename)$pathname)
    dummy__ <- grDevices::pdf(file = filename,
                              onefile=TRUE,
                              width=width*scale*scaleWidth,
                              height=height*scale*scaleHeight,
                              ...)
    if ("gtable" %in% class(gr)) {
      print(graphics::plot(gr))
    } else {
      if ("ggplot" %in% class(gr)) {
        print(gr)
      } else {
        if ("list" %in% class(gr)& "arrangelist" %in% class(gr[[1]])) {
          plyr::l_ply(gr, print)
        } else {
          if ("list" %in% class(gr)) {
            printGrid(gr, nrow = nrow, ncol = ncol)
          } else {
            if ("IQRslideplot" %in% class(gr)) {
              print(gr)
            } else {
              stopIQR("Unhandled graphics object")
            }
          }
        }
      }
    }
    aux_closePDFs()
    genComplianceLog(filename)
    if (is_enabled_rdsMode()) {
      saveRDS(gr,file=paste0(filename,".rds"))
    }
  }
}
#' Start export to PDF (deprecated)
#'
#' PDF export is started with IQRoutputPDFstart. Then all graphical output
#' goes to this PDF file. PDF export into this file is ended by
#' IQRoutputPDFend. If the compliance mode is on (.COMPLIANCE_MODE is set to TRUE)
#' then the exported PDF file is annotated by an additional log file.
#' The name of the generated compliance log file is the same name as the text file name
#' but with appended ".log". The extension for the exported text file will be kept
#' unchanged in the exported log file name. In order for the log file generation to
#' work the global variable "COMPLIANCE_MODE_SCRIPT_NAME" needs to be defined by the user
#' and in each R script set to the name of the script.
#'
#' It is the call to IQRoutputPDFend that actually generates the log file.
#'
#' @param filename Name of the PDF file to generate. Can include path. If
#'   the folder in the path is not present, it will be created.
#' @param width width of figure in inch
#' @param height height of figure in inch
#' @param scale relative factor for scaling of both width and height
#' @param scaleWidth relative factor for scaling width
#' @param scaleHeight relative factor for scaling height
#' @param ... Additional arguments passed to \code{\link[grDevices]{pdf}()}
#' @export
#' @family Output & Compliance
IQRoutputPDFstart <- function(filename, width = 21/2.54, height = 21/2.54*3/4, scale=1, scaleWidth=1, scaleHeight=1, ...) {
  aux_closePDFs()
  if (!is.null(filename)) {
    aux_mkdir(aux_fileparts(filename)$pathname)
    if (file.exists(filename)) {
      unlink(filename)
    }
    dummy__ <- grDevices::pdf(file = paste0(aux_strrep(filename,".pdf",""),".pdf"), onefile=TRUE,
                              width=width*scale*scaleWidth,
                              height=height*scale*scaleHeight, ...)
  }
}
#' Ends export to PDF (deprecated)
#'
#' PDF export is started with IQRoutputPDFstart. Then all graphical output
#' goes to this PDF file. PDF export into this file is ended by
#' IQRoutputPDFend. If the compliance mode is on (.COMPLIANCE_MODE is set to TRUE)
#' then the exported PDF file is annotated by an additional log file.
#' The name of the generated compliance log file is the same name as the text file name
#' but with appended ".log". The extension for the exported text file will be kept
#' unchanged in the exported log file name. In order for the log file generation to
#' work the global variable "COMPLIANCE_MODE_SCRIPT_NAME" needs to be defined by the user
#' and in each R script set to the name of the script.
#'
#' It is the call to IQRoutputPDFend that actually generates the log file.
#'
#' @param filename Name of the PDF file
#' @export
#' @family Output & Compliance
IQRoutputPDFend <- function(filename) {
  aux_closePDFs()
  if (!is.null(filename)) {
    genComplianceLog(paste0(aux_strrep(filename,".pdf",""),".pdf"))
  }
}
#' Convert dataframe to IQRoutputTable object and allow export
#'
#' This function allows to export a data.frame as a table with optional
#' title and footer information. Export is done if a filename is provided.
#' If the compliance mode is on (.COMPLIANCE_MODE is set to TRUE) then this function
#' also generates a compliance log file (only if filename is provided).
#' The name of the generated compliance log file is the same name as the text file name
#' but with appended ".log". The function also returns an IQRoutputTable object
#' that essentially is a list with xtable, xtitle, and xfooter entries.
#' If compliance model is enabled, the IQRoutputTable object will also be stored as
#' an RDS file, allowing subsequent further processing.
#'
#' @param xtable Table to be exported as dataframe
#' @param xfooter Character vector of length 1 or >1. If multiple elements are provided,
#' they are pasted with a newline character as separator.
#' @param xtitle Character with table title text
#' @param object IQRoutputTable object. If defined, xtable is not used but xfooter,
#'   xtitle and report (if defined) will be set in the object, overwriting old content
#' @param filename String with filename (can include path)
#' @param report TRUE: add reporting tags
#'               FALSE: do not add (for scfreen output only)
#'               NULL: same as TRUE if xtable provided.
#' @param na.string Character string to replace NA entries by. Defaults to NULL.
#' @param FLAGreplaceRoundBracketsHeader If TRUE then "(" and ")" in table headers will be replaced
#'   by `"["` and `"["`. If FALSE, then no such replacement is done. Needed for compatibility with IQReport on Linux.
#' @param verbose Logical, should the output be returned to the console. This argument is
#'   only effective when filename is provided.
#' @return IQRoutputTable object. This object can be converted to text using the
#'   function text_IQRoutputTable().
#' @export
#'
#' @family Output & Compliance
IQRoutputTable <- function(xtable=NULL,
                           xfooter=NULL,
                           xtitle=NULL,
                           object=NULL,
                           filename=NULL,
                           report=NULL,
                           na.string=NULL,
                           FLAGreplaceRoundBracketsHeader=TRUE,
                           verbose=NULL) {
  if (!is.null(filename)) {
    filename <- paste0(aux_strrep(filename,".txt",""),".txt")
    aux_mkdir(aux_fileparts(filename)$pathname)
  }
  if (is.null(verbose)) verbose <- TRUE
  if (is.null(xtable) & is.null(object)) stopIQR("Neither xtable nor object defined")
  if (!is.null(xtable) & !is.null(object)) stopIQR("Both xtable and object defined")
  if (!is.null(xtable) & is.null(report)) report <- TRUE
  if (!is.null(xfooter)) {
    xfooter <- paste(xfooter, collapse = "\n")
  }
  if (!is.null(object) & (!"IQRoutputTable" %in% class(object))) stopIQR("Input argument object is not an IQRoutputTable object")
  if (is.null(object)) {
    out__ <- list(
      xtable = xtable,
      xtitle = xtitle,
      xfooter = xfooter,
      report = report,
      filename = filename
    )
  } else {
    out__ <- list(
      xtable = object$xtable,
      xtitle = {
        if (is.null(xtitle)) {
          x__ <- object$xtitle
        } else {
          x__ <- xtitle
        }
        x__
      },
      xfooter =  {
        if (is.null(xfooter)) {
          x__ <- object$xfooter
        } else {
          x__ <- xfooter
        }
        x__
      },
      report =  {
        if (is.null(report)) {
          x__ <- object$report
        } else {
          x__ <- report
        }
        x__
      },
      filename =  {
        if (is.null(filename)) {
          x__ <- object$filename
        } else {
          x__ <- filename
        }
        x__
      }
    )
  }
  class(out__) <- c("IQRoutputTable",class(out__))
  if (!is.null(filename)) {
    aux_filewrite(text=text_IQRoutputTable(out__,
                                           report=report,
                                           na.string=na.string,
                                           FLAGreplaceRoundBracketsHeader=FLAGreplaceRoundBracketsHeader),
                  filename=paste0(aux_strrep(filename,".txt",""),".txt"))
    genComplianceLog(filename)
    if (is_enabled_rdsMode()) {
      saveRDS(out__,file=paste0(filename,".rds"))
    }
  }
  if (is.null(filename) | verbose) {
    return(out__)
  }
}
#' Overloading "print" for IQRoutputTable objects
#'
#' @param x IQRoutputTable object
#' @param ... Additional parameters
#' @export
print.IQRoutputTable <- function (x, ...) {
  cat(text_IQRoutputTable(x,report = FALSE))
  cat("\n\nIQRoutputTable object")
}
#' Convert IQRoutputTable to text representation
#'
#' @param object IQRoutputTable object
#' @param report TRUE: add reporting tags
#'               FALSE: do not add (for scfreen output only).
#'               Also stored in object but can be overridden here
#' @param na.string Character string to replace NA entries by. Defaults to NULL.
#' @param FLAGreplaceRoundBracketsHeader If TRUE then "(" and ")" in table headers will be replaced
#'   by `"["` and `"["`. If FALSE, then no such replacement is done. Needed for compatibility with IQReport on Linux.
#' @return Character string with table in text format
#' @export
text_IQRoutputTable <- function (object,report=NULL, na.string=NULL,FLAGreplaceRoundBracketsHeader=TRUE) {
  if (!"IQRoutputTable" %in% class(object)) stopIQR("Input argument not an IQRoutputTable object")
  xtable <- object$xtable
  xfooter <- object$xfooter
  xtitle <- object$xtitle
  if (is.null(report)) report <- object$report
  if (FLAGreplaceRoundBracketsHeader) {
    n__ <- names(xtable)
    n__ <- gsub("(","[",n__,fixed = TRUE)
    n__ <- gsub(")","]",n__,fixed = TRUE)
    names(xtable) <- n__
  }
  if (!is.null(xfooter)) {
    if (report) {
      xfooter <- gsub("\n","<br>",xfooter,fixed = TRUE)
      xfooter <- gsub("^\\* ","\\\\* ",xfooter)
      xfooter <- gsub("<br>\\* ","<br>\\\\* ",xfooter)
    } else {
      xfooter <- gsub("<br>","\n",xfooter,fixed = TRUE)
    }
  }
  if (report) {
    trid__ <- "<TR>"
    tfid__ <- "<TF>"
    ttid__ <- "<TT>"
    thid__ <- "<TH>"
  } else {
    trid__ <- ""
    tfid__ <- ""
    ttid__ <- ""
    thid__ <- ""
  }
  tr__ <- format(data.frame(lapply(xtable,function (x) as.character(x)),stringsAsFactors = FALSE),justify="left")
  if (!is.null(na.string))
    tr__ <- data.frame(lapply(tr__, function(x) {x[grepl("^NA$", trimws(x))] <- na.string; x} ))
  if ("matrix" %in% class(xtable)){
    th__ <- paste0("X",c(1:ncol(xtable)))
  }  else{ 
    th__ <- colnames(xtable)
  }
  widths <- rep(1,ncol(xtable))
  for (coli__ in c(1:length(widths))) {
    tr_nchar <- nchar(tr__[,coli__])
    tr_nchar <- ifelse(length(tr_nchar) == 0, 0, tr_nchar)
    th_nchar <- nchar(th__[coli__])
    th_nchar <- ifelse(length(th_nchar) == 0, 0, th_nchar)
    widths[coli__] <- max(1, max(tr_nchar, th_nchar))
  }
  for (coli__ in c(1:length(th__))){
    th__[coli__] <- format(th__[coli__], width = widths[coli__])
  }
  th__ <- paste(th__,collapse= " | ")
  th__ <- paste(thid__," ",th__, collapse= "")
  for (coli__ in c(1:length(widths))){
    tr__[,coli__] <- sprintf(paste('%-',widths[coli__], 's',sep=''),tr__[,coli__])
  }
  tr__ <- utils::capture.output(utils::write.table(tr__,quote=FALSE,sep=" | ",row.names = FALSE,col.names=FALSE))
  tr__ <- paste(tr__,collapse = paste0("\n",trid__,"   "))
  tr__ <- paste(paste0(trid__,"  "),tr__,collapse="")
  if (!is.null(xfooter)) {
    if (report) {
      tf__ <- paste(tfid__,"  ",xfooter, collapse= "")
    } else {
      tf__ <- paste0(tfid__,xfooter, collapse= "")
    }
    tf__ <- format(tf__, width = nchar(th__))
  } else {
    tf__ <- ""
  }
  if (!is.null(xtitle)) {
    tt__ <- paste(ttid__," ",xtitle, collapse="")
    tt__ <- format(tt__, width = nchar(th__))
  } else {
    tt__ <- ""
  }
  tablength__ <- max(nchar(th__),nchar(tt__))  
  nminus <- 7
  if (!report) nminus <- 3
  xxx <- "       "
  if (!report) xxx <- "   "
  thsep__ <- paste0(c(xxx,rep("-",tablength__-nminus),"\n"),collapse="")
  tfsep__ <- paste0(c(xxx,rep("-",tablength__-nminus)),collapse="")
  ttsep__ <- paste0(c(xxx,rep("=",tablength__-nminus),"\n"),collapse="")
  if (is.null(xtitle)) {
    IQRtableHead__ <- paste(th__,thsep__, sep = "\n")
  } else {
    IQRtableHead__ <- paste(tt__,ttsep__,th__,thsep__, sep = "\n")
  }
  if (is.null(xfooter)) {
    IQRtableFoot__ <- NULL
  } else {
    IQRtableFoot__ <- paste(tfsep__,tf__, sep = "\n")
  }
  allTable__ <- paste0(IQRtableHead__,tr__,"\n",IQRtableFoot__)
  allTable__
}
#' Create IQRoutputFigure object and/or print graph object to file
#'
#' This function allows to print a (list of) figure(s), i.e, plot objects,
#' as png or pdf. The figures are arranged in given number of rows and columns
#' potentially across several pages (or files in case of png's). A common title,
#' subtitle, footer, and potentially legend is plotted on each page.
#' Alternatively to a list of plot object, an IQRoutputFigure object (see Details)
#' can be the input to this function.
#'
#' In case, a pdf of png file is created, the figures are distributed to rows, columns,
#' and pages and title, subtitle, footer, and legend are added using the function
#' [createPages_IQRoutputFigure]. The resulting list of plot objects is printed using
#' [IQRoutputPDF] or [IQRoutputPNG] depending on the encountered file ending.
#'
#' In any case, the function will return an object of class `IQRoutputFigure` that stores
#' the plots, page layout and plot dimension information that the user provided. The object,
#' a list, contains the following fields:
#'
#' * `content`: (list of) plot object(s)
#' * `title`: Common title (string) to be printed on top of page, optional.
#' * `subtitle`: Common subtitle (string) to be printed below title, optional.
#' * `footer`: Common footer (string) to be printed at bottom of page, optional.
#' * `legend`: User-provided legend (gtable object), optional
#' * `filename`: File to which the figure was printed if a filename was
#'              provided when the IQRoutputFigure function was called, optional.
#' * `opt.layout`: list with layout options (nrow, ncol, npage, FLAGlegend,
#'              legend.position, legend.relsize, title.relheight, subtitle.relheight
#'              footer.relheight). See [createPages_IQRoutputFigure]. optional
#' * `opt.pagesize`: list with page dimension/resolution options (width, height, scale,
#'               scaleWidth, scaleHeight, res). See [IQRoutputPDF] and
#'               [IQRoutputPNG]. optional
#'
#' If the input was an IQRoutputFigure, existing fields will be overwritten or
#' extended and/or non-existing fields added by arguments given. Note that
#' if a field existed and no new corresponding input was given, this field
#' is kept as is.
#'
#' @md
#'
#' @param x (list of) ggplot object(s) or IQRoutputFigure object.
#' @param title Character string with figure title
#' @param subtitle Character string with figure subtitle
#' @param footer Character string with figure footer
#' @param filename Filename with either .pdf or .png suffix
#' @param FLAGreport Flag whether figure annotation for reporting is prepared
#' @param opt.pagesize List with page size settings for plotting to pngor pdf device (see [opt.pagesize])
#' @param opt.layout List with further settings for page layout (see [opt.layout])
#' @param ... Additional arguments passed to IQRoutputPDF or IQRoutputPNG
#'
#' @return IQRoutputFigure object. (If filename given, graph is printed to file)
#' @export
#'
#' @family Output & Compliance
#'
#' @examples
#' \dontrun{
#'   
#'   grList <- lapply(1:10, function(i) {
#'      dat <- data.frame(x=rnorm(100), y = runif(100), label = rep(c("Blue", "Red")))
#'      ggplot(dat, aes(x,y,color = label)) +
#'        geom_point() +
#'        scale_color_manual(values = c("Blue" = "firebrick", "Red" = "navyblue"))
#'   })
#'   
#'   figobj <- IQRoutputFigure(
#'     x = grList,
#'     title = "10 plots with random numbers",
#'     subtitle = "plots are distributed to 3 columns and 2 rows",
#'     footer = "note the color confusion",
#'     nrow = 2, ncol = 3, filename = "Example.pdf"
#'   )
#'   
#'   names(figobj)
#'   
#'   unlink("Example.pdf")
#' }
#' @author Anne Kümmel, IntiQuan
IQRoutputFigure <- function(x = NULL,
                            title = NULL,
                            subtitle = NULL,
                            footer = NULL,
                            filename=NULL,
                            FLAGreport = FALSE,
                            opt.pagesize = list(width = 21/2.54, height = 21/2.54*3/4,
                                                scale = 1, scaleWidth=1, scaleHeight=1,
                                                res = 300),
                            opt.layout   = list(nrow = NULL, ncol = NULL, npage = NULL,
                                                legend.option = c("as.is", "remove", "first.on.page"),
                                                legend.object = NULL,
                                                legend.position = "right", legend.relsize = 0.2,
                                                title.relheight = 0.05, subtitle.relheight = 0.05, footer.relheight = 0.05),
                            ...) {
  on.exit({if (!is.null(filename)) aux_closeDevice(aux_fileparts(filename)[["fileext"]])})
  if (inherits(x, "IQRoutputFigure")) {
    object <- x
    gr <- NULL
  } else {
    object <- NULL
    gr <- x
  }
  names.opt.layout   <- names(formals("opt.layout"))
  names.opt.pagesize <- names(formals("opt.pagesize"))
  if (!all(names(opt.layout) %in% names.opt.layout)) {
    stopIQR(paste0("No valid layout option(s):", paste0(setdiff(names(opt.layout), names.opt.layout), collapse = ", ")))
  }
  if (!all(names(opt.pagesize) %in% names.opt.pagesize)) {
    stopIQR(paste0("No valid page size option(s): ", paste0(setdiff(names(opt.pagesize), names.opt.pagesize), collapse = ", ")))
  }
  inputs__ <- as.list(match.call(expand.dots = TRUE))
  inputoptions__ <- inputs__[!names(inputs__) %in% c("", "x", "title", "subtitle", "footer", "filename", "FLAGreport")]
  if ("opt.layout" %in% names(inputoptions__)) inputoptions__$opt.layout <- opt.layout 
  if ("opt.pagesize"   %in% names(inputoptions__)) inputoptions__$opt.pagesize   <- opt.pagesize 
  for (optk in intersect(names(inputoptions__), names.opt.layout)) {
    if (!"opt.layout" %in% names(inputoptions__)) inputoptions__$opt.layout <- list()
    inputoptions__$opt.layout[[optk]] <- eval(inputoptions__[[optk]])
    inputoptions__[[optk]] <- NULL
  }
  for (optk in intersect(names(inputoptions__), names.opt.pagesize)) {
    if (!"opt.pagesize" %in% names(inputoptions__)) inputoptions__$opt.pagesize <- list()
    inputoptions__$opt.pagesize[[optk]] <- eval(inputoptions__[[optk]])
    inputoptions__[[optk]] <- NULL
  }
  if ("legend.option" %in% names(inputoptions__$opt.layout)) {
    if (length(inputoptions__$opt.layout$legend.option) != 1) warningIQR("Only first element of legend.option is used.")
    if (is.numeric(inputoptions__$opt.layout$legend.option)) {
      if (!inputoptions__$opt.layout$legend.option[1] %in% 1:3) {
        stopIQR("legend.option needs to be numeric input of 1, 2, or 3, or character input of 'as.is', 'remove', or 'common'.")
      } else {
        inputoptions__$opt.layout$legend.option <- c("as.is", "remove", "common")[inputoptions__$opt.layout$legend.option]
      }
    } else {
      if (!inputoptions__$opt.layout$legend.option[1] %in% c("as.is", "remove", "common")) {
        stopIQR("legend.option needs to be numeric input of 1, 2, or 3, or character input of 'as.is', 'remove', or 'common'.")
      }
    }
  }
  argslist__ <- as.list(formals())
  argslist__ <- argslist__[!names(argslist__) %in% c("x", "title", "subtitle", "footer", "filename", "FLAGreport", "...")]
  argslist__$opt.layout <- eval(argslist__$opt.layout)
  argslist__$opt.pagesize <- eval(argslist__$opt.pagesize)
  defoptions__ <- argslist__[setdiff(names(argslist__), intersect(names(inputoptions__), c(names.opt.layout, names.opt.pagesize)))]
  if ("opt.layout" %in% names(inputoptions__)) defoptions__$opt.layout <- defoptions__$opt.layout[setdiff(names(defoptions__$opt.layout), names(inputoptions__$opt.layout))]
  if ("opt.pagesize"   %in% names(inputoptions__)) defoptions__$opt.pagesize   <- defoptions__$opt.pagesize[setdiff(names(defoptions__$opt.pagesize), names(inputoptions__$opt.pagesize))]
  if (!is.null(object)) {
    defoptions__ <- defoptions__[setdiff(names(defoptions__), c(names(object$opt.layout), names(object$opt.pagesize)))]
    if ("opt.layout" %in% names(object)) defoptions__$opt.layout <- defoptions__$opt.layout[setdiff(names(defoptions__$opt.layout), names(object$opt.layout))]
    if ("opt.pagesize"   %in% names(object)) defoptions__$opt.pagesize   <- defoptions__$opt.pagesize[setdiff(names(defoptions__$opt.pagesize), names(object$opt.pagesize))]
  }
  if (!is.null(object)) {
    output__ <- object
    if ("opt.layout" %in% names(inputoptions__)) {
      new.opt.layout <- inputoptions__$opt.layout
    } else {
      new.opt.layout <- list()
    }
    for (layopt in names(new.opt.layout)) {
      if (is.null(output__$opt.layout)) output__$opt.layout <- list()
      output__$opt.layout[[layopt]] <- new.opt.layout[[layopt]]
    }
    if ("opt.pagesize" %in% names(inputoptions__)){
      new.opt.pagesize <- eval(inputoptions__$opt.pagesize)
    } else {
      new.opt.pagesize <- list()
    }
    for (pagopt in names(new.opt.pagesize)) {
      if (is.null(output__$opt.pagesize)) output__$opt.pagesize <- list()
      output__$opt.pagesize[[pagopt]] <- new.opt.pagesize[[pagopt]]
    }
    if ("title" %in% names(inputs__)) output__$title <- title
    if ("subtitle" %in% names(inputs__)) output__$subtitle <- subtitle
    if ("footer" %in% names(inputs__)) output__$footer <- footer
    if ("filename" %in% names(inputs__)) output__$filename <- filename
  }
  if (!is.null(gr)) {
    if (!(is_plot_object(gr) | all(sapply(gr, is_plot_object))))
      stopIQR("Input gr needs to be plot object or list of these")
    if (is_plot_object(gr)) gr <- list(gr)
    output__ <- list(
      content = gr,
      title = title,
      subtitle = subtitle,
      footer = footer,
      filename = filename
    )
    output__ <- purrr::discard(output__, is.null)
    if (!is.null(attr(gr, "plotdata"))) output__$plotdata <- attr(gr, "plotdata")
    if ("opt.layout" %in% names(inputoptions__)){
      opt.layout <- eval(inputoptions__$opt.layout)
    } else {
      opt.layout <- list()
    }
    if (length(opt.layout) > 0) output__$opt.layout <- opt.layout
    if ("opt.pagesize" %in% names(inputoptions__)){
      opt.pagesize <- eval(inputoptions__$opt.pagesize)
    } else {
      opt.pagesize <- list()
    }
    if (length(opt.pagesize) > 0) output__$opt.pagesize <- opt.pagesize
    class(output__) <- c("IQRoutputFigure", class(output__))
  }
  if (ifelse("legend.option" %in% names(output__$opt.layout), output__$opt.layout$legend.option != "common", TRUE)) {
    if (!is.null(output__$opt.layout$legend.object)) warningIQR("Legend object provided, but legend option not set to 'common'. Object will be ignored when plotting.")
    if (!is.null(output__$opt.layout$legend.position)) warningIQR("Legend position provided, but legend option not set to 'common'. Setting will be ignored when plotting.")
    if (!is.null(output__$opt.layout$legend.relsize)) warningIQR("Relative legend size provided, but legend option not set to 'common'. Setting will be ignored when plotting.")
  }
  if (!is.null(filename)) {
    objectPlot__ <- output__
    if (FLAGreport) {
      anntitle__  <- paste0("Title : ", ifelse("title" %in% names(objectPlot__),objectPlot__$title,""))
      annfooter__ <- paste0("Footer : ", ifelse("footer" %in% names(objectPlot__),objectPlot__$footer,""))
      annotation <- c(anntitle__,annfooter__)
      objectPlot__$title  <- NULL
      objectPlot__$footer <- NULL
    }
    layoutargs__ <- c(list(x=objectPlot__), defoptions__$opt.layout)
    layoutargs__$legend.option <- layoutargs__$legend.option[1]
    pages__     <- do.call(createPages_IQRoutputFigure, layoutargs__)
    devargs__ <- c(objectPlot__$opt.pagesize, defoptions__$opt.pagesize) 
    for (opt__ in names(devargs__)) assign(opt__, devargs__[[opt__]])
    fileType <- dplyr::case_when(
      grepl("[.]pdf$", filename) ~ "PDF",
      grepl("[.]png$", filename) ~ "PNG",
      TRUE ~ NA_character_
    )
    if (is.na(fileType)) stopIQR("File name needs to have .pdf or .png suffix. Only PDF or PNG output is handled.")
    if (fileType == "PDF") {
      argsDot <- list(...)
      namesDotKeep <- setdiff(names(argsDot), names.opt.pagesize) 
      namesDotKeep <- intersect(namesDotKeep, names(formals("pdf"))) 
      argsDot <- argsDot[namesDotKeep]
      do.call(IQRoutputPDF, c(list(gr=pages__, filename=filename,
                                                width = width, height = height,
                                                scale = scale, scaleWidth = scaleWidth, scaleHeight = scaleHeight,
                                                nrow = 1, ncol = 1), argsDot))
      if (FLAGreport) aux_filewrite(annotation, filename = paste0(filename,".ann"))
    }
    if (fileType == "PNG") {
      if (is_plot_object(pages__)) pages__ <- list(pages__)
      if (length(pages__) > 1) {
        nformat <- paste0("%.",1+floor(log10(length(pages__))),"d")
        .fname <- paste0(aux_strrep(filename,".png",""),"_", sprintf(nformat,seq_along(pages__)), ".png")
      } else {
        .fname <- filename
      }
      purrr::map2(.fname, pages__, function(.f,.p) {
        IQRoutputPNG(.p, filename=.f,
                     width = width, height = height, res = res,
                     scale = scale, scaleWidth = scaleWidth, scaleHeight = scaleHeight, ...)
        if (FLAGreport) aux_filewrite(annotation, filename = paste0(.f,".ann"))
      })
    }
    return(invisible(output__))
  } else {
    return(output__)
  }
}
#' Print IQRoutputFigure to pages (ggplot objects)
#'
#' @param x IQRoutputFigure object
#' @param nrow number of rows per page
#' @param ncol number of columns per page
#' @param npage number of pages. If nrow and ncol is given, this input is ignored.
#' @param legend.option Character or numeric whether to leave legend in plots as is ('as.is', 1),
#'              remove all legends from the plots ('remove', 2), or plot common legend ('common', 3).
#'              As common legend, the legend of the first plot per page is used if not user provided
#'              by 'legend.object'
#' @param legend.object User-provided legend
#' @param legend.position Position of legend relative to main plots.
#'                 Can be 'right' (default), 'left', 'top', or 'bottom'.
#' @param legend.relsize Fraction of plot region (width for left or right position,
#'                 height for top or bottom position) allocated to legend.
#' @param title.relheight Fraction of page height allocated to plot title (if exists).
#' @param subtitle.relheight Fraction of page height allocated to plot subtitle (if exists).
#' @param footer.relheight Fraction of page height allocated to plot footer (if exists).
#' @param ... Additional arguments (unused)
#'
#' @export
#' @author Anne Kümmel, IntiQuan
print.IQRoutputFigure <- function(
  x,
  nrow = NULL, ncol = NULL, npage = NULL,
  legend.option = "as.is", legend.object = NULL, legend.position = "right",
  legend.relsize = 0.2,
  title.relheight = 0.05, subtitle.relheight = 0.05, footer.relheight = 0.05,
  ...
) {
  inputoptions__ <- as.list(match.call(expand.dots = TRUE))
  inputoptions__ <- inputoptions__[!names(inputoptions__) %in% c("", "x")]
  plot_pages__ <- do.call(createPages_IQRoutputFigure, c(list(x=x), inputoptions__))
  if (is_plot_object(plot_pages__)) {
    print(plot_pages__)
  } else {
    lapply(plot_pages__, print)
    npages__ <- length(plot_pages__)
    if (npages__>1) {
      cat(paste0(length(plot_pages__), " pages were printed to the graphics device."))
    }
  }
  return(invisible(NULL))
}
#' Plot function for IQRoutputFigure (calls print)
#'
#' @param x IQRoutputFigure object
#' @param ... Arguments passed to print function
#'
#' @export
#' @author Anne Kümmel, IntiQuan
plot.IQRoutputFigure <- function(x, ...) {
  print(x, ...)
}
#' Summary function for IQRoutputFigure
#'
#' Prints information on IQRoutputFigure with no return.
#'
#' @param object IQRoutputFigure object
#' @param ... Additional arguments (unused)
#'
#' @return NULL
#' @export
#' @author Anne Kümmel, IntiQuan
summary.IQRoutputFigure <- function(object, ...) {
  x <- object
  dfmain__ <- tibble::tribble(
    ~a               , ~b,
    "Title"          , ifelse(is.null(object$title),crayon::silver("- none -"),object$title),
    "Number of plots", length(object$content),
    "Subtitle"       , ifelse(is.null(object$subtitle),crayon::silver("- none -"),object$subtitle),
    "Footer"         , ifelse(is.null(object$footer),crayon::silver("- none -"),object$footer),
    "Filename"       , ifelse(is.null(object$filename),crayon::silver("- none -"),object$filename)
  )
  dfmain__$a <- format(dfmain__$a)
  dflayout__ <- NULL
  if ("opt.layout" %in% names(object)) {
    if (length(object$opt.layout)>0) {
      if ("legend.object" %in% names(object$opt.layout)) object$opt.layout$legend.object <- "Common legend provided"
      dflayout__ <- data.frame(a=names(object$opt.layout), b=sapply(object$opt.layout,c), stringsAsFactors = FALSE)
      dflayout__$a <- format(dflayout__$a)
    }
  }
  dfdev__ <- NULL
  if ("opt.pagesize" %in% names(object)) {
    if (length(object$opt.pagesize)>0) {
      dfdev__ <- data.frame(a=names(object$opt.pagesize), b=sapply(object$opt.pagesize,c), stringsAsFactors = FALSE)
      dfdev__$a <- format(dfdev__$a)
    }
  }
  cat("=== IQRoutputFigure object ===\n")
  for (k in 1:nrow(dfmain__)) cat(dfmain__$a[k], " : ", dfmain__$b[k],"\n", sep = "")
  if (!is.null(dflayout__)) {
    cat(crayon::blue("--- Defined layout settings:\n"))
    for (k in 1:nrow(dflayout__)) cat(crayon::blue(paste0(dflayout__$a[k], " : ", dflayout__$b[k],"\n")))
  } else {
    cat(crayon::silver("--- No layout settings\n"))
  }
  if (!is.null(dfdev__)) {
    cat(crayon::green("--- Defined page size settings for writing PDF or PNG:\n"))
    for (k in 1:nrow(dfdev__)) cat(crayon::green(paste0(dfdev__$a[k], " : ", dfdev__$b[k],"\n")))
  } else {
    cat(crayon::silver("--- No page size settings defined\n"))
  }
  return(invisible(NULL))
}
#' Prints summary for the IQRoutputFigures contained in the list
#'
#' @param x IQRplotMulti object
#' @param ... Additional arguments (unused)
#'
#' @return list of list of ggplot objects, one per page
#' @export
#' @author Anne Kümmel, IntiQuan
print.IQRoutputFigureList <- function(x, ...) {
  lapply(x, function(x) summary.IQRoutputFigure(x))
}
#' Convenience function for setting additional
#' page printing options (opt.pagesize) for IQRoutputFigure
#'
#' @md
#'
#' @param width Page with in inches
#' @param height Page height in inches
#' @param scale Relative factor for scaling of both width and height
#' @param scaleWidth Relative factor for scaling width
#' @param scaleHeight Relative factor for scaling height
#' @param res Resolution in DPI (applies for PNG output)
#'
#' @return Returns a opt.pagesize list for IQRoutputFigure
#' @export
#'
#' @author Anne Kuemmel, IntiQuan
#'
opt.pagesize <- function(width = NULL, height = NULL,
                         scale = NULL,
                         scaleWidth  = NULL, scaleHeight = NULL,
                         res         = NULL) {
  out <- list()
  out$width       <- width
  out$height      <- height
  out$scale       <- scale
  out$scaleWidth  <- scaleWidth
  out$scaleHeight <- scaleHeight
  out$res         <- res
  return(out)
}
#' Convenience function for setting additional
#' page layout options (opt.layout) for IQRoutputFigure
#'
#' @md
#'
#' @param nrow Number of rows per page
#' @param ncol Number of columns per page
#' @param npage Number of pages (ignored if both nrow and ncol are given)
#' @param legend.option Character or numeric whether to leave legend in plots as is ('as.is', 1),
#'              remove all legends from the plots ('remove', 2), or plot common legend ('common', 3).
#'              As common legend, the legend of the first plot per page is used if not user provided
#'              by 'legend.object'
#' @param legend.object User-provided legend
#' @param legend.position Position of legend relative to main plots.
#'                 Can be 'right' (default), 'left', 'top', or 'bottom'.
#' @param legend.relsize Fraction of plot region (width for left or right position,
#'                 height for top or bottom position) allocated to legend.
#' @param title.relheight Fraction of page height allocated to plot title (if exists).
#' @param subtitle.relheight Fraction of page height allocated to plot subtitle (if exists).
#' @param footer.relheight Fraction of page height allocated to plot footer (if exists).
#'
#' @return Returns a opt.layout list for IQRoutputFigure
#' @export
#'
#' @author Anne Kuemmel, IntiQuan
opt.layout <- function(nrow               = NULL,
                       ncol               = NULL,
                       npage              = NULL,
                       legend.option      = NULL,
                       legend.object      = NULL,
                       legend.position    = NULL,
                       legend.relsize     = NULL,
                       title.relheight    = NULL,
                       subtitle.relheight = NULL,
                       footer.relheight   = NULL) {
  out <- list()
  out$nrow               = nrow
  out$ncol               = ncol
  out$npage              = npage
  out$legend.option      = legend.option
  out$legend.object      = legend.object
  out$legend.position    = legend.position
  out$legend.relsize     = legend.relsize
  out$title.relheight    = title.relheight
  out$subtitle.relheight = subtitle.relheight
  out$footer.relheight   = footer.relheight
  return(out)
}
#' Plotting a list of ggplot objects in a grid defined by nrow and ncol
#'
#' Useful when having a lot of plots that should be presented in a page saving manner.
#'
#' @param plotList List of ggplot objectsx
#' @param nrow Number of rows per page
#' @param ncol Number of cols per page
#' @export
printGrid <- function (plotList,nrow=4,ncol=4) {
  if (length(plotList)==0) return(invisible(NULL))
  x <- which(!sapply(plotList, is.null))
  plotList <- plotList[x]
  pieces__ <- aux_splitVectorEqualPieces(x = 1:length(plotList), nrow*ncol)
  for (k__ in seq_along(pieces__)) {
    pO__ <- cowplot::plot_grid(plotlist=plotList[pieces__[[k__]]],nrow=nrow,ncol=ncol)
    print(pO__)
  }
}
#' Export general data.frame object as XPT file
#'
#' A data.frame object can be exported to a SAS XPT file.
#' Additionally, labels can be provided with the addColLabels input argument.
#'
#' @param data data.frame to export as XPT
#' @param filename Filename of the XPT file to create (not more than 8 characters without extension)
#' @param addColLabels List with named elements. Names are column names and
#'   the value of an element is the label. If defining addColLabels for columns that
#'   are already handled automatically, the provided labels will overwrite the automatic ones.
#'   Example: \code{addColLabels <- list(IXGDF="Other label",NEWCCOL="Label for NEWCOL")}
#' @family General Data I/O
#' @family Output & Compliance
#' @export
IQRoutputXPT <- function(data, filename = NULL, addColLabels = NULL) {
  if (is.null(filename)) {
    stopIQR("filename must be provided")
  }
  if (!is.null(addColLabels)) {
    for (k in seq_along(addColLabels)) {
      data <- addLabel(data = data,colName = names(addColLabels)[k],label = addColLabels[[k]])
    }
  }
  x <- aux_fileparts(filename)
  filename <- x$filename
  pathname <- x$pathname
  if (nchar(filename) > 8) {
    stopIQR("please provide a filename with length of max 8 characters (w/o extension)")
  }
  aux_mkdir(pathname)
  haven::write_xpt(
    data = data,
    path = file.path(pathname, paste0(filename, ".xpt")),
    name = filename,
    version = 5
  )
  genComplianceLog(file.path(pathname, paste0(filename, ".xpt")))
  return(invisible(NULL))
}
#' Simple auxiliary to load a CSV dataset
#'
#' If a .atr file is present, this will be loaded as well.
#' A .atr is present if the csv file has been created using the
#' export function from the IQRdataGENERAL object. Loading the .atr
#' information will give metadata as attributes about covariates etc.
#'
#' na.strings=c("."," ","","NA","NaN"),stringsAsFactors=FALSE
#'
#' @param filename Path/filename to the raw or zipped csv dataset. Supports files ending with .csv or .csv.gz.
#' See [data.table::fread()] for details.
#' @return A data.frame with the contents of the CSV file
#' @family General Data I/O
#' @export
#' @author Henning Schmidt, Daniel Kaschek, Daniel Lill, IntiQuan
#' @md
#' @importFrom data.table fread
IQRloadCSVdata <- function(filename) {
  is_zip__ <- grepl("\\.csv\\.zip$", filename, ignore.case = TRUE)
  if (is_zip__) {
    outdir__ <- file.path(tempdirIQR(), "CSV")
    unlink(outdir__, recursive = TRUE)
    utils::unzip(filename, exdir = outdir__)
    myfiles__ <- list.files(outdir__, full.names = TRUE)
    if (length(myfiles__) == 0) stopIQR("Zip archive did not contain files.")
    if (length(myfiles__) > 1) stopIQR("Automatic CSV reading from zip file with multiple files is not supported.")
    if (!grepl("\\.csv$", myfiles__)) stopIQR("The zip file did not contain a csv file.")
    data__ <- as.data.frame(data.table::fread(myfiles__, na.strings=c(".","", "NA","NaN"), strip.white = TRUE,showProgress=FALSE))
    unlink(outdir__, recursive = TRUE)
    filenameATR__ <- paste0(aux_strrep(filename, ".csv.zip",""),".atr")
  } else {
    data__ <- as.data.frame(data.table::fread(filename,
                                              na.strings=c(".","", "NA","NaN"),
                                              strip.white = TRUE))
    filenameATR__ <- gsub('\\.csv(.gz)?$','.atr', filename)
  }
  atrcontents <- loadAttributeFile(filenameATR__)
  attributes(data__) <- c(attributes(data__),atrcontents)
  return(data__)
}
loadAttributeFile <- function(filenameATR) {
  if (!file.exists(filenameATR)) return(NULL)
  atrContent__ <- aux_fileread(filenameATR)
  var0 <- ls() 
  eval(parse(text=atrContent__))
  var1 <- ls()
  if (!exists("atrcontents")) {
    atrcontents <- list()
    attrnames__ <- setdiff(var1, c(var0, "var0"))
    atrcontents <- plyr::alply(attrnames__, 1, function(x__) get(x__))
    names(atrcontents) <- attrnames__
    attr(atrcontents, "split_type") <- NULL
    attr(atrcontents, "split_labels") <- NULL
  }
  out__ <- atrcontents
  return(out__)
}
#' Simple auxiliary to save a CSV dataset with attributes
#'
#' If non-standard attributes are present in the data.frame object,
#' these are stored as a .atr file in addition to the .csv file.
#' Loading with IQRloadCSVdata will restore the full object with attributes
#' NA is stored as ".". Compliance mode not supported with this function.
#'
#' @param filename Path/filename to the csv dataset.
#' If file ends with .csv.gz, the csv will be zipped by [data.table::fwrite()]
#' @param data A data.frame with the contents of the CSV file
#' @param na Character to represent NA values
#' @param quote TRUE: use "" quotes, FALSE: do not (for NONMEM and MONOLIX)
#' @param row.names TRUE: export row names FALSE: do not (typical use in IQnca)
#' @param FLAGattributes FALSE: do not export custom attributes as .atr file. TRUE: do (default)
#' @param replaceComma NULL: do not replace comma. Character: replace by user-provided character
#' @family General Data I/O
#' @export
#'
#' @md
#'
#' @author Henning Schmidt, Daniel Kaschek, Daniel Lill, IntiQuan
#' @importFrom data.table fwrite
#'
IQRsaveCSVdata <- function(data,filename,na=".",quote=FALSE,row.names=FALSE,FLAGattributes=TRUE,replaceComma=NULL) {
  if (is.null(filename)) return()
  aux_mkdir(aux_fileparts(filename)$pathname)
  containsComma__ <- unlist(lapply(data, function(x__) {
    if (is.character(x__)||is.factor(x__)){
      return(any(grepl(",", x__)))
    } else {
      return(FALSE)
    }
  }))
  if (!is.null(replaceComma) & any(containsComma__)) {
    for (i__ in which(containsComma__)) data[[i__]] <- gsub(",", replaceComma, data[[i__]])
  }
  if (is.null(replaceComma) & any(containsComma__)) {
    stopIQR("Column(s) ", paste(names(containsComma__)[containsComma__], collapse = ", "),
            " contain(s) comma. Cannot export as csv.\n",
            "  Use argument 'replaceComma' to define a replacement character.")
  }
  if (!grepl(".csv",filename)) filename <- paste0(filename,".csv")
  data.table::fwrite(x=data, file=filename, na=na, quote=quote, row.names=row.names)
  if (FLAGattributes) {
    attrStandard__ <- c("names","row.names","class", ".internal.selfref") 
    attr__ <- attributes(data)
    for (k in seq_along(attrStandard__)) {
      attr__[[attrStandard__[[k]]]] <- NULL
    }
    if (length(attr__) == 0) return(invisible(NULL))
    ATTRTEXT__ <- paste0("# Attributes file for dataset ",filename,"\n\n")
    ATTRTEXT__ <- paste0(ATTRTEXT__,"atrcontents <- list()\n\n")
    for (k in seq_along(attr__)) {
      n__ <- names(attr__[k])
      v__ <- attr__[[k]]
      ATTRTEXT__ <- paste0(ATTRTEXT__,"atrcontents$",n__," <- ",paste0(deparse(v__),collapse=""),"\n\n")
    }
    filenameATR <- gsub('\\.csv(.gz)?$','.atr', filename)
    aux_filewrite(ATTRTEXT__,filenameATR)
  }
}
#' Loading SAS data
#'
#' Using the haven package to load SAS data:
#' Reading supports both sas7bdat and xpt files.
#'
#' Some arguments from haven::read_sas are disabled on purpose as
#' there are till bugs with them ... for now only use it for importing SAS files :)
#'
#' Removing "," in character columns by default (can be switched off)
#'
#' @param data_file Path to sas7bdat data file.
#' @param as.data.frame Logical. If TRUE a data.frame is returned.
#' @param noLabels Logical. If TRUE label attributes are removed
#' @param replaceComma If TRUE the "," is exchanged for " " - otherwise not
#' @return data frame
#'   Variable labels are stored in the "label" attribute of each variable. It is
#'   not printed on the console, but the RStudio viewer will show it.
#' @family General Data I/O
#' @export
IQRloadSASdata <- function(data_file,as.data.frame=TRUE,noLabels=TRUE,replaceComma=TRUE) {
  if (grepl(".xpt",tolower(data_file),fixed = TRUE)){
    data__ <- haven::read_xpt(data_file)
  } else {
    if (grepl(".sas7bdat",tolower(data_file),fixed = TRUE)){
      data__ <- haven::read_sas(data_file)
    } else {
      stopIQR("Unknown data file extension")
    }
  }
  if (as.data.frame) data__ <- as.data.frame(data__)
  if (noLabels) {
    data__ <- unlabel_dataframe(data__)
  }
  flagmessage <- TRUE
  if (replaceComma) {
    for (k in seq_along(data__)) {
      col <- data__[[k]]
      if (is.character(col)) {
        newcol <- gsub(","," ",col)
        if (any(newcol!=col) & flagmessage) {
          message("Commata in character elements removed from the data during import and replaced by space (' ')")
          flagmessage <- FALSE
        }
      } else {
        newcol <- col
      }
      data__[[k]] <- newcol
    }
  }
  return(data__)
}
#' Remove labelled class and label
#'
#' @param data data.frame with potentially labeled columns
#' @param removeLabelledClass TRUE removes the class "labelled"
#' @param removeLabel TRUE removes the label attribute
#' @return data.frame with Unlabelled cols
#' @export
unlabel_dataframe <- function(data,removeLabelledClass=TRUE,removeLabel=FALSE) {
  ddddd__ <- sapply(names(data), function (name) {
    if (removeLabel) attr(data[[name]],"label") <<- NULL
    if (removeLabelledClass) class(data[[name]]) <<- setdiff(class(data[[name]]),"labelled")
  })
  return(data)
}
#' Remove commata in elements in data.frame
#'
#' @param data data.frame with potentially commata in elements
#' @param replaceComma Character to replace commata with
#' @return data.frame without commata in elements
#' @export
removeCommata_dataframe <- function(data,replaceComma=" ") {
  ddddd__ <- sapply(names(data), function (name) {
    if (!is.numeric(data[[name]])) data[[name]] <<- gsub(",",replaceComma,data[[name]])
  })
  return(data)
}
#' Extract label information of columns in data.frames
#'
#' Useful if data has been imported e.g. using IQRloadSASdata.
#' By default an IQRoutputTable object is returned for easier reading or reporting.
#' Alternatively a data.frame can be returned. Sorting is possible if desired.
#'
#' @param data data.frame
#' @param orderAlphabetically If TRUE then output ordered alphabetically by column names
#' @param table If TRUE then an IQRoutputTable is returned. Otherwise a data.frame
#' @export
getLabels_dataframe <- function (data,orderAlphabetically=FALSE,table=TRUE) {
  out <- do.call(rbind,lapply(names(data), function (name) {
    x <- attr(data[[name]],"label")
    if (is.null(x)) x <- ""
    data.frame(
      COLNAME = name,
      COLLABEL = x,
      stringsAsFactors = FALSE
    )
  }))
  if (orderAlphabetically) out <- dplyr::arrange(out,COLNAME)
  if (table) {
    out <- IQRoutputTable(xtable = out,xtitle="Content information of data.frame")
  }
  out
}
#' Conversion of date/time in string format to numeric values
#'
#' @param dateString Vector of dates/times in string format
#' @param format Format of the date/time information, defined as the format for as.POSIXxt()
#' @return List with the elements time_seconds, time_minutes, time_hours, time_days, time_weeks, time_years
#' @export
date2time_IQRdataProgramming <- function (dateString,format="%Y-%m-%dT%H:%M") {
  time_seconds <- as.numeric(as.POSIXct(dateString,format=format))
  time_minutes <- time_seconds/60
  time_hours <- time_minutes/60
  time_days <- time_hours/24
  time_weeks <- time_days/7
  time_years <- time_days/365
  out <- list(
    time_seconds = time_seconds,
    time_minutes = time_minutes,
    time_hours = time_hours,
    time_days = time_days,
    time_weeks = time_weeks,
    time_years = time_years
  )
  out
}
#' Conversion of date/time in string format to day in string format
#'
#' @param dateString Vector of dates/times in string format
#' @param format Format of the date/time information, defined as the format for as.POSIXxt()
#' @param formatday Format for the day date string, defined as format for as.POSIXct()
#' @return Vector of date strings
#' @export
date2dateday_IQRdataProgramming <- function (dateString,format="%Y-%m-%dT%H:%M",formatday="%Y-%m-%d") {
  z <- as.POSIXct(dateString,format=format)
  out <- format(z,formatday)
  out
}
#' Conversion of date/time in string format to time in string format
#'
#' @param dateString Vector of dates/times in string format
#' @param format Format of the date/time information, defined as the format for as.POSIXxt()
#' @param formattime Format for the time string, defined as format for as.POSIXct()
#' @return Vector of time strings
#' @export
date2datetime_IQRdataProgramming <- function (dateString,format="%Y-%m-%dT%H:%M",formattime="%H:%M") {
  z <- as.POSIXct(dateString,format=format)
  out <- format(z,formattime)
  out
}
#' Generate individual PK concentration figures for reporting
#'
#' Individuals are split to different pages. Lines are per default colored by PROFILE and panels for each dose group generated.
#' Underlying, the function `figure_lines_IQdataNCA` is used.
#'
#' Ignored records (IGNORER) are removed from these plots. IGNORSUM and IGNORNCA records are included.
#' Ignored subjects (IGNOREI) are included. In addition to a PDF file with the figures a folder containing
#' amongst other meta information an rmd file will be generated. The RMD file is for seamless reporting in Word with IQReport.
#'
#' @param data IQdataNCA object
#' @param lines_by grouping used for linetypes
#' @param panel_by variable to create panels by
#' @param time Character string to defined the time information used ("asis", "nominal", "actual").
#' "asis" will use the selected time information in the dataset (TIME or TAFD). "nominal" will use NTIME or NTAFD.
#' "actual" will use ATIME or ATAFD.
#' @param use_TAFD logical. FALSE (default) uses TIME, NTIME, or ATIME, depending on 'time' settings. TRUE will use
#' TAFD, NTAFD, or ATAFD.
#' @param figure_number Character string with figure number information, added to the title for each figure, if defined
#' @param figure_head Column names of covariates unique within pages to be displayed above the table
#' @param labels names character vector with labels for columns
#' @param yscale character identifying whether to plot linear ("lin")
#'        or semilogarithmic ("log") or both views ("linlog")
#' @param legend_pos specifying legend position ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param sameXaxis Flag whether to use same x axis scale across pages (splits). Defaults to TRUE.
#' @param sameYaxis Flag whether to use same y axis scale across pages (splits). Defaults to TRUE.
#' @param filename Name of PDF file to generate with plots
#' @param ... Arguments to [IQRoutputFigure] to adjust plotting settings
#'
#' @return Returns a list of all plots if filename is NULL
#' @export
#' @family TLF customized
figure_indivConc_IQdataNCA <- function(
    data,
    lines_by      = "PROFILE",
    panel_by      = "DOSE",
    time          = "asis", 
    use_TAFD      = FALSE, 
    figure_number = NULL,
    figure_head   = c("SEX", "AGE", "RACE"),
    labels        = NULL,
    yscale        = "linlog",
    legend_pos    = "bottom",
    sameXaxis     = FALSE,
    sameYaxis     = FALSE,
    filename      = NULL,
    ...
) {
  figure_lines_IQdataNCA(
    data          = data,
    figure_number = figure_number,
    figure_head   = figure_head,
    split_by      = "USUBJID",
    lines_by      = lines_by,
    panel_by      = panel_by,
    labels        = labels,
    yscale        = yscale,
    legend_pos    = legend_pos,
    time          = time,
    use_TAFD      = use_TAFD,
    dosenorm      = FALSE,
    sameXaxis     = sameXaxis,
    sameYaxis     = sameYaxis,
    filename      = filename,
    type          = "indiv_conc_figure",
    ...
  )
}
#' Generate line figures of PK concentration for reporting
#'
#' Lines connect data for each profile within one individual. Lines can be color-stratified and distributed to different panels.
#'
#' Ignored records (IGNORER) are removed from these plots. IGNORSUM and IGNORNCA records are included.
#' Ignored subjects (IGNOREI) are included. In addition to a PDF file with the figures a folder containing
#' amongst other meta information an rmd file will be generated. The RMD file is for seamless reporting in Word with IQReport.
#'
#'
#' @param data IQdataNCA object
#' @param split_by Column to split figures to separate pages.
#' @param lines_by grouping used for linetypes
#' @param panel_by variable to create panels by
#' @param time Character string to defined the time information used ("asis", "nominal", "actual").
#' "asis" will use the selected time information in the dataset (TIME or TAFD). "nominal" will use NTIME or NTAFD.
#' "actual" will use ATIME or ATAFD.
#' @param use_TAFD logical. FALSE (default) uses TIME, NTIME, or ATIME, depending on 'time' settings. TRUE will use
#' TAFD, NTAFD, or ATAFD.
#' @param figure_number Character string with figure number information, added to the title for each figure, if defined
#' @param figure_head Column names of covariates unique within pages to be displayed above the table
#' @param labels names character vector with labels for columns
#' @param yscale character identifying whether to plot linear ("lin")
#'        or semilogarithmic ("log") or both views ("linlog")
#' @param dosenorm logical whether to plot dose normalized concentrations
#' @param legend_pos specifying legend position ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param sameXaxis Flag whether to use same x axis scale across pages (splits). Defaults to TRUE.
#' @param sameYaxis Flag whether to use same y axis scale across pages (splits). Defaults to TRUE.
#' @param filename Name of PDF file to generate with plots
#' @param type Type of figure (meta data)
#' @param ... Arguments to [IQRoutputFigure] to adjust plotting settings
#'
#' @return Returns a list of all plots if filename is NULL
#' @export
#' @family TLF customized
figure_lines_IQdataNCA <- function(
    data,
    split_by      = "PROFILE",
    lines_by      = "GROUP",
    panel_by      = "STUDYID",
    dosenorm      = FALSE,
    time          = "asis",
    use_TAFD        = TRUE,
    figure_number = NULL,
    figure_head   = NULL,
    labels        = NULL,
    yscale        = c("linlog", "lin", "log"),
    legend_pos    = "bottom",
    sameXaxis     = TRUE,
    sameYaxis     = TRUE,
    filename      = NULL,
    type          = "overlay_figure",
    ...
) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  time   <- match.arg(time, c("asis","nominal","actual"))
  yscale <- match.arg(yscale, c("linlog", "lin", "log"))
  lloq <- unique(data$LLOQ)
  if (length(lloq) > 1) stopIQR("LLOQ not unique accross subjects. Only first occurring value is considered.")
  if (any(is.na(lloq))) warningIQR("NA values occurring for LLOQ.")
  if (!(is.character(lines_by) & length(lines_by) == 1)) stopIQR("'lines_by' needs to be character indicating single column.")
  if (!(is.character(panel_by) & length(panel_by) == 1)) stopIQR("'panel_by' needs to be character indicating single column.")
  if (!all(split_by %in% names(data))) stopIQR(paste0("Unknown column specified as 'split_by': ", paste0(setdiff(split_by, names(data)), collapse = ", ")))
  if (!all(lines_by %in% names(data))) stopIQR(paste0("Unknown column specified as 'lines_by': ", paste0(setdiff(lines_by, names(data)), collapse = ", ")))
  if (!all(panel_by %in% names(data))) stopIQR(paste0("Unknown column specified as 'panel_by': ", paste0(setdiff(panel_by, names(data)), collapse = ", ")))
  message("Generating PK concentration figures individual ...")
  d <- removeFrom_IQdataNCA(data = data,
                                    FLAGremoveIGNOREI = FALSE,
                                    FLAGremoveIGNORER = TRUE,
                                    FLAGremoveIGNORSUM = FALSE,
                                    FLAGremoveIGNORNCA = FALSE)
  d$IGNORE <- dplyr::case_when(
    !is.na(d$IGNOREI) ~ "Outlier-ignored in NCA and summary figures",
    !is.na(d$IGNORNCA) & !is.na(d$IGNORSUM)  ~ "Outlier-ignored in NCA and summary figures",
    !is.na(d$IGNORNCA) ~ "Outlier-ignored in NCA",
    !is.na(d$IGNORSUM) ~ "Outlier-ignored in summary figures",
    TRUE ~ "Regular")
  res <- handleTIMEplot_IQdataNCA(d = d, FLAGTIME = time, USETAD = !use_TAFD)
  d <- res$d
  xlabtext <- res$xlabtext
  d <- d[!is.na(d$CONCPLIN) & !is.na(d$CONCPLOG),]
  d$CONCPLOG[d$CONCPLOG==0] <- NA
  d <- d[!is.na(d$TIMEPLOT),]
  title <- switch(
    type,
    indiv_conc_figure = .figure_individual_pkconc,
    overlay_figure = .figure_spaghetti_pkconc
  )
  title <- updateFigureNumberTitle_IQdataNCA(title,figure_number,figureindex = NULL)
  if (d$FGBQPLIN[1] == d$FGBQPLOG[1] | yscale %in% c("lin", "log")){
    fgbqp <- ifelse(grepl("lin", yscale), d$FGBQPLIN[1], d$FGBQPLOG[1])
    footer <- figure_footer(fgbqp)
  } else {
    footer <- paste0(
      "Linear scale: ", figure_footer(d$FGBQPLIN[1]), "\n",
      "Log scale: ", figure_footer(d$FGBQPLOG[1])
    )
  }
  if (grepl("log", yscale)) {
    footer <- paste0(footer, " Zero values not shown on log-transformed axis.")
  }
  if (any(d$IGNORE %in% "Outlier-ignored in NCA and summary figures"))
    footer <- paste0(footer, "\n* indicates observation ignored in NCA and summaries.")
  if (any(d$IGNORE %in% "Outlier-ignored in NCA"))
    footer <- paste0(footer, "\nx indicates observation ignored in NCA.")
  if (any(d$IGNORE %in% "Outlier-ignored in summary figures"))
    footer <- paste0(footer, "\nx indicates observation ignored in summaries")
  if (dosenorm) {
    d$CONCPLOG <- d$CONCPLOG/d$DOSE
    d$CONCPLIN <- d$CONCPLIN/d$DOSE
    CONCUNITDN <- paste0(d$CONCUNIT[1],"/",d$DOSEUNIT[1])
  }
  deflabels <- get_default_labels(d)
  mylabels  <- c(deflabels[setdiff(names(deflabels), names(labels))], labels)
  ylimlin <- NULL
  ylimlog <- NULL
  if (sameYaxis) {
    ylimlin <- c(0, max(d$CONCPLIN)*1.25)
    ylimlog <- c(min(c(d$CONCPLOG, data$LLOQ[1]))/2, max(d$CONCPLOG)*1.25)
  }
  ylabtext <- paste0("Analyte concentration [", data$CONCUNIT[1], "]")
  if (dosenorm) yaxistext <- paste0("Dose-normalized analyte concentration [",CONCUNITDN,"]")
  xlim <- NULL
  if (sameXaxis) {
    xlim <- c(min(d$TIMEPLOT),max(d$TIMEPLOT))
  }
  if (!is.null(split_by)){
    dS <- split(d, d[split_by])
  } else {
    dS <- list(d)
  }
  if (!is.null(figure_head)){
    check_common <- sapply(dS, function(s) {
      nrow(unique(as.data.frame(s)[,figure_head, drop = FALSE])) == 1
    })
    if (any(!check_common)) {
      stopIQR("Values to be displayed as subtitle ('figure_head') not unique for the split used.")
    }
  }
  allplots <- lapply(seq_along(dS), function (k) {
    plot_lines_IQdataNCA(
      d          = dS[[k]],
      lines_by   = lines_by,
      panel_by   = panel_by,
      figure_head = figure_head,
      labels     = mylabels,
      yscale     = yscale,
      dosenorm   = dosenorm,
      legend_pos = legend_pos,
      xlabtext   = xlabtext,
      xlim       = xlim,
      ylabtext   = ylabtext,
      ylimlin    = ylimlin,
      ylimlog    = ylimlog
    )
  })
  figs <- list(
    content  = allplots,
    title    = title,
    filename = filename,
    number   = figure_number,
    footer   = footer,
    data     = dS,
    type     = type,
    code     = "pending"
  )
  class(figs) <- c("IQncaFigure", class(figs))
  if (!is.null(filename)) {
    write_IQncaFigure(figs, filename,...)
  } else {
    return(figs)
  }
}
#' Plot individual concentration data of IQnca data object
#'
#' @param d IQncaData object with the subset of data to be plotted here
#' @param figure_head Column names of covariates unique within pages to be displayed above the table
#' @param lines_by grouping used for linetypes
#' @param panel_by variable to create panels by
#' @param labels names character vector with labels for columns
#' @param yscale character identifying whether to plot linear ("lin")
#'        or semilogarithmic ("log") or both views ("linlog")
#' @param dosenorm logical whether to plot dose normalized concentrations
#' @param legend_pos specifying legend position
#' @param xlabtext label of x axis (time)
#' @param xlim limits of x axis (time) to be applied to all panels/pages
#' @param ylabtext Label for y axis
#' @param ylimlin Limits to be applied to linear y axis. If NULL, no scale enforced.
#' @param ylimlog Limits to be applied to logarithmic y axis. If NULL, no scale enforced.
#'
#' @return individual line plots of concentrations
plot_lines_IQdataNCA <- function(
    d,
    figure_head,
    lines_by,
    panel_by,
    labels,
    yscale = "linlog",
    dosenorm = FALSE,
    legend_pos,
    xlabtext,
    xlim,
    ylabtext,
    ylimlin,
    ylimlog
) {
  d$PANELS  <- d[[panel_by]] 
  d$PANELS  <- factor(d$PANELS, levels = unique(d$PANELS[order(nchar(d$PANELS), d$PANELS)] ))
  panel_name <- labels[panel_by]
  if (is.na(panel_name)) panel_name <- panel_by
  d$LINES <- d[[lines_by]] 
  d$LINES <- factor(d$LINES, levels = unique(d$LINES[order(nchar(d$LINES), d$LINES)] ))
  lines_name <- labels[lines_by]
  if (is.na(lines_name)) lines_name <- lines_by
  panellabels  <- paste(panel_name, ": ",unique(d$PANELS),sep="")
  names(panellabels) <- unique(d$PANELS)
  panellabels <- as_labeller(panellabels)
  plottitle <- lapply(figure_head, function(cc) {
    d[[cc]][1]
  })
  names(plottitle) <- figure_head
  labeled_figure_head <- intersect(figure_head, names(labels))
  names(plottitle)[match(labeled_figure_head, names(plottitle))] <- labels[labeled_figure_head]
  plottitle <- unlist(lapply(seq_along(plottitle), function(p)  {
    paste0(names(plottitle)[p], ": ", plottitle[p] )
  }))
  plottitle <- paste(plottitle, collapse=" \n")
  values_shapes <- c("Outlier-ignored in NCA and summary figures" = 8,
                     "Outlier-ignored in NCA" = 4,
                     "Outlier-ignored in summary figures" = 3,
                     "Regular" = 19)
  plotlin <- IQRggplot(data = d[!is.na(d$CONCPLIN),], aes(x=TIME, y=CONCPLIN, shape = IGNORE)) +
    geom_line(aes(linetype = LINES, group = interaction(USUBJID, PROFILE, LINES))) +
    geom_point(aes(shape = IGNORE)) +
    scale_shape_manual(values = values_shapes,
                       guide = 'none') +
    labs(x = xlabtext,
         y = ylabtext,
         linetype = lines_name)+
    labs(subtitle ="Linear view" ) +
    theme(legend.position=legend_pos,
          title = element_text(size=10)) +
    scale_x_continuous()+
    facet_wrap(~PANELS, labeller = panellabels)
  if (!dosenorm) {
    plotlin <- plotlin +
      geom_hline(mapping = aes(yintercept = unique(d$LLOQ)),
                 colour="orange",linetype="dotted") +
      geom_text(aes(max(d$NTIME) ,unique(d$LLOQ),label = "LLOQ", vjust = -1, hjust=1), colour="orange")
  }
  plotlog <- IQRggplot(data = d[!is.na(d$CONCPLOG),], aes(x=TIME, y=CONCPLOG)) +
    geom_line(aes(linetype = LINES, group = interaction(USUBJID, PROFILE, LINES))) +
    geom_point(aes(shape = IGNORE)) +
    scale_shape_manual(values = values_shapes,
                       guide = 'none') +
    labs(x=xlabtext,
         y=ylabtext,
         linetype = lines_name)+
    labs(subtitle ="Semilogarithmic view" ) +
    theme(legend.position = legend_pos,
          title = element_text(size=10)) +
    scale_x_continuous() +
    scale_y_log10_IQnca() +
    facet_wrap(~PANELS, labeller = panellabels)
  if (!dosenorm) {
    plotlog <- plotlog +
      geom_hline(mapping = aes(yintercept = unique(d$LLOQ)),
                 colour="orange",linetype="dotted") +
      geom_text(aes(max(d$NTIME) ,unique(d$LLOQ),label = "LLOQ", vjust = -1, hjust=1), colour="orange")
  }
  if (!is.null(xlim)) {
    suppressMessages(plotlog <- plotlog + coord_cartesian(xlim = xlim))
    suppressMessages(plotlin <- plotlin + coord_cartesian(xlim = xlim))
  }
  if (!is.null(ylimlin)) suppressMessages(plotlin <- plotlin + coord_cartesian(ylim = ylimlin))
  if (!is.null(ylimlog)) suppressMessages(plotlog <- plotlog + coord_cartesian(ylim = ylimlog))
  figure <- switch(
    yscale,
    lin = plotlin,
    log = plotlog,
    linlog = ggpubr::ggarrange(plotlin, plotlog,
                               common.legend = TRUE,
                               legend=legend_pos,
                               ncol = 1, nrow = 2 )
  )
  figure <- ggpubr::annotate_figure(figure,
                                    top = ggpubr::text_grob(plottitle,
                                                            color = "black"))
  figure
}
#' Generate summary figures of PK concentration for reporting
#'
#' An average and a variability statistic are calculated for the stratification defined by the
#' split to pages (`split_by`), linetype stratification (`lines_by`), and facetting to different panels (`panel_by`).
#' The average and variability can be defined by the user:
#'
#' | Input         | Keyword | Description |
#' | ------------- | ------- | ----------- |
#' | statistic_avg | MEAN    | Arithmetic mean |
#' | statistic_avg | GMEAN   | Geometric mean |
#' | statistic_avg | MEDIAN  | Median |
#' | statistic_var | SD      | Standard deviation (arithmetic) |
#' | statistic_var | GSD     | Standard deviation (geometric) |
#' | statistic_var | PI90    | 90%-interval |
#' | statistic_var | PI95    | 95%-interval |
#'
#'
#' Ignored records (IGNORER) are removed from these plots. IGNORSUM and IGNORNCA records are included.
#' Ignored subjects (IGNOREI) are included. In addition to a PDF file with the figures a folder containing
#' amongst other meta information an rmd file will be generated. The RMD file is for seamless reporting in Word with IQReport.
#'
#'
#' @param data IQdataNCA object
#' @param statistic_avg Statistic to be used to present 'average' value. MEAN, GMEAN, or MEDIAN. See description.
#' @param statistic_var Statistic to be used to present 'variablity' of values. SD, GSD, PI90, or PI95. See description.
#' @param split_by Column to split figures to separate pages.
#' @param lines_by grouping used for linetypes
#' @param panel_by variable to create panels by
#' @param use_TAFD logical. FALSE (default) uses TIME, NTIME, or ATIME, depending on 'time' settings. TRUE will use
#' TAFD, NTAFD, or ATAFD.
#' @param figure_number Character string with figure number information, added to the title for each figure, if defined
#' @param figure_head Column names of covariates unique within pages to be displayed above the table
#' @param labels names character vector with labels for columns
#' @param yscale character identifying whether to plot linear ("lin")
#'        or semilogarithmic ("log") or both views ("linlog")
#' @param dosenorm logical whether to plot dose normalized concentrations
#' @param legend_pos specifying legend position ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param sameXaxis Flag whether to use same x axis scale across pages (splits). Defaults to TRUE.
#' @param sameYaxis Flag whether to use same y axis scale across pages (splits). Defaults to TRUE.
#' @param filename Name of PDF file to generate with plots
#' @param type Type of figure (meta data)
#' @param ... Arguments to [IQRoutputFigure] to adjust plotting settings
#'
#' @return Returns a list of all plots if filename is NULL
#' @export
#' @family TLF customized
figure_summary_IQdataNCA <- function(
    data,
    statistic_avg = c("MEAN", "GMEAN", "MEDIAN"),
    statistic_var = c("SD", "GSD", "PI90", "PI95"),
    split_by      = "PROFILE",
    lines_by      = "GROUP",
    panel_by      = "STUDYID",
    dosenorm      = FALSE,
    use_TAFD      = FALSE,
    figure_number = NULL,
    figure_head   = NULL,
    labels        = NULL,
    yscale        = "linlog",
    legend_pos    = "bottom",
    sameXaxis     = TRUE,
    sameYaxis     = TRUE,
    filename      = NULL,
    type          = "figure_summary",
    ...
) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  lloq <- unique(data$LLOQ)
  if (length(lloq) > 1) stopIQR("LLOQ not unique accross subjects. Only first occurring value is considered.")
  if (any(is.na(lloq))) warningIQR("NA values occurring for LLOQ.")
  if (!(is.character(lines_by) & length(lines_by) == 1)) stopIQR("'lines_by' needs to be character indicating single column.")
  if (!(is.character(panel_by) & length(panel_by) == 1)) stopIQR("'panel_by' needs to be character indicating single column.")
  if (!all(split_by %in% names(data))) stopIQR(paste0("Unknown column specified as 'split_by': ", paste0(setdiff(split_by, names(data)), collapse = ", ")))
  if (!all(lines_by %in% names(data))) stopIQR(paste0("Unknown column specified as 'lines_by': ", paste0(setdiff(lines_by, names(data)), collapse = ", ")))
  if (!all(panel_by %in% names(data))) stopIQR(paste0("Unknown column specified as 'panel_by': ", paste0(setdiff(panel_by, names(data)), collapse = ", ")))
  message("Generating figures of PK concentration summaries ...")
  statistic_avg <- match.arg(statistic_avg, c("MEAN", "GMEAN", "MEDIAN"))
  statistic_var0 <- match.arg(statistic_var, c("SD", "GSD", "PI90", "PI95"))
  if (statistic_var0 == "PI90") statistic_var <- c("P05", "P95")
  if (statistic_var0 == "PI95") statistic_var <- c("P025", "P975")
  fig_stat_labels <- c(MEAN = "Arithmetic mean", GMEAN = "Geometric mean", MEDIAN = "Median",
                       SD   = "+SD", GSD = "+geom.SD", PI90="90% Interval", PI95="95% Interval")
  d <- removeFrom_IQdataNCA(data = data,
                               FLAGremoveIGNOREI = TRUE,
                               FLAGremoveIGNORER = TRUE,
                               FLAGremoveIGNORSUM = TRUE,
                               FLAGremoveIGNORNCA = FALSE)
  res <- handleTIMEplot_IQdataNCA(d = d, FLAGTIME = "nominal", USETAD = !use_TAFD)
  d <- res$data
  xlabtext <- res$xlabtext
  d <- d[!is.na(d$CONCPLIN) & !is.na(d$CONCPLOG),]
  d$CONCPLOG[d$CONCPLOG==0] <- NA
  d <- d[!is.na(d$TIMEPLOT),]
  deflabels <- get_default_labels(d)
  mylabels  <- c(deflabels[setdiff(names(deflabels), names(labels))], labels)
  d$CONCPLOGDN <- d$CONCPLOG/d$DOSE
  d$CONCPLINDN <- d$CONCPLIN/d$DOSE
  CONCUNITDN <- paste0(data$CONCUNIT[1],"/",data$DOSEUNIT[1])
  title <- .figure_summary_pkconc
  title <- updateFigureNumberTitle_IQdataNCA(title,
                                                     figurenumber = figure_number, figureindex = NULL,
                                                     avg = fig_stat_labels[[statistic_avg]], var = fig_stat_labels[[statistic_var0]])
  if (d$FGBQPLIN[1] == d$FGBQPLOG[1] | yscale %in% c("lin", "log")){
    fgbqp <- ifelse(grepl("lin", yscale), d$FGBQPLIN[1], d$FGBQPLOG[1])
    footer <- figure_footer(fgbqp)
  } else {
    footer <- paste0(
      "Linear scale: ", figure_footer(d$FGBQPLIN[1]), "\n",
      "Log scale: ", figure_footer(d$FGBQPLOG[1])
    )
  }
  if (grepl("log", yscale)) {
    footer <- paste0(footer, " Zero values not shown on log-transformed axis.")
  }
  dS <- dplyr::group_by(d, dplyr::across(dplyr::all_of(c(figure_head, split_by, lines_by, panel_by, "TIMEPLOT", "LLOQ"))))
  dS <- tidyr::nest(dS)
  dS$summlin <- lapply(dS$data, calc_stats_raw, value_col = ifelse(dosenorm, "CONCPLINDN", "CONCPLIN"))
  dS$summlog <- lapply(dS$data, calc_stats_raw, value_col = ifelse(dosenorm, "CONCPLOGDN", "CONCPLOG"))
  avglin <- do.call(rbind, lapply(dS$summlin, function(s) s[,statistic_avg, drop = FALSE]) )
  avglog <- do.call(rbind, lapply(dS$summlog, function(s) s[,statistic_avg, drop = FALSE] ))
  varlin <- do.call(rbind, lapply(dS$summlin, function(s) s[,statistic_var, drop = FALSE]) )
  varlog <- do.call(rbind, lapply(dS$summlog, function(s) s[,statistic_var, drop = FALSE]) )
  dP <- dplyr::ungroup(subset(dS, select = -c(data, summlin, summlog)))
  dP$avglin  <- avglin[[1]]
  dP$avglog  <- avglog[[1]]
  if (ncol(varlin) == 2) {
    dP$varlinl <- varlin[[1]]
    dP$varlinu <- varlin[[2]]
    dP$varlogl <- varlog[[1]]
    dP$varlogu <- varlog[[2]]
  } else {
    if (statistic_var == "SD"){
      dP$varlinl <- avglin[[1]] - varlin[[1]]
      dP$varlinu <- avglin[[1]] + varlin[[1]]
      dP$varlogl <- avglog[[1]] - varlog[[1]]
      dP$varlogu <- avglog[[1]] + varlog[[1]]
    } else {
      dP$varlinl <- exp(log(avglin[[1]]) - log(varlin[[1]]))
      dP$varlinu <- exp(log(avglin[[1]]) + log(varlin[[1]]))
      dP$varlogl <- exp(log(avglog[[1]]) - log(varlog[[1]]))
      dP$varlogu <- exp(log(avglog[[1]]) + log(varlog[[1]]))
    }
  }
  if (!is.null(split_by)){
    dPS <- split(dP, dP[,split_by])
  } else {
    dPS <- list(dP)
  }
  if (!is.null(figure_head)) {
    check_common <- sapply(dPS, function(s) {
      nrow(unique(as.data.frame(s)[,figure_head, drop = FALSE])) == 1
    })
    if (any(!check_common)) {
      stopIQR("Values to be displayed as subtitle ('figure_head') not unique for the split used.")
    }
  }
  ylimlin <- NULL
  ylimlog <- NULL
  if (sameYaxis) {
    ylimlin <- c(0, max(dP$varlinu)*1.25)
    ylimlog <- c( min(c(dP$varlogl, data$LLOQ[1]))/2, max(dP$varlinu)*1.25)
  }
  ylabtext <- paste0(fig_stat_labels[[statistic_avg]], " (",fig_stat_labels[[statistic_var0]], ")\nanalyte conc. [", data$CONCUNIT[1], "]")
  if (dosenorm) ylabtext <- paste0("Dose-normalized",fig_stat_labels[[statistic_avg]], " (",fig_stat_labels[[statistic_var0]], ")\nanalyte concentration [",CONCUNITDN,"]")
  xlim <- NULL
  if (sameXaxis){
    xlim <- c(min(d$TIMEPLOT),max(d$TIMEPLOT))
  }
  allplots <- lapply(seq_along(dPS), function (k) {
    plot_summary_IQdataNCAsum(
      d          = dPS[[k]],
      lines_by   = lines_by,
      panel_by   = panel_by,
      figure_head = figure_head,
      labels     = mylabels,
      yscale     = yscale,
      dosenorm   = dosenorm,
      legend_pos = legend_pos,
      xlabtext   = xlabtext,
      xlim       = xlim,
      ylabtext   = ylabtext,
      ylimlin    = ylimlin,
      ylimlog    = ylimlog
    )
  })
  figs <- list(
    content = allplots,
    title = title,
    filename = filename,
    number = figure_number,
    footer = footer,
    data = dS,
    type = type,
    code = "pending"
  )
  class(figs) <- c("IQncaFigure", class(figs))
  if (!is.null(filename)) {
    write_IQncaFigure(figs, filename,...)
  } else {
    return(figs)
  }
}
#' Generate one page with summary figure
#'
#' @param d Data frame with summary of PK concentration to be plot on one page
#' @param lines_by grouping used for linetypes
#' @param panel_by variable to create panels by
#' @param figure_head Column names of covariates unique within pages to be displayed above the table
#' @param labels names character vector with labels for columns
#' @param yscale character identifying whether to plot linear ("lin")
#'        or semilogarithmic ("log") or both views ("linlog")
#' @param dosenorm logical whether to plot dose normalized concentrations
#' @param legend_pos specifying legend position ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param xlabtext label of x axis (time)
#' @param xlim limits of x axis (time) to be applied to all panels/pages
#' @param ylabtext Label for y axis
#' @param ylimlin Limits to be applied to linear y axis. If NULL, no scale enforced.
#' @param ylimlog Limits to be applied to logarithmic y axis. If NULL, no scale enforced.
#'
#' @return Returns a figure (ggplot or arranged ggplot object)
#' @family NCA Data Figures
plot_summary_IQdataNCAsum <- function(
    d,
    lines_by,
    panel_by,
    figure_head,
    labels,
    yscale,
    dosenorm,
    legend_pos,
    xlabtext,
    xlim,
    ylabtext,
    ylimlin,
    ylimlog
  ) {
  d$PANELS  <- d[[panel_by]] 
  d$PANELS  <- factor(d$PANELS, levels = unique(d$PANELS[order(nchar(d$PANELS), d$PANELS)] ))
  panel_name <- labels[panel_by]
  if (is.na(panel_name)) panel_name <- panel_by
  d$LINES <- d[[lines_by]] 
  d$LINES <- factor(d$LINES, levels = unique(d$LINES[order(nchar(d$LINES), d$LINES)] ))
  lines_name <- labels[lines_by]
  if (is.na(lines_name)) lines_name <- lines_by
  panellabels  <- paste(panel_name, ": ",unique(d$PANELS),sep="")
  names(panellabels) <- unique(d$PANELS)
  panellabels <- as_labeller(panellabels)
  lloq <- d$LLOQ[1]
  plottitle <- lapply(figure_head, function(cc) {
    d[[cc]][1]
  })
  names(plottitle) <- figure_head
  labeled_figure_head <- intersect(figure_head, names(labels))
  names(plottitle)[match(labeled_figure_head, names(plottitle))] <- labels[labeled_figure_head]
  plottitle <- unlist(lapply(seq_along(plottitle), function(p)  {
    paste0(names(plottitle)[p], ": ", plottitle[p] )
  }))
  plottitle <- paste(plottitle, collapse=" \n")
  plotlin <- IQRggplot(data = d, aes(x = TIMEPLOT, y = avglin)) +
    geom_line(aes(linetype=LINES)) +
    geom_errorbar(aes(ymin = varlinl, ymax = varlinu, linetype = LINES)) +
    geom_point(aes(shape = LINES)) +
    geom_hline(mapping= aes(yintercept = lloq), colour = "orange", linetype = "dotted") +
    geom_text(aes(x = max(xlim) , y = lloq, label = "LLOQ"), vjust = -0.5, hjust=1, colour="orange") +
    labs(x = xlabtext,
         y = ylabtext,
         linetype = lines_name,
         shape = lines_name) +
    labs(subtitle ="Linear view" ) +
    theme(legend.position = legend_pos, title = element_text(size=10))  +
    facet_wrap(~PANELS, labeller = panellabels)
  plotlog <- IQRggplot(data = d, aes(x = TIMEPLOT, y = avglog)) +
    geom_line(aes(linetype=LINES)) +
    geom_errorbar(aes(ymin = varlogl, ymax = varlogu, linetype = LINES)) +
    geom_point(aes(shape = LINES)) +
    geom_hline(mapping= aes(yintercept = lloq), colour = "orange", linetype = "dotted") +
    geom_text(aes(x = max(xlim) , y = lloq, label = "LLOQ"), vjust = -0.5, hjust=1, colour="orange") +
    scale_y_log10_IQnca() +
    labs(x = xlabtext,
         y = ylabtext,
         linetype = lines_name,
         shape = lines_name) +
    labs(subtitle ="Semilogarithmic view" ) +
    theme(legend.position = legend_pos, title = element_text(size=10))  +
    facet_wrap(~PANELS, labeller = panellabels)
  if (!is.null(xlim)) {
    suppressMessages(plotlog <- plotlog + coord_cartesian(xlim = xlim))
    suppressMessages(plotlin <- plotlin + coord_cartesian(xlim = xlim))
  }
  if (!is.null(ylimlin)) suppressMessages(plotlin <- plotlin + coord_cartesian(ylim = ylimlin))
  if (!is.null(ylimlog)) suppressMessages(plotlog <- plotlog + coord_cartesian(ylim = ylimlog))
  figure <- switch(
    yscale,
    lin = plotlin,
    log = plotlog,
    linlog = ggpubr::ggarrange(plotlin, plotlog,
                               common.legend = TRUE,
                               legend=legend_pos,
                               ncol = 1, nrow = 2 )
  )
  figure <- ggpubr::annotate_figure(figure,
                                    top = ggpubr::text_grob(plottitle,
                                                            color = "black"))
  figure
}
#' Function to plot IQncaFigure
#'
#' A PDF with the of the figure(s) is produced for the given filename. This will contain the title and footers.
#' Next to the file, a folder with meta information (e.g., the rmd file and adjusted pdf) is produced. The rmd and the
#' adjusted PDF can be used to generate a report via IQReport.
#'
#' @param figs IQncaFigure object
#' @param filename File to print the figures to.
#' @param ... Arguments to [IQRoutputFigure] to adjust plotting settings
#'
#' @return Nothing. Output to files.
#' @export
#'
write_IQncaFigure <- function(figs, filename, ...) {
  npages <- length(figs$content)
  mypath         <- dirname(filename)
  mybasename     <- basename(filename)
  metafolder     <- paste0(mybasename, "_meta")
  filePDF        <- paste0(mybasename,".pdf")
  fileRMD        <- paste0(mybasename, ".rmd")
  fileData       <- paste0(mybasename, "_data.rds")
  fileAnnotation <- paste0(mybasename,".ann")
  fileCode       <- paste0(mybasename,".R")
  plots <- lapply(seq_len(npages), function(k) {
    titlek <- cowplot::ggdraw() + cowplot::draw_label(paste0(figs$title, " (", k,"/", npages, ")"), fontface = "bold", x=0, hjust=0, size = 14)
    cowplot::plot_grid(
      titlek,
      figs$content[[k]],
      ncol = 1,
      rel_heights = c(0.05,0.95)
    )
  })
  IQRoutputFigure(plots,
                  footer = figs$footer,
                  filename = file.path(mypath, filePDF), ...)
  IQRoutputFigure(figs$content,
                  filename = file.path(mypath, metafolder, filePDF), ...)
  IQRoutputRDS(figs$data, filename = file.path(mypath, metafolder, fileData))
  anntitle__  <- paste0("Title : ", figs$title)
  annfooter__ <- paste0("Footer : ", figs$footer)
  annotation <- c(anntitle__,annfooter__)
  aux_filewrite(annotation, filename = file.path(mypath, metafolder, fileAnnotation))
  aux_filewrite(figs$code, filename = file.path(mypath, metafolder, fileCode))
  genRMDfigurefile_IQdataNCA(title = figs$title,
                                     figurenumber = figs$number, logY = NULL,
                                     filename = file.path(mypath, metafolder, filePDF),
                                     strat=NULL, stratdefault=NULL)
  return(invisible(NULL))
}
#' Internal function defining BLQ footer text for figures.
#'
#' @param x string defining setting for BLQ handling
#'
#' @return Footer text
figure_footer <- function(x) {
  footer <- switch(
    tolower(x),
    asconc   = "Values <LLOQ were considered analogue to BLQ handling used for NCA.",
    asis     = "Values <LLOQ were considered as contained in source data.",
    missing  = "Values <LLOQ were considered as contained in source data.",
    zero     = "Values <LLOQ were considered as zero.",
    'lloq/2' = "Values <LLOQ were considered as LLOQ/2",
    lloq     = "Values <LLOQ were considered as LLOQ"
  )
}
#' Replacement for stop function to be used in IQnca
#'
#' @param ...	zero or more objects which can be coerced to character (and which are pasted together with no separator) or a single condition object.
#' @param call. logical, indicating if the call should become part of the error message.
#' @param domain see documentation or same arument in stop() function
#' @export
stopIQR <- function(..., call.=FALSE,domain=NULL){
  stop(..., call.=call., domain=domain)
}
#' Replacement for warning function to be used in IQnca
#'
#' @param ...	zero or more objects which can be coerced to character (and which are pasted together with no separator) or a single condition object.
#' @param call. logical, indicating if the call should become part of the warning message.
#' @param domain see documentation or same arument in warning() function
#' @export
warningIQR <- function(..., call.=FALSE,domain=NULL){
  warning(..., call.=call., domain=domain)
}
#' Determine the operating system
#'
#' @return String "windows", "mac", or "unix"
#' @export
aux_getOS <- function(){
  i <- Sys.info()[["sysname"]]
  if (i=="Windows") return("windows")
  if (i=="Darwin") return("mac")
  if (i=="Linux") return("unix")
  stopIQR("Unknown operating system")
}
aux_closePDFs <- function() {
  tryCatch({
    mydevs <- grDevices::dev.list()
    dummy <- sapply(mydevs[names(mydevs) %in% c("pdf")], function (x) {
      grDevices::dev.off(x)
    })
  },error=function(x){})
  if (1==1) {a=1}
}
aux_closePNGs <- function() {
  if (aux_getOS()=="mac") {
    grDevices::graphics.off()
    return(invisible(NULL))
  }
  tryCatch({
    mydevs <- grDevices::dev.list()
    dummy <- sapply(mydevs[names(mydevs) %in% c("png")], function (x) {
      grDevices::dev.off(x)
    })
  },error=function(x){})
  return(invisible(NULL))
}
aux_closeDevice <- function(device) {
  if (grepl("pdf", device, ignore.case = TRUE)) aux_closePDFs()
  if (grepl("png", device, ignore.case = TRUE)) aux_closePNGs()
}
#' Check version function for packages
#'
#' @param pkgName String with name of package (defaults to IQnca)
#' @param IQdesktop String defining the IQdesktop version used for the analysis
#' @param exactVersion String with exact version of package required
#' @param minVersion String with minimum version of package required
#' @param Rversion The user can provide the R version used for this analysis.
#'   If provided (example: Rversion="3.5.1") then it will be checked if the current R
#'   version matches it. If not, there will be an error message.
#' @param isRopen The user can define whether R Open is to be used for the analysis (TRUE or FALSE).
#'   If provided then it will be checked if this is correct. If not, there will be an error message.
#' @param OS The user can provide which operating system used for this analysis.
#'   If provided (example: OS="unix") then it will be checked if the OS matches it.
#'   If not, there will be an error message.
#' @return Version of package as string (if minVersion=NULL)
#'         TRUE if version of package >= minVersion
#'         Error message if version of package < minVersion
#' @export
aux_version <- function(pkgName="IQnca", IQdesktop=NULL, exactVersion=NULL, minVersion=NULL,Rversion=NULL,isRopen=NULL,OS=NULL) {
  curVersion__ <- tryCatch(
    utils::packageVersion(pkgName),
    error=function (err) return ("No version present")
    )
  if (is.null(curVersion__)) return (NULL) 
  if (is.null(minVersion) & is.null(exactVersion) & is.null(IQdesktop)) {
    return(curVersion__)
  }
  if (!is.null(IQdesktop)) {
    if (!isIQdesktop()) stopIQR("You are not using IQdesktop (might not be a problem if everything else is well set up)")
    currentIQdesktopVersion <- getIQdesktopversion()
    if (currentIQdesktopVersion!=IQdesktop) stopIQR(sprintf("IQdesktop version %s required. You are using version %s",IQdesktop,currentIQdesktopVersion))
  }
  if (!is.null(minVersion)) {
    if (curVersion__ < minVersion) {
      stopIQR(sprintf("Package %s version is %s - but at least %s is required", pkgName, curVersion__, minVersion))
    }
  }
  if (!is.null(exactVersion)) {
    if (curVersion__ != exactVersion) {
      stopIQR(sprintf("Package %s version is %s - but version %s is required", pkgName, curVersion__, exactVersion))
    }
  }
  if (!is.null(Rversion)) {
    currentVersion <- paste0(version$major,".",version$minor)
    if (currentVersion != Rversion) {
      stopIQR(paste0("Your current R version (",currentVersion,") does not match the version the analysis was written for (",Rversion,").\n  You can on your own risk remove the 'Rversion' argument in the aux_version() function and run this script.\n  Or use the indicated version. We need to do this in order to ensure 100% reproducibility at all times."))
    }
  }
  if (!is.null(isRopen)) {
    if (!is.logical(isRopen)) stopIQR("isRopen needs to be TRUE or FALSE.")
    instPack <- utils::installed.packages()[,1]
    currentIsRopen <- "MicrosoftR" %in% instPack
    if (isRopen == TRUE & currentIsRopen == FALSE) stopIQR("You are using R, but the script was written for R open.\n  You can on your own risk remove the 'isRopen' argument in the aux_version() function and run this script.\n  Or use R Open instead of R. We need to do this in order to ensure 100% reproducibility at all times.")
    if (isRopen == FALSE & currentIsRopen == TRUE) stopIQR("You are using R open, but the script was written for R.\n  You can on your own risk remove the 'isRopen' argument in the aux_version() function and run this script.\n  Or use R instead of R Open. We need to do this in order to ensure 100% reproducibility at all times.")
  }
  if (!is.null(OS)) {
    if (OS != .Platform$OS.type) {
      stopIQR(paste0("Your current operating system (",.Platform$OS.type,") does not match the OS the analysis was written for (",OS,").\n  You can on your own risk remove the 'OS' argument in the aux_version() function and run this script.\n  Or use the indicated OS. We need to do this in order to ensure 100% reproducibility at all times."))
    }
  }
  return(TRUE)
}
#' Splits a string based on a separator substring
#'
#' By default a "," is used as separator.
#'
#' @param input A string to split
#' @param separator A substring to use as separator
#' @return A character vector with the split elements
#' @export
aux_explode <- function(input,separator=",") {
  return(unlist(strsplit(input,separator)))
}
#' Splits strings by separator only if separators not in parentheses
#'
#' By default a "," is used as separator.
#'
#' @param input A string to split
#' @param separator A substring to use as separator
#' @param group "round", "square", or "curly" allowing grouping by parantheses
#'   of defined type in which elements are not exploded
#' @return A character vector with the split elements
#' @export
aux_explodePC <- function(input,separator=",",group="round") {
  if (group=="round") {
    groupStart__ <- "("
    groupEnd__   <- ")"
  }
  if (group=="square") {
    groupStart__ <- "["
    groupEnd__   <- "]"
  }
  if (group=="curly") {
    groupStart__ <- "{"
    groupEnd__   <- "}"
  }
  if (group!="round" & group!="square" & group!="curly") stopIQR("wrong group definition")
  elements        <- c()
  openParenthesis <- 0
  lastIndex       <- 1
  elementIndex    <- 1
  for (k2 in 1:nchar(input)) {
    if (substr(input,k2,k2) == groupStart__) {
      openParenthesis <- openParenthesis + 1
    } else {
      if (substr(input,k2,k2) == groupEnd__) {
        openParenthesis <- openParenthesis - 1;
      } else {
        if ((substr(input,k2,k2) == separator) & (openParenthesis == 0)) {
          elements[elementIndex] <- aux_strtrim(substr(input,lastIndex,k2-1))
          elementIndex           <- elementIndex + 1
          lastIndex              <- k2+1
        }
      }
    }
  }
  elements[elementIndex] <- aux_strtrim(substr(input,lastIndex,nchar(input)))
  return(elements)
}
#' Replaces some string in a string
#'
#' Legacy function ... not using regexpr
#'
#' @param origstr A string to replace something in
#' @param oldsubstr Substring to replace
#' @param newsubstr Substring to use for replacement
#' @return Updated string
#' @export
aux_strrep <- function(origstr,oldsubstr,newsubstr) {
  return(gsub(oldsubstr, newsubstr, origstr, fixed="TRUE"))
}
#' aux_fileparts function
#'
#' Writes a formatted string to a text file
#'
#' @param filename.with.path path to file
#' @export
#' @examples
#' aux_fileparts("/path/to/file.csv")
aux_fileparts <- function(filename.with.path){
  pathname <- dirname(filename.with.path)
  filename <- basename(filename.with.path)
  fileext <- gsub(".*(\\.[^\\.]*)$","\\1",filename)
  filename <- gsub("(.*)(\\.[^\\.]*)$","\\1",filename)
  for (k in seq_along(filename)) {
    if(fileext[k]==filename[k]) fileext[k] <- ""
  }
  return(list(pathname=pathname,filename=filename,fileext=fileext))
}
#' aux_filewrite function
#'
#' Writes a formatted string to a text file
#'
#' @param text text to write
#' @param filename filename possibly including path
#' @export
aux_filewrite <- function(text,filename) {
  if (is.null(filename)) return(0)
  fid <- aux_fopen(filename, mode="w")
  write(text, fid)
  aux_fclose(fid)
}
#' aux_fileread function
#'
#' Reads contents of a text file
#'
#' @param filename filename possibly including path
#' @param collapserows indicates if rows should be collapsed to a string (default: TRUE)
#' @export
aux_fileread <- function(filename,collapserows=TRUE) {
  fid <- aux_fopen(filename, mode="r")
  text <- readLines(fid)
  aux_fclose(fid)
  if (collapserows) {
    text <- paste(text,collapse="\n")
  }
  return(text)
}
#' aux_mkdir function
#'
#' Creates a folder if it does not yet exist
#'
#' @param pathdir path to folder to create
#' @export
aux_mkdir <- function(pathdir) {
  suppressWarnings(if (!file.exists(pathdir)) dir.create(pathdir,recursive='TRUE'))
}
#' aux_rmdir function
#'
#' Removes a folder
#'
#' @param pathdir path to folder to remove
#' @export
aux_rmdir <- function(pathdir) {
  unlink(pathdir,recursive = 'TRUE')
}
aux_fopen <- function(filename,mode="w") {
  if (mode=="w") aux_mkdir(aux_fileparts(filename)$pathname)
  fid <- file(filename,open=mode)
  return(fid)
}
aux_fclose <- function(fid) {
  close(fid)
}
aux_fwrite <- function(fid,text) {
  write(text,fid)
}
aux_isnumericVector <- function(input) {
  return (!(NA %in% suppressWarnings(as.numeric(as.character(input)))))
}
#' geometric mean auxiliary
#'
#' @param x x
#' @param na.rm FALSE or TRUE
#' @return exp(mean(log(x))). Note that only positive values are considered!!!
#' @export
geomean <- function (x, na.rm = FALSE)
{
  if (!is.vector(x, mode = "numeric") || is.factor(x))
    stopIQR("'x' must be a numeric vector")
  wna <- which(is.na(x))
  if (length(wna)) {
    if (na.rm)
      x <- x[-wna]
    else return(NA)
  }
  if (any(x <= 0)) {
    warningIQR("Non-positive values in 'x'")
    return(NA)
  }
  else return(exp(mean(log(x[x>0]))))
}
#' geometric sd auxiliary
#'
#' @param x x
#' @param na.rm logical scalar indicating whether to remove missing values from x.
#'   If na.rm=FALSE (the default) and x contains missing values, then a missing
#'   value (NA) is returned. If na.rm=TRUE, missing values are removed from x
#'   prior to computing the coefficient of variation.
#' @param sqrt.unbiased logical scalar specifying what method to use to compute
#'   the sample standard deviation of the log-transformed observations. If
#'   sqrt.unbiased=TRUE (the default), the square root of the unbiased estimator
#'   of variance is used, otherwise the method of moments estimator of standard
#'   deviation is used. See the DETAILS section for more information.
#' @return geometric SD
#' @export
geosd <- function (x, na.rm = FALSE, sqrt.unbiased = TRUE)
{
  if (!is.vector(x, mode = "numeric") || is.factor(x))
    stopIQR("'x' must be a numeric vector")
  wna <- which(is.na(x))
  if (length(wna)) {
    if (na.rm)
      x <- x[-wna]
    else return(NA)
  }
  if (any(x <= 0)) {
    warningIQR("Non-positive values in 'x'")
    return(NA)
  }
  else {
    sd.log <- sd(log(x))
    if (!sqrt.unbiased) {
      n <- length(x)
      sd.log <- sqrt((n - 1)/n) * sd.log
    }
  }
  exp(sd.log)
}
#' geometric CV auxiliary
#'
#' @param x x
#' @param na.rm logical scalar indicating whether to remove missing values from x.
#'   If na.rm=FALSE (the default) and x contains missing values, then a missing
#'   value (NA) is returned. If na.rm=TRUE, missing values are removed from x
#'   prior to computing the coefficient of variation.
#' @return geometric CV
#' @export
geocv <- function (x, na.rm = FALSE)
{
  sqrt(exp(stats::sd(log(x), na.rm = na.rm)^2) - 1) * 100
}
#' Cluster Data in Blocks of Similar x Values and Summarize y Values per Block
#'
#' @param x x values or data.frame of x and y values
#' @param y y values or NULL, if x is a data.frame of x and y values
#' @param groupsize smallest expected group size
#' @param resolution gaps between groups of data points greater than \code{resolution}
#' lead to separation of groups.
#' @param lambda penalization of intra-group variance, set to 1 to have more groups
#' and set to 0 to get less but larger groups.
#' @param iterlim maximum number of iterations the algorithm takes.
#' @param log cluster on \code{log(x)} or on \code{x}. Does not change the value of  \code{resolution}.
#'
#' @details Data points are sorted by increasing x value and assigned into groups of size \code{groupsize}.
#' Next, groups separated by less than \code{resolution} are merged. In the following iterative algorithm,
#' the L1-distance of each data point to each of the groups is computed and wheighted by the groups geometric
#' standard deviation. Data points are then reassigned to the closest group. The procedure is repeated until
#' group membership does not change any more.
#'
#' @return \code{clusterX()} returns a data.frame with x, y and group values. Group is returned as a factor with numerically sorted levels.
#' @examples
#' \dontrun{
#'
#' library(ggplot2)
#'
#' 
#' timesD <- c(2, 10, 15, 30, 60, 120)
#' myfn <- function(x) 100*(1-exp(-.03*x))*exp(-.1*x)
#' 
#' times <- unlist(lapply(timesD, function(x) stats::rnorm(runif(1, 2, 10), x, 0.2*x)))
#' 
#' x <- data.frame(
#'   TIME = times,
#'   VALUE = stats::rnorm(length(times), myfn(times), 0.1*myfn(times) + 1)
#' )
#' x <- subset(x, TIME > 0)
#'
#' 
#' stat <- statXY(x, groupsize = 5, resolution = .01)
#' out <- attr(stat, "clusterOut")
#'
#'
#' 
#' P <- ggplot(out, aes(x = TIME, y = VALUE, color = block, pch = block)) + geom_point() +
#'   annotate("line", x = stat$TIME, y = stat$MEDIAN.VALUE) +
#'   annotate("line", x = stat$TIME, y = c(stat$P5.VALUE), lty = 2) +
#'   annotate("line", x = stat$TIME, y = c(stat$P95.VALUE), lty = 2)
#' print(P)
#' print(P + scale_x_log10())
#'
#'
#' }
#' @export
#' @importFrom stats median
#' @importFrom utils tail
clusterX <- function(x, y = NULL, groupsize = 5, resolution = 0.1, lambda = 1, iterlim = 100, log = FALSE) {
  n_inner <- groupsize
  n_iter <- iterlim
  n_min <- 1 
  alpha <- 0 
  if (log & any(x <= 0))
    stopIQR("When argument log = TRUE, x values must be strictly positive.")
  if (is.null(y)) y <- rep(1, length(x))
  if (!is.null(y) & length(y) != length(x))
    stopIQR("x and y must have the same length.")
  x <- cbind.data.frame(TIME = x, VALUE = y)
  x <- x[order(x[[1]]),]
  is_finite <- Reduce("&", lapply(x, is.finite))
  if (all(!is_finite))
    stopIQR("x contains only NA/NaN/Inf values.")
  if (any(!is_finite)) {
    x <- x[is_finite,]
    warningIQR("x or y contained NA/NaN/Inf values. These were removed befor applying the clustering algorithm.")
  }
  if (log) x[[1]] <- log(x[[1]])
  getMu <- function(x) {
    do.call(rbind, lapply(split(x, x[["block"]]), function(d) {
      c(mu1 = median(d[[1]]),
        mu2 = median(d[[2]]))
    }))
  }
  getB <- function(x) {
    do.call(rbind, lapply(split(x, x[["block"]]), function(d) {
      c(b1 = max(c(sum(abs(d[[1]] - median(d[[1]])))/length(d[[1]]), 1e-16)),
        b2 = max(c(sum(abs(d[[2]] - median(d[[2]])))/length(d[[2]]), 1e-16)))
    }))
  }
  getBlocks <- function(d) {
    d <- d[!is.na(d)]
    q <- structure(as.numeric(names(d)), names = d)
    cl <- list()
    if (length(d) > 0) {
      for (i in 1:length(d)) {
        cl[[i]] <- c(as.numeric(q[1]), as.numeric(d[1]))
        for (j in 1:length(d)) {
          cl_new <- unique(c(cl[[i]], d[names(d) %in% as.character(cl[[i]])], q[names(q) %in% as.character(cl[[i]])]))
          if (length(setdiff(cl_new, cl[[i]])) == 0) break else cl[[i]] <- cl_new
        }
        d <- d[setdiff(names(d), cl[[i]])]
        q <- structure(as.numeric(names(d)), names = d)
        if (length(d) == 0) break
      }
    }
    return(cl)
  }
  x[["block"]] <- ceiling(seq_along(x[[1]])/n_inner)
  counts <- table(x[["block"]])
  if (tail(counts, 1) < n_min) x[["block"]][x[["block"]] == length(counts)] <- length(counts) - 1
  mu <- getMu(x)
  d <- outer(mu[, 1], mu[, 1], function(x, y) abs(x - y)/resolution); diag(d) <- Inf
  d <- apply(d, 1, function(x) which(x <= 1)[1])
  blocks <- getBlocks(d)
  for (b in blocks) x[["block"]][x[["block"]] %in% b] <- b[1]
  blocks <- unique(x[["block"]])
  x[["block"]] <- match(x[["block"]], blocks)
  for (i in 1:n_iter) {
    mu <- getMu(x)
    b <- getB(x)
    d1 <- t(t(outer(x[, 1], mu[, 1], function(x, y)  abs(x - y)))/b[,1] + lambda*log(b[,1]^2))
    d2 <- t(t(outer(x[, 2], mu[, 2], function(x, y)  abs(x - y)))/b[,2] + lambda*log(b[,2]^2))
    d <- cos(alpha)*d1 + sin(alpha)*d2
    block_old <- x[["block"]]
    block_new <- apply(d, 1, which.min)
    counts <- table(block_new)
    x[["block"]] <- block_new
    if (any(counts < n_min)) {
      small_blocks <- counts[counts < n_min]
      k <- 1
      while (k <= length(small_blocks)) {
        block_nr <- names(small_blocks)[k]
        block_counts <- small_blocks[[k]]
        while (block_counts < n_min) {
          block_nr_next <- as.character(as.numeric(block_nr[length(block_nr)]) + 1)
          block_nr <- c(block_nr, block_nr_next)
          block_counts <- block_counts + counts[block_nr_next]
        }
        index <- x[["block"]] %in% block_nr
        x[["block"]][index] <- block_nr[1]
        available <- which(names(small_blocks) %in% as.character(x[["block"]]))
        if (any(available > k)) {
          k <- seq_along(small_blocks)[available[available > k]][1]
        } else break
      }
    }
    x <- x[order(x[["block"]], x[[1]]),]
    blocks <- unique(x[["block"]])
    x[["block"]] <- match(x[["block"]], blocks)
    if (all(block_new == block_old) & all(counts >= n_min)) {
      break
    }
  }
  if (i == n_iter) warningIQR("Algorithm did not converge.")
  if (log) x[[1]] <- exp(x[[1]])
  x[["block"]] <- factor(as.character(x[["block"]]), levels = as.character(sort(unique(x[["block"]]))))
  return(x)
}
#' @param quantiles the requested quantiles, usually 0.05 and 0.95. Quantiles are returned in columns named \code{PX.VALUE},
#' where \code{X = round(100*quantiles)}.
#' @param ... arguments going to \code{clusterX()}.
#'
#' @details \code{statXY()} computes a data.frame with the following columns
#' \itemize{
#' \item \code{GROUP = group}, group identifier
#' \item \code{TIME = mean(x)}, mean group x value, usually time
#' \item \code{MEAN.VALUE = mean(y)}, mean group y value
#' \item \code{MEDIAN.VALUE = median(y)}, median of group y values
#' \item \code{SD.VALUE = sd(y)}, standard deviation of group y values
#' \item \code{SE.VALUE = sd(y)/sqrt(length(y))}, standard error of group y values
#' \item \code{GEOMMEAN.VALUE = exp(mean(log(y)))}, geometrical mean of y values
#' \item \code{GEOMSD.VALUE = exp(sd(log(y)))}, geometrical standard deviation of y values
#' \item \code{PX.VALUE = quantile(y, probs = X/100)}, X\%-quantile of the group y values
#' }
#'
#' @return \code{statXY()} returns summary information as a data frame. The output of \code{clusterX()}
#' is returned in the attribute "clusterOut".
#' @rdname clusterX
#' @export
#' @importFrom stats median quantile
statXY <- function(x, y = NULL, ..., quantiles = c(0.05, 0.95)) {
  clout <- clusterX(x = x, y = y, ...)
  if (any(y <= 0))
    warningIQR("NA values returned for GEOMETRICAL MEAN or SD due to y-values <= 0.")
  out <- do.call(rbind, lapply(split(clout, clout[[3]]), function(d) {
    t <- d[[1]]
    y <- d[[2]]
    group <- as.numeric(as.character(d[[3]]))
    d <- data.frame(
      GROUP = group[1],
      TIME = mean(t, na.rm = TRUE),
      MEAN.VALUE = mean(y, na.rm = TRUE),
      MEDIAN.VALUE = median(y, na.rm = TRUE),
      SD.VALUE = sd(y, na.rm = TRUE),
      SE.VALUE = sd(y, na.rm = TRUE)/sqrt(length(y)),
      GEOMMEAN.VALUE = suppressWarnings(exp(mean(log(y)))),
      GEOMSD.VALUE = suppressWarnings(exp(sd(log(y))))
    )
    quantile_names <- paste0("P", round(100*quantiles), ".VALUE")
    quantile_data <- lapply(quantiles, function(q) as.numeric(stats::quantile(y, probs = q, na.rm = TRUE)))
    names(quantile_data) <- quantile_names
    quantile_data <- as.data.frame(quantile_data)
    cbind(d, quantile_data)
  }))
  attr(out, "clusterOut") <- clout
  return(out)
}
#' Summary stats per time point (no clustering)
#'
#' @param x x/time vector
#' @param y value vector
#' @return statistics (mean, median, etc. for each time point).
#' @export
#' @importFrom stats median quantile
statXYnominal <- function (x,y) {
  z <- data.frame(
    x = x,
    y = y
  )
  allTIME <- sort(unique(z$x))
  do.call(rbind,lapply(seq_along(allTIME), function (k) {
    time <- allTIME[k]
    values <- z$y[x==time]
    data.frame(
      TIME = time,
      MEAN.VALUE = mean(values,na.rm = TRUE),
      MEDIAN.VALUE = median(values,na.rm = TRUE),
      SD.VALUE = sd(values,na.rm = TRUE),
      SE.VALUE = sd(values, na.rm = TRUE)/sqrt(length(values)),
      GEOMMEAN.VALUE = suppressWarnings(exp(mean(log(values)))),
      GEOMSD.VALUE = suppressWarnings(exp(sd(log(values))))
    )
  }))
}
#' Split vector into equal pieces
#'
#' Splits a vector in pieces with defined length and
#' a remainder. Useful when to split datasets by ID for plotting over
#' several pages.
#'
#' @param x Vector to be splitted
#' @param n Number of elements per piece
#' @return List with the split pieces
#' @examples
#' x <- 1:20
#' aux_splitVectorEqualPieces(x,3)
#' @export
aux_splitVectorEqualPieces <- function(x,n) {
  if (n>=length(x)) return(list(x))
  n_pieces_same_length <- round(length(x)/n)
  out   <- list()
  for (k in 1:n_pieces_same_length)
    out[[k]] <- x[seq(1+(k-1)*n,n+(k-1)*n)]
  if (n_pieces_same_length*n<length(x)) {
    k <- length(out)+1
    out[[length(out)+1]] <- x[seq(1+(k-1)*n,length(x))]
  }
  return(out)
}
#' Prepad string with char to defined length
#'
#' @param value2prefill String to pre fill
#' @param lengthString Length of final string
#' @param fillChar Char to fill with
#' @export
aux_preFillChar <- function(value2prefill,lengthString,fillChar) {
  if (nchar(value2prefill) >= lengthString) {
    lengthString <- nchar(value2prefill)
  }
  result <- paste0(c(paste0(rep(fillChar,lengthString-nchar(as.character(value2prefill))),collapse=""),
                     as.character(value2prefill)),collapse="")
  return(result)
}
#' Trims a strings leading and trailing white spaces
#'
#' @param input A string to trim
#' @return Trimmed string
#' @export
aux_strtrim <- function(input) {
  return(gsub("^\\s+|\\s+$", "", input))
}
#' Postpad string with char to defined length
#'
#' @param value2postfill String to post fill
#' @param lengthString Length of final string
#' @param fillChar Char to fill with
#' @export
aux_postFillChar <- function(value2postfill,lengthString,fillChar) {
  if (nchar(value2postfill) >= lengthString) {
    lengthString <- nchar(value2postfill)
  }
  result <- tryCatch({
    paste0(c(as.character(value2postfill)),
           paste0(rep(fillChar,lengthString-nchar(as.character(value2postfill))),collapse=""),
           collapse="")
  }, error = function (msg) {
    result
  })
  return(result)
}
#' Remove unnecessary dot-dot from (relative) paths.
#'
#' @param path Path character string
#'
#' @return string with simplified path (no .. within path)
#' @export
#'
#' @examples aux_simplifypath("this/does/not/../work")
aux_simplifypath <- function(path) {
  tmp0 <- aux_explode(path,"/")
  tmp0 <- tmp0[grep("^[.]$", tmp0, invert = TRUE)]
  tmp <- tmp0
  idxnodots <- c(grep("..",tmp, fixed = TRUE, invert = TRUE),length(tmp)+1)
  step <- diff(idxnodots)
  while (any(step>1)) {
    idx0 <- which(step>1)[1]
    tmp <- tmp[-(idxnodots[idx0]+c(0,1))]
    idxnodots <- c(grep("..",tmp, fixed = TRUE, invert = TRUE),length(tmp)+1)
    step <- diff(idxnodots)
  }
  paste0(tmp,collapse = "/")
}
#' Check whether environment is IQdesktop
isIQdesktop <- function() {
  if (!file.exists("/IQDESKTOP/.version"))
    return(FALSE)
  return(TRUE)
}
#' Get IQDesktop version
getIQdesktopversion <- function () {
  if (!isIQdesktop()) return (NULL)
  aux_fileread("/IQDESKTOP/.version")
}
#' geom_uperrorbar ... nice to have
#' @param mapping See geom_errorbar
#' @param data See geom_errorbar
#' @param stat See geom_errorbar
#' @param position See geom_errorbar
#' @param ... See geom_errorbar
#' @param na.rm See geom_errorbar
#' @param show.legend See geom_errorbar
#' @param inherit.aes See geom_errorbar
#' @export
geom_uperrorbar <- function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomUperrorbar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
#' Needed for geom_uperrorbar
#' @export
GeomUperrorbar <- ggproto("GeomUperrorbar", Geom,
                          default_aes = aes(colour = "black", size = 0.5, linetype = 1, width = 0.5,
                                            alpha = NA),
                          draw_key = draw_key_path,
                          required_aes = c("x", "y", "ymax"),
                          setup_data = function(data, params) {
                            data$width <- data$width %II%
                              params$width %II% (resolution(data$x, FALSE) * 0.9)
                            transform(data,
                                      xmin = x - width / 2, xmax = x + width / 2, width = NULL
                            )
                          },
                          draw_panel = function(data, panel_scales, coord, width = NULL) {
                            GeomPath$draw_panel(data.frame(
                              x = as.vector(rbind(data$xmin, data$xmax, NA, data$x,   data$x)),
                              y = as.vector(rbind(data$ymax, data$ymax, NA, data$ymax, data$y)),
                              colour = rep(data$colour, each = 5),
                              alpha = rep(data$alpha, each = 5),
                              size = rep(data$size, each = 5),
                              linetype = rep(data$linetype, each = 5),
                              group = rep(1:(nrow(data)), each = 5),
                              stringsAsFactors = FALSE,
                              row.names = 1:(nrow(data) * 5)
                            ), panel_scales, coord)
                          }
)
#' Needed for geom_uperrorbar
#' @param a a
#' @param b b
#' @export
"%II%" <- function(a, b) {
  if (!is.null(a)) a else b
}
mybreaks <- function(n_major, n_minor, allowAddMinor = TRUE, allowReduceMajor = TRUE) {
  function(limits) {
    upper <- log10(limits[2])
    lower <- log10(limits[1])
    n <- ceiling(upper) - floor(lower)
    step <- 1
    logbreaks <- seq(floor(lower), ceiling(upper), step)
    nombreaks <- 10^logbreaks
    nombreaks.inrange <- nombreaks[nombreaks > limits[1] & nombreaks < limits[2]]
    if (allowAddMinor & length(nombreaks.inrange) < n_major & n_minor > 0) {
      minbreaks <- do.call("c", lapply(seq_len(length(logbreaks) - 1), function(i) {
        allminbreaks <- seq(10^logbreaks[i], 10^logbreaks[i+1], length.out = 10)
        allminbreaks[seq(1, 10, length.out = n_minor + 2)][-c(1, n_minor + 2)]
      }))
      minbreaks.inrange <- unique(minbreaks[minbreaks > limits[1] & minbreaks < limits[2]])
      nombreaks <- sort(union(nombreaks, minbreaks.inrange))
    }
    if (allowReduceMajor & length(nombreaks.inrange) > n_major) {
      nombreaks <- nombreaks.inrange[seq(1, length(nombreaks.inrange), by = floor(length(nombreaks.inrange)/n_major))]
    }
    nombreaks
  }
}
myminorbreaks <- function(n_major, n_minor) {
  n_minor <- 10
  function(limits) {
    logbreaks <- log10(mybreaks(n_major, n_minor, allowAddMinor = FALSE, allowReduceMajor = FALSE)(limits))
    minbreaks <- do.call("c", lapply(seq_len(length(logbreaks) - 1), function(i) {
      seq(10^logbreaks[i], 10^logbreaks[i+1], length.out = 10)
    }))
    return(minbreaks)
  }
}
mylabels <- function(labeltype = "natural") {
  function(breaks) {
    if (labeltype == "standard") {
      labels <- format(breaks)
    }
    if (labeltype == "scientific") {
      logbreaks <- log10(breaks)
      labels <- parse(text = paste0("10^", logbreaks))
    }
    if (labeltype == "natural") {
      labels <- format(breaks, scientific = FALSE, drop0trailing = TRUE, trim = TRUE)
    }
    if (labeltype == "comma") {
      labels <- format(breaks, scientific = FALSE, drop0trailing = TRUE, trim = TRUE, big.mark = ",")
    }
    return(labels)
  }
}
#' Log scale for x or y axis with proper grid lines
#'
#' Use as alternative to scale_y_log10().
#'
#' @param labeltype character to determine how labels are printed, either "natural" (default),
#' "comma" (like natural + comma separation for large numbers),
#' "scientific" (nicely print 10 to the power of ...), or "standard" (standard R).
#' @param n_major the number of major labels aiming at. Major labels will always be
#' powers of 10. With `n_major` it is possible to reduce the number of powers shown.
#' @param n_minor the exact number of minor labels, i.e. labels between powers of 10.
#' The argument is only used if the number of major labels drops below `n_major`.
#' @param ... other arguments going to scale_y_log10. If
#' `breaks`, `minor_breaks` and `labels` are provided, they overwrite the
#' generated breaks, minor breaks or labels.
#' @export
scale_y_log10_IQnca <- function(labeltype = c("natural", "comma", "scientific", "standard"), n_major = 4, n_minor = 0, ...) {
  labeltype <- match.arg(labeltype)
  args <- list(...)
  if (n_major < 1) stopIQR("scale_y_log10_IQnca() requires n_major to be > 0.")
  if (n_minor < 0) stopIQR("scale_y_log10_IQnca() requires n_minor to be >= 0.")
  if (length(args) > 0 & is.null(names(args)) | any(names(args) %in% ""))
    stopIQR("scale_y_log10_IQnca() requires that arguments passed via ... are named.")
  args <- c(args, list(breaks = mybreaks(n_major, n_minor),
                       minor_breaks = myminorbreaks(n_major, n_minor),
                       labels = mylabels(labeltype = labeltype)))
  args <- args[!duplicated(names(args))]
  do.call(scale_y_log10, args)
}
#' @rdname scale_y_log10_IQnca
#' @export
scale_x_log10_IQnca <- function(labeltype = c("natural", "comma", "scientific", "standard"), n_major = 4, n_minor = 0, ...) {
  labeltype <- match.arg(labeltype)
  args <- list(...)
  if (n_major < 1) stopIQR("scale_x_log10_IQnca() requires n_major to be > 0.")
  if (n_minor < 0) stopIQR("scale_x_log10_IQnca() requires n_minor to be >= 0.")
  if (length(args) > 0  & is.null(names(args)) | any(names(args) %in% ""))
    stopIQR("scale_x_log10_IQnca() requires that arguments passed via ... are named.")
  args <- c(args, list(breaks = mybreaks(n_major, n_minor),
                       minor_breaks = myminorbreaks(n_major, n_minor),
                       labels = mylabels(labeltype = labeltype)))
  args <- args[!duplicated(names(args))]
  do.call(scale_x_log10, args)
}
plot_slides <- function(plotlist, nrow = 3, ncol = 3, legend = NULL, legend.width = 0.2) {
  plots__ <- plotlist
  attr_out__ <- attr(plotlist, "plotData")
  plotsPerSlide__ <- nrow*ncol
  nPlots__ <- length(plots__)
  nSlides__ <- ceiling(nPlots__/plotsPerSlide__)
  slides__ <- lapply(seq_len(nSlides__), function(i__) {
    range__ <- plotsPerSlide__*(i__ - 1) + 1:plotsPerSlide__
    myplots__ <- vector("list", plotsPerSlide__)
    for (j__ in seq_len(plotsPerSlide__)) {
      if (range__[j__] > length(plots__)) {
        myplots__[[j__]] <- ggplot() + theme_classic()
      } else {
        myplots__[[j__]] <- plots__[[range__[j__]]]
      }
    }
    myplots__
  })
  output__ <- lapply(slides__, function(myslide__) {
    out__ <- suppressWarnings(cowplot::plot_grid(plotlist = myslide__, align = "hv", nrow = nrow, ncol = ncol))
    if (!is.null(legend))
      out__ <- cowplot::plot_grid(out__, legend, ncol = 2, rel_widths = c(1-legend.width, legend.width))
    return(out__)
  })
  attr(output__, "plotData") <- attr_out__
  class(output__) <- "IQRslideplot"
  return(output__)
}
#' @export
print.IQRslideplot <- function(x, ..., FLAGsilent = FALSE) {
  if (length(x) > 1) cat(length(x), "page output has been printed.\n")
  for (n__ in x) print(n__)
}
plotListObjects_ggplot <- function (myList) {
  plots <- aux_extractObjects(myList, "ggplot")
  for (p in plots) print(p)
}
get_timelabelIQR <- function(data) paste0("Time (",tolower(unique(data$TIMEUNIT)),")")
get_obslabelIQR <- function(data) paste0(unique(data$NAME), " (", unique(data$UNIT), ")")
scale_color_IQRblloq <- scale_color_manual("BLOQ", values = c("yes" = '#F15A60', "no" = '#737373'), drop = FALSE)
scale_shape_IQRblloq <- scale_shape_manual("BLOQ", values = c("yes" = 4, "no" = 19), drop = FALSE)
handle_obsScalesIQR <- function(scale, varNames, default = "log"){
  sc__ <- scale
  vn__ <- varNames
  if (is.null(sc__)) sc__ <- default
  if (!all(sc__ %in% c("log","linear")))
    stopIQR("Unknown scale defined. Needs to be 'linear' or 'log'.")
  if ( is.null(names(sc__)) ){
    if ( length(sc__) == 1 ){
      out__ <- data.frame(
        NAME = vn__,
        scale = sc__,
        stringsAsFactors = FALSE
      )
    } else {
      stopIQR("If given scale unnamed, provide only 'linear' or 'log' or name by observation.")
    }
  } else {
    if ( all(names(sc__) %in% vn__) ) {
      miss.scale__ <- setdiff(vn__,names(sc__))
      for (missk in miss.scale__) sc__ <- eval(parse(text=paste0("c(sc__, \"",missk,"\" = \"",default,"\")")))
      out__ <- data.frame(
        NAME = names(sc__),
        scale = sc__,
        stringsAsFactors = FALSE
      )
    } else {
      stopIQR("Scale defined for non-existing observation.")
    }
  }
  return(out__)
}
scale_y_apply <- function(scaletype, ...){
  if (scaletype == "log") {
    scale_y_log10(...)
  } else {
    scale_y_continuous(...)
  }
}
scale_x_apply <- function(scaletype, ...){
  if (scaletype == "log") {
    scale_x_log10(...)
  } else {
    scale_x_continuous(...)
  }
}
median90range <- function(x) {
  return(
    data.frame(
      y    = stats::median(x, na.rm=TRUE),
      ymin = stats::quantile(x, 0.05, na.rm=TRUE),
      ymax = stats::quantile(x, 0.95, na.rm=TRUE)
    )
  )
}
samplesize  <- function(x) {
  N__ = sum(!is.na(x))
  return(c(y=N__,label=N__))
}
handle_stratificationIQR <- function(x, stratify) {
  data__ <- x
  strat__ <- stratify
  if (!is.null(attr(data__,"covInfo")))
    covInfo0__ <- subset(attr(data__,"covInfo"), !TIME.VARYING)
  else
    covInfo0__ <- data.frame()
  if (!is.null(attr(data__,"catInfo")))
    catInfo0__ <- subset(attr(data__,"catInfo"), !TIME.VARYING)
  else
    catInfo0__ <- data.frame()
  missingstrat__ <- setdiff(stratify, c(covInfo0__$COLNAME, catInfo0__$COLNAME))
  if (length(missingstrat__) != 0){
    warningIQR('Following stratifier(s) are no time-independent covariates in the dataset: ',paste0(missingstrat__, collapse = ", "))
    strat__ = intersect(strat__,c(covInfo0__$COLNAME, catInfo0__$COLNAME))
  }
  stratcont__ <- intersect(strat__, covInfo0__$COLNAME)
  for (k__ in seq_along(stratcont__)) {
    strk__ <- stratcont__[k__]
      strmed__ <- stats::median(unique(as.data.frame(data__)[,c("USUBJID",strk__)])[[strk__]], na.rm = TRUE)
      strknew__ <- paste0(strk__,"CAT")
      data__[[strknew__]] <- (data__[[strk__]] > strmed__)+1
      covInfo0k__ <- subset(covInfo0__, COLNAME == strk__)
      catInfo0__ <- rbind(catInfo0__,
                          data.frame(COLNAME = strknew__, NAME = covInfo0k__$NAME, UNIT = covInfo0k__$UNIT, VALUES = "1,2",
                                     VALUETXT = paste0("< ",strmed__,covInfo0k__$UNIT,",",">= ",strmed__,covInfo0k__$UNIT),
                                     TIME.VARYING = FALSE)
      )
      strat__[strat__ == strk__] <- strknew__
  }
  return(list(data__, strat__, catInfo0__))
}
get_xposDosing <- function(xx__, tmin, tmax) {
  nd__ <- length(unique(xx__$NAME))
  if (nd__ == 1) xx__$TIMEdos <- xx__$TIME else {
    xx__$DNo <- as.numeric(factor(xx__$NAME))
    mar <- 0.005 * (tmax-tmin)  
    a = 2*mar / (nd__-1)
    b = - a - mar
    xx__$TIMEdos <- xx__$TIME + a*xx__$DNo+b
  }
  out <- xx__
  out
}
get_yposDosing <- function(xx__, ymin, ymax, sc__) {
  if (ymin == ymax) {
    if (sc__ == "linear"){
      ymin <- ymin-0.5
      ymax <- ymax
    } else {
      ymin <- 10^(log10(ymin)-0.5)
      ymax <- ymax
    }
  }
  dmax <- max(xx__$VALUE, na.rm = TRUE)
  xx__$ystart <- ymin
  if (dmax == 0) { 
    xx__$yend = xx__$ystart
  } else {
    if (sc__ == "linear"){
      xx__$yend   <- ymin + 0.5*(ymax-ymin)*xx__$VALUE/dmax
    } else {
      xx__$yend   <- exp(log(ymin)+0.5*(log(ymax)-log(ymin))*xx__$VALUE/dmax)
    }
  }
  out <- xx__
  out
}
get_labelDosing <- function(xx__) {
  xx__$labelDos <- ifelse(
    xx__$ADDL == 0 | is.na(xx__$ADDL),
    xx__$VALUE,
    paste0(xx__$ADDL+1, "x ", xx__$VALUE, " every ", round(xx__$II)," ",  xx__$TIMEUNIT)
  )
  if (dim(xx__)[1] > 1) {
    xx__ <- xx__[order(xx__$NAME,xx__$TIME),]
    for (k in seq(dim(xx__)[1],2,-1) )
      if (xx__$ADDL[k] == 0 | is.na(xx__$ADDL[k])) {
        xx__$labelDos[k] <- ifelse(
          xx__$labelDos[k] == xx__$labelDos[k-1] & xx__$NAME[k] == xx__$NAME[k-1],
          "",
          xx__$labelDos[k]
        )
      }
  }
  out <- xx__
  out
}
add_LayerDosingSingle <- function(plobj__, dInfo__, ymin, ymax, tmin, tmax, sc__) {
  dInfo__ <- get_yposDosing(dInfo__, ymin, ymax, sc__)
  dInfo__ <- get_xposDosing(dInfo__, tmin, tmax)
  dInfo__ <- get_labelDosing(dInfo__)
  out__ <- plobj__ +
  geom_segment(data=dInfo__, mapping = aes_string(x="TIMEdos", xend = "TIMEdos", y = "ystart", yend = "yend", color = "NAME"), size = 0.5, linetype = 2) +
    geom_text(data=dInfo__, mapping = aes_string(x="TIMEdos", y="yend", label = "labelDos", color = "NAME"), angle = 90, size = 2.5, hjust = 0, show.legend = FALSE) +
    scale_color_manual("", values=IQncaColors[2:20])
  out__
}
add_LayerDosingMulti <- function(plobj__, dInfo__, ymin, ymax, tmin, tmax, sc__) {
  dInfoS__ <- subset(dInfo__, ADDL == 0)
  if ("NT" %in% names(dInfo__)) {
    dInfoS__ <- dInfoS__[order(dInfoS__$NT),]
  } else {
    dInfoS__ <- dInfoS__[order(dInfoS__$TIME),]
  }
  diffT <- diff(dInfoS__$TIME)
  medianDiffT <- stats::median(diffT)
  diffdiffT <- abs(diff(diffT))
  idxBreak1 <- c(TRUE, FALSE, diffdiffT/diffT[1:(length(diffT)-1)] > 0.3)
  idxBreak2 <- c(TRUE, diff(dInfoS__$VALUE) > 0)
  idxBreak <- c(which(idxBreak1 | idxBreak2), dim(dInfoS__)[1]+1)
  dInfoSM__ <- data.frame(NAME = rep(dInfoS__$NAME[1], length(idxBreak)-1), TIME=NA,VALUE=NA,ADDL=NA,II=NA)
  for (k in 1:(length(idxBreak)-1)) {
    dInfoSM__$TIME[k]  <- dInfoS__$TIME[idxBreak[k]]
    dInfoSM__$VALUE[k] <- dInfoS__$VALUE[idxBreak[k]]
    dInfoSM__$ADDL[k]  <- idxBreak[k+1]-idxBreak[k]-1
    if (dInfoSM__$ADDL[k] == 0) {
      dInfoSM__$II[k]    <- medianDiffT
    } else {
      dInfoSM__$II[k]    <- mean(c(diffT,diffT[length(diffT)])[idxBreak[k]:(idxBreak[k+1]-2)])
    }
  }
  dInfoM__ <- subset(dInfo__, ADDL >  0)
  dInfoAll__ <- rbind(
    within(dInfoSM__, {FLAGautoMerge = TRUE}),
    within(dInfoM__[,c("NAME","TIME","VALUE","ADDL","II")], {FLAGautoMerge = FALSE})
  )
  dInfoAll__ <- get_yposDosing(dInfoAll__, ymin, ymax, sc__)
  dInfoAll__ <- get_labelDosing(dInfoAll__)
  out__ <- plobj__ +
    geom_rect(data=dInfoAll__,
              mapping = aes_string(
                xmin="TIME", xmax = "TIME+(ADDL+1)*II",
                ymin = "ystart", ymax = "yend",
                fill="NAME"),
              color = "transparent", alpha=0.5) +
    geom_rect(data=subset(dInfoAll__, FLAGautoMerge),
              mapping = aes_string(
                xmin="TIME", xmax = "TIME+(ADDL+1)*II",
                ymin = "ystart", ymax = "yend"),
              color = "grey20", fill = NA, size = 0.5, linetype = 3) +
    geom_text(data=dInfoAll__, mapping = aes_string(x="TIME", y="yend", label = "labelDos", color="NAME"), angle = 90, size = 2.5, hjust = 0, show.legend = FALSE) +
    scale_color_manual("", values=IQncaColors[2:20]) +
    scale_fill_manual("", values=IQncaColors[2:20])
  out__
}
handle_MDVBLQplot <- function(x, FLAGremoveMDV = TRUE) {
  out__ <- x[!is.na(x$VALUE),]
  if (FLAGremoveMDV) {
    out__ <- x[x$MDV==0,]
  } else {
    out__$MDVann <- factor(out__$MDV, levels = c(0,1), labels = c("no", "yes"))
  }
  BLOQ <- as.numeric(out__$VALUE < out__$LLOQ)
  BLOQ[is.na(BLOQ)] <- 0
  out__$BLOQ <- BLOQ
  out__$BLOQ <- factor(out__$BLOQ, levels = c(0,1), labels = c("no", "yes"))
  out__
}
handle_duplicatedLevels <- function(catInfo__) {
  for (k in seq_along(catInfo__$COLNAME)) {
    vals__ = aux_explode(catInfo__$VALUES[k])
    txts__ = aux_explode(catInfo__$VALUETXT[k])
    if (any(duplicated(txts__))) {
      warningIQR("Non-unique text values in", catInfo__$COLNAME[k])
      utxts__ <- unique(txts__)
      for (txtk__ in utxts__) {
        idx__ <- txts__ %in% txtk__
        if (sum(idx__) > 1) {
          txts__[idx__] <- paste0(txts__[idx__],".",vals__[idx__])
        }
      }
      catInfo__$VALUETXT[k] <- paste0(txts__, collapse = ",")
    }
  }
  catInfo__
}
#' Determine layout for distribution single plots to rows, columns, and pages
#'
#' If both nrow and ncol are given, npage is determined appropriately.
#' For other cases missing nrow/ncol is determined to match the required npage or to distribute on as many pages as possible.
#' - either nrow or ncol is missing: assume 1 for the missing input, determine npage accordingly or use the given number of pages.
#' - both nrow and ncol are missing: determined to match npage and keep an ratio of nrow/ncol approx 3/4 or general one page per plot
#'
#' @param nplots Number of plots to distribute
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param npage Number of pages
#'
#' @return list with nrow, ncol, npage
#' @export
#' @author Anne Kümmel, IntiQuan
aux_plotlayout <- function(nplots, nrow = NULL, ncol = NULL, npage = NULL) {
  if (is.null(nrow) & is.null(ncol)) {
    if (is.null(npage)) npage <- nplots
    nPerPage <- ceiling(nplots / npage)
    nrow <- floor(sqrt(4/3*nPerPage))
    ncol <- ceiling(nPerPage/nrow)
    nPerPage <- ncol*nrow
  } else {
    if (is.null(ncol)){
      if (is.null(npage)) {
        ncol <- 1
        npage <- ceiling(nplots/nrow)
      } else {
        nPerPage <- ceiling(nplots / npage)
        ncol <- ceiling(nPerPage/nrow)
      }
    }
    if (is.null(nrow)){
      if (is.null(npage)) {
        nrow <- 1
        npage <- ceiling(nplots/ncol)
      } else {
        nPerPage <- ceiling(nplots / npage)
        nrow <- ceiling(nPerPage/ncol)
      }
    }
  }
  nPerPage <- nrow*ncol
  npage <-  ceiling(nplots / nPerPage)
  return(list(nrow=nrow, ncol=ncol, npage=npage))
}
is_plot_object <- function(x) {
  ggplot2::is.ggplot(x) | gtable::is.gtable(x)
}
remove_legend <- function(x) {
  if (ggplot2::is.ggplot(x)) x <- x + theme(legend.position = "none")
  return(x)
}
#' Create pages from IQRoutputFigure
#'
#' Creates plot objects using `cowplot` arranging the plots from the
#' [IQRoutputFigure] into rows and columns and potentially multiple
#' pages. The title, subtitle, footer, and legend are plotted on each
#' created page.
#'
#' Per default, plots are arranged to one page and a legend is plotted
#' on the right-hand side of the plots. The user can provide a plot object
#' of the legend. If not provided the legend is taken from the first plot
#' object in `content`.
#' If options for the layout (`opt.layout`) are given in the IQRoutputFigure
#' they will be applied unless a value is given as input.
#' Note that potential page dimention and resolution settings (`opt.page`)
#' do not apply here.
#'
#' @param x IQRoutputFigure object
#' @param nrow number of rows per page
#' @param ncol number of columns per page
#' @param npage number of pages. If nrow and ncol is given, this input is ignored.
#' @param legend.option Character or numeric whether to leave legend in plots as is ('as.is', 1),
#'              remove all legends from the plots ('remove', 2), or plot common legend ('common', 3).
#'              As common legend, the legend of the first plot per page is used if not user provided
#'              by 'legend.object'
#' @param legend.object User-provided legend
#' @param legend.position Position of legend relative to main plots.
#'                 Can be 'right' (default), 'left', 'top', or 'bottom'.
#' @param legend.relsize Fraction of plot region (width for left or right position,
#'                 height for top or bottom position) allocated to legend.
#' @param title.relheight Fraction of page height allocated to plot title (if exists).
#' @param subtitle.relheight Fraction of page height allocated to plot subtitle (if exists).
#' @param footer.relheight Fraction of page height allocated to plot footer (if exists).
#'
#' @return list of plot objects or single plot object
#' @author Anne Kümmel, IntiQuan
#' @export
#' @examples
#' \dontrun{
#'  
#'  createPages_IQRoutputFigure(figobj, nrow = 2, ncol = 3)
#'  
#'  
#'  createPages_IQRoutputFigure(figobj, nrow = 2, npage = 3)
#' }
createPages_IQRoutputFigure <- function(x,
                                  nrow = NULL, ncol = NULL, npage = NULL,
                                  legend.option = c("as.is", "remove", "common"),
                                  legend.object = NULL, legend.position = "right", legend.relsize = 0.2,
                                  title.relheight = 0.05, subtitle.relheight = 0.05, footer.relheight = 0.05) {
  if (!is.null(x$opt.layout)) {
    for (opt in names(x$opt.layout)) assign(opt, x$opt.layout[[opt]])
  }
  inputoptions <- as.list(match.call(expand.dots = TRUE))
  inputoptions <- inputoptions[!names(inputoptions) %in% c("", "x")]
  for (opt in names(inputoptions)) assign(opt, inputoptions[[opt]])
  if (!is.character(legend.option)) stopIQR("legend.option needs to be character ('as.is', 'remove', or 'common')")
  legend.option <- match.arg(legend.option)
  x <- purrr::compact(x)
  if (is_plot_object(x$content)) x$content <- list(x$content)
  if (!legend.position %in% c("right","left","bottom","top")) stopIQR("Legend position needs to be either right, left, bottom, or top.")
  nplots <- length(x$content)
  layout  <- aux_plotlayout(nplots, nrow, ncol, npage)
  nrow__  <- layout$nrow
  ncol__  <- layout$ncol
  npages__ <- layout$npage
  nPerPage <- nrow__ * ncol__
  pageIndex__ <- rep(1:npages__, each=nPerPage)[1:nplots]
  if (legend.option == "as.is") {
    legend__ <- list(NULL)[rep(1,npages__)]
    if (!is.null(legend.object)) warningIQR("Provided legend object ignored.\nIt is only considered as common legend when setting legend.option = 'common'. ")
  } else if (legend.option == "remove"){
    x$content <- lapply(x$content, remove_legend)
    legend__ <- list(NULL)[rep(1,npages__)]
  } else if (legend.option == "common") {
    x$content <- lapply(x$content, remove_legend)
    if (!is.null(legend.object)) {
      legend__ <- list(legend.object)[rep(1, npages__)]
    } else {
      legend__ <- lapply(which(!duplicated(pageIndex__)), function(ii) {
              ll__ <- tryCatch(cowplot::get_legend(x$content[[ii]] + theme(legend.position=legend.position)), error = function(e) cat("First figure of page has no legend."))
              if ("try-error" %in% class(ll__)) ll__ <- NULL
              ll__
      })
    }
    relsizes__ <- c(1-legend.relsize, legend.relsize)
    addlegendfun__ <- switch(legend.position,
                             top    = function(pc, legend) cowplot::plot_grid(legend, pc, ncol = 1, rel_heights = rev(relsizes__)),
                             bottom = function(pc, legend) cowplot::plot_grid(pc, legend, ncol = 1, rel_heights = relsizes__),
                             left   = function(pc, legend) cowplot::plot_grid(legend, pc, nrow = 1, rel_widths  = rev(relsizes__)),
                             right  = function(pc, legend) cowplot::plot_grid(pc, legend, nrow = 1, rel_widths  = relsizes__)
    )
  }
  plot_content <- lapply(1:npages__, function(p__) {
    oo__ <- cowplot::plot_grid(plotlist = x$content[pageIndex__ == p__], nrow = nrow__, ncol = ncol__)
    if (legend.option == "common" & !is.null(legend__[[p__]])) {
      oo__ <- addlegendfun__(oo__, legend__[[p__]])
    }
    oo__
  })
  relheights__ <- 1
  plot_subtitle <- NULL
  if ("subtitle" %in% names(x)){
    plot_subtitle <- cowplot::ggdraw() + cowplot::draw_label(x$subtitle, fontface = "plain", x=0, hjust=0)
    relheights__ <- c(subtitle.relheight,relheights__)
  }
  plot_title <- NULL
  if ("title" %in% names(x)) {
    plot_title <- cowplot::ggdraw() + cowplot::draw_label(x$title, fontface = "bold", x=0, hjust=0)
    relheights__ <- c(title.relheight,relheights__)
  }
  plot_footer <- NULL
  if ("footer" %in% names(x)) {
    plot_footer <- cowplot::ggdraw() + cowplot::draw_label(x$footer, fontface = "plain", x=0, hjust=0, size = 10)
    relheights__ <- c(relheights__, footer.relheight)
  }
  relheights__[relheights__==1] <- 2-sum(relheights__)
  plot_pages__ <- lapply(1:npages__, function(p__) {
    plist <- c(list(plot_title, plot_subtitle), plot_content[p__], list(plot_footer))
    cowplot::plot_grid(
      plotlist = plist[!sapply(plist, is.null)],
      ncol = 1,
      rel_heights = relheights__
    )
  })
  if (length(plot_pages__) == 1) plot_pages__ <- plot_pages__[[1]]
  return(plot_pages__)
}
#' IQ NCA's standard theme
#'
#' The standard gg theme is based on `theme_bw`.
#'
#' @param base_size numeric, font-size
#' @param base_family character, font-name
#' @export
themeIQnca <- function(base_size = 12, base_family = "") {
  colors <- list(
    medium = c(gray = '#737373', red = '#F15A60', green = '#7AC36A', blue = '#5A9BD4', orange = '#FAA75B', purple = '#9E67AB', maroon = '#CE7058', magenta = '#D77FB4'),
    dark = c(black = '#010202', red = '#EE2E2F', green = '#008C48', blue = '#185AA9', orange = '#F47D23', purple = '#662C91', maroon = '#A21D21', magenta = '#B43894'),
    light = c(gray = '#CCCCCC', red = '#F2AFAD', green = '#D9E4AA', blue = '#B8D2EC', orange = '#F3D1B0', purple = '#D5B2D4', maroon = '#DDB9A9', magenta = '#EBC0DA')
  )
  gray <- colors$medium["gray"]
  black <- colors$dark["black"]
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(line = element_line(colour = "black"),
          rect = element_rect(fill = "white", colour = NA),
          text = element_text(colour = "black"),
          axis.text = element_text(size = rel(1.0), colour = "black"),
          axis.text.x = element_text(margin=unit(c(4, 4, 0, 4), "mm")),
          axis.text.y = element_text(margin=unit(c(4, 4, 4, 0), "mm")),
          axis.ticks = element_line(colour = "black"),
          axis.ticks.length = unit(-2, "mm"),
          legend.key = element_rect(colour = NA),
          panel.border = element_rect(colour = "black"),
          strip.background = element_rect(fill = "white", colour = NA),
          strip.text = element_text(size = rel(1.0)))
}
#' Color table for IQnca
#'
#' @export
IQncaColors <- rep_len(c("#000000", "#C5000B", "#0084D1",
                            "#579D1C", "#FF950E", "#4B1F6F",
                            "#1B9E77", "#D95F02", "#7570B3"), length.out = 5000)
#' Color table for IQnca
#'
#' @param ... passed to scale_color_manual(...)
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = as.factor(sprintf("%2i",1:19)), y = 1)
#' IQRggplot(df, aes(x,y, color = x)) +
#'   geom_point() + scale_color_IQnca()
#' }
scale_color_IQnca <- function(...) {
  scale_color_manual(..., values = IQncaColors)
}
#' Color table for IQnca
#'
#' @param ... passed to scale_fill_manual(...)
#' @export
scale_fill_IQnca <- function(...) {
  scale_fill_manual(..., values = IQncaColors)
}
#' ggplot functionality implementing IQnca style
#'
#' @param ... typical ggplot input arguments
#' @param fontsize fontsize used for IQR Tools theme
#' @return ggplot2 object
#' @export
IQRggplot <- function(..., fontsize = 12) {
  p__ <- ggplot(...) + themeIQnca(base_size = fontsize)
  return(p__)
}
#' Listing of ignored subjects and records
#'
#' Reports ignored subjects and records with the reason for ignoring from the analysis.
#' Displays on screen and can also save the information to a file as an IQRoutputTable object.
#'
#' @param data IQdataNCA object for which to do the reporting
#' @param listingnumber Character string with listing number information, added to the title for each table, if defined
#' @param labels Customized labels for columns that substitute the column name in the IQdataNCA object.
#' @param filename Filename, possibly including path, to store are IQRoutputTable text file for Word reporting
#'
#' @export
#' @family NCA Data Listings
listing_ignoredSubjectsRecords_IQdataNCA <- function (
    data,
    listingnumber = NULL,
    labels        = NULL,
    filename      = NULL
) {
  deflabels <- get_default_labels(data)
  mylabels  <- c(deflabels[setdiff(names(deflabels), names(labels))], labels)
  if (nrow(data[!is.na(data$IGNORER),]) > 0) {
    ignrecords <- cbind(Type="Ignored record",unique(data[!is.na(data$IGNORER),c("IGNORER","USUBJID","PROFILE","IX","ATIME","NTIME","ACONC")]))
    names(ignrecords)[2] <- "Reason"
  } else {
    ignrecords <- NULL
  }
  if (nrow(data[!is.na(data$IGNOREI),]) > 0) {
    ignsubjects <- cbind(Type="Ignored subject",unique(data[!is.na(data$IGNOREI),c("IGNOREI","USUBJID","PROFILE")]))
    names(ignsubjects)[2] <- "Reason"
    if (!is.null(ignrecords)) {
      ignsubjects$IX <- "-"
      ignsubjects$ATIME <- "-"
      ignsubjects$NTIME <- "-"
      ignsubjects$ACONC <- "-"
    }
  } else {
    ignsubjects <- NULL
  }
  igninfo <- rbind(ignsubjects,ignrecords)
  sublabels <- intersect(names(mylabels), names(igninfo))
  for (k in seq_along(sublabels)) {
    names(igninfo)[names(igninfo) == sublabels[k]] <- mylabels[[sublabels[k]]]
  }
  loadSetupOptions_IQnca()
  title <- .listing_excludedanalysis_pkconc
  title <- updateListingNumberTitle_IQdataNCA(title = title,listingnumber = listingnumber, listingindex = "none")
  if (is.null(igninfo)) {
    igninfo <- data.frame(
      INFO = "No ignored records or subjects"
    )
    IQRoutputTable(xtable = igninfo,xtitle = title,filename = filename,
                   xfooter = "These subjects and records are ignored in the totality of the analysis and reporting.")
  } else {
    N1 = nrow(ignsubjects); if (is.null(N1)) N1 <- 0
    N2 = nrow(ignrecords); if (is.null(N2)) N2 <- 0
    IQRoutputTable(xtable = igninfo,xtitle = title,filename = filename,
                   xfooter = "These subjects and records are ignored in the totality of the analysis and reporting.")
  }
}
#' Report ignored records from summary tables and/or NCA parameter determination
#'
#' Reports records that are ignored from either summary tables or NCA parameter determination
#' with the reason why. Displays on screen and can also save the information to a file as an IQRoutputTable object.
#'
#' @param data IQdataNCA object for which to do the reporting
#' @param labels Labels for columns to substitute the corresponding standard column names
#' @param filename Filename, possibly including path, to store are IQRoutputTable text file for Word reporting
#' @export
#' @family NCA Data
reportIgnoredSUMNCA_IQdataNCA <- function (
    data,
    labels = NULL,
    filename=NULL
) {
  deflabels <- get_default_labels(data)
  mylabels  <- c(deflabels[setdiff(names(deflabels), names(labels))], labels)
  if (nrow(data[!is.na(data$IGNORSUM),]) > 0) {
    ignrecordsSUM <- cbind(IGNORED.FROM="Summary",unique(data[!is.na(data$IGNORSUM),c("IGNORSUM","USUBJID","PROFILE","IX","ATIME","NTIME","ACONC")]))
    names(ignrecordsSUM)[2] <- "Reason"
  } else {
    ignrecordsSUM <- NULL
  }
  if (nrow(data[!is.na(data$IGNORNCA),]) > 0) {
    ignrecordsNCA <- cbind(IGNORED.FROM="NCA",unique(data[!is.na(data$IGNORNCA),c("IGNORNCA","USUBJID","PROFILE","IX","ATIME","NTIME","ACONC")]))
    names(ignrecordsNCA)[2] <- "Reason"
  } else {
    ignrecordsNCA <- NULL
  }
  igninfo <- rbind(ignrecordsSUM,ignrecordsNCA)
  if (!is.null(igninfo)) {
    igninfo <- do.call(rbind,lapply(split(igninfo,igninfo$USUBJID), function (x) {
      do.call(rbind,lapply(split(x,x$PROFILE), function (y) {
        do.call(rbind,lapply(split(y,y$IX), function (z) {
          if (nrow(z) > 1) {
            z$IGNORED.FROM <- paste0(z$IGNORED.FROM,collapse = " & ")
            if (length(unique(z$Reason)) > 1) {
              z$Reason <- paste0(z$Reason,collapse = " & ")
            }
            z <- z[1,]
          }
          z
        }))
      }))
    }))
  }
  sublabels <- intersect(names(mylabels), names(igninfo))
  for (k in seq_along(sublabels)) {
    names(igninfo)[names(igninfo) == sublabels[k]] <- mylabels[[sublabels[k]]]
  }
  if (is.null(igninfo)) {
    igninfo <- data.frame(
      INFO = "No ignored records (SUM or NCA)"
    )
    IQRoutputTable(xtable = igninfo,xtitle = paste0("Ignored records from data summary tables (N=0) and NCA PK parameter determination (N=0)"),filename = filename,
                   xfooter = "These records are excluded from data summary tables and/or NCA PK parameter determination. They do appear in lisitings of individual concentrations.")
  } else {
    N1 = nrow(ignrecordsSUM); if (is.null(N1)) N1 <- 0
    N2 = nrow(ignrecordsNCA); if (is.null(N2)) N2 <- 0
    names(igninfo)[[1]] <- "Ignored in"
    IQRoutputTable(xtable = igninfo,xtitle = paste0("Ignored records from data summary tables (N=",N1,") and NCA PK parameter determination (N=",N2,")"),filename = filename,
                   xfooter = "These records are excluded from data summary tables and/or NCA PK parameter determination. They do appear in lisitings of individual concentrations.")
  }
}
#' Table summarizing the available observations
#'
#' Statistics on number of subjects, samples, records below LLOQ, and ignored records are
#' calculated and presented. Counts are stratified per default by profile.
#'
#' @param data IQncaData
#' @param stratifyBy character vector defining the columns used for stratification (default: `Profile`)
#' @param labels Named vector with labels that should be used as names for stratification column(s)
#' @param tableTitle Table title
#' @param footerAddText Note to be added to table footer
#' @param filename filename to print table to
#' @param FLAGpatients whether to use the term "patients" instead of "subjects"
#' @param FLAGtotal whether a row with the total numbers should be added
#' @param digits Number of significant digits for rounding of percentages
#' @param report Argiment to IQRoutputTable whether to prepare the table for reporting
#'
#' @return IQRoutputTable if no filename given. If filename is given, table is printed to file and nothing is returned.
#' @export
#' @family NCA Data
#'
#' @examples
#' \dontrun{
#' summaryObservations_IQdataNCA(
#'   data = dataNCA,
#'   stratifyBy    = c("GROUP", "ANALYTE"),
#'   labels = c("GROUP" = "Dose", "ANALYTE" = "Analyte"),
#'   filename = "summary_observations.txt"
#' )
#' }
summaryObservations_IQdataNCA <- function(
    data,
    stratifyBy    = "PROFILE",
    labels        = NULL,
    tableTitle    = NULL,
    footerAddText = NULL,
    filename      = NULL,
    FLAGpatients  = FALSE,
    FLAGtotal     = TRUE,
    digits        = 4,
    report        = NULL
) {
  if (!is_IQdataNCA(data)) {
    stopIQR("Input argument 'data' is not an IQdataNCA object")
  }
  deflabels <- get_default_labels(data)
  mylabels  <- c(deflabels[setdiff(names(deflabels), names(labels))], labels)
  data <- removeFrom_IQdataNCA(data = data,
                                       FLAGremoveIGNOREI = FALSE,
                                       FLAGremoveIGNORER = TRUE,
                                       FLAGremoveIGNORSUM = FALSE,
                                       FLAGremoveIGNORNCA = FALSE)
  if (length(stratifyBy) > 1) {
    data$STRAT <- Reduce(function(c1,c2) paste0(c1,"_",c2), data[,stratifyBy])
  } else {
    data$STRAT <- data[[stratifyBy]]
  }
  dS <- split(data, data$STRAT)
  if (FLAGtotal) {
    dS$TOTAL <- within(data, { STRAT <- "Total" })
    dS$TOTAL[[stratifyBy[1]]] <- "Total"
    if (length(stratifyBy) > 1) {
      for (kk in 2:length(stratifyBy)) {
        dS$TOTAL[[stratifyBy[kk]]] <- "-"
      }
    }
  }
  tab <- do.call(rbind,
                     lapply(seq_along(dS),
                            function(k) {
                              datak <- as.data.frame(dS[[k]])
                              Nindiv        <- length(unique(datak$USUBJID))
                              Nsamples      <- nrow(datak)
                              NsamplesBLOQ  <- sum(datak$BLLOQ)
                              NblqPostFD    <- sum(datak$BLLOQ[!(datak$PROFTYPE == "FD" & datak$TIME <= 0)])
                              NmissingCONC  <- sum(is.na(datak$ACONC))
                              NmissingTIME  <- sum(is.na(datak$ATIME) & is.na(datak$NTIME))
                              Nnon0preFD    <- sum(datak$BLLOQ[(datak$PROFTYPE == "FD" & datak$TIME <= 0)] == 0)
                              NignoredI     <- sum(!is.na(datak$IGNOREI))
                              NignoredSUM   <- sum(!is.na(datak$IGNORSUM))
                              NignoredNCA   <- sum(!is.na(datak$IGNORNCA))
                              out <- datak[1,stratifyBy, drop = FALSE]
                              entry_string <- function(n, ntot) paste0(n, " (", signif(100 * n/ntot, digits), "%)")
                              out <- dplyr::mutate(out,
                                `N subjects` = Nindiv,
                                `N samples`  = Nsamples,
                                `N BLOQ samples`                  = entry_string(NsamplesBLOQ, Nsamples),
                                `N BLOQ samples post first dose`  = entry_string(NblqPostFD,   Nsamples),
                                `N missing observations^a^`       = entry_string(NmissingCONC, Nsamples),
                                `N missing time information^b^`   = entry_string(NmissingTIME, Nsamples),
                                `N ignored as individual ignored` = entry_string(NignoredI,    Nsamples),
                                `N ignored in summaries`          = entry_string(NignoredSUM,  Nsamples),
                                `N ignored in NCA`                = entry_string(NignoredNCA,  Nsamples)
                              )
                            }))
  if (FLAGpatients) names(tab) <- gsub("subjects", "patients", names(tab))
  sub_labels <- mylabels[intersect(names(mylabels), names(tab))]
  for (k in seq_along(sub_labels)) {
    names(tab)[names(tab) == names(sub_labels)[k]] <- sub_labels[[k]]
  }
  footer <- "N: Number of; BLOQ: Below limit of quantification<br>^a^ Prior to potential imputations.<br>^b^ Neither actual nor nominal time available.<br>"
  if (!is.null(footerAddText)) footer <- paste0(footer, "<br>", footerAddText)
  if (is.null(tableTitle)) {
    tableTitle <- "Summary of available observations"
  }
    if (!is.null(filename)) {
      filename <- paste0(aux_strrep(filename, ".txt", ""),
                         ".txt")
      IQRoutputTable(xtable = tab, xfooter = footer,
                     xtitle = tableTitle, report = report, filename = filename)
      return(invisible(NULL))
    }
    out <- IQRoutputTable(xtable = tab, xfooter = footer,
                               xtitle = tableTitle, report = report)
    out
}
#' Generate a detailed listing of individual pharmacokinetic concentrations
#'
#' The listing is split by a user-given column and default to USUBJID.
#' If a filename is given, an rmd file is generated allowing easy reporting in Word with IQReport. Otherwise,
#' a IQncaTableList object is returned containing all tables, titles, and footers which can be exported as an rmd file using
#' [write_IQncaTableList].
#' Important: Ignored records (INGORER) are not reported in these listings! Ignored subjects (IGNOREI) are reported
#' but it is indicated that they are ignored (not in the simple version)
#'
#' @param data IQdataNCA object
#' @param table_number Character string with listing number information, added to the title for each table, if defined
#' @param table_split_by Column name to define the split to different tables (pages)
#' @param table_head Column names of covariates unique within splits to be displayed above the table
#' @param table_cols Column names to be displayed as table columns
#' @param labels Named vector with labels that should be used for column names or as name for the common covariates.
#' @param roundfun Rounding function or named list of rounding function (expecting the numeric vector and number of digits to be input arguments). Defaults to [signif]. Suggested alternative is [round].
#' @param digits Named vector with digits to be used for rounding for different columns
#' @param fontsize Fontsize to be used in the table
#' @param page_orientation Page orientation, 'landscape' or 'portrait'
#' @param filename Filename to export the listing IQRrmd object to
#'
#' @return An IQRrmd object exported to a file or IQncaTableList
#' @export
#' @family TLF customized
listing_PKconc_IQdataNCA <- function (
    data,
    table_number     = NULL,
    table_split_by   = "USUBJID",
    table_head       = c("SEX", "AGE", "RACE"),
    table_cols       = c("PROFILE","GROUP","DOSE"),
    labels           = NULL,
    roundfun         = signif,
    digits           = NULL,
    fontsize         = 8,
    page_orientation = c("landscape", "portrait"),
    filename         = NULL
) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  if (!(is.character(table_split_by) & length(table_split_by) == 1)) stopIQR("'table_split_by' needs to be character of length 1.")
  if (!(is.character(table_head)|is.null(table_head))) stopIQR("'table_head' needs to be character vector or NULL.")
  if (!is.character(table_cols)) stopIQR("'table_cols' needs to be character vector.")
  if (!all(table_split_by %in% names(data))) stopIQR(paste0("Unknown column specified as 'table_split_by': ", paste0(setdiff(table_split_by, names(data)), collapse = ", ")))
  if (!all(table_head %in% names(data))) stopIQR(paste0("Unknown column specified as 'table_head': ", paste0(setdiff(table_head, names(data)), collapse = ", ")))
  if (!all(table_cols %in% names(data))) stopIQR(paste0("Unknown column specified as 'table_cols': ", paste0(setdiff(table_cols, names(data)), collapse = ", ")))
  data <- removeFrom_IQdataNCA(data = data,
                                       FLAGremoveIGNOREI = FALSE,
                                       FLAGremoveIGNORER = TRUE,
                                       FLAGremoveIGNORSUM = FALSE,
                                       FLAGremoveIGNORNCA = FALSE)
  data <- data[!is.na(data$NTIME),]
  message("Generating PK concentration listings ...")
  if (!is.null(filename)) {
    filename <- paste0(gsub(".rmd","",filename),".rmd")
  }
  data$SPLIT <- data[[table_split_by]]
  allSPLIT <- unique(data$SPLIT)
  check_common <- unique(as.data.frame(data)[,c("SPLIT",table_head), drop = FALSE])
  idx_dub <- which(duplicated(check_common$SPLIT))
  if (length(idx_dub) > 0) {
    stopIQR("Values to be displayed as subtitle ('table_head') not unique for the split used.")
  }
  if ( !is.function(roundfun) ) {
    if (!setequal(names(roundfun), names(digits))) stopIQR("If 'roundfun' is not a single function, it needs to have the same names as 'digits'.")
    if (!all(sapply(roundfun, class) %in% "function")) stopIQR("If 'roundfun' is not a single function, it needs to be a vector of functions.")
  }
  tabs <- lapply(seq_along(allSPLIT), function(k) {
    d <- data[data$SPLIT == allSPLIT[k],]
    tab <- rawlisting_PKconc_IQdataNCA(d = d,
                                       split_index    = k,
                                       table_number   = table_number,
                                       table_split_by = table_split_by,
                                       table_head     = table_head,
                                       table_cols     = table_cols,
                                       labels         = labels,
                                       filename       = filename,
                                       roundfun       = roundfun,
                                       digits         = digits)
    tab
  })
  mytitle <- .listing_concdetailed_pkconc
  mytitle <- updateListingNumberTitle_IQdataNCA(mytitle,table_number,listingindex = NULL)
  tabs <- list(content = tabs,
               title = mytitle,
               filename = filename,
               number = table_number,
               type = "listing")
  class(tabs) <- c("IQncaTableList", class(tabs))
  if (!is.null(filename)) {
    write_IQncaTableList(tabs, fontsize = fontsize, filename, page_orientation = page_orientation)
  } else {
    return(tabs)
  }
}
#' Generates lising of individual PK concentrations for a single split of the IQdataNCA
#'
#' IGNORER records are REALLY not reported ... so that should be HANDLED WITH CARE!
#'
#' @param d IQncaData object to be display in one split
#' @param table_number Character string with listing number information, added to the title for each table, if defined
#' @param split_index Number of current split
#' @param table_split_by Column that the split was done by (for labelling)
#' @param table_head Column names of covariates unique within splits to be displayed above the table
#' @param table_cols Column names to be displayed as table columns
#' @param labels Named vector with labels that should be used for column names or as name for the common covariates.
#' @param roundfun Rounding function (expecting the numeric vector and number of digits to be input arguments). Defaults to [signif]. Suggested alternative is [round].
#' @param digits Named vector with digits to be used for rounding for different columns
#' @param filename Filename to export the listing IQRrmd object to
#'
#' @return IQncaTable object
rawlisting_PKconc_IQdataNCA <- function (
    d,
    table_number = NULL,
    split_index  = 1,
    table_split_by,
    table_head,
    table_cols,
    labels,
    roundfun      = signif,
    digits        = NULL,
    filename      = NULL
) {
  deflabels <- get_default_labels(d)
  mylabels  <- c(deflabels[setdiff(names(deflabels), names(labels))], labels)
  dd <- d[is.na(d$IGNORER),]
  tab <- as.data.frame(dd)
  tab <- apply_digits(tab, digits, f = roundfun)
  tab$CONC[tab$BLLOQ %in% 1] <- paste0("BLLOQ (<",tab$LLOQ[tab$BLLOQ %in% 1],")")
  tab$BLLOQ <- NULL
  tab$LLOQ <- NULL
  tab$CONC[is.na(tab$CONC)] <- "NV"
  tab$COMMENT <- tab$COMMENTR
  tab$COMMENT[is.na(tab$COMMENT)] <- ""
  tab$COMMENTR <- NULL
  tab$CONC <- sapply(seq_along(tab$CONC), function (k) {
    out <- paste0(tab$CONC[k]," ")
    if (!is.na(tab$IGNORSUM[k])) out <- paste0(out,"o")
    if (!is.na(tab$IGNORNCA[k])) out <- paste0(out,"+")
    aux_strtrim(out)
  })
  tab$IGNORSUM[is.na(tab$IGNORSUM)] <- ""
  tab$IGNORNCA[is.na(tab$IGNORNCA)] <- ""
  tab$IGNORE <- paste0(tab$IGNORSUM,":::",tab$IGNORNCA)
  tab$IGNORE <- gsub("^:::","",tab$IGNORE)
  tab$IGNORSUM <- NULL
  tab$IGNORNCA <- NULL
  tab$IGNORE[!is.na(tab$IGNOREI)] <- paste0("Subject ignored: ",tab$IGNOREI[!is.na(tab$IGNOREI)])
  tab$COMMENT <- unname(sapply(tab$COMMENT, function (c) {
    if (!grepl(":::",c)) return(c)
    paste0(unique(aux_explode(c,separator = ":::")),collapse = ", ")
  }))
  tab$IGNORE <- unname(sapply(tab$IGNORE, function (c) {
    if (!grepl(":::",c)) return(c)
    paste0(unique(aux_explode(c,separator = ":::")),collapse = ", ")
  }))
  subtitle <- lapply(table_head, function(cc) {
    tab[[cc]][1]
  })
  names(subtitle) <- table_head
  labeled_common_covs <- intersect(table_head, names(mylabels))
  names(subtitle)[match(labeled_common_covs, names(subtitle))] <- mylabels[labeled_common_covs]
  tab <- tab[, table_cols]
  conserved_cols <- c("IX", "ATIME", "NTIME", "TIME", "PCDTC", "EXSTDTC", "CONC", "BLLOQ", "LLOQ", "IGNORE", "COMMENT")
  cols_to_clean <- setdiff(names(tab), conserved_cols)
  tab_grouping <- tab[,cols_to_clean]
  for (kcol in seq_along(cols_to_clean)) {
    if (kcol == 1) {
      tab[[cols_to_clean[kcol]]] <- remove_duplicates(tab[[cols_to_clean[kcol]]])
    } else {
      col_grouping <- do.call(paste, c(tab_grouping[1:kcol], sep = "_"))
      tab[[cols_to_clean[kcol]]] <- remove_duplicates(tab[[cols_to_clean[kcol]]], group = col_grouping)
    }
  }
  for (k in seq_along(mylabels)) {
    tab       <- addLabel(tab, names(mylabels)[k], mylabels[k])
  }
  loadSetupOptions_IQnca()
  mytitle <- .listing_concdetailed_pkconc
  mytitle <- updateListingNumberTitle_IQdataNCA(mytitle, table_number, split_index)
  if (!is.null(table_split_by)) {
    if (table_split_by %in% names(mylabels)) {
      split_label <- mylabels[[table_split_by]]
    } else {
      split_label <- table_split_by
    }
    mytitle <- paste0(mytitle, " - ", split_label, ": ", d[[table_split_by]][1])
  }
  scriptName <- getScriptName_IQdataNCA()
  if (dd$FLAGTIME[1]=="nominal") analysistimetext <- "Analysis time set to nominal time."
  if (dd$FLAGTIME[1]=="actual") {
    if (dd$FATIMIMP[1]=="nominal") {
      analysistimetext <- "Analysis time set to actual time with imputation of missing values from nominal time."
    } else {
      analysistimetext <- "Analysis time set to actual time."
    }
  }
  footertext <- paste0(
    "NV: No value collected.\n",
    "o Value was not considered for summary and inferential procedures.\n",
    "+ Value was excluded from estimation of PK parameters.\n",
    "Values <LLOQ were reported as 'BLLOQ (<LLOQ)', where LLOQ was replaced by the actual value.\n",
    analysistimetext, "\n",
    {if ("COMMENT" %in% names(tab)) "^a^ Includes reporting of selected method for BLLOQ record handling in descriptive statistics (summary tables).\n"},
    "IQnca version: ", aux_version(pkgName = "IQnca"), "\n",
    "Script: ", scriptName,"\n",
    "Output: ",filename, "\n",
    "Execution date: ", Sys.time()
  )
  out <- list(
    content  = tab,
    title    = mytitle,
    subtitle = subtitle,
    footer   = footertext,
    number   = table_number,
    index    = split_index,
    type     = "listing"
  )
  class(out) <- c("IQncaTable", class(out))
  out
}
#' Generate a detailed listing of individual pharmacokinetic parameters
#'
#' The listing is split by a user-given column and defaults to USUBJID.
#' If a filename is given, an rmd file is generated allowing easy reporting in Word with IQReport. Otherwise,
#' a IQncaTableList object is returned containing all tables, titles, and footers which can be exported as an rmd file using
#' [write_IQncaTableList].
#'
#' As a default, PK parameters are displayed in rows which can be switched to a column-wise table.
#'
#' @param data IQdataNCA object
#' @param table_number Character string with listing number information, added to the title for each table, if defined
#' @param table_split_by Column name to define the split to different tables
#' @param table_head Column names of covariates unique within splits to be displayed above the table
#' @param table_cols Column names to be displayed as table columns
#' @param table_pkpars Character string defining the PK parameters to report in the table
#'   By default these parameters will be selected based on the "Standard" parameters defined in the IQparamNCA_Specification.xlsx
#'   document, subject to the profile and administration type. The user can select "All" or "Standard".
#'   Alternatively, parameterReport can also be a vector or parameter names (based on PKPARAMCD in IQparamNCA_Specification.xlsx).
#'   Instead of the standard or all ones then these will be considered in the listing (also subject to profile and
#'   administration type).
#' @param pkpars_as_cols flag whether to display parameters in colums. Defaults to FALSE such that parameters are listed in rows.
#' @param labels Named vector with labels that should be used for column names or as name for the common covariates.
#' @param roundfun Rounding function (expecting the numeric vector and number of digits to be input arguments). Defaults to [signif]. Suggested alternative is [round].
#' @param digits Named vector with digits to be used for rounding for different columns
#' @param fontsize Fontsize to be used in the table
#' @param page_orientation Page orientation, 'landscape' or 'portrait'
#' @param filename Filename to export the listing IQRrmd object to
#'
#' @return An IQRrmd object exported to a file or IQncaTableList
#' @export
#' @family TLF customized
#'
#' @examples
#' \dontrun{
#' 
#' listing_PKpars_IQnca(
#'   resNCA,
#'   table_number = "2.1",
#'   table_split_by = "USUBJID",
#'   table_cols = c("DOSE", "FORM"),
#'   table_head = c("USUBJID","SEX", "AGE", "RACE", "SEQUENCE"),
#'   table_pkpars = c("TMAX", "CMAX", "AUCLST", "AUCTAU", "LAMZHL", "AUCPEP"),
#'   roundfun = list("TMAX" = round, "CMAX" = round, "AUCLST" = round, "AUCTAU" = round, "LAMZHL" = signif, "AUCPEP" = round),
#'   digits   = c("TMAX" = Inf, "CMAX" = 2, "AUCLST" = 1, "AUCTAU" = 1, "LAMZHL" = 4, "AUCPEP" = 2),
#'   filename = file.path(.outputFolder, "01_lising_pars")
#' )
#'
#' 
#' listing_PKpars_IQnca(
#'   resNCA,
#'   table_number = "2.1",
#'   table_split_by = "PROFILE",
#'   table_cols = "USUBJID",
#'   table_head = c("FORM","DOSE"),
#'   table_pkpars = c("TMAX", "CMAX", "AUCLST", "AUCTAU", "LAMZHL", "AUCPEP"),
#'   pkpars_as_cols = TRUE,
#'   roundfun = list("TMAX" = round, "CMAX" = round, "AUCLST" = round, "AUCTAU" = round, "LAMZHL" = signif, "AUCPEP" = round),
#'   digits   = c("TMAX" = Inf, "CMAX" = 2, "AUCLST" = 1, "AUCTAU" = 1, "LAMZHL" = 4, "AUCPEP" = 2),
#'   filename = file.path(.outputFolder, "02_lising_pars_wide")
#' )
#' }
listing_PKpars_IQnca <- function (
    data,
    table_number     = NULL,
    table_split_by   = "USUBJID",
    table_head       = c("SEX", "AGE", "RACE"),
    table_cols       = c("PROFILE","GROUP","DOSE"),
    table_pkpars  = "standard",
    pkpars_as_cols  = FALSE,
    labels           = NULL,
    roundfun         = signif,
    digits           = 4,
    fontsize         = 8,
    page_orientation = c("landscape", "portrait"),
    filename         = NULL
) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  if (!(is.character(table_split_by) & length(table_split_by) == 1)) stopIQR("'table_split_by' needs to be character of length 1.")
  if (!(is.character(table_head)|is.null(table_head))) stopIQR("'table_head' needs to be character vector or NULL.")
  if (!is.character(table_cols)) stopIQR("'table_cols' needs to be character vector.")
  if (!all(table_split_by %in% names(data))) stopIQR(paste0("Unknown column specified as 'table_split_by': ", paste0(setdiff(table_split_by, names(data)), collapse = ", ")))
  if (!all(table_head %in% names(data))) stopIQR(paste0("Unknown column specified as 'table_head': ", paste0(setdiff(table_head, names(data)), collapse = ", ")))
  if (!all(table_cols %in% names(data))) stopIQR(paste0("Unknown column specified as 'table_cols': ", paste0(setdiff(table_cols, names(data)), collapse = ", ")))
  if (!is.character(table_pkpars)) stopIQR("'table_pkpars' needs to be either 'standard', 'all', or a character vector containing parameters names to report (PARAMCD in IQparamNCA_Specification.xlsx).")
  if (!is.null(filename)) {
    filename <- paste0(gsub(".rmd","",filename),".rmd")
  }
  message("Generating PK parameter listings ...")
  spec_pkpars <- attr(data, "paramspec")
  spec_aucint <- attr(data, "intervalAUC")
  info <- getValidityPKparam_IQRnca(data = data, parameterReport = table_pkpars)
  pkparamInfo  <- info$pkparamInfo
  userSelected <- info$userSelected
  if (!userSelected) {
    table_pkpars <- unique(pkparamInfo$PARAMCD)
  } else {
    pkparamInfo <- pkparamInfo[pkparamInfo$PARAMCD %in% table_pkpars,]
  }
  cols_from_input <- unique(c("USUBJID", "PROFILE", table_cols, table_head, table_split_by))
  pardata <- dplyr::left_join(
    pkparamInfo,
    unique(data[,cols_from_input]),
    by = intersect(names(pkparamInfo), cols_from_input)
  )
  def_digits <- rep(NA, length(table_pkpars))
  names(def_digits) <- table_pkpars
  mydigits <- c(def_digits[setdiff(names(def_digits), names(digits))], digits)
  myroundfun <- roundfun
  if (!is.function(roundfun)) {
    def_roundfun <- structure(lapply(1:length(mydigits), function(o) signif), names = names(mydigits))
    myroundfun   <- c(def_roundfun[setdiff(names(def_roundfun), names(roundfun))], roundfun)
  }
  pardata$SPLIT <- pardata[[table_split_by]]
  allSPLIT <- unique(pardata$SPLIT)
  check_common <- unique(as.data.frame(pardata)[,c("SPLIT",table_head), drop = FALSE])
  idx_dub <- which(duplicated(check_common$SPLIT))
  if (length(idx_dub) > 0) {
    stopIQR("Values to be displayed as subtitle ('table_head') not unique for the split used.")
  }
  tabs <- lapply(seq_along(allSPLIT), function(k) {
    d <- pardata[pardata$SPLIT == allSPLIT[k],]
    tab <- rawlisting_PKpars_IQnca(
      d = d,
      table_split_by = table_split_by,
      table_head = table_head,
      table_cols = table_cols,
      table_pkpars = table_pkpars,
      pkpars_as_cols = pkpars_as_cols,
      labels = labels,
      split_index = k,
      table_number = table_number,
      filename = filename,
      roundfun = myroundfun,
      digits = mydigits
    )
    tab
  })
  mytitle <- .listing_pkparameter_pkconc
  mytitle <- updateListingNumberTitle_IQdataNCA(mytitle,table_number,listingindex = NULL)
  tabs <- list(content = tabs,
               title = mytitle,
               filename = filename,
               number = table_number,
               type = "listing")
  class(tabs) <- c("IQncaTableList", class(tabs))
  if (!is.null(filename)) {
    write_IQncaTableList(tabs, fontsize = fontsize, filename, page_orientation = page_orientation)
  } else {
    return(tabs)
  }
}
#' Generates lising of individual PK parameters for a single split of the IQdataNCA
#'
#' @param d IQnca object to be display in one split
#' @param table_number Character string with listing number information, added to the title for each table, if defined
#' @param split_index Number of current split
#' @param table_split_by Column that the split was done by (for labelling)
#' @param table_head Column names of covariates unique within splits to be displayed above the table
#' @param table_cols Column names to be displayed as table columns
#' @param table_pkpars Character string defining the PK parameters to report in the table
#' @param pkpars_as_cols flag whether to display parameters in colums. Defaults to FALSE such that parameters are listed in rows.
#' @param labels Named vector with labels that should be used for column names or as name for the common covariates.
#' @param roundfun Rounding function (expecting the numeric vector and number of digits to be input arguments). Defaults to [signif]. Suggested alternative is [round].
#' @param digits Named vector with digits to be used for rounding for different columns
#' @param filename Filename to export the listing IQRrmd object to
#'
#' @return IQncaTable object
rawlisting_PKpars_IQnca <- function(
    d,
    table_number=NULL,
    split_index,
    table_split_by,
    table_head,
    table_cols,
    table_pkpars,
    pkpars_as_cols,
    labels,
    digits,
    roundfun,
    filename
) {
  loadSetupOptions_IQnca()
  deflabels <- get_default_labels(attr(d, "data"))
  mylabels  <- c(deflabels[setdiff(names(deflabels), names(labels))], labels)
  dtrans <- tidyr::pivot_wider(d[,c("PROFILE", "USUBJID", "PARAMCD", "VALUE")], names_from = "PARAMCD", values_from = "VALUE")
  dtrans <- apply_digits(dtrans, digits, f = roundfun)
  dround <- tidyr::pivot_longer(dtrans, cols = dplyr::all_of(table_pkpars), names_to = "PARAMCD", values_to = "VALUE")
  d <- dplyr::left_join(subset(d, select = -VALUE), dround, by = c("PROFILE", "USUBJID", "PARAMCD"))
  d$VALUE <- as.character(d$VALUE)
  d$VALUE[is.na(d$VALUE)] <- "NC"
  d$REASONNA = aux_strtrim(d$REASONNA)
  d$REASONNOTRELIABLE = aux_strtrim(d$REASONNOTRELIABLE)
  ix_reason_NA <- nchar(d$REASONNA) > 0
  d$VALUE[ix_reason_NA]  <- paste0(d$VALUE[ix_reason_NA]," ^",gsub(" ","^ ^",d$REASONNA[ix_reason_NA],fixed = TRUE),"^")
  d$REASONNOTRELIABLE[nchar(d$REASONNA) > 0] <- ""
  ix_reason_NR <- nchar(d$REASONNOTRELIABLE) > 0
  d$VALUE[ix_reason_NR]  <- paste0(d$VALUE[ix_reason_NR]," ^",gsub(" ","^ ^",d$REASONNOTRELIABLE[ix_reason_NR],fixed = TRUE),"^")
  d$PARAMCOL <- paste0(d$NAME, " (",d$UNIT,")")
  subtitle <- lapply(table_head, function(cc) {
    d[[cc]][1]
  })
  names(subtitle) <- table_head
  labeled_common_covs <- intersect(table_head, names(mylabels))
  names(subtitle)[match(labeled_common_covs, names(subtitle))] <- mylabels[labeled_common_covs]
  d$PARAMCD       <- factor(d$PARAMCD, levels = table_pkpars)
  parameter_order <- unique(d[, c("PARAMCD", "PARAMCOL")])
  parameter_order$PARAMCD <- factor(parameter_order$PARAMCD, levels = table_pkpars)
  parameter_order <- dplyr::arrange(parameter_order, PARAMCD)
  d$PARAMCOL      <- factor(d$PARAMCOL, levels = parameter_order$PARAMCOL)
  tab <- d[,c(table_cols, "PARAMCOL", "VALUE")]
  tab$XXX <- Reduce(function(c1,c2) paste0(c1,"_",c2), tab[,table_cols])
  tab <- do.call(rbind, lapply(split(tab, tab$XXX), function(x) {
    dplyr::arrange(x, PARAMCOL)
  }) )
  tab$XXX <- NULL
  if (pkpars_as_cols) {
    tab <- tidyr::pivot_wider(tab, names_from = "PARAMCOL", values_from = "VALUE")
  }
  cols_to_clean <- table_cols
  tab_grouping <- tab[,cols_to_clean]
  for (kcol in seq_along(cols_to_clean)) {
    if (kcol == 1) {
      tab[[cols_to_clean[kcol]]] <- remove_duplicates(tab[[cols_to_clean[kcol]]])
    } else {
      col_grouping <- do.call(paste, c(tab_grouping[1:kcol], sep = "_"))
      tab[[cols_to_clean[kcol]]] <- remove_duplicates(tab[[cols_to_clean[kcol]]], group = col_grouping)
    }
  }
  for (k in seq_along(mylabels)) {
    tab       <- addLabel(tab, names(mylabels)[k], mylabels[k])
  }
  mytitle <- .listing_pkparameter_pkconc
  mytitle <- updateListingNumberTitle_IQdataNCA(mytitle, table_number, split_index)
  if (!is.null(table_split_by)) {
    if (table_split_by %in% names(mylabels)) {
      split_label <- mylabels[[table_split_by]]
    } else {
      split_label <- table_split_by
    }
    mytitle <- paste0(mytitle, " - ", split_label, ": ", d[[table_split_by]][1])
  }
  scriptName <- getScriptName_IQdataNCA()
  footertext <- paste0(
    "NC: Not calculated.\n",
    ifelse (any(grepl(.footnoteChar_LAMZ_NA,d$REASONNA,fixed = TRUE)), paste0("^",.footnoteChar_LAMZ_NA,"^"," ","Value not calculated. Reason: terminal slope could not be determined. Value was not considered for summary and inferential procedures.\n") ,""),
    ifelse (any(grepl(.footnoteChar_DOSE0_NA,d$REASONNA,fixed = TRUE)), paste0("^",.footnoteChar_DOSE0_NA,"^"," ","Value not calculated. Reason: dose was 0. Value was not considered for summary and inferential procedures.\n") ,""),
    ifelse (any(grepl(.footnoteChar_ISSUE_UNCAUGHT_NA,d$REASONNA,fixed = TRUE)), paste0("^",.footnoteChar_ISSUE_UNCAUGHT_NA,"^"," ","Value not calculated. Reason: undefined. Value was not considered for summary and inferential procedures.\n") ,""),
    ifelse (any(grepl(.footnoteChar_IGNOREDSUBJECT_NA,d$REASONNA,fixed = TRUE)), paste0("^",.footnoteChar_IGNOREDSUBJECT_NA,"^"," ","Value not calculated. Reason: Subject ignored with reason '",d$IGNOREI[1],"'.\n") ,""),
    ifelse (any(grepl(.footnoteChar_SPAN_LOW,d$REASONNOTRELIABLE,fixed = TRUE)), paste0("^",.footnoteChar_SPAN_LOW,"^"," ","Value not reliably calculated. Reason: SPAN<",.SPAN_MIN,". Value was not considered for summary and inferential procedures.\n") ,""),
    ifelse (any(grepl(.footnoteChar_LAMZNPT_LOW,d$REASONNOTRELIABLE,fixed = TRUE)), paste0("^",.footnoteChar_LAMZNPT_LOW,"^"," ","Value not reliably calculated. Reason: LAMZNPT<",.LAMZNPT_MIN,". Value was not considered for summary and inferential procedures.\n") ,""),
    ifelse (any(grepl(.footnoteChar_R2ADJ_LOW,d$REASONNOTRELIABLE,fixed = TRUE)), paste0("^",.footnoteChar_R2ADJ_LOW,"^"," ","Value not reliably calculated. Reason: R2ADJ<",.R2ADJ_MIN,". Value was not considered for summary and inferential procedures.\n") ,""),
    ifelse (any(grepl(.footnoteChar_AUCOEXTR_HIGH,d$REASONNOTRELIABLE,fixed = TRUE)), paste0("^",.footnoteChar_AUCOEXTR_HIGH,"^"," ","Value not reliably calculated. Reason: extrapolated AUC (observed)>",.AUCEXTRAP_MAX,"%. Value was not considered for summary and inferential procedures.\n") ,""),
    ifelse (any(grepl(.footnoteChar_AUCPEXTR_HIGH,d$REASONNOTRELIABLE,fixed = TRUE)), paste0("^",.footnoteChar_AUCPEXTR_HIGH,"^"," ","Value not reliably calculated. Reason: extrapolated AUC (predicted)>",.AUCEXTRAP_MAX,"%. Value was not considered for summary and inferential procedures.\n") ,""),
    "IQnca version: ", aux_version(pkgName = "IQnca"), "\n",
    "Script: ", scriptName,"\n",
    "Output: ",filename, "\n",
    "Execution date: ", Sys.time()
  )
  out <- list(
    content  = tab,
    title    = mytitle,
    subtitle = subtitle,
    footer   = footertext,
    number   = table_number,
    index    = split_index,
    type     = "listing"
  )
  class(out) <- c("IQncaTable", class(out))
  out
}
#' Calculate statistics for a value column in a data frame
#'
#' The function calculates various statistics and rounds the values to the given number of digits.
#' Statistics that cannot be determined are given as "NC". The following statistics are calculated:
#'
#' | Keyword   | Description |
#' | --------- | ------------|
#' |   MEAN    | Arithmetic mean |
#' |   SD      | Standard deviation (arithmetic) |
#' |   SE      | Standard error (arithmetic) |
#' |   CVPMN   | Coefficient of variation in % (arithmetic) |
#' |   CI95MN  | 95%-confidence interval for arithmetic mean  |
#' |   CI90MN  | 90%-confidence interval for arithmetic mean  |
#' |   GMEAN   | Geometric mean   |
#' |   GSD     | Standard error (geometric) |
#' |   CVPGM   | Coefficient of variation in % (geometric)  |
#' |   CI95GM  | 95%-confidence interval for geometric mean |
#' |   CI90GM  | 90%-confidence interval for geometric mean |
#' |   MEDIAN  | Median |
#' |   MIN     | Minimum value |
#' |   MAX     | Maximum value |
#'
#' @param d data frame
#' @param value_col character indicating the column in the data frame to be summarized
#' @param digits Number of digits to to be used for rounding (if none given, the default of the applied function is used.).
#'               Single numeric to be applied to all. Named numeric vector to specify digits for stats separately.
#'               Names need to correspond to the keyword for the respective statistic.
#' @param roundfun Function or list of functions to be used for rounding (defaults to [signif]). If a list is given, names need to correspond
#'    to the keyword for the respective statistic.
#'    It is assumed that the given rounding function has two inputs: numeric to be rounded integer defining the number of digits used for rounding
#'
#' @md
#' @return data frame with one row containing the statistics for value column of input dataset
#'
#' @examples
#' \dontrun{
#' par_stats <- calc_summary_stats(
#'   dataNCA,
#'   value_col = "CONC",
#'   digits = c(MEAN = 3, SD = 4, GMEAN= 3, GSD = 4))
#' }
calc_summary_stats <- function(d, value_col, digits = NULL, roundfun = signif) {
  stat_cols <- c("MEAN", "SD", "SE", "CVPMN", "CI95MN", "CI90MN", "GMEAN", "GSD", "CVPGM", "CI95GM", "CI90GM", "MEDIAN", "MIN", "MAX")
  if (!all(names(digits) %in% stat_cols)) {
    digmiss <- setdiff(names(digits), stat_cols)
    stopIQR(paste0("Digits for rounding set for unknown statistic: ", paste0(digmiss, collapse = ", ")))
  }
  if (!is.null(digits) & is.null(names(digits))) def_digit <- digits else def_digit <- NA
  def_digits_stat <- rep(def_digit, length(stat_cols))
  names(def_digits_stat) <- stat_cols
  if (!is.null(names(digits))) {
    digits_stat <- c(def_digits_stat[setdiff(names(def_digits_stat), names(digits))], digits)
  } else {
    digits_stat <- def_digits_stat
  }
  if (is.function(roundfun)) def_roundfun <- roundfun else def_roundfun <- signif
  def_roundfun_stat <- structure(lapply(1:length(stat_cols), function(o) def_roundfun), names = stat_cols)
  if (!is.null(names(roundfun))) {
    roundfun_stat <- c(def_roundfun_stat[setdiff(names(def_roundfun_stat), names(roundfun))], roundfun)
  } else {
    roundfun_stat <- def_roundfun_stat
  }
  digits_stat[["CI95MNl"]] <- digits_stat[["CI95MNu"]] <- digits_stat[["CI95MN"]]
  digits_stat[["CI90MNl"]] <- digits_stat[["CI90MNu"]] <- digits_stat[["CI90MN"]]
  digits_stat[["CI95GMl"]] <- digits_stat[["CI95GMu"]] <- digits_stat[["CI95GM"]]
  digits_stat[["CI90GMl"]] <- digits_stat[["CI90GMu"]] <- digits_stat[["CI90GM"]]
  digits_stat <- digits_stat[-which(names(digits_stat) %in% c("CI95MN", "CI90MN", "CI95GM", "CI90GM"))]
  roundfun_stat[["CI95MNl"]] <- roundfun_stat[["CI95MNu"]] <- roundfun_stat[["CI95MN"]]
  roundfun_stat[["CI90MNl"]] <- roundfun_stat[["CI90MNu"]] <- roundfun_stat[["CI90MN"]]
  roundfun_stat[["CI95GMl"]] <- roundfun_stat[["CI95GMu"]] <- roundfun_stat[["CI95GM"]]
  roundfun_stat[["CI90GMl"]] <- roundfun_stat[["CI90GMu"]] <- roundfun_stat[["CI90GM"]]
  summ <- calc_stats_raw(d, value_col = value_col)
  summ <- apply_digits(summ, digits = digits_stat, f = roundfun_stat)
  summ <- within(summ, {
    SD        = ifelse(is.na(SD),"NC",summ$SD)
    SE        = ifelse(is.na(SE),"NC",summ$SE)
    CVPMN     = ifelse(MEAN == 0 | is.na(summ$CVPMN),"NC",summ$CVPMN)
    CI95MN    = ifelse(is.na(SD), "NC", paste0(CI95MNl, " - ", CI95MNu))
    CI90MN    = ifelse(is.na(SD), "NC", paste0(CI90MNl, " - ", CI90MNu))
    GMEAN     = ifelse(any(0 %in%  d$CONC),"NC",GMEAN)
    GSD       = ifelse(is.na(GSD),"NC", GSD)
    CVPGM     = ifelse(any(0 %in%  d$VALUE) | is.na(CVPGM),"NC",CVPGM)
    CI95GM    = ifelse(is.na(GSD), "NC", paste0(CI95GMl, " - ", CI95GMu))
    CI90GM    = ifelse(is.na(GSD), "NC", paste0(CI90GMl, " - ", CI90GMu))
  })
  summ <- summ[, c("N", stat_cols)]
  summ
}
#' Calculation of various statistics
#'
#' Returns data frame with unformatted, numerical results
#'
#' @param d data frame over which rows summary stats will be calculated
#' @param value_col column in data frame which is summarized
#'
#' @return data frame
#'
calc_stats_raw <- function(d, value_col) {
  d$VALUE <- d[[value_col]]
  N       <- length(d$VALUE[!is.na(d$VALUE)])           
  MEAN    <- mean(d$VALUE, na.rm = TRUE)
  SD      <- sd(d$VALUE, na.rm = TRUE)
  SE      <- SD/sqrt(N)
  CVPMN   <- suppressWarnings(100*SD/abs(MEAN))
  CI95MNl <- MEAN + SE*stats::qnorm(0.025)
  CI95MNu <- MEAN + SE*stats::qnorm(0.975)
  CI90MNl <- MEAN + SE*stats::qnorm(0.05)
  CI90MNu <- MEAN + SE*stats::qnorm(0.95)
  GMEAN   <- suppressWarnings(geomean(d$VALUE, na.rm = TRUE))
  GSD     <- suppressWarnings(geosd(d$VALUE, na.rm = TRUE))
  GSE     <- exp(log(GSD)/sqrt(N))
  CVPGM   <- suppressWarnings(geocv(d$VALUE, na.rm = TRUE))
  CI95GMl <- exp(log(GMEAN) + log(GSE)*stats::qnorm(0.025))
  CI95GMu <- exp(log(GMEAN) + log(GSE)*stats::qnorm(0.975))
  CI90GMl <- exp(log(GMEAN) + log(GSE)*stats::qnorm(0.05))
  CI90GMu <- exp(log(GMEAN) + log(GSE)*stats::qnorm(0.95))
  MEDIAN  <- median(d$VALUE, na.rm = TRUE)
  MIN     <- min(d$VALUE, na.rm = TRUE)
  MAX     <- max(d$VALUE, na.rm = TRUE)
  P025    <- quantile(d$VALUE, probs = 0.025, na.rm = TRUE)
  P05     <- quantile(d$VALUE, probs = 0.05, na.rm = TRUE)
  P95     <- quantile(d$VALUE, probs = 0.95, na.rm = TRUE)
  P975    <- quantile(d$VALUE, probs = 0.975, na.rm = TRUE)
  summ <- data.frame(
    N       = N       ,
    MEAN    = MEAN    ,
    SD      = SD      ,
    SE      = SE      ,
    CVPMN   = CVPMN   ,
    CI95MNl = CI95MNl ,
    CI95MNu = CI95MNu ,
    CI90MNl = CI90MNl ,
    CI90MNu = CI90MNu ,
    GMEAN   = GMEAN   ,
    GSD     = GSD     ,
    CVPGM   = CVPGM   ,
    CI95GMl = CI95GMl ,
    CI95GMu = CI95GMu ,
    CI90GMl = CI90GMl ,
    CI90GMu = CI90GMu ,
    MEDIAN  = MEDIAN  ,
    MIN     = MIN     ,
    MAX     = MAX     ,
    P025    = P025    ,
    P05     = P05     ,
    P95     = P95     ,
    P975    = P975
  )
  summ
}
#' Define default labels for columns for IQdataNCA object
#'
#' @param d IQdataNCA object
#'
#' @return Named vector of labels. Names are column names for which labels apply
#' @export
#' @examples
#' \dontrun{
#' column_labels <- get_default_labels(dataNCA)
#' }
get_default_labels <- function(d) {
  c(
    IX        = "Sample index",
    USUBJID   = "Subject",
    PROFILE   = "Profile",
    GROUP     = "Group",
    DOSE      = paste0("Dose [",d$DOSEUNIT[1],"]"),
    ATIME     = paste0("Actual time [",getTIMEUNITname_IQdataNCA(d),"]"),
    NTIME     = paste0("Nominal time [",getTIMEUNITname_IQdataNCA(d),"]"),
    TIME      = paste0("Analysis time [",getTIMEUNITname_IQdataNCA(d),"]"),
    PCDTC     = "Date/Time of PK sampling",
    EXSTDTC   = "Date/Time of dosing",
    PCTPT     = "Time point",
    CONC      = paste0("Concentration [",d$CONCUNIT[1],"]"),
    ACONC     = paste0("Actual concentration [",d$CONCUNIT[1],"]"),
    COMMENT   = "Comment^a^",
    IGNORE    = "Exclusion reason",
    VISIT     = "Visit",
    PERIOD    = "Period of study",
    SEQUENCE  = "Sequence",
    COUNTRY   = "Country identifier",
    COMPOUND  = "Compound",
    ANALYTE   = "Analyte",
    MATRIX    = "Matrix",
    SITEID    = "Site identifier",
    AGE       = "Age in years",
    SEX       = "Sex",
    RACE      = "Race",
    Statistic = "Statistic",
    N         = "N",
    MEAN      = "Mean",
    SD        = "SD",
    MIN       = "Min",
    MEDIAN    = "Median",
    GMEAN     = "Geo-mean",
    MAX       = "Max",
    CVPMN     = "CV% mean",
    CVPGM     = "CV% geo-mean",
    CI95MN    = "95%-CI mean",
    CI95GM    = "95%-CI geo-mean",
    CI90MN    = "90%-CI mean",
    CI90GM    = "90%-CI geo-mean",
    PARAMCOL  = "Parameter (Unit)",
    VALUE     = "Value"
  )
}
#' Function to apply rounding on columns
#'
#' Note: Need to check that attributes are not lost during rounding!!!
#'
#' @param x Data frame
#' @param digits Named vector with number of digits. Names correspond to the columns in `x` to round.
#' @param f Rounding function. Either named vector of rounding functions or function.
#'
#' @return Numeric vector with rounded values
#'
#' @examples
#' \dontrun{
#' conc_rounded <- apply_digits(
#'   conc,
#'   digits = c(MEAN = 3, SD = 4, GMEAN = 3, GSD = 4)
#' )
#' }
apply_digits <- function(x, digits, f = signif) {
  if (is.function(f)) {
    ff <- structure(lapply(1:length(digits), function(o) f), names = names(digits))
  } else {
    if (!all(names(digits) %in% names(f))) stopIQR("For all columns with given digit the rounding function needs to be defined.")
    ff <- f
  }
  for (k in seq_along(digits)) {
    col <- names(digits)[k]
    sig <- digits[k]
    if (!col %in% names(x)) {
      warningIQR("Digit for rounding given for ", col, " which does not exist, thus not applied.")
    } else {
      if (is.na(sig)) {
        x[[col]] <- ff[[col]](x[[col]])
      } else {
        x[[col]] <- ff[[col]](x[[col]],sig)
      }
    }
  }
  x
}
#' Reduce character to the non-repeating entries
#'
#' Entries which are the same as the preceding one are are replaced by empty character.
#' The inverse of insert_duplicates().
#'
#' @param x character vector.
#' @param group optional vector of the same lenght as x that can be coerced to a factor.
#' If `group` is provided, duplicates are only removed within the same group.
#' @return character of the same length as x.
remove_duplicates <- function(x, group = NULL) {
  x <- as.character(x)
  N <- length(x)
  if (N == 1) return(x)
  if (is.null(group)) group <- rep(1, length(x))
  group <- as.numeric(as.factor(group))
  is_same_as_preceding <- (x[2:N] == x[1:(N-1)]) & (group[2:N] == group[1:(N-1)])
  x[2:N][is_same_as_preceding] <- ""
  x
}
#' Function to define the time column and their labeling used for plotting
#'
#' Column TIMEPLOT is added to the dataset which should be used when plotting the data.
#' The label for the time axis is defined.
#'
#' @param d IQdataNCA object
#' @param FLAGTIME character being either "nominal" (nominal time), "actual" (actual time), or "asis" (use setting as pre-defined in dataset).
#' @param USETAD FLAG whether to use time after last dose (TRUE, default) or time after first dose (FALSE)
#' @param add_unit Flag whether to add unit to axis label (defaults to true)
#'
#' @return list with updated IQdataNCA object and time axis label
handleTIMEplot_IQdataNCA <- function (d, FLAGTIME="nominal", USETAD=TRUE, add_unit = TRUE) {
  if (!is_IQdataNCA(d)) stopIQR("d is not an IQdataNCA object")
  if (!FLAGTIME %in% c("asis","nominal","actual")) stopIQR("time_used argument should be 'asis', 'nominal', or 'actual'")
  if (USETAD) {
    if (FLAGTIME=="asis") {
      d$TIMEPLOT <- d$TIME
      xlabtext <- "Nominal time"
      if (d$FLAGTIME[1] == "actual") {
        xlabtext <- "Actual time"
        if (d$FATIMIMP[1] == "nominal") {
          xlabtext <- "Nominal time"
        }
      }
    }
    if (FLAGTIME=="actual") {
      d$TIMEPLOT <- d$ATIME
      xlabtext <- "Actual time"
    }
    if (FLAGTIME=="nominal") {
      d$TIMEPLOT <- d$NTIME
      xlabtext <- "Nominal time"  }
  } else {
    if (FLAGTIME=="asis") {
      d$TIMEPLOT <- d$TAFD
      xlabtext <- "Nominal time post first dose"
      if (d$FLAGTIME[1] == "actual") {
        xlabtext <- "Actual time post first dose"
        if (d$FATIMIMP[1] == "nominal") {
          xlabtext <- "Actual time post first dose"
        }
      }
    }
    if (FLAGTIME=="actual") {
      d$TIMEPLOT <- d$ATAFD
      xlabtext <- "Actual time post first dose"
    }
    if (FLAGTIME=="nominal") {
      d$TIMEPLOT <- d$NTAFD
      xlabtext <- "Nominal time post first dose"
    }
  }
  if (add_unit) {
    time_unit <- tolower(d$TIMEUNIT[1])
    xlabtext <- paste0(xlabtext, " [", time_unit, "]")
  }
  d <- d[!is.na(d$TIMEPLOT),]
  list(data=d,xlabtext=xlabtext)
}
#' Replace the column name by its label if defined.
#'
#' Applies to the labeled columns only. Others keep there name.
#'
#' @param df data frame with potentially labeled columns
#'
#' @return data frame with substituted column names
#'
#' @examples
#' \dontrun{
#' df <- replace_colname_by_label(df)
#' }
replace_colname_by_label <- function(df) {
  tmp <- unlist(sapply(df, function(c) {
    n <- attr(c, "label")[[1]]
    if (is.null(n)) {return(NA)} else {return(n)}
  }))
  names(df)[!is.na(tmp)] <- tmp[!is.na(tmp)]
  df
}
#' Determine reliability of the NCA results from IQnca object
#'
#' Reliability is determined based on 3 criteria applying to %AUC extrapolated,
#' adjusted Rsquared threshold, and length of the time interval from which the half life
#' was determined.
#'
#' @param data IQnca object.
#' @param crit_AUCPEO max. percent AUC extrapolation acceptable, 20% by default.
#' @param crit_R2ADJ min. adjusted Rsquared value required, 0.85 by default.
#' @param crit_tinterval_thalf min. length of the time interval from which the terminal
#' slope was determined, 1.5*thalf by default.
#'
#' @return data.frame with USUBJID, GROUP, PROFILE, the success of each criterion,
#' and TOTAL = success of all criteria
#'
#' @export
#' @family NCA Parameter Tables
reliabilityTable_IQnca <- function(data, crit_AUCPEO = 20, crit_R2ADJ = 0.85, crit_tinterval_thalf = 1.5) {
  r <- data
  d <- attr(data, "dataNCA")
  r[["AUC_Criterion"]] <- r$AUCPEO < crit_AUCPEO
  r[["RADJ_Criterion"]] <- r$R2ADJ > crit_R2ADJ
  r[["THALF_Criterion"]] <- r$SPAN > crit_tinterval_thalf
  out <- data.frame(
    USUBJID = r$USUBJID,
    GROUP = r$GROUP,
    PROFILE = r$PROFILE,
    AUC = ifelse(is.na(r[["AUC_Criterion"]]), "-", c("no", "yes")[as.numeric(r[["AUC_Criterion"]]) + 1]),
    RADJ = ifelse(is.na(r[["RADJ_Criterion"]]), "-", c("no", "yes")[as.numeric(r[["RADJ_Criterion"]]) + 1]),
    THALF = ifelse(is.na(r[["THALF_Criterion"]]), "-", c("no", "yes")[as.numeric(r[["THALF_Criterion"]]) + 1]),
    stringsAsFactors = FALSE
  )
  out[["TOTAL"]] <- ifelse(out[["AUC"]] == "yes" & out[["RADJ"]] == "yes" & out[["THALF"]] == "yes", "yes", "no")
  AUC.label <- paste0("%AUCextr < ", crit_AUCPEO, "%")
  R2ADJ.label <- paste0("R2adjusted > ", crit_R2ADJ)
  THALF.label <- paste0("tint > ", crit_tinterval_thalf, " t1/2")
  names(out) <- c("USUBJID", "GROUP", "PROFILE", AUC.label, R2ADJ.label, THALF.label, "TOTAL")
  out
}
#' Modify or add columns (PK parameters) to the NCA result
#'
#' The function allows to overwrite or derive new PK parameters based on the existing parameters,
#' e.g., transforming the result to a different unit, or deriving per body weight parameters.
#' Alternatively, if the computation of additional PK parameters requires more than
#' just an algebraic relationship, the function can be used to "register" manually
#' added columns of custom PK parameters.
#'
#' @param data IQnca object, i.e., the output of [nca_IQdataNCA].
#' @param name Character of length 1. The column name to be generated (corresponds to PKPARAMCD).
#' @param based_on Character of length 1 or NULL (default). An existing column from which the new
#' parameter inherits all the attributes which are not explicitly provided by other argument of
#' this function.
#' @param formula One-sided formula of the form "~expression". Here expression is an algebraic
#' expression by which the new parameter is computed based on existing columns in `data`. See
#' details for more information.
#' @param name_short Character of length 1 or NULL (default). A short, humanly readable name of
#' the added parameter. If `NULL`, the value proposed by `based_on` will be used. If also `based_on`
#' is `NULL`, `name` will be used.
#' @param name_long Character of length 1 or NULL (default). A slightly longer, humanly readable
#' name of the added parameter. **This name will appear in tables and listings**. If `NULL`, the
#' value proposed by `based_on` will be used. If also `based_on` is `NULL`, `name` will be used.
#' @param description Character of length 1 or NULL (default). A longer description of
#' the added parameter. If `NULL`, the value proposed by `based_on` will be used. If also `based_on`
#' is `NULL`, `name` will be used.
#' @param unit Character of length 1 or NULL (default). The unit can be provided explicitly,
#' e.g., mg, mL, kg, etc., or using keywords recognized by IQnca, see details. If `NULL`, the
#' value proposed by `based_on` will be used. If also `based_on` is `NULL`, "-" will be used.
#' @param sd Logical or NULL (default). Indicates whether the param should be reported for
#' single dose profiles. If `NULL`, the value proposed by `based_on` will be used. If also
#' `based_on` is `NULL`, `TRUE` will be assumed.
#' @param fd Logical or NULL (default). Indicates whether the param should be reported for
#' first dose profiles. If `NULL`, the value proposed by `based_on` will be used. If also
#' `based_on` is `NULL`, `TRUE` will be assumed.
#' @param ss Logical or NULL (default). Indicates whether the param should be reported for
#' steady-state profiles. If `NULL`, the value proposed by `based_on` will be used. If also
#' `based_on` is `NULL`, `TRUE` will be assumed.
#' @param bolus Logical or NULL (default). Indicates whether the param should be reported for
#' administration type "BOLUS". If `NULL`, the value proposed by `based_on` will be used. If also
#' `based_on` is `NULL`, `TRUE` will be assumed.
#' @param infusion Logical or NULL (default). Indicates whether the param should be reported for
#' administration type "INFUSION". If `NULL`, the value proposed by `based_on` will be used. If also
#' `based_on` is `NULL`, `TRUE` will be assumed.
#' @param extravascular Logical or NULL (default). Indicates whether the param should be reported for
#' administration type "EXTRAVASCULAR". If `NULL`, the value proposed by `based_on` will be used. If also
#' `based_on` is `NULL`, `TRUE` will be assumed.
#' @param reporting "standard", "slope", "other", or `NULL` (default). See details. If `NULL`, the value
#' proposed by `based_on` will be used. If also `based_on` is `NULL`, "standard" will be assumed.
#' @param reporting_across_dose Logical or NULL (default). Indicates whether the parameter should
#' appear in summary tables for which PK parameters were summarized across doses. See details.
#' If `NULL`, the value proposed by `based_on` will be used. If also `based_on` is `NULL`,
#' `FALSE` will be assumed.
#'
#' @return IQnca object including the new or modified column, an updated units attribute and
#' an updated parameter specification table (paramspec) attribute.
#'
#' @details For the interpretation of **`formula`**, the right-hand side of the formula is parsed
#' to an R `expression` which is then evaluated in a local environment generated by `data` using
#' the `with` function.
#'
#' **Units** can be stated explicitly or using key words recognized by IQnca. Valid keywords
#' are `CONCUNIT`, `TIMEUNIT`, `DOSEUNIT`, `CLROUTINE`, and `VOLROUTINE`. Here, `CLROUTINE`
#' and `VOLROUTINE` refer to clearance and volume routines. These units are computed by
#' internal routines.
#'
#' **Reporting** of values is categorized by "standard", "slope", and "other". Here, "standard"
#' indicates parameters which are reported by default, "slope" indicates parameters related
#' to slope determination, and "other" are the remaining PK parameters. Which parameters
#' are acutally reported in tables and listings can be fine-tuned by the table and listings
#' functions if IQnca.
#'
#' **Reporting across dose** means that PK parameters summarized across different dose groups
#' are reported. Summarizing across doses can be valid for parameters which are expected
#' to be independent of dose, e.g., TMAX, clearance, terminal half-life. Summarizing across
#' doses is usually not valid for parameters such as Cmax and AUC.
#'
#' @family NCA Parameter Tables
#' @family NCA Parameter Listings
#' @family NCA Parameter Export
#'
#' @author Daniel Kaschek, IntiQuan
#'
#' @export
mutate_IQnca <- function(data, name, based_on = NULL, formula,
  name_short = NULL, name_long = NULL, description = NULL, unit = NULL,
  sd = NULL, fd = NULL, ss = NULL, bolus = NULL, infusion = NULL, extravascular = NULL,
  reporting = NULL, reporting_across_dose = NULL
) {
  if (!is_IQnca(data)) stopIQR("data must be an IQnca object")
  paramspec <- getparamspec_IQdataNCA(data)
  if (!is.character(name) && length(name) != 1) stopIQR("name must be character of length 1")
  if (!is.null(reporting)) reporting <- match.arg(reporting)
  if (!is.null(based_on) && !based_on %in% paramspec[["PKPARAMCD"]])
    stopIQR("based_on must be NULL or an existing parameter of the parameter specification (PKPARAMCD)")
  for (arg in c("name_short", "name_long", "description", "unit")) {
    myarg <- get(arg)
    if (!is.null(myarg) && !(is.character(myarg) & length(myarg) == 1))
      stopIQR(paste0(arg, " must be NULL or character vector of length 1"))
  }
  for (arg in c("sd", "fd", "ss", "bolus", "infusion", "extravascular", "reporting_across_dose")) {
    myarg <- get(arg)
    if (!is.null(myarg) && !(is.logical(myarg) & length(myarg) == 1))
      stopIQR(paste0(arg, " must be NULL or logical vector of length 1"))
  }
  if (is.null(based_on) & name %in% names(data) & name %in% paramspec[["PKPARAMCD"]]) based_on <- name
  if (is.null(based_on)) {
    template <- data.frame(
      PKPARAMCD = name,
      Name = name,
      PKPARAM = name,
      Description = name,
      SD = "X",
      FD = "X",
      SS = "X",
      BOLUS = "X",
      INFUSION = "X",
      EXTRAVASCULAR = "X",
      Reporting = "standard",
      ReportingAcrossDose = "NO",
      Formulas = NA_character_,
      Type = "Numeric",
      Unit.Calculation = "-",
      stringsAsFactors = FALSE
    )
  }  else {
    template <- paramspec[match(based_on, paramspec[["PKPARAMCD"]]), ]
  }
  template[["PKPARAMCD"]] <- name
  if (!is.null(name_short)) template[["Name"]] <- name_short
  if (!is.null(name_long)) template[["PKPARAM"]] <- name_long
  if (!is.null(description)) template[["Description"]] <- description
  if (!is.null(unit)) template[["Unit.Calculation"]] <- unit
  if (!is.null(sd)) template[["SD"]] <- ifelse(sd, "X", NA_character_)
  if (!is.null(fd)) template[["FD"]] <- ifelse(fd, "X", NA_character_)
  if (!is.null(ss)) template[["SS"]] <- ifelse(ss, "X", NA_character_)
  if (!is.null(bolus)) template[["BOLUS"]] <- ifelse(bolus, "X", NA_character_)
  if (!is.null(infusion)) template[["INFUSION"]] <- ifelse(infusion, "X", NA_character_)
  if (!is.null(extravascular)) template[["EXTRAVASCULAR"]] <- ifelse(extravascular, "X", NA_character_)
  if (!is.null(reporting)) template[["Reporting"]] <- reporting
  if (!is.null(reporting_across_dose)) template[["RepoartingAcrossDose"]] <- ifelse(reporting_across_dose, "YES", "NO")
  formula.char <- as.character(formula)
  if (!length(formula.char) == 2 || formula.char[1] != "~") stopIQR("formula must be one-sided formula, i.e., of the form '~expression'")
  formula.rhs <- parse(text = formula.char[2])
  symbols <- all.vars(formula)
  if (!all(symbols %in% names(data))) stopIQR("variables contained in formula must be present in the IQnca object (argument data)")
  data[[name]] <- with(as.list(data), eval(formula.rhs))
  template[["Formulas"]] <- formula.char[2]
  template[["Type"]] <- stringr::str_to_sentence(class(data[[name]]))
  if (name %in% paramspec[["PKPARAMCD"]]) {
    paramspec[match(name, paramspec[["PKPARAMCD"]]),] <- template
  } else {
    paramspec <- rbind(paramspec, template)
  }
  mydataNCA <- attr(data, "dataNCA")
  myunits <- attr(data, "units")
  milliLiter <- attr(data, "milliLiter")
  attr(mydataNCA, "paramspec") <- paramspec
  units <- units_IQnca(mydataNCA, milliLiter)
  attr(data, "paramspec") <- paramspec
  attr(data, "units") <- units$units
  return(data)
}
#' Generate default NCA report
#'
#' Collects all generated results (RMD) files and creates a combined RMD file. Optionally,
#' if IQReport is available it also generates a Word file.
#' @param reportfile Report file name. Note that a certain structure is assumed in the generation of the
#' RMD files containing figures. The location of the report file needs to match this in terms of the
#' relative paths used in the figure RMD files.
#' @param resultpaths Paths to folders with generated RMD results (listings, tables, and figures). All
#' present RMD files will be included.
#' @param templatestyle IQReport template file.
#' @param title Title of the report
#' @param docx logical. If TRUE and IQReport is present a Word DOCX file will be generated. If FALSE then only
#' a combined RMD file will be generated. The latter is useful if one wants to continue editing the file as a
#' stand alone NCA report.
#'
#' @family NCA Report
#' @export
report_IQnca <- function(reportfile,resultpaths,templatestyle="DefaultStyle.rmdt",title="Some NCA",docx=TRUE) {
  reportpath <- aux_fileparts(reportfile)$pathname
  plainfiles <- do.call(c,lapply(resultpaths, function (p) list.files(path = p,pattern = "*.rmd",full.names = TRUE)))
  metafiles <- do.call(c, lapply(resultpaths, function(p) {
    pp <- grep("_meta", list.dirs(p, full.names = TRUE, recursive = FALSE), value = TRUE)
    do.call(c,lapply(pp, function (p) list.files(path = p,pattern = "*.rmd",full.names = TRUE)))
  }))
  allfiles <- c(plainfiles, metafiles)
  text <- ""
  for (k in seq_along(allfiles)) {
    content <- aux_fileread(allfiles[k], collapserows = FALSE)
    content <- gsub("!LANDSCAPE","",content)
    if (k>1) content <- c("","","!NEWPAGE","","",content)
    idxFIG <- grep("!FIG[", content, fixed = TRUE)
    if (length(idxFIG)>0) {
      for (kk in idxFIG) {
        figpath <- stringr::str_extract(content[kk], "\\([:graph:]*\\)")
        figpath <- stringr::str_replace_all(figpath, "\\(|\\)", "")
        if (!file.exists(figpath)) warningIQR("Figure not found at given path: ", figpath)
        if (reportpath != ".") {
          fullreportpath <- strsplit(aux_simplifypath(file.path(getwd(),reportpath)), split = "/")[[1]]
          fullreportpath <- fullreportpath[fullreportpath!=""]
          fullfigpath    <- strsplit(aux_simplifypath(file.path(getwd(),figpath)), split = "/")[[1]]
          fullfigpath    <- fullfigpath[fullfigpath!=""]
          max <- min(length(fullfigpath), length(fullreportpath))
          i <- 1
          same <- fullreportpath[i] == fullfigpath[i]
          while (same && i+1 <= max) {
            i <- i+1
            same <- fullreportpath[i] == fullfigpath[i]
          }
          if (!same) i <- i-1
          part1 <- fullreportpath[setdiff(seq_along(fullreportpath), 1:i)]
          if (length(part1) == 0) {part1 <- "."} else {part1 <- rep("..", length(part1))}
          part2 <- fullfigpath[setdiff(seq_along(fullfigpath), 1:i)]
          if (length(part2) == 0) part2 <- "."
          newfigpath <- file.path(paste0(part1, collapse = "/"), paste0(part2, collapse = "/"))
          message("New relative path set: ", figpath, " --> ", newfigpath)
          content[kk] <- sub(figpath, newfigpath, content[kk], fixed = "TRUE")
        }
      }
    }
    content <- paste0(content, collapse = "\n")
    text <- paste0(text,content,"\n")
  }
  rmd <- rmdEMPTY() +
    rmdTITLE(template=templatestyle,title = title) +
    rmdLANDSCAPE() +
    text
  export_IQRrmd(rmd,filename = reportfile)
  IQReport(reportfile)
  unlink(paste0(reportfile,".bak"))
}
#' Get name of time unit in IQdataNCA object
#'
#' This is useful for display and annotation purposes in plots and tables.
#' Example "min", "hour", etc.
#'
#' @param data IQdataNCA object
#' @return Returns the name of the unit ready for display in plots and tables and listings.
#' @export
getTIMEUNITname_IQdataNCA <- function (data) {
  if (toupper(data$TIMEUNIT[1]) == "SECONDS") return("second")
  if (toupper(data$TIMEUNIT[1]) == "MINUTES") return("min")
  if (toupper(data$TIMEUNIT[1]) == "HOURS") return("hour")
  if (toupper(data$TIMEUNIT[1]) == "DAYS") return("day")
  if (toupper(data$TIMEUNIT[1]) == "WEEKS") return("week")
}
#' Get symbol of time unit in IQdataNCA object
#'
#' This is useful for use of the time unit in units and constructed units for
#' derived NCA PK parameters. Example "min", "h", etc.
#'
#' @param data IQdataNCA object
#' @return Returns the symbol of the unit ready for display in plots and tables and listings.
#' @export
getTIMEUNITsymbol_IQdataNCA <- function (data) {
  if (toupper(data$TIMEUNIT[1]) == "SECONDS") return("s")
  if (toupper(data$TIMEUNIT[1]) == "MINUTES") return("min")
  if (toupper(data$TIMEUNIT[1]) == "HOURS") return("h")
  if (toupper(data$TIMEUNIT[1]) == "DAYS") return("d")
  if (toupper(data$TIMEUNIT[1]) == "WEEKS") return("wk")
}
updateFigureNumberTitle_IQdataNCA <- function (title,figurenumber,figureindex, avg=NULL,var=NULL) {
  if (is.null(figurenumber)) {
    title <- gsub("Figure NRPLACEHOLDER-INDEXPLACEHOLDER ","",title,fixed = TRUE)
  } else {
    title <- gsub("NRPLACEHOLDER",figurenumber,title,fixed = TRUE)
  }
  if (!is.null(figureindex)) {
    title <- gsub("INDEXPLACEHOLDER",figureindex,title,fixed = TRUE)
  } else {
    title <- gsub("-INDEXPLACEHOLDER","",title,fixed = TRUE)
  }
  if (!is.null(avg)) {
    title <- gsub("AVGPLACEHOLDER",avg,title,fixed = TRUE)
  }
  if (!is.null(var)) {
    title <- gsub("VARPLACEHOLDER",var,title,fixed = TRUE)
  }
  title
}
updateListingNumberTitle_IQdataNCA <- function (title,listingnumber,listingindex) {
  if (is.null(listingnumber)) {
    title <- gsub("Listing NRPLACEHOLDER-INDEXPLACEHOLDER ","",title,fixed = TRUE)
    title <- gsub("Listing NRPLACEHOLDER ","",title,fixed = TRUE)
  } else {
    title <- gsub("NRPLACEHOLDER",listingnumber,title,fixed = TRUE)
  }
  if (!is.null(listingindex)) {
    title <- gsub("INDEXPLACEHOLDER",listingindex,title,fixed = TRUE)
  } else {
    title <- gsub("-INDEXPLACEHOLDER","",title,fixed = TRUE)
  }
  title
}
updateTableNumberTitle_IQdataNCA <- function (title,tablenumber,tableindex) {
  if (is.null(tablenumber)) {
    title <- gsub("Table NRPLACEHOLDER-INDEXPLACEHOLDER ","",title,fixed = TRUE)
    title <- gsub("Table NRPLACEHOLDER ","",title,fixed = TRUE)
  } else {
    title <- gsub("NRPLACEHOLDER",tablenumber,title,fixed = TRUE)
  }
  if (!is.null(tableindex)) {
    title <- gsub("INDEXPLACEHOLDER",tableindex,title,fixed = TRUE)
  } else {
    title <- gsub("-INDEXPLACEHOLDER","",title,fixed = TRUE)
  }
  title
}
#' Load an IQdataNCA object from CSV
#'
#' Load it and set class to IQdataNCA. Currently no additional checks
#'
#' @param filename Filename with path to IQdataNCA object (csv or xpt)
#' @export
#' @family NCA Data
load_IQdataNCA <- function (filename) {
  if (grepl(".csv$",filename)) {
    out <- IQRloadCSVdata(filename)
  } else if (grepl(".xpt$",filename)) {
    out <- IQRloadSASdata(filename)
    tmp <- paste0(tempfile(),".csv")
    IQRsaveCSVdata(out,tmp)
    out <- IQRloadCSVdata(tmp)
    unlink(tmp,force = TRUE)
    out <- updateslope_IQdataNCA(out)
  }
  class(out) <- c("IQdataNCA",class(out))
  out
}
getScaleIQRoutputFigure_nindiv <- function (nindiv) {
  floor(sqrt(nindiv)) + 0.1
}
removeFrom_IQdataNCA <- function (data,FLAGremoveIGNOREI,FLAGremoveIGNORER,FLAGremoveIGNORSUM,FLAGremoveIGNORNCA) {
  out <- data
  if (FLAGremoveIGNOREI) out <- out[is.na(out$IGNOREI),]
  if (FLAGremoveIGNORER) out <- out[is.na(out$IGNORER),]
  if (FLAGremoveIGNORSUM) out <- out[is.na(out$IGNORSUM),]
  if (FLAGremoveIGNORNCA) out <- out[is.na(out$IGNORNCA),]
  out
}
getScriptName_IQdataNCA <- function () {
  e__ <- globalenv()
  if (!("COMPLIANCE_MODE_SCRIPT_NAME" %in% ls(e__))) {
    scriptName <- "UNKNOWN"
  } else {
    scriptName <- e__$COMPLIANCE_MODE_SCRIPT_NAME
  }
  scriptName
}
#' Generate a table of statistical summaries of pharmacokinetic concentrations
#'
#' The table will be generated already as an IQRrmd object, allowing easy reporting in Word with IQReport.
#' The table is done per PROFILE and GROUP.
#' Important: Ignored records (INGORER & IGNORSUM) are not reported in these tables! Ignored subjects (IGNOREI) are
#' removed as well.
#'
#' @param data IQdataNCA object
#' @param tablenumber Character string with table number information, added to the title for each table, if defined
#' @param fontsizetable Fontsize to be used in the table
#' @param filename Filename to export the listing IQRrmd object to
#' @param SIGNIF Significant digits for table values
#' @param maxCol Number of nominal times to be put into the same table in case of simple=TRUE. Tables might become to wide even for landscape
#' so a plit up of columns in several tables can be enforced in this manner.
#' @return An IQRrmd object exported to a file
#' @export
#' @family NCA Data Tables
table_summary_conc_IQdataNCA <- function (data,tablenumber=NULL,fontsizetable=8,filename="table_summary_concentrations",SIGNIF=4,maxCol=10) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  if (maxCol<4) maxCol <- 4
  NTall <- sort(unique(data$NTIME))
  data <- removeFrom_IQdataNCA(data = data,
                               FLAGremoveIGNOREI = TRUE,
                               FLAGremoveIGNORER = TRUE,
                               FLAGremoveIGNORSUM = TRUE,
                               FLAGremoveIGNORNCA = FALSE)
  message("Generating PK concentration summary tables ...")
  if (!is.null(filename)) {
    filename <- paste0(gsub(".rmd","",filename),".rmd")
  }
  data <- data[!is.na(data$NTIME),]
  data <- dplyr::arrange(data, PROFILE, GROUPN, NTIME)
  text <- rmdEMPTY() +
    rmdLANDSCAPE() +
    rmdSECTION(title = updateTableNumberTitle_IQdataNCA(.table_summary_pkconc,tablenumber,NULL),
               level = 1,numbered = FALSE)
  table <- list()
  data <- dplyr::arrange(data, GROUPN, PROFILE)
  data$SPLIT <- paste(data$GROUP, data$PROFILE)
  data$SPLIT <- factor(data$SPLIT, levels = unique(data$SPLIT))
  dS <- split(data, data$SPLIT)
  for (k in seq_along(dS)) {
    d <- dS[[k]]
    object <- tableSummaryGroupProfile_IQdataNCA(d = d,NTall=NTall,tableindex = k,tablenumber=tablenumber,fontsizetable=fontsizetable,filename=filename,SIGNIF=SIGNIF,maxCol=maxCol)
    if ("IQRrmd" %in% class(object)) {
      textindiv <- object
      if (k==1) {
        text <- text + textindiv + "\n"
      } else {
        text <- text + rmdNEWPAGE() + "\n" + textindiv + "\n"
      }
    }
    if ("IQRoutputTable" %in% class(object)) {
      table <- c(table, structure(list(object), names = as.character(d$SPLIT[1])))
    }
  }
  if (!is.null(filename)) {
    export_IQRrmd(text,filename)
  } else {
    return(table)
  }
}
tableSummaryGroupProfile_IQdataNCA <-  function (d,NTall,tableindex,tablenumber=NULL,fontsizetable=8,filename,SIGNIF=4,maxCol=10) {
  NTmissing <- setdiff(NTall,unique(d$NTIME))
  if (!is.null(filename)) {
    pieces <- aux_splitVectorEqualPieces(x = unique(d$NTIME),maxCol)
  } else {
    pieces <- aux_splitVectorEqualPieces(x = unique(d$NTIME),Inf)
  }
  text <- rmdEMPTY()
  for (kpiece in seq_along(pieces)) {
    NT <- pieces[[kpiece]]
    NT <- NT[!is.na(NT)]
    d2 <- d[d$NTIME %in% NT,]
    dS2 <- split(d2,d2$NTIME)
    res <- do.call(rbind,lapply(seq_along(dS2), function (knt) {
      d3 <- dS2[[knt]]
      if (nrow(d3)==0) return (NULL)
      N      <- nrow(d3[!is.na(d3$NTIME) & !is.na(d3$CONC),])            
      MEAN   <- mean(d3$CONC,na.rm = TRUE)
      SD     <- sd(d3$CONC,na.rm = TRUE)
      CVPMN  <- suppressWarnings(100*SD/MEAN)
      GMEAN  <- suppressWarnings(geomean(d3$CONC, na.rm = TRUE))
      CVPGM  <- suppressWarnings(geocv(d3$CONC,na.rm = TRUE))
      MEDIAN <- median(d3$CONC,na.rm = TRUE)
      MIN    <- min(d3$CONC,na.rm = TRUE)
      MAX    <- max(d3$CONC,na.rm = TRUE)
      data.frame(
        NTIME     = d3$NTIME[1],
        N         = N,
        MEAN      = signif(MEAN,SIGNIF),
        SD        = ifelse(is.na(SD),"NC",signif(SD,SIGNIF)),
        MIN       = signif(MIN,SIGNIF),
        MEDIAN    = signif(MEDIAN,SIGNIF),
        GMEAN     = ifelse(any(0 %in%  d3$CONC),"NC",signif(GMEAN,SIGNIF)),
        MAX       = signif(MAX,SIGNIF),
        CVPMN     = ifelse(MEAN==0 | is.na(CVPMN),"NC",signif(CVPMN,SIGNIF)),
        CVPGM     = ifelse(any(0 %in%  d3$CONC) | is.na(CVPGM),"NC",signif(CVPGM,SIGNIF)),
        stringsAsFactors = FALSE
      )
    }))
    resT <- data.frame(t(res),stringsAsFactors = FALSE)
    resT <- rbind(resT[1,,drop=FALSE],rep(" ",ncol(resT)),resT[2:nrow(resT),,drop=FALSE])
    tab <- cbind(GROUP = d$GROUP[1],
                 PARAMETER = rownames(resT),
                 resT)
    names(tab) <- c("Treatment","Parameter",paste0("Nominal Time [",getTIMEUNITsymbol_IQdataNCA(d),"]"),rep("",ncol(tab)-3))
    tab$Treatment <- as.character(tab$Treatment)
    tab$Parameter <- as.character(tab$Parameter)
    tab[2,3] <- paste0("**Concentration [",d$CONCUNIT[1],"]**")
    tab[2,1] <- "**Treatment**"
    tab[2,2] <- "**Stats**"
    tab[1,1] <- ""
    tab[1,2] <- ""
    names(tab)[c(1,2)] <- ""
    rownames(tab) <- NULL
    tab[[1]][duplicated(tab[[1]])] <- ""
    tab[[2]][tab[[2]]=="N"] <- "N"
    tab[[2]][tab[[2]]=="MEAN"] <- "Mean"
    tab[[2]][tab[[2]]=="SD"] <- "SD"
    tab[[2]][tab[[2]]=="MIN"] <- "Min"
    tab[[2]][tab[[2]]=="MEDIAN"] <- "Median"
    tab[[2]][tab[[2]]=="GMEAN"] <- "Geo-mean"
    tab[[2]][tab[[2]]=="MAX"] <- "Max"
    tab[[2]][tab[[2]]=="CVPMN"] <- "CV% mean"
    tab[[2]][tab[[2]]=="CVPGM"] <- "CV% geo-mean"
    scriptName <- getScriptName_IQdataNCA()
    blloqtext <- paste0("Lower limit of quantitation: ",d$LLOQ[1]," ",d$CONCUNIT[1],". Pre-first dose <LLOQ values were handled as: ",gsub("lloq","LLOQ",d$FLGBLQPR[1]),". ",
                        "<LLOQ values between >=LLOQ values were handled as: ",gsub("lloq","LLOQ",d$FLGBLQIN[1]),". ",
                        "First <LLOQ value post last >= LLOQ was handled as: ",gsub("lloq","LLOQ",d$FLGBLQP1[1]),". ",
                        "Second to last First <LLOQ value post last >= LLOQ was handled as: ",gsub("lloq","LLOQ",d$FLGBLQPO[1]),".")
    NTmissingText <- NULL
    if (length(NTmissing) > 1) NTmissingText <- paste0("No evaluable concentration information for nominal time: ",paste0(NTmissing,collapse=", ")," ",getTIMEUNITname_IQdataNCA(d),"s\n")
    footertext <- paste0(
      NTmissingText,
      "NC: Not calculated.\n",
      "Geo-mean: Geometric mean.\n",
      "CV%% mean = coefficient of variation (%%)=SD/mean * 100.\n",
      "CV%% geo-mean=(sqrt (exp (variance for log transformed data)-1)) * 100.\n",
      blloqtext,"\n",
      "Values in table are reported with ",SIGNIF," significant digits.\n",
      "IQnca version: ", aux_version(pkgName = "IQnca"), "\n",
      "Script: ", scriptName,"\n",
      "Output: ",filename, "\n",
      "Execution date: ", Sys.time()
    )
    loadSetupOptions_IQnca()
    title <- .table_summary_pkconc
    title <- updateTableNumberTitle_IQdataNCA(title,tablenumber,tableindex)
    title <- paste0(title," of ",d$MATRIX[1]," ",d$COMPOUND[1], " (",d$ANALYTE[1],") - ",d$PROFILE[1])
    if (length(pieces)>1) {
      title <- paste0(title," (",kpiece," of ",length(pieces),")")
    }
    if (kpiece > 1 & length(pieces)>1) text <- text + rmdNEWPAGE() + "\n"
    blockIdentifier <- paste0("table1_",gsub("-","x",tableindex))
    text <- text + paste0("!BLOCKSTART[keepNext](",blockIdentifier,")\n")
    text <- text + "**" + title + "**" +
      rmdTABLEDF(tab,footertext = footertext,ignoreCaption = TRUE,fontsize = fontsizetable) + "\n"
    text <- text + paste0("\n!BLOCKEND(",blockIdentifier,")\n")
    table <- IQRoutputTable(tab, xtitle = title, xfooter = footertext)
  }
  if (!is.null(filename)) {
    return(text)
  } else {
    return(table)
  }
}
#' Generate another table of statistical summaries of pharmacokinetic concentrations
#'
#' The table will be generated already as an IQRrmd object, allowing easy reporting in Word with IQReport.
#' The table is done per PROFILE and per NTIME time point. All data belonging to a profile and a timepoint are shown in one table.
#' There is a header with some general information. Then the table, followed by the table footer.
#' Important: Ignored records (INGORER & IGNORSUM) are not reported in these tables! Ignored subjects (IGNOREI) are
#' removed as well.
#'
#' @param data IQdataNCA object
#' @param tablenumber Character string with table number information, added to the title for each table, if defined
#' @param fontsizetable Fontsize to be used in the table
#' @param filename Filename to export the listing IQRrmd object to
#' @param SIGNIF Significant digits for values
#' @return An IQRrmd object exported to a file
#' @export
#' @family NCA Data Tables
table_summary_conc_by_time_IQdataNCA <- function (data,tablenumber=NULL,fontsizetable=8,filename="table_conc_by_time",SIGNIF=4) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  data <- removeFrom_IQdataNCA(data = data,
                               FLAGremoveIGNOREI = TRUE,
                               FLAGremoveIGNORER = TRUE,
                               FLAGremoveIGNORSUM = TRUE,
                               FLAGremoveIGNORNCA = FALSE)
  message("Generating PK concentration summary tables ...")
  if (!is.null(filename)) {
    filename <- paste0(gsub(".rmd","",filename),".rmd")
  }
  data <- data[!is.na(data$NTIME),]
  data <- dplyr::arrange(data, PROFILE, NTIME, GROUPN)
  data$NTIMEfac <- factor(data$NTIME,levels=sort(unique(data$NTIME)))
  data$GROUPfac <- factor(data$GROUP,levels=unique(data$GROUP))
  text <- rmdEMPTY() +
    rmdLANDSCAPE() +
    rmdSECTION(title = updateTableNumberTitle_IQdataNCA(.table_summary_pkconc,tablenumber,NULL),
               level = 1,numbered = FALSE)
  table <- list()
  dS <- split(data,data$PROFILE)
  for (kprofile in seq_along(dS)) {
    d1 <- dS[[kprofile]]
    dS1 <- split(d1,d1$NTIMEfac, drop = TRUE)
    for (kntime in seq_along(dS1)) {
      d2 <- dS1[[kntime]]
      object <- tableSummaryTimePoint_bytime_IQdataNCA(data = d2,tableindex = paste0(kprofile,"-",kntime),tablenumber=tablenumber,fontsizetable=fontsizetable,filename=filename,SIGNIF=SIGNIF)
      if ("IQRrmd" %in% class(object)) {
        textindiv <- object
        if (kntime==1) {
          text <- text + textindiv + "\n"
        } else {
          text <- text + rmdNEWPAGE() + "\n" + textindiv + "\n"
        }
      }
      if ("IQRoutputTable" %in% class(object)) {
        table <- c(table, structure(list(object), names = paste0(d2$PROFILE[1],"-",d2$NTIMEfac[1])))
      }
    }
  }
  if (!is.null(filename)) {
    export_IQRrmd(text,filename)
  } else {
    return(table)
  }
}
tableSummaryTimePoint_bytime_IQdataNCA <-  function (data,tableindex,tablenumber=NULL,fontsizetable=8,filename="unknown",SIGNIF=4) {
  dS2 <- split(data,data$GROUPfac)
  tab <- do.call(rbind,lapply(seq_along(dS2), function (kgroup) {
    d3 <- dS2[[kgroup]]
    if (nrow(d3)==0) return (NULL)
    N      <- nrow(d3[!is.na(d3$NTIME) & !is.na(d3$CONC),])            
    MEAN   <- mean(d3$CONC,na.rm = TRUE)
    SD     <- sd(d3$CONC,na.rm = TRUE)
    CVPMN  <- suppressWarnings(100*SD/MEAN)
    GMEAN  <- suppressWarnings(geomean(d3$CONC, na.rm = TRUE))
    CVPGM  <- suppressWarnings(geocv(d3$CONC,na.rm = TRUE))
    MEDIAN <- median(d3$CONC,na.rm = TRUE)
    MIN    <- min(d3$CONC,na.rm = TRUE)
    MAX    <- max(d3$CONC,na.rm = TRUE)
    data.frame(
      PROFILE   = d3$PROFILE[1],
      PCTPT     = d3$PCTPT[1],
      NTIME     = d3$NTIME[1],
      GROUP     = d3$GROUP[1],
      N         = N,
      MEAN_SD   = paste0(signif(MEAN,SIGNIF)," (",signif(SD,SIGNIF),")"),
      CVPMN     = ifelse(MEAN==0,"-^a^",signif(CVPMN,SIGNIF)),
      GMEAN     = ifelse(any(0 %in%  d3$CONC),"-^b^",signif(GMEAN,SIGNIF)),
      CVPGM     = ifelse(any(0 %in%  d3$CONC),"-^b^",signif(CVPGM,SIGNIF)),
      MEDIAN    = signif(MEDIAN,SIGNIF),
      MINMAX    = paste0("[",signif(MIN,SIGNIF),", ",signif(MAX,SIGNIF),"]"),
      UNIT      = data$CONCUNIT[1],
      stringsAsFactors = FALSE
    )
  }))
  tab$PROFILE[duplicated(tab$PROFILE)] <- ""
  tab$PCTPT[duplicated(tab$PCTPT)] <- ""
  tab$NTIME[duplicated(tab$NTIME)] <- ""
  names(tab) <- c("Profile",
                  "Time point",
                  paste0("Nominal time [",getTIMEUNITname_IQdataNCA(data),"]"),
                  "Group",
                  "N",
                  "Mean (SD)",
                  "CV% mean",
                  "Geo-mean",
                  "CV% geo-mean",
                  "Median",
                  "[Min, Max]",
                  "Unit"
  )
  scriptName <- getScriptName_IQdataNCA()
  blloqtext <- paste0("Pre-first dose <LLOQ values were handled as: ",gsub("lloq","LLOQ",data$FLGBLQPR[1]),". ",
                      "<LLOQ values between >=LLOQ values were handled as: ",gsub("lloq","LLOQ",data$FLGBLQIN[1]),". ",
                      "First <LLOQ value post last >= LLOQ was handled as: ",gsub("lloq","LLOQ",data$FLGBLQP1[1]),". ",
                      "Second to last First <LLOQ value post last >= LLOQ was handled as: ",gsub("lloq","LLOQ",data$FLGBLQPO[1]),".")
  footertext <- paste0(
    "NA: Not available.\n",
    "CV%% = coefficient of variation (%%)=SD/mean*100.\n",
    "Geo-mean: Geometric mean.\n",
    "CV%% geo-mean=(sqrt (exp (variance for log transformed data)-1))*100.\n",
    "^a^ CV%% mean not presented when the mean is 0.\n",
    "^b^ Geo-mean and CV%% geo-mean not presented when the minimum concentration is zero at respective timepoint.\n",
    blloqtext,"\n",
    "Values in table are reported with ",SIGNIF," significant digits.\n",
    "IQnca version: ", aux_version(pkgName = "IQnca"), "\n",
    "Script: ", scriptName,"\n",
    "Output: ",filename, "\n",
    "Execution date: ", Sys.time()
  )
  text <- rmdEMPTY()
  loadSetupOptions_IQnca()
  title <- .table_summary_pkconc
  title <- updateTableNumberTitle_IQdataNCA(title,tablenumber,tableindex)
  title <- paste0(title," of ",data$MATRIX[1]," ",data$COMPOUND[1], " (",data$ANALYTE[1],") - ",data$PROFILE[1])
  blockIdentifier <- paste0("table2_",gsub("-","x",tableindex))
  text <- text + paste0("!BLOCKSTART[keepNext](",blockIdentifier,")\n")
  text <- text + "**" + title + "**"
  text <- text + rmdTABLEDF(df = tab,ignoreCaption = TRUE,fontsize = fontsizetable,footertext = footertext)
  text <- text + paste0("\n!BLOCKEND(",blockIdentifier,")\n")
  table <- IQRoutputTable(tab, xtitle = title, xfooter = footertext)
  if (!is.null(filename)) {
    return(text)
  } else {
    return(table)
  }
}
#' Plot of summary dose-normalized PK concentrations (geometric mean and 95% CI)
#'
#' Always only for nominal time (NTIME or NTAFD)!
#' Since dose-normalization is done, standard stratifictation is done by PROFILE.
#' Additional stratification can be provided by the user and is shown in different colors.
#' Default stratification is by GROUP (ordered by GROUPN).
#' Ignored subjects (IGNOREI) and ignored records (IGNORER and IGNORSUM) are
#' excluded from these plots.
#'
#' @param data IQdataNCA object
#' @param figurenumber Character string with figure number information, added to the title for each figure, if defined
#' @param logY Log y axis is TRUE, otherwise linear Y axis
#' @param strat Character. Name of column to use for stratification (default: GROUP). The only requirement
#' is that the entries in the stratification column are unique per subject per profile. The columns can contain
#' numeric of aphanumeric entries. They can be categorical or continuous. However, it is advisable for the purpose
#' of useful plots to use continuous numeric columns only for stratification when there is a certain limited
#' number of unique values. But it is your call entirely :-)
#' @param scaleXsame logical. It TRUE then the same min anx max value on the X-axis will be used for each plot
#' @param scaleYsame logical. It TRUE then the same min anx max value on the Y-axis will be used for each plot
#' @param filename Name of PDF file to generate with plots
#' @param nindiv Number of plots per page if exported to PDF
#' @param USETAD logical. TRUE uses TIME, NTIME, or ATIME, depending on FLAGTIME settings. FALSE will use
#' TAFD, NTAFD, or ATAFD.
#' @param alpha alpha value for the plotting.
#' @return Returns a list of all plots if filename is NULL
#' @export
#' @family NCA Data Figures
figure_summary_geommean_dosenorm_IQdataNCA <- function (data,figurenumber=NULL,logY=TRUE,strat="GROUP",scaleXsame=TRUE,scaleYsame=TRUE,filename="summarygeommeandn.pdf",nindiv=1,USETAD=TRUE,alpha=0.8) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  if (!strat %in% names(data)) stopIQR("Colum '",group,"' is not available in the data - select a different column name for stratification")
  message("Generating PK concentration figures summary geometric mean dose normalized ...")
  if (any(unlist(sapply(split(data,data$PROFILE), function (d) {
    sapply(split(d,d$USUBJID), function (d2) {
      length(unique(d2[[strat]]))
    })
  })) != 1)) stopIQR("Chosen 'strat' column does not contain unique entries per subject per profile")
  data <- removeFrom_IQdataNCA(data = data,
                               FLAGremoveIGNOREI = TRUE,
                               FLAGremoveIGNORER = TRUE,
                               FLAGremoveIGNORSUM = TRUE,
                               FLAGremoveIGNORNCA = FALSE)
  res <- handleCONCTIMEplot_IQdataNCA(d = data,logY = logY,FLAGTIME="nominal",USETAD=USETAD)
  data <- res$data
  xlabtext <- res$xlabtext
  data <- data[!is.na(data$CONCPLOT),]
  if (logY) data <- data[data$CONCPLOT!=0,]
  data <- data[!is.na(data$TIMEPLOT),]
  xlim <- c(min(data$TIMEPLOT),max(data$TIMEPLOT))
  if (strat=="GROUP") {
    data <- dplyr::arrange(data, GROUPN, PROFILE)
    data$STRATification <- data[[strat]]
  } else {
    data$STRATification <- data[[strat]]
    data <- dplyr::arrange(data, STRATification, PROFILE)
  }
  data$STRATification <- factor(data$STRATification, levels = unique(data$STRATification))
  data$CONCPLOTDN <- data$CONCPLOT/data$DOSE
  CONCUNITDN <- paste0(data$CONCUNIT[1],"/",data$DOSEUNIT[1])
  dS <- split(data,data$PROFILE)
  ylim <- c(0,-Inf)
  if (logY) {
    if (data$LLOQ[1]==0) {
      ylim[1] <- min(data$CONCPLOT)/2/max(data$DOSE,na.rm = TRUE)
    } else {
      ylim[1] <- data$LLOQ[1]/3/max(data$DOSE,na.rm = TRUE)
    }
  }
  allplots <- lapply(seq_along(dS), function (k) {
    figureindex <- k
    d <- dS[[k]]
    d2S <- split(d,d$STRATification)
    stats <- do.call(rbind,lapply(seq_along(d2S), function (kx) {
      d2 <- d2S[[kx]]
      if (nrow(d2)==0) return (NULL)
      statsk <- suppressWarnings(statXYnominal(x=d2$TIMEPLOT,y=d2$CONCPLOTDN))
      statsk$MEAN_MINUS_SD <- statsk$MEAN.VALUE-statsk$SD.VALUE
      if (logY) {
        statsk$MEAN_MINUS_SD[statsk$MEAN_MINUS_SD<=0] <- min(d$LLOQ)/10
      } else {
        statsk$MEAN_MINUS_SD[statsk$MEAN_MINUS_SD<=0] <- 0
      }
      statsk$MEAN_PLUS_SD <- statsk$MEAN.VALUE+statsk$SD.VALUE
      cbind(STRAT=names(d2S)[kx],statsk[,c("TIME","GEOMMEAN.VALUE","GEOMSD.VALUE")])
    }))
    stats$GEOMMEAN_CI95_LOW <- exp(log(stats$GEOMMEAN.VALUE) - 1.96*log(stats$GEOMSD.VALUE))
    stats$GEOMMEAN_CI95_HIGH <- exp(log(stats$GEOMMEAN.VALUE) + 1.96*log(stats$GEOMSD.VALUE))
    p <- IQRggplot(data=stats,aes(x=TIME,y=GEOMMEAN.VALUE,group=STRAT,color=STRAT,linetype=STRAT,shape=STRAT))
    p <- p + scale_color_IQnca(strat,drop=FALSE)
    p <- p + scale_shape_manual(strat,values = c(1:100))
    p <- p + scale_linetype_manual(strat,values = c(1:100),drop=FALSE)
    p <- p + geom_line(alpha=alpha) + geom_point(alpha=alpha, size=3)
    p <- p + geom_uperrorbar(aes(x=TIME,ymax=GEOMMEAN_CI95_HIGH), width=max(stats$TIME)/33)
    if (logY) p <- p + scale_y_log10_IQnca(labeltype = "natural")
    p <- p + xlab(paste0(xlabtext," [",getTIMEUNITname_IQdataNCA(d),"]"))
    p <- p + ylab(paste0("Dose-normalized Concentration [",CONCUNITDN,"]"))
    p <- p + scale_x_continuous(breaks=gettimebreaksplotindiv(d,xlim = xlim)$breaks,minor_breaks = gettimebreaksplotindiv(d,xlim = xlim)$minorbreaks)
    loadSetupOptions_IQnca()
    title <- .figure_summary_geommean_dosenorm_pkconc
    title <- updateFigureNumberTitle_IQdataNCA(title,figurenumber,figureindex)
    title <- paste0(title,"\nof ",d$MATRIX[1]," ",d$COMPOUND[1], " (",d$ANALYTE[1],")")
    if (logY) {
      title <- paste0(title," - Log scale")
    } else {
      title <- paste0(title," - Linear scale")
    }
    subtitle <- paste0(
      d$PROFILE[1]
    )
    p <- p + ggtitle(title,subtitle=subtitle)
    ylim[2] <<- max(stats$GEOMMEAN_CI95_HIGH,ylim[2],na.rm = TRUE)
    p
  })
  if (!scaleXsame) xlim <- NULL
  if (!scaleYsame) ylim <- NULL
  allplots <- lapply(allplots, function (p) p <- p + coord_cartesian(xlim = xlim,ylim = ylim))
  if (is.null(filename)) return(allplots)
  suppressWarnings(IQRoutputFigure(x = allplots,
                                   opt.layout = opt.layout(legend.option = "as.is",ncol = floor(sqrt(nindiv)),nrow=ceiling(nindiv/floor(sqrt(nindiv)))),
                                   opt.pagesize = opt.pagesize(scale = getScaleIQRoutputFigure_nindiv(nindiv)),
                                   filename = paste0(gsub(".pdf","",filename),".pdf")))
  unlink(paste0(gsub(".pdf","",filename),".pdf.log"))
  genRMDfigurefile_IQdataNCA(title=.figure_summary_geommean_dosenorm_pkconc,figurenumber=figurenumber,logY=logY,filename=filename,strat=strat,stratdefault="GROUP")
  invisible(NULL)
}
#' Plot of summary dose-normalized PK concentrations (arithmetic mean and SD)
#'
#' Always only for nominal time (NTIME or NTAFD)!
#' Since dose-normalization is done, standard stratifictation is done by PROFILE.
#' Additional stratification can be provided by the user and is shown in different colors.
#' Default stratification is by GROUP (ordered by GROUPN).
#' Ignored subjects (IGNOREI) and ignored records (IGNORER and IGNORSUM) are
#' excluded from these plots.
#'
#' @param data IQdataNCA object
#' @param figurenumber Character string with figure number information, added to the title for each figure, if defined
#' @param logY Log y axis is TRUE, otherwise linear Y axis
#' @param strat Character. Name of column to use for stratification (default: GROUP). The only requirement
#' is that the entries in the stratification column are unique per subject per profile. The columns can contain
#' numeric of aphanumeric entries. They can be categorical or continuous. However, it is advisable for the purpose
#' of useful plots to use continuous numeric columns only for stratification when there is a certain limited
#' number of unique values. But it is your call entirely :-)
#' @param scaleXsame logical. It TRUE then the same min anx max value on the X-axis will be used for each plot
#' @param scaleYsame logical. It TRUE then the same min anx max value on the Y-axis will be used for each plot
#' @param filename Name of PDF file to generate with plots
#' @param nindiv Number of plots per page if exported to PDF
#' @param USETAD logical. TRUE uses TIME, NTIME, or ATIME, depending on FLAGTIME settings. FALSE will use
#' TAFD, NTAFD, or ATAFD.
#' @param alpha alpha value for the plotting.
#' @return Returns a list of all plots if filename is NULL
#' @export
#' @family NCA Data Figures
figure_summary_mean_dosenorm_IQdataNCA <- function (data,figurenumber=NULL,logY=TRUE,strat="GROUP",scaleXsame=TRUE,scaleYsame=TRUE,filename="summarymeandn.pdf",nindiv=1,USETAD=TRUE,alpha=0.8) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  if (!strat %in% names(data)) stopIQR("Column '",group,"' is not available in the data - select a different column name for stratification")
  message("Generating PK concentration figures summary mean dose normalized ...")
  if (any(unlist(sapply(split(data,data$PROFILE), function (d) {
    sapply(split(d,d$USUBJID), function (d2) {
      length(unique(d2[[strat]]))
    })
  })) != 1)) stopIQR("Chosen 'strat' column does not contain unique entries per subject per profile")
  data <- removeFrom_IQdataNCA(data = data,
                               FLAGremoveIGNOREI = TRUE,
                               FLAGremoveIGNORER = TRUE,
                               FLAGremoveIGNORSUM = TRUE,
                               FLAGremoveIGNORNCA = FALSE)
  res <- handleCONCTIMEplot_IQdataNCA(d = data,logY = logY,FLAGTIME="nominal",USETAD=USETAD)
  data <- res$data
  xlabtext <- res$xlabtext
  data <- data[!is.na(data$CONCPLOT),]
  if (logY) data <- data[data$CONCPLOT!=0,]
  data <- data[!is.na(data$TIMEPLOT),]
  xlim <- c(min(data$TIMEPLOT),max(data$TIMEPLOT))
  if (strat=="GROUP") {
    data <- dplyr::arrange(data, GROUPN, PROFILE)
    data$STRATification <- data[[strat]]
  } else {
    data$STRATification <- data[[strat]]
    data <- dplyr::arrange(data, STRATification, PROFILE)
  }
  data$STRATification <- factor(data$STRATification, levels = unique(data$STRATification))
  data$CONCPLOTDN <- data$CONCPLOT/data$DOSE
  CONCUNITDN <- paste0(data$CONCUNIT[1],"/",data$DOSEUNIT[1])
  dS <- split(data,data$PROFILE)
  ylim <- c(0,-Inf)
  if (logY) {
    if (data$LLOQ[1]==0) {
      ylim[1] <- min(data$CONCPLOT)/2/max(data$DOSE,na.rm = TRUE)
    } else {
      ylim[1] <- data$LLOQ[1]/3/max(data$DOSE,na.rm = TRUE)
    }
  }
  allplots <- lapply(seq_along(dS), function (k) {
    figureindex <- k
    d <- dS[[k]]
    d2S <- split(d,d$STRATification)
    stats <- do.call(rbind,lapply(seq_along(d2S), function (kx) {
      d2 <- d2S[[kx]]
      if (nrow(d2)==0) return (NULL)
      statsk <- suppressWarnings(statXYnominal(x=d2$TIMEPLOT,y=d2$CONCPLOTDN))
      statsk$MEAN_MINUS_SD <- statsk$MEAN.VALUE-statsk$SD.VALUE
      if (logY) {
        statsk$MEAN_MINUS_SD[statsk$MEAN_MINUS_SD<=0] <- min(d$LLOQ)/10
      } else {
        statsk$MEAN_MINUS_SD[statsk$MEAN_MINUS_SD<=0] <- 0
      }
      statsk$MEAN_PLUS_SD <- statsk$MEAN.VALUE+statsk$SD.VALUE
      cbind(STRAT=names(d2S)[kx],statsk[,c("TIME","MEAN.VALUE","MEAN_MINUS_SD","MEAN_PLUS_SD")])
    }))
    p <- IQRggplot(data=stats,aes(x=TIME,y=MEAN.VALUE,group=STRAT,color=STRAT,linetype=STRAT,shape=STRAT))
    p <- p + scale_color_IQnca(strat)
    p <- p + scale_shape_manual(strat,values = c(1:100))
    p <- p + scale_linetype_manual(strat,values = c(1:100))
    p <- p + geom_line(alpha=alpha) + geom_point(alpha=alpha, size=3)
    p <- p + geom_uperrorbar(aes(x=TIME,ymax=MEAN_PLUS_SD), width=max(stats$TIME)/33)
    if (logY) p <- p + scale_y_log10_IQnca(labeltype = "natural")
    p <- p + xlab(paste0(xlabtext," [",getTIMEUNITname_IQdataNCA(d),"]"))
    p <- p + ylab(paste0("Dose-normalized Concentration [",CONCUNITDN,"]"))
    p <- p + scale_x_continuous(breaks=gettimebreaksplotindiv(d)$breaks,minor_breaks = gettimebreaksplotindiv(d)$minorbreaks)
    loadSetupOptions_IQnca()
    title <- .figure_summary_mean_dosenorm_pkconc
    title <- updateFigureNumberTitle_IQdataNCA(title,figurenumber,figureindex)
    title <- paste0(title,"\nof ",d$MATRIX[1]," ",d$COMPOUND[1], " (",d$ANALYTE[1],")")
    if (logY) {
      title <- paste0(title," - Log scale")
    } else {
      title <- paste0(title," - Linear scale")
    }
    title
    subtitle <- paste0(
      d$PROFILE[1]
    )
    p <- p + ggtitle(title,subtitle=subtitle)
    ylim[2] <<- max(stats$MEAN_PLUS_SD,ylim[2],na.rm = TRUE)
    p
  })
  if (!scaleXsame) xlim <- NULL
  if (!scaleYsame) ylim <- NULL
  allplots <- lapply(allplots, function (p) p <- p + coord_cartesian(xlim = xlim,ylim = ylim))
  if (is.null(filename)) return(allplots)
  suppressWarnings(IQRoutputFigure(x = allplots,
                                   opt.layout = opt.layout(legend.option = "as.is",ncol = floor(sqrt(nindiv)),nrow=ceiling(nindiv/floor(sqrt(nindiv)))),
                                   opt.pagesize = opt.pagesize(scale = getScaleIQRoutputFigure_nindiv(nindiv)),
                                   filename = paste0(gsub(".pdf","",filename),".pdf")))
  unlink(paste0(gsub(".pdf","",filename),".pdf.log"))
  genRMDfigurefile_IQdataNCA(title=.figure_summary_mean_dosenorm_pkconc,figurenumber=figurenumber,logY=logY,filename=filename,strat=strat,stratdefault="GROUP")
  invisible(NULL)
}
#' Plot of summary PK concentrations (geometric mean and 95% CI)
#'
#' Always only for nominal time (NTIME)!
#' Stratified by PROFILE and GROUP (ordered by GROUPN). One plot per stratum.
#' Ignored subjects (IGNOREI) and ignored records (IGNORER and IGNORSUM) are
#' excluded from these plots.
#'
#' @param data IQdataNCA object
#' @param figurenumber Character string with figure number information, added to the title for each figure, if defined
#' @param logY Log y axis is TRUE, otherwise linear Y axis
#' @param scaleXsame logical. It TRUE then the same min anx max value on the X-axis will be used for each plot
#' @param scaleYsame logical. It TRUE then the same min anx max value on the Y-axis will be used for each plot
#' @param filename Name of PDF file to generate with plots
#' @param nindiv Number of plots per page if exported to PDF
#' @param USETAD logical. TRUE uses NTIME. FALSE will use NTAFD
#' @param alpha alpha value for the plotting.
#' @return Returns a list of all plots if filename is NULL
#' @export
#' @family NCA Data Figures
figure_summary_geomean_IQdataNCA <- function (data,figurenumber=NULL,logY=TRUE,scaleXsame=TRUE,scaleYsame=TRUE,filename="summarygeommean.pdf",nindiv=1,USETAD=TRUE,alpha=0.8) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  message("Generating PK concentration figures summary geometric mean ...")
  data <- removeFrom_IQdataNCA(data = data,
                               FLAGremoveIGNOREI = TRUE,
                               FLAGremoveIGNORER = TRUE,
                               FLAGremoveIGNORSUM = TRUE,
                               FLAGremoveIGNORNCA = FALSE)
  res <- handleCONCTIMEplot_IQdataNCA(d = data,logY = logY,FLAGTIME="nominal",USETAD=USETAD)
  data <- res$data
  xlabtext <- res$xlabtext
  data <- data[!is.na(data$CONCPLOT),]
  if (logY) data <- data[data$CONCPLOT!=0,]
  data <- data[!is.na(data$TIMEPLOT),]
  xlim <- c(min(data$TIMEPLOT),max(data$TIMEPLOT))
  data <- dplyr::arrange(data,GROUPN)
  data$GROUP <- factor(data$GROUP, levels = unique(data$GROUP))
  dS <- split(data, data$PROFILE)
  ylim <- c(0,-Inf)
  if (logY) {
    if (data$LLOQ[1]==0) {
      ylim[1] <- min(data$CONCPLOT)/2
    } else {
      ylim[1] <- data$LLOQ[1]/3
    }
  }
  allplots <- lapply(seq_along(dS), function (k) {
    figureindex <- k
    d <- dS[[k]]
    d2S <- split(d,d$GROUP)
    stats <- do.call(rbind,lapply(seq_along(d2S), function (kx) {
      d2 <- d2S[[kx]]
      if (nrow(d2)==0) return (NULL)
      statsk <- suppressWarnings(statXYnominal(x=d2$TIMEPLOT,y=d2$CONCPLOT))
      statsk$MEAN_MINUS_SD <- statsk$MEAN.VALUE-statsk$SD.VALUE
      if (logY) {
        statsk$MEAN_MINUS_SD[statsk$MEAN_MINUS_SD<=0] <- min(d$LLOQ)/10
      } else {
        statsk$MEAN_MINUS_SD[statsk$MEAN_MINUS_SD<=0] <- 0
      }
      statsk$MEAN_PLUS_SD <- statsk$MEAN.VALUE+statsk$SD.VALUE
      cbind(GROUP=names(d2S)[kx],statsk[,c("TIME","GEOMMEAN.VALUE","GEOMSD.VALUE")])
    }))
    stats$GEOMMEAN_CI95_LOW <- exp(log(stats$GEOMMEAN.VALUE) - 1.96*log(stats$GEOMSD.VALUE))
    stats$GEOMMEAN_CI95_HIGH <- exp(log(stats$GEOMMEAN.VALUE) + 1.96*log(stats$GEOMSD.VALUE))
    p <- IQRggplot(data=stats,aes(x=TIME,y=GEOMMEAN.VALUE,group=GROUP,color=GROUP,linetype=GROUP,shape=GROUP))
    p <- p + scale_color_IQnca()
    p <- p + scale_shape_manual(values = c(1:100))
    p <- p + scale_linetype_manual(values = c(1:100))
    p <- p + geom_line(alpha=alpha) + geom_point(alpha=alpha, size=3)
    p <- p + geom_uperrorbar(aes(x=TIME,ymax=GEOMMEAN_CI95_HIGH), width=max(stats$TIME)/33)
    if (logY) p <- p + scale_y_log10_IQnca(labeltype = "natural")
    p <- p + xlab(paste0(xlabtext," [",getTIMEUNITname_IQdataNCA(d),"]"))
    p <- p + ylab(paste0("Concentration [",d$CONCUNIT[1],"]"))
    p <- p + scale_x_continuous(breaks=gettimebreaksplotindiv(d,xlim = xlim)$breaks,minor_breaks = gettimebreaksplotindiv(d,xlim = xlim)$minorbreaks)
    loadSetupOptions_IQnca()
    title <- .figure_summary_geommean_pkconc
    title <- updateFigureNumberTitle_IQdataNCA(title,figurenumber,figureindex)
    title <- paste0(title,"\nof ",d$MATRIX[1]," ",d$COMPOUND[1], " (",d$ANALYTE[1],")")
    if (logY) {
      title <- paste0(title," - Log scale")
    } else {
      title <- paste0(title," - Linear scale")
    }
    title
    subtitle <- paste0(
      d$PROFILE[1]
    )
    p <- p + ggtitle(title,subtitle=subtitle)
    ylim[2] <<- max(stats$GEOMMEAN_CI95_HIGH,ylim[2],na.rm = TRUE)
    p
  })
  if (!scaleXsame) xlim <- NULL
  if (!scaleYsame) ylim <- NULL
  allplots <- lapply(allplots, function (p) p <- p + coord_cartesian(xlim = xlim,ylim = ylim))
  if (is.null(filename)) return(allplots)
  suppressWarnings(IQRoutputFigure(x = allplots,
                                   opt.layout = opt.layout(legend.option = "as.is",ncol = floor(sqrt(nindiv)),nrow=ceiling(nindiv/floor(sqrt(nindiv)))),
                                   opt.pagesize = opt.pagesize(scale = getScaleIQRoutputFigure_nindiv(nindiv)),
                                   filename = paste0(gsub(".pdf","",filename),".pdf")))
  unlink(paste0(gsub(".pdf","",filename),".pdf.log"))
  genRMDfigurefile_IQdataNCA(title=.figure_summary_geommean_pkconc,figurenumber=figurenumber,logY=logY,filename=filename,strat=NULL,stratdefault=NULL)
  invisible(NULL)
}
#' Plot of summary PK concentrations (arithmetic mean and SD)
#'
#' Always only for nominal time (NTIME)!
#' Stratified by PROFILE and GROUP (ordered by GROUPN). One plot per stratum.
#' Ignored subjects (IGNOREI) and ignored records (IGNORER and IGNORSUM) are
#' excluded from these plots.
#'
#' @param data IQdataNCA object
#' @param figurenumber Character string with figure number information, added to the title for each figure, if defined
#' @param logY Log y axis is TRUE, otherwise linear Y axis
#' @param scaleXsame logical. It TRUE then the same min anx max value on the X-axis will be used for each plot
#' @param scaleYsame logical. It TRUE then the same min anx max value on the Y-axis will be used for each plot
#' @param filename Name of PDF file to generate with plots
#' @param nindiv Number of plots per page if exported to PDF
#' @param USETAD logical. TRUE uses NTIME. FALSE will use NTAFD
#' @param alpha alpha value for the plotting.
#' @return Returns a list of all plots if filename is NULL
#' @export
#' @family NCA Data Figures
figure_summary_mean_IQdataNCA <- function (data,figurenumber=NULL,logY=TRUE,scaleXsame=TRUE,scaleYsame=TRUE,filename="summarymean.pdf",nindiv=1,USETAD=TRUE,alpha=0.8) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  message("Generating PK concentration figures summary mean ...")
  data <- removeFrom_IQdataNCA(data = data,
                               FLAGremoveIGNOREI = TRUE,
                               FLAGremoveIGNORER = TRUE,
                               FLAGremoveIGNORSUM = TRUE,
                               FLAGremoveIGNORNCA = FALSE)
  res <- handleCONCTIMEplot_IQdataNCA(d = data,logY = logY,FLAGTIME="nominal",USETAD=USETAD)
  data <- res$data
  xlabtext <- res$xlabtext
  data <- data[!is.na(data$CONCPLOT),]
  if (logY) data <- data[data$CONCPLOT!=0,]
  data <- data[!is.na(data$TIMEPLOT),]
  xlim <- c(min(data$TIMEPLOT),max(data$TIMEPLOT))
  data <- dplyr::arrange(data,GROUPN)
  data$GROUP <- factor(data$GROUP, levels = unique(data$GROUP))
  dS <- split(data, data$PROFILE)
  ylim <- c(0,-Inf)
  if (logY) {
    if (data$LLOQ[1]==0) {
      ylim[1] <- min(data$CONCPLOT)/2
    } else {
      ylim[1] <- data$LLOQ[1]/3
    }
  }
  allplots <- lapply(seq_along(dS), function (k) {
    figureindex <- k
    d <- dS[[k]]
    d2S <- split(d,d$GROUP)
    stats <- do.call(rbind,lapply(seq_along(d2S), function (kx) {
      d2 <- d2S[[kx]]
      if (nrow(d2)==0) return (NULL)
      statsk <- suppressWarnings(statXYnominal(x=d2$TIMEPLOT,y=d2$CONCPLOT))
      statsk$MEAN_MINUS_SD <- statsk$MEAN.VALUE-statsk$SD.VALUE
      if (logY) {
        statsk$MEAN_MINUS_SD[statsk$MEAN_MINUS_SD<=0] <- min(d$LLOQ)/10
      } else {
        statsk$MEAN_MINUS_SD[statsk$MEAN_MINUS_SD<=0] <- 0
      }
      statsk$MEAN_PLUS_SD <- statsk$MEAN.VALUE+statsk$SD.VALUE
      cbind(GROUP=names(d2S)[kx],statsk[,c("TIME","MEAN.VALUE","MEAN_MINUS_SD","MEAN_PLUS_SD")])
    }))
    p <- IQRggplot(data=stats,aes(x=TIME,y=MEAN.VALUE,group=GROUP,color=GROUP,linetype=GROUP,shape=GROUP))
    p <- p + scale_color_IQnca(drop=FALSE)
    p <- p + scale_shape_manual(values = c(1:100))
    p <- p + scale_linetype_manual(values = c(1:100))
    p <- p + geom_line(alpha=alpha) + geom_point(alpha=alpha, size=3)
    p <- p + geom_uperrorbar(aes(x=TIME,ymax=MEAN_PLUS_SD), width=max(stats$TIME)/33)
    if (logY) p <- p + scale_y_log10_IQnca(labeltype = "natural")
    p <- p + xlab(paste0(xlabtext," [",getTIMEUNITname_IQdataNCA(d),"]"))
    p <- p + ylab(paste0("Concentration [",d$CONCUNIT[1],"]"))
    p <- p + scale_x_continuous(breaks=gettimebreaksplotindiv(d,xlim = xlim)$breaks,minor_breaks = gettimebreaksplotindiv(d,xlim = xlim)$minorbreaks)
    loadSetupOptions_IQnca()
    title <- .figure_summary_mean_pkconc
    title <- updateFigureNumberTitle_IQdataNCA(title,figurenumber,figureindex)
    title <- paste0(title,"\nof ",d$MATRIX[1]," ",d$COMPOUND[1], " (",d$ANALYTE[1],")")
    if (logY) {
      title <- paste0(title," - Log scale")
    } else {
      title <- paste0(title," - Linear scale")
    }
    title
    subtitle <- paste0(
      d$PROFILE[1]
    )
    p <- p + ggtitle(title,subtitle=subtitle)
    ylim[2] <<- max(stats$MEAN_PLUS_SD,ylim[2],na.rm = TRUE)
    p
  })
  if (!scaleXsame) xlim <- NULL
  if (!scaleYsame) ylim <- NULL
  allplots <- lapply(allplots, function (p) p <- p + coord_cartesian(xlim = xlim,ylim = ylim))
  if (is.null(filename)) return(allplots)
  suppressWarnings(IQRoutputFigure(x = allplots,
                                   opt.layout = opt.layout(legend.option = "as.is",ncol = floor(sqrt(nindiv)),nrow=ceiling(nindiv/floor(sqrt(nindiv)))),
                                   opt.pagesize = opt.pagesize(scale = getScaleIQRoutputFigure_nindiv(nindiv)),
                                   filename = paste0(gsub(".pdf","",filename),".pdf")))
  unlink(paste0(gsub(".pdf","",filename),".pdf.log"))
  genRMDfigurefile_IQdataNCA(title=.figure_summary_mean_pkconc,figurenumber=figurenumber,logY=logY,filename=filename,strat=NULL,stratdefault=NULL)
  invisible(NULL)
}
#' Plot dose-normalized summary spaghetti plots of IQdataNCA objects
#'
#' Since dose-normalization is done, standard stratifictation is done by PROFILE.
#' Additional stratification can be provided by the user and is shown in different colors.
#' Default stratification is by GROUP (ordered by GROUPN).
#' Ignored records (IGNORER) are excluded from these plots.
#'
#' @param data IQdataNCA object
#' @param figurenumber Character string with figure number information, added to the title for each figure, if defined
#' @param logY Log y axis is TRUE, otherwise linear Y axis
#' @param strat Character. Name of column to use for stratification (default: GROUP). The only requirement
#' is that the entries in the stratification column are unique per subject per profile. The columns can contain
#' numeric of aphanumeric entries. They can be categorical or continuous. However, it is advisable for the purpose
#' of useful plots to use continuous numeric columns only for stratification when there is a certain limited
#' number of unique values. But it is your call entirely :-)
#' @param scaleXsame logical. It TRUE then the same min anx max value on the X-axis will be used for each plot
#' @param scaleYsame logical. It TRUE then the same min anx max value on the Y-axis will be used for each plot
#' @param filename Name of PDF file to generate with plots
#' @param nindiv Number of plots per page if exported to PDF
#' @param FLAGTIME Character string to defined the time information used ("asis", "nominal", "actual").
#' "asis" will use the selected time information in the dataset (TIME or TAFD). "nominal" will use NTIME or NTAFD.
#' "actual" will use ATIME or ATAFD.
#' @param USETAD logical. TRUE uses TIME, NTIME, or ATIME, depending on FLAGTIME settings. FALSE will use
#' TAFD, NTAFD, or ATAFD.
#' @param alpha alpha value for the plotting.
#' @return Returns a list of all plots if filename is NULL
#' @export
#' @family NCA Data Figures
figure_spaghetti_dosenorm_IQdataNCA <- function (data,figurenumber=NULL,logY=TRUE,strat="GROUP",scaleXsame=TRUE,scaleYsame=TRUE,filename="spaghettidn.pdf",nindiv=1,FLAGTIME="nominal",USETAD=TRUE,alpha=0.5) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  if (!FLAGTIME %in% c("asis","nominal","actual")) stopIQR("FLAGTIME argument should be 'asis', 'nominal', or 'actual'")
  if (!strat %in% names(data)) stopIQR("Colum '",group,"' is not available in the data - select a different column name for stratification")
  message("Generating PK concentration figures spaghetti dose-normalized ...")
  if (any(unlist(sapply(split(data,data$PROFILE), function (d) {
    sapply(split(d,d$USUBJID), function (d2) {
      length(unique(d2[[strat]]))
    })
  })) != 1)) stopIQR("Chosen 'strat' column does not contain unique entries per subject per profile")
  data <- removeFrom_IQdataNCA(data = data,
                               FLAGremoveIGNOREI = FALSE,
                               FLAGremoveIGNORER = TRUE,
                               FLAGremoveIGNORSUM = FALSE,
                               FLAGremoveIGNORNCA = FALSE)
  res <- handleCONCTIMEplot_IQdataNCA(d = data,logY = logY,FLAGTIME=FLAGTIME,USETAD=USETAD)
  data <- res$data
  xlabtext <- res$xlabtext
  data <- data[!is.na(data$CONCPLOT),]
  if (logY) data <- data[data$CONCPLOT!=0,]
  data <- data[!is.na(data$TIMEPLOT),]
  xlim <- c(min(data$TIMEPLOT),max(data$TIMEPLOT))
  if (strat=="GROUP") {
    data <- dplyr::arrange(data, GROUPN, PROFILE)
    data$STRATification <- data[[strat]]
    data$STRATification <- factor(data$STRATification, levels = sort(unique(data$STRATification)))
  } else {
    data$STRATification <- data[[strat]]
    data$STRATification <- factor(data$STRATification, levels = sort(unique(data$STRATification)))
    data <- dplyr::arrange(data, STRATification, PROFILE)
  }
  data$CONCPLOTDN <- data$CONCPLOT/data$DOSE
  CONCUNITDN <- paste0(data$CONCUNIT[1],"/",data$DOSEUNIT[1])
  data$BLLOQINFO <- "Non-BLLOQ"
  data$BLLOQINFO[data$BLLOQ %in% 1] <- "BLLOQ"
  data$BLLOQINFO <- factor(data$BLLOQINFO, levels = c("Non-BLLOQ","BLLOQ"))
  dS <- split(data,data$PROFILE)
  ylim <- c(0,-Inf)
  if (logY) {
    if (data$LLOQ[1]==0) {
      ylim[1] <- min(data$CONCPLOT)/2/max(data$DOSE,na.rm = TRUE)
    } else {
      ylim[1] <- data$LLOQ[1]/3/max(data$DOSE,na.rm = TRUE)
    }
  }
  allplots <- lapply(seq_along(dS), function (k) {
    figureindex <- k
    d <- dS[[k]]
    p <- IQRggplot(data=d,aes(x=TIMEPLOT,y=CONCPLOTDN,group=USUBJID,color=STRATification))
    p <- p + scale_color_IQnca(strat)
    p <- p + geom_line(alpha=alpha) + geom_point(aes(shape=BLLOQINFO),alpha=alpha,size=3)
    p <- p + scale_shape_discrete("Type")
    if (logY) p <- p + scale_y_log10_IQnca(labeltype = "natural")
    p <- p + xlab(paste0(xlabtext," [",getTIMEUNITname_IQdataNCA(d),"]"))
    p <- p + ylab(paste0("Dose-normalized Concentration [",CONCUNITDN,"]"))
    p <- p + scale_x_continuous(breaks=gettimebreaksplotindiv(d,xlim = xlim)$breaks,minor_breaks = gettimebreaksplotindiv(d,xlim = xlim)$minorbreaks)
    loadSetupOptions_IQnca()
    title <- .figure_spaghetti_dosenormalized_pkconc
    title <- updateFigureNumberTitle_IQdataNCA(title,figurenumber,figureindex)
    title <- paste0(title,"\nof ",d$MATRIX[1]," ",d$COMPOUND[1], " (",d$ANALYTE[1],")")
    if (logY) {
      title <- paste0(title," - Log scale")
    } else {
      title <- paste0(title," - Linear scale")
    }
    title
    subtitle <- paste0(
      d$PROFILE[1]
    )
    p <- p + ggtitle(title,subtitle=subtitle)
    ylim[2] <<- max(d$CONCPLOTDN,ylim[2],na.rm = TRUE)
    p
  })
  if (!scaleXsame) xlim <- NULL
  if (!scaleYsame) ylim <- NULL
  allplots <- lapply(allplots, function (p) p <- p + coord_cartesian(xlim = xlim,ylim = ylim))
  if (is.null(filename)) return(allplots)
  suppressWarnings(IQRoutputFigure(x = allplots,
                                   opt.layout = opt.layout(legend.option = "as.is",ncol = floor(sqrt(nindiv)),nrow=ceiling(nindiv/floor(sqrt(nindiv)))),
                                   opt.pagesize = opt.pagesize(scale = getScaleIQRoutputFigure_nindiv(nindiv)),
                                   filename = paste0(gsub(".pdf","",filename),".pdf")))
  unlink(paste0(gsub(".pdf","",filename),".pdf.log"))
  genRMDfigurefile_IQdataNCA(title=.figure_spaghetti_dosenormalized_pkconc,figurenumber=figurenumber,logY=logY,filename=filename,strat=strat,stratdefault="GROUP")
  invisible(NULL)
}
genRMDfigurefile_IQdataNCA <- function (title,figurenumber,logY,filename,strat=NULL,stratdefault=NULL) {
  title <- updateFigureNumberTitle_IQdataNCA(title,figurenumber,figureindex = NULL)
  if (!is.null(stratdefault) & !is.null(strat)) {
    if (strat!=stratdefault) {
      title <- paste0(title," - stratified by ",strat)
    }
  }
  if (!is.null(logY)) {
    if (logY) {
      title <- paste0(title," - Log scale")
    } else {
      title <- paste0(title," - Linear scale")
    }
  }
  figurefile <- paste0(gsub(".pdf","",filename),".pdf")
  figurelegend <- paste0(
    "IQnca version: ", aux_version(pkgName = "IQnca"), "\n",
    "Script: ", getScriptName_IQdataNCA(),"\n",
    "Output: ",figurefile, "\n",
    "Execution date: ", Sys.time()
  )
  text <- rmdEMPTY() +
    rmdLANDSCAPE() +
    rmdSECTION(title = title,
               level = 1,numbered = FALSE) +
    rmdFIGURE(file = figurefile,ignoreCaption = TRUE,legend = figurelegend)
  filenameRMD <- paste0(gsub(".pdf","",filename),".rmd")
  export_IQRrmd(text,filename = filenameRMD)
}
#' Plot summary spaghetti plots of IQdataNCA objects (non-dose-normalized)
#'
#' Stratified by GROUP and PROFILE in the data. Ordered by GROUP(N) and then PROFILE. Default colored by USUBJID.
#' Only ignored records (IQGNORER) are excluded from these plots. Stratification in terms of coloring by selected column
#' can be done.
#'
#' @param data IQdataNCA object
#' @param figurenumber Character string with figure number information, added to the title for each figure, if defined
#' @param logY Log y axis is TRUE, otherwise linear Y axis
#' @param strat Character. Name of column to use for stratification (default: NULL => no stratification). The only requirement
#' is that the entries in the stratification column are unique per subject per profile. The columns can contain
#' numeric of aphanumeric entries. They can be categorical or continuous. However, it is advisable for the purpose
#' of useful plots to use continuous numeric columns only for stratification when there is a certain limited
#' number of unique values. But it is your call entirely :-)
#' @param scaleXsame logical. It TRUE then the same min anx max value on the X-axis will be used for each plot
#' @param scaleYsame logical. It TRUE then the same min anx max value on the Y-axis will be used for each plot
#' @param filename Name of PDF file to generate with plots
#' @param nindiv Number of plots per page if exported to PDF
#' @param FLAGTIME Character string to defined the time information used ("asis", "nominal", "actual").
#' "asis" will use the selected time information in the dataset (TIME or TAFD). "nominal" will use NTIME or NTAFD.
#' "actual" will use ATIME or ATAFD.
#' @param USETAD logical. TRUE uses TIME, NTIME, or ATIME, depending on FLAGTIME settings. FALSE will use
#' TAFD, NTAFD, or ATAFD.
#' @param alpha alpha value for the plotting.
#' @return Returns a list of all plots if filename is NULL
#' @export
#' @family NCA Data Figures
figure_spaghetti_IQdataNCA <- function (data,figurenumber=NULL,logY=TRUE,strat="USUBJID",scaleXsame=TRUE,scaleYsame=TRUE,filename="spaghetti.pdf",nindiv=1,FLAGTIME="nominal",USETAD=TRUE,alpha=0.8) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  if (!FLAGTIME %in% c("asis","nominal","actual")) stopIQR("FLAGTIME argument should be 'asis', 'nominal', or 'actual'")
  if (!is.null(strat)) {
    if (!strat %in% names(data)) stopIQR("Colum '", strat, "' is not available in the data - select a different column name for stratification")
  }
  message("Generating PK concentration figures spaghetti ...")
  data <- removeFrom_IQdataNCA(data = data,
                               FLAGremoveIGNOREI = FALSE,
                               FLAGremoveIGNORER = TRUE,
                               FLAGremoveIGNORSUM = FALSE,
                               FLAGremoveIGNORNCA = FALSE)
  res <- handleCONCTIMEplot_IQdataNCA(d = data,logY = logY,FLAGTIME=FLAGTIME,USETAD=USETAD)
  data <- res$data
  xlabtext <- res$xlabtext
  data <- data[!is.na(data$CONCPLOT),]
  if (logY) data <- data[data$CONCPLOT!=0,]
  data <- data[!is.na(data$TIMEPLOT),]
  xlim <- c(min(data$TIMEPLOT),max(data$TIMEPLOT))
  if (!is.null(strat)) data$STRATification <- data[[strat]]
  ylim <- c(0,-Inf)
  if (logY) {
    if (data$LLOQ[1]==0) {
      ylim[1] <- min(data$CONCPLOT)/2
    } else {
      ylim[1] <- data$LLOQ[1]/3
    }
  }
  data <- dplyr::arrange(data, GROUPN, PROFILE)
  data$SPLIT <- paste(data$GROUPN, data$PROFILE)
  data$SPLIT <- factor(data$SPLIT, levels = unique(data$SPLIT))
  dS <- split(data, data$SPLIT)
  allplots <- lapply(seq_along(dS), function (k) {
    figureindex <- k
    d <- dS[[k]]
    if (!is.null(strat)) {
      d$STRATification <- factor(d$STRATification,levels = sort(unique(d$STRATification)))
      p <- IQRggplot(data=d,aes(x=TIMEPLOT,y=CONCPLOT,group=USUBJID,color=STRATification)) +
        scale_color_IQnca(strat)
    } else {
      p <- IQRggplot(data=d,aes(x=TIMEPLOT,y=CONCPLOT,group=USUBJID))
    }
    p <- p + geom_line(alpha=alpha) + geom_point(alpha=alpha,size=3)
    if (!logY | d$LLOQ[1]>0) {
      p <- p + geom_hline(yintercept = d$LLOQ[1], linetype="dashed")
    }
    if (logY) p <- p + scale_y_log10_IQnca(labeltype = "natural")
    p <- p + xlab(paste0(xlabtext," [",getTIMEUNITname_IQdataNCA(d),"]"))
    p <- p + ylab(paste0("Concentration [",d$CONCUNIT[1],"]"))
    p <- p + scale_x_continuous(breaks=gettimebreaksplotindiv(d,xlim = xlim)$breaks,minor_breaks = gettimebreaksplotindiv(d,xlim = xlim)$minorbreaks)
    loadSetupOptions_IQnca()
    title <- .figure_spaghetti_pkconc
    title <- updateFigureNumberTitle_IQdataNCA(title,figurenumber,figureindex)
    title <- paste0(title,"\nof ",d$MATRIX[1]," ",d$COMPOUND[1], " (",d$ANALYTE[1],")")
    if (logY) {
      title <- paste0(title," - Log scale")
    } else {
      title <- paste0(title," - Linear scale")
    }
    subtitle <- paste0(
      d$GROUP[1], " (",d$PROFILE[1],")"
    )
    p <- p + ggtitle(title,subtitle=subtitle)
    ylim[2] <<- max(d$CONCPLOT,ylim[2],na.rm = TRUE)
    p
  })
  if (!scaleXsame) xlim <- NULL
  if (!scaleYsame) ylim <- NULL
  allplots <- lapply(allplots, function (p) p <- p + coord_cartesian(xlim = xlim,ylim = ylim))
  if (is.null(filename)) return(allplots)
  IQRoutputFigure(x = allplots,
                  opt.layout = opt.layout(legend.option = "as.is",ncol = floor(sqrt(nindiv)),nrow=ceiling(nindiv/floor(sqrt(nindiv)))),
                  opt.pagesize = opt.pagesize(scale = getScaleIQRoutputFigure_nindiv(nindiv)),
                  filename = paste0(gsub(".pdf","",filename),".pdf"))
  unlink(paste0(gsub(".pdf","",filename),".pdf.log"))
  genRMDfigurefile_IQdataNCA(title=.figure_spaghetti_pkconc,figurenumber=figurenumber,logY=logY,filename=filename,strat=strat,stratdefault="USUBJID")
  invisible(NULL)
}
#' Generate individual PK concentration figures for reporting
#'
#' Ignored records (IGNORER) are removed from these plots. IGNORSUM and IGNORNCA records are included.
#' Ignored subjects (IGNOREI) are included. In addition to a PDF file with the figures an rmd file will
#' be generated in the same folder and same filename (.rmd) extension). the RMD file is for seamless
#' reporting in Word with IQReport.
#'
#' @param data IQdataNCA object
#' @param figurenumber Character string with figure number information, added to the title for each figure, if defined
#' @param logY Log y axis is TRUE, otherwise linear Y axis
#' @param filename Name of PDF file to generate with plots
#' @param nindiv Number of plots per page if exported to PDF
#' @param FLAGTIME Character string to defined the time information used ("asis", "nominal", "actual").
#' "asis" will use the selected time information in the dataset (TIME or TAFD). "nominal" will use NTIME or NTAFD.
#' "actual" will use ATIME or ATAFD.
#' @param USETAD logical. TRUE uses TIME, NTIME, or ATIME, depending on FLAGTIME settings. FALSE will use
#' TAFD, NTAFD, or ATAFD.
#' @return Returns a list of all plots if filename is NULL
#' @export
#' @family NCA Data Figures
figures_indiv_IQdataNCA <- function(data,
                                    figurenumber=NULL,
                                    logY=TRUE,
                                    filename="figures_individual",
                                    nindiv=1,
                                    FLAGTIME="asis",
                                    USETAD=TRUE) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  if (!FLAGTIME %in% c("asis","nominal","actual")) stopIQR("FLAGTIME argument should be 'asis', 'nominal', or 'actual'")
  message("Generating PK concentration figures individual ...")
  d <- removeFrom_IQdataNCA(data = data,
                            FLAGremoveIGNOREI = FALSE,
                            FLAGremoveIGNORER = TRUE,
                            FLAGremoveIGNORSUM = FALSE,
                            FLAGremoveIGNORNCA = FALSE)
  res <- handleCONCTIMEplot_IQdataNCA(d = data,logY = logY,FLAGTIME=FLAGTIME,USETAD=USETAD)
  data <- res$data
  xlabtext <- res$xlabtext
  data <- data[!is.na(data$CONCPLOT),]
  if (logY) data <- data[data$CONCPLOT!=0,]
  data <- data[!is.na(data$TIMEPLOT),]
  xlim <- c(min(data$TIMEPLOT),max(data$TIMEPLOT))
  data$PROFILE <- factor(data$PROFILE, levels=sort(unique(data$PROFILE)))
  dS <- split(data,data$USUBJID)
  allplots <- lapply(seq_along(dS), function (k) {
    figureindivsplit_IQdataNCA(d = dS[[k]],figureindex=k,figurenumber=figurenumber,logY = logY,FLAGTIME=FLAGTIME,USETAD=USETAD,xlabtext=xlabtext,xlim=xlim)
  })
  if (is.null(filename)) return(allplots)
  IQRoutputFigure(x = allplots,
                  opt.layout = opt.layout(legend.option = "as.is",ncol = floor(sqrt(nindiv)),nrow=ceiling(nindiv/floor(sqrt(nindiv)))),
                  opt.pagesize = opt.pagesize(scale = getScaleIQRoutputFigure_nindiv(nindiv)),
                  filename = paste0(gsub(".pdf","",filename),".pdf"))
  unlink(paste0(gsub(".pdf","",filename),".pdf.log"))
  genRMDfigurefile_IQdataNCA(title=.figure_individual_pkconc,figurenumber=figurenumber,logY=logY,filename=filename,strat=NULL,stratdefault=NULL)
  invisible(NULL)
}
handleCONCTIMEplot_IQdataNCA <- function (d,logY=TRUE,FLAGTIME="nominal",USETAD=TRUE) {
  if (!is_IQdataNCA(d)) stopIQR("d is not an IQdataNCA object")
  if (!FLAGTIME %in% c("asis","nominal","actual")) stopIQR("FLAGTIME argument should be 'asis', 'nominal', or 'actual'")
  d$CONCPLOT <- d$CONCPLIN
  if (logY) d$CONCPLOT <- d$CONCPLOG
  if (USETAD) {
    if (FLAGTIME=="asis") {
      d$TIMEPLOT <- d$TIME
      xlabtext <- "Nominal time"
      if (d$FLAGTIME[1] == "actual") {
        xlabtext <- "Actual time"
        if (d$FATIMIMP[1] == "nominal") {
          xlabtext <- "Nominal time"
        }
      }
    }
    if (FLAGTIME=="actual") {
      d$TIMEPLOT <- d$ATIME
      xlabtext <- "Actual time"
    }
    if (FLAGTIME=="nominal") {
      d$TIMEPLOT <- d$NTIME
      xlabtext <- "Nominal time"  }
  } else {
    if (FLAGTIME=="asis") {
      d$TIMEPLOT <- d$TAFD
      xlabtext <- "Nominal time post first dose"
      if (d$FLAGTIME[1] == "actual") {
        xlabtext <- "Actual time post first dose"
        if (d$FATIMIMP[1] == "nominal") {
          xlabtext <- "Actual time post first dose"
        }
      }
    }
    if (FLAGTIME=="actual") {
      d$TIMEPLOT <- d$ATAFD
      xlabtext <- "Actual time post first dose"
    }
    if (FLAGTIME=="nominal") {
      d$TIMEPLOT <- d$NTAFD
      xlabtext <- "Nominal time post first dose"
    }
  }
  d <- d[!is.na(d$TIMEPLOT),]
  d <- d[!is.na(d$CONCPLOT),]
  list(data=d,xlabtext=xlabtext)
}
figureindivsplit_IQdataNCA <- function (d,figureindex,figurenumber=NULL,logY=TRUE,FLAGTIME="asis",USETAD=TRUE,xlabtext,xlim) {
  if (length(unique(d$USUBJID))!=1) stopIQR("USUBJID not unique")
  p <- IQRggplot(d,aes(x=TIMEPLOT,y=CONCPLOT,color=PROFILE,linetype=PROFILE))
  p <- p + geom_line(size=0.5) +
    geom_point(size=2)
  if (!logY | d$LLOQ[1]>0) {
    p <- p + geom_hline(yintercept = d$LLOQ[1], linetype="dashed")
  }
  p <- p + scale_color_IQnca("Profile")
  p <- p + scale_linetype_manual("Profile",values = c(1:100))
  if (logY) p <- p + scale_y_log10_IQnca(labeltype = "natural")
  p <- p + theme(legend.position="bottom") + guides(col=guide_legend(nrow = 2))
  p <- p + xlab(paste0(xlabtext," [",getTIMEUNITname_IQdataNCA(d),"]"))
  p <- p + ylab(paste0("Concentration [",d$CONCUNIT[1],"]"))
  p <- p + scale_x_continuous(breaks=gettimebreaksplotindiv(d,xlim = xlim)$breaks,minor_breaks = gettimebreaksplotindiv(d,xlim = xlim)$minorbreaks)
  loadSetupOptions_IQnca()
  title <- .figure_individual_pkconc
  title <- updateFigureNumberTitle_IQdataNCA(title,figurenumber,figureindex)
  title <- paste0(title,"\nof ",d$MATRIX[1]," ",d$COMPOUND[1], " (",d$ANALYTE[1],")")
  if (logY) {
    title <- paste0(title," - Log scale")
  } else {
    title <- paste0(title," - Linear scale")
  }
  title
  subtitle <- paste0(
    "Subject: ", d$USUBJID[1]
  )
  p <- p + ggtitle(title,subtitle=subtitle)
  p <- p + coord_cartesian(xlim=xlim)
  p
}
gettimebreaksplotindiv <- function (d,xlim=NULL) {
  loadSetupOptions_IQnca()
  maxTIMEPLOT <- max(c(max(d$TIMEPLOT,na.rm = TRUE),xlim))
  if (getTIMEUNITname_IQdataNCA(d)=="second") {
    by <- 60
    byminor <- 10
    if (by > maxTIMEPLOT) {
      by <- floor(maxTIMEPLOT/10)*10
      if (by==0) by <- 1
      byminor <- by/5
    }
    if (maxTIMEPLOT / by > .maxNxticks) {
      by <- by*floor(by/.maxNxticks*floor(maxTIMEPLOT/by) / by)
      byminor <- by/6
    }
    breaks <- seq(0,maxTIMEPLOT,by = by)
    minorbreaks <- seq(0,maxTIMEPLOT,by = byminor)
  }
  if (getTIMEUNITname_IQdataNCA(d)=="min") {
    by <- 60
    byminor <- 10
    if (by > maxTIMEPLOT) {
      by <- floor(maxTIMEPLOT/10)*10
      if (by==0) by <- 1
      byminor <- by/5
    }
    if (maxTIMEPLOT / by > .maxNxticks) {
      by <- by*floor(by/.maxNxticks*floor(maxTIMEPLOT/by) / by)
      byminor <- by/6
    }
    breaks <- seq(0,maxTIMEPLOT,by = by)
    minorbreaks <- seq(0,maxTIMEPLOT,by = byminor)
  }
  if (getTIMEUNITname_IQdataNCA(d)=="hour") {
    by <- 12
    byminor <- 3
    if (by > maxTIMEPLOT) {
      by <- floor(maxTIMEPLOT/10)*10
      if (by==0) by <- 1
      byminor <- by/5
    }
    if (maxTIMEPLOT / by > .maxNxticks) {
      by <- by*floor(by/.maxNxticks*floor(maxTIMEPLOT/by) / by)
      byminor <- by/4
    }
    breaks <- seq(0,maxTIMEPLOT,by = by)
    minorbreaks <- seq(0,maxTIMEPLOT,by = byminor)
  }
  if (getTIMEUNITname_IQdataNCA(d)=="day") {
    by <- 7
    byminor <- 1
    if (by > maxTIMEPLOT) {
      by <- floor(maxTIMEPLOT/10)*10
      if (by==0) by <- 1
      byminor <- by/5
    }
    if (maxTIMEPLOT / by > .maxNxticks) {
      by <- by*floor(by/.maxNxticks*floor(maxTIMEPLOT/by) / by)
      byminor <- by/7
    }
    breaks <- seq(0,maxTIMEPLOT,by = by)
    minorbreaks <- seq(0,maxTIMEPLOT,by = byminor)
  }
  if (getTIMEUNITname_IQdataNCA(d)=="week") {
    by <- 4
    byminor <- 1
    if (by > maxTIMEPLOT) {
      by <- floor(maxTIMEPLOT/10)*10
      if (by==0) by <- 1
      byminor <- by/5
    }
    if (maxTIMEPLOT / by > .maxNxticks) {
      by <- by*floor(by/.maxNxticks*floor(maxTIMEPLOT/by) / by)
      byminor <- by/4
    }
    breaks <- seq(0,maxTIMEPLOT,by = by)
    minorbreaks <- seq(0,maxTIMEPLOT,by = byminor)
  }
  list(breaks=breaks, minorbreaks=minorbreaks)
}
#' Generate a detailed listing of individual pharmacokinetic concentrations
#'
#' The listing will be generated already as an IQRrmd object, allowing easy reporting in Word with IQReport.
#' The listing is done per USUBJID and provides far more detail than the listing_conc_IQdataNCA function.
#' Important: Ignored records (INGORER) are not reported in these listings! Ignored subjects (IGNOREI) are reported
#' but it is indicated that they are ignored (not in the simple version)
#'
#' @param data IQdataNCA object
#' @param listingnumber Character string with listing number information, added to the title for each table, if defined
#' @param fontsizetable Fontsize to be used in the table
#' @param filename Filename to export the listing IQRrmd object to
#' @param signifATIME Significant digits for actual time
#' @return An IQRrmd object exported to a file
#' @export
#' @family NCA Data Listings
listing_concdetailed_IQdataNCA <- function (data, listingnumber=NULL, fontsizetable=8, filename="listing_concdetailed", signifATIME=5) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  data <- removeFrom_IQdataNCA(data = data,
                               FLAGremoveIGNOREI = FALSE,
                               FLAGremoveIGNORER = TRUE,
                               FLAGremoveIGNORSUM = FALSE,
                               FLAGremoveIGNORNCA = FALSE)
  data <- data[!is.na(data$NTIME),]
  message("Generating PK concentration listings ...")
  if (!is.null(filename)) {
    filename <- paste0(gsub(".rmd","",filename),".rmd")
  }
  loadSetupOptions_IQnca()
  text <- rmdEMPTY() +
    rmdLANDSCAPE() +
    rmdSECTION(title = updateListingNumberTitle_IQdataNCA(.listing_concdetailed_pkconc,listingnumber,NULL),
               level = 1,numbered = FALSE)
  table <- list()
  allUSUBJID <- unique(data$USUBJID)
  for (k in seq_along(allUSUBJID)) {
    d <- data[data$USUBJID==allUSUBJID[k],]
    object <- listingindivPKconc_IQdataNCA(d = d,listingindex = k,listingnumber = listingnumber,fontsizetable = fontsizetable,filename = filename, signifATIME=signifATIME)
    if ("IQRrmd" %in% class(object)) {
      textindiv <- object
      if (k==1) {
        text <- text + textindiv + "\n"
      } else {
        text <- text + rmdNEWPAGE() + "\n" + textindiv + "\n"
      }
    }
    if ("IQRoutputTable" %in% class(object)) {
      table <- c(table, structure(list(object), names = allUSUBJID[k]))
    }
  }
  if (!is.null(filename)) {
    export_IQRrmd(text,filename)
  } else {
    return(table)
  }
}
#' Generate a simple listing of individual pharmacokinetic concentrations
#'
#' The listing will be generated already as an IQRrmd object, allowing easy reporting in Word with IQReport.
#' Simple listing of concentrations vs. nominal time by PROFILE and GROUP.
#' Important: Ignored records (INGORER) are not reported in these listings! Ignored subjects (IGNOREI) are reported
#' but it is indicated that they are ignored (not in the simple version)
#'
#' @param data IQdataNCA object
#' @param listingnumber Character string with listing number information, added to the title for each table, if defined
#' @param fontsizetable Fontsize to be used in the table
#' @param filename Filename to export the listing IQRrmd object to
#' @param signifATIME Significant digits for actual time
#' @param maxCol Number of nominal times to be put into the same table in case of simple=TRUE. Tables might become to wide even for landscape
#' so a plit up of columns in several tables can be enforced in this manner.
#' @return An IQRrmd object exported to a file
#' @export
#' @family NCA Data Listings
listing_conc_IQdataNCA <- function (data,listingnumber=NULL,fontsizetable=8,filename="listing_conc",signifATIME=6,maxCol = 10) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  if (maxCol<4) maxCol <- 4
  data <- removeFrom_IQdataNCA(data = data,
                               FLAGremoveIGNOREI = FALSE,
                               FLAGremoveIGNORER = TRUE,
                               FLAGremoveIGNORSUM = FALSE,
                               FLAGremoveIGNORNCA = FALSE)
  data <- data[!is.na(data$NTIME),]
  message("Generating PK concentration listings ...")
  if (!is.null(filename)) {
    filename <- paste0(gsub(".rmd","",filename),".rmd")
  }
  text <- rmdEMPTY() +
    rmdLANDSCAPE() +
    rmdSECTION(title = updateListingNumberTitle_IQdataNCA(.listing_conc_pkconc,listingnumber,NULL),
               level = 1,numbered = FALSE)
  table <- list()
  data <- dplyr::arrange(data, GROUPN, PROFILE)
  data$SPLIT <- paste(data$GROUP, data$PROFILE)
  data$SPLIT <- factor(data$SPLIT, levels = unique(data$SPLIT))
  dS <- split(data, data$SPLIT)
  for (k in seq_along(dS)) {
    d = dS[[k]]
    object <- listingindivPKconcSimple_IQdataNCA(d = d,listingindex = k,listingnumber = listingnumber,fontsizetable = fontsizetable,filename = filename, maxCol = maxCol)
    if ("IQRrmd" %in% class(object)) {
      textindiv <- object
      if (k==1) {
        text <- text + textindiv + "\n"
      } else {
        text <- text + rmdNEWPAGE() + "\n" + textindiv + "\n"
      }
    }
    if ("IQRoutputTable" %in% class(object)) {
      table <- c(table, structure(list(object), names = as.character(d$SPLIT[1])))
    }
  }
  if (!is.null(filename)) {
    export_IQRrmd(text,filename)
  } else {
    return(table)
  }
}
listingindivPKconcSimple_IQdataNCA <- function (d,listingindex,listingnumber=NULL,fontsizetable=8,filename,maxCol=8) {
  x <- d[,c("GROUP","USUBJID","NTIME","ACONC","BLLOQ")]
  x$ACONC[x$BLLOQ==1] <- "BLQ"
  x$BLLOQ <- NULL
  if (!is.null(filename)) {
    pieces <- aux_splitVectorEqualPieces(x = unique(x$NTIME),maxCol)
  } else {
    pieces <- aux_splitVectorEqualPieces(x = unique(x$NTIME),Inf)
  }
  text <- rmdEMPTY()
  for (kpiece in seq_along(pieces)) {
    NT <- pieces[[kpiece]]
    NT <- NT[!is.na(NT)]
    y <- tidyr::spread(data = x[x$NTIME %in% NT,],key=NTIME,value=ACONC)
    if (ncol(y) >= 4) {
      z <- y[,c(4:ncol(y)),drop=FALSE]
      zz <- rbind(names(z),rep("",ncol(z)),z)
      tab <- data.frame(
        TREATMENT = c("","**Treatment**",y$GROUP),
        SUBJECT = c("","**Subject**",y$USUBJID),
        NTIME1 = c(names(y)[[3]],paste0("**Concentration [", d$CONCUNIT[1],"]**"),y[[3]]),
        zz,
        stringsAsFactors = FALSE
      )
    } else {
      tab <- data.frame(
        TREATMENT = c("","**Treatment**",y$GROUP),
        SUBJECT = c("","**Subject**",y$USUBJID),
        NTIME1 = c(names(y)[[3]],paste0("**Concentration [", d$CONCUNIT[1],"]**"),y[[3]]),
        stringsAsFactors = FALSE
      )
    }
    names(tab)[c(1,2)] <- ""
    names(tab)[3] <- paste0("Nominal Time [", getTIMEUNITsymbol_IQdataNCA(d),"]")
    if (ncol(tab) >= 4) names(tab)[4:ncol(tab)] <- ""
    tab[duplicated(tab[,1]),1] <- ""
    for (kcol in 3:ncol(tab)) {
      ixna <- is.na(tab[,kcol])
      tab[ixna,kcol] <- "NS"
    }
    loadSetupOptions_IQnca()
    title <- .listing_conc_pkconc
    title <- updateListingNumberTitle_IQdataNCA(title,listingnumber,listingindex)
    title <- paste0(title," of ",d$MATRIX[1]," ",d$COMPOUND[1], " (",d$ANALYTE[1],") - ",d$PROFILE[1])
    if (length(pieces)>1) {
      title <- paste0(title," (",kpiece," of ",length(pieces),")")
    }
    if (kpiece > 1 & length(pieces)>1) text <- text + rmdNEWPAGE() + "\n"
    scriptName <- getScriptName_IQdataNCA()
    footertext <- paste0(
      "NS: No Sample.\n",
      "BLQ: Below lower limit of quantitation.\n",
      "Lower limit of quantitation is ",d$LLOQ[1]," ",d$CONCUNIT[1],".\n",
      "IQnca version: ", aux_version(pkgName = "IQnca"), "\n",
      "Script: ", scriptName,"\n",
      "Output: ",filename, "\n",
      "Execution date: ", Sys.time()
    )
    blockIdentifier <- paste0("indiv2_",listingindex)
    text <- text + paste0("!BLOCKSTART[keepNext](",blockIdentifier,")\n")
    text <- text + "**" + title + "**" +
      rmdTABLEDF(tab,footertext = footertext, ignoreCaption = TRUE,fontsize = fontsizetable) + "\n"
    text <- text + paste0("\n!BLOCKEND(",blockIdentifier,")\n")
    table <- IQRoutputTable(tab, xtitle = title, xfooter = footertext)
  }
  if (!is.null(filename)) {
    return(text)
  } else {
    return(table)
  }
}
listingindivPKconc_IQdataNCA <- function (d, listingindex, listingnumber=NULL, fontsizetable=8, filename, signifATIME=signifATIME) {
  info <- d[is.na(d$IGNORER),]
  addcovs <- getaddcovcolumns_IQdataNCA(info)
  info <- as.data.frame(info)
  tab <- as.data.frame(info[,c("IX","PROFILE","GROUP","DOSE","ATIME","NTIME","TIME","CONC","COMMENTR","IGNORSUM","IGNORNCA","BLLOQ","LLOQ")])
  tab$CONC[tab$BLLOQ %in% 1] <- paste0("BLLOQ (<",tab$LLOQ[tab$BLLOQ %in% 1],")")
  tab$BLLOQ <- NULL
  tab$LLOQ <- NULL
  tab$CONC[is.na(tab$CONC)] <- "NV"
  tab$COMMENT <- tab$COMMENTR
  tab$COMMENT[is.na(tab$COMMENT)] <- ""
  tab$COMMENTR <- NULL
  tab$CONC <- sapply(seq_along(tab$CONC), function (k) {
    out <- paste0(tab$CONC[k]," ")
    if (!is.na(tab$IGNORSUM[k])) out <- paste0(out,"o")
    if (!is.na(tab$IGNORNCA[k])) out <- paste0(out,"+")
    aux_strtrim(out)
  })
  tab$IGNORSUM[is.na(tab$IGNORSUM)] <- ""
  tab$IGNORNCA[is.na(tab$IGNORNCA)] <- ""
  tab$IGNORE <- paste0(tab$IGNORSUM,":::",tab$IGNORNCA)
  tab$IGNORE <- gsub("^:::","",tab$IGNORE)
  tab$IGNORSUM <- NULL
  tab$IGNORNCA <- NULL
  tab$COMMENT <- unname(sapply(tab$COMMENT, function (c) {
    if (!grepl(":::",c)) return(c)
    paste0(unique(aux_explode(c,separator = ":::")),collapse = ", ")
  }))
  tab$IGNORE <- unname(sapply(tab$IGNORE, function (c) {
    if (!grepl(":::",c)) return(c)
    paste0(unique(aux_explode(c,separator = ":::")),collapse = ", ")
  }))
  tab$ATIME <- signif(suppressWarnings(as.numeric(tab$ATIME)),signifATIME)
  tab$TIME <- signif(suppressWarnings(as.numeric(tab$TIME)),signifATIME)
  tab$GROUP[duplicated(tab$GROUP)] <- ""
  tab$DOSE[duplicated(tab$DOSE)] <- ""
  tab$PROFILE[duplicated(tab$PROFILE)] <- ""
  names(tab) <- c(
    "Sample index",
    "Profile",
    paste0("Group"),
    paste0("Dose [",d$DOSEUNIT[1],"]"),
    paste0("Actual time [",getTIMEUNITname_IQdataNCA(d),"]"),
    paste0("Nominal time [",getTIMEUNITname_IQdataNCA(d),"]"),
    paste0("Analysis time^a^ [",getTIMEUNITname_IQdataNCA(d),"]"),
    paste0("Concentration [",d$CONCUNIT[1],"]"),
    "Comment^b^",
    "Exclusion reason"
  )
  scriptName <- getScriptName_IQdataNCA()
  if (d$FLAGTIME[1]=="nominal") analysistimetext <- "Analysis time set to nominal time."
  if (d$FLAGTIME[1]=="actual") {
    if (d$FATIMIMP[1]=="nominal") {
      analysistimetext <- "Analysis time set to actual time with imputation of missing values from nominal time."
    } else {
      analysistimetext <- "Analysis time set to actual time."
    }
  }
  footertext <- paste0(
    "NV: No value collected.\n",
    "NA: Not available.\n",
    "o Value was not considered for summary and inferential procedures.\n",
    "+ Value was excluded from estimation of PK parameters.\n",
    "Values <LLOQ were reported as 'BLLOQ (<LLOQ)', where LLOQ was replaced by the actual value.\n",
    "^a^", analysistimetext, "\n",
    "^b^ Includes reporting of selected method for BLLOQ record handling in descriptive statistics (summary tables).\n",
    "BLLOQ data in linear / logarithmic y-axis plots was handled by setting these records to: '",d$FGBQPLIN[1],"' / '",d$FGBQPLOG[1],"', respectively.\n",
    "IQnca version: ", aux_version(pkgName = "IQnca"), "\n",
    "Script: ", scriptName,"\n",
    "Output: ",filename, "\n",
    "Execution date: ", Sys.time()
  )
  text <- rmdEMPTY()
  loadSetupOptions_IQnca()
  title <- .listing_concdetailed_pkconc
  title <- updateListingNumberTitle_IQdataNCA(title,listingnumber,listingindex)
  title <- paste0(title," of ",d$MATRIX[1]," ",d$COMPOUND[1], " (",d$ANALYTE[1],")")
  blockIdentifier <- paste0("indiv1_",listingindex)
  text <- text + paste0("!BLOCKSTART[keepNext](",blockIdentifier,")\n")
  text <- text + "**" + title + "**\n\n"
  tabtitle <- title
  textignore <- NULL
  if (!is.na(info$IGNOREI[1])){
    text <- text + "* !RED(Subject ignored in the analysis. Reason: " + info$IGNOREI[1] + ")\n"
    textignore <- " (Subject ignored in the analysis)"
  }
  text <- text + "* USUBJID: " + info$USUBJID[1] + "\n"
  tabtitle <- paste0(tabtitle, ": USUBJID ", info$USUBJID[1], textignore)
  if (!is.na(info$COMMENTI[1]))
    text <- text + "* Comment: " + info$COMMENTI[1] + "\n"
  if (!is.na(info$COUNTRY[1])){
    text <- text + "* Country: " + info$COUNTRY[1] + "\n"
    tabtitle <- paste0(tabtitle, "; Country ", info$COUNTRY[1])
  }
  if (!is.na(info$SITEID[1])){
    text <- text + "* Site ID: " + info$SITEID[1] + "\n"
    tabtitle <- paste0(tabtitle, "; Site ID ", info$SITEID[1])
  }
  if (!is.na(info$AGE[1])){
    text <- text + "* Age: " + info$AGE[1] + "\n"
    tabtitle <- paste0(tabtitle, "; Age ", info$AGE[1])
  }
  if (!is.na(info$SEX[1])){
    text <- text + "* Gender: " + info$SEX[1] + "\n"
    tabtitle <- paste0(tabtitle, "; Gender ", info$SEX[1])
  }
  if (!is.na(info$RACE[1])){
    text <- text + "* Race: " + info$RACE[1] + "\n"
    tabtitle <- paste0(tabtitle, "; Race ", info$RACE[1])
  }
  text <- text + rmdTABLEDF(df = tab,ignoreCaption = TRUE,fontsize = fontsizetable,footertext = footertext)
  text <- text + paste0("\n!BLOCKEND(",blockIdentifier,")\n")
  table <- IQRoutputTable(tab, xtitle = tabtitle, xfooter = footertext)
  if (!is.null(filename)) {
    return(text)
  } else {
    return(table)
  }
}
#' Generate a listing of individual pharmacokinetic concentration sampling times
#'
#' The listing will be generated already as an IQRrmd object, allowing easy reporting in Word with IQReport.
#' It is a simple listing - just showing ATIME and NTIME by PROFILE and GROUP.
#' Important: Ignored records (INGORER) are not reported in these listings! Ignored subjects (IGNOREI) are reported
#' but it is indicated that they are ignored (not in the simple version).
#'
#' @param data IQdataNCA object
#' @param listingnumber Character string with listing number information, added to the title for each table, if defined
#' @param fontsizetable Fontsize to be used in the table
#' @param filename Filename to export the listing IQRrmd object to
#' @param signifATIME Significant digits for actual time
#' @param maxCol Number of nominal times to be put into the same table in case of simple=TRUE. Tables might become to wide even for landscape
#' so a plit up of columns in several tables can be enforced in this manner.
#' @return An IQRrmd object exported to a file
#' @export
#' @family NCA Data Listings
listing_time_IQdataNCA <- function (data,listingnumber=NULL,fontsizetable=8,filename="listing_time",signifATIME=5,maxCol=10) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  if (maxCol<4) maxCol <- 4
  message("Generating PK sampling time listings ...")
  data <- removeFrom_IQdataNCA(data = data,
                               FLAGremoveIGNOREI = FALSE,
                               FLAGremoveIGNORER = TRUE,
                               FLAGremoveIGNORSUM = FALSE,
                               FLAGremoveIGNORNCA = FALSE)
  data <- data[!is.na(data$NTIME),]
  if (!is.null(filename)) {
    filename <- paste0(gsub(".rmd","",filename),".rmd")
  }
  text <- rmdEMPTY() +
    rmdLANDSCAPE() +
    rmdSECTION(title = updateListingNumberTitle_IQdataNCA(.listing_actualtime_pkconc,listingnumber,NULL),
               level = 1,numbered = FALSE)
  table <- list()
  data <- dplyr::arrange(data, GROUPN, PROFILE)
  data$SPLIT <- paste(data$GROUP, data$PROFILE)
  data$SPLIT <- factor(data$SPLIT, levels = unique(data$SPLIT))
  dS <- split(data, data$SPLIT)
  for (k in seq_along(dS)) {
    d = dS[[k]]
    object <- listingindivPKsamplingtimesSimple_IQdataNCA(d = d,listingindex = k,listingnumber = listingnumber,fontsizetable = fontsizetable,filename = filename, signifATIME=signifATIME,maxCol = maxCol)
    if ("IQRrmd" %in% class(object)) {
      textindiv <- object
      if (k==1) {
        text <- text + textindiv + "\n"
      } else {
        text <- text + rmdNEWPAGE() + "\n" + textindiv + "\n"
      }
    }
    if ("IQRoutputTable" %in% class(object)) {
      table <- c(table, structure(list(object), names = as.character(d$SPLIT[1])))
    }
  }
  if (!is.null(filename)) {
    export_IQRrmd(text,filename)
  } else {
    return(table)
  }
}
#' Generate a listing of individual pharmacokinetic concentration sampling times
#'
#' The listing will be generated already as an IQRrmd object, allowing easy reporting in Word with IQReport.
#' The listing is done per USUBJID and contains more information than the listing_time_IQdataNCA result.
#' Important: Ignored records (INGORER) are not reported in these listings! Ignored subjects (IGNOREI) are reported
#' but it is indicated that they are ignored (not in the simple version).
#'
#' @param data IQdataNCA object
#' @param listingnumber Character string with listing number information, added to the title for each table, if defined
#' @param fontsizetable Fontsize to be used in the table
#' @param filename Filename to export the listing IQRrmd object to
#' @param signifATIME Significant digits for actual time
#' @return An IQRrmd object exported to a file
#' @export
#' @family NCA Data Listings
listing_timedetailed_IQdataNCA <- function (data,listingnumber=NULL,fontsizetable=8,filename="listing_timedetailed",signifATIME=5) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  message("Generating PK sampling time listings ...")
  data <- removeFrom_IQdataNCA(data = data,
                               FLAGremoveIGNOREI = FALSE,
                               FLAGremoveIGNORER = TRUE,
                               FLAGremoveIGNORSUM = FALSE,
                               FLAGremoveIGNORNCA = FALSE)
  data <- data[!is.na(data$NTIME),]
  if (!is.null(filename)) {
    filename <- paste0(gsub(".rmd","",filename),".rmd")
  }
  text <- rmdEMPTY() +
    rmdLANDSCAPE() +
    rmdSECTION(title = updateListingNumberTitle_IQdataNCA(.listing_samplingtimes_pkconc,listingnumber,NULL),
               level = 1,numbered = FALSE)
  table <- list()
  allUSUBJID <- unique(data$USUBJID)
  for (k in seq_along(allUSUBJID)) {
    d <- data[data$USUBJID==allUSUBJID[k],]
    object <- listingindivPKsamplingtimes_IQdataNCA(d = d,listingindex = k,listingnumber = listingnumber,fontsizetable = fontsizetable,filename = filename, signifATIME=signifATIME)
    if ("IQRrmd" %in% class(object)) {
      textindiv <- object
      if (k==1) {
        text <- text + textindiv + "\n"
      } else {
        text <- text + rmdNEWPAGE() + "\n" + textindiv + "\n"
      }
    }
    if ("IQRoutputTable" %in% class(object)) {
      table <- c(table, structure(list(object), names = allUSUBJID[k]))
    }
  }
  if (!is.null(filename)) {
    export_IQRrmd(text,filename)
  } else {
    return(table)
  }
}
listingindivPKsamplingtimesSimple_IQdataNCA <- function (d,listingindex,listingnumber=NULL,fontsizetable=8,filename="unknown",signifATIME=4,maxCol=8) {
  x <- d[,c("GROUP","USUBJID","NTIME","ATIME")]
  x$ATIME <- as.character(signif(x$ATIME,signifATIME))
  if (!is.null(filename)) {
    pieces <- aux_splitVectorEqualPieces(x = unique(x$NTIME),maxCol)
  } else {
    pieces <- aux_splitVectorEqualPieces(x = unique(x$NTIME),Inf)
  }
  text <- rmdEMPTY()
  for (kpiece in seq_along(pieces)) {
    NT <- pieces[[kpiece]]
    NT <- NT[!is.na(NT)]
    y <- tryCatch(
      tidyr::spread(data = x[x$NTIME %in% NT,],key=NTIME,value=ATIME),
      error = function (err) { NULL }
    )
    if (is.null(y)) {
      stopIQR("Check following subjects. Nominal time (NTIME) might not be defined accurately.\n   ",
              paste(unique(d$USUBJID),collapse = ", "),"\nMost likely reason: NTIME of trough value set as predose for following dose.")
    }
    zz <- NULL
    if (ncol(y) >= 4) {
      z <- y[,c(4:ncol(y)),drop=FALSE]
      zz <- rbind(names(z),rep("",ncol(z)),z)
    }
    if (!is.null(zz)) {
      tab <- data.frame(
        TREATMENT = c("","**Treatment**",y$GROUP),
        SUBJECT = c("","**Subject**",y$USUBJID),
        NTIME1 = c(names(y)[[3]],paste0("**Actual Time [", getTIMEUNITsymbol_IQdataNCA(d),"]**"),y[[3]]),
        zz,
        stringsAsFactors = FALSE
      )
    } else {
      tab <- data.frame(
        TREATMENT = c("","**Treatment**",y$GROUP),
        SUBJECT = c("","**Subject**",y$USUBJID),
        NTIME1 = c(names(y)[[3]],paste0("**Actual Time [", getTIMEUNITsymbol_IQdataNCA(d),"]**"),y[[3]]),
        stringsAsFactors = FALSE
      )
    }
    names(tab)[c(1,2)] <- ""
    names(tab)[3] <- paste0("Nominal Time [", getTIMEUNITsymbol_IQdataNCA(d),"]")
    if (ncol(tab)>=4) names(tab)[4:ncol(tab)] <- ""
    tab[duplicated(tab[,1]),1] <- ""
    for (kcol in 3:ncol(tab)) {
      ixna <- is.na(tab[,kcol])
      tab[ixna,kcol] <- "NS"
    }
    loadSetupOptions_IQnca()
    title <- .listing_actualtime_pkconc
    title <- updateListingNumberTitle_IQdataNCA(title,listingnumber,listingindex)
    title <- paste0(title," of ",d$COMPOUND[1], " - ",d$PROFILE[1])
    if (length(pieces)>1) {
      title <- paste0(title," (",kpiece," of ",length(pieces),")")
    }
    if (kpiece > 1 & length(pieces)>1) text <- text + rmdNEWPAGE() + "\n"
    scriptName <- getScriptName_IQdataNCA()
    footertext <- paste0(
      "NS: No Sample.\n",
      "IQnca version: ", aux_version(pkgName = "IQnca"), "\n",
      "Script: ", scriptName,"\n",
      "Output: ",filename, "\n",
      "Execution date: ", Sys.time()
    )
    blockIdentifier <- paste0("indiv3_",listingindex)
    text <- text + paste0("!BLOCKSTART[keepNext](",blockIdentifier,")\n")
    text <- text + "**" + title + "**" +
      rmdTABLEDF(tab,footertext = footertext,ignoreCaption = TRUE,fontsize = fontsizetable) + "\n"
    text <- text + paste0("\n!BLOCKEND(",blockIdentifier,")\n")
    table <- IQRoutputTable(tab, xtitle = title, xfooter = footertext)
  }
  if (!is.null(filename)) {
    return(text)
  } else {
    return(table)
  }
}
listingindivPKsamplingtimes_IQdataNCA <- function (d,listingindex,listingnumber=NULL,fontsizetable=8,filename="unknown",signifATIME=signifATIME) {
  info <- d[is.na(d$IGNORER),]
  addcovs <- getaddcovcolumns_IQdataNCA(info)
  info <- as.data.frame(info)
  info$TIMEDIFF <- info$ATIME-info$NTIME
  tab <- as.data.frame(info[,c("IX","VISIT","EXSTDTC","PCDTC","PCTPT","ATIME","NTIME","TIME","TIMEDIFF","COMMENTR","IGNORSUM","IGNORNCA")])
  tab$COMMENT <- tab$COMMENTR
  tab$COMMENT[is.na(tab$COMMENT)] <- ""
  tab$COMMENTR <- NULL
  tab$TIME <- sapply(seq_along(tab$TIME), function (k) {
    out <- paste0(tab$TIME[k]," ")
    if (!is.na(tab$IGNORSUM[k])) out <- paste0(out,"o")
    if (!is.na(tab$IGNORNCA[k])) out <- paste0(out,"+")
    aux_strtrim(out)
  })
  tab$IGNORSUM[is.na(tab$IGNORSUM)] <- ""
  tab$IGNORNCA[is.na(tab$IGNORNCA)] <- ""
  tab$IGNORE <- paste0(tab$IGNORSUM,":::",tab$IGNORNCA)
  tab$IGNORE <- gsub("^:::","",tab$IGNORE)
  tab$IGNORSUM <- NULL
  tab$IGNORNCA <- NULL
  tab$COMMENT <- unname(sapply(tab$COMMENT, function (c) {
    if (!grepl(":::",c)) return(c)
    paste0(unique(aux_explode(c,separator = ":::")),collapse = ", ")
  }))
  tab$IGNORE <- unname(sapply(tab$IGNORE, function (c) {
    if (!grepl(":::",c)) return(c)
    paste0(unique(aux_explode(c,separator = ":::")),collapse = ", ")
  }))
  tab$ATIME <- signif(suppressWarnings(as.numeric(tab$ATIME)),signifATIME)
  tab$TIMEDIFF <- signif(suppressWarnings(as.numeric(tab$TIMEDIFF)),signifATIME)
  names(tab) <- c(
    "Sample index",
    "Visit",
    "Date/time dosing",
    "Date/time collection",
    "Time point",
    paste0("Actual time [",getTIMEUNITname_IQdataNCA(d),"]"),
    paste0("Nominal time [",getTIMEUNITname_IQdataNCA(d),"]"),
    paste0("Analysis time^a^ [",getTIMEUNITname_IQdataNCA(d),"]"),
    paste0("Diff^b^ [",getTIMEUNITname_IQdataNCA(d),"]"),
    "Comment",
    "Exclusion reason"
  )
  scriptName <- getScriptName_IQdataNCA()
  if (d$FLAGTIME[1]=="nominal") analysistimetext <- "Analysis time set to nominal time."
  if (d$FLAGTIME[1]=="actual") {
    if (d$FATIMIMP[1]=="nominal") {
      analysistimetext <- "Analysis time set to actual time with imputation of missing values from nominal time."
    } else {
      analysistimetext <- "Analysis time set to actual time."
    }
  }
  footertext <- paste0(
    "NA: Not available.\n",
    "o Corresponding concentration value was not considered for summary and inferential procedures (reason documented in the individual concentration listings).\n",
    "+ Corresponding concentration value was excluded from estimation of PK parameters (reason documented in the individual concentration listings).\n",
    "^a^ ", analysistimetext, "\n",
    "^b^ Difference between actual and nominal time.\n",
    signifATIME, " significant digits for actual time and difference between actual and nominal time.\n",
    "IQnca version: ", aux_version(pkgName = "IQnca"), "\n",
    "Script: ", scriptName,"\n",
    "Output: ",filename, "\n",
    "Execution date: ", Sys.time()
  )
  tab$Comment <- NULL
  tab$`Exclusion reason` <- NULL
  text <- rmdEMPTY()
  loadSetupOptions_IQnca()
  title <- .listing_samplingtimes_pkconc
  title <- updateListingNumberTitle_IQdataNCA(title,listingnumber,listingindex)
  title <- paste0(title," of ",d$COMPOUND[1], " - ",d$PROFILE[1])
  blockIdentifier <- paste0("indiv4_",listingindex)
  text <- text + paste0("!BLOCKSTART[keepNext](",blockIdentifier,")\n")
  text <- text + "**" + title + "**\n\n"
  tabtitle <- title
  textignore <- NULL
  if (!is.na(info$IGNOREI[1])){
    text <- text + "* !RED(Subject ignored in the analysis. Reason: " + info$IGNOREI[1] + ")\n"
    textignore <- " (Subject ignored in the analysis)"
  }
  text <- text + "* USUBJID: " + info$USUBJID[1] + "\n"
  tabtitle <- paste0(tabtitle, ": USUBJID ", info$USUBJID[1], textignore)
  if (!is.na(info$COMMENTI[1]))
    text <- text + "* Comment: " + info$COMMENTI[1] + "\n"
  text <- text + "* Profile: " + d$PROFILE[1] + ", Group: " + d$GROUP[1] + "\n"
  tabtitle <- paste0(tabtitle, "; Profile ", info$PROFILE[1], "; Group ", d$GROUP[1])
  text <- text + rmdTABLEDF(df = tab,ignoreCaption = TRUE,fontsize = fontsizetable,footertext = footertext)
  text <- text + paste0("\n!BLOCKEND(",blockIdentifier,")\n")
  table <- IQRoutputTable(tab, xtitle = tabtitle, xfooter = footertext)
  if (!is.null(filename)) {
    return(text)
  } else {
    return(table)
  }
}
#' Convert typical PCORRES column in PC domain to AVALUE col in IQdataNCA
#'
#' Assumption: PCORRES uses "BLQ" for BLLOQ values and all other alphanumerical entries point to unavailable measurements
#' @param PCORRES Character vector with "BLQ" for BLLOQ data, other alphanumerical entries point to unavailable measurements, and
#'   values for actual measurements
#' @return Numeric vector
#' @export
pcorres2aconc_IQdataNCA <- function (PCORRES) {
  out <- PCORRES
  out[out=="BLQ"] <- 0
  suppressWarnings(as.numeric(out))
}
#' Get information on the type of columns and their names for an IQdataNCA object
#'
#' Derived directly from the Excel data spec ... this is more flexible!
#' @return List with named entries. Each entry a vector with column names belonging to the "name" type of columns
#' @export
getColumnNames_IQdataNCA <- function () {
  spec <- getdataspec_IQdataNCA()
  coltypes <- unique(spec$Column.Type)
  out <- lapply(coltypes, function (ct) spec$Column[spec$Column.Type==ct])
  names(out) <- coltypes
  out
}
getColumnNames_IQparamNCA <- function (obj = NULL) {
  spec <- getparamspec_IQdataNCA(obj)
  spec$PKPARAMCD
}
#' Show NCA data specification and display as a table
#'
#' Derived directly from the Excel data spec ... this is more flexible!
#' @export
#' @family NCA Data
dataSpec_IQdataNCA <- function () {
  spec <- getdataspec_IQdataNCA()
  spec$Description <- NULL
  spec$Comment <- NULL
  spec$Related.IQR.Tools.Function <- NULL
  cat(text_IQRoutputTable(IQRoutputTable(spec),report = FALSE))
  cat("\n\nColumns not listed here might be considered additional covariates but can also provide other information")
}
getdataspecPath_IQdataNCA <- function () {
  loadSetupOptions_IQnca()
  if (is.null(.dataSpecPath)) {
    dataSpecPath <- system.file(package = "IQnca","IQdataNCA_Format_Specification.xlsx")
  } else {
    if (!file.exists(.dataSpecPath)) stopIQR("The user defined data format specification file is unavailable")
    dataSpecPath <- .dataSpecPath
  }
  dataSpecPath
}
getparamspecPath_IQdataNCA <- function () {
  loadSetupOptions_IQnca()
  if (is.null(.paramSpecPath)) {
    paramSpecPath <- system.file(package = "IQnca","IQparamNCA_Specification.xlsx")
  } else {
    if (!file.exists(.paramSpecPath)) stopIQR("The user defined data format specification file is unavailable")
    paramSpecPath <- .paramSpecPath
  }
  paramSpecPath
}
getdataspec_IQdataNCA <- function () {
  data.frame(readxl::read_excel(paste0(getdataspecPath_IQdataNCA())),stringsAsFactors = FALSE)
}
getparamspec_IQdataNCA <- function (obj = NULL) {
  paramspec <- NULL
  if (!is.null(obj)) {
    paramspec <- attr(obj, "paramspec")
    if (is.null(paramspec)) warning("Object provided to function getparamspec_IQRdataNCA did not contain a parameter specification. Loading the default specification ...")
  }
  if (is.null(obj) | is.null(paramspec)) {
    paramspec <- data.frame(readxl::read_excel(paste0(getparamspecPath_IQdataNCA()),skip = 2),stringsAsFactors = FALSE)
  }
  return(paramspec)
}
#' Order an IQdataNCA objects columns in standard way
#'
#' Works also when not all columns are present yet. Same order used as defined based on the list and entries
#' returned by function getColumnNames_IQdataNCA(). Columns that are present in the data.frame but not
#' named in getColumnNames_IQdataNCA() will be added at the end in any order
#' @param data data.frame for which the columns should be ordered
#' @return data.frame with order of columns adjusted
#' @export
ordercol_IQdataNCA <- function (data) {
  order <- unique(unname(unlist(getColumnNames_IQdataNCA())))
  cols_not_in_order <- setdiff(names(data),order)
  cols_order <- intersect(order,names(data))
  data[,c(cols_order,cols_not_in_order)]
}
#' Check IQdataNCA
#'
#' General checks for consistency
#' More will be added over time!
#' @param data IQdataNCA object
#' @return Error if something is wrong
#' @export
#' @family NCA Data
check_IQdataNCA <- function (data) {
  if (!is_IQdataNCA(data)) stop("Data is not an IQdataNCA object")
  errors <- c()
  if (any(is.na(data$TIME)))
    errors <- c(errors,"TIME column contains NA values")
  if (any(is.na(data$NTIME)))
    errors <- c(errors,"NTIME column contains NA values - at least the nominal time of sampling should be known?")
  if (!all(is.na(data$ATIME))) {
    ids <- unique(data$USUBJID[!is.na(data$NTIME) & data$NTIME==0 & data$ATIME > 0])
    if (length(ids)>0)
      errors <- c(errors,
                  paste0("NTIME set to 0 when ATIME > 0 (possible wrong predose definition) in subjects:\n",
                         paste0("    ", paste0(ids,collapse=", ")))
      )
  }
  test <- unique(as.data.frame(data)[,c("USUBJID", "GROUP")])
  ngrp_per_id <- dplyr::count(test, USUBJID)
  if (any(ngrp_per_id$n > 1)) {
    errors <- c(errors, paste0("Multiple groups assigned to subkects:\n   ", paste0(ngrp_per_id$USUBJID[ngrp_per_id$n>1], collapse = ", ")))
  }
  if (any(duplicated(dplyr::select(as.data.frame(data), USUBJID, TIME, PROFILE)))) {
    dubs <- unique(data[duplicated(dplyr::select(as.data.frame(data), USUBJID, TIME, PROFILE)), c("USUBJID", "PROFILE")])
    errors <- c(errors, paste0("Duplicated records exist for subjects:\n    ", paste0(dubs$USUBJID, " (", dubs$PROFILE, ")", collapse = ", ")))
  }
  if (length(errors)!=0) {
    errMessage <- paste0(paste0("  - ",errors),collapse = "\n\n")
    errMessage <- paste0("\n=====================================================================\n",
                         "The following problems have been found in the data:\n",
                         "=====================================================================\n",
                         errMessage,"\n")
    warningIQR(errMessage)
  }
}
#' Create an IQdataNCA object
#'
#' Generates an IQdataNCA object. The minimum required columns (see getColumnNames_IQdataNCA) need to be present.
#' Initial slope calculation will be conducted as well, based on the bestslope algorithm. This can later be refined
#' but is useful to get a quick understanding of the data.
#'
#' @param data data.frame with initial definitions
#' @param COMPTYPE Type of compound: "exogenous" or "endogenous"
#' @param FLAGTIME “actual”: actual time used in NCA PK parameter determination, “nominal”: nominal time used in NCA PK parameter determination.
#' Default: "actual" It will be checked if ATIME is defined. If not, then "nominal" will be used.
#' @param FATIMIMP Used in case if basis for analysis is actual time but some actual time point is missing.
#' In this case the user can decide to impute this time point if the nominal time is available.
#' “asis”  or “nominal”. “asis”: if missing then it will not be imputed and not be considered in the analysis.
#' If “nominal” then it will be imputed based on NTIME, NTAFD, NDUR and impact TIME, TAFD, and DUR. Default: "asis"
#' @param FLGBLQPR Handling BLLOQ values before first observation above LLOQ. Options: "asis", "0", " LLOQ/2", "LLOQ", "missing".
#' @param FLGBLQIN Handling BLLOQ values between observations above LLOQ. Options: "asis", "0", " LLOQ/2", " LLOQ ", "missing".
#' @param FLGBLQP1 Handling first BLLOQ value after last observation above LLOQ. Options: "asis", "0", " LLOQ/2", " LLOQ ", "missing".
#' @param FLGBLQPO Handling BLLOQ values after first BLLOQ post last observation above LLOQ. Options: "asis", "0", " LLOQ/2", " LLOQ ", "missing".
#' @param FGBQPLIN Handling BLLOQ values in plots on linear Y axis "asCONC", asis", "0", " LLOQ/2", " LLOQ ", "missing".
#' @param FGBQPLOG Handling BLLOQ values in plots on log Y axis "asCONC", "asis", "0", " LLOQ/2", " LLOQ ", "missing".
#' @param AUCMETHD Defines AUC calculation method. "Linear Log", "LinearUp LogDown", "Linear LinearInterpolation", "Linear LinearLogInterpolation"
#' @param AUCINVAL Character string defining one or more AUC intervals to be calculated. Example: AUCINVAL <- "\[0;24\];\[0;48\]". Note that
#' separators are ";" to allow to store that information also in a CSV file!
#' @param SLOPETOL Tolerance for bestslope algorithm
#' @param FLAGoverwrite If TRUE then derived columns will be regenerated even if already provided in data (safest).
#'   If FALSE then the information in the provided columns will be kept as-is (requires to know exectly what to do).
#' @param FLAGignore If TRUE then default rules for exclusion of records from summary tables and NCA parameter calculation
#'   are used when applicable (e.g. pre-dose samples >LLOQ). If FALSE then all samples by default are passed through as
#'   available in the input data. The first setting is for real use. The second is mainly to allow comparison with results from other
#'   NCA tools.
#' @return IQdataNCA object
#' @export
#' @family NCA Data
IQdataNCA <- function (data,
                       COMPTYPE = "exogenous",
                       FLAGTIME = "actual",
                       FATIMIMP = "asis",
                       FLGBLQPR = "0",
                       FLGBLQIN = "missing",
                       FLGBLQP1 = "LLOQ/2",
                       FLGBLQPO = "missing",
                       FGBQPLIN = "asCONC",
                       FGBQPLOG = "asCONC",
                       AUCMETHD = "Linear Log",
                       AUCINVAL = NA,
                       SLOPETOL = 1e-4,
                       FLAGoverwrite=TRUE,
                       FLAGignore = TRUE) {
  COMPTYPE <- tolower(COMPTYPE)
  FLAGTIME <- tolower(FLAGTIME)
  FATIMIMP <- tolower(FATIMIMP)
  FLGBLQPR <- tolower(FLGBLQPR)
  FLGBLQIN <- tolower(FLGBLQIN)
  FLGBLQP1 <- tolower(FLGBLQP1)
  FLGBLQPO <- tolower(FLGBLQPO)
  FGBQPLIN <- tolower(FGBQPLIN)
  FGBQPLOG <- tolower(FGBQPLOG)
  AUCMETHD <- tolower(AUCMETHD)
  if (!COMPTYPE %in% c("exogenous","endogenous")) stopIQR("COMPTYPE needs to be either 'exogenous' or 'endogenous'")
  if (!FLAGTIME %in% c("actual","nominal")) stopIQR("FLAGTIME needs to be either 'actual' or 'nominal'")
  if (!FATIMIMP %in% c("asis","nominal")) stopIQR("FLAGTIME needs to be either 'asis' or 'nominal'")
  if (!FLGBLQPR %in% c("asis","0","lloq/2","lloq","missing")) stopIQR("FLGBLQPR needs to be either 'asis', '0', 'LLOQ/2', 'LLOQ', or 'missing'")
  if (!FLGBLQIN %in% c("asis","0","lloq/2","lloq","missing")) stopIQR("FLGBLQIN needs to be either 'asis', '0', 'LLOQ/2', 'LLOQ', or 'missing'")
  if (!FLGBLQP1 %in% c("asis","0","lloq/2","lloq","missing")) stopIQR("FLGBLQP1 needs to be either 'asis', '0', 'LLOQ/2', 'LLOQ', or 'missing'")
  if (!FLGBLQPO %in% c("asis","0","lloq/2","lloq","missing")) stopIQR("FLGBLQPO needs to be either 'asis', '0', 'LLOQ/2', 'LLOQ', or 'missing'")
  if (!FGBQPLIN %in% c("asconc","asis","0","lloq/2","lloq","missing")) stopIQR("FGBQPLIN needs to be either 'asCONC', 'asis', '0', 'LLOQ/2', 'LLOQ', or 'missing'")
  if (!FGBQPLOG %in% c("asconc","asis","0","lloq/2","lloq","missing")) stopIQR("FGBQPLOG needs to be either 'asCONC', 'asis', '0', 'LLOQ/2', 'LLOQ', or 'missing'")
  if (!AUCMETHD %in% c("linear log", "linearup logdown", "linear linearinterpolation", "linear linearloginterpolation")) stopIQR("AUCMETHD needs to be either 'linear log', 'linearup logdown', 'linear linearinterpolation', or 'linear linearloginterpolation'")
  missing <- setdiff(getColumnNames_IQdataNCA()$requiredmin,names(data))
  if (length(missing)>0) stopIQR(paste0("Following required columns are missing in data: \n       ",paste0(missing,collapse = ", ")))
  data$ADM <- toupper(data$ADM)
  data$PROFTYPE <- toupper(data$PROFTYPE)
  data$TIMEUNIT <- toupper(data$TIMEUNIT)
  if (!all(data$ADM %in% c("BOLUS","EXTRAVASCULAR","INFUSION"))) stopIQR("ADM needs to be either 'Bolus', 'Extravascular', or 'Infusion'")
  if (!all(data$PROFTYPE %in% c("SD","FD","SS"))) stopIQR("PROFTYPE needs to be either 'SD', 'FD', or 'SS'")
  if (!all(data$TIMEUNIT %in% c("SECONDS","MINUTES","HOURS","DAYS","WEEKS"))) stopIQR("PROFTYPE needs to be either 'Seconds', 'Minutes', 'Hours', 'Days', or 'Weeks'")
  if ("COMPTYPE" %in% names(data)) {
    if (any(is.na(data$COMPTYPE))) stopIQR("COMPTYPE column of input dataset contains NAs.")
    comptype_data <- unique(data$COMPTYPE)
    if (length(comptype_data) > 1) stopIQR("COMPTYPE column of input dataset is not unique.")
    if (!comptype_data %in% c("exogenous","endogenous")) stopIQR("COMPTYPE (of input dataset) needs to be either 'exogenous' or 'endogenous'")
  }
  if (any(data$ADM=="INFUSION")) {
    if (!"ADUR" %in% names(data)) stopIQR("Infusion data present but ADUR not defined")
    if (!"NDUR" %in% names(data)) stopIQR("Infusion data present but NDUR not defined")
  }
  if (any(data$PROFTYPE %in% c("SS","FD"))) {
    if (!"TAU" %in% names(data)) stopIQR("Steady-state (SS) or first-dose (FD) data present but TAU not defined")
  }
  data <- convertNumCharTypeSpec_IQdataNCA(data)
  data <- addconditionalrequiredcolumns_IQdataNCA(data = data)
  data <- addoptionalcolumns_IQdataNCA(data = data)
  data <- addcovariatecolumns_IQdataNCA(data = data)
  data <- addflagcomment_IQdataNCA(data = data,COMPTYPE=COMPTYPE)
  data <-addtimecolumns_IQdataNCA(data=data,FLAGTIME=FLAGTIME,FATIMIMP=FATIMIMP,FLAGoverwrite = FLAGoverwrite)
  message("Sorting rows by STUDYID, PROFILE, USUBJID, TIME")
  data <- dplyr::arrange(data,STUDYID,PROFILE,USUBJID,TIME)
  data <- addix_IQdataNCA(data)
  data <- addblloqcolumns_IQdataNCA(data=data,FLGBLQPR=FLGBLQPR,FLGBLQIN=FLGBLQIN,FLGBLQP1=FLGBLQP1,FLGBLQPO=FLGBLQPO,FGBQPLIN=FGBQPLIN,FGBQPLOG=FGBQPLOG)
  data <- addconccolumns_IQdataNCA(data)
  if (FLAGignore) data <- ignoreSUMdefault_IQdataNCA(data)
  if (FLAGignore) data <- ignoreNCAdefault_IQdataNCA(data)
  data$SLOPETOL <- SLOPETOL
  data <- addslope_IQdataNCA(data)
  data$AUCMETHD <- AUCMETHD
  message("Adding AUCMETHD column with: ",AUCMETHD)
  data <- ordercol_IQdataNCA(data)
  class(data) <- c("IQdataNCA",class(data))
  rownames(data) <- NULL
  data$PROFTYPE <- toupper(data$PROFTYPE)
  data$ADM      <- toupper(data$ADM)
  data$COMPTYPE <- tolower(data$COMPTYPE)
  data$AUCINVAL <- AUCINVAL
  message("Adding AUCINVAL column with: ",AUCINVAL)
  paramspec <- getparamspec_IQdataNCA(obj = NULL)
  attr(data, "paramspec") <- paramspec
  check_IQdataNCA(data)
  data
}
updateslope_IQdataNCA <- function (data) {
  dS1 <- split(data,data$USUBJID)
  do.call(rbind,lapply(seq_along(dS1), function (k1) {
    d1 <- dS1[[k1]]
    dS2 <- split(d1,d1$PROFILE)
    do.call(rbind,lapply(seq_along(dS2), function (k2) {
      d2 <- dS2[[k2]]
      if (d2$FLGSLOPE[1]=="bestslope") {
        out <- bestslopeindiv_IQdataNCA(d = d2)
      } else {
        IXslope = d2$IX[d2$SLOPEPT==1 & is.na(d2$IGNORER) & is.na(d2$IGNORNCA)]
        if (sum(d2$SLOPEPT) != length(IXslope)) {
          message("Ignoring of record(s) led to an update in the slope calculation points for subject '",d2$USUBJID[1],"'")
        }
        out <- getslopeindiv_IQdataNCA(d = d2,IXslope = IXslope)
      }
      out
    }))
  }))
}
#' Define slope points in subjects to be used for manual slope calculation
#'
#' Helper function to construct the manualslope input argument for slope_IQdataNCA()
#'
#' @param USUBJID Single USUBJID
#' @param PROFILE Name of the profile (PROFILE column)
#' @param IXslope Numeric vector with slope points matching the IX column entries in the IQdataNCA object
#' @return Just a list  - can be used as input argument 'manualslope' to slope_IQdataNCA()
#' @export
#' @family NCA Slope
slope_manual_NCA <- function (USUBJID,PROFILE=NULL,IXslope) {
  list(USUBJID=USUBJID,PROFILE=PROFILE,IXslope=IXslope)
}
#' Slope calculation function for IQdataNCA objects
#'
#' Calculates the slope information based on the data. By default the bestslope
#' algorithm is used but for indidivual subjects also the slopepoints can be defined that
#' should be used.
#'
#' @param data IQdataNCA object
#' @param manualslope List of entries. One entry per subject/profile combination, allowing to define the
#'  slope points. A list entry in this list can be generated using the function slope_manual_NCA().
#' @return Updated IQdataNCA object
#' @export
#' @family NCA Slope
slope_IQdataNCA <- function (data,manualslope=NULL) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  if (!is.null(manualslope)) {
    if (!is.list(manualslope)) stopIQR("manualslope argument is not a list")
    dummy <- sapply(manualslope, function (x) {
      if (!"USUBJID" %in% names(x)) stopIQR("USUBJID not present in all elements of manualslope")
      if (!"IXslope" %in% names(x)) stopIQR("IXslope not present in all elements of manualslope")
      if (length(x$USUBJID)!=1) stopIQR("USUBJID in manualslope in at least one case has more than two elements")
    })
  }
  data$FLGSLOPE <- "bestslope"
  for (k in seq_along(manualslope)) {
    def <- manualslope[[k]]
    USUBJID <- def$USUBJID
    PROFILE <- def$PROFILE
    IXslope <- def$IXslope
    if (!USUBJID %in% data$USUBJID) stopIQR("Subject '",USUBJID,"' is not in the data")
    if (!is.null(PROFILE)) {
      if (!PROFILE %in% data$PROFILE) stopIQR("Profile '",PROFILE,"' is not in the data")
      if (!USUBJID %in% data$USUBJID[data$PROFILE %in% PROFILE]) stopIQR("Subject '",USUBJID,"' is not in available in profile ",PROFILE," the data")
      if (!all(IXslope %in% data$IX[data$USUBJID %in% USUBJID & data$PROFILE %in% PROFILE])) stopIQR("Not all IXslope elements present in subject '",USUBJID,"' in profile ",PROFILE)
    } else {
      if (length(unique(data$PROFILE[data$USUBJID %in% USUBJID])) != 1) stopIQR("Profile '",PROFILE,"' is not unique for subject ",USUBJID," in the data")
      if (!all(IXslope %in% data$IX[data$USUBJID %in% USUBJID])) stopIQR("Not all IXslope elements present in subject '",USUBJID)
    }
    if (!is.null(PROFILE)) {
      index_update <- data$USUBJID==USUBJID & data$PROFILE==PROFILE
    } else {
      index_update <- data$USUBJID==USUBJID
    }
    data$FLGSLOPE[index_update] <- "manual"
    data$SLOPEPT[index_update] <- 0
    data$SLOPEPT[index_update & data$IX %in% IXslope] <- 1
  }
  data <- updateslope_IQdataNCA(data)
  data
}
addslope_IQdataNCA <- function (data) {
  dS1 <- split(data,data$USUBJID)
  data <- do.call(rbind,lapply(seq_along(dS1), function (k1) {
    d1 <- dS1[[k1]]
    dS2 <- split(d1,d1$PROFILE)
    do.call(rbind,lapply(seq_along(dS2), function (k2) {
      d2 <- dS2[[k2]]
      bestslopeindiv_IQdataNCA(d = d2)
    }))
  }))
  data
}
#' Calculation of R2 and R2ADJ for slope of profile
#'
#' This function calculates the R2 (R-squared) and R2ADJ (R2-squared adjusted) metrics
#' for the slope fitting. User provided indices for PK samples to be considered will be used
#' if provided. If not provided, the bestSlope algorithm will be used. The result is presented
#' in a graphical manner.
#'
#' The bestslope algorithm considers points from CMAX/TMAX until the last >0 point.
#' For extravascular one point later is used as start point. For INFUSION when >= 5
#' samples available also one point later is started.
#' The algorithm simply determines metrics from all points to the last point with minimally
#' 3 points in the calculation. The resulting best slope is selected as the solution with
#' the maximum number of used concentration points for slope calculation for which the
#' R2ADJ is not smaller than SLOPETOL (in the data) as compared to the maximum R2ADJ.
#'
#' @param data IQdataNCA object
#' @param USUBJID Unique subject ID of subject to do the slope calculation for
#' @param PROFILE Name of the profile. Only needs to be provided when USUBJID appears in more than
#' one profile in the data
#' @param IXslope Vector with manually selected numeric IX values in the data to use for slope calculation
#'   or set to NULL the bestSlope algorithm will be used
#' @param logY Log y axis is TRUE, otherwise linear Y axis
#' @param R2ADJTHRESHOL Threshold for R2ADJ to switch color and annotation
#' @export
#' @family NCA Slope
slopetest_IQnca <- function (data,USUBJID,PROFILE=NULL,IXslope=NULL,logY=TRUE,R2ADJTHRESHOL=0.85) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  if (!USUBJID %in% data$USUBJID) stopIQR("USUBJID not in data")
  if (!is.null(PROFILE)) {
    if (!PROFILE %in% data$PROFILE) stopIQR("PROFILE not in data")
    if (!USUBJID %in% data$USUBJID[data$PROFILE %in% PROFILE]) stopIQR("PROFILE not in data")
  }
  if (is.null(PROFILE)) {
    d <- data[data$USUBJID==USUBJID,]
  } else {
    d <- data[data$USUBJID %in% USUBJID & data$PROFILE %in% PROFILE,]
  }
  if (length(unique(d$PROFILE))>1) stopIQR("PROFILE is not unique in selected subject. Define also PROFILE argument")
  if (!is.null(IXslope)) {
    if (!all(IXslope %in% d$IX)) stopIQR("Not all IXslope points are in the subjects data")
  }
  if (is.null(IXslope)) {
    message("Using bestslope calculation")
    d <- bestslopeindiv_IQdataNCA(d=d)
  } else {
    message("Determining slope based on provided points in IXslope")
    d <- getslopeindiv_IQdataNCA(d=d,IXslope = IXslope)
  }
  plotsinglesplit_IQdataNCA(d=d,logY=logY,R2ADJTHRESHOL=R2ADJTHRESHOL)
}
bestslopeindiv_IQdataNCA <- function (d) {
  SLOPETOL <- d$SLOPETOL[1]
  if (length(unique(d$USUBJID)) != 1) stopIQR("Only one USUBJID allowed in the slope calculation")
  if (length(unique(d$PROFILE)) != 1) stopIQR("Only one PROFILE allowed in the slope calculation")
  doutdefault <- d
  doutdefault$FLGSLOPE <- "bestslope"
  doutdefault$SLOPEPT <- 0
  doutdefault$R2 <- NA
  doutdefault$R2ADJ <- NA
  doutdefault$LAMZNPT <- 0
  doutdefault$LAMZ <- NA
  doutdefault$LAMZICPT <- NA
  doutdefault$CORRXY <- NA
  doutdefault$LAMZLL <- NA
  doutdefault$LAMZUL <- NA
  doutdefault$CLSTP <- NA
  ixuse <- which((is.na(d$IGNORER) & is.na(d$IGNORNCA) & !is.na(d$CONC)))
  duse <- d[ixuse,]
  if (nrow(duse)==0) return(doutdefault)
  if (duse$ADM[1]=="INFUSION") duse <- duse[duse$TIME>duse$DUR[1],]
  if (nrow(duse)==0) return(doutdefault)
  if (length(unique(duse$CONC))==1 & sum(duse$CONC)==0) {
    message("bestslope: all eligible data points have 0 value in subject ",d$USUBJID[1],", in profile: ",d$PROFILE[1])
    dout <- doutdefault
    dout$LAMZNPT <- 0
    dout$LAMZICPT <- unique(duse$CONC)
    return(dout)
  }
  ixstart <- which(duse$CONC == max(duse$CONC))[1]
  ixend <- max(which(duse$CONC>0))
  duse <- duse[ixstart:ixend,]
  if (duse$ADM[1]=="EXTRAVASCULAR") {
    duse <- duse[-1,]
  }
  if (nrow(duse)<2) {
    message("bestslope: to few eligible data points for calculation in subject ",d$USUBJID[1],", in profile: ",d$PROFILE[1])
    return(doutdefault)
  }
  if (length(unique(duse$CONC))==1) {
    message("bestslope: all eligible data points have same value in subject ",d$USUBJID[1],", in profile: ",d$PROFILE[1])
    dout <- doutdefault
    dout$LAMZNPT <- 0
    dout$LAMZICPT <- unique(duse$CONC)
    return(dout)
  }
  IXtest <- list()
  res <- do.call(rbind,lapply(1:(nrow(duse)-1), function (k) {
    IXtest[[k]] <<- duse$IX[k:nrow(duse)]
    cbind(index=k,as.data.frame(unique(getslopeindiv_IQdataNCA(d = duse,IXslope = duse$IX[k:nrow(duse)],FLAGbestlope = TRUE)[,c("R2","R2ADJ","LAMZNPT","LAMZ","LAMZICPT","CORRXY","LAMZLL","LAMZUL","CLSTP")])))
  }))
  rownames(res) <- NULL
  res <- res[res$LAMZNPT>2,,drop=FALSE]
  res <- res[!is.na(res$R2ADJ),,drop=FALSE]
  if (nrow(res)==0) {
    return (doutdefault)
  }
  res$ELIGIBLE <- ifelse(abs(max(res$R2ADJ) - res$R2ADJ) < SLOPETOL, TRUE, FALSE)
  resselected <- res[res$LAMZNPT==max(res$LAMZNPT[res$ELIGIBLE]),]
  IXduse <- IXtest[[resselected$index]]
  dout <- d
  dout$FLGSLOPE <- "bestslope"
  dout$SLOPEPT <- 0; dout$SLOPEPT[dout$IX %in% IXduse] <- 1
  dout$R2 <- resselected$R2
  dout$R2ADJ <- resselected$R2ADJ
  dout$LAMZNPT <- resselected$LAMZNPT
  dout$LAMZ <- resselected$LAMZ
  dout$LAMZICPT <- resselected$LAMZICPT
  dout$CORRXY <- resselected$CORRXY
  dout$LAMZLL <- resselected$LAMZLL
  dout$LAMZUL <- resselected$LAMZUL
  dout$CLSTP <- resselected$CLSTP
  dout
}
getslopeindiv_IQdataNCA <- function (d,IXslope,FLAGbestlope=FALSE)
{
  if (length(unique(d$USUBJID)) != 1) stopIQR("Only one USUBJID allowed in the slope calculation")
  if (length(unique(d$PROFILE)) != 1) stopIQR("Only one PROFILE allowed in the slope calculation")
  IXslope <- unique(IXslope)
  if (!all(IXslope %in% d$IX)) stopIQR("Selected IXslope values not in data")
  if (length(IXslope) <= 1) stopIQR("Provided IXslope vector for slope points needs to have at least 2 unique IXslope values")
  duse <- d[d$IX %in% IXslope,]
  if (any(IXslope %in% d$IX[ !(is.na(d$IGNORER) & is.na(d$IGNORNCA) & !is.na(d$CONC))] ))
    stopIQR("Selection of records (IXslope) not valid for slope calculation (use only non-IGNORER, non-IGNORNCA, and no 'missing' BLLOQ records")
  x = duse$TIME
  y = log(duse$CONC)
  mx  = mean(x)
  my  = mean(y)
  Sxx = sum((x - mx)*(x - mx))
  Sxy = sum((x - mx)*(y - my))
  Syy = sum((y - my)*(y - my))
  b1  = Sxy/Sxx
  n   = length(IXslope)
  if (is.nan(b1) | b1 >= 0) {
    if (!FLAGbestlope) warningIQR("Please select valid points for slope calculation - default NA output returned")
    R2 <- NA
    R2ADJ <- NA
    LAMZNPT <- 0
    LAMZ <- NA
    LAMZICPT <- NA
    CORRXY <- NA
    LAMZLL <- NA
    LAMZUL <- NA
    CLSTP <- NA
  } else {
    LAMZNPT <- n 
    LAMZ    <- -b1 
    LAMZICPT      <- my - b1*mx 
    R2      <- b1 * Sxy/Syy 
    R2ADJ   <- 1 - (1 - R2)*(n - 1)/(n - 2) 
    CORRXY  <- sign(b1)*sqrt(R2) 
    LAMZLL  <- x[1] 
    LAMZUL  <- x[n] 
    CLSTP   <- exp(LAMZICPT + b1 * x[n]) 
  }
  d$FLGSLOPE <- "manual"
  d$SLOPEPT <- as.numeric(d$IX %in% IXslope)
  d$R2 <- R2
  d$R2ADJ <- R2ADJ
  d$LAMZNPT <- LAMZNPT
  d$LAMZ <- LAMZ
  d$LAMZICPT <- LAMZICPT
  d$CORRXY <- CORRXY
  d$LAMZLL <- LAMZLL
  d$LAMZUL <- LAMZUL
  d$CLSTP <- CLSTP
  d
}
#' Generic plot function for IQdataNCA objects
#'
#' The purpose is to allow exploratory plots of the data. The goal is not
#' to produce submission ready figures.
#'
#' Plotting single individual (USUBJID)
#' TIME / CONCPLIN or CONCPLOT
#' Adding additional meta information, including slope calculation, etc.
#'
#' @param x IQdataNCA object
#' @param ... Whatever R wants here. These dots are useless and annoying
#' @param logY Log y axis is TRUE, otherwise linear Y axis
#' @param filename Name of PDF file to generate with plots
#' @param nindiv Number of plots per page if exported to PDF
#' @param R2ADJTHRESHOL Threshold for R2ADJ to switch color and annotation
#'  to warn when R2ADJ<R2ADJTHRESHOL
#' @return A list with individual plots
#' @family NCA Data
#' @export
plot.IQdataNCA <- function(x, ..., logY=TRUE, filename=NULL,nindiv=4,R2ADJTHRESHOL=0.85) {
  x$SPLIT <- paste0(x$USUBJID,"-",x$PROFILE)
  dS <- split(x,x$SPLIT)
  allplots <- lapply(seq_along(dS), function (k) {
    plotsinglesplit_IQdataNCA(d = dS[[k]],logY = logY,R2ADJTHRESHOL=R2ADJTHRESHOL)
  })
  if (is.null(filename)) return(allplots)
  IQRoutputFigure(x = allplots,
                  opt.layout = opt.layout(legend.option = "as.is",ncol = floor(sqrt(nindiv)),nrow=ceiling(nindiv/floor(sqrt(nindiv)))),
                  opt.pagesize = opt.pagesize(scale = getScaleIQRoutputFigure_nindiv(nindiv)),
                  filename = paste0(gsub(".pdf","",filename),".pdf"))
  unlink(paste0(gsub(".pdf","",filename),".pdf.log"))
  invisible(NULL)
}
plotsinglesplit_IQdataNCA <- function (d,logY=TRUE,R2ADJTHRESHOL=0.85) {
  dorig <- d
  if (length(unique(d$USUBJID))!=1) stopIQR("USUBJID not unique")
  if (length(unique(d$PROFILE))!=1) stopIQR("PROFILE not unique")
  mytitle <- paste0("USUBJID: ", dorig$USUBJID[1],
                    "\nPROFILE: ",dorig$PROFILE[1])
  d$CONCPLOT <- d$CONC
  if (nrow(d)==0) {
    return(
      IQRggplot() + ggtitle(mytitle, subtitle = "No evaluable data for this subject")
    )
  }
  d$TIMEPLOT <- d$TIME
  d$BLLOQmethod <- "-"
  d$BLLOQmethod[d$BLLOQPR %in% 1] <- d$FLGBLQPR[d$BLLOQPR %in% 1]
  d$BLLOQmethod[d$BLLOQIN %in% 1] <- d$FLGBLQIN[d$BLLOQIN %in% 1]
  d$BLLOQmethod[d$BLLOQP1 %in% 1] <- d$FLGBLQP1[d$BLLOQP1 %in% 1]
  d$BLLOQmethod[d$BLLOQPO %in% 1] <- d$FLGBLQPO[d$BLLOQPO %in% 1]
  d$BLLOQmethod <- factor(d$BLLOQmethod,levels = c("asis","0","lloq/2","lloq","missing","-"))
  methods <-  c("asis","0",    "lloq/2", "lloq",   "missing", "-")
  colors  <-  c("red", "blue", "green",  "orange", "transparent", "white")
  colors2 <- c("transparent", "transparent", "transparent",  "transparent", "magenta", "transparent")
  d$FILLcolor <- factor(match(as.character(d$BLLOQmethod),methods),levels = 1:6,labels=methods)
  dmissing <- d[d$BLLOQmethod == "missing",]
  d <- d[!is.na(d$TIME),]
  d <- d[!is.na(d$CONCPLOT),]
  timedef <- unique(d$FLAGTIME)
  if (nrow(d)==0) {
    return(
      IQRggplot() + ggtitle(mytitle, subtitle = "All data either with missing time or concentration for this subject")
    )
  }
  p <- IQRggplot(data=d,aes(x=TIME,y=CONCPLOT)) +
    geom_point() +
    geom_line(alpha=0.7) +
    geom_label(aes(label=IX),size=3,fill="white") +
    geom_label(aes(label=IX,fill=FILLcolor),size=3,alpha=0.2) +
    scale_color_manual("BLLOQ handling PK param", values = colors2, drop=FALSE) +
    scale_fill_manual("BLLOQ handling PK param", values = colors, drop=FALSE)
  if (nrow(dmissing) > 0) {
    p  <- p +
      geom_rug(data = dmissing, aes(x=TIME, color = FILLcolor), size = 2, sides = "b")
  }
  if ( !logY | d$LLOQ[1] > 0) {
    p <- p + geom_hline(yintercept = d$LLOQ[1], linetype="dashed")
  }
  if (logY) p <- p + scale_y_log10_IQnca(labeltype = "natural")
  p <- p + theme(legend.position="bottom")
  addsubtitle <- NULL
  background <- "white"
  if ("SLOPEPT" %in% names(d)) {
    dslope <- d[d$SLOPEPT %in% 1,]
    if (nrow(dslope) > 0) {
      if (is.na(d$R2ADJ[1])) {
        p <- p + geom_line(data=dslope,color="darkred")
        p <- p + geom_label(data=dslope,aes(label=IX,fill=FILLcolor),size=3,color="darkred",alpha=0.2)
        addsubtitle <- paste0("Red: ",d$FLGSLOPE[1]," slope calculation (R2: ",signif(d$R2[1],4),", R2ADJ: ",signif(d$R2ADJ,4),")\n")
        background <- "pink1"
      } else {
        if (d$R2ADJ[1]>R2ADJTHRESHOL) {
          p <- p + geom_line(data=dslope,color="green4")
          p <- p + geom_label(data=dslope,aes(label=IX,fill=FILLcolor),size=3,color="green4",alpha=0.2)
          addsubtitle <- paste0("Green: ",d$FLGSLOPE[1]," slope calculation (R2: ",signif(d$R2[1],4),", R2ADJ: ",signif(d$R2ADJ,4),")\n")
          background <- "white"
        } else {
          p <- p + geom_line(data=dslope,color="darkred")
          p <- p + geom_label(data=dslope,aes(label=IX,fill=FILLcolor),size=3,color="darkred",alpha=0.2)
          addsubtitle <- paste0("Red: ",d$FLGSLOPE[1]," slope calculation (R2: ",signif(d$R2[1],4),", R2ADJ: ",signif(d$R2ADJ,4)," < ",R2ADJTHRESHOL,")\n")
          background <- "pink1"
        }
      }
    } else {
      addsubtitle <- "No slope can be determined\n"
      background <- "pink1"
    }
  }
  if (!is.na(d$IGNOREI[1])) mytitle <- paste0("IGNORED Subject: ",d$IGNOREI[1],"\n", mytitle)
  comment <- d$COMMENTI[1]
  if (any(grepl("Pre-first dose concentration >=5% of Cmax!",d$COMMENTR,fixed = TRUE))) {
    background <- "pink1"
    comment <- "A pre-first dose concentration >=5% of Cmax!"
  }
  if (!is.na(comment)) mytitle <- paste0("Comment: ", comment, "\n", mytitle)
  addsubtitle2 <- ""
  ylabtext <- "Nominal time post dose"
  if (d$FLAGTIME[1] == "actual") {
    addsubtitle2 <- ""
    ylabtext <- "Actual time post dose"
    if (d$FATIMIMP[1] == "nominal") {
      addsubtitle2 <- "\nMissing actual times were attempted to be imputed from nominal time"
      ylabtext <- "Actual time post dose"
    }
  }
  p <- p + ggtitle(mytitle, subtitle=
                     paste0(
                       "Dashed horizontal line: LLOQ (",d$LLOQ[1]," ",d$CONCUNIT[1],")\n",
                       addsubtitle,
                       "Orange X: ignored records\n",
                       "Orange O: ignored in summary tables\n",
                       "Orange +: ignored in NCA PK parameter calculation",
                       addsubtitle2
                     ))
  if (!is.na(d$IGNOREI[1])) {
    p <- p + theme(panel.background = element_rect(fill = "lightgrey"))
  } else {
    p <- p + theme(panel.background = element_rect(fill = background))
  }
  p <- p + geom_point(data=d[!is.na(d$IGNORER),],aes(x=TIME,y=CONCPLOT),shape=4,size=5,stroke=2,color="orangered4") +
    geom_point(data=d[!is.na(d$IGNORSUM),],aes(x=TIME,y=CONCPLOT),shape=1,size=5,stroke=2,color="orangered4") +
    geom_point(data=d[!is.na(d$IGNORNCA),],aes(x=TIME,y=CONCPLOT),shape=3,size=5,stroke=2,color="orangered4")
  p <- p + xlab(paste0(ylabtext, " [", getTIMEUNITname_IQdataNCA(d), "]"))
  p <- p + ylab(paste0("Concentration ", d$ANALYTE[1], " [", d$CONCUNIT[1], "]"))
  p <- p + scale_x_continuous(breaks=gettimebreaksplotindiv(d)$breaks,minor_breaks = gettimebreaksplotindiv(d)$minorbreaks)
  p
}
addconccolumns_IQdataNCA <- function (data) {
  message("Initialize CONC with ACONC")
  data$CONC <- data$ACONC
  BLLOQcase   <- c("BLLOQPR", "BLLOQIN", "BLLOQP1", "BLLOQPO")
  BLLOQmethod <- c("FLGBLQPR","FLGBLQIN","FLGBLQP1","FLGBLQPO")
  for (k in seq_along(BLLOQcase)) {
    caseix <- which(data[[BLLOQcase[k]]] == 1)
    method <- unique(data[[BLLOQmethod[k]]][caseix])
    if (length(method)>1) stopIQR("BLLOQ handling method per case should be unique")
    if (length(method)==1) {
      CONC <- data$CONC[caseix]
      LLOQ <- data$LLOQ[caseix]
      if (method=="0") CONC[1:length(CONC)] <- 0
      if (method=="asis") CONC <- CONC
      if (method=="lloq/2") CONC <- LLOQ/2
      if (method=="lloq") CONC <- LLOQ
      if (method=="missing") CONC <- NA
      data$CONC[caseix] <- CONC
      data$COMMENTR[caseix] <- addcomment_IQdataNCA(data$COMMENTR[caseix],addcomment = paste0("BLLOQ handling: ",method))
      message("CONC - BLLOQ case '",BLLOQcase[k], "==1' handled with: ",method)
    }
  }
  if (data$FGBQPLIN[1]=="asconc") {
    message("Initialize CONCPLIN with CONC - same BLLOQ handling as for concentration summary and NCA")
    data$CONCPLIN <- data$CONC
  } else {
    message("Initialize CONCPLIN with ACONC")
    data$CONCPLIN <- data$ACONC
    BLLOQcase   <- c("BLLOQ")
    BLLOQmethod <- c("FGBQPLIN")
    for (k in seq_along(BLLOQcase)) {
      caseix <- which(data[[BLLOQcase[k]]] == 1)
      method <- unique(data[[BLLOQmethod[k]]][caseix])
      if (length(method)!=1) stopIQR("BLLOQ handling method per case should be unique")
      CONC <- data$CONCPLIN[caseix]
      LLOQ <- data$LLOQ[caseix]
      if (method=="0") CONC[1:length(CONC)] <- 0
      if (method=="asis") CONC <- CONC
      if (method=="lloq/2") CONC <- LLOQ/2
      if (method=="lloq") CONC <- LLOQ
      if (method=="missing") CONC <- NA
      data$CONCPLIN[caseix] <- CONC
      message("CONCPLIN - BLLOQ case '",BLLOQcase[k], "==1' handled with: ",method)
    }
  }
  if (data$FGBQPLOG[1]=="asconc") {
    message("Initialize CONCPLOG with CONC - same BLLOQ handling as for concentration summary and NCA")
    data$CONCPLOG <- data$CONC
  } else {
    message("Initialize CONCPLOG with ACONC")
    data$CONCPLOG <- data$ACONC
    BLLOQcase   <- c("BLLOQ")
    BLLOQmethod <- c("FGBQPLOG")
    for (k in seq_along(BLLOQcase)) {
      caseix <- which(data[[BLLOQcase[k]]] == 1)
      method <- unique(data[[BLLOQmethod[k]]][caseix])
      if (length(method)==1) {
        CONC <- data$CONCPLOG[caseix]
        LLOQ <- data$LLOQ[caseix]
        if (method=="0") CONC[1:length(CONC)] <- 0
        if (method=="asis") CONC <- CONC
        if (method=="lloq/2") CONC <- LLOQ/2
        if (method=="lloq") CONC <- LLOQ
        if (method=="missing") CONC <- NA
        data$CONCPLOG[caseix] <- CONC
        message("CONCPLOG - BLLOQ case '",BLLOQcase[k], "==1' handled with: ",method)
      }
    }
  }
  data
}
addblloqcolumns_IQdataNCA <- function (data,FLGBLQPR,FLGBLQIN,FLGBLQP1,FLGBLQPO,FGBQPLIN,FGBQPLOG) {
  if (any(is.na(data$LLOQ))) stopIQR("The LLOQ definition is not allowed to be NA. Please enter the LLOQ in the dataset.")
  message("Set BLLOQ column based on ACONC<LLOQ. ACONC=NA => BLLOQ=NA")
  data$BLLOQ <- as.numeric(data$ACONC < data$LLOQ)
  dS <- split(data,data$USUBJID)
  data <- do.call(rbind,lapply(seq_along(dS), function (k0) {
    dP <- split(dS[[k0]],dS[[k0]]$PROFILE)
    do.call(rbind,lapply(seq_along(dP), function (k) {
      d_with_maybe_NA <- dP[[k]]
      d_with_maybe_NA$ORIGINDEX <- 1:nrow(d_with_maybe_NA)
      d <- d_with_maybe_NA[!is.na(d_with_maybe_NA$BLLOQ),]
      if (nrow(d)==0) {
        d_with_maybe_NA$BLLOQPR <- 0
        d_with_maybe_NA$BLLOQIN <- 0
        d_with_maybe_NA$BLLOQP1 <- 0
        d_with_maybe_NA$BLLOQPO <- 0
        d_with_maybe_NA$ORIGINDEX <- NULL
        return(d_with_maybe_NA)
      }
      d <- d[is.na(d$IGNORER),]
      d <- d[is.na(d$IGNORNCA),]
      BLLOQ <- d$BLLOQ
      if (BLLOQ[1] == 0) {
        ix_leading_BLLOQ <- NULL
      } else {
        ix_noBLLOQ <- which(BLLOQ==0)
        if (length(ix_noBLLOQ)==0) {
          ix_leading_BLLOQ <- 1:nrow(d)
        } else {
          ix_leading_BLLOQ <- 1:(ix_noBLLOQ[1]-1)
        }
      }
      end <- nrow(d)
      if (BLLOQ[end] == 0) {
        ix_trailing_BLLOQ <- NULL
      } else {
        ix_noBLLOQ <- which(BLLOQ==0)
        if (length(ix_noBLLOQ)==0) {
          ix_trailing_BLLOQ <- 1:nrow(d)
        } else {
          ix_trailing_BLLOQ <- (ix_noBLLOQ[length(ix_noBLLOQ)]+1):end
        }
      }
      ix_all_BLLOQ <- which(BLLOQ==1)
      ix_inbetween_BLLOQ <- setdiff(setdiff(ix_all_BLLOQ,ix_leading_BLLOQ),ix_trailing_BLLOQ)
      if (length(ix_trailing_BLLOQ) == 1) {
        ix_inbetween_BLLOQ <- c(ix_inbetween_BLLOQ, ix_trailing_BLLOQ)
        ix_trailing_BLLOQ <- NULL
      }
      if (!any(BLLOQ==0)) {
        ix_trailing_BLLOQ <- NULL
        ix_inbetween_BLLOQ <- NULL
      }
      if (length(ix_trailing_BLLOQ)==0) {
        ix_trailing_BLLOQ_1 <- NULL
        ix_trailing_BLLOQ_others <- NULL
      }
      if (length(ix_trailing_BLLOQ)==1) {
        ix_trailing_BLLOQ_1 <- ix_trailing_BLLOQ[1]
        ix_trailing_BLLOQ_others <- NULL
      }
      if (length(ix_trailing_BLLOQ)>1) {
        ix_trailing_BLLOQ_1 <- ix_trailing_BLLOQ[1]
        ix_trailing_BLLOQ_others <- ix_trailing_BLLOQ[2:length(ix_trailing_BLLOQ)]
      }
      d_with_maybe_NA$BLLOQPR <- 0
      d_with_maybe_NA$BLLOQPR[is.na(d_with_maybe_NA$BLLOQ)] <- NA
      d_with_maybe_NA$BLLOQPR[d$ORIGINDEX[ix_leading_BLLOQ]] <- 1
      d_with_maybe_NA$BLLOQIN <- 0
      d_with_maybe_NA$BLLOQIN[is.na(d_with_maybe_NA$BLLOQ)] <- NA
      d_with_maybe_NA$BLLOQIN[d$ORIGINDEX[ix_inbetween_BLLOQ]] <- 1
      d_with_maybe_NA$BLLOQP1 <- 0
      d_with_maybe_NA$BLLOQP1[is.na(d_with_maybe_NA$BLLOQ)] <- NA
      d_with_maybe_NA$BLLOQP1[d$ORIGINDEX[ix_trailing_BLLOQ_1]] <- 1
      d_with_maybe_NA$BLLOQPO <- 0
      d_with_maybe_NA$BLLOQPO[is.na(d_with_maybe_NA$BLLOQ)] <- NA
      d_with_maybe_NA$BLLOQPO[d$ORIGINDEX[ix_trailing_BLLOQ_others]] <- 1
      d_with_maybe_NA$ORIGINDEX <- NULL
      d_with_maybe_NA
    }))
  }))
  message("Set FLGBLQPR column to ",FLGBLQPR)
  data$FLGBLQPR <- FLGBLQPR
  message("Set FLGBLQIN column to ",FLGBLQIN)
  data$FLGBLQIN <- FLGBLQIN
  message("Set FLGBLQP1 column to ",FLGBLQP1)
  data$FLGBLQP1 <- FLGBLQP1
  message("Set FLGBLQPO column to ",FLGBLQPO)
  data$FLGBLQPO <- FLGBLQPO
  message("Set FGBQPLIN column to ",FGBQPLIN)
  data$FGBQPLIN <- FGBQPLIN
  message("Set FGBQPLOG column to ",FGBQPLOG)
  data$FGBQPLOG <- FGBQPLOG
  data
}
ignoreSUMdefault_IQdataNCA <- function (data) {
  ix_TIME_NA <- which(is.na(data$TIME))
  if (length(ix_TIME_NA) > 0) message("N=",length(ix_TIME_NA)," missing TIME entries have been set to be ignored in summary tables")
  data$IGNORSUM[ix_TIME_NA] <- addcomment_IQdataNCA(data$IGNORSUM[ix_TIME_NA],"Missing analysis time")
  ix_CONC_NA <- which(is.na(data$CONC))
  if (length(ix_CONC_NA) > 0) message("N=",length(ix_CONC_NA)," missing concentration (CONC) entries have been set to be ignored in summary tables")
  data$IGNORSUM[ix_CONC_NA] <- addcomment_IQdataNCA(data$IGNORSUM[ix_CONC_NA],"Missing concentration value")
  if (data$COMPTYPE[1]=="exogenous") {
    ix_CONCPFDGELLOQ <- which(data$TIME <= 0 & data$PROFTYPE %in% c("SD","FD") & !is.na(data$TIME) & !is.na(data$CONC) & data$CONC>=data$LLOQ)
    if (length(ix_CONCPFDGELLOQ) > 0) message("N=",length(ix_CONC_NA)," >=LLOQ pre-first dose records have been set to be ignored in summary tables")
    data$IGNORSUM[ix_CONCPFDGELLOQ] <- addcomment_IQdataNCA(data$IGNORSUM[ix_CONCPFDGELLOQ],"Pre-first dose concentration >=LLOQ")
  }
  data
}
ignoreNCAdefault_IQdataNCA <- function (data) {
  ix_TIME_NA <- which(is.na(data$TIME))
  if (length(ix_TIME_NA) > 0) message("N=",length(ix_TIME_NA)," missing TIME entries have been set to be ignored in PK parameter calculation")
  data$IGNORNCA[ix_TIME_NA] <- addcomment_IQdataNCA(data$IGNORNCA[ix_TIME_NA],"Missing analysis time")
  ix_CONC_NA <- which(is.na(data$CONC))
  if (length(ix_CONC_NA) > 0) message("N=",length(ix_CONC_NA)," missing concentration (CONC) entries have been set to be ignored in summary tables")
  data$IGNORNCA[ix_CONC_NA] <- addcomment_IQdataNCA(data$IGNORNCA[ix_CONC_NA],"Missing concentration value")
  if (data$COMPTYPE[1]=="exogenous") {
    data <- do.call(rbind,lapply(split(data,data$USUBJID), function (d1) {
      do.call(rbind,lapply(split(d1,d1$PROFILE), function (d) {
        if (!d$PROFTYPE[1] %in% c("SD","FD")) return(d)
        ix_CONCPFDGELLOQ <- which(d$TIME <= 0 & !is.na(d$TIME) & !is.na(d$CONC) & d$CONC>=d$LLOQ)
        d$IGNORNCA[ix_CONCPFDGELLOQ] <- addcomment_IQdataNCA(d$IGNORNCA[ix_CONCPFDGELLOQ],"Pre-first dose concentration >=LLOQ")
        ix_CONCPFDGE5pctCMAX <- which(d$TIME <= 0 & !is.na(d$TIME) & !is.na(d$CONC) & d$CONC>=0.05*max(d$CONC,na.rm = TRUE))
        d$COMMENTR[ix_CONCPFDGE5pctCMAX] <- addcomment_IQdataNCA(d$COMMENTR[ix_CONCPFDGE5pctCMAX],"Pre-first dose concentration >=5% of Cmax!")
        d
      }))
    }))
    Nx <- sum(grepl("Pre-first dose concentration (CONC>=LLOQ)",data$IGNORNCA,fixed = TRUE))
    if (Nx > 0) message("N=",Nx," >=LLOQ pre-first dose records (exogenous analyte & single/first dose profiles) have been set to be ignored in PK parameter calculation")
    Nx <- sum(grepl("Pre-first dose concentration >=5% of Cmax!",data$COMMENTR,fixed = TRUE))
    if (Nx > 0) message("N=",Nx," >=LLOQ pre-first dose records (exogenous analyte & single/first dose profiles) have value of >= 5% of Cmax")
  }
  data
}
convertNumCharTypeSpec_IQdataNCA <- function (data) {
  spec <- getdataspec_IQdataNCA()
  info <- spec[,c("Column","Type")]
  info$Type <- toupper(info$Type)
  if (!all(info$Type %in% c("NUMERIC","CHARACTER"))) stopIQR("Data spec document contains a type that is not 'numeric' or 'character'")
  for (k in 1:nrow(info)) {
    col <- info$Column[k]
    type <- info$Type[k]
    if (col %in% names(data)) {
      if (type=="CHARACTER") data[[col]] <- as.character(data[[col]])
      if (type=="NUMERIC") data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
    }
  }
  data
}
#' Define subjects to be ignored for analyses with reason why
#'
#' Ignoring of subjects is done by entering a reason in the IGNOREI column. A subject that is ignored
#' is not considered at all in any step but the listing of individual PK concentrations. This means it does not appear
#' in the PK parameter listings, tables, figures, and neither is considered in the PK parameter determination.
#' An ignored subject will appear in the listings of individual PK concentrations but it will be documented
#' in the text that it is ignored from the rest of the analysis.
#'
#' @param data IQdataNCA object in which to ignore selected subjects
#' @param REASON Reason for ignoring the subject(s)
#' @param USUBJID USUBJID of the subject to be ignored. Can be character vector with multiple USUBJIDs (same reason then)
#' @param PROFILE Profile name (entry in PROFILE column) to which to apply the ignoring (if multiple profiles available in the IQdataNCA object).
#'   If no PROFILE name defined then this is applied to the subject for all possible profiles. PROFILE can be character vector, allowing to define
#'   multiple profiles to which the ignoring should be applied.
#' @return Updated IQdataNCA object with adjusted ignore settings
#' @export
#' @family NCA Data
ignoresubject_IQdataNCA <- function (data,REASON,USUBJID,PROFILE=NULL) {
  if (!is_IQdataNCA(data)) stopIQR("data input argument is not an IQdataNCA object")
  if (length(REASON) != 1) stopIQR("REASON has to be a single character string")
  if (!is.character(REASON)) stopIQR("REASON has to be a single character string")
  if (is.na(REASON)) stopIQR("REASON has to be a single character string")
  if (!all(USUBJID %in% data$USUBJID)) stopIQR("USUBJID is not present in the data")
  if (!is.null(PROFILE)) {
    if (!all(PROFILE %in% data$PROFILE)) stopIQR("PROFILE is not present in the data")
    if (!all(USUBJID %in% data$USUBJID[data$PROFILE %in% PROFILE])) stopIQR("USUBJID is not present in the chosen PROFILE data")
  }
  if (!is.null(PROFILE)) {
    data$IGNOREI[data$USUBJID %in% USUBJID  & data$PROFILE %in% PROFILE] <- REASON
  } else {
    data$IGNOREI[data$USUBJID %in% USUBJID] <- REASON
  }
  data
}
#' Define records to be ignored with reason why
#'
#' Ignoring of records is done by entering a reason in the IGNORER column. A record that is ignored
#' is not considered at all in any step. This means it does not appear in the listings, tables, figures,
#' and neither is considered in the PK parameter determination.
#' After ignoring the slope is recalculated. IT DOES NOT EVEN APPEAR IN THE LISTINGS OF THE
#' individual PK concentrations! So ... handle with care!
#'
#' @param data IQdataNCA object in which to ignore selected records
#' @param REASON Reason for ignoring the record(s)
#' @param USUBJID USUBJID of the subject from which records are to be ignored. Can be character vector with multiple USUBJIDs
#' @param PROFILE Profile name (entry in PROFILE column) to which to apply the ignoring (if multiple profiles available in the IQdataNCA object).
#'   If no PROFILE name defined then this is applied to the subject for all possible profiles. PROFILE can be character vector, allowing to define
#'   multiple profiles to which the ignoring should be applied.
#' @param IX Index (IX column value) of the record(s) to be ignored with the same reason. Can be scalar or vector.
#' @return Updated IQdataNCA object with adjusted ignore settings
#' @export
#' @family NCA Data
ignorerecord_IQdataNCA <- function (data,REASON,USUBJID,PROFILE=NULL,IX) {
  if (!is_IQdataNCA(data)) stopIQR("data input argument is not an IQdataNCA object")
  if (length(REASON) != 1) stopIQR("REASON has to be a single character string")
  if (!is.character(REASON)) stopIQR("REASON has to be a single character string")
  if (is.na(REASON)) stopIQR("REASON has to be a single character string")
  if (!all(USUBJID %in% data$USUBJID)) stopIQR("USUBJID is not present in the data")
  if (!is.null(PROFILE)) {
    if (!all(PROFILE %in% data$PROFILE)) stopIQR("PROFILE is not present in the data")
    if (!all(USUBJID %in% data$USUBJID[data$PROFILE %in% PROFILE])) stopIQR("USUBJID is not present in the chosen PROFILE data")
  }
  if (!is.null(PROFILE)) {
    if (!all(IX %in% data$IX[data$USUBJID %in% USUBJID  &  data$PROFILE %in% PROFILE])) stopIQR("IX is not present in the chosen USUBJID/PROFILE data")
  } else {
    if (!all(IX %in% data$IX[data$USUBJID %in% USUBJID])) stopIQR("IX is not present in the chosen USUBJID data")
  }
  if (!is.null(PROFILE)) {
    data$IGNORER[data$USUBJID %in% USUBJID  &  data$PROFILE %in% PROFILE  &  data$IX %in% IX] <- REASON
  } else {
    data$IGNORER[data$USUBJID %in% USUBJID  &  data$IX %in% IX] <- REASON
  }
  data <- updateslope_IQdataNCA(data)
  data
}
#' Define records to be excluded from summary and inferential procedures
#'
#' Ignoring of records is done by entering a reason in the IGNORSUM column. A record that is ignored
#' for the summary and inferential procedures is not considered for summary tables of the observed
#' concentration data. It will still be listed in the individual concentration listings and might be considered
#' in the PK parameter determination.
#'
#' @param data IQdataNCA object in which to ignore selected records
#' @param REASON Reason for ignoring the record(s)
#' @param USUBJID USUBJID of the subject from which records are to be ignored in the summary procedures. Can be character vector with multiple USUBJIDs
#' @param PROFILE Profile name (entry in PROFILE column) to which to apply the ignoring (if multiple profiles available in the IQdataNCA object).
#'   If no PROFILE name defined then this is applied to the subject for all possible profiles. PROFILE can be character vector, allowing to define
#'   multiple profiles to which the ignoring should be applied. If only one profile present then PROFILE is not required to be defined.
#' @param IX Index (IX column value) of the record(s) to be ignored in summary with the same reason. Can be scalar or vector.
#' @return Updated IQdataNCA object with adjusted ignore settings
#' @export
#' @family NCA Data
ignoreSUM_IQdataNCA <- function (data,REASON,USUBJID,PROFILE=NULL,IX) {
  if (!is_IQdataNCA(data)) stopIQR("data input argument is not an IQdataNCA object")
  if (length(REASON) != 1) stopIQR("REASON has to be a single character string")
  if (!is.character(REASON)) stopIQR("REASON has to be a single character string")
  if (is.na(REASON)) stopIQR("REASON has to be a single character string")
  if (!all(USUBJID %in% data$USUBJID)) stopIQR("USUBJID is not present in the data")
  if (!is.null(PROFILE)) {
    if (!all(PROFILE %in% data$PROFILE)) stopIQR("PROFILE is not present in the data")
    if (!all(USUBJID %in% data$USUBJID[data$PROFILE %in% PROFILE])) stopIQR("USUBJID is not present in the chosen PROFILE data")
  }
  if (!is.null(PROFILE)) {
    if (!all(IX %in% data$IX[data$USUBJID %in% USUBJID  &  data$PROFILE %in% PROFILE])) stopIQR("IX is not present in the chosen USUBJID/PROFILE data")
  } else {
    if (!all(IX %in% data$IX[data$USUBJID %in% USUBJID])) stopIQR("IX is not present in the chosen USUBJID data")
  }
  if (!is.null(PROFILE)) {
    data$IGNORSUM[data$USUBJID %in% USUBJID  &  data$PROFILE %in% PROFILE  &  data$IX %in% IX] <- REASON
  } else {
    data$IGNORSUM[data$USUBJID %in% USUBJID  &  data$IX %in% IX] <- REASON
  }
  data
}
#' Define records to be excluded from NCA PK parameter determination
#'
#' Ignoring of records is done by entering a reason in the IGNORNCA column. A record that is ignored
#' for the NCA parameter determination is not considered for slope calculation or any other PK parameter
#' determination. It will still be listed in the individual concentration listings and might appear in the
#' concentration summaries. After ignoring the slope is recalculated.
#'
#' @param data IQdataNCA object in which to ignore selected records
#' @param REASON Reason for ignoring the record(s)
#' @param USUBJID USUBJID of the subject from which records are to be ignored in the summary procedures. Can be character vector with multiple USUBJIDs
#' @param PROFILE Profile name (entry in PROFILE column) to which to apply the ignoring (if multiple profiles available in the IQdataNCA object).
#'   If no PROFILE name defined then this is applied to the subject for all possible profiles. PROFILE can be character vector, allowing to define
#'   multiple profiles to which the ignoring should be applied. If only one profile present then PROFILE is not required to be defined.
#' @param IX Index (IX column value) of the record(s) to be ignored in NCA parameter determination with the same reason. Can be scalar or vector.
#' @return Updated IQdataNCA object with adjusted ignore settings
#' @export
#' @family NCA Data
ignoreNCA_IQdataNCA <- function (data,REASON,USUBJID,PROFILE=NULL,IX) {
  if (!is_IQdataNCA(data)) stopIQR("data input argument is not an IQdataNCA object")
  if (length(REASON) != 1) stopIQR("REASON has to be a single character string")
  if (!is.character(REASON)) stopIQR("REASON has to be a single character string")
  if (is.na(REASON)) stopIQR("REASON has to be a single character string")
  if (!all(USUBJID %in% data$USUBJID)) stopIQR("USUBJID is not present in the data")
  if (!is.null(PROFILE)) {
    if (!all(PROFILE %in% data$PROFILE)) stopIQR("PROFILE is not present in the data")
    if (!all(USUBJID %in% data$USUBJID[data$PROFILE %in% PROFILE])) stopIQR("USUBJID is not present in the chosen PROFILE data")
  }
  if (!is.null(PROFILE)) {
    if (!all(IX %in% data$IX[data$USUBJID %in% USUBJID  &  data$PROFILE %in% PROFILE])) stopIQR("IX is not present in the chosen USUBJID/PROFILE data")
  } else {
    if (!all(IX %in% data$IX[data$USUBJID %in% USUBJID])) stopIQR("IX is not present in the chosen USUBJID data")
  }
  if (!is.null(PROFILE)) {
    data$IGNORNCA[data$USUBJID %in% USUBJID  &  data$PROFILE %in% PROFILE  &  data$IX %in% IX] <- REASON
  } else {
    data$IGNORNCA[data$USUBJID %in% USUBJID  &  data$IX %in% IX] <- REASON
  }
  data <- updateslope_IQdataNCA(data)
  data
}
#' Check if object is an IQdataNCA object
#'
#' @param input object to check
#' @return TRUE if it is an IQdataNCA object. FALSE if it is not
#' @export
#'
is_IQdataNCA <- function (input)
{
  test <- methods::is(input, "IQdataNCA")
  contains_paramspec <- ("paramspec" %in% names(attributes(input)))
  if (!contains_paramspec) warning("Checked object did not contain a parameter specification. Default specification will be used.")
  test
}
#' Generic summary function for IQdataNCA objects
#'
#' @param object IQdataNCA object
#' @param ... Additional arguments - whatever R hides in these ... bad style!
#' @export
summary.IQdataNCA <- function (object,...) {
  profiles <- unique(object$PROFILE)
  nsubjectsprofile <- sapply(profiles, function (p) length(unique(object$USUBJID[object$PROFILE==p])))
  nsamplesprofile <- sapply(profiles, function (p) length(object$USUBJID[object$PROFILE==p]))
  tab <- data.frame(
    Profile = profiles,
    Nsubjects = nsubjectsprofile,
    Nsamples = nsamplesprofile,
    stringsAsFactors = FALSE
  )
  names(tab) <- c("Profile","Number subjects","Number samples")
  cat(text_IQRoutputTable(IQRoutputTable(tab),report = FALSE))
  x <- getaddcovcolumns_IQdataNCA(object)
  if (length(x) > 0) {
    cat(paste0("\nThe following additional (potential covariate) columns are present:\n  ",paste0(getaddcovcolumns_IQdataNCA(object),collapse = ", ")))
  }
}
#' Generic print function for IQdataNCA objects
#'
#' @param x IQdataNCA object
#' @param ... Additional arguments - whatever R hides in these ... bad style!
#' @export
print.IQdataNCA <- function (x,...) {
  print(as.data.frame(x))
  profiles <- unique(x$PROFILE)
  nsubjectsprofile <- sapply(profiles, function (p) length(unique(x$USUBJID[x$PROFILE==p])))
  nsamplesprofile <- sapply(profiles, function (p) length(x$USUBJID[x$PROFILE==p]))
  tab <- data.frame(
    Profile = profiles,
    Nsubjects = nsubjectsprofile,
    Nsamples = nsamplesprofile,
    stringsAsFactors = FALSE
  )
  if (nrow(tab)>0) {
    names(tab) <- c("Profile","Number subjects","Number samples")
    cat("\n",text_IQRoutputTable(IQRoutputTable(tab),report = FALSE))
    y <- getaddcovcolumns_IQdataNCA(x)
    if (length(y) > 0) {
      cat(paste0("\nThe following additional (potential covariate) columns are present:\n  ",paste0(getaddcovcolumns_IQdataNCA(x),collapse = ", ")),"\n")
    }
  }
  cat("\nIQdataNCA object\n\n")
  message("To better explore the NCA data, export them with export_IQdataNCA() to xlsx and/or xpt format")
}
#' Get additional (undefined potential covariate) columns for IQdataNCA objects
#'
#' @param data IQdataNCA object
#' @export
getaddcovcolumns_IQdataNCA <- function (data) {
  x <- setdiff(names(data),unique(unname(unlist(getColumnNames_IQdataNCA()))))
  x <- setdiff(x,getColumnNames_IQparamNCA(data))
  AUCINTnames <- sapply(attributes(data)$intervalAUC, function (y) y$colname)
  AUCINTDnames <- paste0(AUCINTnames,"D")
  setdiff(x,c(AUCINTnames, AUCINTDnames))
}
addtimecolumns_IQdataNCA <- function (data=data,FLAGTIME,FATIMIMP,FLAGoverwrite=FALSE) {
  if (!"ATAFD" %in% names(data)) {
    data$ATAFD <- data$ATIME
    message("Added column ATAFD with values from ATIME")
  }
  if (!"NTAFD" %in% names(data)) {
    data$NTAFD <- data$NTIME
    message("Added column NTAFD with values from NTIME")
  }
  if ("FLAGTIME" %in% names(data)) {
    message("Overwrote column FLAGTIME with provided value '",FLAGTIME,"'")
  }
  data$FLAGTIME <- FLAGTIME
  if ("FATIMIMP" %in% names(data)) {
    message("Overwrote column FATIMIMP with provided value '",FATIMIMP,"'")
  }
  data$FATIMIMP <- FATIMIMP
  if (FLAGTIME=="actual" & all(is.na(data$ATIME))) {
    message("FLAGTIME is 'actual' but ATIME only includes NA values => resetting FLAGTIME to 'nominal'")
    data$FLAGTIME <- "nominal"
  }
  if (FLAGTIME=="actual") {
    if ("TAFD" %in% names(data)) {
      message("Overwrote column TAFD with values in ATAFD")
    } else {
      message("Added column TAFD with values in ATAFD")
    }
    data$TAFD <- data$ATAFD
  } else {
    if (FLAGTIME!="nominal") stopIQR("Wrong definition of FLAGTIME")
    if ("TAFD" %in% names(data)) {
      message("Overwrote column TAFD with values in NTAFD")
    } else {
      message("Added column TAFD with values in NTAFD")
    }
    data$TAFD <- data$NTAFD
  }
  if (FLAGTIME=="actual") {
    if ("TIME" %in% names(data)) {
      message("Overwrote column TIME with values in ATIME")
    } else {
      message("Added column TIME with values in ATIME")
    }
    data$TIME <- data$ATIME
  } else {
    if (FLAGTIME!="nominal") stopIQR("Wrong definition of FLAGTIME")
    if ("TIME" %in% names(data)) {
      message("Overwrote column TIME with values in NTIME")
    } else {
      message("Added column TIME with values in NTIME")
    }
    data$TIME <- data$NTIME
  }
  if (FLAGTIME=="actual") {
    if ("DUR" %in% names(data)) {
      message("Overwrote column DUR with values in ADUR")
    } else {
      message("Added column DUR with values in ADUR")
    }
    data$DUR <- data$ADUR
  } else {
    if (FLAGTIME!="nominal") stopIQR("Wrong definition of FLAGTIME")
    if ("DUR" %in% names(data)) {
      message("Overwrote column DUR with values in NDUR")
    } else {
      message("Added column DUR with values in NDUR")
    }
    data$DUR <- data$NDUR
  }
  if (FLAGTIME=="actual" & FATIMIMP=="nominal") {
    ix_TAFD_missing <- which(is.na(data$TAFD))
    if (length(ix_TAFD_missing)>0) {
      message("N=",length(ix_TAFD_missing)," missing TAFD entries have been imputed with values in NTAFD")
      data$TAFD[ix_TAFD_missing] <- data$NTAFD[ix_TAFD_missing]
      data$COMMENTR[ix_TAFD_missing] <- addcomment_IQdataNCA(data$COMMENTR[ix_TAFD_missing],"Time imputation based on nominal")
    }
    ix_TIME_missing <- which(is.na(data$TIME))
    if (length(ix_TIME_missing)>0) {
      message("N=",length(ix_TIME_missing)," missing TIME entries have been imputed with values in NTIME")
      data$TIME[ix_TIME_missing] <- data$NTIME[ix_TIME_missing]
      data$COMMENTR[ix_TIME_missing] <- addcomment_IQdataNCA(data$COMMENTR[ix_TIME_missing],"Time imputation based on nominal")
    }
    ix_DUR_missing <- which(is.na(data$DUR) & data$ADM=="INFUSION")
    if (length(ix_DUR_missing)>0) {
      message("N=",length(ix_DUR_missing)," missing DUR entries have been imputed with values in NDUR")
      data$DUR[ix_DUR_missing] <- data$NDUR[ix_DUR_missing]
      data$COMMENTR[ix_DUR_missing] <- addcomment_IQdataNCA(data$COMMENTR[ix_DUR_missing],"Time imputation based on nominal")
    }
  }
  data
}
addcomment_IQdataNCA <- function (commentcolumnvalues,addcomment) {
  out <- rep(NA,length(commentcolumnvalues))
  out[is.na(commentcolumnvalues)] <- addcomment
  out[!is.na(commentcolumnvalues) & !grepl(addcomment,commentcolumnvalues)] <- paste0(commentcolumnvalues[!is.na(commentcolumnvalues) & !grepl(addcomment,commentcolumnvalues)],":::",addcomment)
  out[!is.na(commentcolumnvalues) & grepl(addcomment,commentcolumnvalues)] <- commentcolumnvalues[!is.na(commentcolumnvalues) & grepl(addcomment,commentcolumnvalues)]
  out
}
addflagcomment_IQdataNCA <- function (data,COMPTYPE) {
  if (!"COMPTYPE" %in% names(data)) {
    data$COMPTYPE <- COMPTYPE
    message("Added column COMPTYPE with value ",COMPTYPE)
  }
  if (!"IGNOREI" %in% names(data)) {
    data$IGNOREI <- as.character(NA)
    message("Added column IGNOREI with value NA")
  }
  if (!"IGNORER" %in% names(data)) {
    data$IGNORER <- as.character(NA)
    message("Added column IGNORER with value NA")
  }
  if (!"IGNORSUM" %in% names(data)) {
    data$IGNORSUM <- as.character(NA)
    message("Added column IGNORSUM with value NA")
  }
  if (!"IGNORNCA" %in% names(data)) {
    data$IGNORNCA <- as.character(NA)
    message("Added column IGNORNCA with value 1")
  }
  if (!"COMMENTR" %in% names(data)) {
    data$COMMENTR <- as.character(NA)
    message("Added column COMMENTR with value NA")
  }
  if (!"COMMENTI" %in% names(data)) {
    data$COMMENTI <- as.character(NA)
    message("Added column COMMENTI with value NA")
  }
  data
}
addix_IQdataNCA <- function (data) {
  message("Setting IX column")
  dataout <- do.call(rbind,lapply(split(data,data$USUBJID), function (d1) {
    do.call(rbind,lapply(split(d1,d1$PROFILE), function (d2) {
      d2$IX <- 1:nrow(d2)
      d2
    }))
  }))
  dataout
}
addcovariatecolumns_IQdataNCA <- function (data) {
  if (!"PERIOD" %in% names(data)) {
    data$PERIOD <- as.character(NA)
    message("Added column PERIOD with value NA")
  }
  if (!"SEQUENCE" %in% names(data)) {
    data$SEQUENCE <- as.character(NA)
    message("Added column SEQUENCE with value NA")
  }
  if (!"COUNTRY" %in% names(data)) {
    data$COUNTRY <- as.character(NA)
    message("Added column COUNTRY with value NA")
  }
  if (!"SITEID" %in% names(data)) {
    data$SITEID <- as.character(NA)
    message("Added column SITEID with value NA")
  }
  if (!"AGE" %in% names(data)) {
    data$AGE <- NA
    message("Added column AGE with value NA")
  }
  if (!"SEX" %in% names(data)) {
    data$SEX <- as.character(NA)
    message("Added column SEX with value NA")
  }
  if (!"RACE" %in% names(data)) {
    data$RACE <- as.character(NA)
    message("Added column RACE with value NA")
  }
  data
}
addoptionalcolumns_IQdataNCA <- function (data) {
  if (!"VISIT" %in% names(data)) {
    data$VISIT <- as.character(NA)
    message("Added column VISIT with value NA")
  }
  if (!"VISITNUM" %in% names(data)) {
    data$VISITNUM <- NA
    message("Added column VISITNUM with value NA")
  }
  if (!"PCTPT" %in% names(data)) {
    data$PCTPT <- as.character(NA)
    message("Added column PCTPT with value NA")
  }
  if (!"PCDTC" %in% names(data)) {
    data$PCDTC <- as.character(NA)
    message("Added column PCDTC with value NA")
  }
  if (!"EXSTDTC" %in% names(data)) {
    data$EXSTDTC <- as.character(NA)
    message("Added column EXSTDTC with value NA")
  }
  data
}
addconditionalrequiredcolumns_IQdataNCA <- function (data) {
  if (!"TAU" %in% names(data)) {
    data$TAU <- NA
    message("Added column TAU with value NA")
  }
  if (!"ADUR" %in% names(data)) {
    data$ADUR <- NA
    message("Added column ADUR with value NA")
  }
  if (!"NDUR" %in% names(data)) {
    data$NDUR <- NA
    message("Added column NDUR with value NA")
  }
  data
}
#' AUC calculation
#'
#' Calculation of AUC by selected method.
#' Same methods as in Winnonlin are implemented: "Linear Log", "LinearUp LogDown", "Linear LinearInterpolation", "Linear LinearLogInterpolation"
#' The "Trapezoidal" dose not appear in the name of the method to make it shorter.
#'
#' @param x time or similar vector
#' @param y concentration or similar vector
#' @param AUCMETHD Defines AUC calculation method. "Linear Log", "LinearUp LogDown", "Linear LinearInterpolation", "Linear LinearLogInterpolation"
#' @param TMAX TMAX can be provided. If not provided Tmax will be assumed to be included in the provided data and can be determined. Otherwise
#'   TMAX can be provided.
#' @param last if TRUE only return last value
#' @return A list with entry for AUC and AUMC. Either a scalar (last) or a vector (all values)
#' @export
AUC_IQnca <- function(x, y, AUCMETHD="Linear Log", TMAX=NULL, last=TRUE)
{
  if (!tolower(AUCMETHD) %in% c("linear log", "linearup logdown", "linear linearinterpolation", "linear linearloginterpolation")) stopIQR("AUCMETHD needs to be either 'linear log', 'linearup logdown', 'linear linearinterpolation', or 'linear linearloginterpolation'")
  n <- length(x)
  calc__ <- matrix(nrow=n, ncol=2)
  if (is.null(TMAX)) TMAX <- min(x[which(y==max(y))])
  calc__[1,] <- c(0, 0)
  for (i in 2:n) {
    if (y[i-1]<0 | y[i]<0 | y[i-1]==y[i]) {
      calc__[i,1] <- (x[i] - x[i-1])*(y[i] + y[i-1])/2
      calc__[i,2] <- (x[i] - x[i-1])*(x[i]*y[i] + x[i-1]*y[i-1])/2
    } else if (tolower(AUCMETHD)=="linear log") {
      if (x[i-1] < TMAX) {
        calc__[i,1] <- (x[i] - x[i-1])*(y[i] + y[i-1])/2
        calc__[i,2] <- (x[i] - x[i-1])*(x[i]*y[i] + x[i-1]*y[i-1])/2
      } else {
        k <- (log(y[i - 1]) - log(y[i]))/(x[i] - x[i-1])
        if (k==0 | is.nan(k) | is.infinite(k)) {
          calc__[i,1] <- (x[i] - x[i-1])*(y[i] + y[i-1])/2
          calc__[i,2] <- (x[i] - x[i-1])*(x[i]*y[i] + x[i-1]*y[i-1])/2
        } else {
          calc__[i,1] <- (y[i-1] - y[i])/k
          calc__[i,2] <- (x[i-1]*y[i-1] - x[i]*y[i])/k + (y[i-1] - y[i])/k/k
        }
      }
    } else if (tolower(AUCMETHD)=="linearup logdown") {
      if (y[i] >= y[i - 1]) { 
        calc__[i,1] <- (x[i] - x[i-1])*(y[i] + y[i-1])/2
        calc__[i,2] <- (x[i] - x[i-1])*(x[i]*y[i] + x[i-1]*y[i-1])/2
      } else {
        k <- (log(y[i - 1]) - log(y[i]))/(x[i] - x[i-1])
        if (k==0 | is.nan(k) | is.infinite(k)) {
          calc__[i,1] <- (x[i] - x[i-1])*(y[i] + y[i-1])/2
          calc__[i,2] <- (x[i] - x[i-1])*(x[i]*y[i] + x[i-1]*y[i-1])/2
        } else {
          calc__[i,1] <- (y[i-1] - y[i])/k
          calc__[i,2] <- (x[i-1]*y[i-1] - x[i]*y[i])/k + (y[i-1] - y[i])/k/k
        }
      }
    } else if (tolower(AUCMETHD) %in% c("linear linearinterpolation","linear linearloginterpolation")) {
      calc__[i,1] <- (x[i] - x[i-1])*(y[i] + y[i-1])/2
      calc__[i,2] <- (x[i] - x[i-1])*(x[i]*y[i] + x[i-1]*y[i-1])/2
    }
  }
  if (last) {
    AUC <- cumsum(calc__[,1])[nrow(calc__)]
    AUMC <- cumsum(calc__[,2])[nrow(calc__)]
  } else {
    AUC <- cumsum(calc__[,1])
    AUMC <- cumsum(calc__[,2])
  }
  list(AUC=AUC, AUMC=AUMC)
}
intervalAUC_IQnca <- function(dCalc, t1, t2)
{
  if (t1 > dCalc$TLST[1]) return(list(AUC=NA,AUMC=NA))
  if (t2 > dCalc$TLST[1] & is.na(dCalc$LAMZ[1])) return(list(AUC=NA,AUMC=NA))
  new1 <- interpolate_IQnca(dCalc=dCalc, xnew=t1, FLAGxexistsReturnNULL=TRUE)
  new2 <- interpolate_IQnca(dCalc=dCalc, xnew=t2, FLAGxexistsReturnNULL=TRUE)
  help <- dplyr::arrange(data.frame(
    x = c(dCalc$TIME,new1$xnew,new2$xnew),
    y = c(dCalc$CONC,new1$ynew,new2$ynew)
  ),x)
  x <- help$x
  y <- help$y
  AUC_IQnca(x = x[x>=t1 & x<=t2],y = y[x>=t1 & x<=t2],AUCMETHD = dCalc$AUCMETHD[1],TMAX = dCalc$TMAX[1])
}
interpolate_IQnca <- function(dCalc, xnew, FLAGxexistsReturnNULL)
{
  x <- dCalc$TIME
  y <- dCalc$CONC
  LAMZ <- dCalc$LAMZ[1]
  LAMZICPT <- dCalc$LAMZICPT[1]
  AUCMETHD <- tolower(dCalc$AUCMETHD[1])
  CMAX <- dCalc$CMAX[1]
  TMAX <- dCalc$TMAX[1]
  C0 <- dCalc$C0[1]
  ADM <- dCalc$ADM[1]
  if (xnew %in% x) {
    if (FLAGxexistsReturnNULL) return(list(xnew=NULL, ynew=NULL))
    ynew <- y[which(x==xnew)[1]]
    return(list(xnew=xnew, ynew=ynew))
  }
  if (!AUCMETHD %in% c("linear log", "linearup logdown", "linear linearinterpolation", "linear linearloginterpolation")) stopIQR("AUCMETHD needs to be either 'linear log', 'linearup logdown', 'linear linearinterpolation', or 'linear linearloginterpolation'")
  x1 <- NA
  y1 <- NA
  if (sum(x < xnew) > 0) {
    x1 <- x[max(which(x < xnew))]
    y1 <- y[max(which(x < xnew))]
  }
  x2 <- NA
  y2 <- NA
  if (sum(x > xnew) > 0) {
    x2 <- x[min(which(x > xnew))]
    y2 <- y[min(which(x > xnew))]
  }
  if (!is.na(x1) & !is.na(x2)) {
    if (y1 <= 0 | y2 <= 0 | y1==y2) {
      ynew <- y1 + (y2 - y1)/(x2 - x1)*(xnew - x1)
    } else if (AUCMETHD %in% c("linear log","linear linearloginterpolation")) {
      if (xnew > TMAX | (ADM=="BOLUS" & C0>CMAX)) {
        ynew <- exp(log(y1) + (log(y2) - log(y1))/(x2 - x1)*(xnew - x1))
      } else {
        ynew <- y1 + (y2 - y1)/(x2 - x1)*(xnew - x1)
      }
    } else if (AUCMETHD=="linearup logdown") {
      if (y1>y2) {
        ynew <- exp(log(y1) + (log(y2) - log(y1))/(x2 - x1)*(xnew - x1))
      } else {
        ynew <- y1 + (y2 - y1)/(x2 - x1)*(xnew - x1)
      }
    } else if (AUCMETHD=="linear linearinterpolation") {
      ynew <- y1 + (y2 - y1)/(x2 - x1)*(xnew - x1)
    }
  }
  if (!is.na(x1) & is.na(x2))  ynew <- exp(LAMZICPT-LAMZ*xnew)
  if (is.na(x1)  & !is.na(x2)) ynew <- y2/x2*xnew 
  if (is.na(x1)  & is.na(x2))  return(list(xnew=NULL, ynew=NULL))
  list(xnew=xnew, ynew=ynew)
}
#' Calculation of NCA PK parameters
#'
#' Ignored: ignored subjects (IGNOREI), ignored records (IGNORER and IGNORENCA)
#' Computed parameters and algorithm settings fully controlled by settings in the
#' IQdataNCA object.
#' @param data IQdataNCA object
#' @param milliLiter  By default L (Liter) is used as a standard unit for volume. If desired, by setting this
#'   flag to TRUE also mL (milliliter) can be used. This is defined here during the calculation of the
#'   PK parameters and not in the dataset as numerically it will give the same results (with some commata shifts).
#' @param CTLASTwinnonlinbehavior If BLLOQ handling rule is set to use LLOQ/2 or LLOQ
#' for the first BLLOQ value post last observable, then Clast and Tlast will be set based
#' on this first BLLOQ imputation. This is counterintuitive - but in order to be aligned
#' with Winnonlin we had to add this exception. We do so here as an option ...
#' Use TRUE by default.
#' @return IQnca object with calculated parameters. IQdataNCA object is added as attribute "dataNCA".
#' Information about column names and labels for interval AUC is also stored as attributes.
#' @export
nca_IQdataNCA <- function (data,milliLiter=FALSE,CTLASTwinnonlinbehavior=TRUE) {
  dataOrig <- data
  paramspecOrig <- getparamspec_IQdataNCA(data)
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  data <- removeFrom_IQdataNCA(data = data,
                               FLAGremoveIGNOREI = TRUE,
                               FLAGremoveIGNORER = TRUE,
                               FLAGremoveIGNORSUM = FALSE,
                               FLAGremoveIGNORNCA = TRUE)
  paramSpec <- getparamspec_IQdataNCA(data)
  resultColsMissing <- setdiff(paramSpec$PKPARAMCD,names(data))
  for (k in seq_along(resultColsMissing)) data[resultColsMissing[k]] <- NA
  if (length(unique(data$AUCINVAL)) != 1) stopIQR("AUCINVAL column in dataNCA needs a single unique entry everywhere")
  if (!is.na(data$AUCINVAL[1])) {
    s <- aux_explodePC(input = data$AUCINVAL[1],group = "square",separator = ";")
    intervalAUC <- lapply(seq_along(s), function (k) {
      x <- s[[k]]
      x <- gsub("[","",x,fixed = TRUE)
      x <- gsub("]","",x,fixed = TRUE)
      x <- aux_explode(x,separator = ";")
      if (length(x) != 2) stopIQR("AUCINVAL wrongly defined")
      x <- suppressWarnings(as.numeric(x))
      if (any(is.na(x))) stopIQR("AUCINVAL wrongly defined")
      x <- sort(x)
      if (x[1]==x[2]) stopIQR("AUCINVAL wrongly defined")
      t1 <- x[1]
      t2 <- x[2]
      list(
        colname = paste0("AUCINT",k),
        label   = paste0("[",t1,";",t2," ",getTIMEUNITname_IQdataNCA(data),"]"),
        tstart  = t1,
        tend    = t2
      )
    })
  } else {
    intervalAUC <- NULL
  }
  data$SPLIT <- paste0(data$PROFILE,data$USUBJID)
  dS <- split(data,data$SPLIT)
  lresNCA <- lapply(seq_along(dS), function (kkk) {
    d <- dS[[kkk]]
    d <- d[!is.na(d$CONC),]
    dCalc <- data.frame(d[,c("USUBJID","IGNORNCA","SLOPEPT","COMPTYPE","TIME","CONC","DOSE","DUR","ADM","PROFTYPE","PROFILE","TAU","AUCMETHD","BLLOQ",getColumnNames_IQparamNCA(data))])
    intaucnames <- sapply(intervalAUC, function (x) x$colname)
    if (length(intaucnames) > 0) {
      for (colname in intaucnames) {
        dCalc[[colname]] <- NA
        dCalc[[paste0(colname,"D")]] <- NA
      }
    }
    if (length(unique(dCalc$CONC))==1) {
      if (dCalc$CONC[1] > 0) {
        dCalc$CMAX <- dCalc$CONC[1]
        if (!is.na(dCalc$DOSE[1]) & all(dCalc$DOSE>0)) dCalc$CMAXD <- dCalc$CONC[1]/dCalc$DOSE[1]
        dCalc$TMAX <- dCalc$TIME[1]
        dCalc$TLAG <- 0
        dCalc$CLST <- dCalc$CONC[1]
        dCalc$TLST <- max(dCalc$TIME)
        dCalc$LAMZNPT <- 0
        dCalc$LAMZICPT <- dCalc$CONC[1]
      } else {
        dCalc$CMAX <- 0
        if (!is.na(dCalc$DOSE[1]) & all(dCalc$DOSE>0)) dCalc$CMAXD <- 0
        dCalc$TMAX <- dCalc$TIME[1]
        dCalc$TLAG <- 0
        dCalc$CLST <- 0
        dCalc$TLST <- NA
        dCalc$LAMZNPT <- 0
        dCalc$LAMZICPT <- 0
      }
      dCalc$AUCINTX <- NULL
      dCalc$AUCINTXD <- NULL
      return(dCalc)
    }
    if (sum(dCalc$TIME<0) > 1) stopIQR("More than one pre-dose sample in subject ",dCalc$USUBJID[1])
    FLAGnegTime <- FALSE
    if (any(dCalc$TIME<0)) {
      FLAGnegTime <- TRUE
      dCalc$TIME[dCalc$TIME<0] <- 0
      message("Setting negative TIME(s) to 0 in subject ",dCalc$USUBJID[1])
      if (length(dCalc$TIME[dCalc$TIME==0])>1) {
        stopIQR(paste0("Several pre-dose times available in ",dCalc$USUBJID[1]," (might be due to negative times set to 0)"))
      }
    }
    if (dCalc$ADM[1]=="INFUSION" & any(dCalc$DUR==0)) stopIQR("Infusion mode should have DUR larger than 0 in subject: ",dCalc$USUBJID[1])
    if (dCalc$PROFTYPE[1] %in% c("SS") && sum(dCalc$TIME<=0)==0 && dCalc$ADM != "BOLUS") warningIQR("Analysis of extravascular and infusion steady-state data: no pre-dose sample in the data in subject: ",dCalc$USUBJID[1])
    if (dCalc$COMPTYPE[1] %in% c("endogenous") && sum(dCalc$TIME<=0)==0 && dCalc$ADM != "BOLUS") stopIQR("Analysis of extravascular and infusion data of endogenous analyte: no pre-dose sample in the data in subject: ",dCalc$USUBJID[1])
    if (!0 %in% dCalc$TIME) {
      message(dCalc$ADM[1],"/",dCalc$PROFTYPE[1]," - Missing predose value: C=NA added (will be imputed) at TIME=0 in subject: ",dCalc$USUBJID[1])
      row <- dCalc[1,]
      row$TIME <- 0
      row$CONC <- NA
      dCalc <- rbind(row,dCalc)
    }
    idxT0 = which(as.character(round(dCalc$TIME, 9)) == "0")
    if (length(idxT0) > 1) {if (idxT0 != 1) {stopIQR("Something went wrong with setting up profile concentration with exactly one observation at TIME = 0")}}
    dCalc$TMAX <- dCalc$TIME[which.max(dCalc$CONC)][1]
    dCalc$CMAX <- max(dCalc$CONC,na.rm = TRUE)
    if (!is.na(dCalc$DOSE[1]) & all(dCalc$DOSE>0)) dCalc$CMAXD <- dCalc$CMAX/dCalc$DOSE[1]
    if (dCalc$PROFTYPE[1] %in% c("SD","FD")) {
      dCalc$CMIN <- min(dCalc$CONC,na.rm = TRUE)
      if (!is.na(dCalc$DOSE[1]) & all(dCalc$DOSE>0)) dCalc$CMIND <- dCalc$CMIN/dCalc$DOSE[1]
      dCalc$TMIN <- dCalc$TIME[which(dCalc$CONC==dCalc$CMIN[1])[1]]
    } else {
      dCalc$CMIN <- min(dCalc$CONC[dCalc$TIME<=dCalc$TAU],na.rm = TRUE)
      if (!is.na(dCalc$DOSE[1]) & all(dCalc$DOSE>0)) dCalc$CMIND <- dCalc$CMIN/dCalc$DOSE[1]
      dCalc$TMIN <- dCalc$TIME[which(dCalc$CONC==dCalc$CMIN[1])[1]]
    }
    y <- dCalc$CONC
    x <- dCalc$TIME
    if (dCalc$ADM[1] == "BOLUS" ) {
      if ( is.na(dCalc$CONC[1]) || FLAGnegTime ) {
        if (y[2] < y[3] | y[2] < 0 | y[3] < 0) {
          dCalc$CONC[1] <- y[x==min(x[y > 0],na.rm = TRUE)]
        } else {
          dCalc$CONC[1] <- exp(-x[2]*(log(y[3]) - log(y[2]))/(x[3] - x[2]) + log(y[2]))
        }
        if (FLAGnegTime) {
          message(dCalc$ADM[1],"/",dCalc$PROFTYPE[1]," - Extrapolated C0 used as value for TIME=0 (Observed value at TIME<0 overwritten) in subject: ",dCalc$USUBJID[1])
        } else {
          message(dCalc$ADM[1],"/",dCalc$PROFTYPE[1]," - Extrapolated C0 used as value for TIME=0 (Observed value at TIME=0 not available) in subject: ",dCalc$USUBJID[1])
        }
      } else {
        message(dCalc$ADM[1],"/",dCalc$PROFTYPE[1]," - C0 taken from dataset as value at TIME=0 (No back-extrapolation) in subject: ",dCalc$USUBJID[1])
      }
    } else {
      if (is.na(dCalc$CONC[1])) {
        if (dCalc$PROFTYPE[1] %in% c("SD","FD")) {
          if (dCalc$COMPTYPE[1] == "exogenous") {
            message(dCalc$ADM[1],"/",dCalc$PROFTYPE[1]," - Missing predose value: C=0 imputed at TIME=0 in subject: ",dCalc$USUBJID[1])
            dCalc$CONC[1] <- 0
          } else {
            message(dCalc$ADM[1],"/",dCalc$PROFTYPE[1]," - Missing predose value: minimum concentration imputed at TIME=0 in subject: ",dCalc$USUBJID[1])
            dCalc$CONC[1] <- min(dCalc$CONC, na.rm = TRUE)
          }
        }
        if (dCalc$PROFTYPE[1] %in% c("SS")) {
          if (is.na(dCalc$CONC[1])) {
            message(dCalc$ADM[1],"/",dCalc$PROFTYPE[1]," - Missing predose value: minimum concentration over dosing interval imputed at TIME=0 in subject: ",dCalc$USUBJID[1])
            dCalc$CONC[1] <- min(dCalc$CONC[dCalc$TIME <= dCalc$TAU], na.rm = TRUE)
          }
        }
      }
    }
    C0 <- dCalc$CONC[1]
    dCalc$C0 <- C0
    if (CTLASTwinnonlinbehavior) {
      help <- dCalc[dCalc$CONC>0,c("TIME","CONC","BLLOQ")]
      dCalc$TLST <- help$TIME[nrow(help)]
      dCalc$CLST <- help$CONC[nrow(help)]
    } else {
      help <- dCalc[dCalc$BLLOQ==0,c("TIME","CONC","BLLOQ")]
      dCalc$TLST <- help$TIME[nrow(help)]
      dCalc$CLST <- help$CONC[nrow(help)]
    }
    dCalc$NONTRAILINGZERO <- 0
    dCalc$NONTRAILINGZERO[1:max(which(dCalc$CONC > 0))] <- 1
    dCalc$LAMZHL <- log(2) / dCalc$LAMZ
    AUC <- AUC_IQnca(x = dCalc$TIME[dCalc$TIME<=dCalc$TLST], y = dCalc$CONC[dCalc$TIME<=dCalc$TLST], AUCMETHD = dCalc$AUCMETHD[1])
    dCalc$AUCLST <- AUC$AUC
    dCalc$AUMCLST <- AUC$AUMC
    AUC <- AUC_IQnca(x = dCalc$TIME, y = dCalc$CONC, AUCMETHD = dCalc$AUCMETHD[1])
    dCalc$AUCALL <- AUC$AUC
    dCalc$AUCIFO <- dCalc$AUCLST + dCalc$CLST/dCalc$LAMZ
    dCalc$AUCIFP <- dCalc$AUCLST + dCalc$CLSTP/dCalc$LAMZ
    dCalc$AUCPEO <- (1 - dCalc$AUCLST/dCalc$AUCIFO)*100
    dCalc$AUCPEP <- (1 - dCalc$AUCLST/dCalc$AUCIFP)*100
    dCalc$AUMCIFO <- dCalc$AUMCLST + dCalc$CLST*dCalc$TLST/dCalc$LAMZ + dCalc$CLST/dCalc$LAMZ/dCalc$LAMZ
    dCalc$AUMCIFP <- dCalc$AUMCLST + dCalc$CLSTP*dCalc$TLST/dCalc$LAMZ + dCalc$CLSTP/dCalc$LAMZ/dCalc$LAMZ
    dCalc$AUMCPEO <- (1 - dCalc$AUMCLST/dCalc$AUMCIFO)*100
    dCalc$AUMCPEP <- (1 - dCalc$AUMCLST/dCalc$AUMCIFP)*100
    if (!is.na(dCalc$DOSE[1]) & all(dCalc$DOSE>0)) dCalc$AUCLSTD <- dCalc$AUCLST/dCalc$DOSE[1]
    if (!is.na(dCalc$DOSE[1]) & all(dCalc$DOSE>0)) dCalc$AUCIFOD <- dCalc$AUCIFO/dCalc$DOSE[1]
    if (!is.na(dCalc$DOSE[1]) & all(dCalc$DOSE>0)) dCalc$AUCIFPD <- dCalc$AUCIFP/dCalc$DOSE[1]
    if (dCalc$ADM[1] == "BOLUS") {
      AUCvector <- AUC_IQnca(x = dCalc$TIME[dCalc$NONTRAILINGZERO==1], y = dCalc$CONC[dCalc$NONTRAILINGZERO==1], AUCMETHD = dCalc$AUCMETHD[1], last = FALSE)
      dCalc$AUCPBEO <- AUCvector$AUC[2]/dCalc$AUCIFO*100
      dCalc$AUCPBEP <- AUCvector$AUC[2]/dCalc$AUCIFP*100
    }
    if (dCalc$ADM[1] == "EXTRAVASCULAR") {
      x0 <- dCalc$TIME[dCalc$NONTRAILINGZERO==1]
      y0 <- dCalc$CONC[dCalc$NONTRAILINGZERO==1]
      if (sum(y0) > 0) {
        dCalc$TLAG <- x0[max(min(which(y0>0))-1,1)] 
      } else {
        dCalc$TLAG <- 0
      }
    } else {
      dCalc$TLAG <- NA
    }
    if (dCalc$PROFTYPE[1] %in% c("SS","FD")) {
      dCalc$AUCTAU <- intervalAUC_IQnca(dCalc = dCalc, t1 = 0, t2 = dCalc$TAU[1])$AUC
      dCalc$AUMCTAU <- intervalAUC_IQnca(dCalc = dCalc, t1 = 0, t2 = dCalc$TAU[1])$AUMC
      if (!is.na(dCalc$DOSE[1]) & all(dCalc$DOSE>0)) dCalc$AUCTAUD <- dCalc$AUCTAU/dCalc$DOSE[1]
      if (dCalc$TLST[1] >= dCalc$TAU[1]) {
        dCalc$AUCPTAUE <- 100
      } else {
        dCalc$AUCPTAUE <- 100*dCalc$AUCLST[1]/dCalc$AUCTAU[1]
      }
    }
    if (dCalc$ADM[1] == "EXTRAVASCULAR") {
      if (dCalc$PROFTYPE[1] %in% c("SD","FD")) {
        dCalc$VZFO <- dCalc$DOSE[1]/dCalc$AUCIFO[1]/dCalc$LAMZ[1]
        dCalc$VZFP <- dCalc$DOSE[1]/dCalc$AUCIFP[1]/dCalc$LAMZ[1]
        dCalc$CLFO <- dCalc$DOSE[1]/dCalc$AUCIFO[1]
        dCalc$CLFP <- dCalc$DOSE[1]/dCalc$AUCIFP[1]
        dCalc$MRTEVIFO <- dCalc$AUMCIFO[1]/dCalc$AUCIFO[1]
        dCalc$MRTEVIFP <- dCalc$AUMCIFP[1]/dCalc$AUCIFP[1]
      }
      if (dCalc$PROFTYPE[1] %in% c("SS")) {
        dCalc$CLFSS <- dCalc$DOSE[1]/dCalc$AUCTAU[1]
        dCalc$VZFSS <- dCalc$DOSE[1]/dCalc$AUCTAU[1]/dCalc$LAMZ[1]
        dCalc$MRTEVIFO <- (dCalc$AUMCTAU[1] + dCalc$TAU[1]*(dCalc$AUCIFO[1] - dCalc$AUCTAU[1])) / dCalc$AUCTAU[1]
        dCalc$MRTEVIFP <- (dCalc$AUMCTAU[1] + dCalc$TAU[1]*(dCalc$AUCIFP[1] - dCalc$AUCTAU[1])) / dCalc$AUCTAU[1]
      }
      dCalc$MRTEVLST <- dCalc$AUMCLST[1]/dCalc$AUCLST[1]
    } else {
      if (dCalc$PROFTYPE[1] %in% c("SD","FD")) {
        dCalc$VZO <- dCalc$DOSE[1]/dCalc$AUCIFO[1]/dCalc$LAMZ[1]
        dCalc$VZP <- dCalc$DOSE[1]/dCalc$AUCIFP[1]/dCalc$LAMZ[1]
        dCalc$CLO <- dCalc$DOSE[1]/dCalc$AUCIFO[1]
        dCalc$CLP <- dCalc$DOSE[1]/dCalc$AUCIFP[1]
        if (dCalc$ADM[1] == "BOLUS") {
          dCalc$MRTIVIFO <- dCalc$AUMCIFO[1]/dCalc$AUCIFO[1]
          dCalc$MRTIVIFP <- dCalc$AUMCIFP[1]/dCalc$AUCIFP[1]
        } else {
          dCalc$MRTIVIFO <- dCalc$AUMCIFO[1]/dCalc$AUCIFO[1] - dCalc$DUR[1]/2
          dCalc$MRTIVIFP <- dCalc$AUMCIFP[1]/dCalc$AUCIFP[1] - dCalc$DUR[1]/2
        }
        dCalc$VSSO <- dCalc$MRTIVIFO[1]*dCalc$CLO[1]
        dCalc$VSSP <- dCalc$MRTIVIFP[1]*dCalc$CLP[1]
      }
      if (dCalc$PROFTYPE[1] %in% c("SS")) {
        dCalc$CLSS <- dCalc$DOSE[1]/dCalc$AUCTAU[1]
        dCalc$VZSS <- dCalc$DOSE[1]/dCalc$AUCTAU[1]/dCalc$LAMZ[1]
        if (dCalc$ADM[1] == "BOLUS") {
          dCalc$MRTIVIFO <- (dCalc$AUMCTAU[1] + dCalc$TAU[1]*(dCalc$AUCIFO[1] - dCalc$AUCTAU[1])) / dCalc$AUCTAU[1]
          dCalc$MRTIVIFP <- (dCalc$AUMCTAU[1] + dCalc$TAU[1]*(dCalc$AUCIFP[1] - dCalc$AUCTAU[1])) / dCalc$AUCTAU[1]
        } else {
          dCalc$MRTIVIFO <- (dCalc$AUMCTAU[1] + dCalc$TAU[1]*(dCalc$AUCIFO[1] - dCalc$AUCTAU[1])) / dCalc$AUCTAU[1] - dCalc$DUR[1]/2
          dCalc$MRTIVIFP <- (dCalc$AUMCTAU[1] + dCalc$TAU[1]*(dCalc$AUCIFP[1] - dCalc$AUCTAU[1])) / dCalc$AUCTAU[1] - dCalc$DUR[1]/2
        }
      }
      if (dCalc$ADM[1] == "BOLUS") {
        dCalc$MRTIVLST <- dCalc$AUMCLST[1]/dCalc$AUCLST[1]
      } else {
        dCalc$MRTIVLST <- dCalc$AUMCLST[1]/dCalc$AUCLST[1] - dCalc$DUR[1]/2
      }
    }
    if (!is.null(intervalAUC)) {
      for (i in seq_along(intervalAUC)) {
        if (!all(names(intervalAUC[[i]]) %in% c("colname","label","tstart","tend"))) {
          stopIQR("Interval AUC argument 'intervalAUC' not correctly defined")
        }
        tryCatch({
          colname <- intervalAUC[[i]]$colname
          colnameD <- paste0(colname,"D")
          AUCINT <- intervalAUC_IQnca(dCalc = dCalc,t1 = intervalAUC[[i]]$tstart,t2 = intervalAUC[[i]]$tend)$AUC
          AUCINTD <- NA
          if (!is.na(dCalc$DOSE[1]) & all(dCalc$DOSE>0)) AUCINTD <- AUCINT / dCalc$DOSE[1]
          dCalc[[colname]] <- AUCINT
          dCalc[[colnameD]] <- AUCINTD
        },error=function (err) {
          stopIQR("Interval AUC argument 'intervalAUC' not correctly defined")
        })
      }
    }
    dCalc$SPAN <- (dCalc$LAMZUL[1] - dCalc$LAMZLL[1])/dCalc$LAMZHL[1]
    if (dCalc$PROFTYPE[1] %in% c("SS","FD")) {
      dCalc$CAVG <- dCalc$AUCTAU[1]/dCalc$TAU[1]
      dCalc$FLUCP <- 100*(dCalc$CMAX[1]-dCalc$CMIN[1])/dCalc$CAVG[1]
      dCalc$AILAMZ <- 1/(1-exp(-dCalc$LAMZ[1]*dCalc$TAU[1]))
      dCalc$CTAU <- interpolate_IQnca(dCalc = dCalc,xnew = dCalc$TAU[1],FLAGxexistsReturnNULL=FALSE)$ynew
      dCalc$FLUCPTAU <- 100*(dCalc$CMAX[1]-dCalc$CTAU[1])/dCalc$CAVG[1]
    }
    if (dCalc$PROFTYPE[1] %in% c("SS")) {
      dCalc$SWING <- (dCalc$CMAX[1]-dCalc$CMIN[1])/dCalc$CMIN[1]
      dCalc$SWINGTAU <- (dCalc$CMAX[1]-dCalc$CTAU[1])/dCalc$CTAU[1]
    }
    dCalc$NONTRAILINGZERO <- NULL
    dCalc$AUCINTX <- NULL
    dCalc$AUCINTXD <- NULL
    dCalc
  })
  resNCA <- do.call(rbind,lresNCA)
  resNCA <- resNCA[!duplicated(paste0(resNCA$USUBJID,resNCA$PROFTYPE,resNCA$PROFILE)),]
  resNCA <- resNCA[,setdiff(names(resNCA),setdiff(unname(unlist(getColumnNames_IQdataNCA())),
                                                  c("USUBJID","PROFTYPE","PROFILE",setdiff(getColumnNames_IQdataNCA()$slope,c("FLGSLOPE","SLOPEPT")))))]
  remove_cols <- c("DAY","ATIME","NTIME","ACONC","VISIT","VISITNUM","PCTPT","PCDTC",
                   "EXSTDTC","IX","IGNOREI","IGNORER","IGNORSUM","IGNORNCA","COMMENTR",
                   "COMMENTI","ATAFD","NTAFD","TAFD","TIME","BLLOQ","BLLOQPR","BLLOQIN","BLLOQP1",
                   "BLLOQPO","FLGBLQPR","FLGBLQIN","FLGBLQP1","FLGBLQPO","FGBQPLIN","FGBQPLOG",
                   "CONC","CONCPLIN","CONCPLOG","SLOPEPT",
                   "R2", "R2ADJ", "LAMZNPT", "LAMZ", "LAMZICPT", "CORRXY", "LAMZLL", "LAMZUL", "CLSTP")
  dataOrig_first_reduced <-dataOrig[!duplicated(paste0(dataOrig$USUBJID,dataOrig$PROFTYPE, dataOrig$PROFILE)),!names(dataOrig) %in% remove_cols]
  result <- dplyr::left_join(dataOrig_first_reduced,resNCA,by=c("USUBJID","PROFTYPE","PROFILE") )
  units <- units_IQnca(data,milliLiter=milliLiter)
  for (n in units$CLPARAM) result[[n]] <- units$FACTOR_CL_V*result[[n]]
  for (n in units$VPARAM) result[[n]] <- units$FACTOR_CL_V*result[[n]]
  test <- suppressWarnings(!any(is.na(as.numeric(result$USUBJID))))
  if (test) {
    result$USUBJID <- as.numeric(result$USUBJID)
    result <- dplyr::arrange(result,USUBJID)
  }
  attr(result, "dataNCA") <- dataOrig
  attr(result, "intervalAUC") <- intervalAUC
  attr(result, "units") <- units$units
  attr(result, "milliLiter") <- milliLiter
  attr(result, "paramspec") <- paramspecOrig
  class(result) <- c("IQnca", class(result))
  result
}
#' Generic print function for IQnca objects
#'
#' @param x IQnca object
#' @param ... Additional arguments - whatever R hides in these ... bad style!
#' @export
print.IQnca <- function (x,...) {
  paramSpec <- getparamspec_IQdataNCA(x)
  paramShow <- paramSpec$PKPARAMCD
  dataSpec <- getdataspec_IQdataNCA()
  dataShow <- intersect(dataSpec$Column,names(x))
  all <- unique(c(dataShow,paramShow))
  all <- setdiff(all,c("AUCINTX","AUCINTXD"))
  intAUCcols <- sapply(attributes(x)$intervalAUC, function (y) y$colname)
  if (length(intAUCcols)>0) all <- c(all,intAUCcols)
  colsNotNA <- sapply(1:ncol(x), function (k) !all(is.na(x[[k]])))
  xshow <- x[,colsNotNA]
  print(as.data.frame(xshow))
  profiles <- unique(x$PROFILE)
  nsubjectsprofile <- sapply(profiles, function (p) length(unique(x$USUBJID[x$PROFILE==p])))
  nsamplesprofile <- sapply(profiles, function (p) { y <- attributes(x)$dataNCA; nrow(y[y$PROFILE==p,])})
  tab <- data.frame(
    Profile = profiles,
    Nsubjects = nsubjectsprofile,
    Nsamples = nsamplesprofile,
    stringsAsFactors = FALSE
  )
  if (nrow(tab) > 0) {
    names(tab) <- c("Profile","Number subjects","Number samples")
    cat("\n",text_IQRoutputTable(IQRoutputTable(tab),report = FALSE))
    y <- getaddcovcolumns_IQdataNCA(x)
    if (length(y) > 0) {
      cat(paste0("\nThe following additional (potential covariate) columns are present:\n  ",paste0(getaddcovcolumns_IQdataNCA(x),collapse = ", ")),"\n")
    }
    y <- getIntervalAUCinfo_IQnca(x)
    cat("\n\nInterval AUC information:\n\n")
    cat(y,"\n")
  }
  cat("\nIQnca object\n\n")
  message("To better explore the NCA results, export them with export_IQnca() to text, xlsx, and/or xpt format")
}
#' Define interval AUC calculation
#'
#' Helper function to construct the intervalAUC input argument for nca_IQdataNCA()
#'
#' @param colname Column name in the result
#' @param label Label for the exported output
#' @param tstart Start of the interval
#' @param tend End of the interval
#' @return Just a list - can be used as input argument 'intervalAUC' to nca_IQdataNCA()
#' @export
intervalAUC_NCA <- function (colname,label,tstart,tend) {
  list(colname=colname,label=label,tstart=tstart,tend=tend)
}
getIntervalAUCinfo_IQnca <- function (data) {
  intervalAUC <- attributes(data)$intervalAUC
  if (is.null(intervalAUC)) return("No interval AUC calculated")
  tab <- data.frame(do.call(rbind,lapply(intervalAUC, function (y) y)),stringsAsFactors = FALSE)
  text_IQRoutputTable(IQRoutputTable(xtable = tab),report = FALSE)
}
units_IQnca <- function(data,milliLiter)
{
  timeUnit <- getTIMEUNITsymbol_IQdataNCA(data)
  doseUnit <- data$DOSEUNIT[1]
  concUnit <- data$CONCUNIT[1]
  if (!milliLiter) {
    if (toupper(concUnit) == toupper("g/L"))   concUnit <- "g/L"
    if (toupper(concUnit) == toupper("mg/mL")) concUnit <- "g/L"
    if (toupper(concUnit) == toupper("ug/mL")) concUnit <- "mg/L"
    if (toupper(concUnit) == toupper("ng/mL")) concUnit <- "ug/L"
    if (toupper(concUnit) == toupper("pg/mL")) concUnit <- "ng/L"
  } else {
    if (toupper(concUnit) == toupper("g/L"))   concUnit <- "mg/mL"
    if (toupper(concUnit) == toupper("mg/L")) concUnit <- "ug/mL"
    if (toupper(concUnit) == toupper("ug/L")) concUnit <- "ng/mL"
    if (toupper(concUnit) == toupper("ng/L")) concUnit <- "pg/mL"
  }
  paramspec <- getparamspec_IQdataNCA(data)
  units <- paramspec[,c("PKPARAMCD","Unit.Calculation")]
  units$UNIT <- units$Unit.Calculation
  relativeDose <- FALSE
  doseUnit_lhs <- strsplit(doseUnit, "/")[[1]][1]
  doseUnit_rhs <- paste(strsplit(doseUnit, "/")[[1]][-1], collapse = "/")
  if (nchar(doseUnit_rhs) > 0) relativeDose <- TRUE
  doseUnit_exchg <- doseUnit
  if (relativeDose) doseUnit_exchg <- paste0("(",doseUnit,")")
  units$UNIT <- gsub("DOSEUNIT",doseUnit_exchg,units$UNIT,fixed = TRUE)
  units$UNIT <- gsub("CONCUNIT",concUnit,units$UNIT,fixed = TRUE)
  units$UNIT <- gsub("TIMEUNIT",timeUnit,units$UNIT,fixed = TRUE)
  rDose <- c(  1,    1e3,    1e6,    1e9,  1e12)
  nDose <- c("g",   "mg",   "ug",   "ng",  "pg")
  ix <- which(sapply(nDose, function (x) grepl(paste0("\\<",x,"\\>"),doseUnit)))
  ratioDose <- rDose[ix]
  rConc <- c(    1,     1e3,     1e6,    1e9)
  nConc <- c("(g/L|mg/mL)",  "(mg/L|ug/mL)",  "(ug/L|ng/mL)",    "(ng/L|pg/mL)")
  ix <- which(sapply(nConc, function (x) grepl(paste0("\\<",x,"\\>"),concUnit)))
  ratioConc <- rConc[ix]
  FACTOR_CL_V <- ratioConc/ratioDose
  if (milliLiter) {
    CLEARANCE_UNIT <- paste0("mL/",timeUnit)
    VOLUME_UNIT <- "mL"
    FACTOR_CL_V <- FACTOR_CL_V*1000
  } else {
    CLEARANCE_UNIT <- paste0("L/",timeUnit)
    VOLUME_UNIT <- "L"
    FACTOR_CL_V <- FACTOR_CL_V
  }
  if (relativeDose) {
    CLEARANCE_UNIT <- paste0(CLEARANCE_UNIT, "/", doseUnit_rhs)
    VOLUME_UNIT <- paste0(VOLUME_UNIT, "/", doseUnit_rhs)
  }
  units$UNIT <- gsub("CLROUTINE",CLEARANCE_UNIT,units$UNIT,fixed = TRUE)
  units$UNIT <- gsub("VOLROUTINE",VOLUME_UNIT,units$UNIT,fixed = TRUE)
  vunits <- units$UNIT
  names(vunits) <- units$PKPARAMCD
  addunits <- c(DOSE=doseUnit,
                NDUR=timeUnit,ADUR=timeUnit,DUR=timeUnit,ATIME=timeUnit,NTIME=timeUnit,TIME=timeUnit,TAU=timeUnit,ATAFD=timeUnit,NTAFD=timeUnit,TAFD=timeUnit,
                ACONC=concUnit,CONC=concUnit,LLOQ=concUnit,CONCPLIN=concUnit,CONCPLOG=concUnit)
  namesadd <- c(paste0("AUCINT",1:9), paste0("AUCINT",1:9,"D"))
  valuesadd <- c(rep(vunits["AUCIFO"],9),rep(vunits["AUCIFOD"],9))
  names(valuesadd) <- namesadd
  vunits <- c(vunits,addunits,valuesadd)
  list(
    units = vunits,
    FACTOR_CL_V = FACTOR_CL_V,
    CLPARAM = units$PKPARAMCD[units$Unit.Calculation=="CLROUTINE"],
    VPARAM = units$PKPARAMCD[units$Unit.Calculation=="VOLROUTINE"]
  )
}
#' Check if object is an IQnca object
#'
#' @param input object to check
#' @return TRUE if it is an IQnca object. FALSE if it is not
#' @export
#'
is_IQnca <- function (input)
{
  methods::is(input, "IQnca")
}
#' Export an IQdataNCA object
#'
#' Essentially the dataset is exported as CSV with an attributes file.
#' All information is self contained in the CSV file ... so the attributes file at the moment
#' does not play a real role and is not produced upon export. .
#'
#' @param data IQdataNCA object
#' @param filename Filename with path for export
#' @export
#' @family NCA Data
export_IQdataNCA <- function (data,filename="nca_data") {
  IQRoutputCSV(data = data,filename = filename,FLAGattributes = TRUE,replaceComma = ";")
}
#' Export an IQdataNCA object to an ADNCA dataset
#'
#' General ADPC format cannot contain all the information needed for reproducing an NCA analysis.
#' We instead use an "ADNCA" format that contains all information in the data that is needed
#' for the reproduction of the NCA analysis. Standard file name is "adnca.xpt". The path can be chosen.
#' In addition to the adnca.xpt dataset a define.pdf file is generated, documenting the dataset format.
#'
#' @param data IQdataNCA object
#' @param pathname Path where to store the adnca.xpt dataset
#' @param addColLabels Named list. Names define column names and elements define labels for these columns - only used in generation of ADNCA
#' @export
#' @family NCA Data
exportADNCA_IQdataNCA <- function (data,pathname=".",addColLabels=NULL) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  message("Exporting NCA data to ADNCA ...")
  dataSpec <- getdataspec_IQdataNCA()
  removeColsSlope <- setdiff(getColumnNames_IQdataNCA()$slope,c("FLGSLOPE","SLOPETOL","SLOPEPT"))
  keepCols <- setdiff(names(data),removeColsSlope)
  data <- data[,keepCols]
  undefinedLabelColumns <- c()
  for (k in 1:ncol(data)) {
    colname <- names(data)[k]
    label <- "UNDEFINED"
    ix <- which(colname==dataSpec$Column)
    if (length(ix)==1) label <- dataSpec$Label[ix]
    ix <- which(colname==names(addColLabels))
    if (length(ix)==1) label <- addColLabels[[ix]]
    data <- addLabel(data = data,colName = colname,label = label)
    if (label=="UNDEFINED") undefinedLabelColumns <- c(undefinedLabelColumns,colname)
  }
  if (pathname=="") pathname <- "."
  filename <- file.path(pathname,"adnca.xpt")
  IQRoutputXPT(data = data,filename = filename,addColLabels = addColLabels)
  exportDEFINEdocx_IQdataNCA(data = data,
                             datasetName = "adnca",
                             datasetLocation = pathname,
                             datasetDescription = "NCA analysis dataset",
                             addColLabels = addColLabels,
                             filename = file.path(pathname,"define_adnca"))
  if (length(undefinedLabelColumns) > 0) {
    text <- paste0("Following columns have missing label information - consider specification of 'addColLabels': \n",paste0(undefinedLabelColumns,collapse=", "))
    warningIQR(text)
  }
}
exportDEFINEdocx_IQdataNCA <- function(data,
                                       datasetName,
                                       datasetLocation,
                                       datasetDescription,
                                       addColLabels,
                                       filename) {
  spec <- getdataspec_IQdataNCA()
  dataFirst__ <- data.frame(NAME=datasetName,DESCRIPTION=datasetDescription,LOCATION=datasetLocation,stringsAsFactors=FALSE,row.names=NULL)
  dataDefine__ <- data.frame(NAME=names(data),stringsAsFactors=FALSE,row.names=NULL)
  dataDefine__$TYPE <-lapply(data, function (x) { if (is.numeric(x)) { out <- "Numeric" } else { out <- "String" }; out })
  dataDefine__$LABEL <- sapply(dataDefine__$NAME, function (n) {
    out <- spec$Label[spec$Column==n]
    if (length(out) == 0) out <- "UNKNOWN"
    if (!is.null(addColLabels)) {
      ix <- which(names(addColLabels)==n)
      if (length(ix) == 1) {
        out <- addColLabels[[ix]]
      }
    }
    out
  })
  dataDefine__$DESCRIPTION <- sapply(dataDefine__$NAME, function (n) {
    out <- spec$Description[spec$Column==n]
    if (length(out) == 0) {
      out <- paste0("Values: ",paste0(unique(data[["PRANDIAL"]]),collapse = ", "))
    }
    out
  })
  datasetName__ <- toupper(datasetName)
  RMDTEXT__ <- rmdEMPTY()
  RMDTEXT__ <- RMDTEXT__ + rmdTITLE(title=paste0("Define file for the \"",datasetName__,"\" dataset"),subtitle=NULL,date=NULL)
  RMDTEXT__ <- RMDTEXT__ + rmdNOINTRO()
  RMDTEXT__ <- RMDTEXT__ + "!BLOCKSTART[keepNext](block_kwnext)\n"
  RMDTEXT__ <- RMDTEXT__ + rmdSECTION("Dataset name, description, and location",numbered=FALSE)
  RMDTEXT__ <- RMDTEXT__ + rmdTABLEDF(dataFirst__,label="overview",fontsize=8,caption="Dataset information",ignoreCaption=TRUE)
  RMDTEXT__ <- RMDTEXT__ + rmdNEWPAGE()
  RMDTEXT__ <- RMDTEXT__ + rmdSECTION(paste0(datasetName__," specification"),numbered=FALSE)
  RMDTEXT__ <- RMDTEXT__ + "Missing values are coded in the dataset as '.' and referenced in this specification as 'NA' or 'NC'.\n\n"
  RMDTEXT__ <- RMDTEXT__ + rmdTABLEDF(dataDefine__,label="overview",fontsize=8,caption="Definition of dataset contents",ignoreCaption=TRUE)
  RMDTEXT__ <- RMDTEXT__ + "\n"
  RMDTEXT__ <- RMDTEXT__ + "!BLOCKEND(block_kwnext)\n"
  x <- aux_fileparts(filename)
  filename__ <- paste0(x$pathname,"/",x$filename,".rmd")
  export_IQRrmd(RMDTEXT__,filename__)
  if (has_IQReport_executable()) {
    IQReport(filename__)
  }
}
#' Export an IQnca object
#'
#' Export the NCA results. Pivoted text and CSV format or long format as ADPP XPT dataset
#'
#' @param data IQnca object
#' @param filename Filename with path for export. No extension considered but added automatically.
#' @param SIGNIF Number of significant digits
#' @param TXT logical if TRUE then export as IQRoutputTable (pivoted).
#' @param CSV logical if TRUE then export as CSV file (pivoted).
#' @param parameterReport Character string defining the PK parameters to report in the listing.
#'   By default these parameters will be selected based on the "Standard" parameters defined in the IQparamNCA_Specification.xlsx
#'   document, subject to the profile and administration type. The user can select "All" or "Standard".
#'   Alternatively, parameterReport can also be a vector or parameter names (based on PKPARAMCD in IQparamNCA_Specification.xlsx).
#'   Instead of the standard or all ones then these will be considered in the listing (also subject to profile and
#'   administration type). Note that interval AUC (if calculated is always added by default!)
#' If defined it overrides the default labels.
#' @export
#' @family NCA Parameter Export
export_IQnca <- function (data,filename="nca_results",SIGNIF=8,TXT=FALSE,CSV=TRUE,parameterReport = "standard") {
  attrX <- attributes(data)
  dataNCA <- attrX$dataNCA
  intervalAUC <- attrX$intervalAUC
  units <- attrX$units
  if (!is_IQnca(data)) stopIQR("data is not an IQnca object")
  message("Exporting NCA results ...")
  info <- getValidityPKparam_IQRnca(data = data,parameterReport = parameterReport)
  pkparamInfo <- info$pkparamInfo
  userSelected <- info$userSelected
  covariates  <- c("USUBJID","PROFILE","GROUP","GROUPN","STUDYID","COMPOUND","ANALYTE","MATRIX","ADM","PERIOD","SEQUENCE","COUNTRY","SITEID","AGE","SEX","RACE")
  covariates  <- c(getaddcovcolumns_IQdataNCA(data),covariates)
  covdata     <- dplyr::distinct(data[, covariates, drop = FALSE])  
  pkparamInfo <- dplyr::left_join(pkparamInfo,covdata,by=c("USUBJID","PROFILE","GROUP"))
  filepath  <- gsub(basename(filename),"",filename)
  if (filepath=="") filepath = "."
  file <- aux_explode(basename(filename),"\\.")[1]
  pkparamInfo$VALUE <- signif(pkparamInfo$VALUE)
  if (TXT) exportTAB_IQnca(data = pkparamInfo,filename = file.path(filepath,file),units=units)
  if (CSV) exportCSV_IQnca(data=pkparamInfo,filename = file.path(filepath,file),units=units)
}
exportTAB_IQnca <- function (data,filename,units) {
  dataOrig <- data
  data <- data[,!names(data) %in% c("UNIT","NAME","DESCRIPTION","REASONNA","REASONNOTRELIABLE","IGNOREI","SPAN_OK","LAMZNPT_OK","R2ADJ_OK","AUCPEO_OK","AUCPEP_OK")]
  tab <- tidyr::spread(data = data,key=PARAMCD,value=VALUE)
  namesTab <- names(tab)
  for (k in seq_along(units)) {
    ix <- which(namesTab==names(units)[k])
    namesTab[ix] <- paste0(namesTab[ix]," [",units[k],"]")
  }
  names(tab) <- namesTab
  colNotNA <- sapply(1:ncol(tab), function (k) !all(is.na(tab[[k]])))
  tab <- tab[,colNotNA]
  IQRoutputTable(xtable = tab,filename = paste0(filename,".txt"))
  return(invisible(NULL))
}
exportCSV_IQnca <- function (data,filename,units) {
  dataOrig <- data
  data <- data[,!names(data) %in% c("UNIT","NAME","DESCRIPTION","REASONNA","REASONNOTRELIABLE","IGNOREI","SPAN_OK","LAMZNPT_OK","R2ADJ_OK","AUCPEO_OK","AUCPEP_OK")]
  tab <- tidyr::spread(data = data,key=PARAMCD,value=VALUE)
  colNotNA <- sapply(1:ncol(tab), function (k) !all(is.na(tab[[k]])))
  tab <- tab[,colNotNA]
  namesTab <- names(tab)
  row <- tab[1,]
  for (k in 1:ncol(row)) {
    row[[k]] <- "-"
    ix <- which(names(units)==namesTab[k])
    if (length(ix)==1) {
      row[[k]] <- units[ix]
    }
  }
  tab <- rbind(row,tab)
  IQRoutputCSV(data = tab,filename = paste0(filename,".csv"),replaceComma = ";")
  return(invisible(NULL))
}
#' Export an IQnca object to CDISC PP and ADPP
#'
#' Export the NCA results to PP and ADPP XPT files. Still somewhat exploratory but reasonable.
#' Default filenames pp.xpt and adpp.xpt will be used. Path can be defined.
#'
#' @param data IQnca object
#' @param pathname Path to which the export happens. Default filenames pp.xpt and adpp.xpt will be used. Path can be defined.
#' @param SIGNIF Number of significant digits
#' @param addColLabels Named list. Names define column names and elements define labels for these columns - only used in generation of ADNCA
#' @param parameterReport Character string defining the PK parameters to report in the listing.
#'   By default these parameters will be selected based on the "Standard" parameters defined in the IQparamNCA_Specification.xlsx
#'   document, subject to the profile and administration type. The user can select "All" or "Standard".
#'   Alternatively, parameterReport can also be a vector or parameter names (based on PKPARAMCD in IQparamNCA_Specification.xlsx).
#'   Instead of the standard or all ones then these will be considered in the listing (also subject to profile and
#'   administration type). Note that interval AUC (if calculated is always added by default!)
#' If defined it overrides the default labels.
#' @export
#' @family NCA Parameter Export
exportCDISC_IQnca <- function (data,pathname=".",SIGNIF=8,addColLabels=NULL,parameterReport = "standard") {
  attrX <- attributes(data)
  dataNCA <- attrX$dataNCA
  intervalAUC <- attrX$intervalAUC
  units <- attrX$units
  if (!is_IQnca(data)) stopIQR("data is not an IQnca object")
  data$USUBJID <- as.character(data$USUBJID)
  dataNCA$USUBJID <- as.character(dataNCA$USUBJID)
  message("Exporting NCA results to CDISC format ...")
  info <- getValidityPKparam_IQRnca(data = data,parameterReport = parameterReport)
  pkparamInfo <- info$pkparamInfo
  userSelected <- info$userSelected
  covariates  <- c("USUBJID","PROFILE","GROUP","GROUPN","GROUPU","STUDYID","COMPOUND","ANALYTE","MATRIX","ADM","PERIOD","SEQUENCE","COUNTRY","SITEID","AGE","SEX","RACE")
  covariates  <- c(getaddcovcolumns_IQdataNCA(data),covariates)
  covdata     <- dplyr::distinct(data[, covariates, drop = FALSE])  
  pkparamInfo <- dplyr::left_join(pkparamInfo,covdata,by=c("USUBJID","PROFILE","GROUP"))
  if (pathname=="") pathname = "."
  pkparamInfo$VALUE <- signif(pkparamInfo$VALUE)
  pp <- getPP_IQnca(data=pkparamInfo,dataNCA=dataNCA)
  ppfile <- file.path(pathname,"pp.xpt")
  IQRoutputXPT(data = pp,filename = ppfile,addColLabels = addColLabels)
  adpp <- getADPP_IQnca(pp=pp,dataNCA=dataNCA)
  adppfile <- file.path(pathname,"adpp.xpt")
  IQRoutputXPT(data = adpp,filename = adppfile,addColLabels = addColLabels)
}
getADPP_IQnca <- function (pp,dataNCA) {
  loadSetupOptions_IQnca()
  tab <- data.frame(
    STUDYID = pp$STUDYID,
    USUBJID = as.character(pp$USUBJID), 
    ASEQ = pp$PPSEQ,
    SRCDOM = "PP",
    SRCVAR = "PPSTRESC",
    SRCSEQ = pp$PPSEQ,
    AVISITN = pp$VISITNUM,
    AVISIT = pp$VISIT,
    PARAMCD = pp$PPTESTCD,
    PARAM = pp$PPTEST,
    AVAL = pp$PPSTRESN,
    AVALC = pp$PPSTRESC,
    AVALU = pp$PPSTRESU,
    EXSTDTC = pp$PPRFTDTC,
    PPSTAT = pp$PPSTAT,
    PPREASND = pp$PPREASND,
    PROFILE = as.character(pp$PROFILE),
    stringsAsFactors = FALSE
  )
  covariates <- c("USUBJID","PROFILE","GROUP","GROUPN","GROUPU","COMPOUND","ANALYTE","MATRIX","ADM","PERIOD","SEQUENCE","COUNTRY","SITEID","AGE","SEX","RACE")
  covariates <- c(getaddcovcolumns_IQdataNCA(dataNCA),covariates)
  covdata <- dplyr::distinct(dataNCA[, covariates, drop = FALSE])  
  colsNotNA <- sapply(1:ncol(covdata), function (k) !all(is.na(covdata[[k]])))
  covdata <- covdata[,colsNotNA]
  tab <- suppressWarnings(dplyr::left_join(tab,covdata,by=c("USUBJID","PROFILE")))
  tab <- addLabel(data = tab,colName = "STUDYID",label = "Study Identifier")
  tab <- addLabel(data = tab,colName = "USUBJID",label = "Unique Subject Identifier")
  tab <- addLabel(data = tab,colName = "ASEQ",label = "Sequence Number")
  tab <- addLabel(data = tab,colName = "SRCDOM",label = "Parameter Short Name")
  tab <- addLabel(data = tab,colName = "SRCVAR",label = "Parameter Name")
  tab <- addLabel(data = tab,colName = "SRCSEQ",label = "Parameter Category")
  tab <- addLabel(data = tab,colName = "AVISITN",label = "Result or finding in Original Units")
  tab <- addLabel(data = tab,colName = "AVISIT",label = "Original Units")
  tab <- addLabel(data = tab,colName = "PARAMCD",label = "Result or Finding in Standard Format")
  tab <- addLabel(data = tab,colName = "PARAM",label = "Numeric Result/Finding in Standard Format")
  tab <- addLabel(data = tab,colName = "AVAL",label = "Standard Units")
  tab <- addLabel(data = tab,colName = "AVALC",label = "Completion Status")
  tab <- addLabel(data = tab,colName = "AVALU",label = "Reason Parameter Not Calc/Unreliable")
  tab <- addLabel(data = tab,colName = "EXSTDTC",label = "Specimen Material Type")
  tab <- addLabel(data = tab,colName = "PPSTAT",label = "Visit Number")
  tab <- addLabel(data = tab,colName = "PPREASND",label = "Visit Name")
  tab <- addLabel(data = tab,colName = "PROFILE",label = "Date/Time Reference Point")
  tab <- addLabel(data = tab,colName = "GROUP",label = "Considered Profile")
  tab <- addLabel(data = tab,colName = "GROUPN",label = "Numeric treatment Group")
  tab <- addLabel(data = tab,colName = "GROUPU",label = "Unit Treatment Group")
  tab <- addLabel(data = tab,colName = "COMPOUND",label = "Unit Treatment Group")
  tab <- addLabel(data = tab,colName = "GROUPU",label = "Unit Treatment Group")
  tab <- addLabel(data = tab,colName = "ANALYTE",label = "Unit Treatment Group")
  tab <- addLabel(data = tab,colName = "MATRIX",label = "Unit Treatment Group")
  tab <- addLabel(data = tab,colName = "ADM",label = "Unit Treatment Group")
  tab <- addLabel(data = tab,colName = "PERIOD",label = "Period of Study")
  tab <- addLabel(data = tab,colName = "SEQUENCE",label = "Sequence of Treatment")
  tab <- addLabel(data = tab,colName = "COUNTRY",label = "Country Identifier")
  tab <- addLabel(data = tab,colName = "SITEID",label = "Site Identifier")
  tab <- addLabel(data = tab,colName = "AGE",label = "Age in years")
  tab <- addLabel(data = tab,colName = "SEX",label = "Sex")
  tab <- addLabel(data = tab,colName = "RACE",label = "Race")
  tab
}
getPP_IQnca <- function (data,dataNCA) {
  loadSetupOptions_IQnca()
  if (!is.null(dataNCA$EXSTDTC)) {
    data <- dplyr::left_join(data,unique(dataNCA[,c("USUBJID","PROFILE","PROFTYPE","GROUP","DOSE","EXSTDTC")]),by=c("USUBJID","PROFILE","PROFTYPE","GROUP","DOSE"))
    data$EXSTDTC[is.na(data$EXSTDTC)] <- "undefined"
  } else {
    data$EXSTDTC <- "undefined"
  }
  if (!is.null(dataNCA$VISIT)) {
    info <- unique(dataNCA[,c("USUBJID","PROFILE","PROFTYPE","GROUP","DOSE","VISIT")])
    info$split <- paste0(info$USUBJID,info$PROFILE,info$GROUP,info$DOSE)
    iS <- split(info,info$split)
    info <- do.call(rbind,lapply(iS,function (ik) ik[1,]))
    info$split <- NULL
    data <- dplyr::left_join(data,info,by=c("USUBJID","PROFILE","PROFTYPE","GROUP","DOSE"))
    data$VISIT[is.na(data$VISIT)] <- "undefined"
  } else {
    data$VISIT <- "undefined"
  }
  if (!is.null(dataNCA$VISITNUM)) {
    info <- unique(dataNCA[,c("USUBJID","PROFILE","PROFTYPE","GROUP","DOSE","VISITNUM")])
    info$split <- paste0(info$USUBJID,info$PROFILE,info$GROUP,info$DOSE)
    iS <- split(info,info$split)
    info <- do.call(rbind,lapply(iS,function (ik) ik[1,]))
    info$split <- NULL
    data <- dplyr::left_join(data,info,by=c("USUBJID","PROFILE","PROFTYPE","GROUP","DOSE"))
  } else {
    data$VISITNUM <- NA
  }
  tab <- data.frame(
    STUDYID = data$STUDYID,
    DOMAIN = "PP",
    USUBJID = data$USUBJID,
    PPSEQ = NA,
    PPTESTCD = data$PARAMCD,
    PPTEST = data$DESCRIPTION,
    PPCAT = "PKCONC",
    PPORRES = data$VALUE,
    PPORRESU = data$UNIT,
    PPSTRESC = as.character(data$VALUE),
    PPSTRESN = data$VALUE,
    PPSTRESU = data$UNIT,
    PPSTAT = NA,
    PPREASND = NA,
    PPSPEC = data$MATRIX,
    VISITNUM = data$VISITNUM,
    VISIT = data$VISIT,
    PPRFTDTC = data$EXSTDTC,
    PROFILE = data$PROFILE,
    REASONNA = data$REASONNA,
    REASONNOTRELIABLE = data$REASONNOTRELIABLE,
    IGNOREI = data$IGNOREI,
    SPAN_OK = data$SPAN_OK,
    LAMZNPT_OK = data$LAMZNPT_OK,
    R2ADJ_OK = data$R2ADJ_OK,
    AUCPEO_OK = data$AUCPEO_OK,
    AUCPEP_OK = data$AUCPEP_OK,
    stringsAsFactors = FALSE
  )
  tab$PPSEQ <- match(tab$PPTESTCD,unique(tab$PPTESTCD))
  ixreasonNA <- !nchar(aux_strtrim(tab$REASONNA))==0
  tab$PPORRES[ixreasonNA] <- NA
  tab$PPORRESU[ixreasonNA] <- NA
  tab$PPSTRESC[ixreasonNA] <- NA
  tab$PPSTRESN[ixreasonNA] <- NA
  tab$PPSTRESU[ixreasonNA] <- NA
  tab$PPSTAT[ixreasonNA] <- "NOT DONE"
  REASONNA <- rep("",nrow(tab))
  ix <- grepl(.footnoteChar_LAMZ_NA,tab$REASONNA,fixed = TRUE)
  REASONNA[ix] <- paste0(REASONNA[ix],"Terminal slope could not be determined; ")
  ix <- grepl(.footnoteChar_DOSE0_NA,tab$REASONNA,fixed = TRUE)
  REASONNA[ix] <- paste0(REASONNA[ix],"Dose 0; ")
  ix <- grepl(.footnoteChar_ISSUE_UNCAUGHT_NA,tab$REASONNA,fixed = TRUE)
  REASONNA[ix] <- paste0(REASONNA[ix],"Could not be determined; ")
  REASONNA[nchar(REASONNA)==0] <- as.character(NA)
  tab$PPREASND <- REASONNA
  REASONNOTRELIABLE <- rep("",nrow(tab))
  ix <- grepl(.footnoteChar_SPAN_LOW,tab$REASONNOTRELIABLE,fixed = TRUE) & !is.na(tab$PPORRES)
  REASONNOTRELIABLE[ix] <- paste0(REASONNOTRELIABLE[ix],"SPAN < ",.SPAN_MIN,"; ")
  ix <- grepl(.footnoteChar_LAMZNPT_LOW,tab$REASONNOTRELIABLE,fixed = TRUE) & !is.na(tab$PPORRES)
  REASONNOTRELIABLE[ix] <- paste0(REASONNOTRELIABLE[ix],"Number slope points < ",.LAMZNPT_MIN,"; ")
  ix <- grepl(.footnoteChar_R2ADJ_LOW,tab$REASONNOTRELIABLE,fixed = TRUE) & !is.na(tab$PPORRES)
  REASONNOTRELIABLE[ix] <- paste0(REASONNOTRELIABLE[ix],"R2ADJ < ",.R2ADJ_MIN,"; ")
  ix <- (grepl(.footnoteChar_AUCOEXTR_HIGH,tab$REASONNOTRELIABLE,fixed = TRUE) | grepl(.footnoteChar_AUCPEXTR_HIGH,tab$REASONNOTRELIABLE,fixed = TRUE) ) & !is.na(tab$PPORRES)
  REASONNOTRELIABLE[ix] <- paste0(REASONNOTRELIABLE[ix],"AUC extrapolation > ",.AUCEXTRAP_MAX,"%; ")
  REASONNOTRELIABLE[nchar(REASONNOTRELIABLE)==0] <- as.character(NA)
  ixeligible <- is.na(tab$PPREASND)
  ixnotreliable <- ixeligible & !is.na(REASONNOTRELIABLE)
  tab$PPSTAT[ixnotreliable] <- "NOT RELIABLE"
  tab$PPREASND[ixnotreliable] <- REASONNOTRELIABLE[ixnotreliable]
  ixIGNOREI <- !is.na(tab$IGNOREI)
  tab$PPREASND[ixIGNOREI] <- paste0("Subject ignored due to: ",tab$IGNOREI[ixIGNOREI])
  tab <- tab[,!names(tab) %in% c("REASONNA","REASONNOTRELIABLE","IGNOREI","SPAN_OK","LAMZNPT_OK","R2ADJ_OK","AUCPEO_OK","AUCPEP_OK")]
  tab <- addLabel(data = tab,colName = "STUDYID",label = "Study Identifier")
  tab <- addLabel(data = tab,colName = "DOMAIN",label = "Domain Abbreviation")
  tab <- addLabel(data = tab,colName = "USUBJID",label = "Unique Subject Identifier")
  tab <- addLabel(data = tab,colName = "PPSEQ",label = "Sequence Number")
  tab <- addLabel(data = tab,colName = "PPTESTCD",label = "Parameter Short Name")
  tab <- addLabel(data = tab,colName = "PPTEST",label = "Parameter Name")
  tab <- addLabel(data = tab,colName = "PPCAT",label = "Parameter Category")
  tab <- addLabel(data = tab,colName = "PPORRES",label = "Result or finding in Original Units")
  tab <- addLabel(data = tab,colName = "PPORRESU",label = "Original Units")
  tab <- addLabel(data = tab,colName = "PPSTRESC",label = "Result or Finding in Standard Format")
  tab <- addLabel(data = tab,colName = "PPSTRESN",label = "Numeric Result/Finding in Standard Format")
  tab <- addLabel(data = tab,colName = "PPSTRESU",label = "Standard Units")
  tab <- addLabel(data = tab,colName = "PPSTAT",label = "Completion Status")
  tab <- addLabel(data = tab,colName = "PPREASND",label = "Reason Parameter Not Calc/Unreliable")
  tab <- addLabel(data = tab,colName = "PPSPEC",label = "Specimen Material Type")
  tab <- addLabel(data = tab,colName = "VISITNUM",label = "Visit Number")
  tab <- addLabel(data = tab,colName = "VISIT",label = "Visit Name")
  tab <- addLabel(data = tab,colName = "PPRFTDTC",label = "Date/Time Reference Point")
  tab <- addLabel(data = tab,colName = "PROFILE",label = "Considered Profile")
  tab
}
addLabel <- function (data,colName,label) {
  if (colName %in% names(data)) {
    attr(data[[colName]],"label") <- label
    attr(data[[colName]],"class") <- unique(c("labelled",class(data[[colName]])))
  }
  return(data)
}
#' Simple pragmatic comparison function for IQnca and Winnonlin results
#'
#' @param resIQR result from IQnca
#' @param WNLresFile Pivoted WinNonlin parameter file saved as CSV
#' @param TOL allowed relative difference
#' @param mapAUCINTnames named vector mapping IQR AUCINT column names to Winnonlin col names
#' @export
compareIQRWNL_IQnca <- function (resIQR,WNLresFile,TOL=1e-10,mapAUCINTnames=NULL) {
  WinNonlinResult <- utils::read.csv(WNLresFile,stringsAsFactors = FALSE)
  if (is.character(WinNonlinResult$AUCall)) {
    WinNonlinResult <- WinNonlinResult[-1,]
  }
  temp <- paste0(tempfile(),".csv")
  IQRsaveCSVdata(WinNonlinResult,temp)
  WinNonlinResult <- IQRloadCSVdata(temp)
  unlink(temp,force = TRUE)
  if ("Subject" %in% names(WinNonlinResult)) {
    names(WinNonlinResult)[names(WinNonlinResult) == 'Subject'] <- 'USUBJID'
  } else {
    names(WinNonlinResult)[names(WinNonlinResult) == 'ID'] <- 'USUBJID'
  }
  if (resIQR$ADM[1] %in% "EXTRAVASCULAR"){
    names(WinNonlinResult)[names(WinNonlinResult) == 'MRTlast'] <- 'MRTEVLST'
    names(WinNonlinResult)[names(WinNonlinResult) == 'MRTINF_obs'] <- 'MRTEVIFO'
    names(WinNonlinResult)[names(WinNonlinResult) == 'MRTINF_pred'] <- 'MRTEVIFP'
  } else if (resIQR$ADM[1] %in% c("BOLUS","INFUSION")){
    names(WinNonlinResult)[names(WinNonlinResult) == 'MRTlast'] <- 'MRTIVLST'
    names(WinNonlinResult)[names(WinNonlinResult) == 'MRTINF_obs'] <- 'MRTIVIFO'
    names(WinNonlinResult)[names(WinNonlinResult) == 'MRTINF_pred'] <- 'MRTIVIFP'
  }
  names(WinNonlinResult)[names(WinNonlinResult) == 'Lambda_z_intercept'] <- 'LAMZICPT'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Tlag'] <- 'TLAG'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Vz_F_obs'] <- 'VZFO'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Vz_F_pred'] <- 'VZFP'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Cl_F_obs'] <- 'CLFO'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Cl_F_pred'] <- 'CLFP'
  names(WinNonlinResult)[names(WinNonlinResult) == 'C0'] <- 'C0'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUC_.Back_Ext_obs'] <- 'AUCPBEO'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUC_.Back_Ext_pred'] <- 'AUCPBEP'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Cmax'] <- 'CMAX'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Cmax_D'] <- 'CMAXD'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Tmax'] <- 'TMAX'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Tmin'] <- 'TMIN'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Cmin'] <- 'CMIN'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Clast'] <- 'CLST'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Tlast'] <- 'TLST'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Clast_pred'] <- 'CLSTP'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Ctau'] <- 'CTAU'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Cavg'] <- 'CAVG'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Swing'] <- 'SWING'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Swing_Tau'] <- 'SWINGTAU'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Fluctuation.'] <- 'FLUCP'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Fluctuation._Tau'] <- 'FLUCPTAU'
  names(WinNonlinResult)[names(WinNonlinResult) == 'HL_Lambda_z'] <- 'LAMZHL'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Lambda_z'] <- 'LAMZ'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Lambda_z_lower'] <- 'LAMZLL'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Lambda_z_upper'] <- 'LAMZUL'
  names(WinNonlinResult)[names(WinNonlinResult) == 'No_points_lambda_z'] <- 'LAMZNPT'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Corr_XY'] <- 'CORRXY'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Rsq'] <- 'R2'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Rsq_adjusted'] <- 'R2ADJ'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Span'] <- 'SPAN'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUClast'] <- 'AUCLST'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUClast_D'] <- 'AUCLSTD'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUCall'] <- 'AUCALL'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUCINF_obs'] <- 'AUCIFO'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUCINF_D_obs'] <- 'AUCIFOD'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUC_.Extrap_obs'] <- 'AUCPEO'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUCINF_pred'] <- 'AUCIFP'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUCINF_D_pred'] <- 'AUCIFPD'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUC_.Extrap_pred'] <- 'AUCPEP'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUMClast'] <- 'AUMCLST'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUMCINF_obs'] <- 'AUMCIFO'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUMC_.Extrap_obs'] <- 'AUMCPEO'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUMCINF_pred'] <- 'AUMCIFP'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUMC_.Extrap_pred'] <- 'AUMCPEP'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Vz_obs'] <- 'VZO'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Vz_pred'] <- 'VZP'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Cl_obs'] <- 'CLO'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Cl_pred'] <- 'CLP'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Vss_obs'] <- 'VSSO'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Vss_pred'] <- 'VSSP'
  names(WinNonlinResult)[names(WinNonlinResult) == 'CLss'] <- 'CLSS'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Clss_F_'] <- 'CLFSS'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Vz'] <- 'VZSS'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Vz_F_'] <- 'VZFSS'
  names(WinNonlinResult)[names(WinNonlinResult) == 'Accumulation.Index'] <- 'AILAMZ'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUC_TAU'] <- 'AUCTAU'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUC_TAU_D'] <- 'AUCTAUD'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUMC_TAU'] <- 'AUMCTAU'
  names(WinNonlinResult)[names(WinNonlinResult) == 'AUClower_upper'] <- 'AUCINTX'
  for (k in seq_along(mapAUCINTnames)) {
    nameIQR <- names(mapAUCINTnames)[k]
    nameWNL1 <- gsub("-",".",mapAUCINTnames[k])
    names(WinNonlinResult)[names(WinNonlinResult) == nameWNL1] <- nameIQR
  }
  resIQR <- as.data.frame(resIQR)
  resIQR$USUBJID <- as.numeric(resIQR$USUBJID)
  resIQR <- dplyr::arrange(resIQR,USUBJID)
  WinNonlinResult$USUBJID <- as.numeric(WinNonlinResult$USUBJID)
  WinNonlinResult <- dplyr::arrange(WinNonlinResult,USUBJID)
  resIQR <- resIQR[,sapply(resIQR, is.numeric)]
  WinNonlinResult <- WinNonlinResult[,sapply(WinNonlinResult, is.numeric)]
  params_names <-intersect(names(WinNonlinResult),names(resIQR))
  params_names <- params_names[!params_names %in% "USUBJID"]
  comparison_empty <- data.frame(matrix(ncol = 3, nrow = length(params_names)))
  names(comparison_empty) <-c("VALUE_IQR","VALUE_WNL","DIFF")
  rownames(comparison_empty) <- params_names
  per_subject <- list()
  count <- 0
  for (ks in 1:nrow(resIQR)) {
    comparison <- comparison_empty
    for (k in params_names) {
      tst <- resIQR[[k]]
      ref <- WinNonlinResult[[k]]
      comparison$VALUE_IQR[rownames(comparison) == k] <- tst[ks]
      comparison$VALUE_WNL[rownames(comparison) == k] <- ref[ks]
    }
    comparison$DIFF <- NA
    comparison <- comparison[!is.na(comparison$VALUE_WNL),]
    comparison <- comparison[!is.na(comparison$VALUE_IQR),]
    comparison$DIFF[comparison$VALUE_WNL==0] <- abs(comparison$VALUE_IQR[comparison$VALUE_WNL==0] - comparison$VALUE_WNL[comparison$VALUE_WNL==0])
    comparison$DIFF[comparison$VALUE_WNL!=0] <- abs(comparison$VALUE_IQR[comparison$VALUE_WNL!=0] - comparison$VALUE_WNL[comparison$VALUE_WNL!=0])/comparison$VALUE_WNL[comparison$VALUE_WNL!=0]
    comparison$DIFF[abs(comparison$DIFF)<TOL] <- 0
    comparison <- comparison[!is.na(comparison$VALUE_IQR),]
    if (sum(abs(comparison$DIFF)) > 0) {
      count <- count + 1
      print(paste("USUBJID", resIQR$USUBJID[ks]))
      print(comparison)
    }
    per_subject[[ks]] <- comparison
  }
  return(paste("Issues in",count,"cases."))
}
getValidityPKparam_IQRnca <- function (data,parameterReport="standard") {
  loadSetupOptions_IQnca()
  paramspec <- getparamspec_IQdataNCA(data)
  paramspec <- paramspec[,c("PKPARAMCD", "PKPARAM", "Name","Reporting","SD","FD","SS","BOLUS","INFUSION","EXTRAVASCULAR")]
  if (length(parameterReport)==1) {
    if (!tolower(parameterReport) %in% c("all","standard")) stopIQR("parameterReport needs to be 'standard', 'all', or a character vector with the PKPARAMCD names of the parameters to report")
    if (parameterReport=="standard") {
      paramspec <- paramspec[paramspec$Reporting=="standard",]
    }
    userSelected <- FALSE
  } else {
    parameterReport <- parameterReport[!grepl("AUCINT",parameterReport)]
    wrong <- parameterReport[!parameterReport %in% paramspec$PKPARAMCD]
    if (length(wrong) > 0) stopIQR("The following parameters in parameterReport are not available in the calculated parameters (typo?):\n  ", paste0(wrong,collapse = ", "))
    paramspec <- paramspec[paramspec$PKPARAMCD %in% parameterReport,]
    userSelected <- TRUE
  }
  paramspecReport <- paramspec
  data$SPLIT <- paste0(data$USUBJID,"-",data$PROFILE)
  dS <- split(data,data$SPLIT)
  pkparamALL <- do.call(rbind,lapply(seq_along(dS), function (k) {
    d <- dS[[k]]
    attr <- attributes(d)
    units <- attr$units
    info <- attr$dataNCA[attr$dataNCA$USUBJID==d$USUBJID[1],]
    reportPT <- !is.na(paramspecReport[[d$PROFTYPE[1]]])
    reportADM <- !is.na(paramspecReport[[d$ADM[1]]])
    report <- reportPT & reportADM
    paramspecReportHere <- paramspecReport[report,]
    paramspecReportHere <- paramspecReportHere[!grepl("AUCINTX",paramspecReportHere$PKPARAMCD),]
    paramspecReportHere <- paramspecReportHere[,c("PKPARAMCD","PKPARAM","Name")]
    iaucinfo <- attributes(d)$intervalAUC
    if (length(iaucinfo)>0) {
      paramspecAUCINT <- do.call(rbind,lapply(iaucinfo, function (x) {
        data.frame(
          PKPARAMCD = x$colname,
          PKPARAM = paste0("Interval AUC [",x$tstart,"-",x$tend," ",getTIMEUNITsymbol_IQdataNCA(d),"]"),
          Name = paste0("Interval AUC [",x$tstart,"-",x$tend," ",getTIMEUNITsymbol_IQdataNCA(d),"]"),
          stringsAsFactors = FALSE
        )
      }))
      paramspecAUCINTD <- do.call(rbind,lapply(iaucinfo, function (x) {
        data.frame(
          PKPARAMCD = paste0(x$colname,"D"),
          PKPARAM = paste0("Interval AUC [",x$tstart,"-",x$tend," ",getTIMEUNITsymbol_IQdataNCA(d),"] by Dose"),
          Name = paste0("Interval AUC [",x$tstart,"-",x$tend," ",getTIMEUNITsymbol_IQdataNCA(d),"] by Dose"),
          stringsAsFactors = FALSE
        )
      }))
    } else {
      paramspecAUCINT <- NULL
      paramspecAUCINTD <- NULL
    }
    paramspecReportHere <- rbind(paramspecReportHere,paramspecAUCINT,paramspecAUCINTD)
    param <- as.data.frame(d[,paramspecReportHere$PKPARAMCD])
    rownames(param) <- NULL
    tabparam <- data.frame(
      PARAMCD = names(param),
      VALUE = as.numeric(as.vector(param)),
      stringsAsFactors = FALSE
    )
    tabparam$UNIT <- sapply(tabparam$PARAMCD, function (p) units[p])
    tabparam$NAME <- sapply(tabparam$PARAMCD, function (p) paramspecReportHere$Name[paramspecReportHere$PKPARAMCD==p])
    tabparam$DESCRIPTION <- sapply(tabparam$PARAMCD, function (p) paramspecReportHere$PKPARAM[paramspecReportHere$PKPARAMCD==p])
    tabparam$REASONNA <- ""
    tabparam$REASONNOTRELIABLE <- ""
    tabparam <- data.frame(USUBJID=d$USUBJID[1],PROFILE=d$PROFILE[1],PROFTYPE=d$PROFTYPE[1],GROUP=d$GROUP[1],DOSE=d$DOSE[1],DOSEUNIT=info$DOSEUNIT[1],tabparam,stringsAsFactors = FALSE)
    tabparam$IGNOREI <- info$IGNOREI[1]
    tabparam$SLOPE_OK <- TRUE
    if (is.na(d$LAMZ)) tabparam$SLOPE_OK <- FALSE
    tabparam$DOSEZERO <- FALSE
    if (d$DOSE==0) tabparam$DOSEZERO <- TRUE
    if (is.na(tabparam$IGNOREI[1])) {
      if (!tabparam$SLOPE_OK[1]) {
        reason <- .footnoteChar_LAMZ_NA 
        affectedparam <- .affectedparam_LAMZ_NA
        tabparam$REASONNA[tabparam$PARAMCD %in% affectedparam & is.na(tabparam$VALUE)] <- paste(tabparam$REASONNA[tabparam$PARAMCD %in% affectedparam & is.na(tabparam$VALUE)],reason)
      }
      if (tabparam$DOSEZERO[1]) {
        reason <- .footnoteChar_DOSE0_NA 
        affectedparam <- .affectedparam_DOSE0_NA
        tabparam$REASONNA[tabparam$PARAMCD %in% affectedparam & is.na(tabparam$VALUE)] <- paste( tabparam$REASONNA[tabparam$PARAMCD %in% affectedparam & is.na(tabparam$VALUE)],reason)
      }
    }
    tabparam$SLOPE_OK <- NULL
    tabparam$DOSEZERO <- NULL
    tabparam$SPAN_OK <- TRUE
    tabparam$LAMZNPT_OK <- TRUE
    tabparam$R2ADJ_OK <- TRUE
    tabparam$AUCPEO_OK <- TRUE
    tabparam$AUCPEP_OK <- TRUE
    if (is.na(tabparam$IGNOREI[1])) {
      if (is.na(d$SPAN) | d$SPAN < .SPAN_MIN) tabparam$SPAN_OK <- FALSE
      if (is.na(d$LAMZNPT) | d$LAMZNPT < .LAMZNPT_MIN) tabparam$LAMZNPT_OK <- FALSE
      if (is.na(d$R2ADJ) | d$R2ADJ < .R2ADJ_MIN) tabparam$R2ADJ_OK <- FALSE
      if (is.na(d$AUCPEO) | d$AUCPEO > .AUCEXTRAP_MAX) tabparam$AUCPEO_OK <- FALSE
      if (is.na(d$AUCPEP) | d$AUCPEP > .AUCEXTRAP_MAX) tabparam$AUCPEP_OK <- FALSE
      if (d$ADM=="BOLUS") {
        if (is.na(d$AUCPBEO) | d$AUCPBEO > .AUCEXTRAP_MAX) tabparam$AUCPEO_OK <- FALSE
        if (is.na(d$AUCPBEP) | d$AUCPBEP > .AUCEXTRAP_MAX) tabparam$AUCPEP_OK <- FALSE
      }
      if (!tabparam$AUCPEO_OK[1]) {
        affectedparam <- .affectedparam_AUCOEXTR_HIGH
        tabparam$AUCPEO_OK <- TRUE
        tabparam$AUCPEO_OK[tabparam$PARAMCD %in% affectedparam] <- FALSE
      }
      if (!tabparam$AUCPEP_OK[1]) {
        affectedparam <- .affectedparam_AUCPEXTR_HIGH
        tabparam$AUCPEP_OK <- TRUE
        tabparam$AUCPEP_OK[tabparam$PARAMCD %in% affectedparam] <- FALSE
      }
      if (!tabparam$SPAN_OK[1]) {
        affectedparam <- .affectedparam_SPAN_LOW
        tabparam$SPAN_OK <- TRUE
        tabparam$SPAN_OK[tabparam$PARAMCD %in% affectedparam] <- FALSE
      }
      if (!tabparam$LAMZNPT_OK[1]) {
        affectedparam <- .affectedparam_LAMZNPT_LOW
        tabparam$LAMZNPT_OK <- TRUE
        tabparam$LAMZNPT_OK[tabparam$PARAMCD %in% affectedparam] <- FALSE
      }
      if (!tabparam$R2ADJ_OK[1]) {
        affectedparam <- .affectedparam_R2ADJ_LOW
        tabparam$R2ADJ_OK <- TRUE
        tabparam$R2ADJ_OK[tabparam$PARAMCD %in% affectedparam] <- FALSE
      }
      tabparam$REASONNOTRELIABLE[!tabparam$SPAN_OK] <- paste(tabparam$REASONNOTRELIABLE[!tabparam$SPAN_OK],.footnoteChar_SPAN_LOW)
      tabparam$REASONNOTRELIABLE[!tabparam$LAMZNPT_OK] <- paste(tabparam$REASONNOTRELIABLE[!tabparam$LAMZNPT_OK],.footnoteChar_LAMZNPT_LOW)
      tabparam$REASONNOTRELIABLE[!tabparam$R2ADJ_OK] <- paste(tabparam$REASONNOTRELIABLE[!tabparam$R2ADJ_OK],.footnoteChar_R2ADJ_LOW)
      tabparam$REASONNOTRELIABLE[!tabparam$AUCPEO_OK] <- paste(tabparam$REASONNOTRELIABLE[!tabparam$AUCPEO_OK],.footnoteChar_AUCOEXTR_HIGH)
      tabparam$REASONNOTRELIABLE[!tabparam$AUCPEP_OK] <- paste(tabparam$REASONNOTRELIABLE[!tabparam$AUCPEP_OK],.footnoteChar_AUCPEXTR_HIGH)
      ix <- is.na(tabparam$VALUE) & is.na(tabparam$IGNOREI) & (nchar(tabparam$REASONNA) == 0) & (nchar(tabparam$REASONNOTRELIABLE) == 0)
      tabparam$REASONNA[ix] <- paste(tabparam$REASONNOTRELIABLE[ix],.footnoteChar_ISSUE_UNCAUGHT_NA)
    } else {
      tabparam$REASONNA <- .footnoteChar_IGNOREDSUBJECT_NA
    }
    tabparam
  }))
  attributes(pkparamALL)$dataNCA <- attributes(data)$dataNCA
  list(pkparamInfo=pkparamALL,userSelected=userSelected)
}
#' Generate a listing of calculated PK parameters
#'
#' The listing will be generated already as an IQRrmd object, allowing easy reporting in Word with IQReport.
#' The listing is done per USUBJID and PROFILE.
#'
#' @param data IQnca object
#' @param listingnumber Character string with listing number information, added to the title for each table, if defined
#' @param fontsizetable Fontsize to be used in the table
#' @param filename Filename to export the listing IQRrmd object to
#' @param SIGNIF Number of significant digits
#' @param parameterReport Character string defining the PK parameters to report in the listing.
#'   By default these parameters will be selected based on the "Standard" parameters defined in the IQparamNCA_Specification.xlsx
#'   document, subject to the profile and administration type. The user can select "All" or "Standard".
#'   Alternatively, parameterReport can also be a vector or parameter names (based on PKPARAMCD in IQparamNCA_Specification.xlsx).
#'   Instead of the standard or all ones then these will be considered in the listing (also subject to profile and
#'   administration type). Note that interval AUC (if calculated is always added by default!)
#' @return An IQRrmd object exported to a file
#' @export
#' @family NCA Parameter Listings
listing_pkparameter_IQnca <- function (data,listingnumber=NULL,fontsizetable=8,filename="listing_pkparam",SIGNIF=6,parameterReport="standard") {
  if (!is_IQnca(data)) stopIQR("data is not an IQnca object")
  message("Generating PK parameter listings ...")
  info <- getValidityPKparam_IQRnca(data = data,parameterReport = parameterReport)
  pkparamInfo <- info$pkparamInfo
  userSelected <- info$userSelected
  if (!is.null(filename)) {
    filename <- paste0(gsub(".rmd","",filename),".rmd")
  }
  loadSetupOptions_IQnca()
  text <- rmdEMPTY() +
    rmdLANDSCAPE() +
    rmdSECTION(title = updateListingNumberTitle_IQdataNCA(.listing_pkparameter_pkconc,listingnumber,NULL),
               level = 1,numbered = FALSE)
  table <- list()
  pkparamInfo$SPLIT <- paste0(pkparamInfo$USUBJID,"-",pkparamInfo$PROFILE)
  allSPLIT <- unique(pkparamInfo$SPLIT)
  for (k in seq_along(allSPLIT)) {
    d <- pkparamInfo[pkparamInfo$SPLIT==allSPLIT[k],]
    object <- listingindivPKparam_IQnca(d = d,listingindex = k,listingnumber = listingnumber,fontsizetable = fontsizetable,filename = filename,SIGNIF=SIGNIF,userSelected=userSelected)
    if ("IQRrmd" %in% class(object)) {
      textindiv <- object
      if (k==1) {
        text <- text + textindiv + "\n"
      } else {
        text <- text + rmdNEWPAGE() + "\n" + textindiv + "\n"
      }
    }
    if ("IQRoutputTable" %in% class(object)) {
      table <- c(table, structure(list(object), names = allSPLIT[k]))
    }
  }
  if (!is.null(filename)) {
    export_IQRrmd(text,filename)
  } else {
    return(table)
  }
}
listingindivPKparam_IQnca <- function (d,listingindex,listingnumber=NULL,fontsizetable=8,filename,SIGNIF,userSelected) {
  loadSetupOptions_IQnca()
  attr <- attributes(d)
  info <- attr$dataNCA[attr$dataNCA$USUBJID==d$USUBJID[1],]
  tabparam <- data.frame(
    PROFILE = d$PROFILE,
    GROUP = d$GROUP,
    DOSE = d$DOSE,
    DESCRIPTION = d$DESCRIPTION,
    PARAM = paste0(d$PARAMCD," (",d$UNIT,")"),
    VALUE = {
      out <- signif(d$VALUE,SIGNIF)
      out <- as.character(out)
      out[is.na(out)] <- "NC"
      out
    },
    REASONNA = aux_strtrim(d$REASONNA),
    REASONNOTRELIABLE = aux_strtrim(d$REASONNOTRELIABLE),
    stringsAsFactors = FALSE
  )
  ix_reason_NA <- nchar(tabparam$REASONNA) > 0
  tabparam$VALUE[ix_reason_NA]  <- paste0(tabparam$VALUE[ix_reason_NA]," ^",gsub(" ","^ ^",tabparam$REASONNA[ix_reason_NA],fixed = TRUE),"^")
  tabparam$REASONNOTRELIABLE[nchar(tabparam$REASONNA) > 0] <- ""
  ix_reason_NR <- nchar(tabparam$REASONNOTRELIABLE) > 0
  tabparam$VALUE[ix_reason_NR]  <- paste0(tabparam$VALUE[ix_reason_NR]," ^",gsub(" ","^ ^",tabparam$REASONNOTRELIABLE[ix_reason_NR],fixed = TRUE),"^")
  scriptName <- getScriptName_IQdataNCA()
  footertext <- paste0(
    "NC: Not calculated.\n",
    paste0("Number of significant digits: ",SIGNIF,"\n"),
    ifelse (any(grepl(.footnoteChar_LAMZ_NA,tabparam$REASONNA,fixed = TRUE)), paste0("^",.footnoteChar_LAMZ_NA,"^"," ","Value not calculated. Reason: terminal slope could not be determined. Value was not considered for summary and inferential procedures.\n") ,""),
    ifelse (any(grepl(.footnoteChar_DOSE0_NA,tabparam$REASONNA,fixed = TRUE)), paste0("^",.footnoteChar_DOSE0_NA,"^"," ","Value not calculated. Reason: dose was 0. Value was not considered for summary and inferential procedures.\n") ,""),
    ifelse (any(grepl(.footnoteChar_ISSUE_UNCAUGHT_NA,tabparam$REASONNA,fixed = TRUE)), paste0("^",.footnoteChar_ISSUE_UNCAUGHT_NA,"^"," ","Value not calculated. Reason: undefined. Value was not considered for summary and inferential procedures.\n") ,""),
    ifelse (any(grepl(.footnoteChar_IGNOREDSUBJECT_NA,tabparam$REASONNA,fixed = TRUE)), paste0("^",.footnoteChar_IGNOREDSUBJECT_NA,"^"," ","Value not calculated. Reason: Subject ignored with reason '",d$IGNOREI[1],"'.\n") ,""),
    ifelse (any(grepl(.footnoteChar_SPAN_LOW,tabparam$REASONNOTRELIABLE,fixed = TRUE)), paste0("^",.footnoteChar_SPAN_LOW,"^"," ","Value not reliably calculated. Reason: SPAN<",.SPAN_MIN,". Value was not considered for summary and inferential procedures.\n") ,""),
    ifelse (any(grepl(.footnoteChar_LAMZNPT_LOW,tabparam$REASONNOTRELIABLE,fixed = TRUE)), paste0("^",.footnoteChar_LAMZNPT_LOW,"^"," ","Value not reliably calculated. Reason: LAMZNPT<",.LAMZNPT_MIN,". Value was not considered for summary and inferential procedures.\n") ,""),
    ifelse (any(grepl(.footnoteChar_R2ADJ_LOW,tabparam$REASONNOTRELIABLE,fixed = TRUE)), paste0("^",.footnoteChar_R2ADJ_LOW,"^"," ","Value not reliably calculated. Reason: R2ADJ<",.R2ADJ_MIN,". Value was not considered for summary and inferential procedures.\n") ,""),
    ifelse (any(grepl(.footnoteChar_AUCOEXTR_HIGH,tabparam$REASONNOTRELIABLE,fixed = TRUE)), paste0("^",.footnoteChar_AUCOEXTR_HIGH,"^"," ","Value not reliably calculated. Reason: extrapolated AUC (observed)>",.AUCEXTRAP_MAX,"%. Value was not considered for summary and inferential procedures.\n") ,""),
    ifelse (any(grepl(.footnoteChar_AUCPEXTR_HIGH,tabparam$REASONNOTRELIABLE,fixed = TRUE)), paste0("^",.footnoteChar_AUCPEXTR_HIGH,"^"," ","Value not reliably calculated. Reason: extrapolated AUC (predicted)>",.AUCEXTRAP_MAX,"%. Value was not considered for summary and inferential procedures.\n") ,""),
    "IQnca version: ", aux_version(pkgName = "IQnca"), "\n",
    "Script: ", scriptName,"\n",
    "Output: ",filename, "\n",
    "Execution date: ", Sys.time()
  )
  tabparam$REASONNA <- NULL
  tabparam$REASONNOTRELIABLE <- NULL
  tabparam$PROFILE[duplicated(tabparam$PROFILE)] <- ""
  tabparam$GROUP[duplicated(tabparam$GROUP)] <- ""
  tabparam$DOSE[duplicated(tabparam$DOSE)] <- ""
  names(tabparam) <- c(
    "Profile",
    paste0("Group"),
    paste0("Dose [",d$DOSEUNIT[1],"]"),
    "Description",
    paste0("Parameter (Unit)"),
    paste0("Value")
  )
  text <- rmdEMPTY()
  loadSetupOptions_IQnca()
  title <- .listing_pkparameter_pkconc
  title <- updateListingNumberTitle_IQdataNCA(title,listingnumber,listingindex)
  title <- paste0(title," of ",info$MATRIX[1]," ",info$COMPOUND[1], " (",info$ANALYTE[1],")")
  blockIdentifier <- paste0("pkparam1_",listingindex)
  text <- text + paste0("!BLOCKSTART[keepNext](",blockIdentifier,")\n")
  text <- text + "**" + title + "**\n\n"
  tabtitle <- title
  text <- text + "* USUBJID: " + info$USUBJID[1] + "\n"
  tabtitle <- paste0(tabtitle, ": USUBJID ", info$USUBJID[1])
  if (!is.na(info$COMMENTI[1]))
    text <- text + "* Comment: " + info$COMMENTI[1] + "\n"
  if (!is.na(info$COUNTRY[1])){
    text <- text + "* Country: " + info$COUNTRY[1] + "\n"
    tabtitle <- paste0(tabtitle, "; Country ", info$COUNTRY[1])
  }
  if (!is.na(info$SITEID[1])){
    text <- text + "* Site ID: " + info$SITEID[1] + "\n"
    tabtitle <- paste0(tabtitle, "; Site ID ", info$SITEID[1])
  }
  if (!is.na(info$AGE[1])){
    text <- text + "* Age: " + info$AGE[1] + "\n"
    tabtitle <- paste0(tabtitle, "; Age ", info$AGE[1])
  }
  if (!is.na(info$SEX[1])){
    text <- text + "* Gender: " + info$SEX[1] + "\n"
    tabtitle <- paste0(tabtitle, "; Gender ", info$SEX[1])
  }
  if (!is.na(info$RACE[1])){
    text <- text + "* Race: " + info$RACE[1] + "\n"
    tabtitle <- paste0(tabtitle, "; Race ", info$RACE[1])
  }
  text <- text + rmdTABLEDF(df = tabparam,ignoreCaption = TRUE,fontsize = fontsizetable,footertext = footertext)
  text <- text + paste0("\n!BLOCKEND(",blockIdentifier,")\n")
  table <- IQRoutputTable(tabparam, xtitle = tabtitle, xfooter = footertext)
  if (!is.null(filename)) {
    return(text)
  } else {
    return(table)
  }
}
#' Generate a summary table for PK parameters (by dose group)
#'
#' The table will be generated already as an IQRrmd object, allowing easy reporting in Word with IQReport.
#' Stratification by PROFILE, GROUP, and a potential additional stratification column.
#'
#' @param data IQnca object
#' @param tablenumber Character string with table number information, added to the title for each table, if defined
#' @param strat Name of a column in the NCA result dataset (data) to use for stratification
#' @param fontsizetable Fontsize to be used in the table
#' @param filename Filename to export the listing IQRrmd object to
#' @param SIGNIF Significant digits for values
#' @param parameterReport Character string defining the PK parameters to report in the table
#'   By default these parameters will be selected based on the "Standard" parameters defined in the IQparamNCA_Specification.xlsx
#'   document, subject to the profile and administration type. The user can select "All" or "Standard".
#'   Alternatively, parameterReport can also be a vector or parameter names (based on PKPARAMCD in IQparamNCA_Specification.xlsx).
#'   Instead of the standard or all ones then these will be considered in the listing (also subject to profile and
#'   administration type). Note that interval AUC (if calculated is always added by default!)
#' @return An IQRrmd object exported to a file
#' @export
#' @family NCA Parameter Tables
table_summary_byGROUP_pkparameters_IQnca <- function (data,tablenumber=NULL,strat = NULL,fontsizetable=8,filename="table_bygroup_PKparam",SIGNIF=4,parameterReport="standard") {
  if (!is_IQnca(data)) stopIQR("data is not an IQnca object")
  if (!is.null(strat)) if(!strat %in% names(data)) stopIQR("strat not in NCA result")
  message("Generating PK parameter summary tables ...")
  info <- getValidityPKparam_IQRnca(data = data,parameterReport = parameterReport)
  pkparamInfo <- info$pkparamInfo
  userSelected <- info$userSelected
  covariates  <- c("USUBJID","PROFILE","GROUP","GROUPN","STUDYID","COMPOUND","ANALYTE","MATRIX","ADM","PERIOD","SEQUENCE","COUNTRY","SITEID","AGE","SEX","RACE")
  covariates  <- c(getaddcovcolumns_IQdataNCA(data),covariates)
  covdata     <- dplyr::distinct(data[, covariates, drop = FALSE])  
  pkparamInfo <- dplyr::left_join(pkparamInfo,covdata,by=c("USUBJID","PROFILE","GROUP"))
  non_IGNOREI <- unique(pkparamInfo$USUBJID[is.na(pkparamInfo$IGNOREI)])
  pkparamInfo <- pkparamInfo[pkparamInfo$USUBJID %in% non_IGNOREI,]
  non_REASONNA <- nchar(aux_strtrim(pkparamInfo$REASONNA))==0
  pkparamInfo <- pkparamInfo[non_REASONNA,]
  non_REASONNOTRELIABLE <- nchar(aux_strtrim(pkparamInfo$REASONNOTRELIABLE))==0
  pkparamInfo <- pkparamInfo[non_REASONNOTRELIABLE,]
  if (!is.null(filename)) {
    filename <- paste0(gsub(".rmd","",filename),".rmd")
  }
  pkparamInfo <- dplyr::arrange(pkparamInfo, PROFILE, GROUPN)
  if (!is.null(strat)) {
    pkparamInfo$stratxyz123 <- pkparamInfo[[strat]]
    pkparamInfo <- dplyr::arrange(pkparamInfo, PROFILE, GROUPN,stratxyz123)
  }
  pkparamInfo$GROUPfac <- factor(pkparamInfo$GROUP,levels=unique(pkparamInfo$GROUP))
  title <- updateTableNumberTitle_IQdataNCA(.table_summary_pkparametersbygroup,tablenumber,NULL)
  if (!is.null(strat)) title <- paste0(title," stratified by ",strat)
  text <- rmdEMPTY() +
    rmdLANDSCAPE() +
    rmdSECTION(title = title,
               level = 1,numbered = FALSE)
  table <- list()
  dS <- split(pkparamInfo,pkparamInfo$PROFILE)
  for (kprofile in seq_along(dS)) {
    d1 <- dS[[kprofile]]
    dS1 <- split(d1,d1$GROUPfac)
    count_group <- 1
    for (kgroup in seq_along(dS1)) {
      d2 <- dS1[[kgroup]]
      if (nrow(d2) > 0) {
        object <- tableSummaryPKparameters_byGROUP_IQnca(d = d2,strat=strat,tableindex = paste0(kprofile,"-",count_group),tablenumber=tablenumber,fontsizetable=fontsizetable,filename=filename,SIGNIF=SIGNIF)
        if ("IQRrmd" %in% class(object)) {
          textindiv <- object
          if (kgroup==1 & kprofile==1) {
            text <- text + textindiv + "\n"
          } else {
            text <- text + rmdNEWPAGE() + "\n" + textindiv + "\n"
          }
        }
        if ("IQRoutputTable" %in% class(object)) {
          table <- c(table, structure(list(object), names = paste0(d2$PROFILE[1],"-",as.character(d2$GROUPfac[1]))))
        }
        count_group <- count_group + 1
      }
    }
  }
  if (!is.null(filename)) {
    export_IQRrmd(text,filename)
  } else {
    return(table)
  }
}
tableSummaryPKparameters_byGROUP_IQnca <-  function (d,strat=NULL,tableindex,tablenumber=NULL,fontsizetable=8,filename="unknown",SIGNIF=4) {
  if (is.null(strat)) {
    dS <- split(d,d$PARAMCD)
    tab <- do.call(rbind,lapply(seq_along(dS), function (kgroup) {
      d3 <- dS[[kgroup]]
      if (nrow(d3)==0) return (NULL)
      N      <- nrow(d3[!is.na(d3$VALUE),])            
      MEAN   <- mean(d3$VALUE,na.rm = TRUE)
      SD     <- sd(d3$VALUE,na.rm = TRUE)
      CVPMN  <- suppressWarnings(100*SD/MEAN)
      GMEAN  <- suppressWarnings(geomean(d3$VALUE, na.rm = TRUE))
      CVPGM  <- suppressWarnings(geocv(d3$VALUE,na.rm = TRUE))
      MEDIAN <- median(d3$VALUE,na.rm = TRUE)
      MIN    <- min(d3$VALUE,na.rm = TRUE)
      MAX    <- max(d3$VALUE,na.rm = TRUE)
      data.frame(
        PROFILE   = d3$PROFILE[1],
        GROUP     = d3$GROUP[1],
        NAME   = paste0(d3$DESCRIPTION[1]," (",d3$UNIT[1],")"),
        N      = N,
        MEAN_SD   = paste0(signif(MEAN,SIGNIF)," (",ifelse(N==1,"-^b^",signif(SD,SIGNIF)),")"),
        CVPMN     = ifelse(MEAN==0 | N==1,"-^c^",signif(CVPMN,SIGNIF)),
        GMEAN     = ifelse(any(0 %in%  d3$VALUE) | N==1,"-^d^",signif(GMEAN,SIGNIF)),
        CVPGM     = ifelse(any(0 %in%  d3$VALUE) | N==1,"-^d^",signif(CVPGM,SIGNIF)),
        MEDIAN    = signif(MEDIAN,SIGNIF),
        MINMAX    = paste0("[",signif(MIN,SIGNIF),", ",signif(MAX,SIGNIF),"]"),
        stringsAsFactors = FALSE
      )
    }))
  } else {
    dS0 <- split(d,d[[strat]])
    tab <- do.call(rbind,lapply(seq_along(dS0), function (kstrat) {
      dstrat <- dS0[[kstrat]]
      dS <- split(dstrat,dstrat$PARAMCD)
      do.call(rbind,lapply(seq_along(dS), function (kgroup) {
        d3 <- dS[[kgroup]]
        if (nrow(d3)==0) return (NULL)
        N      <- nrow(d3[!is.na(d3$VALUE),])            
        MEAN   <- mean(d3$VALUE,na.rm = TRUE)
        SD     <- sd(d3$VALUE,na.rm = TRUE)
        CVPMN  <- suppressWarnings(100*SD/MEAN)
        GMEAN  <- suppressWarnings(geomean(d3$VALUE, na.rm = TRUE))
        CVPGM  <- suppressWarnings(geocv(d3$VALUE,na.rm = TRUE))
        MEDIAN <- median(d3$VALUE,na.rm = TRUE)
        MIN    <- min(d3$VALUE,na.rm = TRUE)
        MAX    <- max(d3$VALUE,na.rm = TRUE)
        data.frame(
          PROFILE   = d3$PROFILE[1],
          GROUP     = d3$GROUP[1],
          NAME   = paste0(d3$DESCRIPTION[1]," (",d3$UNIT[1],")"),
          STRAT     = d3[[strat]][1],
          N      = N,
          MEAN_SD   = paste0(signif(MEAN,SIGNIF)," (",ifelse(N==1,"-^b^",signif(SD,SIGNIF)),")"),
          CVPMN     = ifelse(MEAN==0 | N==1,"-^c^",signif(CVPMN,SIGNIF)),
          GMEAN     = ifelse(any(0 %in%  d3$VALUE) | N==1,"-^d^",signif(GMEAN,SIGNIF)),
          CVPGM     = ifelse(any(0 %in%  d3$VALUE) | N==1,"-^d^",signif(CVPGM,SIGNIF)),
          MEDIAN    = signif(MEDIAN,SIGNIF),
          MINMAX    = paste0("[",signif(MIN,SIGNIF),", ",signif(MAX,SIGNIF),"]"),
          stringsAsFactors = FALSE
        )
      }))
    }))
  }
  tab <- dplyr::arrange(tab,NAME)
  if (!is.null(strat)) tab <- dplyr::arrange(tab,NAME,STRAT)
  tab$PROFILE[duplicated(tab$PROFILE)] <- ""
  tab$GROUP[duplicated(tab$GROUP)] <- ""
  tab$NAME[duplicated(tab$NAME)] <- ""
  names(tab)[names(tab)=="PROFILE"] <- "Profile"
  names(tab)[names(tab)=="GROUP"] <- "Group"
  names(tab)[names(tab)=="NAME"] <- "Name (Unit)"
  names(tab)[names(tab)=="STRAT"] <- strat
  names(tab)[names(tab)=="N"] <- "N^a^"
  names(tab)[names(tab)=="MEAN_SD"] <- "Mean (SD)"
  names(tab)[names(tab)=="CVPMN"] <- "CV% mean"
  names(tab)[names(tab)=="GMEAN"] <- "Geo-mean"
  names(tab)[names(tab)=="CVPGM"] <- "CV% geo-mean"
  names(tab)[names(tab)=="MEDIAN"] <- "Median"
  names(tab)[names(tab)=="MINMAX"] <- "[Min, Max]"
  scriptName <- getScriptName_IQdataNCA()
  footertext <- paste0(
    "^a^ Number depends on (reliably) calculated parameters.\n",
    "CV%% = coefficient of variation (%%)=SD/mean*100.\n",
    "Geo-mean: Geometric mean.\n",
    "CV%% geo-mean=(sqrt (exp (variance for log transformed data)-1))*100.\n",
    "^b^ SD not presented when N=1.\n",
    "^c^ CV%% mean not presented when the mean is 0 or N=1.\n",
    "^d^ Geo-mean and CV%% geo-mean not presented when the minimum value is zero or N=1.\n",
    "Values in table are reported with ",SIGNIF," significant digits.\n",
    "IQRnca version: ", aux_version(pkgName = "IQnca"), "\n",
    "Script: ", scriptName,"\n",
    "Output: ",filename, "\n",
    "Execution date: ", Sys.time()
  )
  text <- rmdEMPTY()
  loadSetupOptions_IQnca()
  title <- .table_summary_pkparametersbygroup
  title <- updateTableNumberTitle_IQdataNCA(title,tablenumber,tableindex)
  title <- paste0(title," of ",d$MATRIX[1]," ",d$COMPOUND[1], " (",d$ANALYTE[1],") - ",d$PROFILE[1])
  if (!is.null(strat)) title <- paste0(title," (stratified by ",strat,")")
  blockIdentifier <- paste0("tableparampk1_",gsub("-","x",tableindex))
  text <- text + paste0("!BLOCKSTART[keepNext](",blockIdentifier,")\n")
  text <- text + "**" + title + "**"
  text <- text + rmdTABLEDF(df = tab,ignoreCaption = TRUE,fontsize = fontsizetable,footertext = footertext)
  text <- text + paste0("\n!BLOCKEND(",blockIdentifier,")\n")
  table <- IQRoutputTable(tab, xtitle = title, xfooter = footertext)
  if (!is.null(filename)) {
    return(text)
  } else {
    return(table)
  }
}
#' Generate a summary table for PK parameters (across dose groups)
#'
#' The table will be generated already as an IQRrmd object, allowing easy reporting in Word with IQReport.
#' Stratification by PROFILE and a potential additional stratification column. Grouping across GROUP (dose)
#' will always be done. Meaning that only dose independent parameters should be considered.
#'
#' @param data IQnca object
#' @param tablenumber Character string with table number information, added to the title for each table, if defined
#' @param strat Name of a column in the NCA result dataset (data) to use for stratification
#' @param fontsizetable Fontsize to be used in the table
#' @param filename Filename to export the listing IQRrmd object to
#' @param SIGNIF Significant digits for values
#' @param parameterReport Character string defining the PK parameters to report in the table
#'   By default these parameters will be selected based on the "Standard" parameters defined in the IQparamNCA_Specification.xlsx
#'   document, subject to the profile and administration type. The user can select "All" or "Standard".
#'   Alternatively, parameterReport can also be a vector or parameter names (based on PKPARAMCD in IQparamNCA_Specification.xlsx).
#'   Instead of the standard or all ones then these will be considered in the listing (also subject to profile and
#'   administration type). Note that interval AUC (if calculated is always added by default!)
#' @return An IQRrmd object exported to a file
#' @export
#' @family NCA Parameter Tables
table_summary_acrossGROUP_pkparameters_IQnca <- function (data,tablenumber=NULL,strat = NULL,fontsizetable=8,filename="table_acrossgroup_PKparam",SIGNIF=4,parameterReport="standard") {
  if (!is_IQnca(data)) stopIQR("data is not an IQnca object")
  if (!is.null(strat)) if(!strat %in% names(data)) stopIQR("strat not in NCA result")
  message("Generating PK parameter summary tables ...")
  info <- getValidityPKparam_IQRnca(data = data,parameterReport = parameterReport)
  pkparamInfo <- info$pkparamInfo
  userSelected <- info$userSelected
  if (!userSelected) {
    x <- getparamspec_IQdataNCA(data)
    param <- x$PKPARAMCD[x$ReportingAcrossDose == "YES"]
    pkparamInfo <- pkparamInfo[pkparamInfo$PARAMCD %in% param,]
  }
  covariates  <- c("USUBJID","PROFILE","GROUP","GROUPN","STUDYID","COMPOUND","ANALYTE","MATRIX","ADM","PERIOD","SEQUENCE","COUNTRY","SITEID","AGE","SEX","RACE")
  covariates  <- c(getaddcovcolumns_IQdataNCA(data),covariates)
  covdata     <- dplyr::distinct(data[,covariates, drop = FALSE]) 
  pkparamInfo <- dplyr::left_join(pkparamInfo, covdata, by = c("USUBJID", "PROFILE", "GROUP"))
  non_IGNOREI <- unique(pkparamInfo$USUBJID[is.na(pkparamInfo$IGNOREI)])
  pkparamInfo <- pkparamInfo[pkparamInfo$USUBJID %in% non_IGNOREI,]
  non_REASONNA <- nchar(aux_strtrim(pkparamInfo$REASONNA))==0
  pkparamInfo <- pkparamInfo[non_REASONNA,]
  non_REASONNOTRELIABLE <- nchar(aux_strtrim(pkparamInfo$REASONNOTRELIABLE))==0
  pkparamInfo <- pkparamInfo[non_REASONNOTRELIABLE,]
  if (!is.null(filename)) {
    filename <- paste0(gsub(".rmd","",filename),".rmd")
  }
  pkparamInfo <- dplyr::arrange(pkparamInfo, PROFILE, GROUPN)
  if (!is.null(strat)) {
    pkparamInfo$stratxyz123 <- pkparamInfo[[strat]]
    pkparamInfo <- dplyr::arrange(pkparamInfo, PROFILE, GROUPN,stratxyz123)
  }
  title <- updateTableNumberTitle_IQdataNCA(.table_summary_pkparameters,tablenumber,NULL)
  if (!is.null(strat)) title <- paste0(title," stratified by ",strat)
  text <- rmdEMPTY() +
    rmdLANDSCAPE() +
    rmdSECTION(title = title,
               level = 1,numbered = FALSE)
  table <- list()
  dS <- split(pkparamInfo,pkparamInfo$PROFILE)
  for (kprofile in seq_along(dS)) {
    d1 <- dS[[kprofile]]
    object <- tableSummaryPKparameters_acrossGROUP_IQnca(d = d1,strat=strat,tableindex = paste0(kprofile),tablenumber=tablenumber,fontsizetable=fontsizetable,filename=filename,SIGNIF=SIGNIF)
    if ("IQRrmd" %in% class(object)) {
      textindiv <- object
      if (kprofile==1) {
        text <- text + textindiv + "\n"
      } else {
        text <- text + rmdNEWPAGE() + "\n" + textindiv + "\n"
      }
    }
    if ("IQRoutputTable" %in% class(object)) {
      table <- c(table, structure(list(object), names = d1$PROFILE[1]))
    }
  }
  if (!is.null(filename)) {
    export_IQRrmd(text,filename)
  } else {
    return(table)
  }
}
tableSummaryPKparameters_acrossGROUP_IQnca <-  function (d,strat=NULL,tableindex,tablenumber=NULL,fontsizetable=8,filename="unknown",SIGNIF=4) {
  if (is.null(strat)) {
    dS <- split(d,d$PARAMCD)
    tab <- do.call(rbind,lapply(seq_along(dS), function (kgroup) {
      d3 <- dS[[kgroup]]
      if (nrow(d3)==0) return (NULL)
      N      <- nrow(d3[!is.na(d3$VALUE),])            
      MEAN   <- mean(d3$VALUE,na.rm = TRUE)
      SD     <- sd(d3$VALUE,na.rm = TRUE)
      CVPMN  <- suppressWarnings(100*SD/MEAN)
      GMEAN  <- suppressWarnings(geomean(d3$VALUE, na.rm = TRUE))
      CVPGM  <- suppressWarnings(geocv(d3$VALUE,na.rm = TRUE))
      MEDIAN <- median(d3$VALUE,na.rm = TRUE)
      MIN    <- min(d3$VALUE,na.rm = TRUE)
      MAX    <- max(d3$VALUE,na.rm = TRUE)
      data.frame(
        PROFILE   = d3$PROFILE[1],
        NAME   = paste0(d3$DESCRIPTION[1]," (",d3$UNIT[1],")"),
        N      = N,
        MEAN_SD   = paste0(signif(MEAN,SIGNIF)," (",ifelse(N==1,"-^b^",signif(SD,SIGNIF)),")"),
        CVPMN     = ifelse(MEAN==0 | N==1,"-^c^",signif(CVPMN,SIGNIF)),
        GMEAN     = ifelse(any(0 %in%  d3$VALUE) | N==1,"-^d^",signif(GMEAN,SIGNIF)),
        CVPGM     = ifelse(any(0 %in%  d3$VALUE) | N==1,"-^d^",signif(CVPGM,SIGNIF)),
        MEDIAN    = signif(MEDIAN,SIGNIF),
        MINMAX    = paste0("[",signif(MIN,SIGNIF),", ",signif(MAX,SIGNIF),"]"),
        stringsAsFactors = FALSE
      )
    }))
  } else {
    dS0 <- split(d,d[[strat]])
    tab <- do.call(rbind,lapply(seq_along(dS0), function (kstrat) {
      dstrat <- dS0[[kstrat]]
      dS <- split(dstrat,dstrat$PARAMCD)
      do.call(rbind,lapply(seq_along(dS), function (kgroup) {
        d3 <- dS[[kgroup]]
        if (nrow(d3)==0) return (NULL)
        N      <- nrow(d3[!is.na(d3$VALUE),])            
        MEAN   <- mean(d3$VALUE,na.rm = TRUE)
        SD     <- sd(d3$VALUE,na.rm = TRUE)
        CVPMN  <- suppressWarnings(100*SD/MEAN)
        GMEAN  <- suppressWarnings(geomean(d3$VALUE, na.rm = TRUE))
        CVPGM  <- suppressWarnings(geocv(d3$VALUE,na.rm = TRUE))
        MEDIAN <- median(d3$VALUE,na.rm = TRUE)
        MIN    <- min(d3$VALUE,na.rm = TRUE)
        MAX    <- max(d3$VALUE,na.rm = TRUE)
        data.frame(
          PROFILE   = d3$PROFILE[1],
          NAME   = paste0(d3$DESCRIPTION[1]," (",d3$UNIT[1],")"),
          STRAT     = d3[[strat]][1],
          N      = N,
          MEAN_SD   = paste0(signif(MEAN,SIGNIF)," (",ifelse(N==1,"-^b^",signif(SD,SIGNIF)),")"),
          CVPMN     = ifelse(MEAN==0 | N==1,"-^c^",signif(CVPMN,SIGNIF)),
          GMEAN     = ifelse(any(0 %in%  d3$VALUE) | N==1,"-^d^",signif(GMEAN,SIGNIF)),
          CVPGM     = ifelse(any(0 %in%  d3$VALUE) | N==1,"-^d^",signif(CVPGM,SIGNIF)),
          MEDIAN    = signif(MEDIAN,SIGNIF),
          MINMAX    = paste0("[",signif(MIN,SIGNIF),", ",signif(MAX,SIGNIF),"]"),
          stringsAsFactors = FALSE
        )
      }))
    }))
  }
  tab <- dplyr::arrange(tab,NAME)
  if (!is.null(strat)) tab <- dplyr::arrange(tab,NAME,STRAT)
  tab$PROFILE[duplicated(tab$PROFILE)] <- ""
  tab$NAME[duplicated(tab$NAME)] <- ""
  names(tab)[names(tab)=="PROFILE"] <- "Profile"
  names(tab)[names(tab)=="NAME"] <- "Name (Unit)"
  names(tab)[names(tab)=="STRAT"] <- strat
  names(tab)[names(tab)=="N"] <- "N^a^"
  names(tab)[names(tab)=="MEAN_SD"] <- "Mean (SD)"
  names(tab)[names(tab)=="CVPMN"] <- "CV% mean"
  names(tab)[names(tab)=="GMEAN"] <- "Geo-mean"
  names(tab)[names(tab)=="CVPGM"] <- "CV% geo-mean"
  names(tab)[names(tab)=="MEDIAN"] <- "Median"
  names(tab)[names(tab)=="MINMAX"] <- "[Min, Max]"
  scriptName <- getScriptName_IQdataNCA()
  footertext <- paste0(
    "^a^ Number depends on (reliably) calculated parameters.\n",
    "CV%% = coefficient of variation (%%)=SD/mean*100.\n",
    "Geo-mean: Geometric mean.\n",
    "CV%% geo-mean=(sqrt (exp (variance for log transformed data)-1))*100.\n",
    "^b^ SD not presented when N=1.\n",
    "^c^ CV%% mean not presented when the mean is 0 or N=1.\n",
    "^d^ Geo-mean and CV%% geo-mean not presented when the minimum value is zero or N=1.\n",
    "Values in table are reported with ",SIGNIF," significant digits.\n",
    "IQnca version: ", aux_version(pkgName = "IQnca"), "\n",
    "Script: ", scriptName,"\n",
    "Output: ",filename, "\n",
    "Execution date: ", Sys.time()
  )
  text <- rmdEMPTY()
  loadSetupOptions_IQnca()
  title <- .table_summary_pkparameters
  title <- updateTableNumberTitle_IQdataNCA(title,tablenumber,tableindex)
  title <- paste0(title," of ",d$MATRIX[1]," ",d$COMPOUND[1], " (",d$ANALYTE[1],") - ",d$PROFILE[1])
  if (!is.null(strat)) title <- paste0(title," (stratified by ",strat,")")
  blockIdentifier <- paste0("tableparampk2_",gsub("-","x",tableindex))
  text <- text + paste0("!BLOCKSTART[keepNext](",blockIdentifier,")\n")
  text <- text + "**" + title + "**"
  text <- text + rmdTABLEDF(df = tab,ignoreCaption = TRUE,fontsize = fontsizetable,footertext = footertext)
  text <- text + paste0("\n!BLOCKEND(",blockIdentifier,")\n")
  table <- IQRoutputTable(tab, xtitle = title, xfooter = footertext)
  if (!is.null(filename)) {
    return(text)
  } else {
    return(table)
  }
}
#' Concatenate IQRrmd objects
#'
#' @param a Character string or number or IQRrmd object
#' @param b Character string or number or IQRrmd object
#' @return Concatenated IQRrmd object "ab"
#' @export
"+.IQRrmd" <- function (a,b) {
  out__ <- paste0(a,b)
  class(out__) <- "IQRrmd"
  return(out__)
}
#' Overloading "print" for IQRrmd objects
#'
#' @param x IQRrmd object
#' @param ... Additional parameters
#' @export
print.IQRrmd <- function (x, ...) {
  cat(x)
  cat("\n\nIQRrmd object")
}
#' Export IQRrmd object to an rmd file
#'
#' @param rmdDoc IQRrmd object
#' @param filename Filename (might include path). If not already appended, ".rmd"
#'    will be appended.
#' @export
export_IQRrmd <- function (rmdDoc, filename) {
  filename <- paste0(aux_strrep(filename,".rmd",""),".rmd")
  aux_filewrite(paste0(rmdDoc),filename)
}
#' Create empty IQRrmd object
#'
#' @return Empty IQRrmd object
#' @export
rmdEMPTY <- function() {
  out__ <- ""
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Add a simple line break
#'
#' @return Linebreak RMD object
#' @export
rmdLINEBREAK <- function() {
  out__ <- "\n"
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Start a new paragraph
#'
#' @return New paragraph RMD object
#' @export
rmdPARAGRAPH <- function() {
  out__ <- "\n\n"
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Create empty IQRrmd object
#'
#' @return Empty IQRrmd object
#' @export
rmdEMPTY <- function() {
  out__ <- ""
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Generate title RMD command
#'
#' In the title section of an RMD document the template to be used and the
#' title page information to be shown are defined. IQnca works with IQReport and
#' the DefaultStyle.rmdt template document. If you want to use a different template
#' you can do that but you would need to have one. Contact info@intiquan.com to
#' learn more. You basically can get IQnca generate Word reports in the desired
#' style, required for your organization.
#'
#' @param template Name of the *.rmdt template file to be used
#' @param title Character string with title of the document
#' @param subtitle Character string with subtitle of the document
#' @param date Character string with date to be shown
#' @return Title IQRrmd object
#' @export
rmdTITLE <- function(template="DefaultStyle.rmdt",
                     title="Default Title",
                     subtitle="Default Subtitle",
                     date=format(Sys.Date(),"%d-%b-%Y")) {
  out__ <-              "=====\n"
  out__ <- paste0(out__,"TITLE:    ",title,"\n")
  out__ <- paste0(out__,"SUBTITLE: ",subtitle,"\n")
  out__ <- paste0(out__,"DATE:     ",date,"\n")
  out__ <- paste0(out__,"TEMPLATE: ",template,"\n")
  out__ <- paste0(out__,"=====\n\n")
  class(out__) <- "IQRrmd"
  return(out__)
}
#' Generate newpage RMD command
#'
#' @return New page command as IQRrmd object
#' @export
rmdNEWPAGE <- function() {
  out__ <- "\n!NEWPAGE\n"
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Add an approvals page to the report (after title)
#'
#' This command is only working as expected if the used template actually has an
#' approvals page included. Otherwise this command will be ignored.
#'
#' @return APPROVALS command as IQRrmd object
#' @export
rmdAPPROVALS <- function() {
  out__ <- "\n!APPROVALS\n"
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Do not include TOC/TOF/TOT in DOCX  report
#'
#' The !NOINTRO command will remove the table of contents, figures, and tables
#' from the Word document.
#'
#' @return NOINTRO command as IQRrmd object
#' @export
rmdNOINTRO <- function() {
  out__ <- "\n!NOINTRO\n"
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Switch to Landscape mode
#'
#' This will only work if the section where the landcape command is called contains
#' some text ... tables or figures do not count ...
#'
#' @return Landscape command as IQRrmd object
#' @export
rmdLANDSCAPE <- function() {
  out__ <- "\n!LANDSCAPE\n"
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Switch to Portrait mode
#'
#' @return Portrait command as IQRrmd object
#' @export
rmdPORTRAIT <- function() {
  out__ <- "\n!PORTRAIT\n"
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Generate Section RMD command
#'
#' Allows generation of RMD strings to implement sections (level=1), subsections
#' (level=2), subsubsections (level=3), etc. Sections can be numbered (numbered=TRUE)
#' or unnumbered (numbered=FALSE). A label can be given to be able to crossreference
#' this section.
#'
#' Unnumbered sections have the limitation to level=1 and only a single unnumbered section
#' can be present in a document - the first one.
#'
#' @param title Character string with title of section
#' @param label Character string (no spaces) with label for cross-reference use.
#'  Limited to 40 characters.
#' @param level Integer value (1,2,3,etc.) to indicate the section level
#' @param numbered Boolean to define if section shoud be numbered or not (experimental)
#' @return Section RMD command as IQRrmd object
#' @export
rmdSECTION <- function(title="Section Title",label=NULL,level=1,numbered=TRUE) {
  out__ <- paste0("\n\n",paste0(rep("#",level),collapse=""))
  if (!numbered)
    out__ <- paste0(out__,"*")
  out__ <- paste0(out__," ",title)
  if (!is.null(label))
    out__ <- paste0(out__," {#",label,"}")
  out__ <- paste0(out__,"\n")
  class(out__) <- "IQRrmd"
  return(out__)
}
#' Generate RMD command to include a URL
#'
#' @param url Character string, specifying the URL
#' @param caption Text to display as hyperlink
#' @return URL command as IQRrmd object
#' @export
rmdURL <- function(url="http://www.intiquan.com",caption=NULL) {
  out__ <- "!URL"
  if (!is.null(caption))
    out__ <- paste0(out__,"[",caption,"]")
  out__ <- paste0(out__,"(",url,")")
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Generate RMD command to include a Word comment
#'
#' @param comment Character string, specifying the comment text. Line breaks are
#'   allowed.
#' @param author Character string, specifying the author
#' @return COM command as IQRrmd object
#' @export
rmdCOMMENT <- function(comment="some comment",author=NULL) {
  out__ <- "!COM"
  if (!is.null(author))
    out__ <- paste0(out__,"[author:",author,"]")
  out__ <- paste0(out__,"{",comment,"}")
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Generate RMD command to include a text from a file
#'
#' @param file Character string, specifying the absolute or relative path to the
#' text file to include.
#' @param caption Caption for the text file. If NULL then no caption is shown.
#' @param fontsize Integer value for the fontsize.
#' @param style Word DOCX template style to use for the text (default: "CODE")
#' @param label Character string to be used for cross-referencing. If label and caption
#'   provided, then the caption will be numbered.
#' @return TXT command as IQRrmd object
#' @export
rmdTXT <- function(file,caption=NULL,fontsize=10,style="CODE",label=NULL) {
  out__ <- "\n!TXT"
  if (!is.null(c(caption,style,label))) {
    opttext__ <- ""
    if (!is.null(caption))
      opttext__ <- paste0(opttext__,"caption:",caption,",")
    if (!is.null(style))
      opttext__ <- paste0(opttext__,"style:",style,"(",fontsize,"),")
    if (!is.null(label))
      opttext__ <- paste0(opttext__,"label:",label,",")
    if (substr(opttext__,nchar(opttext__),nchar(opttext__))==",") opttext__ <- substr(opttext__,1,nchar(opttext__)-1)
    out__ <- paste0(out__,"[",opttext__,"]")
  }
  out__ <- paste0(out__,"(",file,")\n")
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Generate RMD command to include a table from a file
#'
#' Allows to generate RMD code to include a table defined in a text file.
#' To include a table from an R dataframe, please use the function
#' rmdTABLEDF.
#'
#' @param file Character string, specifying the absolute or relative path to the
#'   table text file to include.
#' @param caption Caption for the table. If NULL then the title information in the
#'   table text file is used as caption.
#' @param fontsize Integer value for the fontsize.
#' @param label Character string to be used for cross-referencing.
#' @param ignoreCaption Caption can be ignored if desired (no caption will be shown for table)
#' @param valueTable Defines is table can be used as a "Value Table", allowing to reference
#'   values in the table. A value table requires the presence of a "value" and a "label" column.
#'   If these are not present, an error will occur.
#' @export
rmdTABLE <- function(file,caption=NULL,fontsize=10,label=NULL,
                     ignoreCaption=FALSE,
                     valueTable=FALSE) {
  out__ <- "\n!TAB"
  if (!is.null(c(caption,fontsize,label,ignoreCaption,valueTable))) {
    opttext__ <- ""
    if (!is.null(caption))
      opttext__ <- paste0(opttext__,"caption:",caption,",")
    if (!is.null(fontsize))
      opttext__ <- paste0(opttext__,"size:",fontsize,",")
    if (!is.null(label))
      opttext__ <- paste0(opttext__,"label:",label,",")
    if (ignoreCaption) {
      opttext__ <- paste0(opttext__,"ignoreCaption:true,")
    } else {
      opttext__ <- paste0(opttext__,"ignoreCaption:false,")
    }
    if (valueTable) {
      opttext__ <- paste0(opttext__,"valueTable:true,")
    } else {
      opttext__ <- paste0(opttext__,"valueTable:false,")
    }
    if (substr(opttext__,nchar(opttext__),nchar(opttext__))==",") opttext__ <- substr(opttext__,1,nchar(opttext__)-1)
    out__ <- paste0(out__,"[",opttext__,"]")
  }
  out__ <- paste0(out__,"(",file,")\n")
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Generate RMD command to include a table from a dataframe
#'
#' Allows to generate RMD code to include a table defined by a dataframe.
#' To include a table from a table text file, please use the function
#' rmdTABLE.
#'
#' @param df Dataframe to export as table in RMD.
#' @param caption Caption for the table.
#' @param fontsize Integer value for the fontsize.
#' @param label Character string to be used for cross-referencing.
#' @param ignoreCaption Caption can be ignored if desired (no caption will be shown for table)
#' @param valueTable Defines is table can be used as a "Value Table", allowing to reference
#'   values in the table. A value table requires the presence of a "value" and a "label" column.
#'   If these are not present, an error will occur.
#' @param footertext Defined text for the table footer.
#' @return Inline table command as IQRrmd object
#' @export
rmdTABLEDF <- function(df,caption=NULL,fontsize=10,label=NULL,ignoreCaption=FALSE,valueTable=FALSE,footertext=NULL) {
  out__ <- "\n!TABINLINE["
  if (!is.null(caption))
    out__ <- paste0(out__,"caption:",caption,",")
  if (!is.null(fontsize))
    out__ <- paste0(out__,"size:",fontsize,",")
  if (!is.null(label))
    out__ <- paste0(out__,"label:",label,",")
  if (ignoreCaption) {
    out__ <- paste0(out__,"ignoreCaption:true,")
  } else {
    out__ <- paste0(out__,"ignoreCaption:false,")
  }
  if (valueTable) {
    out__ <- paste0(out__,"valueTable:true")
  } else {
    out__ <- paste0(out__,"valueTable:false")
  }
  out__ <- paste0(out__,"]")
  out__
  headerNames__ <- names(df)
  dfchar__ <- data.frame(
    lapply(df, function (x__) {
      out__ <- as.character(x__)
      out__[is.na(out__)] <- "NA"
      out__ <- gsub(pattern="\n",replacement="",x=out__)
      out__ <- gsub(pattern="\r",replacement="",x=out__)
      out__
    })
    ,stringsAsFactors=FALSE
  )
  ncharCol__ <- sapply(1:ncol(dfchar__), function (y__) max(nchar(headerNames__[y__]),max(nchar(dfchar__[[y__]]))))
  headerExp__ <- sapply(seq_along(headerNames__), function(k__) aux_postFillChar(value2postfill=headerNames__[k__],lengthString=ncharCol__[k__],fillChar=" ") )
  headerLine__ <- sapply(headerExp__, function (x__) paste0(rep("-",nchar(x__)),collapse=""))
  headerOut__ <- paste0("| ",
                        paste(headerExp__,collapse = " | "),
                        " |\n",
                        "| ",
                        paste(headerLine__,collapse = " | "),
                        " |\n")
  textContent__ <- paste0(sapply(1:nrow(dfchar__), function (nrow__) {
    textExp__ <- sapply(seq_along(headerNames__), function(k__) aux_postFillChar(value2postfill=dfchar__[nrow__,k__],lengthString=ncharCol__[k__],fillChar=" ") )
    textOut__ <- paste0("| ",
                        paste(textExp__,collapse = " | "),
                        " |\n")
  }),collapse="")
  tableOut__ <- paste0("\n",headerOut__,textContent__)
  if (!is.null(footertext)) {
    footertext <- gsub("\n","<br>",footertext)
    tableOut__ <- paste0(tableOut__,"{",footertext,"}")
  }
  out__ <- paste0(out__,tableOut__)
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Generate RMD command to include a figure from a file
#'
#' Allows to generate RMD code to include a figure defined in a pdf, png, jpg file.
#' From multi page PDFs selected pages or page ranges can be included.
#'
#' @param file Character string, specifying the absolute or relative path to the
#'   figure file to include (pdf, png, jpg, etc.).
#' @param caption Caption for the table. If NULL then the title information in the
#'   table text file is used as caption.
#' @param label Character string to be used for cross-referencing.
#' @param legend Character string with text that is written as "legend" under the figure.
#' @param pages A character string with information about which pages of a multi-page PDF
#'   should be included into the report. If undefined all pages will be exported with the same
#'   caption and figure number. The legend will be repeated after each figure if argument
#'   \code{repeatLegend} is set to TRUE.
#'   Example: pages="3 5-8" exports page 3,5,6,7,and 8.
#' @param scale Number value (percent) for scaling the figure.
#' @param crop Boolean. If set to TRUE then figure will be cropped to remove surrounding white space.
#' @param repeatLegend See \code{legend}.
#' @param ignoreCaption Caption can be ignored if desired (no caption will be shown for figure)
#' @return FIG command as IQRrmd object
#' @export
rmdFIGURE <- function(file,
                      caption=NULL,label=NULL,
                      legend=NULL,
                      pages=NULL,
                      scale=100, crop=TRUE,
                      repeatLegend=TRUE,
                      ignoreCaption=FALSE) {
  out__ <- "\n!FIG"
  if (!is.null(c(caption,label,legend,pages,scale,crop,repeatLegend,ignoreCaption))) {
    opttext__ <- ""
    if (!is.null(caption))
      opttext__ <- paste0(opttext__,"caption:",caption,",")
    if (!is.null(label))
      opttext__ <- paste0(opttext__,"label:",label,",")
    if (!is.null(pages))
      opttext__ <- paste0(opttext__,"pages:",pages,",")
    if (!is.null(scale))
      opttext__ <- paste0(opttext__,"scale:",scale,",")
    if (crop) {
      opttext__ <- paste0(opttext__,"crop:true,")
    } else {
      opttext__ <- paste0(opttext__,"crop:false,")
    }
    if (ignoreCaption) {
      opttext__ <- paste0(opttext__,"ignoreCaption:true,")
    } else {
      opttext__ <- paste0(opttext__,"ignoreCaption:false,")
    }
    if (repeatLegend) {
      opttext__ <- paste0(opttext__,"repeatLegend:true,")
    } else {
      opttext__ <- paste0(opttext__,"repeatLegend:false,")
    }
    if (substr(opttext__,nchar(opttext__),nchar(opttext__))==",") opttext__ <- substr(opttext__,1,nchar(opttext__)-1)
    out__ <- paste0(out__,"[",opttext__,"]")
  }
  out__ <- paste0(out__,"(",file,")")
  if (!is.null(legend))
    out__ <- paste0(out__,"\n{\n",gsub("\n","  \n",legend),"\n}")
  out__ <- paste0(out__,"\n")
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Generate RMD command to include a PDF from a file
#'
#' The main difference to rmdFIGURE is that rmdPDF does not create a caption and
#' not an entry into the List of Figures. This allows to include PDF documents as
#' sections ... if desired. Compliance text is not shown.
#'
#' @param file Character string, specifying the absolute or relative path to the
#'   PDF file to include.
#' @param pages A character string with information about which pages of a multi-page PDF
#'   should be included into the report.
#'   Example: pages="3 5-8" exports page 3,5,6,7,and 8.
#' @param scale Number value (percent) for scaling the figure.
#' @param crop Boolean. If set to TRUE then figure will be cropped to remove surrounding white space.
#' @return PDF command as IQRrmd object
#' @export
rmdPDF <- function(file,pages=NULL,scale=100,crop=TRUE) {
  out__ <- "\n!PDF"
  if (!is.null(c(pages,scale,crop))) {
    opttext__ <- ""
    if (!is.null(pages))
      opttext__ <- paste0(opttext__,"pages:",pages,",")
    if (!is.null(scale))
      opttext__ <- paste0(opttext__,"scale:",scale,",")
    if (crop) {
      opttext__ <- paste0(opttext__,"crop:true,")
    } else {
      opttext__ <- paste0(opttext__,"crop:false,")
    }
    if (substr(opttext__,nchar(opttext__),nchar(opttext__))==",") opttext__ <- substr(opttext__,1,nchar(opttext__)-1)
    out__ <- paste0(out__,"[",opttext__,"]")
  }
  out__ <- paste0(out__,"(",file,")")
  out__ <- paste0(out__,"\n")
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Section Cross reference
#'
#' Allows to reference a section by the section label.
#' "@SEC[caption](label)"
#'
#' @param label Character string, specifying the label of the section. Labels are limited to
#'   40 characters and should not include whitespaces.
#' @param caption A character string with alernative text to show (instead of "Section x.y.z").
#' @return Section cross reference command as IQRrmd object
#' @export
rmdSECref <- function(label,caption=NULL) {
  out__ <- " @SEC"
  if (!is.null(caption))
    out__ <- paste0(out__,"[",caption,"]")
  out__ <- paste0(out__,"(",label,")")
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Text Cross reference
#'
#' Allows to reference an included text by label.
#' "@TXT[caption](label)"
#'
#' If caption ignored the cross-reference will not work.
#'
#' @param label Character string, specifying the label of the text. Labels are limited to
#'   40 characters and should not include whitespaces.
#' @param caption A character string with alernative text to show (instead of "Text x").
#' @return Text cross reference command as IQRrmd object
#' @export
rmdTXTref <- function(label,caption=NULL) {
  out__ <- " @TXT"
  if (!is.null(caption))
    out__ <- paste0(out__,"[",caption,"]")
  out__ <- paste0(out__,"(",label,")")
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Figure Cross reference
#'
#' Allows to reference an included figure by label.
#' "@FIG[caption](label)"
#'
#' If caption ignored the cross-reference will not work.
#'
#' @param label Character string, specifying the label of the figure. Labels are limited to
#'   40 characters and should not include whitespaces.
#' @param caption A character string with alernative text to show (instead of "Text x").
#' @return Figure cross reference command as IQRrmd object
#' @export
rmdFIGref <- function(label,caption=NULL) {
  out__ <- " @FIG"
  if (!is.null(caption))
    out__ <- paste0(out__,"[",caption,"]")
  out__ <- paste0(out__,"(",label,")")
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Table Cross reference
#'
#' Allows to reference an included table by label.
#' "@TAB[caption](label)"
#'
#' If caption ignored the cross-reference will not work.
#'
#' @param label Character string, specifying the label of the table. Labels are limited to
#'   40 characters and should not include whitespaces.
#' @param caption A character string with alernative text to show (instead of "Text x").
#' @return Table cross reference command as IQRrmd object
#' @export
rmdTABref <- function(label,caption=NULL) {
  out__ <- " @TAB"
  if (!is.null(caption))
    out__ <- paste0(out__,"[",caption,"]")
  out__ <- paste0(out__,"(",label,") ")
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Value Table cross reference
#'
#' Allows to refence (display) values that are stored in labeled tables.
#' Two ways are possible:
#'  * Either by referncing row (not counting header row) and column number:
#'    "@VAL[row,col](labelTable)"
#'  * Or by referencing the label in a ValueTable:
#'    "@VAL[labelValue](labelTable)"
#'
#' Alternative way in RMD (but not implemented in this function) - for ValueTables:
#' "@VAL[labelCell:labelCol:valueCol](labelTable)"
#'
#' @param labelTable Character string, specifying the label of the table. Labels are limited to
#'   40 characters and should not include whitespaces.
#' @param row Numeric value (integer) defining the row of the element to report.
#'   Header row not counted.
#' @param col Numeric value (integer) defining the column of the element to report.
#' @param labelValue Character string with the element in the "label" column of a
#'   ValueTable. If used then row and col need to be NULL and vice versa.
#' @return Table element value cross reference command as IQRrmd object
#' @export
rmdVALref <- function(labelTable,row=NULL,col=NULL,labelValue=NULL) {
  if (!is.null(labelValue)) {
    out__ <- paste0(" @VAL[",labelValue,"](",labelTable,") ")
  } else {
    out__ <- paste0(" @VAL[",row,",",col,"](",labelTable,") ")
  }
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Change the size, color, and/or style of a text piece
#'
#' @param text The text to which the changes should apply.
#' @param color string with color name ("blue", "black", "orange", "pink", etc.) In addition to
#'   color names also "#ffeedd" type of definition can be used.
#' @param size Numeric value (integer) defining the font size.
#' @param style Word character style to use. Note it has to be a Character style! Otherwise nothing is happening!
#' @return Textelement as IQRrmd object
#' @export
rmdTEXTSTYLE <- function(text,color=NULL,size=NULL,style=NULL) {
  if (is.null(color) & is.null(size) & is.null(style)) {
    out__ <- text
  } else {
    out__ <- "!TEXTSTYLE["
    x1__ <- NULL
    x2__ <- NULL
    x3__ <- NULL
    if (!is.null(color)) x1__ <- paste0("color=",color)
    if (!is.null(size)) x2__ <- paste0("size=",size)
    if (!is.null(style)) x3__ <- paste0("style=",style)
    out__ <- paste0(out__,paste0(c(x1__,x2__,x3__),collapse = ","),"](",text,")")
  }
  class(out__) <- "IQRrmd"
  return (out__)
}
#' Interface to the IQReport command line interface tool
#'
#' Allows to generate Word DOCX files from IQReport specific rmd files.
#' IQReport is not shipped with IQnca but has to be obtained and installed
#' separately (http://www.intiquan.com/iqreport/).
#'
#' @param RMDfilePath Relative path to the RMD file.
#' @return No return value. But the generated DOCX file is available in the
#'         same folder as the RMD file.
#' @export
IQReport <- function(RMDfilePath){
  if (!has_IQReport_executable())
    stopIQR("The IQReport executable has not been installed.\nYou can obtain IQReport from here: http://www.intiquan.com/iqreport-the-reporting-solution/")
  RMDfilePath <- paste0(aux_strrep(RMDfilePath,".rmd",""),".rmd")
  if (!file.exists(RMDfilePath))
    stopIQR("The provided RMDfilePath argument does not point to an '.rmd' file (case sensitive)")
  if (grepl("~",RMDfilePath,fixed = TRUE))
    stopIQR("The use of '~' in path names is not allowed. Please use a relative path instead!")
  if (grepl(":",RMDfilePath,fixed = TRUE))
    stopIQR("The use of ':' in path names is not allowed. Please use a relative path instead!")
  if (.Platform$OS.type=="windows") {
    oldpath <- getwd()
    fp      <- aux_fileparts(RMDfilePath)
    setwd(fp$pathname)
    RMDfilePathAbsolute <- paste0(getwd(),"/",fp$filename,fp$fileext)
    setwd(oldpath)
    callIQReport <- paste0(.PATH_IQRreport,' "',RMDfilePathAbsolute,'"')
    oldpath <- getwd()
    setwd(aux_fileparts(.PATH_IQRreport)$pathname)
    system(callIQReport)
    setwd(oldpath)
  } else {
    system(paste0("IQReport.sh ",RMDfilePath))
  }
  aux_rmdir("tmp")
  unlink("*.rmd.bak")
}
#' Check if IQReport is installed and available
#'
#' @return Returns TRUE if IQReport is available on the system
#' @export
has_IQReport_executable <- function(){
  loadSetupOptions_IQnca()
  if (file.exists(.PATH_IQRreport)) return(TRUE)
  return(FALSE)
}
#' Function to translate listings or tables (IQncaTableList object) to rmd
#'
#' @param tabs IQncaTableList object
#' @param fontsize Fontsize to be used for table font
#' @param page_orientation Page orientation, 'landscape' or 'portrait'
#'
#' @return r
textRMD_IQncaTableList <- function(
    tabs,
    fontsize = 8,
    page_orientation = c("landscape", "portrait")
) {
  if (!"IQncaTableList" %in% class(tabs)) {
    stopIQR("Input to create rmd needs to be a IQncaTableList.")
  }
  page_orientation <- match.arg(page_orientation, choices = c("landscape", "portrait"))
  ntot <- length(tabs$content)
  text <- rmdEMPTY()
  if (page_orientation == "landscape") {
    text <- text + rmdLANDSCAPE()
  } else {
    text <- text + rmdPORTRAIT()
  }
    text <- text +
      rmdSECTION(title = tabs$title,
               level = 1, numbered = FALSE)
  for (k in seq_along(tabs$content)) {
    tab <- tabs$content[[k]]
    if (k > 1) {
      text <- text + rmdNEWPAGE() + "\n\n"
    }
    blockIdentifier <- paste0("indiv1_",tab$index)
    text <- text + paste0("!BLOCKSTART[keepNext](",blockIdentifier,")\n\n")
    if (ntot > 1) {
      text <- text + "##* " + tab$title  + "\n" 
    }
    for (k in seq_along(tab$subtitle)) {
      name <- names(tab$subtitle)[k]
      value <- tab$subtitle[[k]]
      text <- text + "* " + name + ": " + value + "\n"
    }
    table <- tab$content
    table <- replace_colname_by_label(table)
    text <- text + rmdTABLEDF(df = table,ignoreCaption = TRUE,fontsize = fontsize,footertext = tab$footer)
    text <- text + paste0("\n!BLOCKEND(",blockIdentifier,")\n")
  }
  return(text)
}
#' Writes rmd file for reporting of listing or table in a IQReport-compiled Word document
#'
#' @param tabs IQncaTableList object containing a list of tables (IQncaTable objects) with titles and footers
#' @param filename Filename towrite the rmd to
#' @param fontsize Font to be used in tables
#' @param page_orientation whether to plot table on landscape or portrait page
#'
#' @return Nothing return. Output to file.
#' @export
#'
#' @examples
#' \dontrun{
#' write_IQncaTableList(
#'  tableList,
#'  filename = "../Output/tables/mytable.rmd",
#'  page_orientation = "landscape"
#' )
#' }
write_IQncaTableList <- function(
    tabs,
    filename,
    fontsize = 8,
    page_orientation
) {
  text <- textRMD_IQncaTableList(tabs, fontsize = fontsize, page_orientation)
  export_IQRrmd(text, filename)
  return(invisible(NULL))
}
#' Generate a table of statistical summaries of pharmacokinetic concentrations
#'
#' The table will be generated already as an IQRrmd object, allowing easy reporting in Word with IQReport.
#' The table is done per PROFILE and GROUP.
#' Important: Ignored records (INGORER & IGNORSUM) are not reported in these tables! Ignored subjects (IGNOREI) are
#' removed as well.
#'
#' @param data IQdataNCA object
#' @param stratify_by Columns that define the groups that are summarized
#' @param statistics Statistics to be calculated and displayed. Defined by charater vector of shortnames:
#' \tabular{ll}{
#' Short name   \tab Description  \cr
#' ============ \tab ===========  \cr
#' N            \tab Number of subjects \cr
#' MEAN         \tab Arithmetic mean \cr
#' SD           \tab Arithmetic standard deviaion \cr
#' CVPMN        \tab Arithmetic coefficient of variation (percent) \cr
#' CI95MN       \tab 95% confidence interval for arithmetic mean \cr
#' CI90MN       \tab 90% confidence interval for arithmetic mean \cr
#' GMEAN        \tab Geometric mean \cr
#' GSD          \tab Geometric standard deviaion \cr
#' CVPGM        \tab Geometric coefficient of variation (percent) \cr
#' CI95GM       \tab 95% confidence interval for geometric mean \cr
#' CI90GM       \tab 90% confidence interval for geometric mean \cr
#' MEDIAN       \tab Median \cr
#' MIN          \tab Maximum value \cr
#' MAX          \tab Minimum value \cr
#' }
#' @param table_number Character string with table number information, added to the title for each table, if defined
#' @param table_split_by Column to split tables by (defaults to NULL, hence, only single table produced)
#' @param table_head Common information for split to be displayed above table
#' @param table_cols Columns to be displayed before the summary statistics
#' @param table_compare Single column defining groups for which the statistics are displayed side-by-side
#' @param labels Customized labels for columns that substitute the column name in the IQdataNCA object. Applies to 'table_head', 'table_cols', and the entries of 'table_compare'
#' @param roundfun Rounding function to be used (defaults to [signif])
#' @param digits Number of digits to be used for rounding
#' @param fontsize Fontsize to be used in the table
#' @param page_orientation Page orientation, 'landscape' or 'portrait'
#' @param filename Filename to export the table IQRrmd object to
#'
#' @return An IQRrmd object exported to a file
#' @export
#' @family TLF customized
#' @examples
#' \dontrun{
#' pending
#' }
table_summary_PKconc_IQdataNCA <- function (
    data,
    stratify_by      = c("GROUPN", "PROFILE"),
    statistics       = c("MEAN", "SD", "CVPMN", "GMEAN", "CVPGM", "MEDIAN", "MIN", "MAX"),
    table_number     = NULL,
    table_split_by   = NULL,
    table_head       = c("AGE", "RACE", "SEX"),
    table_cols       = c("PROFILE","GROUP"),
    table_compare    = NULL,
    labels           = NULL,
    roundfun         = signif,
    digits           = c("MEAN" = 3, "SD" = 3, "CVPMN" = 3),
    fontsize         = 8,
    page_orientation = c("landscape", "portrait"),
    filename         = NULL
) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  if (!is.character(stratify_by)) stopIQR("'stratify_by' needs to be character vector.")
  if (!(is.character(table_head)|is.null(table_head))) stopIQR("'table_head' needs to be character vector or NULL.")
  if (!is.character(table_cols)) stopIQR("'table_cols' needs to be character vector.")
  if (!all(stratify_by %in% names(data))) stopIQR(paste0("Unknown column specified as 'stratify_by': ", paste0(setdiff(table_split_by, names(data)), collapse = ", ")))
  if (!all(table_split_by %in% names(data))) stopIQR(paste0("Unknown column specified as 'table_split_by': ", paste0(setdiff(table_split_by, names(data)), collapse = ", ")))
  if (!all(table_head %in% names(data))) stopIQR(paste0("Unknown column specified as 'table_head': ", paste0(setdiff(table_head, names(data)), collapse = ", ")))
  if (!all(table_cols %in% names(data))) stopIQR(paste0("Unknown column specified as 'table_cols': ", paste0(setdiff(table_cols, names(data)), collapse = ", ")))
  if (length(intersect(table_cols, table_compare)) > 0) stopIQR("'table_cols' and 'table_compare' need to be distinct.")
  NTall <- sort(unique(data$NTIME))
  data <- removeFrom_IQdataNCA(data = data,
                               FLAGremoveIGNOREI  = TRUE,
                               FLAGremoveIGNORER  = TRUE,
                               FLAGremoveIGNORSUM = TRUE,
                               FLAGremoveIGNORNCA = FALSE)
  message("Generating PK concentration summary tables ...")
  if (!is.null(filename)) {
    filename <- paste0(gsub(".rmd","",filename),".rmd")
  }
  if (!is.function(roundfun)) if (!all(sapply(roundfun, class) %in% "function")) stopIQR("'roundfun' needs to be function or list of functions.")
  data <- data[!is.na(data$NTIME),]
  if (!is.null(table_split_by)) {
    data$SPLIT <- data[[table_split_by]]
  } else {
    data$SPLIT <- "all"
  }
  allSPLIT <- unique(data$SPLIT)
  check_common <- unique(as.data.frame(data)[,c("SPLIT",table_head), drop = FALSE])
  idx_dub <- which(duplicated(check_common$SPLIT))
  if (length(idx_dub) > 0) {
    stopIQR("Values to be displayed as subtitle ('table_head') not unique for the split used.")
  }
  check_grouping <- unique(as.data.frame(data)[,c(stratify_by, table_cols, table_compare), drop = FALSE])
  idx_dub1 <- which(duplicated(check_grouping[,stratify_by, drop = FALSE]))
  idx_dub2 <- which(duplicated(check_grouping[,c(table_cols, table_compare), drop = FALSE]))
  if (length(idx_dub1) > 0 | length(idx_dub2) > 0) {
    stopIQR("'stratify_by' and 'table_cols'/'table_compare' need to lead to identical stratification.")
  }
  tabs <- lapply(seq_along(allSPLIT), function(k) {
    d <- data[data$SPLIT == allSPLIT[k],]
    tab <- rawtable_summary_PKconc_IQdataNCA(
      d           = d,
      NTall       = NTall,
      table_split_by    = table_split_by,
      stratify_by    = stratify_by,
      table_head = table_head,
      table_cols  = table_cols,
      table_compare = table_compare,
      labels  = labels,
      statistics  = statistics,
      split_index  = k,
      table_number = table_number,
      filename    = filename,
      roundfun    = roundfun,
      digits      = digits
    )
    tab
  })
  mytitle <- .table_summary_pkconc
  mytitle <- updateListingNumberTitle_IQdataNCA(mytitle,table_number,listingindex=NULL)
  tabs <- list(content = tabs,
               title = mytitle,
               filename = filename,
               number = table_number,
               type = "table")
  class(tabs) <- c("IQncaTableList", class(tabs))
  if (!is.null(filename)) {
    write_IQncaTableList(tabs, fontsize = fontsize, filename, page_orientation = page_orientation)
  } else {
    return(tabs)
  }
}
#' Generate a single split of statistical summaries of pharmacokinetic concentrations
#'
#' @param d Split of IQdataNCA object
#' @param stratify_by Columns that define the groups that are summarized
#' @param statistics Statistics to be calculated
#' @param table_number Character string with table number information, added to the title for each table, if defined
#' @param split_index Indec of the current split
#' @param table_split_by Column that the split was done by (for labelling)
#' @param table_head Common information for split to be displayed above table
#' @param table_cols Columns to be displayed before the summary statistics
#' @param table_compare Single column defining groups for which the statistics are displayed side-by-side
#' @param labels Customized labels for columns that substitute the column name in the IQdataNCA object. Applies to 'table_head', 'table_cols', and the entries of 'table_compare'
#' @param NTall Number of nominal time points
#' @param roundfun Rounding function to be used (defaults to [signif])
#' @param digits Number of digits to be used for rounding
#' @param filename Filename to export the table IQRrmd object to
#'
#' @return IQncaTable object
rawtable_summary_PKconc_IQdataNCA <-  function (
    d,
    stratify_by,
    statistics,
    table_number=NULL,
    split_index,
    table_split_by,
    table_head,
    table_cols,
    table_compare,
    labels,
    NTall,
    roundfun = signif,
    digits,
    filename
    ) {
  deflabels <- get_default_labels(d)
  mylabels  <- c(deflabels[setdiff(names(deflabels), names(labels))], labels)
  NTmissing <- setdiff(NTall,unique(d$NTIME))
  d$GROUPING <- ""
  for (k in seq_along(stratify_by)) {
    if (k == 1) {
      d$GROUPING <- d[[stratify_by[k]]]
    } else {
      d$GROUPING <- paste0(d$GROUPING, "_", d[[stratify_by[k]]])
    }
  }
  d <- dplyr::arrange(d, GROUPING, NTIME)
  dS1 <- split(d, d$GROUPING)
  tab <- do.call(rbind, lapply(seq_along(dS1), function(kgr) {
    dd <- dS1[[kgr]]
    dS2 <- split(dd, dd$NTIME)
    res <- do.call(rbind,lapply(seq_along(dS2), function (knt) {
      d3 <- dS2[[knt]]
      if (nrow(d3)==0) return (NULL)
      out <- calc_summary_stats(d3, value_col = "CONC", digits = digits, roundfun = roundfun)
      out <- cbind(data.frame(NTIME = d3$NTIME[1]), out)
      out <- dplyr::mutate_all(out, as.character)
      out
    }))
    if (!all(statistics %in% names(res))) {
      statmiss <- setdiff(statistics, names(res))
      stopIQR(paste0("Unknown statistic: ", paste0(statmiss, collapse = ", ")))
    }
    res <- res[, c("NTIME", statistics)]
    resT <- tidyr::pivot_longer(res, -NTIME, names_to = "Statistic", values_to = "Value")
    resT$GROUPING <- dd$GROUPING[1]
    stat_labels <- stats::na.omit(mylabels[unique(resT$Statistic)])
    for (kst in seq_along(stat_labels)) {
      stat <- names(stat_labels)[kst]
      labe <- stat_labels[[kst]]
      resT$Statistic[which(resT$Statistic == stat)] <- labe
    }
    resT
  }))
  tab <- dplyr::left_join(
    tab,
    unique(d[,c("GROUPING", table_cols, table_compare)]),
    by = "GROUPING"
  )
  tab <- tab[, c(table_cols, "NTIME", "Statistic", "Value", table_compare)]
  wide_col <- NULL
  if (!is.null(table_compare)) {
    wide_col <- unique(tab[[table_compare]])
    tab <- tidyr::pivot_wider(tab, names_from = dplyr::all_of(table_compare), values_from = "Value")
  }
  conserved_cols <- c("IX", "ATIME", "TIME", "PCDTC", "EXSTDTC", "CONC", "BLLOQ", "LLOQ", "IGNORE", "COMMENT", wide_col)
  for (col in setdiff(names(tab), conserved_cols)) {
    tab[[col]] <- remove_duplicates(tab[[col]])
  }
  for (k in seq_along(mylabels)) {
    tab       <- addLabel(tab, names(mylabels)[k], mylabels[k])
  }
  loadSetupOptions_IQnca()
  title <- .table_summary_pkconc
  title <- updateTableNumberTitle_IQdataNCA(title,table_number,split_index)
  if (!is.null(table_split_by)) {
    if (table_split_by %in% names(mylabels)) {
      split_label <- mylabels[[table_split_by]]
    } else {
      split_label <- table_split_by
    }
    title <- paste0(title, " - ", split_label, ": ", d[[table_split_by]][1])
  }
  subtitle <- lapply(table_head, function(cc) {
    d[[cc]][1]
  })
  names(subtitle) <- table_head
  names(subtitle)[stats::na.omit(match(names(mylabels), table_head))] <- stats::na.omit(mylabels[table_head])
    scriptName <- getScriptName_IQdataNCA()
    blloqtext <- paste0("Lower limit of quantitation: ",d$LLOQ[1]," ",d$CONCUNIT[1],". Pre-first dose <LLOQ values were handled as: ",gsub("lloq","LLOQ",d$FLGBLQPR[1]),". ",
                        "<LLOQ values between >=LLOQ values were handled as: ",gsub("lloq","LLOQ",d$FLGBLQIN[1]),". ",
                        "First <LLOQ value post last >= LLOQ was handled as: ",gsub("lloq","LLOQ",d$FLGBLQP1[1]),". ",
                        "Second to last First <LLOQ value post last >= LLOQ was handled as: ",gsub("lloq","LLOQ",d$FLGBLQPO[1]),".")
    NTmissingText <- NULL
    if (length(NTmissing) > 1) NTmissingText <- paste0("No evaluable concentration information for nominal time: ",paste0(NTmissing,collapse=", ")," ",getTIMEUNITname_IQdataNCA(d),"s\n")
    footertext <- paste0(
      NTmissingText,
      "N: Number of non-missing values, NC: Not calculated, SD: Standard deviation \n",
      "Geo-mean: Geometric mean.\n",
      "CV%% mean = coefficient of variation (%%)=SD/mean * 100.\n",
      "CV%% geo-mean=(sqrt (exp (variance for log transformed data)-1)) * 100.\n",
      blloqtext,"\n",
      "IQnca version: ", aux_version(pkgName = "IQnca"), "\n",
      "Script: ", scriptName,"\n",
      "Output: ",filename, "\n",
      "Execution date: ", Sys.time()
    )
    out <- list(
      content  = tab,
      title    = title,
      subtitle = subtitle,
      footer   = footertext,
      number   = table_number,
      index    = split_index,
      type     = "table"
    )
    class(out) <- c("IQncaTable", class(out))
    out
}
#' Generate a table of statistical summaries of pharmacokinetic parameters
#'
#' PK parameters are summarized by a set of statistical values.
#' The function acknowleges the reliability of NCA parameters
#' and only includes reliable ones.
#'
#' @param data IQnca object with individual PK parameters
#' @param stratify_by Columns that define the groups that are summarized
#' @param statistics Statistics to be calculated and displayed. Defined by charater vector of shortnames:
#' \tabular{ll}{
#' Short name   \tab Description  \cr
#' ============ \tab ===========  \cr
#' N            \tab Number of subjects \cr
#' MEAN         \tab Arithmetic mean \cr
#' SD           \tab Arithmetic standard deviaion \cr
#' CVPMN        \tab Arithmetic coefficient of variation (percent) \cr
#' CI95MN       \tab 95% confidence interval for arithmetic mean \cr
#' CI90MN       \tab 90% confidence interval for arithmetic mean \cr
#' GMEAN        \tab Geometric mean \cr
#' GSD          \tab Geometric standard deviaion \cr
#' CVPGM        \tab Geometric coefficient of variation (percent) \cr
#' CI95GM       \tab 95% confidence interval for geometric mean \cr
#' CI90GM       \tab 90% confidence interval for geometric mean \cr
#' MEDIAN       \tab Median \cr
#' MIN          \tab Maximum value \cr
#' MAX          \tab Minimum value \cr
#' }
#' @param table_number Character string with table number information, added to the title for each table, if defined
#' @param table_split_by Column to split tables by (defaults to NULL, hence, only single table produced)
#' @param table_head Common information for split to be displayed above table
#' @param table_cols Columns to be displayed before the summary statistics
#' @param table_pkpars Character string defining the PK parameters to report in the table
#'   By default these parameters will be selected based on the "Standard" parameters defined in the IQparamNCA_Specification.xlsx
#'   document, subject to the profile and administration type. The user can select "All" or "Standard".
#'   Alternatively, parameterReport can also be a vector or parameter names (based on PKPARAMCD in IQparamNCA_Specification.xlsx).
#'   Instead of the standard or all ones then these will be considered in the listing (also subject to profile and
#'   administration type).
#' @param labels Customized labels for columns that substitute the column name in the IQdataNCA object. Applies to 'table_head', 'table_cols', and the entries of 'table_compare'
#' @param roundfun Rounding function to be used (defaults to [signif])
#' @param digits Number of digits to be used for rounding
#' @param fontsize Fontsize to be used in the table
#' @param page_orientation Page orientation, 'landscape' or 'portrait'
#' @param filename Filename to export the table IQRrmd object to
#' @md
#'
#' @return An IQRrmd object exported to a file or IQncaTableList
#' @export
#' @family TLF customized
#'
#' @examples
#' \dontrun{
#' pending
#' }
table_summary_PKpars_IQnca <- function (
    data,
    stratify_by      = c("GROUPN", "PROFILE"),
    statistics    = c("MEAN", "SD", "CVPMN", "GMEAN", "CVPGM", "MEDIAN", "MIN", "MAX"),
    table_number   = NULL,
    table_split_by      = NULL,
    table_head   = c("COMPOUND", "MATRIX", "ANALYTE"),
    table_cols    = c("PROFILE","GROUP"),
    table_pkpars = "standard",
    labels    = NULL,
    roundfun      = signif,
    digits        = c("MEAN" = 3, "SD" = 3, "CV" = 3),
    fontsize = 8,
    page_orientation = c("landscape", "portrait"),
    filename      = NULL
) {
  if (!is_IQdataNCA(data)) stopIQR("data is not an IQdataNCA object")
  if (!is.character(stratify_by)) stopIQR("'stratify_by' needs to be character vector.")
  if (!(is.character(table_head)|is.null(table_head))) stopIQR("'table_head' needs to be character vector or NULL.")
  if (!is.character(table_cols)) stopIQR("'table_cols' needs to be character vector.")
  if (!all(stratify_by %in% names(data))) stopIQR(paste0("Unknown column specified as 'stratify_by': ", paste0(setdiff(table_split_by, names(data)), collapse = ", ")))
  if (!all(table_split_by %in% names(data))) stopIQR(paste0("Unknown column specified as 'table_split_by': ", paste0(setdiff(table_split_by, names(data)), collapse = ", ")))
  if (!all(table_head %in% names(data))) stopIQR(paste0("Unknown column specified as 'table_head': ", paste0(setdiff(table_head, names(data)), collapse = ", ")))
  if (!all(table_cols %in% names(data))) stopIQR(paste0("Unknown column specified as 'table_cols': ", paste0(setdiff(table_cols, names(data)), collapse = ", ")))
  if (!is.null(filename)) {
    filename <- paste0(gsub(".rmd","",filename),".rmd")
  }
  message("Generating PK parameter summary tables ...")
  info <- getValidityPKparam_IQRnca(data = data, parameterReport = table_pkpars)
  pkparamInfo <- info$pkparamInfo
  userSelected <- info$userSelected
  if (!userSelected) {
    table_pkpars <- unique(pkparamInfo$PARAMCD)
  } else {
    pkparamInfo <- pkparamInfo[pkparamInfo$PARAMCD %in% table_pkpars,]
  }
  non_IGNOREI <- unique(pkparamInfo$USUBJID[is.na(pkparamInfo$IGNOREI)])
  pkparamInfo <- pkparamInfo[pkparamInfo$USUBJID %in% non_IGNOREI,]
  non_REASONNA <- nchar(aux_strtrim(pkparamInfo$REASONNA))==0
  pkparamInfo <- pkparamInfo[non_REASONNA,]
  non_REASONNOTRELIABLE <- nchar(aux_strtrim(pkparamInfo$REASONNOTRELIABLE))==0
  pkparamInfo <- pkparamInfo[non_REASONNOTRELIABLE,]
  cols_from_input <- unique(c("USUBJID", "PROFILE", stratify_by, table_cols, table_head, table_split_by))
  pardata <- dplyr::left_join(
    pkparamInfo,
    unique(data[,cols_from_input]),
    by = intersect(names(pkparamInfo), cols_from_input)
  )
  if (!is.null(digits)){
    digit_is_common <- is.null(names(digits)) & is.numeric(digits) & length(digits) == 1
    digit_is_commonstat <- !is.null(names(digits)) & is.numeric(digits)
    digit_is_list <- !is.null(names(digits)) & is.list(digits) & all(sapply(digits, class) %in% "numeric")
    if (!(digit_is_common | digit_is_commonstat | digit_is_list)) stopIQR("'digits' not given in proper format.")
  }
  if (class(digits) == "list") {
    mydigits <- lapply(table_pkpars, function(k) NA)
    names(mydigits) <- table_pkpars
    for (park in names(digits)) {
      if (park %in% table_pkpars) {
        mydigits[[park]] <- digits[[park]]
      } else {
        warningIQR("Digit(s) for rounding given for ", park, " which is not selected, thus not applied.")
      }
    }
  } else {
    mydigits <- lapply(table_pkpars, function(k) { if (is.null(digits)) return(NA) else return(digits) } )
    names(mydigits) <- table_pkpars
  }
  if (!is.null(roundfun)){
    rf_is_common <- is.null(names(roundfun)) & is.function(roundfun)
    rf_is_commonstat <- !is.null(names(roundfun)) & is.list(roundfun) & all(sapply(roundfun, class) %in% "function")
    rf_is_list <- !is.null(names(digits)) & is.list(roundfun) & all(sapply(roundfun, class) %in% "list")
    if (rf_is_list) rf_is_list <- all(sapply(purrr::flatten(roundfun), class) %in% "function")
    if (!(rf_is_common | rf_is_commonstat | rf_is_list)) stopIQR("'roundfun' not given in proper format.")
  }
  if (class(roundfun) == "list" & all(sapply(roundfun, class) %in% "list")) {
    myroundfun <- lapply(table_pkpars, function(k) signif)
    names(myroundfun) <- table_pkpars
    for (park in names(roundfun)) {
      if (park %in% table_pkpars) {
        myroundfun[[park]] <- roundfun[[park]]
      } else {
        warningIQR("Function(s) for rounding given for ", park, " which is not selected, thus not applied.")
      }
    }
  } else {
    myroundfun <- lapply(table_pkpars, function(k) roundfun )
    names(myroundfun) <- table_pkpars
  }
  if (!is.null(table_split_by)) {
    pardata$SPLIT <- pardata[[table_split_by]]
  } else {
    pardata$SPLIT <- "all"
  }
  allSPLIT <- unique(pardata$SPLIT)
  check_common <- unique(as.data.frame(pardata)[,c("SPLIT",table_head), drop = FALSE])
  idx_dub <- which(duplicated(check_common$SPLIT))
  if (length(idx_dub) > 0) {
    stopIQR("Values to be displayed as subtitle ('table_head') not unique for the split used.")
  }
  check_grouping <- unique(as.data.frame(data)[,c(stratify_by, table_cols), drop = FALSE])
  idx_dub1 <- which(duplicated(check_grouping[,stratify_by, drop = FALSE]))
  idx_dub2 <- which(duplicated(check_grouping[,table_cols, drop = FALSE]))
  if (length(idx_dub1) > 0 | length(idx_dub2) > 0) {
    stopIQR("'stratify_by' and 'table_cols' need to lead to identical stratification.")
  }
  tabs <- lapply(seq_along(allSPLIT), function(k) {
    d <- pardata[pardata$SPLIT == allSPLIT[k],]
    tab <- rawtable_summary_PKpars_IQnca(
      d           = d,
      table_split_by    = table_split_by,
      stratify_by    = stratify_by,
      table_head = table_head,
      table_cols  = table_cols,
      table_pkpars = table_pkpars,
      labels  = labels,
      statistics  = statistics,
      split_index  = k,
      table_number = table_number,
      filename    = filename,
      roundfun    = myroundfun,
      digits      = mydigits
    )
    tab
  })
  mytitle <- .table_summary_pkparametersbygroup
  mytitle <- updateListingNumberTitle_IQdataNCA(mytitle,table_number,listingindex=NULL)
  mytitle <- gsub(" by dose group", "", mytitle)
  tabs <- list(content = tabs,
               title = mytitle,
               filename = filename,
               number = table_number,
               type = "table")
  class(tabs) <- c("IQncaTableList", class(tabs))
  if (!is.null(filename)) {
    write_IQncaTableList(tabs, fontsize = fontsize, filename, page_orientation = page_orientation)
  } else {
    return(tabs)
  }
}
#' Generate a single table of statistical summaries of pharmacokinetic parameters
#'
#' @param d Split of IQdataNCA object
#' @param split_index Indec of the current split
#' @param table_number Character string with table number information, added to the title for each table, if defined
#' @param table_split_by Column that the split was done by (for labelling)
#' @param stratify_by Columns that define the groups that are summarized
#' @param table_head Common information for split to be displayed above table
#' @param table_cols Columns to be displayed before the summary statistics
#' @param table_pkpars Character string defining the PK parameters to report in the table
#' @param labels Customized labels for columns that substitute the column name in the IQdataNCA object. Applies to 'table_head', 'table_cols', and the entries of 'table_compare'
#' @param statistics Statistics to be calculated
#' @param roundfun Rounding function to be used (defaults to [signif])
#' @param digits Number of digits to be used for rounding
#' @param filename Filename to export the table IQRrmd object to
#'
#' @return IQncaTable object
#'
rawtable_summary_PKpars_IQnca <- function(
    d,
    stratify_by,
    statistics,
    table_number=NULL,
    split_index,
    table_split_by,
    table_head,
    table_cols,
    table_pkpars,
    labels,
    roundfun = signif,
    digits,
    filename
){
  deflabels <- get_default_labels(attr(d, "data"))
  mylabels  <- c(deflabels[setdiff(names(deflabels), names(labels))], labels)
  d$GROUPING <- ""
  for (k in seq_along(stratify_by)) {
    if (k == 1) {
      d$GROUPING <- d[[stratify_by[k]]]
    } else {
      d$GROUPING <- paste0(d$GROUPING, "_", d[[stratify_by[k]]])
    }
  }
  d <- dplyr::arrange(d, GROUPING)
  dS1 <- split(d, d$GROUPING)
  tab <- do.call(rbind, lapply(seq_along(dS1), function(kgr) {
    dd <- dS1[[kgr]]
    dS2 <- split(dd, dd$PARAMCD)
    res <- do.call(rbind,lapply(seq_along(dS2), function (kpp) {
      d3 <- dS2[[kpp]]
      if (nrow(d3)==0) return ()
      digits_par   <- digits[[d3$PARAMCD[1]]]
      roundfun_par <- roundfun[[d3$PARAMCD[1]]]
      out <- calc_summary_stats(d3, value_col = "VALUE", digits = digits_par, roundfun = roundfun_par)
      out <- cbind(data.frame(PARAMCD = d3$PARAMCD[1]), out)
      if (out$SD     == "NC") out$SD     <- "-^b^"
      if (out$CI95MN == "NC") out$CI95MN <- "-^b^"
      if (out$CI90MN == "NC") out$CI90MN <- "-^b^"
      if (out$CVPMN  == "NC") out$CVPMN  <- "-^c^"
      if (out$GMEAN  == "NC") out$GMEAN  <- "-^d^"
      if (out$CVPGM  == "NC") out$CVPGM  <- "-^e^"
      if (out$CI95GM == "NC") out$CI95GM <- "-^e^"
      if (out$CI90GM == "NC") out$CI90GM <- "-^e^"
      out <- dplyr::mutate_all(out, as.character)
      out
    }))
    res <- res[, c("PARAMCD", statistics)]
    resT <- tidyr::pivot_longer(res, -PARAMCD, names_to = "Statistic", values_to = "Value")
    resT <- tidyr::pivot_wider(resT, names_from = "PARAMCD", values_from = "Value")
    missingpar <- setdiff(table_pkpars, names(resT))
    for (kk in seq_along(missingpar)) {
      resT[[missingpar[kk]]] <- "-"
      resT[[missingpar[kk]]][resT$Statistic == "N"] <- "0"
    }
    resT$GROUPING <- dd$GROUPING[1]
    par_labels <- unique(dd[,c("PARAMCD", "NAME", "UNIT")])
    par_labels$LABEL <- paste0(par_labels$NAME, " (",par_labels$UNIT,")")
    for (k in seq_len(nrow(par_labels))) {
      resT <- addLabel(resT, par_labels$PARAMCD[k], par_labels$LABEL[k])
    }
    stat_labels <- stats::na.omit(mylabels[unique(resT$Statistic)])
    for (kst in seq_along(stat_labels)) {
      stat <- names(stat_labels)[kst]
      labe <- stat_labels[[kst]]
      resT$Statistic[which(resT$Statistic == stat)] <- labe
    }
    resT
  }))
  tab <- dplyr::left_join(
    tab,
    unique(d[,c("GROUPING", table_cols)]),
    by = "GROUPING"
  )
  tab <- tab[, c(table_cols, "Statistic", table_pkpars)]
  cols_to_clean <- table_cols
  tab_grouping <- tab[,cols_to_clean]
  for (kcol in seq_along(cols_to_clean)) {
    if (kcol == 1) {
      tab[[cols_to_clean[kcol]]] <- remove_duplicates(tab[[cols_to_clean[kcol]]])
    } else {
      col_grouping <- do.call(paste, c(tab_grouping[1:kcol], sep = "_"))
      tab[[cols_to_clean[kcol]]] <- remove_duplicates(tab[[cols_to_clean[kcol]]], group = col_grouping)
    }
  }
  for (k in seq_along(mylabels)) {
    tab       <- addLabel(tab, names(mylabels)[k], mylabels[k])
  }
  tab <- addLabel(tab, "N", "N^a^")
  loadSetupOptions_IQnca()
  title <- .table_summary_pkparametersbygroup
  title <- updateTableNumberTitle_IQdataNCA(title,table_number,split_index)
  title <- gsub(" by dose group", "", title)
  if (!is.null(table_split_by)) {
    if (table_split_by %in% names(mylabels)) {
      split_label <- mylabels[[table_split_by]]
    } else {
      split_label <- table_split_by
    }
    title <- paste0(title, " - ", split_label, ": ", d[[table_split_by]][1])
  }
  subtitle <- lapply(table_head, function(cc) {
    d[[cc]][1]
  })
  names(subtitle) <- table_head
  names(subtitle)[stats::na.omit(match(names(mylabels), table_head))] <- stats::na.omit(mylabels[table_head])
  scriptName <- getScriptName_IQdataNCA()
  footertext <- paste0(
    "CV%% = coefficient of variation (%%)=SD/mean * 100.\n",
    "Geo-mean: Geometric mean.\n",
    "CV%% geo-mean=(sqrt (exp (variance for log transformed data)-1)) * 100.\n",
    "^a^ Number of (reliably) calculated parameters.\n",
    "^b^ not calculated when N=1.\n",
    "^c^ not calculated when the mean is 0 or N=1.\n",
    "^d^ not calculated when the minimum value is zero.\n",
    "^e^ not calculated when the minimum value is zero or N=1.\n",
    "\n",
    "IQRnca version: ", aux_version(pkgName = "IQnca"), "\n",
    "Script: ", scriptName,"\n",
    "Output: ",filename, "\n",
    "Execution date: ", Sys.time()
  )
  out <- list(
    content  = tab,
    title    = title,
    subtitle = subtitle,
    footer   = footertext,
    number   = table_number,
    index    = split_index,
    type     = "table"
  )
  class(out) <- c("IQncaTable", class(out))
  out
}
#' @import ggplot2
NULL
#' Loading IQnca setup options
#'
#' These options are stored in the installed R package as the file
#' "setup_options_IQnca.R". Typically the sysadmin or the user itself can
#' edit this file. In addition, the user can have an own user setup_options_IQnca.R
#' file located in the users home folder. If present then the contents of this
#' file are loaded after the contents of the general file, allowing the user
#' to override some of the general settings.
#'
#' Note that while the contents of the general file might change from installed
#' version to version, the contents of the file in the users home folder are
#' not changed. Updates to this file need to be done by the user.
#' @export
loadSetupOptions_IQnca <- function() {
  source(system.file(package="IQnca","setup_options_IQnca.R"))
  if (.ALLOW_USER_SETTINGS_FILE) {
    home__ <- Sys.getenv("HOME")
    file__ <- paste0(home__,"/setup_options_IQnca.R")
    if (file.exists(file__)) {
      source(file__)
    }
  }
}
.onAttach <- function(libname, pkgname) {
  showStartupMessage()
}
.onLoad <- function(libname, pkgname) {
}
.onUnload <- function(libpath) {
}
globalVariables(c(
  ".ALLOW_USER_SETTINGS_FILE",".COMPLIANCE_MODE",".PATH_IQRreport",
  ".RDS_FILES_OUTPUT",".dataSpecPath",".figure_individual_pkconc",
  ".figure_spaghetti_dosenormalized_pkconc",".figure_spaghetti_pkconc",
  ".figure_summary_geommean_dosenorm_pkconc",
  ".figure_summary_geommean_pkconc",".figure_summary_mean_dosenorm_pkconc",
  ".figure_summary_mean_pkconc",".listing_actualtime_pkconc",
  ".listing_conc_pkconc",".listing_concdetailed_pkconc",
  ".listing_excludedanalysis_pkconc",".listing_samplingtimes_pkconc",
  ".paramSpecPath",".table_summary_pkconc","ACONC","ATIME","BLLOQINFO","CONCPLOT",
  "CONCPLOTDN","FILLcolor","GEOMMEAN.VALUE","GEOMMEAN_CI95_HIGH","GROUP","GROUPN",
  "IQRNCA_OVERRIDE_SETTING_COMPLIANCE_MODE","IX","MEAN.VALUE","MEAN_PLUS_SD",
  "NTIME","PROFILE","Result__","STRAT","STRATification","STUDYID","TIME","TIMEPLOT",
  "USUBJID","dataNCA","file.edit","getIQdesktopversion","group","height",
  "isIQdesktop","res","result_paths","scaleHeight","scaleWidth","sd","tempdirIQR",
  "width", ".maxNxticks", "COLNAME",
  ".AUCEXTRAP_MAX",".LAMZNPT_MIN",".R2ADJ_MIN",".SPAN_MIN",
  ".affectedparam_AUCOEXTR_HIGH",".affectedparam_AUCPEXTR_HIGH",
  ".affectedparam_DOSE0_NA",".affectedparam_LAMZNPT_LOW",
  ".affectedparam_LAMZ_NA",".affectedparam_R2ADJ_LOW",
  ".affectedparam_SPAN_LOW",".footnoteChar_AUCOEXTR_HIGH",
  ".footnoteChar_AUCPEXTR_HIGH",".footnoteChar_DOSE0_NA",
  ".footnoteChar_IGNOREDSUBJECT_NA",".footnoteChar_ISSUE_UNCAUGHT_NA",
  ".footnoteChar_LAMZNPT_LOW",".footnoteChar_LAMZ_NA",
  ".footnoteChar_R2ADJ_LOW",".footnoteChar_SPAN_LOW",
  ".listing_pkparameter_pkconc","ADDL","TIME.VARYING","aux_extractObjects",
  ".table_summary_pkparameters",".table_summary_pkparametersbygroup","NAME",
  ".figure_summary_pkconc",
  "stratxyz123","PARAMCD","VALUE",
  "MEAN", "CI95MNl", "CI95MNu", "CI90MNl", "CI90MNu", "CI95GMl", "CI95GMu", "CI90GMl", "CI90GMu",
  "summlin", "summlog", "avglin", "avglog", "varlinl","varlogl","varlinu","varlogu",
  "CONCPLIN", "CONCPLOG", "LINES", "IGNORE",
  "PARAMCOL", "GROUPING"
))
