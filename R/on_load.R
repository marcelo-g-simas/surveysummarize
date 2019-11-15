#' @import logging

.onLoad <- function(libname, pkgname) {

  # Construct file path for logging package
  package_logfile <- normalizePath(file.path(Sys.getenv('TEMP'), 'surveysummarizelogs'), mustWork = FALSE)

  # Create logging directory if it does not already exist
  if (!dir.exists(package_logfile)) {
    dir.create(package_logfile)
  }

  # Set up basic configuration to log to daily files
  basicConfig()
  daily_logfile <- normalizePath(file.path(package_logfile, paste0(gsub('-', '_', Sys.Date()), '.log')), mustWork = FALSE)
  addHandler(writeToFile, file = daily_logfile)
  removeHandler('basic.stdout')

  loginfo('================== Loading surveysummarize ==================', logger = 'info')
  for (i in 1:length(R.version)) {
    loginfo(sprintf('Session Info - %s: %s', names(R.version[i]), R.version[i]), logger = 'info')
  }

}

.onAttach <- function(libname, pkgname) {

	# put surveyvisualize functions in namespace on load while avoiding showing a mess of messages (from surveyvisualize's dependency on ggplot2)
	# this also allows us to avoid using Depends: in DESCRIPTION file while still loading surveyvisualize during library(surveysummarize) and installing it when this package is installed with devtools::install_*
	invisible(suppressWarnings(suppressPackageStartupMessages((library(surveyvisualize, quietly=T)))))

  if(interactive()) {
  	packageStartupMessage("_____________________________________________________________________")
    packageStartupMessage("surveysummarize ") # , paste0(packageVersion("surveysummarize"))
    packageStartupMessage("Developed by Westat: https://www.westat.com")
    packageStartupMessage("=====================================================================")
    packageStartupMessage("GitHub page: https://github.com/Westat-Transportation/surveysummarize")
  }

}
