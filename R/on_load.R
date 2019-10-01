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





