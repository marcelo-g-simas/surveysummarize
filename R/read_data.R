#' @title Read HTS data and weights.
#'
#' @param dataset The study year of the dataset. Currently supports "2001" and "2009".
#' @param select A character vector of NHTS variable names to select for analysis. Defaults to all variables in the codebook.
#' @param csv_path The parent directory of "/csv/dataset/". Defaults to working directory.
#'
#' @details
#' \code{read_data} is a wrapper for reading in variables from the correct csvs
#' and merging the corresponding tables on correct keys. \link[data.table]{fread} and
#' \link[data.table]{merge} are used for performance benefits.
#'
#' @return read_data returns an object of class "HTS.data". It contains the data files and weights necessary for
#' querying the NHTS dataset, used primarily by \link[summarizeNHTS]{summarize_data}.
#'
#' The "HTS.data" object is essentially a list of data.tables broken up by data and weights.
#'
#' Accessing the data:
#' \itemize{
#'   \item \strong{household} - Household data file
#'   \item \strong{person} - Person data file
#'   \item \strong{trip} - Trip data file
#'   \item \strong{vehicle} - Vehicle data file
#' }
#'
#' Accessing the weights:
#' \itemize{
#'   \item \strong{household} - Household weight file. Used for weighting household and vehicle data.
#'   \item \strong{person} - Person weight file. Also includes trip weights at the person level.
#' }
#'
#' @examples
#' \donttest{
#' # Read 2009 NHTS data with specified csv path:
#' nhts_data <- read_data('2009', csv_path = 'C:/NHTS')
#'
#' # Access the data
#' nhts_data$data$household     # household data
#' nhts_data$data$person        # person data
#' nhts_data$data$trip          # trip data
#' nhts_data$data$vehicle       # vehicle data
#'
#' # Access the weights
#' nhts_data$weights$household  # household weights
#' nhts_data$weights$person     # person and trip weights
#' }
#'
#' @export
#' @import data.table
#' @importFrom tools file_path_sans_ext
#' @importFrom yaml read_yaml
#'
read_data <- function(config) {

  if (missing(config) & interactive()) {

    # Get study configurations from package install location
    study_directory <- system.file('studies', package = 'hatstats')
    study_files <- file_path_sans_ext(list.files(study_directory))

    # Choose from a list of studies
    hts_choice <- select.list(
      choices = c('--New Household Travel Survey--', study_files),
      preselect = '--New Household Travel Survey--',
      title = 'Select a Household Travel Survey:',
      graphics = TRUE
    )

    # Load study configuration. If new, opens default text editor for configuration edits.
    if (hts_choice == '--New Household Travel Survey--') {
      tmp <- tempfile(pattern = 'new_hts_config', fileext = '.yaml')
      config_file_path <- file.path(study_directory, '.config.yaml')
      file.copy(config_file_path, tmp)
      try(edit(file = tmp), silent = T)
      config <- read_yaml(tmp, eval.expr = TRUE)
      save_config <- askYesNo(paste0('Save new configuration?\n', dQuote(config$study_name)))

      if (save_config == TRUE) {
        study_file_path <- file.path(study_directory, paste0(config$study_name, '.yaml'))
        file.copy(tmp, study_file_path)
        config <- read_yaml(study_file_path, eval.expr = TRUE)
      }

      unlink(tmp)

    } else if (hts_choice == '') {
      warning('No Household Travel Survey selected.')
      return(NULL)
    } else {
      study_file_path <- file.path(study_directory, paste0(hts_choice, '.yaml'))
      config <- read_yaml(study_file_path, eval.expr = TRUE)
    }

  } else if (missing(config) & !interactive()) {
    stop('The config parameter must be specified in non-interactive environments.')
  }

  if (is.character(config) && grepl('.yaml', config)) {
    config <- read_yaml(config, eval.expr = TRUE)
  }

  # Initalize HTS data object
  hts_obj <- HTS.data$new(config)
  hts_obj$read_all()

  # minimal derived variable support requires this chunk and derived_variables.R
  derived_variable_config <- file.path(hts_obj$config$data_directory, 'derived_variable_config.csv')
  if (file.exists(derived_variable_config)) {
    derived_variables(hts_obj, derived_variable_config)
  }

  # Add constants to globabl options
  constants <- hts_obj$config$constants
  names(constants) <- paste('HTS', names(hts_obj$config$constants), sep = '.')
  options(constants)

  return(hts_obj)
}

#' @importFrom R6 R6Class
HTS.data <- R6Class("HTS.data",
  public = list(
    initialize = function(config) {
      self$config <- config
    },
    print = function(...) {
      cat("<HTS.data> Environment")
    },
    config = NULL,
    data = list(
      trip = NULL,
      tour = NULL,
      person = NULL,
      household = NULL,
      vehicle = NULL
    ),
    weights = list(
      household = NULL,
      person = NULL
    ),
    documentation = list(
      values = NULL,
      variables = NULL
    ),
    read_all = function() {
      self$path_check()
      self$read_documentation()
      for (table_level in names(self$config$levels)) {
        message('=========================================================================')
        message(sprintf('Reading %s data...',table_level))
        self$read_data(table_level)
        if (table_level %in% c('household','person')) {
          message('=========================================================================')
          message(sprintf('Reading %s weights...',table_level))
          self$read_weights(table_level)
        }
      }
      message('=========================================================================')
    },
    path_check = function() {
      if(length(list.files(self$config$data_directory, '.csv', ignore.case = T)) == 0) {
        stop(
          "\nThe directory below does not exist or does not contain csv files.\n", self$config$data_directory,
          "\n- Make sure the correct path is specified."
        )
      }
    },
    #========================================================================================================#
    read_data = function(table_level) {
      config <- self$config$levels
      config <- config[[table_level]]
      self$data[[table_level]] <- fread(
        input = file.path(self$config$data_directory, config$csv$data),
        key = config$key,
        colClasses = 'character'
      )
      setattr(self$data[[table_level]], 'weight', config$weight)
    },
    #========================================================================================================#
    read_weights = function(table_level) {
      config <- self$config$levels[[table_level]]
      if (is.null(config$csv$weights) | is.null(config$weights)) return()
      col_select <- c(config$key, config$weights$final, config$weights$replicates)
      col_classes <- c(
        rep('character',length(config$key)),
        rep('numeric', length(c(config$weights$final, config$weights$replicates)))
      )
      names(col_classes) <- col_select
      self$weights[[table_level]] <- fread(
        input = file.path(self$config$data_directory, config$csv$weights),
        select = col_select,
        colClasses = col_classes,
        key = config$key
      )
      setattr(self$weights[[table_level]], 'final', config$weights$final)
      setattr(self$weights[[table_level]], 'replicates', config$weights$replicates)
    },
    #========================================================================================================#
    read_documentation = function() {

      config <- self$config$documentation
      variables_path <- file.path(self$config$data_directory, config$variables)
      values_path <- file.path(self$config$data_directory, config$values)

      if (!is.null(config$variables) && file.exists(variables_path)) {
        self$documentation$variables <- fread(variables_path, encoding = 'UTF-8')
      }

      if (!is.null(config$values) && file.exists(values_path)) {
        self$documentation$values <- fread(values_path, encoding = 'UTF-8')
      }

    },
    #========================================================================================================#
    prepare = function(level, select = NULL, subset = NULL, annualize = TRUE) {

      if (level == 'household') {

        tbls = c('household','household_weights')
        final = attr(self$weights$household, 'final')
        replicates = attr(self$weights$household, 'replicates')

      } else if (level == 'vehicle') {

        tbls = c('vehicle','household','household_weights')
        final = attr(self$weights$household, 'final')
        replicates = attr(self$weights$household, 'replicates')

      } else if (level == 'person') {

        tbls = c('person','person_weights','household')
        final = attr(self$weights$person, 'final')
        replicates = attr(self$weights$person, 'replicates')

      } else if (level == 'trip') {

        tbls = c('trip','person','person_weights','household')
        final = attr(self$weights$person, 'final')
        replicates = attr(self$weights$person, 'replicates')

      } else if (level == 'tour') {

        tbls = c('tour','person','person_weights','household')
        final = attr(self$weights$person, 'final')
        replicates = attr(self$weights$person, 'replicates')

      }

      if (is.null(subset)) subset <- TRUE
      # subset <- deparse(substitute(subset))

      # Merge tables according to level configuration
      x <- Reduce(f = merge2,
        list(
          household = self$select_data('household', select),
          person = self$select_data('person', select),
          trip = self$select_data('trip', select),
          tour = self$select_data('tour', select),
          vehicle = self$select_data('vehicle', select),
          household_weights = self$weights$household,
          person_weights = self$weights$person
        )[tbls]
      )[eval(parse(text = subset))]

      # Set weight names for future reference
      setattr(x, 'final', final)
      setattr(x, 'replicates', replicates)

      annualized_days <- getOption('HTS.annualized_days')

      # Annualize weights for trip and tour levels
      if (annualize == TRUE & level %in% c('trip','tour') & !is.null(final) & !is.null(replicates)) {
        x[, (final) := .SD * annualized_days, .SDcols = final]
        x[, (replicates) := lapply(.SD, function(x) x * annualized_days), .SD = replicates]
      }

      return(x)

    },
    #========================================================================================================#
    get_levels = function(variables) {
      table_variables <- lapply(self$data, function(x) {
        any(variables %in% colnames(x))
      })
      names(which(unlist(table_variables)))
    },
    #========================================================================================================#
    select_data = function(level, select = colnames(x)) {
      x <- self$data[[level]]
      cols <- colnames(x)
      keys <- key(x)
      final_weight <- attributes(x)$weight$final
      select <- c(keys, select[select %in% cols & !select %in% final_weight])
      x[, .SD, .SDcols = select]
    },
    #========================================================================================================#
    extract_subset_variables = function(condition) {
      if(is.null(condition)) return(NULL)
      subset_vars <- sapply(self$data, function(x) {
        col_ref <- sapply(colnames(x), grepl, x = condition)
        if (length(col_ref) > 0) {
          names(which(col_ref))
        }
      })
      unlist(subset_vars, use.names = FALSE)
    }
  )
)

