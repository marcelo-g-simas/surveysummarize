#' @title Read HTS data, weights, metadata, etc.
#'
#' @param study character. A valid study name.
#' @param project_path character. The project directory.
#'
#' @details
#' \code{read_data} is a wrapper for reading in variables from the correct csvs
#' and merging the corresponding tables on correct keys. \link[data.table]{fread} and
#' \link[data.table]{merge} are used for performance benefits.
#'
#' @return read_data returns an object of class "HTS.data". It contains the data files and weights necessary for
#' querying the HTS datasets, used primarily by \link[surveysummarize]{summarize_data}.
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
#' # Read the data
#' hts_data <- read_data(
#'   study = 'nirpc_2018',
#'   project_path = 'C:/2018 NIRPC Household Travel Survey'
#' )
#'
#' # Access the data
#' hts_data$data$household     # household data
#' hts_data$data$person        # person data
#' hts_data$data$trip          # trip data
#' hts_data$data$vehicle       # vehicle data
#'
#' # Access the weights
#' hts_data$weights$household  # household weights
#' hts_data$weights$person     # person and trip weights
#' }
#'
#' @export
#' @import data.table
#' @import logging
#' @importFrom yaml read_yaml
#'
read_data <- function(study, project_path) {

  # study name should match name of yaml configuration
  study_paths <- list.files(system.file('studies', package = 'surveysummarize'), full.names = T)
  study_names <- sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(study_paths))
  config_path <- study_paths[study == study_names]

  if (length(config_path) > 0) {

    tryCatch({
      loginfo(paste('Reading YAML configuration file:', config_path))
      config <- read_yaml(config_path, eval.expr = TRUE)
    }, error = function(cond) {
      loginfo(sprintf('Failed to parse YAML configuration file %s. %s', config_path, cond))
      stop(sprintf('Failed to parse YAML configuration file %s. %s', config_path, cond))
    })

  } else {
    logerror(paste('Cannot find configuration for specified study:', study))
    stop('Cannot find configuration for specified study: ', study)
  }

  if (dir.exists(project_path)) {
    # Initalize HTS data object
    hts_obj <- HTS.data$new(config, project_path)
    hts_obj$read_all()
    if(length(hts_obj$config$levels$location)>0) {
      hts_obj$append_location_data()
    }
  } else {
    logerror(paste('Cannot find specified project path:', project_path))
    stop('Cannot find specified project path:\n', project_path)
  }

  # program assumes place table needs converting unless configuration is specified
  hts_obj$config$levels$place$convert_place_to_trip_DECISION <- ifelse(length(hts_obj$config$levels$place$convert_place_to_trip)==0,"TRUE",hts_obj$config$levels$place$convert_place_to_trip)

  if(as.logical(hts_obj$config$levels$place$convert_place_to_trip_DECISION) == TRUE) {

  	hts_obj$documentation$variables <- unique(rbind(hts_obj$documentation$variables,
  		hts_obj$documentation$variables[NAME == 'tpurp' & TABLE == 'place', list(NAME = 'tpurp_origin', TABLE, TYPE, LABEL = paste(LABEL, 'at origin'))],
  		hts_obj$documentation$variables[NAME == 'tpurp' & TABLE == 'place', list(NAME = 'tpurp_o_origin', TABLE, TYPE, LABEL = paste(LABEL, 'at origin'))],
  		hts_obj$documentation$variables[NAME == 'actdur' & TABLE == 'place', list(NAME = 'actdur_origin', TABLE, TYPE, LABEL)],
  		data.table(NAME = 'starttime', TABLE = 'place', TYPE = 'numeric', LABEL = 'Start time (ISO 8601 Date and Time)')
  	))

  	hts_obj$documentation$values <- unique(rbind(hts_obj$documentation$values,
  	  hts_obj$documentation$values[NAME == 'tpurp' & TABLE == 'place', list(NAME = 'tpurp_origin', TABLE, VALUE, LABEL)],
  	  hts_obj$documentation$values[NAME == 'tpurp' & TABLE == 'place', list(NAME = 'tpurp_o_origin', TABLE, VALUE, LABEL)]
  	))

  	hts_obj$documentation$variables[TABLE == 'place', TABLE := 'trip']
  	hts_obj$documentation$values[TABLE == 'place', TABLE := 'trip']

  	hts_obj$documentation$variables <- hts_obj$documentation$variables[TABLE %in% c('household','person','trip','trip','tour')]
  	hts_obj$documentation$values <- hts_obj$documentation$values[TABLE %in% c('household','person','trip','trip','tour')]
  }


  # minimal derived variable support requires this chunk and derived_variables.R
  derived_variable_config <- normalizePath(file.path(project_path, config$metadata$derived_variables$csv), mustWork = FALSE)
  if (length(derived_variable_config) > 0 && file.exists(derived_variable_config)) {
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
    initialize = function(config, project_path) {
      self$config <- config
      self$project_path <- project_path
    },
    print = function(...) {
      cat("<HTS.data> Environment")
    },
    config = NULL,
    project_path = NULL,
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
      self$read_documentation()
      for (table_level in names(self$config$levels)) {

        tryCatch({
          message('=========================================================================')
          message(sprintf('Reading %s data...', table_level))
          self$read_data(table_level)
          loginfo(sprintf('Read %s table successfully', table_level))
        }, error = function(cond) {
          logerror(sprintf('Failed to read %s table. ERROR: %s', table_level, cond))
          stop(sprintf('Failed to read %s table. ERROR: %s', table_level, cond))
        })

        if (table_level %in% c('household','person')) {

          tryCatch({
            message('=========================================================================')
            message(sprintf('Reading %s weights...', table_level))
            self$read_weights(table_level)
            loginfo(sprintf('Read %s weights successfully', table_level))
          }, error = function(cond) {
            logerror(sprintf('Failed to read %s weights. ERROR: %s', table_level, cond))
            stop(sprintf('Failed to read %s weights. ERROR: %s', table_level, cond))
          })

        }
      }
      message('=========================================================================')
    },
    #========================================================================================================#
    read_data = function(table_level) {
      if (table_level == 'place') {
        # program assumes place table needs converting unless configuration is specified
        convert_place_to_trip_DECISION <- ifelse(length(self$config$levels$place$convert_place_to_trip)==0,"TRUE",self$config$levels$place$convert_place_to_trip)
        if(as.logical(convert_place_to_trip_DECISION) == TRUE) {
          self$data[['trip']] <- self$place_to_trip()
        } else {
          level_config <- self$config$levels[[table_level]]
          input_csv <- normalizePath(file.path(self$project_path, level_config$data$csv))
          self$data[['trip']] <- fread(
            input = input_csv,
            key = level_config$id,
            colClasses = 'character'
          )
        }
      } else {
        level_config <- self$config$levels[[table_level]]
        input_csv <- normalizePath(file.path(self$project_path, level_config$data$csv))
        self$data[[table_level]] <- fread(
          input = input_csv,
          key = level_config$id,
          colClasses = 'character'
        )
      }
      # setattr(self$data[[table_level]], 'weight', level_config$weight)
    },
    #========================================================================================================#
    read_weights = function(table_level) {

      level_config <- self$config$levels[[table_level]]

      if (is.null(level_config$weights)) return()

      col_select <- c(level_config$id, level_config$weights$final, level_config$weights$replicates)

      col_classes <- c(
        rep('character',length(level_config$id)),
        rep('numeric', length(c(level_config$weights$final, level_config$weights$replicates)))
      )

      names(col_classes) <- col_select
      input_csv <- normalizePath(file.path(self$project_path, level_config$weights$csv))

      self$weights[[table_level]] <- fread(
        input = input_csv,
        select = col_select,
        colClasses = col_classes,
        key = level_config$id
      )

    },
    #========================================================================================================#
    read_documentation = function() {

      variables_path <- normalizePath(file.path(self$project_path, self$config$metadata$variables$csv), mustWork = F)
      values_path <- normalizePath(file.path(self$project_path, self$config$metadata$values$csv), mustWork = F)

      if (length(variables_path) == 0) {
        logwarn('No variable labels specified.')
        warning('No variable labels specified.')
      } else if (!file.exists(variables_path)) {
        logwarn(sprintf('Could not find variable labels csv %s', variables_path))
        warning(sprintf('Could not find variable labels csv %s', variables_path))
      } else {
        tryCatch({
          loginfo(sprintf('Reading variable labels %s.', variables_path))
          self$documentation$variables <- fread(variables_path, encoding = 'UTF-8')
        }, error = function(cond) {
          logerror(sprintf('Failed to read variable labels %s. %s', variables_path, cond))
          stop(sprintf('Failed to read variable labels %s. %s', variables_path, cond))
        })
      }

      if (length(values_path) == 0) {
        logwarn('No value labels specified.')
        warning('No value labels specified.')
      } else if (!file.exists(values_path)) {
        logwarn(sprintf('Could not find value labels csv %s', values_path))
        warning(sprintf('Could not find value labels csv %s', values_path))
      } else {
        tryCatch({
          loginfo(sprintf('Reading value labels %s.', values_path))
          self$documentation$values <- fread(values_path, encoding = 'UTF-8')
        }, error = function(cond) {
          logerror(sprintf('Failed to read value labels %s. %s', values_path, cond))
          stop(sprintf('Failed to read value labels %s. %s', values_path, cond))
        })
      }

    },
    #========================================================================================================#
    prepare = function(table_level, select = NULL, subset = NULL, annualize = TRUE) {

      if (table_level == 'household') {

        tbls = c('household','household_weights')
        final = self$config$levels$household$weights$final
        replicates = self$config$levels$household$weights$replicates

      } else if (table_level == 'vehicle') {

        tbls = c('vehicle','household','household_weights')
        final = self$config$levels$household$weights$final
        replicates = self$config$levels$household$weights$replicates

      } else if (table_level == 'person') {

        tbls = c('person','person_weights','household')
        final = self$config$levels$person$weights$final
        replicates = self$config$levels$person$weights$replicates

      } else if (table_level == 'trip') {

        tbls = c('trip','person','person_weights','household')
        final = self$config$levels$person$weights$final
        replicates = self$config$levels$person$weights$replicates

      } else if (table_level == 'tour') {

        tbls = c('tour','person','person_weights','household')
        final = self$config$levels$person$weights$final
        replicates = self$config$levels$person$weights$replicates

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
      if (annualize == TRUE & table_level %in% c('trip','tour') & !is.null(final) & !is.null(replicates)) {
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
    select_data = function(table_level, select = colnames(x)) {
      x <- self$data[[table_level]]
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
    },
    #========================================================================================================#
    place_to_trip = function() {

      input_csv <- normalizePath(file.path(self$project_path, self$config$levels$place$data$csv))
      # Read place file
      place <- fread(
        input = input_csv,
        colClasses = 'character'
      )

      # Coerce place ids as numeric for sorting
      for (id in self$config$levels$place$id) {
        place[, (id) := as.numeric(get(id))]
      }

      # Set key/order
      setDT(place, key = self$config$levels$place$id)

      # Copy over variables from place 1
      variables <- c('tpurp','deptime','locno')
      for (var in variables) {
        place[, (paste(var, 'origin', sep = '_')) := shift(get(var)), by = eval(self$config$levels$person$id)]
      }

      # Set trip ids in config
      self$config$levels$trip <- list(id = c(self$config$levels$person$id, 'tripno'))

      # collapse places into trips by removing the first place
      trip <- place[placeno > 1, ]
      setnames(trip, self$config$levels$place$id, self$config$levels$trip$id)
      trip$tripno <- as.integer(trip$tripno) - 1

      # Coerce ids back to character
      for (id in self$config$levels$trip$id) {
        trip[, (id) := as.character(get(id))]
      }

      # Set key
      setkeyv(trip, cols = self$config$levels$trip$id)

      return(trip)
    },
    append_location_data = function() {

      # Location variables to append
      loc_cols <- setdiff(names(self$data$location), c('sampno','perno','locno'))

      # Household
      household_location <- self$data$location[loctype == 1, .SD, .SDcols = c(self$config$levels$household$id, loc_cols)]
      household_loc_cols <- paste('home', loc_cols, sep = '_')
      self$data$household[household_location, (household_loc_cols) := mget(loc_cols), on = self$config$levels$household$id]
      self$data$household[, (household_loc_cols) := lapply(.SD, function(x) ifelse(is.na(x), '-1', x)), .SDcols = household_loc_cols]

      # Person Work
      work_location <- self$data$location[loctype == 2, .SD, .SDcols = c(self$config$levels$person$id, loc_cols)]
      work_loc_cols <- paste('work', loc_cols, sep = '_')
      self$data$person[work_location, (work_loc_cols) := mget(loc_cols), on = self$config$levels$person$id]
      self$data$person[, (work_loc_cols) := lapply(.SD, function(x) ifelse(is.na(x), '-1', x)), .SDcols = work_loc_cols]

      # Person School
      school_location <- self$data$location[loctype == 3, .SD, .SDcols = c(self$config$levels$person$id, loc_cols)]
      school_loc_cols <- paste('school', loc_cols, sep = '_')
      self$data$person[school_location, (school_loc_cols) := mget(loc_cols), on = self$config$levels$person$id]
      self$data$person[, (school_loc_cols) := lapply(.SD, function(x) ifelse(is.na(x), '-1', x)), .SDcols = school_loc_cols]

      # Trip Origin
      loc_id <- self$config$levels$location$id[!self$config$levels$location$id %in% self$config$levels$household$id]
      loc_origin_id <- paste(loc_id, 'origin', sep = '_')
      self$data$location[, (loc_origin_id) := get(loc_id)]
      origin_loc_cols <- paste(loc_cols, 'origin', sep = '_')
      self$data$trip[self$data$location, (origin_loc_cols) := mget(loc_cols), on = c(self$config$levels$household$id, loc_origin_id)]
      self$data$location[, (loc_origin_id) := NULL]
      # Trip Destination
      self$data$trip[self$data$location, (loc_cols) := mget(loc_cols), on = self$config$levels$location$id]

    }
  )
)

