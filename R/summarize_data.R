#' Summarize HTS Data
#'
#' Create weighted aggregate tables using HTS data.
#'
#' @param data Object returned by \link[surveysummarize]{read_data}.
#' @param agg Aggregate function label. Either "household_count", "person_count", "trip_count",
#' "sum", "avg", "median", "household_trip_rate", or "person_trip_rate". See \emph{Aggregates} section
#' @param agg_var Character string specifying a numeric variable over which to aggregate.
#' Only relavent when agg is "sum", "avg", or "median"
#' @param by Character vector of one or more variable names to group by. See \emph{Analysis Groups} section.
#' @param subset Character string containing a pre-aggregation subset condition using \link[data.table]{data.table} syntax.
#' See \emph{Filter} section.
#' @param label logical. Use labels for table output?
#' @param prop logical. Use proportions for count aggregates?
#' @param prop_by Character vector of one or more variable names by which to group proportions.
#' @param exclude_missing logical. Exclude missing responses from summary.
#' @return data.table object aggregated by input specifications containing the following fields:
#'
#' \itemize{
#'   \item \code{by} variables. For each \code{by} variable, a column of the same name is created.
#'   They will appear in the order they are listed as \link[base]{factor}s ordered by their codebook values.
#'   \item \strong{Estimate} - Weighted statistic.
#'   \item \strong{SE} - Standard error of the weighted statistic.
#'   \item \strong{Survey} - Surveyed/sampled statistic.
#'   \item \strong{N} - Number of observations/sample size.
#' }
#'
#' @section Aggregates (\code{agg}):
#' What type of aggregate are you interested in?
#'
#' \subsection{Frequencies / Proportions}{
#'   \itemize{
#'     \item \strong{household_count} - Count of households
#'     \item \strong{person_count} - Count of persons
#'     \item \strong{trip_count} - Count of trips
#'     \item \strong{vehicle_count} - Count of vehicles
#'   }
#'   \emph{*Use} \code{prop = TRUE} \emph{in combination with a count aggregate to get the proportion.}
#' }
#'
#' \subsection{Numeric Aggregates (Sum / Average / Median)}{
#'   \emph{Must also specify a numeric aggregate variable using the} \code{agg_var} \emph{parameter.}
#'   \itemize{
#'     \item \strong{sum} - Sum of \code{agg_var}
#'     \item \strong{avg} - Arithmetic mean of \code{agg_var}
#'     \item \strong{median} - Median of \code{agg_var}
#'   }
#' }
#'
#' \subsection{Trip Rates (Daily Person Trips per Person/Household)}{
#'   Simply put, the count of trips divided by the count of persons or households.
#'   \itemize{
#'     \item \strong{household_trip_rate} - Daily trips per household.
#'     \item \strong{person_trip_rate} - Daily trips per person.
#'   }
#' }
#'
#' @section Analysis Groups (\code{by}):
#' By which variables to you wish to aggregate?
#'
#' Similar to \code{GROUP BY} in SQL or a \code{CLASS} statement in SAS.
#' There is no limit to the number of variables specified in the character vector, however many \code{by} variables
#' can result in groups with small sample sizes which need to be interpreted carefully.
#'
#' The data.table returned by summarize_data will include a column (of class \link[base]{factor}) for each \code{by} variable specified.
#'
#' @section Filtering (\code{subset}):
#' Which households/person/trips do you wish to include or exclude?
#'
#' Similar to \code{WHERE} in SQL, \code{subset} allows you to filter observations/rows in the dataset before summarizing/aggregating.
#'
#' \code{subset} is a string that will be evaluated as a logical vector indicating the rows to keep.
#' As mentioned above, the string will be evaluated as the \code{i} index in a \link[data.table]{data.table}.
#' In short, similar to the base function \link[base]{subset},
#' there is no need to specify the data object in which the variables are included
#' (i.e.: your code would look like \code{"var < 10"} instead of \code{"data$var < 10"}).
#'
#' Any variable (or combination of variables) found in the codebook can be used in the subset condition.
#' See \link[base]{Logic} for a refresher on R's logical operators when using more than one logical condition.
#'
#' \subsection{Quoting within quotes}{
#' You will frequently need to include quotes in your string. You can tackle this a few different ways.
#' The following examples would all evaluate the same way:
#'   \itemize{
#'     \item \code{"HHSTATE \%in\% c('GA','FL')"}
#'     \item \code{'HHSTATE \%in\% c("GA","FL")'}
#'     \item \code{"HHSTATE \%in\% c(\"GA\",\"FL\")"}
#'   }
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
#' summarize_data(
#'   data = hts_data,             # Using the hts_data object,
#'   agg = 'person_trip_rate',    # calculate the person trip rate
#'   by = 'sex',                  # by gender
#'   subset = 'emply_ask == "1"'  # for workers
#' )
#' }
#'
#'
#' @export
summarize_data <- function(data, agg, agg_var = NULL, by = NULL, subset = NULL,
                           prop = FALSE, prop_by = NULL, exclude_missing = FALSE, use_labels = TRUE) {

  loginfo(paste('Summary Call:', format(match.call())))

  if (!'HTS.data' %in% class(data)) {
    logerror('data is not an "HTS.data" object (returned by the read_data function).')
    stop('data is not an "HTS.data" object (returned by the read_data function).')
  }

  if (!is.null(by) & !is.character(by)) {
    logerror('The by parameter must be a character string or vector.')
    stop('The by parameter must be a character string or vector.')
  }

  valid_agg_labels <- c('household_count','vehicle_count','person_count','trip_count',
                        'avg','median','sum','person_trip_rate','household_trip_rate')

  if (!agg %in% valid_agg_labels) {
    logerror(paste(agg, 'is an invalid agg parameter.'))
    stop('agg', ' is an invalid agg parameter. Please use one of the following:\n', paste(valid_agg_labels, collapse = ', '))
  }

  # Get variables used in the subset condition
  subset_vars <- data$extract_subset_variables(subset)
  if (exclude_missing == T) {
    subset <- exclude_missing_values(subset, by, data$config$constants$missing_values)
  }

  # List all variables that will need to be accessed (excluding IDs/weights)
  select <- unique(c(by, agg_var, subset_vars))

  # Aggregate data according to agg label
  tbl <- switch(EXPR = agg,
    #================================================================================================#
    # COUNT AGGREGATES
    #================================================================================================#
    household_count = data$prepare('household', select, subset) %>% count(by, prop, prop_by),

    person_count = data$prepare('person', select, subset) %>% count(by, prop, prop_by),

    trip_count = data$prepare('trip', select, subset) %>% count(by, prop, prop_by),

    tour_count = data$prepare('tour', select, subset) %>% count(by, prop, prop_by),

    vehicle_count = data$prepare('vehicle', select, subset) %>% count(by, prop, prop_by),
    #================================================================================================#
    # NUMERIC AGGREGATES
    #================================================================================================#
    avg = {
      aggregate_level <- data$get_levels(agg_var)
      data$prepare(aggregate_level, select, subset) %>% num_agg(by, agg_var, mean, Rcpp_wgtavg)
    },

    median = {
      aggregate_level <- data$get_levels(agg_var)
      data$prepare(aggregate_level, select, subset) %>% num_agg(by, agg_var, median, Rcpp_wgtmed)
    },

    sum = {
      aggregate_level <- data$get_levels(agg_var)
      data$prepare(aggregate_level, select, subset) %>% num_agg(by, agg_var, sum, Rcpp_wgtsum)
    },
    #================================================================================================#
    # TRIP AND TOUR RATES
    #================================================================================================#
    household_trip_rate = {
      rate(
        numerator = data$prepare('trip', select, subset, annualize = F),
        denominator = data$prepare('household', select, subset),
        by = by
      )
    },

    person_trip_rate = {
      rate(
        numerator = data$prepare('trip', select, subset, annualize = F),
        denominator = data$prepare('person', select, subset),
        by = by
      )
    },

    household_tour_rate = {
      rate(
        numerator = data$prepare('tour', select, subset, annualize = F),
        denominator = data$prepare('household', select, subset),
        by = by
      )
    },

    person_tour_rate = {
      rate(
        numerator = data$prepare('tour', select, subset, annualize = F),
        denominator = data$prepare('person', select, subset),
        by = by
      )
    }
    #================================================================================================#
  )


  # Warn if prop = T with non-count aggregates
  if (prop == T & !agg %in% valid_agg_labels[grepl('_count$', valid_agg_labels)]) {
    logwarn('Can only calculate proportions for count aggregates. Ignoring parameter "prop = TRUE".')
    warning('Can only calculate proportions for count aggregates. Ignoring parameter "prop = TRUE".')
  }

  # Set Table Attributes
  setattr(tbl, 'by', by)
  setattr(tbl, 'error', 'Standard Error')
  setattr(tbl, 'prop', prop)

  if (use_labels == TRUE) {

    loginfo('Using variable and value labels.')
    add_lables(tbl, data$documentation$values)
    agg_var_label <- data$documentation$variables[NAME %in% agg_var, LABEL]
    setattr(tbl, 'by_label', sapply(by, function(x) data$documentation$variables[NAME == x, LABEL], simplify = F))

  } else {

    logwarn('Not using variable and value labels.')
    tbl[, (by) := lapply(.SD, factor), .SDcols = by]
    agg_var_label <- agg_var
    setattr(tbl, 'by_label', sapply(by, function(x) x, simplify = F))

  }

  # Set More Table Attributes
  setattr(tbl, 'agg_var', agg_var_label)
  setattr(tbl, 'agg_label', switch(agg,
    household_count = 'Household Frequency',
    vehicle_count = 'Vehicle Count',
    person_count = 'Person Frequency',
    trip_count = 'Trip Frequency',
    tour_count = 'Tour Frequency',
    sum = paste('Sum of', agg_var_label),
    avg = paste('Average', agg_var_label),
    median = paste('Median', agg_var_label),
    person_trip_rate = 'Person Trip Rate',
    household_trip_rate = 'Household Trip Rate',
    person_tour_rate = 'Person Tour Rate',
    household_tour_rate = 'Household Tour Rate'
  ))

  # Assign S3 class to table
  class(tbl) <- c(class(tbl), 'HTS.summary.table')

  # Garbage collection
  invisible(gc())

  setnames(tbl,
    old = c('W','E','S'),
    new = c('Estimate','SE','Survey'),
    skip_absent = TRUE
  )

  return(tbl[])
}

#========================================================================#

#' @export
count <- function(data, by, prop = FALSE, prop_by = NULL) {

  final <- attr(data, 'final')
  replicates <- attr(data, 'replicates')

  # Different handling for unweighted and weighted datasets
  if (is.null(final) || is.null(replicates)) {

    loginfo('Calculating unweighted count...')

    # Aggregates
    out <- data[, list(W = NA, S = .N, N = .N), keyby = mget2(by)]

    # Calculate proportions
    if (prop == TRUE) {
      out[, S := as.double(S)]
      out[, S := prop.table(S), by = prop_by]
    }

    # Place holder for Standard Error
    out[, E := NA]

  } else {

    loginfo('Calculating weighted/unweighted count...')

    # Aggregates
    out <- data[, {
      #==========================================#
      W = list(sum(get(final))) # Final
      R = lapply(.SD, sum)      # Replicates
      #==========================================#
      c(W = W, S = .N, N = .N, R)
      #==========================================#
    }, keyby = mget2(by), .SDcols = replicates]

    # Calculate proportions
    if (prop == TRUE) {

      loginfo('Converting counts to proportions...')

      out[, W := prop.table(W), by = prop_by]
      out[, S := as.double(S)]
      out[, S := prop.table(S), by = prop_by]
      out[, (replicates) := lapply(.SD, prop.table), by = prop_by, .SDcols = replicates]
    }

    loginfo('Calculating standard error from replicate weights...')
    # Calculate standard error using replicates
    out[, E := jk_se(W, .SD), .SD = replicates]
    out[, (replicates) := NULL]

  }

	# if variable is entirely numeric (naturally or coded), order it numerically
	for(i in by) {
		if(any(grepl("[A-z&,;.!?\\/-]", out[, mget(i)][[1]]))) {
		} else {
			setDF(out)
			out[, i] <- as.numeric(out[, i])
			setDT(out)
		}
	}

  return(out[, list(W, E, S, N), keyby = mget2(by)])

}

#========================================================================#

#' @export
num_agg <- function(data, by, agg_var, fun, wfun) {

  gt0 <- sprintf('%s >= 0', agg_var)
  final <- attr(data, 'final')
  replicates <- attr(data, 'replicates')

  if (is.null(final) || is.null(replicates)) {

    loginfo('Calculating unweighted numeric aggregate...')

    data[eval(parse(text = gt0)), {
      #=========================================================#
      x = as.numeric(get(agg_var))  # Convert agg_var to numeric
      S = list(fun(x))              # Surveyed stat
      #=========================================================#
      c(W = NA, E = NA, S = S, N = .N)
      #=========================================================#
    } , keyby = mget2(by)]

  } else {

    loginfo('Calculating weighted/unweighted numeric aggregate and standard error...')

    data[eval(parse(text = gt0)), {
      #=========================================================#
      x = as.numeric(get(agg_var))  # Convert agg_var to numeric
      S = list(fun(x))              # Surveyed stat
      W = list(wfun(x, get(final))) # Final weighted stat
      R = lapply(.SD, wfun, x = x)  # Replicate weighted Stats
      E = jk_se(W, R)               # Standard Error
      #=========================================================#
      c(W = W, E = E, S = S, N = .N)
      #=========================================================#
    } , keyby = mget2(by), .SDcols = replicates]

  }

}

#========================================================================#

#' @export
rate <- function(numerator, denominator, by) {

  # NUMERATOR
  final_numerator <- attr(numerator, 'final')
  replicates_numerator <- attr(numerator, 'replicates')

  # DENOMINATOR
  final_denominator <- attr(denominator, 'final')
  replicates_denominator <- attr(denominator, 'replicates')


  #========================================================================#

  if (is.null(final_numerator) | is.null(replicates_numerator) | is.null(final_denominator) | is.null(replicates_denominator)) {

    loginfo('Calculating unweighted rate aggregate...')

    # NUMERATOR
    numerator_agg <- numerator[, list(W = NA, S = .N, N = .N, R = NA), keyby = mget2(by)]
    # DENOMINATOR
    denominator_agg <- denominator[, list(W = NA, S = .N, N = .N, R = NA), keyby = mget2(by, warn = FALSE)]

    if (nrow(denominator_agg) > 1) {
      merged_counts <- merge(numerator_agg, denominator_agg, suffixes = c('_numerator','_denominator'))
      num_cols <- names(merged_counts)[grepl('_numerator$', names(merged_counts))]
      den_cols <- names(merged_counts)[grepl('_denominator$', names(merged_counts))]
      trip_rate <- merged_counts[, ..num_cols] / merged_counts[, ..den_cols]
      trip_rate_table <- cbind(numerator_agg[, .SD, .SDcols = c(key(numerator_agg), 'N')], trip_rate)
      setnames(trip_rate_table, c('W_numerator','S_numerator'), c('W','S'))
    } else {
      trip_rate <- numerator_agg[, .SD, .SDcols = colnames(denominator_agg)] / denominator_agg[rep(1, nrow(numerator_agg)),]
      trip_rate_table <- cbind(numerator_agg[, .SD, .SDcols = c(key(numerator_agg), 'N')], trip_rate)
    }

    # Standard error placeholder
    trip_rate_table[, E := NA]

  } else {

    loginfo('Calculating weighted/unweighted rate aggregate...')

    # NUMERATOR
    numerator_agg <- numerator[, {
      W = list(sum(get(final_numerator))) # Final Weight
      R = lapply(.SD, sum)      # Replicate Weight
      names(R) = paste0('W', 1:length(R))
      c(W = W, S = .N, N = .N, R)
    } , keyby = mget2(by), .SDcols = replicates_numerator]

    # DENOMINATOR
    denominator_agg <- denominator[, {
      W = list(sum(get(final_denominator))) # Final Weight
      R = lapply(.SD, sum)      # Replicate Weight
      names(R) = paste0('W', 1:length(R))
      c(W = W, S = .N, R)
    } , keyby = mget2(by, warn = FALSE), .SDcols = replicates_denominator]

    if (nrow(denominator_agg) > 1) {
      merged_counts <- merge(numerator_agg, denominator_agg, suffixes = c('_numerator','_denominator'))
      num_cols <- names(merged_counts)[grepl('_numerator$', names(merged_counts))]
      den_cols <- names(merged_counts)[grepl('_denominator$', names(merged_counts))]
      trip_rate <- merged_counts[, ..num_cols] / merged_counts[, ..den_cols]
      trip_rate_table <- cbind(numerator_agg[, .SD, .SDcols = c(key(numerator_agg), 'N')], trip_rate)
      replicate_cols <- grep('^W[0-9]+_numerator$', names(trip_rate_table))
      setnames(trip_rate_table, c('W_numerator','S_numerator'), c('W','S'))
    } else {
      trip_rate <- numerator_agg[, .SD, .SDcols = colnames(denominator_agg)] / denominator_agg[rep(1, nrow(numerator_agg)),]
      trip_rate_table <- cbind(numerator_agg[, .SD, .SDcols = c(key(numerator_agg), 'N')], trip_rate)
      replicate_cols <- grep('^W[0-9]+$', names(trip_rate_table))
    }

    loginfo('Calculating standard error from replicate weights...')

    # Calculate standard error and remove unnecessary columns
    trip_rate_table[, E := jk_se(W, .SD), .SDcols = replicate_cols]
    trip_rate_table[, (replicate_cols) := NULL]

  }

  return(trip_rate_table[, .SD, .SDcols = c(key(trip_rate_table), 'W', 'E', 'S', 'N')])
}

#========================================================================#

#' @export
jk_se <- function(final_weights, replicate_weights, coefficient = getOption('HTS.jk_coeff')) {
  final_weights <- matrix(
    unlist(final_weights, use.names = F),
    ncol = length(final_weights)
  )
  replicate_weights <- matrix(
    unlist(replicate_weights, use.names = F),
    ncol = length(replicate_weights)
  )
  dif <- sweep(replicate_weights, 1, final_weights) ** 2
  E <- apply(dif, 1, function(x) sqrt(coefficient * sum(x, na.rm = T)))
  return(E)
}
