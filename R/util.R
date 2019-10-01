#' @importFrom magrittr %>%
#' @export %>%
#' @importFrom Rcpp evalCpp
#' @useDynLib surveysummarize
#===================================================================================================#

#' @export
mget2 <- function(x, envir = sys.frame(sys.parent(1)), warn = T) {
  if (!is.null(x)) {
    ifnotfound <- lapply(x, function(x) {
      if(!exists(x, envir = envir) & warn == TRUE) {
        warning('Variable: ',x,' does not exist. Program will ignore it.')
      }
      return(NULL)
    })
    Filter(
      Negate(is.null),
      mget(x,ifnotfound = ifnotfound,envir= envir)
    )
  } else NULL
}

#' @export
exclude_missing_values <- function(subset, variables) {
  exclude_missing <- sprintf("(!%s %%in%% c('-9','-88','-8','-7','-77','-1'))", variables)
  exclude_missing <- paste0(exclude_missing, collapse = ' & ')
  if (exclude_missing == '') exclude_missing <- subset
  return(exclude_missing)
}


#' @export
add_lables <- function(tbl, values) {

  if (is.null(values)) return()

  for (var in attr(tbl, 'by')) {
    # Codebook value lookup for current variable (case agnostic)
    varlabs <- values[toupper(NAME) == toupper(var)]
    # No updates if lookup does not exist
    if (nrow(varlabs) == 0) next()
    # Make sure the variable values have same class (for merging)
    class(varlabs$VALUE) <- class(tbl[[var]])
    # Merge summary table with codebook values
    merged <- merge(tbl, varlabs, by.x = var, by.y = 'VALUE', all.x = TRUE, sort = F)
    # Replace original summary table variable with labeled values
    tbl[, (var) := merged[,ifelse(toupper(NAME) != toupper(var) | is.na(NAME), get(var), LABEL)]]
    # Coerce variable as a factor and order levels
    tbl[, (var) := factor(get(var), levels = unique(c(varlabs$LABEL, get(var))))]
  }

}

#' @export
merge2 <- function(x, y) {
  if (is.null(y)) return(x)
  suppressWarnings(merge(x, y, suffixes = c('','_duplicate')))
}
