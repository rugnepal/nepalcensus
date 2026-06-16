#' Browse the Nepal Census data catalog
#'
#' Returns the data catalog tibble listing all 89 datasets in this package.
#' Optionally filter by a keyword matched against table titles.
#'
#' @param search Character. Optional keyword to filter titles
#'   (case-insensitive substring match). If \code{NULL} (default),
#'   all 89 rows are returned.
#' @param type Character. One of \code{"all"} (default), \code{"household"},
#'   or \code{"individual"} to restrict to Hhld or Indv datasets.
#'
#' @return A tibble with columns \code{table_no}, \code{title},
#'   \code{dataset}, and \code{href}.
#'
#' @examples
#' # Full catalog
#' catalog()
#'
#' # Search for education-related tables
#' catalog(search = "education")
#'
#' # Household tables only
#' catalog(type = "household")
#'
#' # Combine filters
#' catalog(search = "absent", type = "household")
#'
#' @export
catalog <- function(search = NULL, type = c("all", "household", "individual")) {
  type <- match.arg(type)
  out <- datacatalog

  if (type == "household") {
    out <- out[grepl("^Household", out$table_no), ]
  } else if (type == "individual") {
    out <- out[grepl("^Individual", out$table_no), ]
  }

  if (!is.null(search)) {
    out <- out[grepl(search, out$title, ignore.case = TRUE), ]
  }

  out
}
