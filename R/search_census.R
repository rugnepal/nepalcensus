#' Search Nepal Census datasets by keyword
#'
#' Convenience wrapper around \code{\link{catalog}} for keyword search.
#' Matches the keyword against dataset titles (case-insensitive).
#'
#' @param keyword Character. Word or phrase to search for in table titles.
#' @param type Character. One of \code{"all"} (default), \code{"household"},
#'   or \code{"individual"}.
#'
#' @return A tibble with matching rows from the data catalog.
#'
#' @examples
#' search_census("disability")
#' search_census("migration", type = "individual")
#' search_census("water")
#'
#' @export
search_census <- function(keyword, type = c("all", "household", "individual")) {
  type <- match.arg(type)
  if (missing(keyword) || !nzchar(keyword)) {
    stop("'keyword' must be a non-empty character string.")
  }
  catalog(search = keyword, type = type)
}
