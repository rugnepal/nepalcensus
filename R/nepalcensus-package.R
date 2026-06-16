#' nepalcensus: Nepal Population and Housing Census 2021 Data
#'
#' Provides tidy access to 89 tabular datasets from the Nepal Population
#' and Housing Census 2021 (NPHC 2078 BS). Datasets cover household
#' characteristics and individual-level demographics cross-tabulated by
#' province, district, and local level.
#'
#' @section Household datasets (Hhld01--Hhld23):
#' Housing ownership, construction materials, utilities, sanitation,
#' household amenities, death records, and absent population abroad.
#'
#' @section Individual datasets (Indv01--Indv71):
#' Population by sex, age, nationality, marital status, disability,
#' literacy, education, migration, fertility, economic activity,
#' occupation, industry, employment status, and birth registration.
#'
#' @section Helper functions:
#' \describe{
#'   \item{\code{\link{catalog}}}{Browse or search the data catalog.}
#'   \item{\code{\link{search_census}}}{Keyword search across dataset titles.}
#' }
#'
#' @source National Statistics Office (NSO) of Nepal.
#'   \url{https://censusnepal.cbs.gov.np}
#'
#' @docType package
#' @name nepalcensus
#' @aliases nepalcensus-package
"_PACKAGE"

utils::globalVariables("datacatalog")


#' Data catalog for Nepal Census 2021
#'
#' A reference table listing all 89 datasets available in this package,
#' with their table numbers, titles, dataset names, and source URLs.
#'
#' @format A tibble with 89 rows and 4 variables:
#' \describe{
#'   \item{table_no}{Character. Table identifier (e.g., "Household 01",
#'     "Individual 09").}
#'   \item{title}{Character. Full descriptive title of the table.}
#'   \item{dataset}{Character. Dataset name as loaded in R (e.g., "Hhld01",
#'     "Indv09"). Pass to \code{data()} to load.}
#'   \item{href}{Character. Source URL on the NSO census download portal.}
#' }
#' @source National Statistics Office (NSO) of Nepal, NPHC 2021.
#'   \url{https://censusnepal.cbs.gov.np/results/downloads/census-dataset}
#' @examples
#' data(datacatalog)
#' head(datacatalog)
#'
#' # Find all migration-related tables
#' datacatalog[grepl("migrat", datacatalog$title, ignore.case = TRUE), ]
"datacatalog"
