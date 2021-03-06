#' The NOAA Significant Earthquake Database
#'
#' @source National Geophysical Data Center / World Data Service (NGDC/WDS): Significant Earthquake Database. National Geophysical Data Center, NOAA. doi:10.7289/V5TD9V7K. \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1/}
#' @format A data frame, cleaned with the eq_clean_data() function, with 46 columns. Columns typically used by the package's functions are:
#' \describe{
#'  \item{DATETIME}{The date of the earthquake showing day-month-year in a Date class format.}
#'  \item{EQ_PRIMARY}{Magnitude of the earthquake.}
#'  \item{COUNTRY}{Country where the earthquake took place.}
#'  \item{LOCATION_NAME}{Name of the location where the earthquake took place.}
#'  \item{TOTAL_DEATHS}{Total number of people dead in the earthquake.}
#'  \item{LONGITUDE}{Longitudinal coordinate of the place  where the earthquake took place.}
#'  \item{LATITUDE}{Latitudinal coordinate of the place  where the earthquake took place.}
#' }
#' @examples
#' \dontrun{
#'  EarthquakeDataFrame
#' }
"EarthquakeDataFrame"
