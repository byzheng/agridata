.silo_api_request <- function(path, query) {
    url <- httr2::url_parse("https://www.longpaddock.qld.gov.au")
    url$path <- path
    url <- httr2::url_modify(url, query = query) |>
        httr2::url_build()

    # Build and perform request
    resp <- httr2::request(url) |>
        httr2::req_perform()

    # Parse response as text (change to resp_body_json if JSON expected)
    result <- httr2::resp_body_string(resp)

    return(result)
}


#' Retrieve Daily Weather Data from SILO PPD
#'
#' Downloads daily weather records from the SILO Patched Point Dataset (PPD) using the SILO API.
#'
#' @details
#' This function accesses the SILO PPD service (see \url{https://www.longpaddock.qld.gov.au/silo/api-documentation/})
#'  to retrieve weather data for a specified station and date range. The data can be returned in various formats and
#'  optionally saved to a file.
#'
#' @param station Character. The SILO PPD station ID.
#' @param start Date. The start date for data retrieval.
#' @param finish Date. The end date for data retrieval.
#' @param email Character. Email address required for API access.
#' @param format Character. Desired data format (e.g., "csv", "json").
#' @param outfile Character. Path to the output file where data will be saved.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side effect of downloading and/or saving data.
#' @export
silo_ppd <- function(station,
                        email,
                        start = as.Date("1889-01-01"),
                        finish = Sys.Date(),
                        format = c("apsim", "standard"),
                        outfile = NULL) {
    stopifnot(is.character(station) | is.numeric(station))
    stopifnot(length(station) == 1)
    stopifnot(length(start) == 1)
    stopifnot(length(finish) == 1)
    stopifnot(class(start) == "Date")
    stopifnot(class(finish) == "Date")
    stopifnot(length(email) == 1)
    stopifnot(is.character(email))

    format <- match.arg(format)

    if (!is.null(outfile)) {
        stopifnot(length(outfile) == 1)
        stopifnot(is.character(outfile))
    }

    start <- format(start, format = "%Y%m%d")
    finish <- format(finish, format = "%Y%m%d")
    path <- "/cgi-bin/silo/PatchedPointDataset.php"
    query <- list(
        station = station,
        format = format,
        start = start,
        finish = finish,
        username = email
    )
    content <- .silo_api_request(path, query)

    if (!is.null(outfile)) {
        writeLines(content, outfile)
    }

    return(content)
}



#' Retrieve Gridded Weather Data from SILO
#'
#' Downloads weather records from the SILO Gridded Dataset API
#' (\url{https://www.longpaddock.qld.gov.au/silo/api-documentation/})
#' for a specified location and date range.
#'
#' @param lon Numeric. Longitude of the location.
#' @param lat Numeric. Latitude of the location.
#' @param start Date or character. Start date of the data request (in "YYYY-MM-DD" format).
#' @param finish Date or character. End date of the data request (in "YYYY-MM-DD" format).
#' @param email Character. Email address required for API access.
#' @param format Character. Data format to request from the API (e.g., "csv", "json").
#' @param outfile Character. Path to the output file where data will be saved.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side effect of downloading and saving the data.
#'
#' @examples
#' \dontrun{
#' get_silo_data(
#'   lon = 151.2,
#'   lat = -27.5,
#'   start = "2020-01-01",
#'   finish = "2020-12-31",
#'   email = "user@example.com",
#'   format = "csv",
#'   output_format = "apsim",
#'   outfile = "weather.apsim"
#' )
#' }
#'
#' @export
silo_grid <- function(lon, lat,
                        email,
                        start = as.Date("1889-01-01"),
                        finish = Sys.Date(),
                        format = c("apsim", "standard"),
                        outfile = NULL) {
    stopifnot(is.numeric(lat))
    stopifnot(is.numeric(lon))
    stopifnot(length(lat) == 1)
    stopifnot(length(lon) == 1)
    stopifnot(lat < -10 & lat > -44)
    stopifnot(lon < 154 & lon > 112)
    stopifnot(length(start) == 1)
    stopifnot(length(finish) == 1)
    stopifnot(class(start) == "Date")
    stopifnot(class(finish) == "Date")
    stopifnot(length(email) == 1)
    stopifnot(is.character(email))

    format <- match.arg(format)

    if (!is.null(outfile)) {
        stopifnot(length(outfile) == 1)
        stopifnot(is.character(outfile))
    }
    start <- format(start, format = "%Y%m%d")
    finish <- format(finish, format = "%Y%m%d")
    path <- "/cgi-bin/silo/DataDrillDataset.php"
    query <- list(
        lat = lat,
        lon = lon,
        format = format,
        start = format(start, format = "%Y%m%d"),
        finish = format(finish, format = "%Y%m%d"),
        username = email
    )
    content <- .silo_api_request(path, query)

    if (!is.null(outfile)) {
        writeLines(content, outfile)
    }

    return(content)
}
