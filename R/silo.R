


#' Get PPD data from SILO
#'
#' @details
#' Get weather record from SILO PPD (https://www.longpaddock.qld.gov.au/silo/api-documentation/)
#'
#' @param station ppd station id
#' @param start start date
#' @param finish finish date
#' @param email email address
#' @param format data format
#' @param outfile output file
#'
#' @return No return values
#' @export
silo_ppd <- function(station,
                      email,
                      start = as.Date("1889-01-01"),
                      finish = Sys.Date(),
                      format = "apsim",
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
    url <- paste0('https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?station=',
                  station, '&format=', format, '&start=',
                  start, '&finish=', finish,
                  '&username=', email)
    message("Get SILO PPD with ", url)
    resp <- httr::GET(url)
    response <- httr::content(resp, "text")
    if (is.null(outfile)) {
        return(response)
    }
    writeLines(response, outfile)
}




#' Get gridded data from SILO
#'
#' @details
#' Get weather record from SILO Gridded Dataset (https://www.longpaddock.qld.gov.au/silo/api-documentation/)
#'
#' @param lon longitude
#' @param lat latitude
#' @param start start date
#' @param finish finish date
#' @param email email address
#' @param format data format
#' @param outfile output file
#'
#' @return No return values
#' @export
silo_grid <- function(lon, lat,
                      email,
                      start = as.Date("1889-01-01"),
                      finish = Sys.Date(),
                      format = "apsim",
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
    url <- paste0('https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php?lat=',
                  lat, '&lon=', lon, '&format=', format, '&start=',
                  start, '&finish=', finish,
                  '&username=', email)
    message("Get SILO Gridded Data with ", url)
    resp <- httr::GET(url)
    response <- httr::content(resp, "text")
    if (is.null(outfile)) {
        return(response)
    }
    writeLines(response, outfile)
}

