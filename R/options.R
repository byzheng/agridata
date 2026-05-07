# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
AGRIDATA_OPTIONS <- settings::options_manager(
)


#' Set or get options for agridata
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#'
#' @return the default and modified options.
#' @export
#' @examples
#' agridata_options()
agridata_options <- function(...){
    # protect against the use of reserved words.
    settings::stop_if_reserved(...)
    AGRIDATA_OPTIONS(...)
}

#' Reset global options for agridata
#'
#' @return the default options
#' @export
#' @examples
#' agridata_reset()
agridata_reset <- function() {
    settings::reset(AGRIDATA_OPTIONS)
}
