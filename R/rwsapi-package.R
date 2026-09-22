#' @name rwsapi
#'
#  @aliases rwsapi
#'
#' @title rwsapi: A package to access data on water quality monitoring in the Netherlands
#'
#' @description
#' Rijkswaterstaat has been collecting water quality data from all Dutch surface waters since the late 1970's.
#' These data and the archive system, DONAR (Data Opslag NAtte Rijkswaterstaat), have been publicly available
#' in different forms over the years, including the waterbase website, and stand-alone application, WADI.
#' Currently, an API is available that serves a website, hosted by Rijkswaterstaat (https://waterinfo.rws.nl).
#' This API is also immediately accesible for application development. The renovated archive system that now
#' harbours the DONAR data is called WADAR (WAter DAta Rijkswaterstaat).
#'
#' This R-package implements a number of functions to access the DONAR/WADAR data from within R using the JSON
#' framework. Beware that because WADAR is implemented in Dutch, many (field)names and descriptions will be in
#' Dutch, while function names and parameter names are in English.
#'
#' The following functions can be explored via the R-helpfile system:
#' \describe{
#'   \item{\code{\link{rws_metadata}} }{Download the data catalog}
#'   \item{\code{\link{rws_observations}} }{Download water quality data}
#'   \item{\code{\link{get_selected_metadata}} }{Obtain an overview of parameter metadata with location where they are measured}
#'   \item{\code{\link{getDDLdata}} }{NOG TE BEKIJKEN}
#'   \item{\code{\link{getDDLdata_by_year}} }{NOG TE BEKIJKEN}
#'   \item{\code{\link{rws_getLocations}} }{Obtain locations for a list of quantities and parameter codes}
#'   \item{\code{\link{rws_getParameters}} }{Obtain parameters for a list of locations}
#' }
#' @import jsonlite httr dplyr rlist sf
#' @importFrom readr write_delim
#' @importFrom stringr str_replace
#' @importFrom stringr str_pad
NULL
