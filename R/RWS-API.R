# ##################### #################### #################### #################### #################### #################### ####################
# Copyright (C) 2025  Willem Stolte                                                                                                                  #
#                                                                                                                                                   #
#    This program is free software: you can redistribute it and/or modify                                                                           #
#    it under the terms of the GNU General Public License as published by                                                                           #
#    the Free Software Foundation, either version 3 of the License, or                                                                              #
#    (at your option) any later version.                                                                                                            #
#                                                                                                                                                   #
#    This program is distributed in the hope that it will be useful,                                                                                #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of                                                                                 #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                                                                                  #
#    GNU General Public License for more details.                                                                                                   #
#                                                                                                                                                   #
#    You should have received a copy of the GNU General Public License                                                                              #
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.                                                                         #
#                                                                                                                                                   #
# ##################### #################### #################### #################### #################### #################### ####################
# This code is based on information provided in https://github.com/hadley/httr/blob/master/vignettes/api-packages.Rmd



#' @title Download the data catalog of the long-term monitoring program of Rijkswaterstaat (NL)
#'
#' @aliases catalog datacatalog
#'
#' @description Obtain a list with tables containing the metadata and datatypes relevant
#'    to query and interpret observational data from the long-term monitoring program of
#'    Rijkswaterstaat.
#'
#' @param filterList Named list of boolean values to select properties that should be
#'                   included in the catalog. Defaults to all TRUE. See details for an
#'                   explanation of the names.
#'
#' @return
#' The return object is a list consisting of 3 named elements:
#' \describe{
#'  \item{content}{
#     A named list containing various vectors and data.frames describing the different
#'    datatypes that come with queries for observations. See details for more explanation.
#'  }
#'  \item{path}{
#'    The API resource or endpoint that was used to obtain the catalog.
#'  }
#'  \item{response}{
#'    An object of the class response as defined in the \code{\link{httr-package}} that
#'    represents the response of the server that was given upon the request sent by this
#'    function.
#'  }
#' }
#' @details
#' TO BE WORKED OUT
#'
#' @examples
#' \dontrun{
#' metadata <- rws_metadata()
#' names(metadata)
#' head(metadata$locatielijst)
#' # extract parameter information
#' parameters <- metadata$aquometadatalijst
#' }
#'
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr map_dbl
#'
#' @export
rws_metadata <- function(
    filterList = list(Compartimenten=T,
                      Grootheden=T,
                      Parameters=T,
                      Eenheden=T,
                      Hoedanigheden=T,
                      Typeringen=T,
                      BioTaxon=T,
                      Organen=T
                     )
) {

  # Helper function to translate list names to lower case when names are present
  # This strategy avoids issues due to inconsistent or altered naming conventions in terms of upper- and lower-case use.
    convert_names_to_lowercase <- function(lst) {
      # If the element is a list
      if (is.list(lst)) {
        # Convert names to lowercase
        if(!is.null(names(lst))) names(lst) <- tolower(names(lst))

        # Recursively apply to each element in the list
        lst <- purrr::map(lst, convert_names_to_lowercase)

      }
      return(lst)
    }

  ua <- httr::user_agent("https://github.com/wstolte/rwsapi")
  path <- "/METADATASERVICES/OphalenCatalogus"
  url <- paste0("https://ddapi20-waterwebservices.rijkswaterstaat.nl", path)

  l = list(CatalogusFilter=filterList)

  resp <- httr::POST(url, body = l, encode = "json")

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return application/json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(
    httr::content(resp, "text", encoding = "UTF-8"),
    simplifyVector = TRUE,
    flatten = TRUE
  )

  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "RWS API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  res <- structure(
    list(
      content = convert_names_to_lowercase(parsed),
      path = path,
      response = resp
    )#,
    # class = "rws_api"
  )

  res$content$aquometadatalijst <- as.data.frame(res$content$aquometadatalijst)
  res$content$aquometadatalocatielijst <- as.data.frame(res$content$aquometadatalocatielijst)
  res$content$locatielijst <- as.data.frame(res$content$locatielijst)

  return(res)
}


#' @title Collect observational data from long-term monitoring efforts at Rijkswaterstaat (NL)
#'
#' @description
#' This is the core function of the package to retrieve water quality data from Rijkswaterstaat.
#' It works by supplying a list that can be be interpreted as query in JSON. From the server's
#' response a list is composed with the actual data, the query that was supplied, and a reponse
#' object as defined in the \code{\link{jsonlite}} package.
#'
#' @param bodylist The message body containing criteria for data selection. See \code{\link{jsonlite}} for more information on how to construct.
#' @param trytimes The number of trials to contact the data server before returning with failure.
#'
#' @return A structured list with three elements:
#' \enumerate{
#    \item content   a dataframe containing the actual observations
#'   \item path      the full server address with argument string
#'   \item response  a list of class 'response' (see \link{jsonlite})
#' }
#'
#' @examples
#' \dontrun{
#' options(digits=22)
#'
#' l2 <- list(
#'   AquoPlusWaarnemingMetadata= list(
#'     AquoMetadata = list(
#'       Compartiment = list(Code = "OW"),
#'       Eenheid = list(Code = "cm"),
#'       Grootheid = list(Code = "Hm0"))),
#'   Locatie = list(
#'     Code = "europlatform"),
#'   Periode = list(Begindatumtijd = "2012-01-27T09:00:00.000+01:00",
#'                  Einddatumtijd = "2012-01-28T09:01:00.000+01:00")
#' )
#' observations <- rws_observations(l2)
#' str(observations)
#' }
#'
#' @export
rws_observations <- function (bodylist, trytimes = 3) {

    nullToNA <- function(x) {
      x[sapply(x, is.null)] <- NA
      return(x)
    }

    # Helper function to translate list names to lower case when names are present
    # This strategy avoids issues due to inconsistent or altered naming conventions in terms of upper- and lower-case use.
    convert_names_to_lowercase <- function(lst) {
      # If the element is a list
      if (is.list(lst)) {
        # Convert names to lowercase
        if(!is.null(names(lst))) names(lst) <- tolower(names(lst))

        # Recursively apply to each element in the list
        lst <- purrr::map(lst, convert_names_to_lowercase)
      }
      return(lst)
    }

    warnings = list()

    ua <- httr::user_agent("https://github.com/wstolte/rwsapi")
    path <- "/ONLINEWAARNEMINGENSERVICES/OphalenWaarnemingen"
    url <- paste0("https://ddapi20-waterwebservices.rijkswaterstaat.nl", path)

    resp <- httr::RETRY(verb = "POST",
                  url = url,
                  ua = ua,
                  body = jsonlite::toJSON(bodylist,
                                auto_unbox = T,
                                digits = NA
                               ),
                  httr::add_headers(.headers = c(`Content-Type` = "application/json",
                                           `Ocp-Apim-Subscription-Key` = "my_subscription_key"
                                          )
                             ),
                  times = trytimes
                 )
    if (httr::http_type(resp) != "application/json") {
        stop("API did not return application/json", call. = FALSE)
    }

    if(resp$status_code != 204) {
      response <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),simplifyVector = FALSE)
#    parsed   <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"),simplifyVector = TRUE,flatten = T)
    } else {
    empty_content <- data.frame(
                locatie.message.id                   = c(), locatie.code                 = c(), locatie.naam                        = c(), coordinatenstelsel           = c(),
                geometriepunt.x                      = c(), geometriepunt.y              = c(), tijdstip                            = c(), statuswaarde                 = c(),
                bemonsteringshoogte                  = c(), referentievlak               = c(), opdrachtgevendeinstantie            = c(), kwaliteitswaarde.code        = c(),
                parameter.wat.omschrijving           = c(), bemonsteringsapparaat.code   = c(), bemonsteringsapparaat.omschrijving  = c(), bemonsteringssoort.code      = c(),
                bemonsteringssoort.omschrijving      = c(), biotaxon.code                = c(), biotaxon.omschrijving               = c(), biotaxontype.code            = c(),
                biotaxontype.omschrijving            = c(), compartiment.code            = c(), compartiment.omschrijving           = c(), eenheid.code                 = c(),
                eenheid.omschrijving                 = c(), grootheid.code               = c(), grootheid.omschrijving              = c(), typering.code                = c(),
                typering.omschrijving                = c(), hoedanigheid.code            = c(), hoedanigheid.omschrijving           = c(), meetapparaat.code            = c(),
                meetapparaat.omschrijving            = c(), orgaan.code                  = c(), orgaan.omschrijving                 = c(), parameter.code               = c(),
                parameter.omschrijving               = c(), groepering.code              = c(), groepering.omschrijving             = c(), waardebepalingstechniek.code = c(),
                waardebepalingstechniek.omschrijving = c(), waardebepalingsmethode.code  = c(), waardebepalingsmethode.omschrijving = c(), waardebewerkingsmethode.code = c(),
                waardebewerkingsmethode.omschrijving = c(), numeriekewaarde              = c(), alphanumeriekewaarde                = c()
    )


      return(structure(list(content = empty_content, path = path, response = resp)))
    }

    if (!response$Succesvol) {
        paste("request not successful", response$Foutmelding)
    }

    response <- convert_names_to_lowercase(response)

    df <- data.frame()
    for (ii in seq(1:length(response$waarnemingenlijst))) {
        print(paste("ii in response.waarnemingenlijst: ", ii))
        if (purrr::is_empty(response$waarnemingenlijst)) {
            next
        }
        if (!purrr::is_empty(purrr::map_dbl(response$waarnemingenlijst[[ii]]$metingenlijst,list("meetwaarde", "waarde_numeriek"), .default = NA))) {
            temp.l <- list(locatie.message.id          = response$waarnemingenlijst[[ii]]$locatie$locatie_messageid,
                locatie.code                          = response$waarnemingenlijst[[ii]]$locatie$code,
                locatie.naam                          = response$waarnemingenlijst[[ii]]$locatie$naam,
                coordinatenstelsel                    = response$waarnemingenlijst[[ii]]$locatie$coordinatenstelsel,
                geometriepunt.x                       = response$waarnemingenlijst[[ii]]$locatie$lon,
                geometriepunt.y                       = response$waarnemingenlijst[[ii]]$locatie$lat,
                tijdstip                              = purrr::map_chr(response$waarnemingenlijst[[ii]]$metingenlijst, list("tijdstip"), .default = NA),                                         # using names as accessors since indices are more difficult to debug when changed on the server
                statuswaarde                          = purrr::map_chr(response$waarnemingenlijst[[ii]]$metingenlijst, list("waarnemingmetadata","statuswaarde"), .default = NA),
                bemonsteringshoogte                   = purrr::map_chr(response$waarnemingenlijst[[ii]]$metingenlijst, list("waarnemingmetadata","bemonsteringshoogte"), .default = NA),
                referentievlak                        = purrr::map_chr(response$waarnemingenlijst[[ii]]$metingenlijst, list("waarnemingmetadata","referentievlak"), .default = NA),
                opdrachtgevendeinstantie              = purrr::map_chr(response$waarnemingenlijst[[ii]]$metingenlijst, list("waarnemingmetadata","opdrachtgevendeinstantie"), .default = NA),
                kwaliteitswaarde.code                 = purrr::map_chr(response$waarnemingenlijst[[ii]]$metingenlijst, list("waarnemingmetadata","kwaliteitswaardecode"), .default = NA),
#                aquometadata.message.id = response$waarnemingenlijst[[ii]]$aquometadata$aquometadata_MessageID,
                parameter.wat.omschrijving            = response$waarnemingenlijst[[ii]]$aquometadata$parameter_wat_omschrijving,
                bemonsteringsapparaat.code            = response$waarnemingenlijst[[ii]]$aquometadata$bemonsteringsapparaat$code,
                bemonsteringsapparaat.omschrijving    = response$waarnemingenlijst[[ii]]$aquometadata$bemonsteringsapparaat$omschrijving,
                bemonsteringssoort.code               = response$waarnemingenlijst[[ii]]$aquometadata$bemonsteringssoort$code,
                bemonsteringssoort.omschrijving       = response$waarnemingenlijst[[ii]]$aquometadata$bemonsteringssoort$omschrijving,
                biotaxon.code                         = response$waarnemingenlijst[[ii]]$aquometadata$biotaxon$code,
                biotaxon.omschrijving                 = response$waarnemingenlijst[[ii]]$aquometadata$biotaxon$omschrijving,
                biotaxontype.code                     = response$waarnemingenlijst[[ii]]$aquometadata$biotaxontype$code,
                biotaxontype.omschrijving             = response$waarnemingenlijst[[ii]]$aquometadata$biotaxontype$omschrijving,
#                biotaxoncompartiment.code = response$waarnemingenlijst[[ii]]$aquometadata$BioTaxon_compartiment$code,                                    # no longer present in API response
#                biotaxoncompartiment.omschrijving = response$waarnemingenlijst[[ii]]$aquometadata$BioTaxon_compartiment$omschrijving,                    # no longer present in API response
                compartiment.code                     = response$waarnemingenlijst[[ii]]$aquometadata$compartiment$code,
                compartiment.omschrijving             = response$waarnemingenlijst[[ii]]$aquometadata$compartiment$omschrijving,
                eenheid.code                          = response$waarnemingenlijst[[ii]]$aquometadata$eenheid$code,
                eenheid.omschrijving                  = response$waarnemingenlijst[[ii]]$aquometadata$eenheid$omschrijving,
                grootheid.code                        = response$waarnemingenlijst[[ii]]$aquometadata$grootheid$code,
                grootheid.omschrijving                = response$waarnemingenlijst[[ii]]$aquometadata$grootheid$omschrijving,
                typering.code                         = response$waarnemingenlijst[[ii]]$aquometadata$typering$code,
                typering.omschrijving                 = response$waarnemingenlijst[[ii]]$aquometadata$typering$omschrijving,
                hoedanigheid.code                     = response$waarnemingenlijst[[ii]]$aquometadata$hoedanigheid$code,
                hoedanigheid.omschrijving             = response$waarnemingenlijst[[ii]]$aquometadata$hoedanigheid$omschrijving,
                meetapparaat.code                     = response$waarnemingenlijst[[ii]]$aquometadata$meetapparaat$code,
                meetapparaat.omschrijving             = response$waarnemingenlijst[[ii]]$aquometadata$meetapparaat$omschrijving,
#                monsterbewerkingsmethode.code         = response$waarnemingenlijst[[ii]]$aquometadata$MonsterBewerkingsMethode$code,                    # no longer present in API response
#                monsterbewerkingsmethode.omschrijving = response$waarnemingenlijst[[ii]]$aquometadata$MonsterBewerkingsMethode$omschrijving,            # no longer present in API response
                orgaan.code                           = response$waarnemingenlijst[[ii]]$aquometadata$orgaan$code,
                orgaan.omschrijving                   = response$waarnemingenlijst[[ii]]$aquometadata$orgaan$omschrijving,
                parameter.code                        = response$waarnemingenlijst[[ii]]$aquometadata$parameter$code,
                parameter.omschrijving                = response$waarnemingenlijst[[ii]]$aquometadata$parameter$omschrijving,
#                plaatsbepalingsapparaat.code = response$waarnemingenlijst[[ii]]$aquometadata$PlaatsbepalingsApparaat$code,                             # no longer present in API response
#                plaatsbepalingsapparaat.omschrijving = response$waarnemingenlijst[[ii]]$aquometadata$PlaatsbepalingsApparaat$omschrijving,             # no longer present in API response
                groepering.code                       = response$waarnemingenlijst[[ii]]$aquometadata$Groepering$code,
                groepering.omschrijving               = response$waarnemingenlijst[[ii]]$aquometadata$Groepering$omschrijving,
                waardebepalingstechniek.code          = response$waarnemingenlijst[[ii]]$aquometadata$waardebepalingsTechniek$code,
                waardebepalingstechniek.omschrijving  = response$waarnemingenlijst[[ii]]$aquometadata$waardebepalingsTechniek$omschrijving,
                waardebepalingsmethode.code           = response$waarnemingenlijst[[ii]]$aquometadata$waardebepalingsMethode$code,
                waardebepalingsmethode.omschrijving   = response$waarnemingenlijst[[ii]]$aquometadata$waardebepalingsmethode$omschrijving,
                waardebewerkingsmethode.code          = response$waarnemingenlijst[[ii]]$aquometadata$waardebewerkingsmethode$code,
                waardebewerkingsmethode.omschrijving  = response$waarnemingenlijst[[ii]]$aquometadata$waardebewerkingsmethode$omschrijving,
                numeriekewaarde = purrr::map_dbl(response$waarnemingenlijst[[ii]]$metingenlijst,list("meetwaarde", "waarde_numeriek"), .default = NA),
                alphanumeriekewaarde = purrr::map_chr(response$waarnemingenlijst[[ii]]$metingenlijst, list("meetwaarde", "waarde_alfanumeriek"), .default = NA)
            )
            temp.df <- as.data.frame(nullToNA(temp.l))
        }
        else temp.df <- data.frame()
        if (ii != 1) {
            df = dplyr::bind_rows(df, temp.df)
        }
        else {
            df = temp.df
        }
        if (httr::http_error(resp)) {
            stop(sprintf("RWS API request failed [%s]\n%s\n<%s>",
                httr::status_code(resp), response$message, response$documentation_url),
                call. = FALSE)
        }
    }
    return(structure(list(content = df, path = path, response = resp)))
}

#' @title Get locations as Web Feature Service
#'
#' @description
#' Query for all locations from all monitoring networks available in WADAR (Rijkswaterstaat).
#'
#' @return spatial dataframe containing all monitoring locations
#'
#' @examples
#' \dontrun{
#' allLocations <- rws_wfsLocations()
#' }
#' @export
rws_wfsLocations <- function(){

  locationsurl = "https://geo.rijkswaterstaat.nl/services/ogc/hws/DDAPI20/ows?SERVICE=WFS&VERSION=1.1.0&REQUEST=GetFeature&TYPENAME=locaties&outputFormat=application/json"
  locs = sf::st_read(locationsurl)
  return(locs)

}



#' @title Get locations with latest observations from Web Feature Service
#'
#' @description
#' Fetches spatial object with all monitoring locations and their latest observation for all parameter/quantities combinations. This is a VERY large file. Consider caching it, or apply a filter ()
#'
#' @return spatial dataframe containing locations with latest observations
#'
#' @examples
#' \dontrun{
#' latestObservations <- rws_wfsLatestObs(parFilter = "Waterhoogte", outputFormat = "application/json")
#' plot(latestObservations[,"id"])
#' latestObservations <- rws_wfsLatestObs(parFilter = "Waterhoogte", outputFormat = "csv")
#' plot(latestObservations$TIJDSTIP_LAATSTE_METING, latestObservations$WAARDE_LAATSTE_METING)
#' }
#' @export
rws_wfsLatestObs <- function(parFilter = "Waterhoogte", outputFormat = "application/json"){

  url_list <- structure(
    list(
      scheme = "https",
      hostname = "geo.rijkswaterstaat.nl",
      port = NULL,
      path = "services/ogc/hws/DDAPI20/ows",
      query = list(
        SERVICE = "WFS",
        VERSION = "1.1.0",
        REQUEST = "GetFeature",
        TYPENAME = "locatiesmetlaatstewaarneming",
        FILTER = paste0(" <Filter> <PropertyIsLike escape=\"!\" singleChar=\".\" wildCard=\"*\"> <PropertyName>PARAMETER_WAT_OMSCHRIJVING</PropertyName> <Literal>*", parFilter, "*</Literal> </PropertyIsLike> </Filter> "),
        outputFormat = outputFormat),
      params = NULL,
      fragment = NULL,
      username = NULL,
      password = NULL),
    class = "url"
  )

  url <- httr::build_url(url_list)

  if(outputFormat == 'csv') {
    locs_with_latest_obs = readr::read_csv(url)
  } else {
    locs_with_latest_obs = sf::st_read(url)
  }

  return(locs_with_latest_obs)

}
