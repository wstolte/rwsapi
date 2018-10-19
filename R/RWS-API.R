
## based on
##https://github.com/hadley/httr/blob/master/vignettes/api-packages.Rmd

library(httr)
library(jsonlite)

#' Collects metadata for long term monitoring observation at Rijkswaterstaat (NL)
#'
#' @param path The request path. Default is "/METADATASERVICES_DBO/OphalenCatalogus/".
#' @param filterList List objects in request. Default is "list(Eenheden=T,Grootheden=T,Hoedanigheden=T)"
#' @return A structured list with metadata, class "rws_api"
#' @examples
#' metadata <- rws_metadata()
#' # parse content of response
#' parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = T )
#' # extract unique locations
#' locations <- parsed$LocatieLijst
#' # extract parameters
#' parameters <- data.frame(parameter = parsed$AquoMetadataLijst)

rws_metadata <- function(
  path = "/METADATASERVICES_DBO/OphalenCatalogus/",
  filterList = list(Eenheden=T, Grootheden=T, Parameters=T, Hoedanigheden=T)
  ) {
  library(httr)
  library(jsonlite)
  ua <- user_agent("https://waterwebservices.rijkswaterstaat.nl")
  path = path
  url <- modify_url(ua$options$useragent, path = path)

  l = list(CatalogusFilter=filterList)

  resp <- POST(url, ua, body = l, encode = "json")

  if (http_type(resp) != "application/json") {
    stop("API did not return application/json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = TRUE )


  if (http_error(resp)) {
    stop(
      sprintf(
        "RWS API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "rws_api"
  )
}


#' Collects observation data for long term monitoring observation at Rijkswaterstaat (NL)
#'
#' @param bodylist The message body containing criteria for data selection.
#' @return A structured list with data, class "rws_api"
#' @examples
#' options(digits=22)
#'
#' l2 <- list(
#'   AquoPlusWaarnemingMetadata= list(
#'     AquoMetadata = list(
#'       Compartiment = list(Code = "OW"),
#'       Eenheid = list(Code = "cm"),
#'       MeetApparaat = list(Code = "109"),
#'       Grootheid = list(Code = "Hm0"))),
#'   Locatie = list(
#'     X = 518882.33332024701,
#'     Y = 5760829.1172958901,
#'     Code = "EURPFM"),
#'   Periode = list(Begindatumtijd = "2012-01-27T09:00:00.000+01:00",
#'                  Einddatumtijd = "2012-01-28T09:01:00.000+01:00")
#' )
#' observation <- rws_observations(l2)
#' content(observation$response, "text")
#' parsed <- jsonlite::fromJSON(content(observation$response, "text"), simplifyVector = T )
#' parsed$WaarnemingenLijst$MetingenLijst[[1]] %>% View()
#'
rws_observations <- function(bodylist) {
  path = "/ONLINEWAARNEMINGENSERVICES_DBO/OphalenWaarnemingen/"
  url <- modify_url("https://waterwebservices.rijkswaterstaat.nl", path = path)
  library(httr)
  library(jsonlite)
  ua <- user_agent("https://waterwebservices.rijkswaterstaat.nl")
  resp <- POST(url = url,
               ua,
               body=toJSON(bodylist, auto_unbox = T, digits = NA),
               add_headers(.headers = c("Content-Type"="application/json","Ocp-Apim-Subscription-Key"="my_subscrition_key"))
  )

  if (http_type(resp) != "application/json") {
    stop("API did not return application/json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  if (http_error(resp)) {
    stop(
      sprintf(
        "RWS API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "rws_api"
  )
}


#' Collects quantities and parameters observed at a number of stations
#'
#' @param parsedMetaData parsed list of metadata generated from rws_metadata()
#' @param locationlist character vector of selected stations
#' @return dataframe containing observed quantities and parameters
#' @examples
#' metadata <- rws_metadata()
#' # parse content of response
#' parsedmetadata <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = T )
#' catalogue <- DDLgetParametersForLocations(parsedmetadata, c("Dreischor", "Herkingen", "Scharendijke diepe put"))
require(tidyverse)
DDLgetParametersForLocations <- function(parsedMetaData, locationlist) {
  parsedMetaData$LocatieLijst %>%
    filter(Code %in% locationlist) %>%
    left_join(parsedMetaData$AquoMetadataLocatieLijst) %>%
    left_join(jsonlite::flatten(parsedMetaData$AquoMetadataLijst, recursive = T), by = c(AquoMetaData_MessageID = "AquoMetadata_MessageID")) %>%
    dplyr::select(Code,
             X,
             Y,
             Naam,
             Grootheid.Code,
             Hoedanigheid.Code,
             Parameter.Code,
             Parameter.Omschrijving,
             Parameter_Wat_Omschrijving,
             Eenheid.Code
             ) %>%
    left_join(enrichedMetadata, by = c(Parameter_Wat_Omschrijving = "parameter"))
}

