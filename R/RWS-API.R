
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
  filterList = list(Eenheden=T,Grootheden=T,Hoedanigheden=T)
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



DDLgetParametersForLocations <- function(parsedMetaData, locationlist) {
  load("data/parametermap.rda")
  parsedMetaData$LocatieLijst %>% filter(Naam %in% locationlist) %>%
    left_join(parsedMetaData$AquoMetadataLocatieLijst) %>%
    left_join(jsonlite::flatten(parsedMetaData$AquoMetadataLijst, recursive = T), by = c(AquoMetaData_MessageID = "AquoMetadata_MessageID")) %>%
    group_by(Naam, Grootheid.Code, Hoedanigheid.Code, Parameter_Wat_Omschrijving) %>%
    summarise(n = n()) %>%
    left_join(parametermap, by = c(Parameter_Wat_Omschrijving = "parameter"))
}

