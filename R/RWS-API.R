
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
  filterList = list(Eenheden=T, Grootheden=T, Parameters=T, Hoedanigheden=T, Compartimenten = T)
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
    )#,
    # class = "rws_api"
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
    )#,
    # class = "rws_api"
  )
}


#' Collects observed quantities and parameters for stations
#'
#' @param metadata parsed list of metadata generated from rws_metadata()
#' @param grootheidcode character vector of selected grootheid.code according to AQUO
#' @param parametercode character vector of selected parameter.code according to AQUO
#' @return dataframe containing locations where grootheidcode and parametercode occur
#' @examples
#' metadata <- rws_metadata()
#' getLocations(metadata, 'SALNTT', 'NVT')
#' getLocations(metadata, 'salntt', 'nvt') # no case-sensitivity
getLocations <- function(metadata, grootheidcode, parametercode) {
  require(tidyverse)

  if(!is.null(metadata$content)) myMetadata <- metadata$content else myMetadata <- metadata

  grootheidcode = 'salntt'; parametercode = 'nvt'

  rlist::list.flatten(myMetadata$AquoMetadataLijst) %>%
    dplyr::bind_cols() %>%
    `names<-`(tolower(names(.))) %>%
    dplyr::filter(tolower(grootheid.code) %in% tolower(grootheidcode),
                  tolower(parameter.code) %in% tolower(parametercode)) %>%
    dplyr::left_join(as_tibble(rlist::list.flatten(myMetadata$AquoMetadataLocatieLijst)),
                     by = c(aquometadata_messageid = 'AquoMetaData_MessageID')) %>%
    dplyr::left_join(as_tibble(rlist::list.flatten(myMetadata$LocatieLijst))) %>%
    `names<-`(tolower(names(.))) %>%
  dplyr::select(
    aquometadata_messageid,
    locatie_messageid,
    parameter_wat_omschrijving,
    compartiment.code,
    compartiment.omschrijving,
    eenheid.code,
    eenheid.omschrijving,
    grootheid.code,
    grootheid.omschrijving,
    hoedanigheid.code,
    hoedanigheid.omschrijving,
    parameter.code,
    parameter.omschrijving,
    locatie.naam = naam,
    locatie.code = code,
    x,
    y,
    coordinatenstelsel
  )
}


#' Collects observed quantities and parameters for stations
#'
#' @param metadata parsed list of metadata generated from rws_metadata()
#' @param locatiecode character vector of selected locatie.code
#' @return dataframe containing locations where grootheidcode and parametercode occur
#' @examples
#' metadata <- rws_metadata()
#' getLocations(metadata, 'SALNTT', 'NVT')
#' getLocations(metadata, 'salntt', 'nvt') # no case-sensitivity
getParameters <- function(metadata, locatiecode) {
  require(tidyverse)

  if(!is.null(metadata$content)) myMetadata <- metadata$content else myMetadata <- metadata

  locatiecode = 'grootgnd'

  as_tibble(rlist::list.flatten(myMetadata$LocatieLijst)) %>%
    `names<-`(tolower(names(.))) %>%
    # distinct(code) %>% View()
    dplyr::filter(tolower(code) %in% tolower(locatiecode)) %>%
    dplyr::left_join(as_tibble(rlist::list.flatten(myMetadata$AquoMetadataLocatieLijst)),
                     by = c(locatie_messageid = 'Locatie_MessageID')) %>%
    dplyr::left_join(bind_cols(rlist::list.flatten(myMetadata$AquoMetadataLijst)),
                     by = c(AquoMetaData_MessageID = 'AquoMetadata_MessageID')) %>%
    `names<-`(tolower(names(.))) %>%
    dplyr::select(
      aquometadata_messageid,
      locatie_messageid,
      parameter_wat_omschrijving,
      compartiment.code,
      compartiment.omschrijving,
      eenheid.code,
      eenheid.omschrijving,
      grootheid.code,
      grootheid.omschrijving,
      hoedanigheid.code,
      hoedanigheid.omschrijving,
      parameter.code,
      parameter.omschrijving,
      locatie.naam = naam,
      locatie.code = code,
      x,
      y,
      coordinatenstelsel
    )
}






#' makes list for requesting observation data from rws api
#'
#' @param mijnCatalogus catalogue created using rws_metadata
#' @param locationlist character vector of selected stations code or name
#' @return dataframe containing observed quantities and parameters
#' @examples
#' metadata <- rws_metadata()
#' # parse content of response
#' parsedmetadata <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = T )
#' catalogue <- DDLgetParametersForLocations(parsedmetadata, c("Dreischor", "Herkingen", "Scharendijke diepe put"))
makeDDLapiList <- function(mijnCatalogus, beginDatumTijd, eindDatumTijd, mijnCompartiment = NULL){
  result <- list()
  for(ii in seq(1:length(mijnCatalogus[,1]))){
    #messageID meegeven waanneer op parameter_wat_omschrijving gezocht wordt.
    if(ii==1)  ll <- list()
    l <- list(
      AquoPlusWaarnemingMetadata= list(
        AquoMetadata = list(
          # Compartiment = list(Code = mijnCompartiment),
          Parameter = list(Code = mijnCatalogus$Parameter.Code[ii]),
          # Eenheid = list(Code = mijnEenheid),
          # MeetApparaat = mijnMeetapparaat,
          Grootheid = list(Code = mijnCatalogus$Grootheid.Code[ii]),
          Hoedanigheid = list(Code = mijnCatalogus$Hoedanigheid.Code[ii])
        )
      ),
      Locatie = list(
        X = as.character(mijnCatalogus["X"][ii,]),
        Y = as.character(mijnCatalogus["Y"][ii,]),
        Code = as.character(mijnCatalogus["Code"][ii,])),
      Periode = list(Begindatumtijd = beginDatumTijd,
                     Einddatumtijd = eindDatumTijd)
    )
    ll[[ii]] <- l
  }
  return(ll)
}


