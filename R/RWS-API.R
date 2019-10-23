
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

  parsed <- jsonlite::fromJSON(content(resp, "text", ), simplifyVector = TRUE )


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



nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}



#' Collects observation data for long term monitoring observation at Rijkswaterstaat (NL) and parses into dataframe (tibble)
#'
#' @param bodylist The message body containing criteria for data selection.
#' @return A structured list with a dataframe, path and response
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

  response <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  for(ii in seq(1:length(response$WaarnemingenLijst))) {
    if(purrr::is_empty(response$WaarnemingenLijst)) next
    if(!purrr::is_empty(as.numeric(response$WaarnemingenLijst[[ii]]$MetingenLijst %>% map_chr(list("Meetwaarde", "Waarde_Numeriek"), .default = NA)))) {
      df <- data.frame()
      temp.l= list(
        locatie.message.id = response$WaarnemingenLijst[[ii]]$Locatie$Locatie_MessageID,
        locatie.code = response$WaarnemingenLijst[[ii]]$Locatie$Code,
        locatie.naam = response$WaarnemingenLijst[[ii]]$Locatie$Naam,
        coordinatenstelsel = response$WaarnemingenLijst[[ii]]$Locatie$Coordinatenstelsel,
        geometriepunt.x = response$WaarnemingenLijst[[ii]]$Locatie$X,
        geometriepunt.y = response$WaarnemingenLijst[[ii]]$Locatie$Y,
        # locationname = ,
        tijdstip = response$WaarnemingenLijst[[ii]]$MetingenLijst %>% map_chr(list(1), .default = NA),
        statuswaarde = response$WaarnemingenLijst[[ii]]$MetingenLijst %>% map_chr(list(3,1,1), .default = NA),
        bemonsteringshoogte = response$WaarnemingenLijst[[ii]]$MetingenLijst %>% map_chr(list(3,2,1), .default = NA),
        referentievlak = response$WaarnemingenLijst[[ii]]$MetingenLijst %>% map_chr(list(3,3,1), .default = NA),
        opdrachtgevendeinstantie = response$WaarnemingenLijst[[ii]]$MetingenLijst %>% map_chr(list(3,4,1), .default = NA),
        kwaliteitswaarde.code = response$WaarnemingenLijst[[ii]]$MetingenLijst %>% map_chr(list(3,5,1), .default = NA),
        aquometadata.message.id = response$WaarnemingenLijst[[ii]]$AquoMetadata$AquoMetadata_MessageID,
        parameter.wat.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$Parameter_Wat_Omschrijving,
        bemonsteringsapparaat.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$BemonsteringsApparaat$Code,
        bemonsteringsapparaat.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$BemonsteringsApparaat$Omschrijving,
        bemonsteringssoort.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$BemonsteringsSoort$Code,
        bemonsteringssoort.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$BemonsteringsSoort$Omschrijving,
        biotaxon.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$BioTaxon$Code,
        biotaxon.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$BioTaxon$Omschrijving,
        biotaxoncompartiment.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$BioTaxon_Compartiment$Code,
        biotaxoncompartiment.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$BioTaxon_Compartiment$Omschrijving,
        compartiment.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$Compartiment$Code,
        compartiment.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$Compartiment$Omschrijving,
        eenheid.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$Eenheid$Code,
        eenheid.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$Eenheid$Omschrijving,
        grootheid.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$Grootheid$Code,
        grootheid.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$Grootheid$Omschrijving,
        hoedanigheid.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$Hoedanigheid$Code,
        hoedanigheid.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$Hoedanigheid$Omschrijving,
        meetapparaat.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$MeetApparaat$Code,
        meetapparaat.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$MeetApparaat$Omschrijving,
        monsterbewerkingsmethode.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$MonsterBewerkingsMethode$Code,
        monsterbewerkingsmethode.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$MonsterBewerkingsMethode$Omschrijving,
        orgaan.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$Orgaan$Code,
        orgaan.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$Orgaan$Omschrijving,
        parameter.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$Parameter$Code,
        parameter.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$Parameter$Omschrijving,
        plaatsbepalingsapparaat.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$PlaatsBepalingsApparaat$Code,
        plaatsbepalingsapparaat.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$PlaatsBepalingsApparaat$Omschrijving,
        typering.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$Typering$Code,
        typering.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$Typering$Omschrijving,
        waardebepalingstechniek.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$WaardeBepalingstechniek$Code,
        waardebepalingstechniek.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$WaardeBepalingstechniek$Omschrijving,
        waardebepalingsmethode.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$WaardeBepalingsmethode$Code,
        waardebepalingsmethode.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$WaardeBepalingsmethode$Omschrijving,
        waardebewerkingsmethode.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$WaardeBewerkingsmethode$Code,
        waardebewerkingsmethode.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$WaardeBewerkingsmethode$Omschrijving,
        numeriekewaarde = as.numeric(response$WaarnemingenLijst[[ii]]$MetingenLijst %>% map_chr(list("Meetwaarde", "Waarde_Numeriek"), .default = NA))
      )
      temp.df <- as.data.frame(nullToNA(temp.l))
    }
    else temp.df <- data.frame()
    if(ii != 1){
      df = rbind(df, temp.df)
    }else {
      df = temp.df
    }

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
    return(
      structure(
        list(
          content = df,
          path = path,
          response = resp
        )#,
        # class = "rws_api"
      )
    )
  }
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
rws_getLocations <- function(metadata, grootheidcode, parametercode) {
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


#' Collects observed quantities and parameters for stations.
#'
#' @param metadata parsed list of metadata generated from rws_metadata()
#' @param locatiecode character vector of selected locatie.code
#' @return dataframe containing locations where grootheidcode and parametercode occur
#' @examples
#' metadata <- rws_metadata()
#' getLocations(metadata, 'SALNTT', 'NVT')
#' getLocations(metadata, 'salntt', 'nvt') # no case-sensitivity
rws_getParameters <- function(metadata, locatiecode = NULL, locatiename = NULL) {
  require(tidyverse)

  if(!is.null(metadata$content)) myMetadata <- metadata$content else myMetadata <- metadata

  # locatiecode = 'grootgnd' # for testing

  as_tibble(rlist::list.flatten(myMetadata$LocatieLijst)) %>%
    `names<-`(tolower(names(.))) %>%
    {if (is.null(locatiename)) filter(., code %in% locatiecode) else filter(., naam %in% locatiename)} %>%
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
#' @return dataframe containing observed quantities and parameters
#' @examples
#' metadata <- rws_metadata()
#' # parse content of response
#' parsedmetadata <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = T )
#' catalogue <- DDLgetParametersForLocations(parsedmetadata, c("Dreischor", "Herkingen", "Scharendijke diepe put"))
rws_makeDDLapiList <- function(mijnCatalogus, beginDatumTijd, eindDatumTijd, mijnCompartiment = NULL){
  result <- list()
  for(ii in seq(1:length(mijnCatalogus[,1]))){
    #messageID meegeven waanneer op parameter_wat_omschrijving gezocht wordt.
    if(ii==1)  ll <- list()
    l <- list(
      AquoPlusWaarnemingMetadata= list(
        AquoMetadata = list(
          Compartiment = list(Code = mijnCompartiment),
          Parameter = list(Code = mijnCatalogus$parameter.code[ii]),
          # Eenheid = list(Code = mijnEenheid),
          # MeetApparaat = mijnMeetapparaat,
          Grootheid = list(Code = mijnCatalogus$grootheid.code[ii]),
          Hoedanigheid = list(Code = mijnCatalogus$hoedanigheid.code[ii])
        )
      ),
      Locatie = list(
        X = as.character(mijnCatalogus["x"][ii,]),
        Y = as.character(mijnCatalogus["y"][ii,]),
        Code = as.character(mijnCatalogus["locatie.code"][ii,])),
      Periode = list(Begindatumtijd = beginDatumTijd,
                     Einddatumtijd = eindDatumTijd)
    )
    ll[[ii]] <- l
  }
  return(ll[[1]])
}



#' selects locations within DDL based on WFD water bodies from the Netherlands
#'
#' @param metadata metadata from DDL. download using rwsapi::rws_metadata()
#' @param locationlist character vector of selected locations code or name
#' @param myWaterBody Name or partial name of the waterbody of interest
#' @param buffer_in_m buffer for finding locations in meters
#' @return dataframe with selected locations
#' @examples
#' metadata <- rws_metadata()
#' select_locations_in_waterbody(metadata, "westerschelde", 0)
#' select_locations_in_waterbody(metadata, "westerschelde", 2000) # also retrieves "Schaar van Ouden Doel".
select_locations_in_waterbody <- function(metadata, myWaterBody, buffer_in_m) {

  require(sf)
  require(dplyr)
  # check if metadata is correct, name is correct
  # comment: run this first:
  # > metadata <- rwsapi::rws_metadata() # gets complete catalog
  locsTable <- metadata$content$LocatieLijst
  if(locsTable %>% distinct(Coordinatenstelsel) %>% length() == 1){
    locs_sf <- sf::st_as_sf(locsTable, coords = c("X", "Y"), crs = 25831)
    locs_sf_rd <- sf::st_transform(locs_sf, crs = 28992)
  } else print("warning, multiple epsg, sf object not produced")

  # download water bodies for 2006 , 2018 returns error for some reason
  typename='kaderrichtlijnwater:krw_oppervlaktewaterlichamen_vlakken_rws_2006'
  dsn = 'https://geodata.nationaalgeoregister.nl/kaderrichtlijnwater/wfs?service=WFS&request=getCapabilities'
  wb <- sf::st_read(dsn, "kaderrichtlijnwater:krw_oppervlaktewaterlichamen_vlakken_rws_2006")
  # st_crs(wb) # check crs
  mijnShape <- wb[grepl(x = tolower(wb$OWMNAAM), pattern = tolower(myWaterBody)),]
  # buffer_in_m <- 2000 # for testing
  mijnLocaties <- sf::st_intersection(locs_sf_rd, sf::st_buffer(mijnShape, buffer_in_m)) %>%
    sf::st_drop_geometry() %>% distinct(Code) %>%
    left_join(locsTable)
  return(mijnLocaties)
}

#' selects locations within DDL based on polygon
#'
#' @param metadata metadata from DDL. download using rwsapi::rws_metadata()
#' @param locationlist character vector of selected locations code or name
#' @param polygon polygon of interest as sf object (?sf)
#' @param buffer_in_m buffer for finding locations in meters
#' @return dataframe with selected locations
#' @examples
#' metadata <- rws_metadata()
#' select_locations_in_waterbody(metadata, "westerschelde", 0)
#' select_locations_in_waterbody(metadata, "westerschelde", 2000) # also retrieves "Schaar van Ouden Doel".
#' select_locations_by_polygon
select_locations_by_polygon <- function(metadata, polygon, buffer_in_m) {

  require(sf)
  require(dplyr)
  # check if metadata is correct, name is correct
  # comment: run this first:
  # > metadata <- rwsapi::rws_metadata() # gets complete catalog
  locsTable <- metadata$content$LocatieLijst
  if(locsTable %>% distinct(Coordinatenstelsel) %>% length() == 1){
    locs_sf <- sf::st_as_sf(locsTable, coords = c("X", "Y"), crs = 25831)
    locs_sf_rd <- sf::st_transform(locs_sf, crs = 28992)
  } else print("warning, multiple epsg, sf object not produced")

  mijnShape <- sf::st_transform(polygon, crs = 28992)

  # buffer_in_m <- 2000 # for testing
  mijnLocaties <- sf::st_intersection(locs_sf_rd, sf::st_buffer(mijnShape, buffer_in_m)) %>%
    sf::st_drop_geometry() %>% distinct(Code) %>%
    left_join(locsTable)
  return(mijnLocaties)
}

