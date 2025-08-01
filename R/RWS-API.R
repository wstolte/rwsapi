
## based on
##https://github.com/hadley/httr/blob/master/vignettes/api-packages.Rmd

library(httr)
library(jsonlite)


#' High level function, retrieves observation data from data distribution layer rws. For each year, a separate file is written.
#'
#' @param startyear Start year of requested data
#' @param endyear End year of requested data
#' @param myCatalogue Dataframe with location and parameter information
#' @param outDir Directory to save the downloaded information
#' @return Downloaded information will be saved as csv in \code{outDir}
#' @examples
#' metadata <- rwsapi::rws_metadata() # gets complete catalog
#' subsTable <- metadata$content$AquoMetadataLijst
#' locsTable <- metadata$content$LocatieLijst
#' mijnLocaties = c("SOELKKPDOT")
#' mijnParameters = c("PO4", "NO3")
#' mijnGrootheden = c("CONCTTE")
#' mijnHoedanigheden = c("Pnf")
#' mijnCatalogus <- rwsapi::rws_getParameters(metadata, locatiecode = mijnLocaties) %>%
#'   filter(parameter.code %in% mijnParameters)
#' getDDLdata(startyear = 2015, endyear = 2020, myCatalogue = mijnCatalogus, outDir = "testData")
#' @export
getDDLdata_by_year <- function(startyear = integer(), endyear = integer(), myCatalogue, outDir = tempdir()) {

  if(outDir == tempdir()){
    print(paste("No output directory given, saving results in", tempdir()))
    # print("Proceed? y/n")
  }

  if(!dir.exists(outDir)) dir.create(outDir, recursive = T)

  # startdate <- paste0(startyear, "-01-01T09:00:00.000+01:00")
  # enddate <- paste0(endyear, "-12-31T23:00:00.000+01:00")

  # getList <- rwsapi::rws_makeDDLapiList(beginDatumTijd = startdate,
  #                               eindDatumTijd = enddate,
  #                               mijnCatalogus = myCatalogue
  # )

  for(year in seq(startyear, endyear, 1)){
    startdate <- paste0(year, "-01-01T09:00:00.000+01:00")
    enddate <- paste0(year + 1, "-12-31T23:00:00.000+01:00")
    getList <- rwsapi::rws_makeDDLapiList(beginDatumTijd = startdate,
                                          eindDatumTijd = enddate,
                                          mijnCatalogus = myCatalogue
    )
    for(jj in c(1:length(getList))){   #
      print(paste("getting", jj, myCatalogue$locatie.code[jj], year, myCatalogue$compartiment.code[jj], myCatalogue$grootheid.code[jj], myCatalogue$parameter.code[jj]))
      response <- rwsapi::rws_observations2(bodylist = getList[[jj]])
      if(!is.null(response) & nrow(response$content)!=0){
        filename <- paste(
          myCatalogue$locatie.code[jj],
          myCatalogue$compartiment.code[jj],
          stringr::str_replace(myCatalogue$grootheid.code[jj], "[^A-Za-z0-9]+", "_"),
          myCatalogue$parameter.code[jj],
          stringr::str_replace(myCatalogue$hoedanigheid.code[jj], "[^A-Za-z0-9]+", "_"),
          year,
          "ddl.csv", sep = "_")
        write_delim(response$content, file = file.path(outDir, filename), delim = ";")} else {
          print(paste("no data available for", myCatalogue$locatie.code[jj], myCatalogue$compartiment.code[jj], myCatalogue$grootheid.code[jj], myCatalogue$parameter.code[jj], myCatalogue$hoedanigheid.code[jj]))
        }
    }
  }
}



#' High level function, retrieves observation data from data distribution layer rws
#'
#' @param startyear Start year of requested data
#' @param endyear End year of requested data
#' @param myCatalogue Dataframe with location and parameter information
#' @param outDir Directory to save the downloaded information
#' @return Downloaded information will be saved as csv in \code{outDir}
#' @examples
#' metadata <- rwsapi::rws_metadata() # gets complete catalog
#' subsTable <- metadata$content$AquoMetadataLijst
#' locsTable <- metadata$content$LocatieLijst
#' mijnLocaties = c("SOELKKPDOT")
#' mijnParameters = c("PO4", "NO3")
#' mijnGrootheden = c("CONCTTE")
#' mijnHoedanigheden = c("Pnf")
#' mijnCatalogus <- rwsapi::rws_getParameters(metadata, locatiecode = mijnLocaties) %>%
#'   filter(parameter.code %in% mijnParameters)
#' getDDLdata(startyear = 2015, endyear = 2020, myCatalogue = mijnCatalogus, outDir = "testData")
#' @export
getDDLdata <- function(startyear = integer(), endyear = integer(), myCatalogue, outDir = tempdir()) {

  if(outDir == tempdir()){
    print(paste("No output directory given, saving results in", tempdir()))
    # print("Proceed? y/n")
  }

  if(!dir.exists(outDir)) dir.create(outDir, recursive = T)

  startdate <- paste0(startyear, "-01-01T09:00:00.000+01:00")
  enddate <- paste0(endyear, "-12-31T23:00:00.000+01:00")

  getList <- rwsapi::rws_makeDDLapiList(beginDatumTijd = startdate,
                                eindDatumTijd = enddate,
                                mijnCatalogus = myCatalogue
  )

    for(jj in c(1:length(getList))){   #
      print(paste("getting", jj, myCatalogue$locatie.code[jj], paste0(startyear, "- ", endyear), myCatalogue$compartiment.code[jj], myCatalogue$grootheid.code[jj], myCatalogue$parameter.code[jj]))
      response <- rwsapi::rws_observations2(bodylist = getList[[jj]])
      if(!is.null(response) & nrow(response$content)!=0){
        filename <- paste(
          myCatalogue$locatie.code[jj],
          myCatalogue$compartiment.code[jj],
          stringr::str_replace(myCatalogue$grootheid.code[jj], "[^A-Za-z0-9]+", "_"),
          myCatalogue$parameter.code[jj],
          stringr::str_replace(myCatalogue$hoedanigheid.code[jj], "[^A-Za-z0-9]+", "_"),
          startyear, endyear,
          "ddl.csv", sep = "_")
        write_delim(response$content, file = file.path(outDir, filename), delim = ";")} else {
          print(paste("no data available for", myCatalogue$locatie.code[jj], myCatalogue$compartiment.code[jj], myCatalogue$grootheid.code[jj], myCatalogue$parameter.code[jj], myCatalogue$hoedanigheid.code[jj]))
    }
  }
}


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

  parsed <- jsonlite::fromJSON(
    content(resp, "text", encoding = "UTF-8"),
    simplifyVector = TRUE
    )


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
rws_metadata2 <- function(
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

  parsed <- jsonlite::fromJSON(
    content(resp, "text", encoding = "UTF-8"),
    simplifyVector = TRUE,
    flatten = T
  )


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


#' Collects selection of metadata for long term monitoring observation at Rijkswaterstaat (NL)
#'
#' @param compartiment Compartment (matrix) used for filtering metadata. Compartments can be expressed as codes, or names (omschrijving). Examples are "OW" for surface water, "BS" for Bottom/Sediment.
#' @param grootheid Grootheid (quantity) used for filtering metadata.
#' @param parameter Parameter (quality) used for filtering metadata.
#' @param locatie Location used for filtering metadata, expressed as code or name (omschrijving).
#' @param ... extra arguments to be passed on to rws_metadata(). See ?rws_metadata
#' @return A structured list with metadata, class "rws_api"
#' @examples
#' # Collect all metadata:
#' metadata <- get_selected_metadata()
#' Collect all metadata for quantity "Waterhoogte":
#' selectedmetadata <- get_selected_metadata(grootheid = "Waterhoogte")
get_selected_metadata <- function(
    compartiment = NULL,
    grootheid = NULL,
    parameter = NULL,
    hoedanigheid = NULL,
    locatie = NULL,
    ...
    # filterlist = list(Eenheden=T, Grootheden=T, Parameters=T, Hoedanigheden=T, Compartimenten = T),
    # path = "/METADATASERVICES_DBO/OphalenCatalogus/"
) {

  require(rwsapi)
  require(tidyverse)

  md <- rwsapi::rws_metadata(...)

  md$content$AquoMetadataLijst %>%
    unnest(
      names_sep = ".",
      c(Compartiment, Eenheid, Grootheid, Hoedanigheid, Parameter)) %>%
    filter(
      if(is.null(grootheid)) TRUE else Grootheid.Omschrijving %in% grootheid | Grootheid.Code %in% grootheid,
      if(is.null(parameter)) TRUE else Parameter.Omschrijving %in% parameter | Parameter.Code %in% parameter,
      if(is.null(hoedanigheid)) TRUE else Hoedanigheid.Code %in% hoedanigheid | Hoedanigheid.Code %in% hoedanigheid,
      if(is.null(compartiment)) TRUE else Compartiment.Code %in% compartiment | Compartiment.Code %in% compartiment
    ) %>%
    left_join(
      md$content$AquoMetadataLocatieLijst,
      by = c(AquoMetadata_MessageID = "AquoMetaData_MessageID")
    ) %>%
    left_join(md$content$LocatieLijst) %>%
    filter(if(is.null(locatie)) TRUE else Naam %in% locatie |  Code %in% locatie) %>%
    rename_with(tolower) %>%
    rename(
      locatie.naam = naam,
      locatie.code = code
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
rws_observations2 <- function(bodylist, trytimes = 3) {

  warnings = list()

  path = "/ONLINEWAARNEMINGENSERVICES_DBO/OphalenWaarnemingen/"
  url <- modify_url("https://waterwebservices.rijkswaterstaat.nl", path = path)
  library(httr)
  library(jsonlite)
  ua <- user_agent("https://github.com/wstolte/rwsapi")

  # result <- try(RJSONIO::fromJSON("http://graph.facebook.com/?ids=this.username.does.not.exist.because.i.made.it.up"), silent=TRUE)`
  # or use RETRY()

  # old code without retry
  # resp <- POST(url = url,
  #              ua,
  #              body=toJSON(bodylist, auto_unbox = T, digits = NA),
  #              add_headers(.headers = c("Content-Type"="application/json","Ocp-Apim-Subscription-Key"="my_subscrition_key"))
  # )
  #

  resp <- RETRY(
    verb = "POST",
    url = url,
    ua = ua,
    body=toJSON(bodylist, auto_unbox = T, digits = NA),
      add_headers(.headers = c("Content-Type"="application/json","Ocp-Apim-Subscription-Key"="my_subscrition_key")),
    times = trytimes
  )



  if (http_type(resp) != "application/json") {
    stop("API did not return application/json", call. = FALSE)
  }

  response <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  if (!response$Succesvol) {
    paste("request not succefull", response$Foutmelding)
  }



  df <- data.frame()

  for (ii in seq(1:length(response$WaarnemingenLijst))) {
    print(paste("ii in response.waarnemingenlijst: ", ii))
    # ii= 1
    if (purrr::is_empty(response$WaarnemingenLijst)) {next}
    if (!purrr::is_empty(as.numeric(response$WaarnemingenLijst[[ii]]$MetingenLijst %>%
                                    purrr::map_dbl(list("Meetwaarde", "Waarde_Numeriek"), .default = NA)))) {
      temp.l = list(locatie.message.id = response$WaarnemingenLijst[[ii]]$Locatie$Locatie_MessageID,
                    locatie.code = response$WaarnemingenLijst[[ii]]$Locatie$Code,
                    locatie.naam = response$WaarnemingenLijst[[ii]]$Locatie$Naam,
                    coordinatenstelsel = response$WaarnemingenLijst[[ii]]$Locatie$Coordinatenstelsel,
                    geometriepunt.x = response$WaarnemingenLijst[[ii]]$Locatie$X,
                    geometriepunt.y = response$WaarnemingenLijst[[ii]]$Locatie$Y,
                    tijdstip = response$WaarnemingenLijst[[ii]]$MetingenLijst %>% map_chr((list(1)), .default = NA),
                    statuswaarde = response$WaarnemingenLijst[[ii]]$MetingenLijst %>% map_chr(list(3, 1, 1), .default = NA),
                    bemonsteringshoogte = response$WaarnemingenLijst[[ii]]$MetingenLijst %>% map_chr((list(3, 2, 1)), .default = NA_character_),
                    referentievlak = response$WaarnemingenLijst[[ii]]$MetingenLijst %>% map_chr(list(3, 3, 1), .default = NA),
                    opdrachtgevendeinstantie = response$WaarnemingenLijst[[ii]]$MetingenLijst %>% map_chr(list(3, 4, 1), .default = NA),
                    kwaliteitswaarde.code = response$WaarnemingenLijst[[ii]]$MetingenLijst %>% map_chr((list(3, 5, 1)), .default = NA),
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
                    typering.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$Typering$Code,
                    typering.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$Typering$Omschrijving,
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
                    groepering.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$Groepering$Code,
                    groepering.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$Groepering$Omschrijving,
                    waardebepalingstechniek.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$WaardeBepalingstechniek$Code,
                    waardebepalingstechniek.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$WaardeBepalingstechniek$Omschrijving,
                    waardebepalingsmethode.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$WaardeBepalingsmethode$Code,
                    waardebepalingsmethode.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$WaardeBepalingsmethode$Omschrijving,
                    waardebewerkingsmethode.code = response$WaarnemingenLijst[[ii]]$AquoMetadata$WaardeBewerkingsmethode$Code,
                    waardebewerkingsmethode.omschrijving = response$WaarnemingenLijst[[ii]]$AquoMetadata$WaardeBewerkingsmethode$Omschrijving,
                    numeriekewaarde = as.numeric(response$WaarnemingenLijst[[ii]]$MetingenLijst %>%
                                                   purrr::map_dbl(list("Meetwaarde", "Waarde_Numeriek"),
                                                           .default = NA_real_)),
                    alphanumeriekewaarde = as.numeric(response$WaarnemingenLijst[[ii]]$MetingenLijst %>%
                                                   map_chr((list("Meetwaarde", "Waarde_Alphanumeriek")),
                                                           .default = NA_character_))
                    )
      temp.df <- as.data.frame(nullToNA(temp.l))
      # print("temp.df")
      # print(paste("number of lines: ", length(temp.df$locatie.naam)))
      # print(paste("location: ", unique(temp.df$locatie.naam)))
      # print(paste("date range", range(temp.df$tijdstip)))
    }
    else temp.df <- data.frame()
    if (ii != 1) {
      df = bind_rows(df, temp.df)
    }
    else {
      df = temp.df
    }
    # print("df")
    # print(paste("number of lines: ", length(df$locatie.naam)))
    # print(paste("location: ", unique(df$locatie.naam)))
    # print(paste("date range", range(df$tijdstip)))

    if (http_error(resp)) {
      stop(sprintf("RWS API request failed [%s]\n%s\n<%s>",
                   status_code(resp), parsed$message, parsed$documentation_url),
           call. = FALSE)
    }
  }
  return(structure(list(content = df, path = path, response = resp)))
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
rws_getLocations <- function(metadata, grootheidcode, parametercode = NULL) {
  require(tidyverse)

  if(!is.null(metadata$content)) myMetadata <- metadata$content else myMetadata <- metadata

  # grootheidcode = 'salntt'; parametercode = 'nvt'

  rlist::list.flatten(myMetadata$AquoMetadataLijst) %>%
    dplyr::bind_cols() %>%
    `names<-`(tolower(names(.))) %>%
    dplyr::filter(tolower(grootheid.code) %in% tolower(grootheidcode)) %>%
    dplyr::filter(if(!is.null(parametercode)) tolower(parameter.code) %in% tolower(parametercode) else TRUE) %>%
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
rws_getParameters <- function(metadata, locatiecode = NULL, locatienaam = NULL) {
  require(tidyverse)

  if(!is.null(metadata$content)) myMetadata <- metadata$content else myMetadata <- metadata

  # locatiecode = 'grootgnd' # for testing

  as_tibble(rlist::list.flatten(myMetadata$LocatieLijst)) %>%
    `names<-`(tolower(names(.))) %>%
    {if (is.null(locatienaam)) filter(., code %in% locatiecode) else filter(., naam %in% locatienaam)} %>%
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
rws_makeDDLapiList <- function(mijnCatalogus, beginDatumTijd, eindDatumTijd){
  for(ii in seq(1:dim(mijnCatalogus)[1])){
    #messageID meegeven waanneer op parameter_wat_omschrijving gezocht wordt.
    if(ii==1)  ll <- list()
    l <- list(
      AquoPlusWaarnemingMetadata= list(
        AquoMetadata = list(
          Compartiment = list(Code = mijnCatalogus$compartiment.code[ii]),
          Parameter = list(Code = mijnCatalogus$parameter.code[ii]),
          # Eenheid = list(Code = mijnEenheid),
          # MeetApparaat = mijnMeetapparaat,
          Grootheid = list(Code = mijnCatalogus$grootheid.code[ii]),
          Hoedanigheid = list(Code = mijnCatalogus$hoedanigheid.code[ii])
        )
      ),
      Locatie = list(
        X = str_pad(as.character(mijnCatalogus["x"][ii,]), 16, "right", "0"),
        Y = str_pad(as.character(mijnCatalogus["y"][ii,]), 16, "right", "0"),
        Code = as.character(mijnCatalogus["locatie.code"][ii,])),
      Periode = list(Begindatumtijd = beginDatumTijd,
                     Einddatumtijd = eindDatumTijd)
    )
    ll[[ii]] <- l
  }
  return(ll)
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

rws_wmsLocations <- function(){
  url = "https://waterwebservices.rijkswaterstaat.nl/services/ogc/hws/wmdc15/ows?SERVICE=WFS&VERSION=1.1.0&REQUEST=GetFeature&TYPENAME=wmdc15:locaties"
  url = "https://waterwebservices.rijkswaterstaat.nl/services/distributielaagWFS/distributielaag_dbo?SERVICE=WFS&VERSION=1.1.0&REQUEST=GetFeature&TYPENAME=locatiesmetlaatstewaarneming&Maxfeatures=50"
httr::parse_url(url)
  df <- st_read(url)
}
