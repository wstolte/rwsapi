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


#' @title High level function, retrieves observation data from data distribution layer rws. For each year, a separate file is written.
#'
#' @description
#' TO BE WORKED OUT
#'
#' @param startyear Start year of requested data
#' @param endyear End year of requested data
#' @param myCatalogue Dataframe with location and parameter information
#' @param outDir Directory to save the downloaded information
#'
#' @return Downloaded information will be saved as csv in \code{outDir}
#'
#' @examples
#' metadata <- rws_metadata() # gets complete catalog
#' subsTable <- metadata$content$AquoMetadataLijst
#' locsTable <- metadata$content$LocatieLijst
#' mijnLocaties = c("soelekerkepolder.oost")
#' mijnParameters = c("PO4", "NO3")
#' mijnGrootheden = c("CONCTTE")
#' mijnHoedanigheden = c("Pnf")
#' #mijnCatalogus <- rws_getParameters(metadata, locatiecode = mijnLocaties) %>%
#' #  dplyr::filter(parameter.code %in% mijnParameters)
#' #getDDLdata(startyear = 2015, endyear = 2020, myCatalogue = mijnCatalogus, outDir = "testData")
#'
#' @export
getDDLdata_by_year <- function(startyear = integer(), endyear = integer(), myCatalogue, outDir = tempdir()) {

  if(outDir == tempdir()){
    print(paste("No output directory given, saving results in", tempdir()))
    # print("Proceed? y/n")
  }

  if(!dir.exists(outDir)) dir.create(outDir, recursive = T)

  # startdate <- paste0(startyear, "-01-01T09:00:00.000+01:00")
  # enddate <- paste0(endyear, "-12-31T23:00:00.000+01:00")

  # getList <- rws_makeDDLapiList(beginDatumTijd = startdate,
  #                               eindDatumTijd = enddate,
  #                               mijnCatalogus = myCatalogue
  # )

  for(year in seq(startyear, endyear, 1)){
    startdate <- paste0(year, "-01-01T09:00:00.000+01:00")
    enddate <- paste0(year + 1, "-12-31T23:00:00.000+01:00")
    getList <- rws_makeDDLapiList(beginDatumTijd = startdate,
                                          eindDatumTijd = enddate,
                                          mijnCatalogus = myCatalogue
    )
    for(jj in c(1:length(getList))){   #
      print(paste("getting", jj, myCatalogue$locatie.code[jj], year, myCatalogue$compartiment.code[jj], myCatalogue$grootheid.code[jj], myCatalogue$parameter.code[jj]))
      response <- rws_observations(bodylist = getList[[jj]])
      if(!is.null(response) & nrow(response$content)!=0){
        filename <- paste(
          myCatalogue$locatie.code[jj],
          myCatalogue$compartiment.code[jj],
          stringr::str_replace(myCatalogue$grootheid.code[jj], "[^A-Za-z0-9]+", "_"),
          myCatalogue$parameter.code[jj],
          stringr::str_replace(myCatalogue$hoedanigheid.code[jj], "[^A-Za-z0-9]+", "_"),
          year,
          "ddl.csv", sep = "_")
        readr::write_delim(response$content, file = file.path(outDir, filename), delim = ";")} else {
          print(paste("no data available for", myCatalogue$locatie.code[jj], myCatalogue$compartiment.code[jj], myCatalogue$grootheid.code[jj], myCatalogue$parameter.code[jj], myCatalogue$hoedanigheid.code[jj]))
        }
    }
  }
}



#' @title High level function, retrieves observation data from data distribution layer rws
#'
#' @description
#' TO BE WORKED OUT
#'
#' @param startyear Start year of requested data
#' @param endyear End year of requested data
#' @param myCatalogue Dataframe with location and parameter information
#' @param outDir Directory to save the downloaded information
#'
#' @return Downloaded information will be saved as csv in \code{outDir}
#'
#' @examples
#' \dontrun{
#' require(magrittr)
#' metadata <- rws_metadata() # gets complete catalog
#' subsTable <- metadata$content$aquometadatalijst
#' locsTable <- metadata$content$locatielijst
#' mijnLocaties = c("soelekerkepolder.oost")
#' mijnParameters = c("PO4", "NO3")
#' mijnGrootheden = c("CONCTTE")
#' mijnHoedanigheden = c("Pnf")
#' mijnCatalogus <- rws_getParameters(metadata, locatiecode = mijnLocaties) %>%
#'   dplyr::filter(parameter.code %in% mijnParameters)
#' getDDLdata(startyear = 2015, endyear = 2020, myCatalogue = mijnCatalogus, outDir = "testData")
#' }
#'
#' @export
getDDLdata <- function(startyear = integer(), endyear = integer(), myCatalogue, outDir = tempdir()) {

  if(outDir == tempdir()){
    print(paste("No output directory given, saving results in", tempdir()))
    # print("Proceed? y/n")
  }

  if(!dir.exists(outDir)) dir.create(outDir, recursive = T)

  startdate <- paste0(startyear, "-01-01T09:00:00.000+01:00")
  enddate <- paste0(endyear, "-12-31T23:00:00.000+01:00")

  getList <- rws_makeDDLapiList(beginDatumTijd = startdate,
                                eindDatumTijd = enddate,
                                mijnCatalogus = myCatalogue
  )

    for(jj in c(1:length(getList))){   #
      print(paste("getting", jj, myCatalogue$locatie.code[jj], paste0(startyear, "- ", endyear), myCatalogue$compartiment.code[jj], myCatalogue$grootheid.code[jj], myCatalogue$parameter.code[jj]))
      response <- rws_observations(bodylist = getList[[jj]])
      if(!is.null(response) & nrow(response$content)!=0){
        filename <- paste(
          myCatalogue$locatie.code[jj],
          myCatalogue$compartiment.code[jj],
          stringr::str_replace(myCatalogue$grootheid.code[jj], "[^A-Za-z0-9]+", "_"),
          myCatalogue$parameter.code[jj],
          stringr::str_replace(myCatalogue$hoedanigheid.code[jj], "[^A-Za-z0-9]+", "_"),
          startyear, endyear,
          "ddl.csv", sep = "_")
        readr::write_delim(response$content, file = file.path(outDir, filename), delim = ";")} else {
          print(paste("no data available for", myCatalogue$locatie.code[jj], myCatalogue$compartiment.code[jj], myCatalogue$grootheid.code[jj], myCatalogue$parameter.code[jj], myCatalogue$hoedanigheid.code[jj]))
    }
  }
}


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
#' \donttest{
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

#' @title Collects selection of metadata for long term monitoring observation at Rijkswaterstaat (NL)
#'
#' @description
#' TO BE WORKED OUT
#'
#' @param compartiment Compartment (matrix) used for filtering metadata. Compartments can be expressed as codes, or names (omschrijving). Examples are "OW" for surface water, "BS" for Bottom/Sediment.
#' @param grootheid Grootheid (quantity) used for filtering metadata.
#' @param parameter Parameter (quality) used for filtering metadata.
#' @param hoedanigheid Hoedanigheid used for filtering metadata.
#' @param locatie Location used for filtering metadata, expressed as code or name (omschrijving).
#'
#' @return A structured list with metadata, class "rws_api"
#'
#' @examples
#' \dontrun{
#' # Collect all metadata:
#' metadata <- get_selected_metadata()
#' #Collect all metadata for quantity "Waterhoogte":
#' selectedmetadata <- get_selected_metadata(grootheid = "Waterhoogte")
#' }
#' @importFrom tidyr unnest
#'
#' @export
get_selected_metadata <- function(
    compartiment = NULL,
    grootheid = NULL,
    parameter = NULL,
    hoedanigheid = NULL,
    locatie = NULL
    # filterlist = list(Eenheden=T, Grootheden=T, Parameters=T, Hoedanigheden=T, Compartimenten = T),
    # path = "/METADATASERVICES_DBO/OphalenCatalogus/"
) {

  md <- rws_metadata()

  #unnested <- tidyr::unnest(md$content$aquometadatalijst,names_sep = ".",c(compartiment, eenheid, grootheid, hoedanigheid, parameter))
  unnested <- md$content$aquometadatalijst

  selectionfilter <- ifelse(is.null(grootheid),    TRUE, unnested$grootheid.omschrijving %in% grootheid | unnested$grootheid.code %in% grootheid) &
                     ifelse(is.null(parameter),    TRUE, unnested$parameter.omschrijving %in% parameter | unnested$parameter.code %in% parameter) &
                     ifelse(is.null(hoedanigheid), TRUE, unnested$hoedanigheid.code %in% hoedanigheid | unnested$hoedanigheid.code %in% hoedanigheid) &
                     ifelse(is.null(compartiment), TRUE, unnested$compartiment.code %in% compartiment | unnested$compartiment.code %in% compartiment)

  filtered <- unnested[selectionfilter,]

  merged1 <- dplyr::left_join(filtered,md$content$aquometadatalocatielijst,by = c(aquometadata_messageid = "aquometadata_messageid"))
  merged2 <- dplyr::left_join(merged1,md$content$locatielijst)

  selectionfilter <- ifelse(is.null(locatie), TRUE, merged2$naam %in% locatie |  merged2$code %in% locatie)
  filtered2 <- merged2[selectionfilter,]
  names(filtered2) <- tolower(names(filtered2))
  names(filtered2)[names(filtered2)%in% c("naam","code")] <- paste0("locatie.",names(filtered2)[names(filtered2)%in% c("naam","code")])
  return(filtered2)
}

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}



#' @title Collect observational data from long-term monitoring efforts at Rijkswaterstaat (NL)
#'
#' @description
#' TO BE WORKED OUT
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
#'       MeetApparaat = list(Code = "109"),
#'       Grootheid = list(Code = "Hm0"))),
#'   Locatie = list(
#'     Code = "europlatform"),
#'   Periode = list(Begindatumtijd = "2012-01-27T09:00:00.000+01:00",
#'                  Einddatumtijd = "2012-01-28T09:01:00.000+01:00")
#' )
#' observations <- rws_observations(l2)
#' str(observations)
#' }
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
    response <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),simplifyVector = FALSE)
#    parsed   <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"),simplifyVector = TRUE,flatten = T)


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
                typering.code                         = response$waarnemingenlijst[[ii]]$aquometadata$typering$code,
                typering.omschrijving                 = response$waarnemingenlijst[[ii]]$aquometadata$typering$omschrijving,
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

#' @title Collects observed quantities and parameters for stations
#'
#' @description
#' TO BE WORKED OUT
#'
#' @param metadata parsed list of metadata generated from rws_metadata()
#' @param grootheidcode character vector of selected grootheid.code according to AQUO
#' @param parametercode character vector of selected parameter.code according to AQUO
#'
#' @return dataframe containing locations where grootheidcode and parametercode occur
#'
#' @examples
#' metadata <- rws_metadata()
#' rws_getLocations(metadata, 'SALNTT', 'NVT')
#' rws_getLocations(metadata, 'salntt', 'nvt') # no case-sensitivity
#'
#' @export
rws_getLocations <- function(metadata, grootheidcode, parametercode = NULL) {

  if(!is.null(metadata$content)) myMetadata <- metadata$content else myMetadata <- metadata

  # grootheidcode = 'salntt'; parametercode = 'nvt'

  flattened <- dplyr::bind_cols(rlist::list.flatten(myMetadata$aquometadatalijst))
  names(flattened) <- tolower(names(flattened))

  filtered <- flattened[tolower(flattened$grootheid.code) %in% tolower(grootheidcode),]

  if(!is.null(parametercode)) filtered <- filtered[tolower(filtered$parameter.code) %in% tolower(parametercode),]

  merged1 <- dplyr::left_join(filtered,dplyr::as_tibble(rlist::list.flatten(myMetadata$aquometadatalocatielijst)), by = c(aquometadata_messageid = 'aquometadata_messageid'))
  merged2 <- dplyr::left_join(merged1,dplyr::as_tibble(rlist::list.flatten(myMetadata$locatielijst)))
  names(merged2) <- tolower(names(merged2))
  res <- merged2[,c("aquometadata_messageid",
                    "locatie_messageid",
                    "parameter_wat_omschrijving",
                    "compartiment.code","compartiment.omschrijving",
                    "eenheid.code","eenheid.omschrijving",
                    "grootheid.code","grootheid.omschrijving",
                    "hoedanigheid.code","hoedanigheid.omschrijving",
                    "parameter.code","parameter.omschrijving",
                    "naam","code","lon","lat","coordinatenstelsel"
                   )]
  return(res)
}


#' @title Collects observed quantities and parameters for stations.
#'
#' @description
#' TO BE WORKED OUT
#'
#' @param metadata parsed list of metadata generated from rws_metadata()
#' @param locatiecode character vector of selected locatie.code
#' @param locatienaam character vector of selected locatie.naam
#'
#' @return dataframe containing locations where grootheidcode and parametercode occur
#'
#' @examples
#' metadata <- rws_metadata()
#' rws_getParameters(metadata, locatiecode='4epetroleumhaven')
#' rws_getParameters(metadata, locatienaam='A12 platform')
#'
#' @export
rws_getParameters <- function(metadata, locatiecode = NULL, locatienaam = NULL) {

  if(!is.null(metadata$content)) myMetadata <- metadata$content else myMetadata <- metadata

  dat <- dplyr::as_tibble(rlist::list.flatten(myMetadata$locatielijst))
  names(dat) <- tolower(names(dat))

  if (is.null(locatienaam)) {
     filtered1 <- dplyr::filter(dat, dat$code %in% locatiecode)
  } else {
     filtered1 <- dplyr::filter(dat, dat$naam %in% locatienaam)
  }

  merged1 <- dplyr::left_join(filtered1,dplyr::as_tibble(rlist::list.flatten(myMetadata$aquometadatalocatielijst)),by = c(locatie_messageid = 'locatie_messageid'))
  merged2 <- dplyr::left_join(merged1,dplyr::bind_cols(rlist::list.flatten(myMetadata$aquometadatalijst)), by = c(aquometadata_messageid = 'aquometadata_messageid'))
  names(merged2) <- tolower(names(merged2))
  res <- merged2[,c("aquometadata_messageid",
                    "locatie_messageid","parameter_wat_omschrijving",
                    "compartiment.code","compartiment.omschrijving",
                    "eenheid.code","eenheid.omschrijving",
                    "grootheid.code","grootheid.omschrijving",
                    "hoedanigheid.code","hoedanigheid.omschrijving",
                    "parameter.code","parameter.omschrijving",
                    "naam","code","lon","lat","coordinatenstelsel"
                   )]
  names(res)[names(res)%in% c("naam","code")] <- paste0("locatie.",names(res)[names(res)%in% c("naam","code")])
  return(res)
}

#' @title makes list for requesting observation data from rws api
#'
#' @description
#' TO BE WORKED OUT
#'
#' @param mijnCatalogus catalogue created using rws_metadata
#' @param beginDatumTijd date/time indication of the first observation to select
#' @param eindDatumTijd date/time indication of the last observation to select
#'
#' @return dataframe containing observed quantities and parameters
#'
#' @examples
#' metadata <- rws_metadata()
#' # parse content of response
#' #catalogue <- DDLgetParametersForLocations(parsedmetadata, c("Dreischor", "Herkingen", "Scharendijke diepe put"))
#'
#' @export
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
        Lon = stringr::str_pad(as.character(mijnCatalogus["lon"][ii,]), 16, "right", "0"),
        Lat = stringr::str_pad(as.character(mijnCatalogus["lat"][ii,]), 16, "right", "0"),
        Code = as.character(mijnCatalogus["locatie.code"][ii,])),
      Periode = list(Begindatumtijd = beginDatumTijd,
                     Einddatumtijd = eindDatumTijd)
    )
    ll[[ii]] <- l
  }
  return(ll)
}


#                  # selects locations within DDL based on WFD water bodies from the Netherlands
#                  #
#                  # @param metadata metadata from DDL. download using rws_metadata()
#                  # @param myWaterBody Name or partial name of the waterbody of interest
#                  # @param buffer_in_m buffer for finding locations in meters
#                  # @return dataframe with selected locations
#                  # @examples
#                  # metadata <- rws_metadata()
#                  # select_locations_in_waterbody(metadata, "westerschelde", 0)
#                  # select_locations_in_waterbody(metadata, "westerschelde", 2000) # also retrieves "Schaar van Ouden Doel".
#                  # @export
#                  select_locations_in_waterbody <- function(metadata, myWaterBody, buffer_in_m) {
#
#                    # check if metadata is correct, name is correct
#                    # comment: run this first:
#                    # > metadata <- rws_metadata() # gets complete catalog
#                    locsTable <- metadata$content$locatielijst
#
#                    if(length(dplyr::distinct(locsTable, coordinatenstelsel) == 1)) {
#                      locs_sf <- sf::st_as_sf(locsTable, coords = c("lon", "lat"), crs = 25831)
#                      locs_sf_rd <- sf::st_transform(locs_sf, crs = 28992)
#                    } else print("warning, multiple epsg, sf object not produced")
#
#                    # download water bodies for 2006 , 2018 returns error for some reason
#                    typename='kaderrichtlijnwater:krw_oppervlaktewaterlichamen_vlakken_rws_2006'
#                    dsn = 'https://geodata.nationaalgeoregister.nl/kaderrichtlijnwater/wfs?service=WFS&request=getCapabilities'
#                    wb <- sf::st_read(dsn, "kaderrichtlijnwater:krw_oppervlaktewaterlichamen_vlakken_rws_2006")
#                    # st_crs(wb) # check crs
#                    mijnShape <- wb[grepl(x = tolower(wb$OWMNAAM), pattern = tolower(myWaterBody)),]
#                    # buffer_in_m <- 2000 # for testing
#
#                    selected <- sf::st_drop_geometry(sf::st_intersection(locs_sf_rd, sf::st_buffer(mijnShape, buffer_in_m)))
#                    codes <- dplyr::distinct(selected,code)
#                    mijnLocaties <- dplyr::left_join(codes,locsTable)
#                    return(mijnLocaties)
#                  }
#
#                  # selects locations within DDL based on polygon
#                  #
#                  # @param metadata metadata from DDL. download using rws_metadata()
#                  # @param polygon polygon of interest as sf object (?sf)
#                  # @param buffer_in_m buffer for finding locations in meters
#                  # @return dataframe with selected locations
#                  # @examples
#                  # metadata <- rws_metadata()
#                  # select_locations_in_waterbody(metadata, "westerschelde", 0)
#                  # select_locations_in_waterbody(metadata, "westerschelde", 2000) # also retrieves "Schaar van Ouden Doel".
#                  # select_locations_by_polygon
#                  # @export
#                  select_locations_by_polygon <- function(metadata, polygon, buffer_in_m) {
#
#                    # check if metadata is correct, name is correct
#                    # comment: run this first:
#                    # > metadata <- rws_metadata() # gets complete catalog
#                    locsTable <- metadata$content$locatielijst
#
#                    if(length(dplyr::distinct(locsTable, coordinatenstelsel) == 1)) {
#                      locs_sf <- sf::st_as_sf(locsTable, coords = c("lon", "lat"), crs = 25831)
#                      locs_sf_rd <- sf::st_transform(locs_sf, crs = 28992)
#                    } else print("warning, multiple epsg, sf object not produced")
#
#                    mijnShape <- sf::st_transform(polygon, crs = 28992)
#
#                    # buffer_in_m <- 2000 # for testing
#                    selected <- sf::st_drop_geometry(sf::st_intersection(locs_sf_rd, sf::st_buffer(mijnShape, buffer_in_m)))
#                    codes <- dplyr::distinct(selected,code)
#                    mijnLocaties <- dplyr::left_join(codes,locsTable)
#                    return(mijnLocaties)
#                  }
#
rws_wmsLocations <- function(){
  url = "https://waterwebservices.rijkswaterstaat.nl/services/ogc/hws/wmdc15/ows?SERVICE=WFS&VERSION=1.1.0&REQUEST=GetFeature&TYPENAME=wmdc15:locaties"
  url = "https://waterwebservices.rijkswaterstaat.nl/services/distributielaagWFS/distributielaag_dbo?SERVICE=WFS&VERSION=1.1.0&REQUEST=GetFeature&TYPENAME=locatiesmetlaatstewaarneming&Maxfeatures=50"
httr::parse_url(url)
  df <- sf::st_read(url)
}
