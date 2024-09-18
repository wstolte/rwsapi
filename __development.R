
# not run
#
# #===check functionality=======================
#
#
source("R/RWS-API.R")
require(tidyverse)

metadata <- rws_metadata() # gets complete catalog

iNH4 <- which(metadata$content$AquoMetadataLijst$Parameter$Omschrijving == "ammonium")
metadata$content$AquoMetadataLijst$Parameter_Wat_Omschrijving[iNH4]

# locaties waar ammonium wordt/werd gemeten
myLocations <- rws_getLocations(metadata, grootheidcode = "CONCTTE", parametercode = "NH4")

# Stoffen die op Dreischor worden/werden gemeten
mySubstances <- rws_getParameters(metadata = metadata, locatienaam = c("Dreischor"))

myCatalogue <- rwsapi::rws_getParameters(
  metadata = metadata,
  locatienaam = c("Dreischor")) %>%
  dplyr::filter(parameter.omschrijving == "ammonium")

# per parameter,

startyear = 2005
endyear = 2009
startdate <- paste0(startyear, "-11-27T09:00:00.000+01:00")
enddate <- paste0(endyear, "-01-28T09:01:00.000+01:00")

getList <- rws_makeDDLapiList(beginDatumTijd = startdate,
                        eindDatumTijd = enddate,
                        mijnCatalogus = myCatalogue
)

for(jj in seq(1:length(getList))){
  # jj = 29
  response <- rws_observations2(bodylist = getList[[jj]])
  response$content$Foutmelding

  # loop over differet WaarnemingsLijst objects
  for(ii in seq(1:length(response$content$WaarnemingenLijst))) {
    temp.df = data.frame(
      locationcode = response$content$WaarnemingenLijst[[ii]]$Locatie$Code,
      EPSG = response$content$WaarnemingenLijst[[ii]]$Locatie$Coordinatenstelsel,
      X = response$content$WaarnemingenLijst[[ii]]$Locatie$X,
      Y = response$content$WaarnemingenLijst[[ii]]$Locatie$Y,
      # locationname = ,
      datetime = response$content$WaarnemingenLijst[[ii]]$MetingenLijst %>% map_chr(list(1), .default = NA),
      quantity = response$content$WaarnemingenLijst[[ii]]$AquoMetadata$Grootheid$Code,
      parameter = response$content$WaarnemingenLijst[[ii]]$AquoMetadata$Parameter$Code,
      Compartiment = response$content$WaarnemingenLijst[[ii]]$AquoMetadata$Compartiment$Code,
      quality = response$content$WaarnemingenLijst[[ii]]$AquoMetadata$Hoedanigheid$Code,
      value = response$content$WaarnemingenLijst[[ii]]$MetingenLijst %>% map_chr(list("Meetwaarde", "Waarde_Numeriek"), .default = NA),
      unit = response$content$WaarnemingenLijst[[ii]]$AquoMetadata$Eenheid$Code
    )
    if(ii != 1){
      df = rbind(df, temp.df)
    }else df = temp.df
  }
  if(jj != 1){
    df_all = rbind(df_all, df)
  }else df_all = df
}


#==== test tidyjson package =============================

# install.packages("tidyjson")
require(tidyjson)

source("R/RWS-API.R")
require(tidyverse)
require(rwsapi)
metadata <- rws_metadata() # gets complete catalog

# test
metadata$content$AquoMetadataLijst %>% tidyjson::as_tibble()
names(md)

which(metadata$content$AquoMetadataLijst$Parameter$Omschrijving == "ammonium")
metadata$content$AquoMetadataLijst$Parameter_Wat_Omschrijving[327]

myCatalogue <- rwsapi::rws_getParameters(
  metadata = metadata,
  locatiename = c("Dreischor")) %>%
  dplyr::filter(parameter.omschrijving == "ammonium")

# per parameter,

startyear = 2005
endyear = 2009
startdate <- paste0(startyear, "-11-27T09:00:00.000+01:00")
enddate <- paste0(endyear, "-01-28T09:01:00.000+01:00")

getList <- rws_makeDDLapiList(beginDatumTijd = startdate,
                          eindDatumTijd = enddate,
                          mijnCompartiment = "OW",
                          mijnCatalogus = myCatalogue
)

path = "/ONLINEWAARNEMINGENSERVICES_DBO/OphalenWaarnemingen/"
url <- modify_url("https://waterwebservices.rijkswaterstaat.nl", path = path)
library(httr)
library(jsonlite)
ua <- user_agent("https://waterwebservices.rijkswaterstaat.nl")

resp <- POST(url = url,
             ua,
             body=toJSON(getList[[1]], auto_unbox = T, digits = NA),
             add_headers(.headers = c("Content-Type"="application/json","Ocp-Apim-Subscription-Key"="my_subscrition_key"))
)


if (http_type(resp) != "application/json") {
  stop("API did not return application/json", call. = FALSE)
}

content(resp, "text") %>% View()

response <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

response <- tidyjson::as_tibble(content(resp, "parsed"))

content(resp, "parsed")[[1]][[1]]$Locatie %>% tidyjson::as_tibble() %>% View()
content(resp, "parsed")[[1]][[1]]$AquoMetadata %>% tidyjson::as_tibble() %>% View()
content(resp, "parsed")[[1]][[1]]$MetingenLijst[[1]] %>% tidyjson::as_tibble() %>% View()


## alternatieve manier om response te parsen using rrapply package
##

install.packages("rrapply")
library(rrapply)
library(tidyverse)

metingen <- rrapply(response$WaarnemingenLijst[[1]]$MetingenLijst, how = "melt") %>% ## melt to long df
  select(L1, L3, value) %>%
  pivot_wider(names_from = "L3", values_from = "value")                   ## reshape to wide df

locaties <- rrapply(response$WaarnemingenLijst[[1]]$Locatie, how = "melt") %>% ## melt to long df
    pivot_wider(names_from = "L1", values_from = "value")                   ## reshape to wide df

aquometadata <- rrapply(response$WaarnemingenLijst[[1]]$AquoMetadata, how = "melt") %>% ## melt to long df
  unite("name", c(L1, L2), sep = ".", na.rm = T, remove = T) %>%
  pivot_wider(names_from = "name", values_from = "value")                   ## reshape to wide df

bind_cols(metingen, locaties, aquometadata) %>% View()

## map over lists

result <- map(response$WaarnemingenLijst, function(x){

  metingen <- rrapply(x$MetingenLijst, how = "melt") %>% ## melt to long df
    select(L1, L3, value) %>%
    pivot_wider(names_from = "L3", values_from = "value")                   ## reshape to wide df

  locaties <- rrapply(x$Locatie, how = "melt") %>% ## melt to long df
    pivot_wider(names_from = "L1", values_from = "value")                   ## reshape to wide df

  aquometadata <- rrapply(x$AquoMetadata, how = "melt") %>% ## melt to long df
    unite("name", c(L1, L2), sep = ".", na.rm = T, remove = T) %>%
    pivot_wider(names_from = "name", values_from = "value")                   ## reshape to wide df

  bind_cols(locaties, metingen, aquometadata)

}) %>%

  bind_rows()
