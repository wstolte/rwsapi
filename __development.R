
# not run
#
# require(tidyverse)
#
enrichedMetadata_dev <- read.csv2("data/enrichedMetadata.csv")
## add more groups or change group names if necessary. Careful!!
# option, not used anymore.
# devtools::use_data(enrichedMetadata, donarlocations,
#                    internal = TRUE, overwrite = TRUE)

enrichedMetadata <- enrichedMetadata_dev
enrichedMetadata$Parameter.Omschrijving <- gsub("\'","___", (enrichedMetadata$Parameter.Omschrijving))
enrichedMetadata$Parameter_Wat_Omschrijving <- gsub("\'","___", (enrichedMetadata$Parameter_Wat_Omschrijving))

donarlocations

# save("enrichedMetadata", "donarlocations", file = "data/rwsapidata.rdata")



#
# #===check functionality=======================
#
#
source("R/RWS-API.R")
data(rwsapidata)

require(tidyverse)

metadata <- rws_metadata() # gets complete catalog

which(metadata$content$AquoMetadataLijst$Parameter$Omschrijving == "ammonium")

grep(x = metadata$content$AquoMetadataLijst$Parameter_Wat_Omschrijving, pattern = "chlorofyl")

metadata$content$AquoMetadataLijst$Parameter_Wat_Omschrijving[298]

myCatalogue <- DDLgetParametersForLocations(myMetadata = metadata, locationlist = c("Dreischor", "Herkingen", "Scharendijke diepe put")) %>%
  dplyr::filter(parametergroep == "eutrophication")


# per parameter,

startyear = 2005
endyear = 2009
startdate <- paste0(startyear, "-11-27T09:00:00.000+01:00")
enddate <- paste0(endyear, "-01-28T09:01:00.000+01:00")

getList <- makeDDLapiList(beginDatumTijd = startdate,
                        eindDatumTijd = enddate,
                        mijnCompartiment = "OW",
                        mijnCatalogus = myCatalogue
)

for(jj in seq(1:length(getList))){
  # jj = 29
  response <- rws_observations(bodylist = getList[[jj]])
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






