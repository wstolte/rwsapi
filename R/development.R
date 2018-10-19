
# not run
#
# require(tidyverse)
#
# ## run once to add parametermap as rdata file to the package
# # parametermap <- read_delim("../RWS-WebServices/parameters-en-groepen.csv", delim = ";")
# # devtools::use_data(parametermap)
#
# #===check functionality=======================
#
#
# source("R/RWS-API.R")
# require(tidyverse)
#
metadata <- rws_metadata() # gets complete catalog
parsedmetadata <- jsonlite::fromJSON(content(metadata$resp, "text"), simplifyVector = T )
# # which(parsedmetadata$AquoMetadataLijst$Parameter$Omschrijving == "ammonium")

grep(x = parsedmetadata$AquoMetadataLijst$Parameter_Wat_Omschrijving, pattern = "chlorofyl")
parsedmetadata$AquoMetadataLijst$Parameter_Wat_Omschrijving[298]



catalogue <- DDLgetParametersForLocations(parsedmetadata, c("Dreischor", "Herkingen", "Scharendijke diepe put"))


load("D:/GitHub_working_copies/rwsapi/R/sysdata.rda")

enrichedMetadata <- read_delim("d:/REPOS-CHECK-OUTS/digitaleWaterSysteemRapportage/grevelingen_website/metadataDDL.csv", delim = ";")
names(enrichedMetadata)[names(enrichedMetadata)=="Klasse"] <- "parametergroep"
save(list = c("enrichedMetadata", "donarlocations"), file = 'R/sysdata.rda')

