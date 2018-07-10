
require(tidyverse)

parametermap <- read_delim("../RWS-WebServices/parameters-en-groepen.csv", delim = ";")

devtools::use_data(parametermap)



#===check functionality=======================


source("R/RWS-API.R")
require(tidyverse)

metadata <- rws_metadata() # gets complete catalog


parsedmetadata <- jsonlite::fromJSON(content(metadata$resp, "text"), simplifyVector = T )


parameters <- DDLgetParametersForLocations(parsedmetadata, c("Dreischor", "Herkingen", "Scharendijke diepe put"))

