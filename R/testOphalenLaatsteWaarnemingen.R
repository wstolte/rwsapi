#
# require(httr)
# library(jsonlite)
#
# ua <- user_agent("https://waterwebservices.rijkswaterstaat.nl")
#
# path = "/ONLINEWAARNEMINGENSERVICES_DBO/OphalenLaatsteWaarnemingen/"
# url <- modify_url("https://waterwebservices.rijkswaterstaat.nl", path = path)
#
# # https://waterwebservices.rijkswaterstaat.nl/ONLINEWAARNEMINGENSERVICES_DBO/OphalenWaarnemingen
# # GET(url)
#
# # example json string from report
# jsontext <- '{"AquoPlusWaarnemingMetadataLijst":{"AquoMetadata":{"Compartiment":{"Code":"OW"},"Eenheid":{"Code":"cm"},"Grootheid":{"Code":"H1/3"}}},"LocatieLijst":[{"X":518882.333320247,"Y":5760829.11729589,"Code":"EURPFM"}]}'
# l1 <- jsonlite::fromJSON(jsontext)
#
# # entered as list
# l2 <- list(
#   AquoPlusWaarnemingMetadata= list(
#     AquoMetadata = list(
#       Compartiment = list(Code = "OW"),
#       Eenheid = list(Code = "cm"),
#       # Meetapparaat = list(Code = "109"),
#       Grootheid = list(Code = "Hm0"))),
#   Locatie = list(X = 518882.333320247,
#                  Y = 5760829.11729589,
#                  Code = "EURPFM"),
#   Periode = list(Begindatumtijd = "2012-01-27T09:00:00.000+01:00",
#                  Einddatumtijd = "2012-01-27T09:01:00.000+01:00")
# )
#
# resp <- POST(url, ua, body = l1, encode = "json")
# content(resp, "text")
# parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = T )


## voorbeeld message body
# {"AquoPlusWaarnemingMetadata":
# {"AquoMetadata":{"Compartiment":
# {"Code":"OW"},
#   "Eenheid":{"Code":"cm"},
#   "MeetApparaat":{"Code":"109"},
#   "Grootheid":{"Code":"Hm0"}}},
#   "Locatie":{"X":518882.333320247,"Y":5760829.11729589,"Code":"EURPFM"},
#   "Periode":{"Begindatumtijd":"2012-01-27T09:00:00.000+01:00","Einddatumtijd":"2012-01-27T09:01:00.000+01:00"}}
