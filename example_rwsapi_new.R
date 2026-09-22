
# devtools::install_github("wstolte/rwsapi@develop-wadar-adaptation")

require(rwsapi)

md <- rwsapi::rws_metadata()

catalogue <- md$content$locatielijst %>%
  full_join(md$content$aquometadatalocatielijst) %>%
  full_join(md$content$aquometadatalijst)

mycatalogue <- catalogue %>%
  filter(
    grepl("maassluis", naam, ignore.case = T),
    parameter.omschrijving == "chloride"
  )

l <- rws_observation_query(
  metadata = mycatalogue,
  start_date = as.Date("2000-01-01"),
  end_date = as.Date("2000-01-31")
)

observations <- rws_observations(l)
df <- observations$content


mycatalogue2 <- catalogue %>%
  filter(
    grepl("maassluis", naam, ignore.case = T),
    grepl("chloride", parameter.omschrijving)
  )

l2 <- rws_observation_queries(
  metadata = mycatalogue2,
  start_date = as.Date("2000-01-01"),
  end_date = as.Date("2001-03-01")
  )

observations <- lapply(l2, rws_observations)

obs_df <- dplyr::bind_rows(
  Filter(Negate(is.null),
         lapply(observations, `[[`, "content"))
)




