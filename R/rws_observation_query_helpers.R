# rwsapi helper functions -------------------------------------------------

# Build a single observation query from one catalogue row
#' @title Build query suitable for DDL request.
#'
#' @description
#' TO BE WORKED OUT
#'
#' @param df metadata dataframe with exactly one row containing both metadata and locations
#' @param start_date start date for data download
#' @param end_date end date for data download
#' @param filters column headers to be used for query
#' @param tz time zone
#'
#' @return DDL query list
#' @export
#'
#' @examples
#' \dontrun{
#' md <- rwsapi::rws_metadata()
#'
#' catalogue <- md$content$locatielijst %>%
#'   full_join(md$content$aquometadatalocatielijst) %>%
#'   full_join(md$content$aquometadatalijst)
#'
#' mycatalogue <- catalogue %>%
#'   filter(
#'     grepl("maassluis", naam, ignore.case = T),
#'     parameter.omschrijving == "chloride"
#'   )
#'
#' l <- rws_observation_query(
#'   metadata = mycatalogue,
#'   start_date = as.Date("2000-01-01"),
#'   end_date = as.Date("2000-01-31")
#' )
#'
#' observations <- rws_observations(l)
#' df <- observations$content
#' }
#'
rws_observation_query <- function(
    metadata,
    start_date,
    end_date,
    filters = c("compartiment", "grootheid", "parameter"),
    tz = "Europe/Amsterdam"
) {

  stopifnot(is.data.frame(metadata))

  if (nrow(metadata) != 1) {
    stop(
      "metadata must contain exactly one row. ",
      "Use rws_observation_queries() for multiple rows."
    )
  }

  if (!inherits(start_date, "Date")) {
    stop("start_date must be a Date object")
  }

  if (!inherits(end_date, "Date")) {
    stop("end_date must be a Date object")
  }

  field_map <- c(
    compartiment = "compartiment.code",
    grootheid    = "grootheid.code",
    parameter    = "parameter.code",
    hoedanigheid = "hoedanigheid.code",
    eenheid      = "eenheid.code",
    orgaan       = "orgaan.code",
    typering     = "typering.code",
    biotaxon     = "biotaxon.code"
  )

  if (identical(filters, "all")) {
    filters <- names(field_map)
  }

  aquo_metadata <- list()

  for (field in filters) {

    if (!field %in% names(field_map)) {
      warning(sprintf("Unknown filter ignored: %s", field))
      next
    }

    col_name <- field_map[[field]]

    if (!col_name %in% names(metadata)) {
      next
    }

    value <- metadata[[col_name]][1]

    if (is.na(value) || value %in% c("", "NVT")) {
      next
    }

    api_name <- tools::toTitleCase(field)

    aquo_metadata[[api_name]] <- list(
      Code = value
    )
  }

  start_datetime <- format(
    as.POSIXct(start_date, tz = tz),
    "%Y-%m-%dT00:00:00.000%z"
  )

  end_datetime <- format(
    as.POSIXct(end_date, tz = tz),
    "%Y-%m-%dT23:59:59.999%z"
  )

  start_datetime <- sub(
    "([+-][0-9]{2})([0-9]{2})$",
    "\\1:\\2",
    start_datetime
  )

  end_datetime <- sub(
    "([+-][0-9]{2})([0-9]{2})$",
    "\\1:\\2",
    end_datetime
  )

  list(
    AquoPlusWaarnemingMetadata = list(
      AquoMetadata = aquo_metadata
    ),
    Locatie = list(
      Code = metadata$code[1]
    ),
    Periode = list(
      Begindatumtijd = start_datetime,
      Einddatumtijd = end_datetime
    )
  )
}


# Build one or more observation queries from a catalogue
#' @title Reduce dataframe columns by nesting single-value columns.
#'
#' @description
#' TO BE WORKED OUT
#'
#' @param metadata DDL metadata and location, can be multiple lines
#' @param start_date start date for data download
#' @param end_date end date for data download
#' @param filters column headers to be used for query
#' @param tz time zone
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' md <- rwsapi::rws_metadata()
#'
#' catalogue <- md$content$locatielijst %>%
#'   full_join(md$content$aquometadatalocatielijst) %>%
#'   full_join(md$content$aquometadatalijst)
#'
#' mycatalogue <- catalogue %>%
#'   filter(
#'     grepl("maassluis", naam, ignore.case = T),
#'     grepl("chloride", parameter.omschrijving)
#'   )
#'
#' l2 <- rws_observation_queries(
#' metadata = mycatalogue2,
#' start_date = as.Date("2000-01-01"),
#' end_date = as.Date("2001-03-01")
#' )
#'
#' observations <- lapply(l2, rws_observations)
#'
#' obs_df <- dplyr::bind_rows(
#'   Filter(Negate(is.null),
#'          lapply(observations, `[[`, "content"))
#'          )
#' }
#'
rws_observation_queries <- function(
    metadata,
    start_date,
    end_date,
    filters = c("compartiment", "grootheid", "parameter"),
    tz = "Europe/Amsterdam"
) {

  stopifnot(is.data.frame(metadata))

  n <- nrow(metadata)

  if (n == 0) {
    stop("metadata contains no rows")
  }

  if (n > 1) {
    warning(sprintf(
      "metadata contains %s rows; returning a list of %s queries.",
      n,
      n
    ))
  }

  lapply(
    seq_len(n),
    function(i) {
      rws_observation_query(
        metadata = metadata[i, , drop = FALSE],
        start_date = start_date,
        end_date = end_date,
        filters = filters,
        tz = tz
      )
    }
  )
}
