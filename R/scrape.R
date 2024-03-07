# to scrape data
library(tidyverse)
library(gsheet)
library(fuzzyjoin)


qs_url <- "https://docs.google.com/spreadsheets/d/1E-z17KdP1RNzUOwubxs9q79fF119pSg5/edit#gid=2061677816"
maps_url <- "https://docs.google.com/spreadsheets/d/1E-z17KdP1RNzUOwubxs9q79fF119pSg5/edit#gid=1514390075"

hood_reader <- function(url, sheet) {
  data <- gsheet::construct_download_url(url = url, format = "csv")
  data <- gsheet::gsheet2tbl(url, sheetid = sheet)
  data_types <- unlist(data[1, ])
  hoods <- type_convert(data[2:nrow(data), ])

  list(hoods, data_types)
}

question_cleaner <- function(input) {
  data <- input[[1]]
  hood_names <- unique(str_extract(names(data[52:546]), "^.+[^...\\d+]"))

  # Clean up, rename columns and select the most relevant ones for analysis.
  # I don't think this is destructive

  data <- data %>%
    rename(
      "id" = 1,
      "sub" = 2,
      "sub_time" = 3,
      "postcode" = 4,
      "live_in_sheff" = 7,
      "fixed_abode" = 14,
      "age" = 21,
      "community_feels" = 33,
      "community_feels_free" = 50,
      "hood_id_full" = 51
    ) %>%
    select(
      "id",
      "sub",
      "sub_time",
      "postcode",
      "live_in_sheff",
      "fixed_abode",
      "age",
      "community_feels",
      "community_feels_free",
      "hood_id_full"
    ) %>%
    mutate(postcode = case_when(
      NA ~ NA,
      str_detect(postcode, "\\d+") ~ paste("S", str_extract(postcode, "\\d+"), sep = ""),
      str_detect(postcode, "^[^\\d]+$") ~ NA,
      TRUE ~ postcode
    ))

  # This cleans up the multi-select boxes. I think these will need to be split out into separate data frames for plotting
  data$community_feels <- map(data$community_feels, \(x) as.logical(toupper(str_split(str_extract(x, "[^\\[].*[^\\]]"), ",")[[1]])))
  data$hood_id_full <- map(data$hood_id_full, \(x) as.numeric(str_split(str_extract(x, "[^\\[].*[^\\]]"), ",")[[1]]))
  data$community_labels <- map(data$community_feels, \(x) names(input[[2]][34:49])[x])
  data$hood_id_full <- map(data$hood_id_full, \(x) if (any(!is.na(x))) {
    set_names(x, hood_names)
  })

  data
}

map_cleaner <- function(input) {
  data <- input[[1]]

  data |>
    rename(
      "id" = 1,
      "sub" = 2,
      "sub_time" = 3,
      "index" = 4,
      "wkt" = 7,
      "geojson" = 8,
      "name" = 9,
      "desc" = 10,
      "live" = 11,
      "work" = 12,
      "visit" = 13,
      "leisure" = 14,
      "other" = 15,
      "hoods_free" = 16
    ) |>
    select(-desc) |>
    mutate(
      name = tolower(name),
      name = trimws(name, which = "both"),
      name = str_remove(name, "\\p{P}")
    )
}

maps <- hood_reader(maps_url)
questions <- hood_reader(qs_url)


data_m <- map_cleaner(maps)

questions[[2]]

hood_names <- tibble('name' = unique(str_extract(names(questions[[1]][52:546]), "^.+[^...\\d+]")))

######
#delete this
######

data_m |> 
  drop_na(name) |> 
  stringdist_left_join(hood_names, by = c("name"), method = 'jw', distance_col = 'dist', max_dist = .16) |> 
  select(name.x, name.y, dist) |> 
  view()










