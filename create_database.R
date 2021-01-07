library(tidyverse)
library(lubridate)

# Read a source file ------------------------------------------------------
read_source_file <- function(filename) {
  read_data <- read.table(filename,
    sep = "\n",
    skip = 23 + 1, stringsAsFactors = FALSE,
    quote = ""
  ) %>%
    mutate(
      STAID = str_sub(V1, 1, 5) %>% as.numeric(),
      SOUID = str_sub(V1, 7, 12) %>% as.numeric(),
      SOUNAME = str_sub(V1, 14, 53),
      CN = str_sub(V1, 55, 56),
      LAT = str_sub(V1, 58, 66),
      LON = str_sub(V1, 68, 77),
      HGTH = str_sub(V1, 79, 82) %>% as.numeric(),
      ELEI = str_sub(V1, 84, 87),
      START = str_sub(V1, 89, 96) %>% parse_date(format = "%Y%m%d"),
      STOP = str_sub(V1, 98, 105) %>% parse_date(format = "%Y%m%d"),
      PARID = str_sub(V1, 107, 111),
      PARNAME = str_sub(V1, 113, 163)
    ) %>%
    select(-V1) %>%
    as_tibble()
  return(read_data)
}
# Get the list of source files
source_files <- list.files("data",
  pattern = glob2rx("ECA_blend_source*txt"),
  full.names = TRUE
)
# Read all the source files
source_data <- lapply(source_files, read_source_file) %>%
  bind_rows()

# Read station file ---------------------------------------------------
read_station_file <- function(filename) {
  read_data <- read_csv(filename,
    skip = 17
  )
  return(read_data)
}
# Get the list of station files
stn_files <- list.files("data",
  pattern = glob2rx("ECA_blend_station*txt"),
  full.names = TRUE
)
# Read all the station files
stn_data <- lapply(stn_files, read_station_file) %>%
  bind_rows() %>%
  distinct()

## Create single structure ------------------------------------------
eobs_data <- source_data %>%
  mutate(base_ELEI = str_trim(ELEI) %>% str_sub(1, 2)) %>%
  group_by(STAID, base_ELEI) %>%
  summarise(
    n_sources = n(),
    START = min(START),
    STOP = max(STOP)
  ) %>%
  inner_join(stn_data) %>%
  rowwise() %>%
  mutate( 
    filename = sprintf(
      "%s_STAID%06d.txt",
      base_ELEI,
      STAID
    ),
    lat_dec = as.numeric(str_split(LAT, ":", simplify = TRUE)[1]) +
      sign(as.numeric(str_split(LAT, ":", simplify = TRUE)[1]))*
      as.numeric(str_split(LAT, ":", simplify = TRUE)[2]) / 60 +
      sign(as.numeric(str_split(LAT, ":", simplify = TRUE)[1]))*
      as.numeric(str_split(LAT, ":", simplify = TRUE)[3]) / 3600,
    lon_dec = as.numeric(str_split(LON, ":", simplify = TRUE)[1]) +
      sign(as.numeric(str_split(LON, ":", simplify = TRUE)[1]))*
      as.numeric(str_split(LON, ":", simplify = TRUE)[2]) / 60 +
      sign(as.numeric(str_split(LON, ":", simplify = TRUE)[1]))*
      as.numeric(str_split(LON, ":", simplify = TRUE)[3]) / 3600,
    years_length = round(as.numeric(difftime(STOP, START, units = "days")) / 365.25)
  )

# Save the data for the Shiny application
write_rds(eobs_data, "eobs-database-stations.rds")
