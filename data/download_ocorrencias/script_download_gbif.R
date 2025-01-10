library(dismo)
library(dplyr)
library(tidyr)

gbif_data <- dismo::gbif(
  genus = "Amazona",
  species = "vinacea",
  geo = TRUE,
  removeZeros = TRUE,
  download = TRUE
)

gbif_data <- as.data.frame(gbif_data)

write.csv(gbif_data, "data/download_ocorrencias/oc_gbif_completo.csv", row.names = FALSE)

# filtragem dos dados de ocorrência
gbif_data <- gbif_data %>%
  select(gbifID, lat, lon, eventDate, country, species)

gbif_data$database <- "Gbif"

colnames(gbif_data) <- c("id", "lat", "lon", "date", "country", "species", "database")

gbif_data <- gbif_data %>%
  filter(!is.na(id)) %>%
  filter(!is.na(lat)) %>%
  filter(!is.na(lon)) %>%
  filter(!is.na(date)) %>%
  filter(!is.na(country)) %>%
  filter(!is.na(species)) %>%
  filter(if_all(everything(), ~ nchar(.) > 0)) # Remove rows that have missing data in any of the columns

gbif_data <- gbif_data %>%
  distinct() %>%
  distinct(lat, lon, .keep_all = TRUE)

gbif_data$date <- gsub("-00", "-01", gbif_data$date)
gbif_data$date <- as.Date(gbif_data$date, format = "%Y-%m-%d")

gbif_data <- gbif_data[gbif_data$date > as.Date("2004-01-01"), ]

write.csv(gbif_data, "data/download_ocorrencias/oc_gbif_filtrado.csv", row.names = FALSE)
