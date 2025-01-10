library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# Define the species name and make the API request
species <- "Amazona+vinacea"
url <- paste0("https://www.xeno-canto.org/api/2/recordings?query=", species)

# Make the GET request
response <- GET(url)

# Parse the response as JSON
data <- content(response, as = "text", encoding = "UTF-8")
parsed_data <- fromJSON(data)
print(parsed_data)

recordings_df <- parsed_data$recordings
parsed_data_df <- as.data.frame(recordings_df)

write.csv(parsed_data_df, "data/download_ocorrencias/oc_xeno-canto_completo.csv", row.names = FALSE)

# Extract the relevant geolocation data columns
xc_data <- parsed_data$recordings
xc_data$latitude <- as.numeric(xc_data$lat)
xc_data$longitude <- as.numeric(xc_data$lng)

xc_data$species <- paste(xc_data$gen, xc_data$sp, sep = " ")

xc_data <- xc_data %>%
  dplyr::select(id, lat, lng, date, cnt, species)

xc_data$database <- "Xeno-Canto"

colnames(xc_data) <- c("id", "lat", "lon", "date", "country", "species", "database")

xc_data <- xc_data %>%
  filter(!is.na(id)) %>%
  filter(!is.na(lat)) %>%
  filter(!is.na(lon)) %>%
  filter(!is.na(date)) %>%
  filter(!is.na(country)) %>%
  filter(!is.na(species)) %>%
  filter(if_all(everything(), ~ nchar(.) > 0)) # Remove rows that have missing data in any of the columns

xc_data <- xc_data %>%
  distinct() %>%
  distinct(lat, lon, .keep_all = TRUE)

xc_data$date <- gsub("-00", "-01", xc_data$date)
xc_data$date <- as.Date(xc_data$date, format = "%Y-%m-%d")

xc_data <- xc_data[xc_data$date > as.Date("2004-01-01"), ]

write.csv(xc_data, "data/download_ocorrencias/oc_xeno-canto_filtrado.csv", row.names = FALSE)
