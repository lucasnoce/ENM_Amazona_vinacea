# Load the package
library(rinat)
library(dplyr)
library(stringr)
library(tidyr)

# Download occurrence data for Amazona vinacea from iNaturalist
species_data <- get_inat_obs(query = "Amazona vinacea")

# View the first few records
head(species_data)

write.csv(species_data, "data/download_ocorrencias/oc_inaturalist_completo.csv", row.names = FALSE)

# Filter to retain only geolocation and other key columns
inat_data <- species_data %>%
  dplyr::select(id, latitude, longitude, observed_on, place_guess, scientific_name)

country_lookup <- data.frame(
  code = c("BR", "AR", "CZ", "PL"),  # Add more country codes as needed
  name = c("Brazil", "Argentina", "Czech Republic", "Poland")
)

inat_data <- inat_data %>%
  mutate(country_code = case_when(
    str_detect(place_guess, ", ") ~ str_sub(place_guess, -2),  # For entries with a comma
    TRUE ~ str_extract(place_guess, "\\b(\\w+)$")  # For other entries, get the last word (country code or full country name)
  )) %>%
  left_join(country_lookup, by = c("country_code" = "code")) %>%  # Join to get full country name
  mutate(country = ifelse(is.na(name), country_code, name)) %>%  # Use country_code if name is NA
  select(-country_code, -name, -place_guess)  # Remove temporary columns

inat_data <- inat_data %>%
  dplyr::select(id, latitude, longitude, observed_on, country, scientific_name)

inat_data$database <- "iNaturalist"

colnames(inat_data) <- c("id", "lat", "lon", "date", "country", "species", "database")

inat_data <- inat_data %>%
  filter(!is.na(id)) %>%
  filter(!is.na(lat)) %>%
  filter(!is.na(lon)) %>%
  filter(!is.na(date)) %>%
  filter(!is.na(country)) %>%
  filter(!is.na(species)) %>%
  filter(if_all(everything(), ~ nchar(.) > 0)) # Remove rows that have missing data in any of the columns

inat_data <- inat_data %>%
  distinct() %>%
  distinct(lat, lon, .keep_all = TRUE)

inat_data$date <- gsub("-00", "-01", inat_data$date)
inat_data$date <- as.Date(inat_data$date, format = "%Y-%m-%d")

inat_data <- inat_data[inat_data$date > as.Date("2004-01-01"), ]

write.csv(inat_data, "data/download_ocorrencias/oc_inaturalist_filtrado.csv", row.names = FALSE)
