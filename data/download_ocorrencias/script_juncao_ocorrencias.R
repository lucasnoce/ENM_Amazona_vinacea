library(dplyr)
library(tidyr)
library(sf)
library(terra)

xc <- read.csv("data/download_ocorrencias/oc_xeno-canto_completo.csv", header = TRUE)
inat <- read.csv("data/download_ocorrencias/oc_inaturalist_completo.csv", header = TRUE)
gbif <- read.csv("data/download_ocorrencias/oc_gbif_completo.csv", header = TRUE)

combined_df <- bind_rows(xc, inat, gbif)
write.csv(combined_df, "data/download_ocorrencias/oc_combinado_completo.csv", row.names = FALSE)


xc <- read.csv("data/download_ocorrencias/oc_xeno-canto_filtrado.csv", header = TRUE)
inat <- read.csv("data/download_ocorrencias/oc_inaturalist_filtrado.csv", header = TRUE)
gbif <- read.csv("data/download_ocorrencias/oc_gbif_filtrado.csv", header = TRUE)

combined_df <- bind_rows(xc, inat, gbif)
combined_df <- combined_df %>%
  mutate(myID = row_number())
write.csv(combined_df, "data/download_ocorrencias/oc_combinado_filtrado.csv", row.names = FALSE)

combined_df <- combined_df %>%
  filter(!is.na(id)) %>%
  filter(!is.na(lat)) %>%
  filter(!is.na(lon)) %>%
  filter(!is.na(date)) %>%
  filter(!is.na(country)) %>%
  filter(!is.na(species)) %>%
  filter(if_all(everything(), ~ nchar(.) > 0)) # Remove rows that have missing data in any of the columns

combined_df <- combined_df %>%
  drop_na() %>%
  distinct() %>%
  distinct(lat, lon, .keep_all = TRUE)

combined_df$date <- gsub("-00", "-01", combined_df$date)
combined_df$date <- as.Date(combined_df$date, format = "%Y-%m-%d")
combined_df <- combined_df[combined_df$date > as.Date("2004-01-01"), ]

combined_df_flag <- CoordinateCleaner::clean_coordinates(
  x = combined_df,
  species = "species",
  lon = "lon",
  lat = "lat",
  seas_scale = 10,
  tests = c("capitals",       # radius around capitals
            "centroids",      # radius around country and province centroids
            "duplicates",     # records from one species with identical coordinates
            "equal",          # equal coordinates
            "gbif",           # radius around GBIF headquarters
            "institutions",   # radius around biodiversity institutions
            "outliers",       # records far away from all other records of this species
            "seas",           # in the sea
            "urban",          # within urban area
            "validity",       # outside reference coordinate system
            "zeros" ),        # plain zeros and lat = lon
  capitals_rad=100,
  centroids_rad=100
)

combined_df_flag_rem <- combined_df %>%
  dplyr::filter(combined_df_flag$.summary == TRUE)

write.csv(combined_df_flag_rem, "data/download_ocorrencias/oc_combinado_muito_filtrado.csv", row.names = FALSE)


## ================================================
# Check points and manually remove inconsistencies:

br_ext_xmin <- -55.0
br_ext_xmax <- -34.0
br_ext_ymin <- -35.0
br_ext_ymax <- -15.0

brazil_extent <- ext(br_ext_xmin, br_ext_xmax, br_ext_ymin, br_ext_ymax)
brazil_bbox <- st_as_sfc(st_bbox(c(xmin = br_ext_xmin, ymin = br_ext_ymin, xmax = br_ext_xmax, ymax = br_ext_ymax), crs = st_crs(4674)))

BR <- read_sf(dsn="variaveis/Estados_Brasil/EstadosBR_IBGE_LLWGS84.shp")
MA <- read_sf(dsn="temp/polygonma.shp")
MA_s2000 <- st_transform(MA, crs = 4674)
BR_s2000 <- st_transform(BR, crs = 4674)
BR_s2000_bbox <- st_intersection(BR_s2000, brazil_bbox)

oc_combined <- read.csv("data/download_ocorrencias/oc_combinado_muito_filtrado.csv")
oc_combined <- oc_combined %>%
  select(species, lon, lat)
oc_combined <- st_as_sf(
  x = oc_combined,
  coords = c("lon", "lat"),
  crs = 4674
)
plot(st_geometry(BR_s2000_bbox))
plot(MA_s2000, add=TRUE)
points(oc_combined, col="green", cex=0.5)
# Confirm points to be removed mannualy:
points(oc_combined[963,], col="red", cex=0.5)
points(oc_combined[656,], col="red", cex=0.5)
points(oc_combined[1115,], col="red", cex=0.5)
points(oc_combined[896,], col="red", cex=0.5)

combined_df_flag_rem <- combined_df_flag_rem[-963, ]
combined_df_flag_rem <- combined_df_flag_rem[-656, ]
combined_df_flag_rem <- combined_df_flag_rem[-1115, ]
combined_df_flag_rem <- combined_df_flag_rem[-896, ]

write.csv(combined_df_flag_rem, "data/download_ocorrencias/oc_combinado_muito_filtrado.csv", row.names = FALSE)



## ================================================
# Check points for each database:

oc_gbif <- read.csv("data/download_ocorrencias/oc_gbif_filtrado.csv")
oc_gbif_geom <- oc_gbif %>%
  select(species, lon, lat) %>%    # seleciona apenas essas 3 colunas
  distinct() %>%                   # remove ocorrências duplicadas
  drop_na()                        # remove espaços vazios
oc_gbif_geom <- st_as_sf(
  x = oc_gbif_geom,
  coords = c("lon", "lat"),
  crs = 4674
)

oc_inat <- read.csv("data/download_ocorrencias/oc_inaturalist_filtrado.csv")
oc_inat_geom <- oc_inat %>%
  select(species, lon, lat) %>%    # seleciona apenas essas 3 colunas
  distinct() %>%                   # remove ocorrências duplicadas
  drop_na()                        # remove espaços vazios
oc_inat_geom <- st_as_sf(
  x = oc_inat_geom,
  coords = c("lon", "lat"),
  crs = 4674
)

oc_xeno <- read.csv("data/download_ocorrencias/oc_xeno-canto_filtrado.csv")
oc_xeno_geom <- oc_xeno %>%
  select(species, lon, lat) %>%    # seleciona apenas essas 3 colunas
  distinct() %>%                   # remove ocorrências duplicadas
  drop_na()                        # remove espaços vazios
oc_xeno_geom <- st_as_sf(
  x = oc_xeno_geom,
  coords = c("lon", "lat"),
  crs = 4674
)

plot(st_geometry(BR_s2000))
plot(MA, add=TRUE)
points(oc_gbif_geom, col="red", cex=0.5)
points(oc_inat_geom, col="green", cex=0.5)
points(oc_xeno_geom, col="blue", cex=0.5)
legend("bottomright",                                # Position of the legend
       legend = c("Gbif", "iNaturalist", "Xeno-Canto"),          # Labels for the legend
       col = c("red", "green", "blue"),                    # Line colors
       pch = 16)

