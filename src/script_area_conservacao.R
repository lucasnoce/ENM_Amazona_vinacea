library(raster)
library(terra)
library(sf)

projecao_crs <- 4674
area_proj <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"

path_MA_shp  <- "temp/polygonma.shp"
path_cnuc_pol_shp <- "temp/shp_cnuc_2024_10/shp_cnuc_2024_10_pol.shp"

path_binarios <- list(
  "binarios/Jan-05/presente.tiff",
  "binarios/Jan-05/2021-2040 ssp245.tiff",
  "binarios/Jan-05/2021-2040 ssp585.tiff",
  "binarios/Jan-05/2041-2060 ssp245.tiff",
  "binarios/Jan-05/2041-2060 ssp585.tiff",
  "binarios/Jan-05/2061-2080 ssp245.tiff",
  "binarios/Jan-05/2061-2080 ssp585.tiff",
  "binarios/Jan-05/2081-2100 ssp245.tiff",
  "binarios/Jan-05/2081-2100 ssp585.tiff"
)

MA <- read_sf(dsn = path_MA_shp)
MA_s2000 <- st_transform(MA, crs = projecao_crs)

cnuc_pol <- st_read(dsn = path_cnuc_pol_shp)

cnuc_pol <- cnuc_pol %>%
  select(uc_id, nome_uc, cria_ano, ha_total, uf, grupo, categoria, cat_iucn, situacao, geometry) %>%
  drop_na()

cnuc_pol_select <- cnuc_pol %>%
  select(uc_id, ha_total, grupo, categoria, cat_iucn, geometry) %>%
  filter(grupo == "Proteção Integral")

cnuc_pol_filtered <- st_transform(cnuc_pol_select, crs = projecao_crs)

area_matrix <- matrix(NA, nrow = length(path_binarios), ncol = 3) # matriz para armazenar os valores das áreas

cenario <- list(
  "Presente",
  "2021-2040 ssp245",
  "2021-2040 ssp585",
  "2041-2060 ssp245",
  "2041-2060 ssp585",
  "2061-2080 ssp245",
  "2061-2080 ssp585",
  "2081-2100 ssp245",
  "2081-2100 ssp585"
)

binary_colors <- c("#440154ff", "#fde725ff")

for ( i in seq_along( path_binarios ) ) {
  print(i)
  binarios_raster <- terra::rast(path_binarios[[i]])
  binarios_raster <- crop(binarios_raster, extent(MA_s2000))
  binarios_raster <- mask(binarios_raster, MA_s2000)
  binarios_raster <- crop(binarios_raster, extent(cnuc_pol_filtered))
  binarios_raster <- mask(binarios_raster, cnuc_pol_filtered)
  binarios_raster <- raster::stack(binarios_raster)
  
  binarios_projected <- projectRaster(binarios_raster, crs = area_proj)  # Reproject to UTM
  par(oma = c(0, 0, 0, 0))  # bottom, left, top, right
  plot(binarios_projected,
       main = cenario[[i]],
       col = binary_colors,
       axes = FALSE,
       legend = FALSE)
  
  legend(
    "topleft",
    inset = c(0.02, 0),
    legend = c("0", "1"),
    fill = binary_colors,
    title = "Adequabilidade",
    bty = "n",
    x.intersp = 6,
    adj = c(8, .5))
  
  cell_size_m <- res(binarios_projected)[1]
  cell_area_m2 <- cell_size_m^2
  cell_area_km2 <- cell_area_m2 / 1e6
  
  freq_table     <- freq(binarios_projected)
  count_non_na   <- sum(freq_table[freq_table[, "value"] %in% c(0, 1), "count"])
  total_area_km2 <- count_non_na * cell_area_km2
  
  area_class_0_km2 <- as.numeric(freq_table[freq_table[, "value"] %in% c(0), "count"] * cell_area_km2)
  area_class_1_km2 <- as.numeric(freq_table[freq_table[, "value"] %in% c(1), "count"] * cell_area_km2)
  
  area_matrix[i, 1] <- area_class_0_km2
  area_matrix[i, 2] <- area_class_1_km2
  area_matrix[i, 3] <- total_area_km2
}

rownames(area_matrix) <- cenario
colnames(area_matrix) <- c("Area_1", "Area_0", "Area_Total")
print(area_matrix)

write.table(area_matrix, "defesa/area_binario_UC.csv", row.names = TRUE, col.names = TRUE, quote = FALSE, sep="\t")



