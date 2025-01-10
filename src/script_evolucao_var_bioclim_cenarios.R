# Carregar os pacotes necessários
library(terra)  # Para trabalhar com raster
library(pROC)   # Para calcular métricas como TSS
library(raster)

path_working_directory  <- "C:/Users/dudas/Documents/enm"
setwd(path_working_directory)

sufixo_bio_pres <- "wc2.1_30s_bio_"
path_bio_pres   <- "variaveis/wc2.1_30s_bio/"

path_modelos <- list(
  "projecoes/Nov-20_ens_p_1_output.tif",
  "projecoes/2021_245/fut_ens_Nov-20(1).tif",
  "projecoes/2021_585/fut_ens_Nov-20(1).tif",
  "projecoes/2041_245/fut_ens_Nov-20(1).tif",
  "projecoes/2041_585/fut_ens_Nov-20(1).tif",
  "projecoes/2061_245/fut_ens_Nov-20(1).tif",
  "projecoes/2061_585/fut_ens_Nov-20(1).tif",
  "projecoes/2081_245/fut_ens_Nov-20(1).tif",
  "projecoes/2081_585/fut_ens_Nov-20(1).tif"
)

adequabilidade_raster <- rast("projecoes/Nov-20_ens_p_1_output.tif")
presenca <- read.csv("data/download_ocorrencias/Av_espacializado_MA.csv")  # CSV com colunas de longitude e latitude

# Presente -----

var_bio_pres <- raster::stack(
  list.files(
    path = path_bio_pres,
    pattern = ".tif",
    full.names = TRUE
  )
)

var_bio_pres <- subset(var_bio_pres, c(
  paste0(sufixo_bio_pres,"1"), paste0(sufixo_bio_pres,"2"), paste0(sufixo_bio_pres,"3"), paste0(sufixo_bio_pres,"4"),
  paste0(sufixo_bio_pres,"5"), paste0(sufixo_bio_pres,"6"), paste0(sufixo_bio_pres,"7"), paste0(sufixo_bio_pres,"8"),
  paste0(sufixo_bio_pres,"9"), paste0(sufixo_bio_pres,"10"), paste0(sufixo_bio_pres,"11"), paste0(sufixo_bio_pres,"12"),
  paste0(sufixo_bio_pres,"13"), paste0(sufixo_bio_pres,"14"), paste0(sufixo_bio_pres,"15"), paste0(sufixo_bio_pres,"16"),
  paste0(sufixo_bio_pres,"17"), paste0(sufixo_bio_pres,"18"), paste0(sufixo_bio_pres,"19")
))

brazil_extent <- extent(br_ext_xmin, br_ext_xmax, br_ext_ymin, br_ext_ymax)

# corta as variáveis só na região do quadrado (diminui o tamanho e tempo de processamento)
var_ambdata_qd <- crop(var_ambdata, brazil_extent)
var_landcover_qd <- crop(var_landcover, brazil_extent)
var_bio_qd <- crop(var_bio, brazil_extent)


area_matrix <- matrix(NA, nrow = length(path_modelos), ncol = 2) # matriz para armazenar os valores das áreas
cenario <- list()

for ( i in seq_along( path_modelos ) ) {
  adequabilidade_raster <- rast(path_modelos[[i]])
  # 11. Aplicar o threshold ao raster contínuo para gerar o mapa binário
  raster_binario <- adequabilidade_raster >= threshold_ideal
  
  cenario[[i]] <- c()
  if( i == 1 ){
    cenario[[i]] <- "presente"
  }else{
    cenario[[i]] <- sub(".*/([^/]+)/.*", "\\1", path_modelos[[i]])
  }
  
  # 12. Visualizar o raster binário
  par(oma = c(0.5, 0.5, 1, 0.5))  # bottom, left, top, right
  plot(raster_binario)
  mtext( cenario[[i]], side = 3, outer = TRUE, line = -1, cex = 1, font = 2 )
  
  # 13. Salvar o mapa binário como arquivo TIFF
  raster_file_name <- paste0("binarios/25nov/", cenario[[i]], ".tiff")
  writeRaster(raster_binario, raster_file_name, overwrite = TRUE)
  
  # Binarizar as previsões com o threshold
  predito_binario_023 <- ifelse(preditos >= 0.45, 1, 0)
  
  # Criar a matriz de confusão
  matriz_confusao_023 <- table(observados, predito_binario_023)
  print(matriz_confusao_023)
  
  
  # Cálculo das áreas:
  
  raster_binario <- raster(raster_binario)
  
  # Check the resolution (area per cell) if in projected CRS
  # If CRS is projected, area_per_cell is resolution_x * resolution_y
  crs_proj <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
  projected_raster <- projectRaster(raster_binario, crs = crs_proj)  # Reproject to UTM
  
  cell_size_m <- res(projected_raster)[1]
  cell_area_m2 <- cell_size_m^2
  cell_area_km2 <- cell_area_m2 / 1e6
  
  area_true_km2 <- cellStats(projected_raster, stat = "sum") * cell_area_km2
  area_false_km2 <- (ncell(projected_raster) - cellStats(projected_raster, stat = "sum")) * cell_area_km2
  
  # Print results
  print(area_true_km2)
  print(area_false_km2)
  
  area_matrix[i, 1] <- area_true_km2
  area_matrix[i, 2] <- area_false_km2
}

colnames(area_matrix) <- c("Area_True", "Area_False")
rownames(area_matrix) <- cenario
print(area_matrix)


