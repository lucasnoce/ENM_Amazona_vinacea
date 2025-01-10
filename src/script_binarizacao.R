# Carregar os pacotes necessários
library(terra)  # Para trabalhar com raster
library(pROC)   # Para calcular métricas como TSS
library(raster)

path_working_directory  <- "C:/Users/dudas/Documents/enm"
setwd(path_working_directory)

path_modelos <- list(
  "defesa/projecoes/Jan-05_ens_pres(1).tif",
  "defesa/projecoes/Jan-05_ens_fut_2021_245(1).tif",
  "defesa/projecoes/Jan-05_ens_fut_2021_585(1).tif",
  "defesa/projecoes/Jan-05_ens_fut_2041_245(1).tif",
  "defesa/projecoes/Jan-05_ens_fut_2041_585(1).tif",
  "defesa/projecoes/Jan-05_ens_fut_2061_245(1).tif",
  "defesa/projecoes/Jan-05_ens_fut_2061_585(1).tif",
  "defesa/projecoes/Jan-05_ens_fut_2081_245(1).tif",
  "defesa/projecoes/Jan-05_ens_fut_2081_585(1).tif"
)

MA <- read_sf(dsn = "temp/polygonma.shp")
MA_s2000 <- st_transform(MA, crs = projecao_crs)

# Carregar o raster contínuo (modelo de adequabilidade)
adequabilidade_raster <- rast("projecoes/Nov-20_ens_p_1_output.tif")

# Carregar os pontos de presença
presenca <- read.csv("data/download_ocorrencias/Av_espacializado_MA.csv")  # CSV com colunas de longitude e latitude

# Converter os pontos de presença em objeto SpatVector
presenca_vect <- vect(presenca, geom = c("longitude", "latitude"), crs = crs(adequabilidade_raster))

# 3. Converter o SpatRaster para RasterLayer (necessário para usar randomPoints)
adequabilidade_raster_raster <- as(adequabilidade_raster, "Raster")

# 4. Carregar os pontos de background
background <- read.csv("C:/Users/dudas/Documents/enm/modelos/pontos_background.csv")  # CSV com colunas de longitude e latitude

# Converter os pontos de background para objeto SpatVector
background_vect <- vect(background, geom = c("longitude", "latitude"), crs = crs(adequabilidade_raster))

# Visualizar os dados de presença e background no mapa
plot(adequabilidade_raster)
points(presenca_vect, col = "red")  # Pontos de presença em vermelho
points(background_vect, col = "blue")  # Pontos de background em azul

# 5. Extrair valores de adequabilidade para pontos de presença e background
valores_presenca <- terra::extract(adequabilidade_raster, presenca_vect)  # Usando terra::extract
valores_background <- terra::extract(adequabilidade_raster, background_vect)  # Usando terra::extract

# 6. Criar um vetor combinando presença (1) e background (0)
observados <- c(rep(1, nrow(valores_presenca)), rep(0, nrow(valores_background)))
preditos <- c(valores_presenca[, 2], valores_background[, 2])  # Os valores de adequabilidade extraídos

# 7. Função para calcular TSS a partir de um threshold (nova função corrigida)
calc_TSS <- function(threshold, observados, preditos) {
  # Binarizar as previsões com base no threshold
  predito_binario <- ifelse(preditos >= threshold, 1, 0)
  
  # Calcular a matriz de confusão
  matriz_confusao <- table(observados, predito_binario)
  
  # Garantir que a matriz tenha o formato 2x2 (presença e ausência)
  if (nrow(matriz_confusao) < 2 || ncol(matriz_confusao) < 2) {
    # Se faltar uma classe, preenche com zeros
    matriz_confusao <- matrix(c(matriz_confusao, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
  }
  
  # Extrair True Positive (TP), False Positive (FP), True Negative (TN), False Negative (FN)
  TP <- matriz_confusao[2, 2]
  FP <- matriz_confusao[1, 2]
  TN <- matriz_confusao[1, 1]
  FN <- matriz_confusao[2, 1]
  
  # Calcular Sensibilidade e Especificidade
  sensibilidade <- TP / (TP + FN)
  especificidade <- TN / (TN + FP)
  
  # Calcular TSS
  TSS <- sensibilidade + especificidade - 1
  return(TSS)
}

# 8. Definir uma série de thresholds (por exemplo, de 0 a 1 com passos de 0.01)
thresholds <- seq(0, 1, by = 0.01)

# 9. Calcular TSS para cada threshold
TSS_valores <- sapply(thresholds, calc_TSS, observados = observados, preditos = preditos)
TSS_valores_limpo <- TSS_valores[!is.nan(TSS_valores)]

# 10. Identificar o threshold que maximiza o TSS
threshold_ideal <- thresholds[which.max(TSS_valores_limpo)]
cat("O threshold que maximiza o TSS é:", threshold_ideal)


area_matrix <- matrix(NA, nrow = 2+length(path_modelos), ncol = 2) # matriz para armazenar os valores das áreas
area_matrix[nrow(area_matrix), ] <- c("threshold:", threshold_ideal)
cenario <- list()

for ( i in seq_along( path_modelos ) ) {
  print(i)
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
  predito_binario_023 <- ifelse(preditos >= threshold_ideal, 1, 0)
  
  # Criar a matriz de confusão
  matriz_confusao_023 <- table(observados, predito_binario_023)
  print(matriz_confusao_023)
  
  
  # Cálculo das áreas:
  
  raster_binario <- raster(raster_binario)
  raster_binario <- crop(raster_binario, extent(MA_s2000))
  raster_binario <- mask(raster_binario, MA_s2000)
  
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

cenario <- list(
  "Presente",
  "2021-2040 ssp245",
  "2021-2040 ssp585",
  "2041-2060 ssp245",
  "2041-2060 ssp585",
  "2061-2080 ssp245",
  "2061-2080 ssp585",
  "2081-2100 ssp245",
  "2081-2100 ssp585",
  "", ""
)

rownames(area_matrix) <- cenario
colnames(area_matrix) <- c("Area_True", "Area_False")
print(area_matrix)

write.table(area_matrix, "defesa/area_adequabilidade_binarizado.csv", row.names = TRUE, col.names = TRUE, quote = FALSE, sep="\t")

