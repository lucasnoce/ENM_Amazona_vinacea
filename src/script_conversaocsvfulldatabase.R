library(sf)
library(dplyr)
library(spThin)
library(tidyverse)
library(raster)
library(spatstat)
library(sdm)

# Carrega os dados do CSV (substitua 'caminho/para/seu/arquivo.csv' pelo caminho real)
dados <- read.csv("C:/enm/data/AV_full_database.csv")

# Verifica se há NAs nas coordenadas, longitude e latitude
anyNA_coords <- any(is.na(dados$longitude) | is.na(dados$latitude))

# Remove linhas com NAs nas coordenadas (longitude e latitude)
dados <- dados[complete.cases(dados$longitude, dados$latitude), ]

# Verifica e trata valores problemáticos nas coordenadas
invalid_longitude <- which(!grepl("^-?\\d+\\.?\\d*$", dados$longitude))
invalid_latitude <- which(!grepl("^-?\\d+\\.?\\d*$", dados$latitude))

# Mostra quais linhas possuem problemas de conversão
if (length(invalid_longitude) > 0) {
  cat("Valores problemáticos em longitude:\n")
  print(dados[invalid_longitude, c("longitude", "latitude")])
}

if (length(invalid_latitude) > 0) {
  cat("Valores problemáticos em latitude:\n")
  print(dados[invalid_latitude, c("longitude", "latitude")])
}

# Trata os valores problemáticos, por exemplo, definindo-os como NA
dados$longitude[invalid_longitude] <- NA
dados$latitude[invalid_latitude] <- NA

# Remove linhas com NAs nas coordenadas novamente, se necessário
dados <- dados[complete.cases(dados$longitude, dados$latitude), ]

# Converte coordenadas para numérico
dados <- dados %>%
  mutate(across(c("longitude", "latitude"), as.numeric))

# Exporta os dados limpos para um novo arquivo CSV
write.csv(dados, "C:/enm/data/AV_full_database_cleaned.csv", row.names = TRUE)

# Mensagem de confirmação
cat("Os dados filtrados foram exportados para 'C:/enm/data/AV_full_database_cleaned.csv'\n")