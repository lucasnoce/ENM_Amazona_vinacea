library(sf)
library(dplyr)
library(spThin)
library(tidyverse)
library(raster)
library(spatstat)
library(sdm)

# 1. Dados de ocorrência, paisagem e ambientais-----

## 1.1. Ocorrências -----

# Carrega os dados do CSV  FILTRADOS ANTERIORMENTE (substitua 'caminho/para/seu/arquivo.csv' pelo caminho real)
sp <- read.csv("C:/enm/data/AV_full_database_cleaned.csv")

### 1.1.2. Tratamento dos dados -----
#
# Descr.: Pega os dados baixados ou carregados do csv ou xlsx, filtra e limpa
# esses dados.
#

# Carrega a geometria da Mantiqueira (substitua 'caminho/para/seu/arquivo.shp' pelo caminho real)

# shapefile da Mantiqueira
mantiq <- read_sf(dsn="C:/Trabalhos_QGIS/Mantiqueira/Mantiqueira_Completa/mant_completa_SIRGAS2000.shp")

# Filtra as ocorrências dentro da Mantiqueira
sp_mantiq <- st_as_sf(sp, coords = c("longitude", "latitude"), crs = st_crs(mantiq)) %>%
  st_intersection(mantiq)

# Extrai as coordenadas para colunas separadas
coords <- st_coordinates(sp_mantiq)

# Converte de volta para data frame, adiciona as colunas de coordenadas separadas
# e remove a coluna de coordenadas juntas 'POINT(lon,lat)'
sp_mantiq <- cbind(sp_mantiq, coords)
sp_mantiq <- st_set_geometry(sp_mantiq, NULL)

# renomeia as colunas
colnames(sp_mantiq) <- c("species", "lon", "lat")

# seleciona só as ocorrências dentro da mantiqueira
sp_mantiq <- st_intersection(mantiq)  # gera um warning, mas tudo bem, é normal

# extrai as coordenadas para colunas separadas
coords <- st_coordinates(sp_mantiq)

# converte de volta para data frame, adiciona as colunas de coordenadas separadas
# e remove a coluna de coordenadas juntas 'POINT(lon,lat)'
sp_mantiq <- cbind(sp_mantiq, coords)
sp_mantiq <- st_set_geometry(sp_mantiq, NULL)

# seleciona só as colunas de espécie, long e lat
sp_mantiq <- sp_mantiq %>%
  select(species, X, Y)

# renomeia as colunas
colnames(sp_mantiq) <- c("species", "lon", "lat")

### 1.1.3. Espacialização geográfica -----
#
# Descr.: Pega os dados baixados e limpos (sp_br), faz 'reps' cópias e calcula o
# número de ocorrências espaçadas de pelo menos 'thin.par' km para cada cópia.
# Deve-se escolher o maior número de ocorrências (indicado na resposta do
# Console) OU o número de ocorrências que tapareceu no número de réplicas.
#
sp_thin <- thin(
  loc.data = sp_mantiq,
  lat.col = "lat",
  long.col = "lon",
  spec.col = "species",
  thin.par = 10,                    # espacialização em km
  reps = 500,                       # número de réplicas do algoritmo de seleção
  locs.thinned.list.return = TRUE,
  write.files = FALSE,
  write.log.file = FALSE
)
# selecionar        ▼
sp_thin <- sp_thin[[1]]                          # seleciona o conjunto de dados desejado (vide explicação acima)
colnames(sp_thin) <- c("longitude", "latitude")  # renomeia as colunas para lower case


# exporta os dados espacializados para um arquivo csv. É importante exportar
# para conseguir fazer o plot dos mapas de presente e futuro depois.
write.csv(sp_thin, "C:/enm/data/AV_full_database_espacializado.csv", row.names = FALSE)



## 1.2. Variáveis de paisagem -----
#
# Descr.: Seleciona as variáveis de paisagem.
#
# Nota: Variáveis baixadas do AMBDATA.
#

### 1.2.1. AMBDATA -----
#
# Descr.: Seleciona as variáveis do AMBDATA.
#

# lista os arquivos .asc da pasta variaveis/paisagem
asc_files <- list.files(path = "variaveis/ambdata/", pattern = ".asc$", full.names = TRUE)


# pega os noems dos arquivos
var_ambdata_names <- basename(asc_files)
var_ambdata_names <- tools::file_path_sans_ext(var_ambdata_names)

# carrega os arquivos para uma lista de rasters
var_ambdata_list <- lapply(asc_files, raster)

# Combina os arquivos em um RasterStack
var_ambdata <- stack(var_ambdata_list)

# plota cada uma individualmente para verificacao
plot(var_ambdata)

### 1.2.1.1 IBGE -----
#
# Descr.: Seleciona a variáveL de vegetação do IBGE
## carrega o shapefile da variável adicional
var_shapefile <- st_read("C:/enm/variaveis/IBGE")

# definir a extensão e resolução para o novo raster, aqui usando var_ambdata como referência
ext <- extent(var_ambdata)
res <- res(var_ambdata)

# cria um raster vazio com a mesma extensão e resolução
r <- raster(ext, ncol = ncol(var_ambdata), nrow = nrow(var_ambdata))

# converte o shapefile para raster, usando a coluna de interesse para valores
var_shapefile_raster <- rasterize(var_shapefile, r, field = "ar_poli_km")

### 1.2.2. LandCover -----
#
# Descr.: Seleciona as variáveis do LandCover
#

# carrega as variáveis já baixadas do caminho indicado como um RasterStack
var_landcover <- raster::stack(
  list.files(
    path = "variaveis/landcover/",
    pattern = ".tif",
    full.names = TRUE
  )
)

# reoraniza as variáveis em ordem crescente se elas foram carregadas do PC e não baixadas
sufixo_landcover <- "consensus_full_class_"
var_landcover <- subset(var_landcover, c(
  paste0(sufixo_landcover,"2"), paste0(sufixo_landcover,"3"),
  paste0(sufixo_landcover,"9")
))



## 1.3. Variáveis ambientais -----
#
# Descr.: Seleciona as variáveis ambientais.
#
# Nota 1: Ajuda do R com função: no Console, digitar "?'nome_pacote'::'nome_função'".
# Nota 2: Pode ser o 'worldclim_country' pra pegar variáveis só do país.
#

# baixa as variáveis do WorldClim
#var_bio <- geodata::worldclim_global(
#  var = "bio",         # tipo de variável para baixar
#  res = 0.5,           # quantos minutos de resolução (minutos de grau coordenadas)
#  path = "variaveis/"  # caminho para armazenar as variáveis
#)

# carrega as variáveis já baixadas do caminho indicado como um RasterStack
var_bio <- raster::stack(
  list.files(
    path = "variaveis/wc2.1_30s_bio/",
    pattern = ".tif",
    full.names = TRUE
  )
)

# reoraniza as variáveis em ordem crescente se elas foram carregadas do PC e não baixadas
sufixo_bio <- "wc2.1_30s_bio_"
var_bio <- subset(var_bio, c(
  paste0(sufixo_bio,"1"), paste0(sufixo_bio,"2"), paste0(sufixo_bio,"3"), paste0(sufixo_bio,"4"),
  paste0(sufixo_bio,"5"), paste0(sufixo_bio,"6"), paste0(sufixo_bio,"7"), paste0(sufixo_bio,"8"),
  paste0(sufixo_bio,"9"), paste0(sufixo_bio,"10"), paste0(sufixo_bio,"11"), paste0(sufixo_bio,"12"),
  paste0(sufixo_bio,"13"), paste0(sufixo_bio,"14"), paste0(sufixo_bio,"15"), paste0(sufixo_bio,"16"),
  paste0(sufixo_bio,"17"), paste0(sufixo_bio,"18"), paste0(sufixo_bio,"19")
))



## 1.4. Junção das variáveis -----
#
# Descr.: Junta as variáveis carregadas em um só objeto.
#

### 1.4.1. Junção e recorte para a Mantiqueira -----
#
# Descr.: Recorta as variáveis carregadas para a área da Mantiqueira.
#

# carrega o shapefile da Mantiqueira
mantiq <- read_sf(dsn="C:/Trabalhos_QGIS/Mantiqueira/Mantiqueira_Completa/mant_completa_SIRGAS2000.shp")

# define um quadrado com a Mantiqueira dentro para cortar as variáveis
brazil_extent <- extent(-55.0, -34.0, -30.0, -15.0)  # xmin, xmax, ymin, ymax

# corta as variáveis só na região do quadrado (diminui o tamanho e tempo de processamento)
var_ambdata_qd <- crop(var_ambdata, brazil_extent)
var_landcover_qd <- crop(var_landcover, brazil_extent)
var_bio_qd <- crop(var_bio, brazil_extent)

# faz um resample para que os dois grupos de variáveis fiquem exatamente com o
# mesmo tamanho (extent)
var_ambdata_qd <- resample(var_ambdata_qd, var_bio_qd, method='ngb')
var_shapefile_qd <- resample(var_shapefile_qd, var_bio_qd, method = 'ngb')

# agrupa todas as variáveis em um único RasterStack
var_mantiq_qd <- stack(var_ambdata_qd, var_landcover_qd, var_bio_qd, var_shapefile_qd)

# reprojeta todas as variáveis para SIRGAS 2000
var_mantiq_qd <- projectRaster(var_mantiq_qd, crs = crs(mantiq))

# delineamento da área de estudo (Mantiqueira)
var_mantiq <- crop(var_mantiq_qd, mantiq)
var_mantiq <- mask(var_mantiq, mantiq)


### 1.4.2. Armazena os resultados -----
#
# Descr.: Armazena as variáveis calculadas.
#

# salva os nomes das colunas
var_mantiq_layer_names <- names(var_mantiq)
write.csv(names(var_mantiq), "variaveis/variaveis_prontas/var_mantiq_layer_names.csv", row.names = FALSE)

# salva as variáveis
raster::writeRaster(
  var_mantiq,
  filename = "variaveis/variaveis_prontas/var_mantiq",
  format = "GTiff",
  overwrite = TRUE
)


### 1.4.3. Carrega as variáveis -----
#
# Descr.: Carrega as variáveis salvas para economizar tempo.
#

# carrega as variaveis armazenadas
var_mantiq <- brick("variaveis/variaveis_prontas/var_mantiq.tif")

# carrega os nomes das colunas
var_mantiq_layer_names <- read.csv("variaveis/variaveis_prontas/var_mantiq_layer_names.csv", header = FALSE)$V1[-1]

# aplica os nomes das colunas
names(var_mantiq) <- var_mantiq_layer_names


### 1.4.4. Conferir os resultados -----
#
# Descr.: Confere as variáveis juntas.
#

# conferir os resultados
print(names(var_mantiq))  # nomes das variáveis adicionadas

plot(var_mantiq[[5:14]])  # plota o primeiro resultado
plot(mantiq, add=TRUE)    # adiciona o shapefile da Mantiqueira ao plot

# limpa o Environment, mantendo apenas os dados indicados
rm(list = setdiff(ls(), c(
  "var_mantiq", "var_ambdata_qd", "var_landcover_qd", "var_bio_qd",
  "mantiq", "sp_thin", "sp_mantiq",
  "pal", "exclude_pearson")))



## 1.5. Tratamento das variáveis -----
#
# Descr.: Extrai, analisa e filtra os valores ambientais para cada ocorrência.
#

# extrai valores de cada variável para cada ocorrência
vinacea <- raster::extract(var_mantiq, sp_thin)


### 1.5.1. Método VIF -----
#
# Descr.: Análise de multicolinearidade por VIF.
#
# Nota 1: th = valor máximo de correlação entre variáveis (se for acima disso,
#         a variável é descartada)
# Nota 2: se ficar com poucas variáveis (menos de 3), reduzir o valor de th
#
vinacea_vif <- usdm::vifstep(vinacea, th = 5)  # Naimi e Araujo (2016)
vinacea_vif                                    # sumário do VIF

# exclui variáveis correlacionadas
bio_vinacea_vif <- usdm::exclude(var_mantiq, vinacea_vif)

# conferir os plots das variáveis selecionadas
plot(bio_vinacea_vif[[4]])

## 1.6. Bias grid -----
#
# Descr.: Calcula o bias grid, medida de enviesamento das ocorrências.
#

# coordenadas que delimitam o shapefile
mantiq_xy <- extent(mantiq)

# Manter apenas um ponto de cada conjunto duplicado
sp_mantiq_unique <- unique(sp_mantiq)

# Converte ocorrências em pontos spaciais (coordenadas)
pp <- ppp(sp_mantiq$lon,
          sp_mantiq$lat,
          window = owin(
            xrange = c(mantiq_xy[1], mantiq_xy[2]),
            yrange = c(mantiq_xy[3], mantiq_xy[4])))

# Calcula o bias grid suavizado
require(raster)
bias_grid <- density(pp, sigma=0.5)
bias_grid <- raster(bias_grid)

# Corta o bias grid para a Mantiqueira
bias_grid <- raster::crop(bias_grid, mantiq)
bias_grid <- raster::mask(bias_grid, mantiq)

# conferir os resultados
plot(bias_grid)         # plota o bias grid calculado
plot(mantiq, add=TRUE)  # adiciona o shapefile da Mantiqueira ao plot

# limpa o Environment, mantendo apenas os dados indicados
rm(list = setdiff(ls(), c(
  "bio_vinacea_vif", "bias_grid",
  "var_mantiq", "var_ambdata_qd", "var_landcover_qd", "var_bio_qd",
  "mantiq", "sp_thin", "sp_mantiq",
  "pal", "exclude_pearson")))




# **************************************************************************** #
# 2. Validar ocorrências e variáveis -----
#
# Descr.: Ver cada plot das variáveis individualmente para conferir o nicho
# realizável da espécie.
#
# Obs: se os pontos ainda estiverem muito aglomerados, pode voltar e refazer o
# sp_thin aumentando os km (thin.par).
#

# VIF:
plot(bio_vinacea_vif[[4]])        # plota uma variável (pode ir trocando o número para ver cada uma)
points(sp_thin, col = "red")  # adiciona os pontos de ocorrência ao plot da variável

# Pearson:
plot(bio_vinacea_pearson[[1]])        # plota uma variável (pode ir trocando o número para ver cada uma)
points(sp_thin, col = "red")  # adiciona os pontos de ocorrência ao plot da variável





# **************************************************************************** #
# 3. Ajustar e treinar modelos de adequabilidade -----
#
# Descr.: Roda o modelo de adequabilidade a partir dos dados de ocorrência e dos
# dados ambientais.
#

## 3.1. Criar instruções do modelo -----
#
# Descr.: Cria as instruções necessárias para o modelo.
#

bio_vinacea_vif <- raster::stack(bio_vinacea_vif) # transformar objeto de Brick para Stack
bio_vinacea_pearson <- raster::stack(bio_vinacea_pearson) # transformar objeto de Brick para Stack

# Adiciona uma coluna à tabela dos dados de ocorrência
sp_thin <- sp_thin %>%  # cria a coluna de presença da espécie (adiciona uma coluna com valor 1)
  mutate(vinacea = 1)    # nome da coluna = 'vinacea'

# Transforma em uma tabela georreferenciada (pontos espacializados)
coordinates(sp_thin) <- c("longitude", "latitude")

# Cria a instrução para o modelo (VIF)
mdata_vif <- sdmData(
  formula = vinacea ~ .,             # usar a coluna de presença para fazer a modelagem
  train = sp_thin,                   # dados de ocrrência estão no objeto sp_thin
  predictors = bio_vinacea_vif,      # dados ambientais estão no objeto bio_vinacea_vif
  bg = list(                         # 10 mil pontos de background aleatórios ao longo da área de estudo (pode ser um arquivo de viés)
    n = 1000,
    method = "gRandom",
    remove = TRUE,
    bias = bias_grid                 # Adiciona a grade de viés (bias grid)
  )
)

# VIF (cross-validation):
modelo_vif <- sdm(
  formula = vinacea ~ .,    # dados para a modelagem vem da a coluna de presença
  data = mdata_vif,         # instruções da modelagem 
  methods = "Maxent",       # algoritmo a ser usado
  replication = "cv",       # modo de sorteio dos dados pra modelagem (subsamplig)
  cv.folds = 54,            # repetir n vezes (ideal repetir pelo menos 15 vezes)
  reg = 2,
  parallelSettings = list(  # roda em paralelo
    ncore = 2,              # número de núcleos do processador para utilizar na modelagem
    method = "parallel"
  )
)


## 3.3. Sumário e validação estatística -----
modelo_vif      # sumário
modelo_pearson  # sumário

sdm::roc(modelo_vif)      # curva ROC (valores de AUC), ideal seria assim ┌─ (próximo de 1)
sdm::roc(modelo_pearson)  # curva ROC (valores de AUC), ideal seria assim ┌─ (próximo de 1)

getVarImp(modelo_vif)      # mostra a influência/importância de cada variável para o modelo
getVarImp(modelo_pearson)  # mostra a influência/importância de cada variável para o modelo


# **************************************************************************** #
# 4. Projetar mapas de adequabilidade -----
#
# Descr.: Roda o algoritmo de projeção para cada variável selecionada e junta
# os resultados em um único mapa.
#

## 4.1. Projetar por algoritmo (no presente) -----
#
# Descr.: Roda o algoritmo de predição.
#

proj_vinacea <- predict(
  modelo_vif,                               # projetar o modelo calculado
  newdata = bio_vinacea_vif,                # projetar em cima das variáveis ambientais/climáticas atuais
  filename = "projecoes/proj_vinacea.tif",  # arquivo onde serão armazenados os resultados
  overwrite = TRUE,
  parallelSetting = list(                   # roda em paralelo
    ncore = 2,                              # número de núcleos do processador para utilizar na modelagem
    method = "parallel"
  )
)

names(proj_vinacea)  # mostra os nomes das projeções (msm núm que o núm de repetições (n) do modelo)
plot(proj_vinacea[[1:9]])   # plota essas projeções pra conferir


## 4.2. Ensemble -----
#
# Descr.: Junta as 'n' projeções geradas pelo predict do modelo em um único mapa.
#
# Nota: Esse mapa resultante NÃO É um mapa de distribuição geográfica ou de
# distribuição potencial, é apenas o mapa de adequabilidade ambiental (nicho
# realizável da espécie)
#

#bio_vinacea_vif <- rast(bio_vinacea_vif)

ens_vinacea <- ensemble(
  modelo_vif,                 # modelo calculado
  newdata = bio_vinacea_vif,  # variáveis ambientais
  filename = "projecoes/ensemble_vinacea.tif",
  pFilename = "projecoes/ensemble_predict.tif",
  overwrite = TRUE,
  setting = list(
    method = "weighted",  # dá um peso diferente para cada projeção (tem vários outros métodos que podem ser usados)
    stat = "TSS",         # calcula isso com base no TSS
    opt = 2               # seleciona a opção de maximização da especificidade e sensibilidade do modelo (queremos pegar o melhor caso possível)
  )
)

# Checar o resultado do ensemble
plot(ens_vinacea)
points(sp_thin, col = "red")





