# ------------------------------------------------------------------------ #
# Dissertação de Mestrado: Padrões de ocorrência e lacunas na distribuição #
# do Papagaio-de-peito-roxo (Amazona vinacea) na Serra da Mantiqueira      #
#                                                                          #
# Autora: Maria Eduarda da Silva                                           #
# Data: 02/09/2024                                                         #
# Contato: dudas1lva@outlook.com                                           #
#                                                                          #
# Descrição: este script implementa uma modelagem de Nicho Ecológico do    #
# Papagaio-de-peito-roxo (Amazona vinacea) na região da Serra da           #
# Mantiqueira.                                                             #
#                                                                          #
# Notas:                                                                   #
# - Este script foi baseado no II Workshop CITA do Dr. Gustavo R. Brito    #
#   (reis.brito@unesp.br). Para mais informações, acesse                   #
#   <https://youtu.be/drtVevgIogA?si=RYlzG5VuXYsRguiB>                     #
# ------------------------------------------------------------------------ #

# **************************************************************************** #
# 0. Setup inicial do script -----

## 0.1. Variáveis de controle -----

path_working_directory  <- "C:/Users/lucas/Documents/Projects/enm"
setwd(path_working_directory)

interromper_execucao  <- 5      # <n>: (1, 2, ...) executa até a seção <n> e para antes de <n+1> / FALSE: roda todas as seções em sequência
plots_intermediarios  <- TRUE   # TRUE: faz plots de verificação ao longo do código / FALSE: só plota o mapa final
limpar_environment    <- FALSE  # TRUE: vai limpando o environment ao longo do código / FALSE: nunca limpa o environment
num_cores             <- 8      # número de cores usados nos cálculos
num_obs               <- 0      # ** NÃO ALTERAR ** inicializa a variável que armazenará o número de ocorrencias
projecao_crs          <- 4674
br_ext_xmin <- -59.0
br_ext_xmax <- -34.0
br_ext_ymin <- -35.0
br_ext_ymax <- -2.0

t1_dados_ocorrencia                             <- TRUE   # Dados de Ocorrência
  t1_opt_carregar_ocorrencias_espacializadas   	<- FALSE  # TRUE: carrega as ocorrencias espacializadas / FALSE: filtra e espacializa os dados
  t1_opt_excluir_ocorrencias_fora_da_MA        	<- FALSE  # TRUE: carrega as ocorrencias espacializadas / FALSE: filtra e espacializa os dados
  t1_val_espacializacao                         <- 23     # valor da espacializaçao em km
  t1_path_ocorrencias           			          <- "data/download_ocorrencias/oc_combinado_muito_filtrado.csv"
  t1_path_ocorrencias_espacializadas            <- "data/download_ocorrencias/Av_espacializado_MA.csv"
  t1_path_MA_shp                                <- "temp/polygonma.shp"

t2_variaveis_paisagem   <- TRUE   # Variáveis de Paisagem (AMBDATA e LandCover)
  t2_path_ambdata	    	<- "variaveis/ambdata/"
  t2_path_landcover	  	<- "variaveis/landcover/"
  t2_sufixo_landcover   <- "consensus_full_class_"

t3_variaveis_ambientais       <- TRUE  # Variáveis Ambientais (WorldClim)
  t3_opt_carregar_variaveis 	<- TRUE  # TRUE: carrega as variáveis ambientais / FALSE: baixa as variáveis do WorldClim
  t3_sufixo_bio               <- "wc2.1_30s_bio_"
  t3_path_download		        <- "variaveis/wc2.1_30s_bio/"
  t3_path_carregar		        <- t3_path_download

t4_juncao_variaveis                 <- TRUE   # Junção e Recorte das Variáveis
  t4_opt_carregar_variaveis_prontas	<- FALSE  # TRUE: carrega as variáveis juntas e recortadas / FALSE: junta e recorta as variáveis (crop, mask, resample, etc.)
  t4_val_resample_method	          <- 'bilinear'
  t4_path_variaveis_prontas_layers	<- "variaveis/variaveis_prontas/var_MA_layer_names.csv"
  t4_path_variaveis_prontas_raster	<- "variaveis/variaveis_prontas/var_MA"

t5_selecao_VIF            <- TRUE  # Seleção por método VIF
  t5_val_VIF_threshold	  <- 5     # threshold (th)
  t5_opt_force_variables  <- TRUE  # Força a seleção de uma variável independentemente do resultado do VIF

t6_bias_grid    <- TRUE   # Bias grid
  t6_val_sigma  <- 0.5    # valor de ajuste do bias grid

t7_modelos_adequabilidade     <- TRUE       # Modelos de Adequabilidade
  t7_opt_salvar_modelo_sdm	  	  <- FALSE       # TRUE: salva o modelo como '.RData' / FALSE: não salva o modelo
  t7_val_mdata_background		  <- 10000      # número de pontos de background
  t7_val_mdata_method	  	  	<- "gRandom"  # método de geração dos pontos de background (sdmData)
  t7_val_modelo_methods   		<- "Maxent"   # método(s) de treino do modelo (sdm)
  t7_val_modelo_replication   <- "boot"       # modo de sorteio dos dados para modelagem (Cross-Validation)
  t7_val_modelo_cv_folds      <- num_obs    # número de replicações na calibração do modelo
  t7_val_modelo_reg		        <- 2          # penalidade para modelos de alta complexidade
  t7_path_salvar_modelo       <- "modelos/"
  
t8_projetar_mapas                     <- TRUE        # Projetar mapas de Adequabilidade
  t8_val_ensemble_method            	<- "weighted"  # peso diferente para cada projeção
  t8_val_ensemble_stat          	  	<- "TSS"       # cálculo com base na estatística TSS
  t8_val_ensemble_opt             		<- 2           # maximização da especificidade e sensibilidade do modelo
  t8_opt_salvar_modelo_ens                <- TRUE
  t8_path_projecao	                	<- "projecoes/"
  t8_path_ensemble_presente	          <- "projecoes/"
  t8_path_ensemble_predict_presente   <- "projecoes/"

t9_cenarios_futuros            	  	<- TRUE         # Cenários Futuros
  t9_opt_download_cenarios     	  	<- FALSE        # TRUE: faz download dos cenários do WorldClim / FALSE: carrega os cenários já baixados
  t9_opt_carregar_cenarios_br_ext   <- TRUE         # TRUE: carrega cenários cortados no quadrado do BR e reprojetados / FALSE: carrega cenários originais (mundo inteiro)
  t9_val_download_model		    	  	<- "MIROC6"     # modelo climático (Ortega 2022)
  t9_val_download_ssp	    	   	  	<- c("245","585")          # cenário otimista (126), mediano positivo (245), mediano negativo (370) ou pessimista (585)
  t9_val_download_time  		   	  	<- c("2021-2040","2041-2060","2061-2080","2081-2100")  # período de tempo
  t9_val_download_var	    	   	  	<- "bioc"                  # variável bioclimática
  t9_val_download_res   		   	  	<- 0.5                     # resolução (5 minutos de grau)
  t9_val_ensemble_method  	   	  	<- t8_val_ensemble_method  # peso diferente para cada projeção
  t9_val_ensemble_stat	  	   	  	<- t8_val_ensemble_stat    # cálculo com base na estatística TSS
  t9_val_ensemble_opt		       	  	<- t8_val_ensemble_opt     # maximização da especificidade e sensibilidade do modelo
  t9_path_download		         	  	<- "variaveis/futuros/"
  t9_path_cenarios_originais   	  	<- "variaveis/world_clim/futuro/wc2.1_30s/"
  t9_path_cenarios_cortados   	  	<- "variaveis/world_clim/futuro/wc2.1_30s/brazil_extent/"
  t9_path_ensemble_futuro           <- "projecoes/"
  t9_path_ensemble_predict_futuro   <- "projecoes/"
  t9_sufixo_bio_fut            	  	<- t3_sufixo_bio

t10_mapas_finais        <- TRUE   # Geração dos mapas
  t10_opt_salvar_mapas  <- FALSE
  t10_path_salvar_plot  <- "imagens/"


# Salva o mês, dia e id
current_date <- Sys.Date()
current_day <- format(current_date, "%d")
current_month <- format(current_date, "%b")

read_date_csv <- read.csv("id_contador.csv")
last_row_csv <- tail(read_date_csv, 1)

if ( last_row_csv[1] == current_month ){
  if ( last_row_csv[2] == as.numeric(current_day) ){
    # incrementa +1 a cada vez que roda o script, de forma que os modelos salvos não ficam com o mesmo nome
    id_modelos_salvamento <- last_row_csv[3] + 1
  }else{
    id_modelos_salvamento <- 0
  }
}else{
  id_modelos_salvamento <- 0
}

id_modelos_salvamento <- id_modelos_salvamento[[1]]
log_date_csv <- data.frame(month = current_month, day = current_day, id = id_modelos_salvamento)
write.table(log_date_csv, "id_contador.csv", sep = ",", col.names = !file.exists("id_contador.csv"), 
            row.names = FALSE, append = TRUE)


options(scipen = 999) # remover notação científica dos dados
pal <- c("#5382A0", "#76B9A5", "#E8E1A7", "#DC6D37", "#E02423") # paleta de cores
pal2 <- c("#2b83ba", "#8ac8a0", "#f1f04e", "#f59053", "#d7191c")

if ( limpar_environment == TRUE ){
  lista_var_environment <- c(
    "t1_dados_ocorrencia", "t1_opt_carregar_ocorrencias", "t1_opt_carregar_dados_filtrados", "t1_val_espacializacao", "t1_path_ocorrencias", "t1_path_ocorrencias_espacializadas",
    "t2_variaveis_paisagem", "t2_path_ambdata", "t2_path_landcover", "t2_sufixo_landcover",
    "t3_variaveis_ambientais", "t3_opt_carregar_variaveis", "t3_path_download", "t3_path_carregar", "t3_sufixo_bio",
    "t4_juncao_variaveis", "t4_opt_carregar_variaveis_prontas", "t4_val_resample_method", "t4_path_variaveis_prontas_layers", "t4_path_variaveis_prontas_raster",
    "t5_selecao_VIF", "t5_val_VIF_threshold",
    "t6_bias_grid", "t6_val_sigma",
    "t7_modelos_adequabilidade", "t7_val_mdata_background", "t7_val_mdata_method", "t7_val_modelo_methods", "t7_val_modelo_replication", "t7_val_modelo_reg",
    "t8_projetar_mapas", "t8_val_ensemble_method", "t8_val_ensemble_stat", "t8_val_ensemble_opt", "t8_path_projecao", "t8_path_ensemble_presente", "t8_path_ensemble_predict_presente",
    "t9_cenarios_futuros", "t9_val_download_model", "t9_val_download_ssp", "t9_val_download_time", "t9_val_download_var", "t9_val_download_res", "t9_val_ensemble_method", "t9_val_ensemble_stat", "t9_val_ensemble_opt", "t9_path_download", "t9_sufixo_bio_fut",
    "t10_mapas_finais",
    "interromper_execucao", "plots_intermediarios", "limpar_environment", "num_cores", "num_obs", "projecao_crs", "pal"
  )
}

## 0.2. Pacotes -----

library(ggspatial)
library(raster)
library(sdm)
library(spThin)
library(tidyverse)
library(sf)
library(spatstat)
library(boot)
library(caret)
library(patchwork)
library(corrplot)
library(ggplot2)
library(gridExtra)





# **************************************************************************** #
# 1. Dados de ocorrência -----

if ( t1_dados_ocorrencia == TRUE ){
  ## 1.1. Carregamento das ocorrências e tratamento dos dados -----
  #
  # Descr.: Carrega as ocorrências da espécie (gbif + iNaturalist + Xeno-Canto) e
  # filtra esses dados.
  #
  
  # shapefile da Mata Atlântica
  MA <- read_sf(dsn=t1_path_MA_shp)
  MA_s2000 <- st_transform(MA, crs = projecao_crs)
  
  if ( limpar_environment == TRUE ){
    lista_var_environment <- c(lista_var_environment, "MA_s2000")
  }
  
  if ( t1_opt_carregar_ocorrencias_espacializadas == TRUE ){
    sp_thin <- read.csv(t1_path_ocorrencias_espacializadas)
    if ( limpar_environment == TRUE ){
      lista_var_environment <- c(lista_var_environment, "sp_thin")
    }
  }else{
    ### 1.1.1. Filtragem -----
    
    # carrega os dados
    sp <- read.csv(t1_path_ocorrencias)
    
    # filtragem dos dados de ocorrência
    sp_br <- sp %>%
      select(species, lon, lat) %>%    # seleciona apenas essas 3 colunas
      distinct() %>%                   # remove ocorrências duplicadas
      drop_na()                        # remove espaços vazios
    
    # transforma os dados em um objeto do tipo necessário para as próximas operações
    sp_br_coord <- st_as_sf(
      x = sp_br,
      coords = c("lon", "lat"),
      crs = projecao_crs
    )
    
    # Selecionar as ocorrências que estão dentro dos polígonos da Mata Atlântica
    if ( t1_opt_excluir_ocorrencias_fora_da_MA == TRUE ){
      sp_MA <- st_intersection(sp_br_coord, MA_s2000)  # gera um warning, mas tudo bem, é normal
    }else{
      sp_MA <- sp_br_coord
    }
    
    
    ## 1.1.2. Buffer -----
    
    sp_MA_coord <- sp_MA %>%
      select(species, geometry)
    
    if ( plots_intermediarios == TRUE ){
      plot(st_geometry(MA_s2000))
      points(sp_MA_coord, col="red", cex=0.5)
    }
    
    # Criar o Mínimo Polígono Convexo (MCP)
    poligono <- st_convex_hull(st_union(sp_MA_coord))
    
    # Criar buffer de 2 graus ao redor do MCP
    buffer_dist <- units::set_units(2, "degrees")
    buffer_area <- st_buffer(poligono, dist = buffer_dist)
    
    # Plotar área de estudo com o buffer
    if ( plots_intermediarios == TRUE ){
      plot(st_geometry(buffer_area), col = "lightblue", main = "Área de Estudo com Buffer de 2 Graus")
      plot(st_geometry(poligono), add = TRUE, border = "red")
      plot(st_geometry(sp_MA_coord), add = TRUE, col = "black")
      plot(st_geometry(MA_s2000), add = TRUE)
    }
    
    # Transformar a projeção para EPSG:4674 se necessário (já foi feito)
    buffer <- st_transform(buffer_area, crs = projecao_crs)
    buffer <- as(buffer, "Spatial")
    
    # extrai as coordenadas para colunas separadas
    coords <- st_coordinates(sp_MA)
    
    # converte de volta para data frame, adiciona as colunas de coordenadas separadas
    # e remove a coluna de coordenadas juntas 'POINT(lon,lat)'
    sp_MA <- cbind(sp_MA, coords)
    sp_MA <- st_set_geometry(sp_MA, NULL)
    
    # seleciona só as colunas de espécie, long e lat
    sp_MA <- sp_MA %>%
      select(species, X, Y)
    
    # renomeia as colunas
    colnames(sp_MA) <- c("species", "lon", "lat")
    
    ## 1.1.3. Espacialização geográfica -----
    #
    # Descr.: Pega os dados baixados e limpos (sp_br), faz 'reps' cópias e calcula o
    # número de ocorrências espaçadas de pelo menos 'thin.par' km para cada cópia.
    # Deve-se escolher o maior número de ocorrências (indicado na resposta do
    # Console) OU o número de ocorrências que tapareceu no número de réplicas.
    #
    thin_output <- capture.output(
      sp_thin <- thin(
        loc.data = sp_MA,
        lat.col = "lat",
        long.col = "lon",
        spec.col = "species",
        thin.par = t1_val_espacializacao, # espacialização em km
        reps = 500,                       # número de réplicas do algoritmo de seleção
        locs.thinned.list.return = TRUE,
        write.files = FALSE,
        write.log.file = FALSE
      ),
      split = TRUE
    )
    
    # Pega os valores calculados pelo sp_thin (número de ocorrênicas espacializadas e número de vezes que esse conjunto se repetiu dentre as 'reps' iterações) e encontra a primeira lista de ocorrências com maior número de repetições
    max_records <- thin_output[grep("lat.long.thin.count", thin_output) + 1]
    max_records <- paste(max_records, collapse = " ")
    max_records <- unlist(strsplit(max_records, " "))
    max_records <- Filter(function(x) x != "", max_records)
    max_records <- as.numeric(max_records)
    
    count_records <- thin_output[grep("lat.long.thin.count", thin_output) + 2]
    count_records <- paste(count_records, collapse = " ")
    count_records <- unlist(strsplit(count_records, " "))
    count_records <- Filter(function(x) x != "", count_records)
    count_records <- as.numeric(count_records)
    
    max_count_records <- max(count_records)
    max_count_records_pos <- which.max(count_records)
    
    num_records <- max_records[max_count_records_pos]
    idx_records <- which(sapply(sp_thin, function(x) nrow(x) == num_records))
    idx_records <- idx_records[1]
    
    sp_thin <- sp_thin[[idx_records]]                # seleciona o conjunto de dados desejado (vide explicação acima)
    colnames(sp_thin) <- c("longitude", "latitude")  # renomeia as colunas para lower case
    num_obs <- nrow(sp_thin)
    
    if ( t7_val_modelo_cv_folds == 0 ){
      t7_val_modelo_cv_folds <- num_obs <- num_obs
    }
    
    # exporta os dados espacializados para um arquivo csv. É importante exportar
    # para conseguir fazer o plot dos mapas de presente e futuro depois.
    write.csv(sp_thin, t1_path_ocorrencias_espacializadas, row.names = FALSE)
    
    if ( limpar_environment == TRUE ){
      lista_var_environment <- c(lista_var_environment, "sp", "sp_MA", "buffer", "sp_thin", "num_obs")
    }
  } # t1_opt_carregar_ocorrencias_espacializadas #
  
  if ( limpar_environment == TRUE ){
    rm(list = setdiff(ls(), lista_var_environment))
  }
} # t1_dados_ocorrencia #



if ( interromper_execucao == 1 ){
  stop("Execução interrompida após seção 1")
}



# **************************************************************************** #
# 2. Variáveis de paisagem -----
#
# Descr.: Seleciona as variáveis de paisagem.
#
# Nota: Variáveis baixadas do AMBDATA.
#

if ( t2_variaveis_paisagem == TRUE ){
  ## 2.1. AMBDATA -----
  #
  # Descr.: Seleciona as variáveis do AMBDATA.
  #
  
  # lista os arquivos .asc da pasta variaveis/paisagem
  asc_files <- list.files(path = t2_path_ambdata, pattern = ".asc$", full.names = TRUE)
  
  # pega os nomes dos arquivos
  #var_ambdata_names <- basename(asc_files)
  #var_ambdata_names <- tools::file_path_sans_ext(var_ambdata_names)
  
  # carrega os arquivos para uma lista de rasters
  var_ambdata_list <- lapply(asc_files, raster)
  
  # Combina os arquivos em um RasterStack
  var_ambdata <- stack(var_ambdata_list)
  
  # plota cada uma individualmente para verificacao
  if ( plots_intermediarios == TRUE ){
    plot(var_ambdata)  
  }

  # 2.2. LandCover -----
  #
  # Descr.: Seleciona as variáveis do LandCover
  #
  
  # carrega as variáveis já baixadas do caminho indicado como um RasterStack
  var_landcover <- raster::stack(
    list.files(
      path = t2_path_landcover,
      pattern = ".tif",
      full.names = TRUE
    )
  )
  
  # reoraniza as variáveis em ordem crescente se elas foram carregadas do PC e não baixadas
  var_landcover <- subset(var_landcover, c(
    paste0(t2_sufixo_landcover,"2"), paste0(t2_sufixo_landcover,"3"),
    paste0(t2_sufixo_landcover,"9")
  ))
  
  if ( limpar_environment == TRUE ){
    lista_var_environment <- c(lista_var_environment,
                             "var_ambdata", "var_landcover")
    rm(list = setdiff(ls(), lista_var_environment))
  }
} # t2_variaveis_paisagem #



if ( interromper_execucao == 2 ){
  stop("Execução interrompida após seção 2")
}



# **************************************************************************** #
# 3. Variáveis ambientais -----
#
# Descr.: Seleciona as variáveis ambientais.
#
# Nota 1: Ajuda do R com função: no Console, digitar "?'nome_pacote'::'nome_função'".
# Nota 2: Pode ser o 'worldclim_country' pra pegar variáveis só do país.
#

if ( t3_variaveis_ambientais == TRUE ){
  if ( t3_opt_carregar_variaveis == TRUE ){
    # carrega as variáveis já baixadas do caminho indicado como um RasterStack
    var_bio <- raster::stack(
      list.files(
        path = t3_path_download,
        pattern = ".tif",
        full.names = TRUE
      )
    )
  }else{
    # baixa as variáveis do WorldClim
    var_bio <- geodata::worldclim_global(
      var = "bio",         # tipo de variável para baixar
      res = 0.5,           # quantos minutos de resolução (minutos de grau coordenadas)
      path = t3_path_carregar  # caminho para armazenar as variáveis
    )
  } # t3_opt_carregar_variaveis #
  
  # reoraniza as variáveis em ordem crescente se elas foram carregadas do PC e não baixadas
  var_bio <- subset(var_bio, c(
    paste0(t3_sufixo_bio,"1"), paste0(t3_sufixo_bio,"2"), paste0(t3_sufixo_bio,"3"), paste0(t3_sufixo_bio,"4"),
    paste0(t3_sufixo_bio,"5"), paste0(t3_sufixo_bio,"6"), paste0(t3_sufixo_bio,"7"), paste0(t3_sufixo_bio,"8"),
    paste0(t3_sufixo_bio,"9"), paste0(t3_sufixo_bio,"10"), paste0(t3_sufixo_bio,"11"), paste0(t3_sufixo_bio,"12"),
    paste0(t3_sufixo_bio,"13"), paste0(t3_sufixo_bio,"14"), paste0(t3_sufixo_bio,"15"), paste0(t3_sufixo_bio,"16"),
    paste0(t3_sufixo_bio,"17"), paste0(t3_sufixo_bio,"18"), paste0(t3_sufixo_bio,"19")
  ))
  
  if ( limpar_environment == TRUE ){
    lista_var_environment <- c(lista_var_environment, "var_bio")
    rm(list = setdiff(ls(), lista_var_environment))
  }
} # t3_variaveis_ambientais #



if ( interromper_execucao == 3 ){
  stop("Execução interrompida após seção 3")
}



# **************************************************************************** #
# 4. Junção e Recorte das variáveis -----
#
# Descr.: Junta as variáveis carregadas em um só objeto e recorta para a área
# da Mata Atlântica.
#

if ( t4_juncao_variaveis == TRUE ){
  if ( t4_opt_carregar_variaveis_prontas == TRUE ){
    # carrega as variaveis armazenadas
    var_MA <- brick(paste0(t4_path_variaveis_prontas_raster,".tif"))
    
    # carrega os nomes das colunas
    var_MA_layer_names <- read.csv(t4_path_variaveis_prontas_layers, header = FALSE)$V1[-1]
    
    # aplica os nomes das colunas
    names(var_MA) <- var_MA_layer_names
  }else{
    # define um quadrado com a Mata Atlântica dentro para cortar as variáveis
    brazil_extent <- extent(br_ext_xmin, br_ext_xmax, br_ext_ymin, br_ext_ymax)
    
    # corta as variáveis só na região do quadrado (diminui o tamanho e tempo de processamento)
    var_ambdata_qd <- crop(var_ambdata, brazil_extent)
    var_landcover_qd <- crop(var_landcover, brazil_extent)
    var_bio_qd <- crop(var_bio, brazil_extent)
    
    # faz um resample para que os dois grupos de variáveis fiquem exatamente com o mesmo tamanho (extent)
    var_ambdata_qd <- resample(var_ambdata_qd, var_bio_qd, method = t4_val_resample_method)
    
    # agrupa todas as variáveis em um único RasterStack
    var_MA_qd <- stack(var_ambdata_qd, var_landcover_qd, var_bio_qd)
    
    # reprojeta todas as variáveis para SIRGAS 2000
    var_MA_qd <- projectRaster(var_MA_qd, crs = projecao_crs)
    
    # delineamento da área de estudo (Mantiqueira)
    var_MA <- crop(var_MA_qd, buffer)
    var_MA <- mask(var_MA, buffer)
    
    if ( limpar_environment == TRUE ){
      lista_var_environment <- c(lista_var_environment,
                               "var_ambdata_qd", "var_landcover_qd", "var_bio_qd",
                               "var_MA_qd")
    }
    
    # Armazenamento:
    # salva os nomes das colunas
    var_MA_layer_names <- names(var_MA)
    write.csv(names(var_MA), t4_path_variaveis_prontas_layers, row.names = FALSE)
    
    # salva as variáveis
    raster::writeRaster(
      var_MA,
      filename = t4_path_variaveis_prontas_raster,
      format = "GTiff",
      overwrite = TRUE
    )
  } # t4_opt_carregar_variaveis_prontas #
  
  # conferir os resultados
  print(names(var_MA))  # nomes das variáveis adicionadas
  
  if ( plots_intermediarios == TRUE ){
    plot(var_MA[[1]])  # plota o primeiro resultado
    plot(st_geometry(MA_s2000), add=TRUE)    # adiciona o shapefile da Mantiqueira ao plot
    plot(buffer, add=TRUE)      # adiciona o shape do buffer ao plot
  }
  
  if ( limpar_environment == TRUE ){
    lista_var_environment <- c(lista_var_environment,
                             "var_MA")
    rm(list = setdiff(ls(), lista_var_environment))
  }
} # t4_juncao_variaveis #



if ( interromper_execucao == 4 ){
  stop("Execução interrompida após seção 4")
}



# **************************************************************************** #
# 5. Método VIF -----
#
# Descr.: Análise de multicolinearidade por VIF.
#
# Nota 1: th = valor máximo de correlação entre variáveis (se for acima disso,
#         a variável é descartada)
# Nota 2: se ficar com poucas variáveis (menos de 3), reduzir o valor de th
#

if ( t5_selecao_VIF == TRUE ){
  # extrai valores de cada variável para cada ocorrência
  vinacea <- raster::extract(var_MA, sp_thin)
  
  vinacea_vif <- usdm::vifstep(vinacea, th = t5_val_VIF_threshold)  # Naimi e Araujo (2016)
  
  vinacea_vif                                    # sumário do VIF
  
  # exclui variáveis correlacionadas
  bio_vinacea_vif <- usdm::exclude(var_MA, vinacea_vif)
  
  # conferir os plots das variáveis selecionadas
  if ( plots_intermediarios == TRUE ){
    plot(bio_vinacea_vif[[3]])        # plota uma variável (pode ir trocando o número para ver cada uma)
    points(sp_thin, col = "red", cex = 0.5)  # adiciona os pontos de ocorrência ao plot da variável
  }
  
  if ( limpar_environment == TRUE ){
    lista_var_environment <- c(lista_var_environment,
                             "vinacea_vif", "bio_vinacea_vif")
    rm(list = setdiff(ls(), lista_var_environment))
  }
  
  if ( t5_opt_force_variables == TRUE ){
    correlation_matrix <- cor(vinacea, method = "pearson")
    corrplot(correlation_matrix, method = "circle")
    
    bio_vinacea_vif <- dropLayer(bio_vinacea_vif, "wc2.1_30s_bio_9")
    
    layer_index <- which(names(var_MA) == "altitude_br")
    bio_vinacea_vif <- addLayer(bio_vinacea_vif, var_MA[[layer_index]])
    names(bio_vinacea_vif)
  }
} # t5_selecao_VIF #



if ( interromper_execucao == 5 ){
  stop("Execução interrompida após seção 5")
}



# **************************************************************************** #
# 6. Bias grid -----
#
# Descr.: Calcula o bias grid, medida de enviesamento das ocorrências.
#

if ( t6_bias_grid == TRUE ){
  # coordenadas que delimitam o shapefile
  # MA_xy <- extent(MA_s2000)
  buffer_xy <- extent(buffer)
  
  # Converte ocorrências em pontos espaciais (coordenadas)
  pp <- ppp(sp_MA$lon,
            sp_MA$lat,
            window = owin(
              xrange = c(buffer_xy[1], buffer_xy[2]),
              yrange = c(buffer_xy[3], buffer_xy[4])))
  
  # Calcula o bias grid suavizado
  require(raster)
  bias_grid <- density(pp, sigma = t6_val_sigma)
  bias_grid <- raster(bias_grid)
  
  # Corta o bias grid para a Mantiqueira
  # bias_grid <- raster::crop(bias_grid, MA_s2000)
  # bias_grid <- raster::mask(bias_grid, MA_s2000)
  
  # conferir os resultados
  if ( plots_intermediarios == TRUE ){
    plot(bias_grid)         # plota o bias grid calculado
    # plot(MA_s2000, add=TRUE)  # adiciona o shapefile da Mantiqueira ao plot
  }
  
  
  # se habilitado, limpa o Environment mantendo apenas os dados indicados
  if ( limpar_environment == TRUE ){
    lista_var_environment <- c(lista_var_environment, "bias_grid")
    rm(list = setdiff(ls(), lista_var_environment))
  }
} # t6_bias_grid #



if ( interromper_execucao == 6 ){
  stop("Execução interrompida após seção 6")
}



# **************************************************************************** #
# 7. Modelos de adequabilidade -----
#
# Descr.: Roda o modelo de adequabilidade a partir dos dados de ocorrência e dos
# dados ambientais.
#

if ( t7_modelos_adequabilidade == TRUE ){
  ## 7.1. Instruções do modelo -----
  #
  # Descr.: Cria as instruções necessárias para o modelo.
  #
  
  bio_vinacea_vif <- raster::stack(bio_vinacea_vif) # transformar objeto de Brick para Stack
  
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
      n = t7_val_mdata_background,
      method = t7_val_mdata_method,
      remove = TRUE,
      bias = bias_grid                 # Adiciona a grade de viés (bias grid)
    )
  )
  
  #Acessar os pontos de background:
  bg_points <- mdata_vif@species[[1]]@background
  
  bg_coords <- mdata_vif@info@coords[bg_points, ]  # Obter coordenadas correspondentes
  
  # Criar um dataframe com as coordenadas
  bg_df <- data.frame(
    longitude = bg_coords[, 2],
    latitude = bg_coords[, 3]
  )
  
  # Defina o caminho para salvar os pontos de background
  bg_file_path <- paste0(t7_path_salvar_modelo, current_month, "-", current_day,
                         "_pontos_background(", id_modelos_salvamento, ").csv")
  
  # Salve os pontos de background em um arquivo CSV
  write.csv(bg_df, file = bg_file_path, row.names = FALSE)
  
  # Mensagem de confirmação
  cat("Pontos de background salvos em:", bg_file_path, "\n")
  
  # 7.2. Calibração do modelo -----
  #
  # Descr.: Calibra o modelo com o método cross-validation:
  
  modelo_vif <- sdm(
    formula = vinacea ~ .,    # dados para a modelagem vem da a coluna de presença
    data = mdata_vif,         # instruções da modelagem 
    methods = "Maxent",       # algoritmo a ser usado
    replication = "boot",       # modo de sorteio dos dados pra modelagem (subsamplig)
    n = 100,  # Number of bootstrap iterations
    test.percent = 30,  # 70% training, 30% testing
    maxit = 500,                   # 500 iterações
    # threshold = 1e-5,
    reg = 2,
    parallelSettings = list(  # roda em paralelo
      ncore = num_cores,      # número de núcleos do processador para utilizar na modelagem
      method = "parallel"
    )
  )
  
  # todo:
  # add variables to all options (check if leaving an option empty is the same as not adding it at all)
  # add option to set fixed seed
  
  # 7.3. Resumo e validação estatística -----
  names(bio_vinacea_vif)  # variáveis escolhidas
  modelo_vif              # sumário do modelo
  
  sdm::roc(modelo_vif)                        # curva ROC (valores de AUC), ideal seria assim ┌─ (próximo de 1)
  modelo_vif_varImp <- getVarImp(modelo_vif)  # mostra a influência/importância de cada variável para o modelo
  print(modelo_vif_varImp)
  
  ev <- getEvaluation(modelo_vif, stat = c('AUC', 'COR', 'TSS', 'sensitivity', 'specificity'), opt = 2)
  mean_values <- apply(ev, 2, mean, na.rm = TRUE)
  tss_values <- ev$TSS
  print(mean_values)
  print(tss_values)
  
  response_curve_bio <- getResponseCurve(modelo_vif)
  plot(response_curve_bio)
  title("")
  
  # 7.4. Salvar o modelo treinado -----
  if ( t7_opt_salvar_modelo_sdm == TRUE ){
    path_modelo <- paste0(t7_path_salvar_modelo, current_month, "-", current_day,
                          "_mod_pres(", id_modelos_salvamento, ").RData")
    save(modelo_vif, file = path_modelo)
  }
  
  if ( limpar_environment == TRUE ){
    lista_var_environment <- c(lista_var_environment,
                             "mdata_vif", "modelo_vif",
                             "modelo_vif_roc", "modelo_vif_varImp")
    rm(list = setdiff(ls(), lista_var_environment))
  }
} # t7_modelos_adequabilidade #



if ( interromper_execucao == 7 ){
  stop("Execução interrompida após seção 7")
}



# **************************************************************************** #
# 8. Projetar mapas de adequabilidade -----
#
# Descr.: Roda o algoritmo de projeção para cada variável selecionada e junta
# os resultados em um único mapa.
#
if ( t8_projetar_mapas == TRUE ){
  ## 8.1. Predict Presente -----
  #
  # Descr.: Junta as 'n' projeções geradas pelo predict do modelo em um único mapa.
  #
  # Nota: Esse mapa resultante NÃO É um mapa de distribuição geográfica ou de
  # distribuição potencial, é apenas o mapa de adequabilidade ambiental (nicho
  # realizável da espécie)
  #
  
  path_filename <- paste0(t8_path_ensemble_predict_presente, current_month, "-", current_day,
                          "_rast_pred_pres(", id_modelos_salvamento, ").tif")
  
  proj_vinacea <- predict(
    modelo_vif,                               # projetar o modelo calculado
    newdata = bio_vinacea_vif,                # projetar em cima das variáveis ambientais/climáticas atuais
    filename = path_filename,  # arquivo onde serão armazenados os resultados
    overwrite = TRUE,
    parallelSetting = list(                   # roda em paralelo
      ncore = num_cores,                              # número de núcleos do processador para utilizar na modelagem
      method = "parallel"
    )
  )
  
  if ( plots_intermediarios == TRUE ){
    names(proj_vinacea)  # mostra os nomes das projeções (msm núm que o núm de repetições (n) do modelo)
    plot(proj_vinacea[[1:6]])   # plota essas projeções pra conferir
  }
  
  if ( limpar_environment == TRUE ){
    lista_var_environment <- c(lista_var_environment, "proj_vinacea")
  }

  # 8.2. Ensemble Presente -----
  #
  # Descr.: Junta as 'n' projeções geradas pelo predict do modelo em um único mapa.
  #
  # Nota: Esse mapa resultante NÃO É um mapa de distribuição geográfica ou de
  # distribuição potencial, é apenas o mapa de adequabilidade ambiental (nicho
  # realizável da espécie)
  #
  
  #bio_vinacea_vif <- rast(bio_vinacea_vif)
  
  path_filename <- paste0(t8_path_ensemble_presente, current_month, "-", current_day,
                          "_ens_pres(", id_modelos_salvamento, ").tif")  # Adicionando um sufixo
  path_PFILENAME <- paste0(t8_path_ensemble_predict_presente, current_month, "-", current_day,
                           "_rast_ens_pred(", id_modelos_salvamento, ").tif")  # Adicionando um sufixo
  
  
  if (file.exists(path_filename)) {
    file.remove(path_filename)
  }
  if (file.exists(path_PFILENAME)) {
    file.remove(path_PFILENAME)
  }
  
  require(sdm)
  ens_vinacea <- sdm::ensemble(
    modelo_vif,                 # modelo calculado
    newdata = bio_vinacea_vif,  # variáveis ambientais
    filename = path_filename,
    pFilename = path_PFILENAME,
    overwrite = TRUE,
    setting = list(
      method = t8_val_ensemble_method,  # dá um peso diferente para cada projeção (tem vários outros métodos que podem ser usados)
      stat = t8_val_ensemble_stat,         # calcula isso com base no TSS
      opt = t8_val_ensemble_opt               # seleciona a opção de maximização da especificidade e sensibilidade do modelo (queremos pegar o melhor caso possível)
    )
  )
  
  ens_vinacea_pres_mask <- crop(ens_vinacea, extent(MA_s2000))
  ens_vinacea_pres_mask <- mask(ens_vinacea_pres_mask, MA_s2000)
  
  path_modelo <- paste0(t8_path_projecao, current_month, "-", current_day,
                        "_ens_pres_cut_(", id_modelos_salvamento, ").tif")
  
  if (file.exists(path_modelo)) {
    file.remove(path_modelo)
  }
  
  writeRaster(ens_vinacea_pres_mask, path_modelo, overwrite = TRUE)
  
  # Checar o resultado do ensemble
  if ( plots_intermediarios == TRUE ){
    plot(ens_vinacea)
    points(sp_thin, col = "red", cex = 0.5)
  }
  
  if ( t8_opt_salvar_modelo_ens == TRUE ){
    path_modelo <- paste0(t8_path_ensemble_presente, current_month, "-", current_day,
                          "_ens_pres(", id_modelos_salvamento, ").RData")
    save(ens_vinacea, file = path_modelo)
  }
  
  # 8.3. Curva de Resposta -----
  #
  # Descr.: Gera e plota a curva de resposta do modelo
  #
  
  response_curve_bio <- getResponseCurve(modelo_vif)
  str(response_curve_bio)
  
  # Define as variáveis e suas unidades de medida
  variables_units <- list(
    "altitude_br" = "m",
    "consensus_full_class_2" = "%",
    "wc2.1_30s_bio_3" = "%"
  )
  
  # Função para gerar o gráfico com unidade de medida
  plot_response_curve <- function(variable_name, unit) {
    curve_data <- as.data.frame(response_curve_bio@response[[variable_name]])
    colnames(curve_data)[1] <- "Valor"
    
    ggplot(curve_data, aes(x = Valor, y = `maxent_ID-1`)) + # Ajuste 'maxent_ID-1' conforme necessário
      geom_line(color = "blue") + labs(
        title = paste(variable_name),
        x = paste(variable_name, "(", unit, ")", sep = ""),  # Unidade de medida no eixo x
        y = "Probabilidade de Presença"
      ) +
      theme_minimal() + theme(
        plot.title = element_text(size = 11, face = "bold"),  # Título maior
        axis.title = element_text(size = 8),                  # Títulos dos eixos
        axis.text = element_text(size = 8),                   # Texto dos eixos
        legend.text = element_text(size = 10),                # Texto da legenda (se houver)
        legend.title = element_text(size = 12)                # Título da legenda
      )
  }
  
  # Gera e exibe todos os gráficos para cada variável e unidade
  lapply(names(variables_units), function(var) {
    plot_response_curve(var, variables_units[[var]])
  })
  
  # Gera todos os gráficos
  plots <- lapply(names(variables_units), function(var) {
    plot_response_curve(var, variables_units[[var]])
  })
  
  # Organiza os gráficos lado a lado
  do.call(grid.arrange, c(plots, ncol = 3))  # Ajuste ncol para o número de colunas desejado
  
  
  if ( limpar_environment == TRUE ){
    lista_var_environment <- c(lista_var_environment, "ens_vinacea")
    rm(list = setdiff(ls(), lista_var_environment))
  }
} # t8_projetar_mapas #



if ( interromper_execucao == 8 ){
  stop("Execução interrompida após seção 8")
}



# **************************************************************************** #
# 9. Cenários Futuros -----
#
# Descr.: Baixa as variáveis de cenário futuro, seleciona as mesmas usadas para
# o cenário presente, cria o ensemble para o cenário futuro e compara com o do
# presente.
#

if ( t9_cenarios_futuros == TRUE ){
  ## 9.1. Download do cenário futuro -----
  #
  # Descr.: Faz o download das variáveis ambientais para os cenários futuros escolhidos.
  #
  if ( t9_opt_download_cenarios == TRUE ){
    for (i in t9_val_download_ssp) {
      for (j in t9_val_download_time) {
        bio_fut <- geodata::cmip6_world(  # modelos de circulação global do futuro (último relatório IPCC), cenário SSC
          model = t9_val_download_model,  # modelo climático (Ortega 2022), escolher um bom para a região desejada
          ssp = i,                        # cenário otimista (126), mediano positivo (245), mediano negativo (370) ou pessimista (585), escolher tbm baseado no grau de ameaça da espécie
          time = j,                       # período de tempo
          var = t9_val_download_var,      # variável bioclimática
          res = t9_val_download_res,      # mesma resolução usada no presente (5 minutos de grau)
          path = t9_path_download         # caminho para download
        )
      }
    }
  }else{ # carrega os cenários já baixados
    
    # 9.2. Seleção das variáveis -----
    #
    # Descr.: Seleciona automaticamente as mesmas variáveis usadas para o cenário presente.
    #
    
    if ( t9_opt_carregar_cenarios_br_ext == FALSE ){ # carrega cenários originais (não cortados, do mundo inteiro)
      # Lista todos os arquivos .tif na pasta
      tif_files <- list.files(path = t9_path_cenarios_originais, pattern = ".tif$", full.names = TRUE)
      
      # Carrega esses arquivos para a variavel bio_fut_list
      bio_fut_list <- lapply(tif_files, rast)
      
      # define um quadrado que contém a Mata Atlântica para cortar os cenários (diminui o tamanho dos arquivos)
      brazil_extent <- extent(br_ext_xmin, br_ext_xmax, br_ext_ymin, br_ext_ymax)
      
      func_recortar_bio_fut <- function( bio_list ){
        bio_fut <- crop(bio_list, brazil_extent)
        bio_fut <- raster::stack(bio_fut)
        bio_fut <- projectRaster(bio_fut, crs = projecao_crs)
        return(bio_fut)
      }
      
      bio_fut_1 <- func_recortar_bio_fut(bio_fut_list[[1]])
      bio_fut_2 <- func_recortar_bio_fut(bio_fut_list[[2]])
      bio_fut_3 <- func_recortar_bio_fut(bio_fut_list[[3]])
      bio_fut_4 <- func_recortar_bio_fut(bio_fut_list[[4]])
      bio_fut_5 <- func_recortar_bio_fut(bio_fut_list[[5]])
      bio_fut_6 <- func_recortar_bio_fut(bio_fut_list[[6]])
      bio_fut_7 <- func_recortar_bio_fut(bio_fut_list[[7]])
      bio_fut_8 <- func_recortar_bio_fut(bio_fut_list[[8]])
      
      lista_cenarios_originais <- list.files(
        path = t9_path_cenarios_originais,
        pattern = ".tif$",
        full.names = FALSE
      )
      
      # salva os cenários cortados no quadrado
      path_file_1 <- paste0(t9_path_cenarios_cortados, lista_cenarios_originais[1])
      path_file_2 <- paste0(t9_path_cenarios_cortados, lista_cenarios_originais[2])
      path_file_3 <- paste0(t9_path_cenarios_cortados, lista_cenarios_originais[3])
      path_file_4 <- paste0(t9_path_cenarios_cortados, lista_cenarios_originais[4])
      path_file_5 <- paste0(t9_path_cenarios_cortados, lista_cenarios_originais[5])
      path_file_6 <- paste0(t9_path_cenarios_cortados, lista_cenarios_originais[6])
      path_file_7 <- paste0(t9_path_cenarios_cortados, lista_cenarios_originais[7])
      path_file_8 <- paste0(t9_path_cenarios_cortados, lista_cenarios_originais[8])
      
      writeRaster(bio_fut_1, path_file_1, format="GTiff", overwrite=TRUE)
      writeRaster(bio_fut_2, path_file_2, format="GTiff", overwrite=TRUE)
      writeRaster(bio_fut_3, path_file_3, format="GTiff", overwrite=TRUE)
      writeRaster(bio_fut_4, path_file_4, format="GTiff", overwrite=TRUE)
      writeRaster(bio_fut_5, path_file_5, format="GTiff", overwrite=TRUE)
      writeRaster(bio_fut_6, path_file_6, format="GTiff", overwrite=TRUE)
      writeRaster(bio_fut_7, path_file_7, format="GTiff", overwrite=TRUE)
      writeRaster(bio_fut_8, path_file_8, format="GTiff", overwrite=TRUE)
    }else{ # carrega cenários já cortados (quadrado brazil_extent)
      # Lista todos os arquivos .tif na pasta
      tif_files <- list.files(path = t9_path_cenarios_cortados, pattern = ".tif$", full.names = TRUE)
      
      # Carrega esses arquivos para a variavel bio_fut_list
      bio_fut_list <- lapply(tif_files, rast)
      
      bio_fut_1 <- bio_fut_list[[1]]
      bio_fut_2 <- bio_fut_list[[2]]
      bio_fut_3 <- bio_fut_list[[3]]
      bio_fut_4 <- bio_fut_list[[4]]
      bio_fut_5 <- bio_fut_list[[5]]
      bio_fut_6 <- bio_fut_list[[6]]
      bio_fut_7 <- bio_fut_list[[7]]
      bio_fut_8 <- bio_fut_list[[8]]
      
      func_renomear_var_bio <- function( bio_fut ){
        names_bio <- c()
        for (i in 1:nlyr(bio_fut)) {
          names_bio <- c(names_bio, paste0(t9_sufixo_bio_fut, i))
        }
        return(names_bio)
      }
      
      names(bio_fut_1) <- func_renomear_var_bio(bio_fut_1)
      names(bio_fut_2) <- func_renomear_var_bio(bio_fut_2)
      names(bio_fut_3) <- func_renomear_var_bio(bio_fut_3)
      names(bio_fut_4) <- func_renomear_var_bio(bio_fut_4)
      names(bio_fut_5) <- func_renomear_var_bio(bio_fut_5)
      names(bio_fut_6) <- func_renomear_var_bio(bio_fut_6)
      names(bio_fut_7) <- func_renomear_var_bio(bio_fut_7)
      names(bio_fut_8) <- func_renomear_var_bio(bio_fut_8)
    } # t9_opt_carregar_cenarios_br_ext #
    
    # 9.3. Incluir as outras variáveis -----
    #
    # Descr.: Inclui as outras variáveis (uso e cobertura do solo).
    #
    
    # Identifica automaticamente as mesmas variáveis bioclimáticas do presente (método vif) nos cenários futuros
    brazil_extent <- extent(br_ext_xmin, br_ext_xmax, br_ext_ymin, br_ext_ymax)
    
    if ( t5_opt_force_variables == TRUE ){
      vinacea_vif_names <- names(bio_vinacea_vif)
    }else{
      vinacea_vif_names <- vinacea_vif@results$Variables
    } # t5_opt_force_variables #
    
    var_bio_names_1 <- names(bio_fut_1)
    bio_fut_indexes <- as.vector(na.omit(match(vinacea_vif_names, var_bio_names_1)))
    
    var_bio_fut_vif_1 <- raster::stack(raster::subset(bio_fut_1, bio_fut_indexes))
    var_bio_fut_vif_2 <- raster::stack(raster::subset(bio_fut_2, bio_fut_indexes))
    var_bio_fut_vif_3 <- raster::stack(raster::subset(bio_fut_3, bio_fut_indexes))
    var_bio_fut_vif_4 <- raster::stack(raster::subset(bio_fut_4, bio_fut_indexes))
    var_bio_fut_vif_5 <- raster::stack(raster::subset(bio_fut_5, bio_fut_indexes))
    var_bio_fut_vif_6 <- raster::stack(raster::subset(bio_fut_6, bio_fut_indexes))
    var_bio_fut_vif_7 <- raster::stack(raster::subset(bio_fut_7, bio_fut_indexes))
    var_bio_fut_vif_8 <- raster::stack(raster::subset(bio_fut_8, bio_fut_indexes))
    
    var_ambdata_names <- names(var_ambdata_qd)
    var_ambdata_indexes <- as.vector(na.omit(match(vinacea_vif_names, var_ambdata_names)))
    var_ambdata_fut_vif <- raster::subset(var_ambdata_qd, var_ambdata_indexes)
    var_ambdata_fut_vif <- projectRaster(var_ambdata_fut_vif, crs = projecao_crs)
    
    var_landcover_names <- names(var_landcover_qd)
    var_landcover_indexes <- as.vector(na.omit(match(vinacea_vif_names, var_landcover_names)))
    var_landcover_fut_vif <- raster::subset(var_landcover_qd, var_landcover_indexes)
    var_landcover_fut_vif <- projectRaster(var_landcover_fut_vif, crs = projecao_crs)
    
    func_add_var <- function( var_bio ){
      # var_stack <- resample(var_bio, var_ambdata_fut_vif, method = t4_val_resample_method)
      var_stack <- stack(var_ambdata_fut_vif, var_landcover_fut_vif, var_bio)
      var_stack <- crop(var_stack, buffer)
      var_stack <- mask(var_stack, buffer)
      return(var_stack)
    }
    
    if ( limpar_environment == TRUE ){
      lista_var_environment <- c(lista_var_environment, "bio_fut_list", "bio_fut_num")
    }
    
    var_fut_vinacea_1 <- func_add_var(var_bio_fut_vif_1)
    var_fut_vinacea_2 <- func_add_var(var_bio_fut_vif_2)
    var_fut_vinacea_3 <- func_add_var(var_bio_fut_vif_3)
    var_fut_vinacea_4 <- func_add_var(var_bio_fut_vif_4)
    var_fut_vinacea_5 <- func_add_var(var_bio_fut_vif_5)
    var_fut_vinacea_6 <- func_add_var(var_bio_fut_vif_6)
    var_fut_vinacea_7 <- func_add_var(var_bio_fut_vif_7)
    var_fut_vinacea_8 <- func_add_var(var_bio_fut_vif_8)
  } # t9_opt_download_cenarios #
  
  if ( plots_intermediarios == TRUE ){
    plot(var_fut_vinacea_1) # plotagem do mapa
  }
  
  ### 9.4. Renomear as variáveis -----
  #
  # Descr.: Renomeia automaticamente as variáveis para ficar igual ao cenário presente.
  #
  
  func_renomear_var_fut <- function(){
    names_fut <- c()
    for (i in var_ambdata_indexes) {
      names_fut <- c(names_fut, paste0(names(var_ambdata_qd)[i]))
    }
    for (i in var_landcover_indexes) {
      names_fut <- c(names_fut, paste0(names(var_landcover_qd)[i]))
    }
    for (i in bio_fut_indexes) {
      names_fut <- c(names_fut, paste0(t9_sufixo_bio_fut, i))
    }
    return(names_fut)
  }
  
  names(var_fut_vinacea_1) <- func_renomear_var_fut()
  names(var_fut_vinacea_2) <- func_renomear_var_fut()
  names(var_fut_vinacea_3) <- func_renomear_var_fut()
  names(var_fut_vinacea_4) <- func_renomear_var_fut()
  names(var_fut_vinacea_5) <- func_renomear_var_fut()
  names(var_fut_vinacea_6) <- func_renomear_var_fut()
  names(var_fut_vinacea_7) <- func_renomear_var_fut()
  names(var_fut_vinacea_8) <- func_renomear_var_fut()
  
  var_fut_vinacea_1 <- raster::stack(var_fut_vinacea_1)
  var_fut_vinacea_2 <- raster::stack(var_fut_vinacea_2)
  var_fut_vinacea_3 <- raster::stack(var_fut_vinacea_3)
  var_fut_vinacea_4 <- raster::stack(var_fut_vinacea_4)
  var_fut_vinacea_5 <- raster::stack(var_fut_vinacea_5)
  var_fut_vinacea_6 <- raster::stack(var_fut_vinacea_6)
  var_fut_vinacea_7 <- raster::stack(var_fut_vinacea_7)
  var_fut_vinacea_8 <- raster::stack(var_fut_vinacea_8)
  
  if ( plots_intermediarios == TRUE ){
    plot(var_fut_vinacea_1) # plotagem do mapa
  }
  
  if ( limpar_environment == TRUE ){
    lista_var_environment <- c(lista_var_environment,
                             "var_fut_vinacea_1", "var_fut_vinacea_2", "var_fut_vinacea_3", "var_fut_vinacea_4",
                             "var_fut_vinacea_5", "var_fut_vinacea_6", "var_fut_vinacea_7", "var_fut_vinacea_8")
  }
  
  ## 9.5. Ensemble futuro -----
  #
  # Descr.: Cria o ensemble para o cenário futuro.
  #
  
  # Função que cria um nome de arquivo, será chamada abaixo para cada cenário
  func_path_filename <- function( is_ens, pred, ssp, time_period ){
    # f_name <- paste0(t9_path_ensemble_futuro, current_month, "-", current_day,
    #       "_ens_f", pred, ssp, "_",time_period, "_", id_modelos_salvamento, ".tif")
    if( is_ens ){
      f_name <- paste0(t9_path_ensemble_futuro, time_period, "_", ssp, "/",
                       current_month, "-", current_day, "_ens_fut_", ssp, "_", time_period, pred, 
                       "(", id_modelos_salvamento, ").tif")
    }else{
      f_name <- paste0(t9_path_ensemble_futuro, time_period, "_", ssp, "/",
                       current_month, "-", current_day, "_pred_fut_", ssp, "_", time_period, 
                       "(", id_modelos_salvamento, ").tif")
    }
    if (file.exists(f_name)) {
      file.remove(f_name)
    }
    return(f_name)
  }
  
  # Função que roda um ensemble, será chamada abaixo para cada cenário
  func_ensemble <- function( new_data, ens_file_name, ens_p_file_name, pred_p_file_name ){
    require(sdm)
    proj_vin <- predict(
      modelo_vif,                               # projetar o modelo calculado
      newdata = new_data,                # projetar em cima das variáveis ambientais/climáticas atuais
      filename = pred_p_file_name,  # arquivo onde serão armazenados os resultados
      overwrite = TRUE,
      parallelSetting = list(                   # roda em paralelo
        ncore = num_cores,                              # número de núcleos do processador para utilizar na modelagem
        method = "parallel"
      )
    )
    ens_vin <- sdm::ensemble(
      modelo_vif,
      newdata = proj_vin,
      filename = ens_file_name,
      pFilename = ens_p_file_name,
      overwrite = TRUE,
      setting = list(
        method = t9_val_ensemble_method,
        stat = t9_val_ensemble_stat,
        opt = t9_val_ensemble_opt
      )
    )
    return(ens_vin)
  }
  
  path_filename <- func_path_filename(TRUE, "", "245", "2021")
  path_pFilename <- func_path_filename(TRUE, "_pred", "245", "2021")
  path_pred_pFilename <- func_path_filename(FALSE, "", "245", "2021")
  tryCatch( ens_vinacea_fut_1 <- func_ensemble(var_fut_vinacea_1, path_filename, path_pFilename, path_pred_pFilename) )
  
  path_filename <- func_path_filename(TRUE, "", "245", "2041")
  path_pFilename <- func_path_filename(TRUE, "_pred", "245", "2041")
  path_pred_pFilename <- func_path_filename(FALSE ,"", "245", "2041")
  tryCatch( ens_vinacea_fut_2 <- func_ensemble(var_fut_vinacea_2, path_filename, path_pFilename, path_pred_pFilename) )
  
  path_filename <- func_path_filename(TRUE, "", "245", "2061")
  path_pFilename <- func_path_filename(TRUE, "_pred", "245", "2061")
  path_pred_pFilename <- func_path_filename(FALSE ,"", "245", "2061")
  tryCatch( ens_vinacea_fut_3 <- func_ensemble(var_fut_vinacea_3, path_filename, path_pFilename, path_pred_pFilename) )
  
  path_filename <- func_path_filename(TRUE, "", "245", "2081")
  path_pFilename <- func_path_filename(TRUE, "_pred", "245", "2081")
  path_pred_pFilename <- func_path_filename(FALSE ,"", "245", "2081")
  tryCatch( ens_vinacea_fut_4 <- func_ensemble(var_fut_vinacea_4, path_filename, path_pFilename, path_pred_pFilename) )
  
  path_filename <- func_path_filename(TRUE, "", "585", "2021")
  path_pFilename <- func_path_filename(TRUE, "_pred", "585", "2021")
  path_pred_pFilename <- func_path_filename(FALSE ,"", "585", "2021")
  tryCatch( ens_vinacea_fut_5 <- func_ensemble(var_fut_vinacea_5, path_filename, path_pFilename, path_pred_pFilename) )
  
  path_filename <- func_path_filename(TRUE, "", "585", "2041")
  path_pFilename <- func_path_filename(TRUE, "_pred", "585", "2041")
  path_pred_pFilename <- func_path_filename(FALSE ,"", "585", "2041")
  tryCatch( ens_vinacea_fut_6 <- func_ensemble(var_fut_vinacea_6, path_filename, path_pFilename, path_pred_pFilename) )
  
  path_filename <- func_path_filename(TRUE, "", "585", "2061")
  path_pFilename <- func_path_filename(TRUE, "_pred", "585", "2061")
  path_pred_pFilename <- func_path_filename(FALSE ,"", "585", "2061")
  tryCatch( ens_vinacea_fut_7 <- func_ensemble(var_fut_vinacea_7, path_filename, path_pFilename, path_pred_pFilename) )
  
  path_filename <- func_path_filename(TRUE, "", "585", "2081")
  path_pFilename <- func_path_filename(TRUE, "_pred", "585", "2081")
  path_pred_pFilename <- func_path_filename(FALSE ,"", "585", "2081")
  tryCatch( ens_vinacea_fut_8 <- func_ensemble(var_fut_vinacea_8, path_filename, path_pFilename, path_pred_pFilename) )
  
  func_path_modelo <- function( ssp, time_period, file_extension ){
    f_name <- paste0("defesa/projecoes/", current_month, "-", current_day, "_ens_fut_cut_",
                     time_period, "_", ssp, "(", id_modelos_salvamento, ")", file_extension)
    if (file.exists(f_name)) {
      file.remove(f_name)
    }
    return(f_name)
  }
  
  path_modelo <- func_path_modelo("245", "2021", ".RData")
  save(ens_vinacea_fut_1, file = path_modelo)
  
  path_modelo <- func_path_modelo("245", "2041", ".RData")
  save(ens_vinacea_fut_2, file = path_modelo)
  
  path_modelo <- func_path_modelo("245", "2061", ".RData")
  save(ens_vinacea_fut_3, file = path_modelo)
  
  path_modelo <- func_path_modelo("245", "2081", ".RData")
  save(ens_vinacea_fut_4, file = path_modelo)
  
  path_modelo <- func_path_modelo("585", "2021", ".RData")
  save(ens_vinacea_fut_5, file = path_modelo)
  
  path_modelo <- func_path_modelo("585", "2041", ".RData")
  save(ens_vinacea_fut_6, file = path_modelo)
  
  path_modelo <- func_path_modelo("585", "2061", ".RData")
  save(ens_vinacea_fut_7, file = path_modelo)
  
  path_modelo <- func_path_modelo("585", "2081", ".RData")
  save(ens_vinacea_fut_8, file = path_modelo)
  
  ens_vinacea_fut_1_mask <- crop(ens_vinacea_fut_1, extent(MA_s2000))
  ens_vinacea_fut_1_mask <- mask(ens_vinacea_fut_1_mask, MA_s2000)
  
  ens_vinacea_fut_2_mask <- crop(ens_vinacea_fut_2, extent(MA_s2000))
  ens_vinacea_fut_2_mask <- mask(ens_vinacea_fut_2_mask, MA_s2000)
  
  ens_vinacea_fut_3_mask <- crop(ens_vinacea_fut_3, extent(MA_s2000))
  ens_vinacea_fut_3_mask <- mask(ens_vinacea_fut_3_mask, MA_s2000)
  
  ens_vinacea_fut_4_mask <- crop(ens_vinacea_fut_4, extent(MA_s2000))
  ens_vinacea_fut_4_mask <- mask(ens_vinacea_fut_4_mask, MA_s2000)
  
  ens_vinacea_fut_5_mask <- crop(ens_vinacea_fut_5, extent(MA_s2000))
  ens_vinacea_fut_5_mask <- mask(ens_vinacea_fut_5_mask, MA_s2000)
  
  ens_vinacea_fut_6_mask <- crop(ens_vinacea_fut_6, extent(MA_s2000))
  ens_vinacea_fut_6_mask <- mask(ens_vinacea_fut_6_mask, MA_s2000)
  
  ens_vinacea_fut_7_mask <- crop(ens_vinacea_fut_7, extent(MA_s2000))
  ens_vinacea_fut_7_mask <- mask(ens_vinacea_fut_7_mask, MA_s2000)
  
  ens_vinacea_fut_8_mask <- crop(ens_vinacea_fut_8, extent(MA_s2000))
  ens_vinacea_fut_8_mask <- mask(ens_vinacea_fut_8_mask, MA_s2000)
  
  path_modelo <- func_path_modelo("245", "2021", ".tif")
  writeRaster(ens_vinacea_fut_1_mask, path_modelo, overwrite = TRUE)
  
  path_modelo <- func_path_modelo("245", "2041", ".tif")
  writeRaster(ens_vinacea_fut_2_mask, path_modelo, overwrite = TRUE)
  
  path_modelo <- func_path_modelo("245", "2061", ".tif")
  writeRaster(ens_vinacea_fut_3_mask, path_modelo, overwrite = TRUE)
  
  path_modelo <- func_path_modelo("245", "2081", ".tif")
  writeRaster(ens_vinacea_fut_4_mask, path_modelo, overwrite = TRUE)
  
  path_modelo <- func_path_modelo("585", "2021", ".tif")
  writeRaster(ens_vinacea_fut_5_mask, path_modelo, overwrite = TRUE)
  
  path_modelo <- func_path_modelo("585", "2041", ".tif")
  writeRaster(ens_vinacea_fut_6_mask, path_modelo, overwrite = TRUE)
  
  path_modelo <- func_path_modelo("585", "2061", ".tif")
  writeRaster(ens_vinacea_fut_7_mask, path_modelo, overwrite = TRUE)
  
  path_modelo <- func_path_modelo("585", "2081", ".tif")
  writeRaster(ens_vinacea_fut_8_mask, path_modelo, overwrite = TRUE)
  
  if ( limpar_environment == TRUE ){
    lista_var_environment <- c(lista_var_environment,
                             "ens_vinacea_fut_1", "ens_vinacea_fut_2", "ens_vinacea_fut_3", "ens_vinacea_fut_4",
                             "ens_vinacea_fut_5", "ens_vinacea_fut_6", "ens_vinacea_fut_7", "ens_vinacea_fut_8")
  }
  
  ## 9.6. Comparação dos ensembles (Presente vs Futuro) -----
  #
  # Descr.: Plota os dois ensembles (presente e futuro) para comparação.
  #
  if ( plots_intermediarios == TRUE ){
    par(mfrow = c(1, 2)) # pareia os plots
    
    plot(ens_vinacea)       # ensemble do presente
    plot(ens_vinacea_fut_2) # ensemble de um dos cenários futuros
    
    dev.off() # para de parear os plots
  }
  
  if ( limpar_environment == TRUE ){
    rm(list = setdiff(ls(), lista_var_environment))
  }
} # t9_cenarios_futuros #



if ( interromper_execucao == 9 ){
  stop("Execução interrompida após seção 9")
}



# **************************************************************************** #
# 10. Geração dos mapas -----
#
# Descr.: Plota os mapas finais.
#

sp_plot <- read.csv(t1_path_ocorrencias_espacializadas)  # le os dados pra deixar em um formato que o geom_point entende

func_plotar_graficos <- function( ensemble_v ){
  plot_futuro <- ggplot() +
    geom_raster(data = ensemble_v, aes(x, y, fill = ensemble_weighted)) +
    geom_sf(data = MA_s2000, color = "#464646", fill = NA) +
    geom_point(
      data = sp_plot,
      aes(x = longitude, y = latitude, color = "black"), size = .7
    ) +
    scale_fill_gradientn(
      colours = pal2,
      na.value = NA,
      limits = c(0.00, 1.00)
    ) + # parâmetros da escala de cor
    scale_color_manual(
      values = "black",
      name = NULL,
      labels = expression(italic("Amazona vinacea (Kuhl, 1820)"))
    ) +
    coord_sf() +
    annotation_scale(location = "br", width_hint = 0.5) +
    labs(
      x = "Longitude", # texto do eixo x
      y = "Latitude", # texto do eixo y
      fill = "Adequabilidade\n    Ambiental"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "lightgrey"),
      panel.grid.minor = element_line(color = "lightgrey"),
      legend.title = element_text(face = "bold", size = 12, vjust = 3)
    )
  
  return(plot_futuro)
}

if ( t10_mapas_finais == TRUE ){
  ## 10.1. Ajuste dos tipos de objeto -----
  ens_vinacea_presente <- as.data.frame(ens_vinacea, xy = TRUE)
  
  # 10.2. Plot do Presente -----
  plot_presente <- func_plotar_graficos(ens_vinacea_presente)
  plot_presente # plot do mapa presente
  
  if ( limpar_environment == TRUE ){
    lista_var_environment <- c(lista_var_environment,
                               "ens_vinacea_presente", "plot_presente")
  }
  
  # 10.3. Plot do Futuro -----
  if ( t9_cenarios_futuros == TRUE ){
    ens_vinacea_futuro_1 <- as.data.frame(ens_vinacea_fut_1, xy = TRUE)
    ens_vinacea_futuro_2 <- as.data.frame(ens_vinacea_fut_2, xy = TRUE)
    ens_vinacea_futuro_3 <- as.data.frame(ens_vinacea_fut_3, xy = TRUE)
    ens_vinacea_futuro_4 <- as.data.frame(ens_vinacea_fut_4, xy = TRUE)
    ens_vinacea_futuro_5 <- as.data.frame(ens_vinacea_fut_5, xy = TRUE)
    ens_vinacea_futuro_6 <- as.data.frame(ens_vinacea_fut_6, xy = TRUE)
    ens_vinacea_futuro_7 <- as.data.frame(ens_vinacea_fut_7, xy = TRUE)
    ens_vinacea_futuro_8 <- as.data.frame(ens_vinacea_fut_8, xy = TRUE)
    
    plot_futuro_1 <- func_plotar_graficos(ens_vinacea_futuro_1)
    plot_futuro_2 <- func_plotar_graficos(ens_vinacea_futuro_2)
    plot_futuro_3 <- func_plotar_graficos(ens_vinacea_futuro_3)
    plot_futuro_4 <- func_plotar_graficos(ens_vinacea_futuro_4)
    plot_futuro_5 <- func_plotar_graficos(ens_vinacea_futuro_5)
    plot_futuro_6 <- func_plotar_graficos(ens_vinacea_futuro_6)
    plot_futuro_7 <- func_plotar_graficos(ens_vinacea_futuro_7)
    plot_futuro_8 <- func_plotar_graficos(ens_vinacea_futuro_8)
    
    plot_2021_2040 <- plot_futuro_1 | plot_futuro_5
    plot_2041_2060 <- plot_futuro_2 | plot_futuro_6
    plot_2061_2080 <- plot_futuro_3 | plot_futuro_7
    plot_2081_2100 <- plot_futuro_4 | plot_futuro_8
    
    plot_futuro_1
    plot_futuro_2
    plot_futuro_3
    plot_futuro_4
    plot_futuro_5
    plot_futuro_6
    plot_futuro_7
    plot_futuro_8
    
    if ( limpar_environment == TRUE ){
      lista_var_environment <- c(lista_var_environment, "plot_futuro")
    }
  } # t9_cenarios_futuros #
  
  # 10.4. Exportar imagens -----
  if ( t10_opt_salvar_mapas == TRUE ){
    path_plot <- paste0(t10_path_salvar_plot, current_month, "-", current_day,
                          "_plot_p_", id_modelos_salvamento, ".png")
    ggsave(
      filename = path_plot,
      plot = plot_presente,
      device = "png",
      scale = 2,
      bg = "white"
    )
    
    if ( t9_cenarios_futuros == TRUE ){
      func_salvar_plot_futuro <- function( fut_id, fut_plot ){
        path_plot <- paste0(t10_path_salvar_plot, current_month, "-", current_day,
                            "_plot_f", fut_id, "_", id_modelos_salvamento, ".png")
        ggsave(
          filename = path_plot,
          plot = fut_plot,
          device = "png",
          scale = 2,
          bg = "white"
        )
      }
      
      plot_futuro_ret <- func_salvar_plot_futuro( "1", plot_futuro_1)
      plot_futuro_ret <- func_salvar_plot_futuro( "2", plot_futuro_2)
      plot_futuro_ret <- func_salvar_plot_futuro( "3", plot_futuro_3)
      plot_futuro_ret <- func_salvar_plot_futuro( "4", plot_futuro_4)
      plot_futuro_ret <- func_salvar_plot_futuro( "5", plot_futuro_5)
      plot_futuro_ret <- func_salvar_plot_futuro( "6", plot_futuro_6)
      plot_futuro_ret <- func_salvar_plot_futuro( "7", plot_futuro_7)
      plot_futuro_ret <- func_salvar_plot_futuro( "8", plot_futuro_8)
    } # t9_cenarios_futuros #
  } # t10_opt_salvar_mapas #
  
} # t10_mapas_finais #
