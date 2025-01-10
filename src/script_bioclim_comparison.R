# WorldClim variables side to side

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
library(grid)
library(tidyr)
library(dplyr)
library(data.table)


# 0. Config -----

path_working_directory  <- "C:/Users/dudas/Documents/enm"
setwd(path_working_directory)

projecao_crs <- 4674
br_ext_xmin  <- -59.0
br_ext_xmax  <- -34.0
br_ext_ymin  <- -35.0
br_ext_ymax  <- -2.0

comp_path_ocorrencias        <- "data/download_ocorrencias/oc_combinado_muito_filtrado.csv"
comp_sufixo_bio              <- "wc2.1_30s_bio_"
comp_path_download		       <- "variaveis/wc2.1_30s_bio/"
comp_path_cenarios_originais <- "variaveis/futuros/climate/wc2.1_30s/"
comp_sufixo_bio_fut          <- "wc2.1_30s_bio_"

layers_to_plot <- c(3, 7, 8, 9, 12, 13)

flag_load_occurrences <- FALSE
flag_load_rasters     <- FALSE
flag_plot_maps        <- TRUE
flag_plot_charts      <- FALSE

lista_var_environment <- c(
  "comp_var_bio_pres", "comp_var_bio_fut1", "comp_var_bio_fut2", "comp_var_bio_fut3", "comp_var_bio_fut4", "comp_var_bio_fut5", "comp_var_bio_fut6", "comp_var_bio_fut7", "comp_var_bio_fut8",
  "comp_var_bio_fut1_raster", "comp_var_bio_fut2_raster", "comp_var_bio_fut3_raster", "comp_var_bio_fut4_raster", "comp_var_bio_fut5_raster", "comp_var_bio_fut6_raster", "comp_var_bio_fut7_raster", "comp_var_bio_fut8_raster"
)

env_clear <- function(){
  rm(list = setdiff(ls(), lista_var_environment))
}


# 1. Occurrences -----

if( flag_load_occurrences == TRUE ){
  comp_sp <- read.csv(comp_path_ocorrencias)
  comp_sp_br <- comp_sp %>%
    select(species, lon, lat) %>%
    distinct() %>%
    drop_na()
  
  comp_sp_br_coord <- st_as_sf(
    x = comp_sp_br,
    coords = c("lon", "lat"),
    crs = projecao_crs
  )
  
  comp_sp_MA <- comp_sp_br_coord
  comp_sp_MA_coord <- comp_sp_MA %>%
    select(species, geometry)
  
  comp_poligono <- st_convex_hull(st_union(comp_sp_MA_coord))
  comp_buffer_dist <- units::set_units(2, "degrees")
  comp_buffer_area <- st_buffer(comp_poligono, dist = comp_buffer_dist)
  comp_buffer <- st_transform(comp_buffer_area, crs = projecao_crs)
  comp_buffer <- as(comp_buffer, "Spatial")
}


# 2. Present Raster -----

if( flag_load_rasters == TRUE ){
  comp_var_bio_pres <- raster::stack(
    list.files(
      path = comp_path_download,
      pattern = ".tif",
      full.names = TRUE
    )
  )
  
  comp_var_bio_pres <- subset(comp_var_bio_pres, c(
    paste0(comp_sufixo_bio,"1"), paste0(comp_sufixo_bio,"2"), paste0(comp_sufixo_bio,"3"), paste0(comp_sufixo_bio,"4"),
    paste0(comp_sufixo_bio,"5"), paste0(comp_sufixo_bio,"6"), paste0(comp_sufixo_bio,"7"), paste0(comp_sufixo_bio,"8"),
    paste0(comp_sufixo_bio,"9"), paste0(comp_sufixo_bio,"10"), paste0(comp_sufixo_bio,"11"), paste0(comp_sufixo_bio,"12"),
    paste0(comp_sufixo_bio,"13"), paste0(comp_sufixo_bio,"14"), paste0(comp_sufixo_bio,"15"), paste0(comp_sufixo_bio,"16"),
    paste0(comp_sufixo_bio,"17"), paste0(comp_sufixo_bio,"18"), paste0(comp_sufixo_bio,"19")
  ))
  
  brazil_extent <- extent(br_ext_xmin, br_ext_xmax, br_ext_ymin, br_ext_ymax)
  comp_var_bio_pres <- crop(comp_var_bio_pres, brazil_extent)
  comp_var_bio_pres <- raster::stack(comp_var_bio_pres)
  comp_var_bio_pres <- crop(comp_var_bio_pres, comp_buffer)
  comp_var_bio_pres <- mask(comp_var_bio_pres, comp_buffer)
  
  # comp_altitude <- brick("variaveis/ambdata/altitude_br.tif")
  comp_altitude <- list.files(path = "variaveis/ambdata/", pattern = ".asc$", full.names = TRUE)
  comp_altitude <- lapply(comp_altitude, raster)
  comp_altitude <- raster::stack(comp_altitude)
  comp_altitude <- resample(comp_altitude, comp_var_bio_pres, method = "bilinear")
  comp_altitude <- projectRaster(comp_altitude, crs = projecao_crs)
  comp_altitude <- crop(comp_altitude, brazil_extent)
  comp_altitude <- raster::stack(comp_altitude)
  comp_altitude <- crop(comp_altitude, comp_buffer)
  comp_altitude <- mask(comp_altitude, comp_buffer)
  
  altitude_breaks <- seq(0, 2500, by = 500)
  altitude_num_classes <- length(altitude_breaks) - 1
  altitude_classes <- seq(1, altitude_num_classes, 1)
  # altitude_colors <- rainbow(altitude_num_classes)
  # altitude_colors <- c("#000004", "#50127B", "#B6367A", "#FB8761", "#FCFDBF")
  altitude_colors <- gray.colors(altitude_num_classes, start = 0.75, end = 0, alpha = 0.35)
  
  altitude_reclass_matrix <- cbind(altitude_breaks[-length(altitude_breaks)], altitude_breaks[-1], altitude_classes)
  altitude_reclass_matrix <- rbind(altitude_reclass_matrix, c(-Inf, min(altitude_breaks), 1))
  altitude_reclass_matrix <- rbind(altitude_reclass_matrix, c(max(altitude_breaks), Inf, altitude_num_classes))
  
  comp_altitude_filled <- reclassify(comp_altitude, altitude_reclass_matrix)
}


# 3. Future Rasters -----

if( flag_load_rasters == TRUE ){
  tif_files <- list.files(path = comp_path_cenarios_originais, pattern = ".tif$", full.names = TRUE)
  comp_var_bio_fut_list <- lapply(tif_files, rast)
  
  comp_var_bio_fut1 <- crop(comp_var_bio_fut_list[[1]], brazil_extent)
  comp_var_bio_fut2 <- crop(comp_var_bio_fut_list[[2]], brazil_extent)
  comp_var_bio_fut3 <- crop(comp_var_bio_fut_list[[3]], brazil_extent)
  comp_var_bio_fut4 <- crop(comp_var_bio_fut_list[[4]], brazil_extent)
  comp_var_bio_fut5 <- crop(comp_var_bio_fut_list[[5]], brazil_extent)
  comp_var_bio_fut6 <- crop(comp_var_bio_fut_list[[6]], brazil_extent)
  comp_var_bio_fut7 <- crop(comp_var_bio_fut_list[[7]], brazil_extent)
  comp_var_bio_fut8 <- crop(comp_var_bio_fut_list[[8]], brazil_extent)
  
  func_renomear_var_bio <- function( bio_fut ){
    names_bio <- c()
    for (i in 1:nlyr(bio_fut)) {
      names_bio <- c(names_bio, paste0(comp_sufixo_bio_fut, i))
    }
    return(names_bio)
  }
  
  names(comp_var_bio_fut1) <- func_renomear_var_bio(comp_var_bio_fut1)
  names(comp_var_bio_fut2) <- func_renomear_var_bio(comp_var_bio_fut2)
  names(comp_var_bio_fut3) <- func_renomear_var_bio(comp_var_bio_fut3)
  names(comp_var_bio_fut4) <- func_renomear_var_bio(comp_var_bio_fut4)
  names(comp_var_bio_fut5) <- func_renomear_var_bio(comp_var_bio_fut5)
  names(comp_var_bio_fut6) <- func_renomear_var_bio(comp_var_bio_fut6)
  names(comp_var_bio_fut7) <- func_renomear_var_bio(comp_var_bio_fut7)
  names(comp_var_bio_fut8) <- func_renomear_var_bio(comp_var_bio_fut8)
  
  comp_var_bio_fut1_raster <- raster::stack(comp_var_bio_fut1)
  comp_var_bio_fut2_raster <- raster::stack(comp_var_bio_fut2)
  comp_var_bio_fut3_raster <- raster::stack(comp_var_bio_fut3)
  comp_var_bio_fut4_raster <- raster::stack(comp_var_bio_fut4)
  comp_var_bio_fut5_raster <- raster::stack(comp_var_bio_fut5)
  comp_var_bio_fut6_raster <- raster::stack(comp_var_bio_fut6)
  comp_var_bio_fut7_raster <- raster::stack(comp_var_bio_fut7)
  comp_var_bio_fut8_raster <- raster::stack(comp_var_bio_fut8)
  
  comp_var_bio_fut1_raster <- crop(comp_var_bio_fut1_raster, comp_buffer)
  comp_var_bio_fut2_raster <- crop(comp_var_bio_fut2_raster, comp_buffer)
  comp_var_bio_fut3_raster <- crop(comp_var_bio_fut3_raster, comp_buffer)
  comp_var_bio_fut4_raster <- crop(comp_var_bio_fut4_raster, comp_buffer)
  comp_var_bio_fut5_raster <- crop(comp_var_bio_fut5_raster, comp_buffer)
  comp_var_bio_fut6_raster <- crop(comp_var_bio_fut6_raster, comp_buffer)
  comp_var_bio_fut7_raster <- crop(comp_var_bio_fut7_raster, comp_buffer)
  comp_var_bio_fut8_raster <- crop(comp_var_bio_fut8_raster, comp_buffer)
  
  comp_var_bio_fut1_raster <- mask(comp_var_bio_fut1_raster, comp_buffer)
  comp_var_bio_fut2_raster <- mask(comp_var_bio_fut2_raster, comp_buffer)
  comp_var_bio_fut3_raster <- mask(comp_var_bio_fut3_raster, comp_buffer)
  comp_var_bio_fut4_raster <- mask(comp_var_bio_fut4_raster, comp_buffer)
  comp_var_bio_fut5_raster <- mask(comp_var_bio_fut5_raster, comp_buffer)
  comp_var_bio_fut6_raster <- mask(comp_var_bio_fut6_raster, comp_buffer)
  comp_var_bio_fut7_raster <- mask(comp_var_bio_fut7_raster, comp_buffer)
  comp_var_bio_fut8_raster <- mask(comp_var_bio_fut8_raster, comp_buffer)
}

# 4. Plots -----

color_gradient <- colorRampPalette(c("#2b83ba", "#8ac8a0", "#f1f04e", "#f59053", "#d7191c"))

## 4.1. Plot maps -----

if( flag_plot_maps == TRUE ){
  raster_stacks <- list(
    comp_var_bio_pres,
    comp_var_bio_fut1_raster,
    comp_var_bio_fut3_raster,
    NULL,
    comp_var_bio_fut5_raster,
    comp_var_bio_fut7_raster,
    comp_var_bio_pres,
    comp_var_bio_fut2_raster,
    comp_var_bio_fut4_raster,
    NULL,
    comp_var_bio_fut6_raster,
    comp_var_bio_fut8_raster
  )
  
  map_titles <- list(
    "Presente", "2021-2040 ssp245", "2041-2060 ssp245",
    "",         "2061-2080 ssp245", "2081-2100 ssp245",
    "Presente", "2021-2040 ssp585", "2041-2060 ssp585",
    "",         "2061-2080 ssp585", "2081-2100 ssp585"
  )
  
  plot_titles <- list(
    "BIO 3 - Isotermalidade (BIO2/BIO7) (×100)",
    "BIO 7 - Intervalo Anual de Temperatura (BIO5-BIO6)",
    "BIO 8 - Temperatura Média do Trimestre Mais Úmido",
    "BIO 9 - Temperatura Média do Trimestre Mais Seco",
    "BIO 12 - Precipitação Anual",
    "BIO 13 - Precipitação do Mês Mais Úmido"
  )
  
  plot_title_idx <- 1
  layer_idx<-13
  for (layer_idx in layers_to_plot) {
    par(mfrow = c(4, 3), mar = c(1.5, 3.5, 2, 3.5), oma = c(1, 0, 1, 2))  # bottom, left, top, right
    print(paste0("layer=", layer_idx))
    
    scale_min_list <- list()
    scale_max_list <- list()
    
    for (i in seq_along(raster_stacks)) {
      raster_stack <- raster_stacks[[i]]
      
      if (is.null(raster_stack)) {
        scale_min_list[[i]] <- NA
        scale_max_list[[i]] <- NA
        next
      }
      
      scale_min_list[[i]] <- cellStats(raster_stack[[layer_idx]], stat = "min")
      scale_max_list[[i]] <- cellStats(raster_stack[[layer_idx]], stat = "max")
    }
    
    scale_min_vector <- unlist(scale_min_list, use.names = FALSE)
    scale_max_vector <- unlist(scale_max_list, use.names = FALSE)
    
    scale_min <- min(scale_min_vector, na.rm = TRUE)
    scale_max <- max(scale_max_vector, na.rm = TRUE)
    zlim_range <- c(scale_min, scale_max)
    print(zlim_range)
    
    for (i in seq_along(raster_stacks)) {
      raster_stack <- raster_stacks[[i]]
      
      if (!is.null(raster_stack)) {
        layer <- raster_stack[[layer_idx]]
        if (!is.null(layer)) {
          plot(layer,
               zlim = zlim_range,
               main = map_titles[[i]],
               col = color_gradient(100),
               legend.width = 1.5,
               xaxt = "n", yaxt = "n",
               xlab = "", ylab = "")
          contour(comp_altitude_filled,
                  add = TRUE,
                  col = altitude_colors,
                  lwd = 0.05)
        }
      } else {
        plot.new()
      }
    }
    
    # mtext(
    #   plot_titles[[plot_title_idx]],
    #   side = 3,
    #   outer = TRUE,
    #   line = 0.5,
    #   cex = 1.5,
    #   font = 2
    # )
    # plot_title_idx <- plot_title_idx + 1
  }
}


## 4.2. Plot charts -----

if( flag_plot_charts == TRUE ){
  raster_stacks <- list(
    comp_var_bio_pres,
    comp_var_bio_fut1_raster,
    comp_var_bio_fut3_raster,
    comp_var_bio_fut5_raster,
    comp_var_bio_fut7_raster,
    comp_var_bio_pres,
    comp_var_bio_fut2_raster,
    comp_var_bio_fut4_raster,
    comp_var_bio_fut6_raster,
    comp_var_bio_fut8_raster
  )
  
  map_titles <- list(
    "Presente", "2021-2040 ssp245", "2041-2060 ssp245", "2061-2080 ssp245", "2081-2100 ssp245",
    "Presente", "2021-2040 ssp585", "2041-2060 ssp585", "2061-2080 ssp585", "2081-2100 ssp585"
  )
  
  scenario_count <- 1
  scenario_titles <- list(
    "BIO 3 - Isotermalidade (BIO2/BIO7) (×100)",
    "BIO 7 - Intervalo Anual de Temperatura (BIO5-BIO6)",
    "BIO 8 - Temperatura Média do Trimestre Mais Úmido",
    "BIO 9 - Temperatura Média do Trimestre Mais Seco",
    "BIO 12 - Precipitação Anual",
    "BIO 13 - Precipitação do Mês Mais Úmido"
  )
  
  num_classes <- 10
  plot_title_idx <- 1
  area_matrix_245_long <- NULL
  area_matrix_585_long <- NULL
  
  for (layer_idx in layers_to_plot) {
    print(paste0("layer = ", layer_idx))
    
    scale_min_list <- list()
    scale_max_list <- list()
    
    print("dbg: scale bounds")
    for (i in seq_along(raster_stacks)) {
      raster_stack <- raster_stacks[[i]]
      
      if (is.null(raster_stack)) {
        print("dbg: null raster_stack")
        scale_min_list[[i]] <- NA
        scale_max_list[[i]] <- NA
        next
      }
      
      scale_min_list[[i]] <- cellStats(raster_stack[[layer_idx]], stat = "min")
      scale_max_list[[i]] <- cellStats(raster_stack[[layer_idx]], stat = "max")
    }
    
    scale_min_vector <- unlist(scale_min_list, use.names = FALSE)
    scale_max_vector <- unlist(scale_max_list, use.names = FALSE)
    
    scale_min <- min(scale_min_vector, na.rm = TRUE)
    scale_max <- max(scale_max_vector, na.rm = TRUE)
    
    classes <- seq(1, num_classes, 1)  # Classes for each range
    breaks <- seq(scale_min, scale_max, length.out = (num_classes+1))  # 6 values to create 5 intervals
    
    reclass_matrix <- cbind(breaks[-length(breaks)], breaks[-1], classes)
    reclass_matrix <- rbind(reclass_matrix, c(-Inf, scale_min, 1))  # Assign values <38.0 to class 0
    reclass_matrix <- rbind(reclass_matrix, c(scale_max, Inf, num_classes))   # Assign values >50.0 to class 4
    # print(reclass_matrix)
    
    crs_proj <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
    
    area_matrix <- matrix(NA, nrow = (length(raster_stacks)), ncol = num_classes)
    # area_matrix[2,] <- classes
    
    for (i in seq_along(raster_stacks)) {
      print(paste0("dbg: raster num = ", i))
      raster_stack <- raster_stacks[[i]]
      raster_classified <- reclassify(raster_stack[[layer_idx]], reclass_matrix)
      
      projected_raster <- projectRaster(raster_classified, crs = crs_proj)  # Reproject to UTM
      raster_values <- values(projected_raster)
      
      cell_size_m   <- res(projected_raster)[1]
      cell_area_m2  <- cell_size_m^2
      cell_area_km2 <- cell_area_m2 / 1e6
      
      freq_table <- freq(projected_raster)
      
      for ( j in seq(num_classes) ){
        if( is.na(freq_table[j,1]) ){
          area_table <- 0
        }else{
          area_table <- as.numeric( freq_table[j,2] * cell_area_km2 )
        }
        area_matrix[i, j] <- area_table
      }
    }
    
    rownames(area_matrix) <- c(map_titles)
    print(area_matrix)
    
    num_rows <- nrow(area_matrix)
    num_cols <- ncol(area_matrix)
    half_rows <- ceiling((num_rows) / 2)
    
    area_matrix_245 <- matrix(NA, nrow = (half_rows + 2), ncol = num_cols)
    area_matrix_585 <- matrix(NA, nrow = (half_rows + 2), ncol = num_cols)
    
    area_matrix_245[2,] <- classes
    area_matrix_585[2,] <- classes
    
    area_matrix_245[3:7, ] <- area_matrix[1:5, ]
    area_matrix_585[3:7, ] <- area_matrix[6:num_rows, ]
    
    rownames(area_matrix_245) <- c(scenario_titles[[scenario_count]], "Time Period", map_titles[1:5])
    rownames(area_matrix_585) <- c(scenario_titles[[scenario_count]], "Time Period", map_titles[6:10])
    scenario_count <- scenario_count + 1
    
    area_matrix_245_long <- rbind(area_matrix_245_long, area_matrix_245)
    area_matrix_585_long <- rbind(area_matrix_585_long, area_matrix_585)
    
    nrow_245  <- nrow(area_matrix_245_long)
    ncol_245  <- ncol(area_matrix_245_long)
    empty_row <- matrix(NA, nrow = 1, ncol = ncol_245)
    
    area_matrix_245_long <- rbind(area_matrix_245_long, empty_row)
    area_matrix_585_long <- rbind(area_matrix_585_long, empty_row)
  }
  
  # Replace NA values in empty rows by ""
  area_matrix_245_long[apply(is.na(area_matrix_245_long), 1, all), ] <- ""
  area_matrix_585_long[apply(is.na(area_matrix_585_long), 1, all), ] <- ""
  
  area_matrix_245_csv_name <- "variaveis/bio_clim_comparison/comparison_245.csv"
  area_matrix_585_csv_name <- "variaveis/bio_clim_comparison/comparison_585.csv"
  
  write.table(area_matrix_245_long, area_matrix_245_csv_name, row.names = TRUE, col.names = FALSE, quote = FALSE, sep="\t")
  write.table(area_matrix_585_long, area_matrix_585_csv_name, row.names = TRUE, col.names = FALSE, quote = FALSE, sep="\t")
}



