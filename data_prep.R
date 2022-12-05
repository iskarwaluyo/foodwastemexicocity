# SET ENVIRONMENT TO WORKING DIRECTORY
setwd("~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/foodwastemexicocity")

# DO NOT RUN... LOAD RDATA AND RUN THE OTHER R FILES
# THIS FILE ONLY CREATES RDATA FILES FROM RAW DATA

# DO NOT RUN... LOAD RDATA AND RUN THE OTHER R FILES
# THIS FILE ONLY CREATES RDATA FILES FROM RAW DATA

library(sf)
# READ BORROUGH AND NEIGHBORHOOD SHAPE FILES AS SIMPLE FEATURES
alcaldias <- st_read("~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/foodwastemexicocity/DATA/gis_data/ALCALDIAS/ALCALDIAS.shp")
colonias <- st_read("~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/foodwastemexicocity/DATA/gis_data/COLONIAS/densidad_vivha/ri_11.shp")

# TRANSFORM BORROUGH NAMES TO UPER AND REMOVE ACCENTS
alcaldias$NOM_MUN <- toupper(alcaldias$NOM_MUN)
alcaldias$NOM_MUN <- gsub("Á", "A", alcaldias$NOM_MUN)
alcaldias$NOM_MUN <- gsub("É", "E", alcaldias$NOM_MUN)
alcaldias$NOM_MUN <- gsub("Í", "I", alcaldias$NOM_MUN)
alcaldias$NOM_MUN <- gsub("Ó", "O", alcaldias$NOM_MUN)
alcaldias$NOM_MUN <- gsub("Ú", "U", alcaldias$NOM_MUN)

# GLOBAL ENVIRONMENT CLEAN UP
rm(alcaldias)
rm(colonias)

# CHANGE LOCATION TO DIRECTORY WHERE DATA IS CONTAINED
data_directory <- "~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/foodwastemexicocity/DATA/raw_data/"
file_name <- paste0(data_directory, "denue_inegi_09_2021.csv", sep = "")

denue_data <- read.csv(file = file_name, header=TRUE, fileEncoding="latin1")

## ADVERTENCIA.
## SE RECOMIENDA PARA R ES QUE SE REQUIERE APROXIMADAMENTE 3 VECES EL TAMAÑO DE LOS ARCHIVOS UTILIZADOS DE RAM
## LOS ARCHIVOS DEL DENUE Y DATOS SHAPE PESAN APROXIMADAMENTE 2 GB
## POR LO CUAL SE RECOMIENDA TENER 8 GB LIBRES DE MEMORIA RAM PARA CORRER EL ALGORITMO

# -------------------- CATEGORIZING -------------------------

  denue_data$CATEGORIA <- ""

  denue_data$CATEGORIA <- ifelse(
    grepl("^4311|^4312", denue_data$codigo_act),
    "COMERCIO AL POR MAYOR", denue_data$CATEGORIA)
  denue_data$CATEGORIA <- ifelse(
    grepl("^4611|^4612|^46211|^462111|^462112", denue_data$codigo_act),
    "COMERCIO AL POR MENOR", denue_data$CATEGORIA)
  denue_data$CATEGORIA <- ifelse(
    grepl("^6111|^6112|^6113|^6114|^6115|^6116|^6117", denue_data$codigo_act),
    "SERVICIOS EDUCATIVOS", denue_data$CATEGORIA)
  denue_data$CATEGORIA <- ifelse(
    grepl("^713", denue_data$codigo_act),
    "ESPARCIMIENTO", denue_data$CATEGORIA)
  denue_data$CATEGORIA <- ifelse(
    grepl("^721", denue_data$codigo_act),
    "ALOJAMIENTO", denue_data$CATEGORIA)
  denue_data$CATEGORIA <- ifelse(
    grepl("^722", denue_data$codigo_act),
    "ALIMENTOS", denue_data$CATEGORIA)
  denue_data$CATEGORIA <- ifelse(
    denue_data$CATEGORIA == "", "OTRA ACTIVIDAD", denue_data$CATEGORIA)


# -------------------- DATOS 2 FACTOR EMP ---------------

  denue_data$FACTOR_EMP <- ""
  for(i in 1:nrow(denue_data)){
    if(denue_data[i,7] == "0 a 5 personas"){denue_data[i,44] = 1}
    if(denue_data[i,7] == "6 a 10 personas"){denue_data[i,44] = 2}
    if(denue_data[i,7] == "11 a 30 personas"){denue_data[i,44] = 3}
    if(denue_data[i,7] == "31 a 50 personas"){denue_data[i,44] = 4}
    if(denue_data[i,7] == "51 a 100 personas"){denue_data[i,44] = 5}
    if(denue_data[i,7] == "101 a 250 personas"){denue_data[i,44] = 6}
    if(denue_data[i,7] == "251 y más personas"){denue_data[i,44] = 7}
  }

  denue_data$FACTOR_EMP <- as.numeric(denue_data$FACTOR_EMP)

  # --- FACTOR CLI ---

    denue_data$FACTOR_CLI <- ""
  
    for(i in 1:nrow(denue_data)){
      if(denue_data[i,43] == "ALIMENTOS"){denue_data[i,45] = 2}
      if(denue_data[i,43] == "ALOJAMIENTO"){denue_data[i,45] = 3}
      if(denue_data[i,43] == "COMERCIO AL POR MAYOR"){denue_data[i,45] = 4}
      if(denue_data[i,43] == "COMERCIO AL POR MENOR"){denue_data[i,45] = 4}
      if(denue_data[i,43] == "ESPARCIMIENTO"){denue_data[i,45] = 4}
      if(denue_data[i,43] == "SERVICIOS EDUCATIVOS"){denue_data[i,45] = 2}
      if(denue_data[i,43] == "OTRA ACTIVIDAD"){denue_data[i,45] = 0}
    }

  # ----
  
    denue_data$FACTOR_TGE <- ""
  
    for(i in 1:nrow(denue_data)){
      if(denue_data[i,43] == "ALIMENTOS"){denue_data[i,46] = 2}
      if(denue_data[i,43] == "ALOJAMIENTO"){denue_data[i,46] = 3}
      if(denue_data[i,43] == "COMERCIO AL POR MAYOR"){denue_data[i,46] = 4}
      if(denue_data[i,43] == "COMERCIO AL POR MENOR"){denue_data[i,46] = 4}
      if(denue_data[i,43] == "ESPARCIMIENTO"){denue_data[i,46] = 4}
      if(denue_data[i,43] == "SERVICIOS EDUCATIVOS"){denue_data[i,46] = 1}
      if(denue_data[i,43] == "OTRA ACTIVIDAD"){denue_data[i,46] = 0}
    }
  
    denue_data$FACTOR_SIT <- ""

    for(i in 1:nrow(denue_data)){
      if(denue_data[i,43] == "ALIMENTOS"){denue_data[i,47] = 3}
      if(denue_data[i,43] == "ALOJAMIENTO"){denue_data[i,47] = 3}
      if(denue_data[i,43] == "COMERCIO AL POR MAYOR"){denue_data[i,47] = 1}
      if(denue_data[i,43] == "COMERCIO AL POR MENOR"){denue_data[i,47] = 2}
      if(denue_data[i,43] == "ESPARCIMIENTO"){denue_data[i,47] = 2}
      if(denue_data[i,43] == "SERVICIOS EDUCATIVOS"){denue_data[i,47] = 3}
      if(denue_data[i,43] == "OTRA ACTIVIDAD"){denue_data[i,47] = 0}
    }

   # denue_dataB <- denue_data # BACKUP FOR TESTING
    denue_data$FWI_score <- as.numeric(denue_data$FACTOR_EMP) * as.numeric(denue_data$FACTOR_SIT)
    
    # SAVE R DATA FROM GLOBAL ENVIRONMENT FOR FASTER LOADING TIMES IN FUTURE
    setwd("~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/foodwastemexicocity/DATA/RData")
    save(alcaldias, colonias, file = "spatial_data.RData")
    save(denue_data, file = "denue_data.RData")
    setwd("~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/foodwastemexicocity")
    
    # GLOBAL ENVIRONMENT CLEAN UP
    rm(denue_data, data_directory, file_name, i)

    
    
