# LOAD DATA
 load("~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/foodwastemexicocity/DATA/RData/spatial_data.RData")
 load("~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/foodwastemexicocity/DATA/RData/denue_data.RData")
 load("~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/foodwastemexicocity/DATA/RData/foodscape_data.RData")

# TRANSFORM DATA FRAME TO SPATIAL SIMPLE FEATURES DATAFRAME USING SF PACKAGE

library(sf)
sf_use_s2(FALSE)
denue_data_foodscape <- st_as_sf(denue_data_foodscape, coords = c("longitud", "latitud"), crs = 4326)

# CONFIGURE SPATIAL PROJECTIONS SO ALL DATA MATCHES
library(rgeos)
crs.project <- CRS("+init=epsg:4326")
alcaldias <- st_transform(alcaldias, "EPSG:4326")
colonias <- st_transform(colonias, "EPSG:4326")
denue_data_foodscape <- st_transform(denue_data_foodscape, "EPSG:4326")

# CLEAN UP POLYGONS

alcaldias <- st_make_valid(alcaldias)
colonias <- st_make_valid(colonias)
# THERE WERE ADDITIONAL DATA FROM OTHER STATES, THIS LINE REMOVES IT
alcaldias <- subset(alcaldias, alcaldias$CVE_ENT=="09")
denue_data_foodscape <- st_make_valid(denue_data_foodscape)


# SPLIT BY CATEGORY FOR ANALYSIS BY CATEGORY

denue_data_catsplit <- split(denue_data_foodscape, denue_data_foodscape$CATEGORIA)

split_names <- names(denue_data_catsplit)

for (i in 1:length(denue_data_catsplit))
{
  assign(split_names[i], denue_data_catsplit[[i]])
}


# TEST PLOTS. PLOTS SHOULD OVERLAP IF CORRECTLY CONFIGURED
# plot(alcaldias, col = 2)
# points(denue_data_foodscape)
# plot(colonias)

# OVERLAYS AND MAP ALGEBRA IS EASIER WITH sp package
# sp package REQUIRES ELEMENTS TO BE TRANSFORMED TO SPATIAL FEATURES
# TRANSFORM TO SPATIAL FEATURES

# alcaldias <- as_Spatial(alcaldias)
# colonias <- as_Spatial(colonias)
# denue_data_foodscape <- as_Spatial(denue_data_foodscape)

# TEST PLOTS. PLOTS SHOULD OVERLAP IF CORRECTLY CONFIGURED
# plot(alcaldias)
# plot(colonias, add = T, col = 3)
# plot(denue_data_foodscape, add = T)

# AGGREGATE FEATURES BY INTERSECTION
# BOROUGH ANALYSIS
denue_data_borough_aggregate <- aggregate(denue_data_foodscape[c("FWI_score")], alcaldias, sum)
denue_data_borough_aggregate <- cbind(alcaldias, denue_data_borough_aggregate)
denue_data_borough_aggregate$geometry.1 <- NULL
denue_data_borough_aggregate[is.na(denue_data_borough_aggregate)] = 0

# NEIGHBORHOOD ANALYSIS
denue_data_hood_aggregate <- aggregate(denue_data_foodscape[c("FWI_score")], colonias, sum)
denue_data_hood_aggregate <- cbind(colonias, denue_data_hood_aggregate)
denue_data_hood_aggregate$geometry.1 <- NULL
denue_data_hood_aggregate[is.na(denue_data_hood_aggregate)] = 0


denue_data_hood_aggregate_rest <- aggregate(Restaurants[c("FWI_score")], colonias, sum)
denue_data_hood_aggregate_rest <-  cbind(colonias, denue_data_hood_aggregate_rest)
denue_data_hood_aggregate_rest$geometry.1 <- NULL 
denue_data_hood_aggregate_rest[is.na(denue_data_hood_aggregate_rest)] = 0


denue_data_hood_aggregate_retail <- aggregate(`Retail stores`[c("FWI_score")], colonias, sum)
denue_data_hood_aggregate_retail <- cbind(colonias, denue_data_hood_aggregate_retail)
denue_data_hood_aggregate_retail$geometry.1 <- NULL
denue_data_hood_aggregate_retail[is.na(denue_data_hood_aggregate_retail)] = 0


denue_data_hood_aggregate_wholesale <- aggregate(`Wholesale stores`[c("FWI_score")], colonias, sum)
denue_data_hood_aggregate_wholesale <- cbind(colonias, denue_data_hood_aggregate_wholesale)
denue_data_hood_aggregate_wholesale$geometry.1 <- NULL
denue_data_hood_aggregate_wholesale[is.na(denue_data_hood_aggregate_wholesale)] = 0


denue_data_hood_aggregate_hotel_motel <- aggregate(`Hotels?Motels`[c("FWI_score")], colonias, sum)
denue_data_hood_aggregate_hotel_motel <- cbind(colonias, denue_data_hood_aggregate_hotel_motel)
denue_data_hood_aggregate_hotel_motel$geometry.1 <- NULL
denue_data_hood_aggregate_hotel_motel[is.na(denue_data_hood_aggregate_hotel_motel)] = 0


denue_data_hood_aggregate_education <- aggregate(`Leisure time`[c("FWI_score")], colonias, sum)
denue_data_hood_aggregate_education <-cbind(colonias, denue_data_hood_aggregate_education)
denue_data_hood_aggregate_education$geometry.1 <- NULL
denue_data_hood_aggregate_education[is.na(denue_data_hood_aggregate_education)] = 0


denue_data_hood_aggregate_leisure <- aggregate(`Education services`[c("FWI_score")], colonias, sum)
denue_data_hood_aggregate_leisure <- cbind(colonias, denue_data_hood_aggregate_leisure)
denue_data_hood_aggregate_leisure$geometry.1 <- NULL
denue_data_hood_aggregate_leisure[is.na(denue_data_hood_aggregate_leisure)] = 0


summary(denue_data_hood_aggregate$FWI_score)
summary(denue_data_hood_aggregate_rest$FWI_score)
summary(denue_data_hood_aggregate_retail$FWI_score)
summary(denue_data_hood_aggregate_wholesale$FWI_score)
summary(denue_data_hood_aggregate_hotel_motel$FWI_score)
summary(denue_data_hood_aggregate_education$FWI_score)
summary(denue_data_hood_aggregate_leisure$FWI_score)


# SPATIAL AUTOCORRELATION
library(rgeoda)
# ATTRIBUTES OF WEIGHTS
##queen_w <- queen_weights(denue_data_hood_aggregate)
#summary(queen_w)

#is_symmetric(queen_w)
#has_isolates(queen_w)
#weights_sparsity(queen_w)
#VECINOS DE LA OBSERVACIÓN 1 (idx)
#vecinos <- get_neighbors(queen_w, idx = 1)

# LISA ALL OUTLETS
# FUNCTION USES RGEODA AND R GDAL TO ESTIMATE AND PLOT LISA
# RESTULTS FOR EACH QUERY ARE EXPORTED IN TO A SHAPE FILE

library(rgdal)

lisa_function <- function(simple_features_name, file_name){
  shape_select <- simple_features_name
  file_name <<- file_name
  
  weights <- queen_weights(shape_select, order=1, include_lower_order = FALSE, precision_threshold = 0)
  geometries <- shape_select["FWI_score"]
  lisa <- local_moran(weights, geometries)
  
  lisa_colors <- lisa_colors(lisa)
  etiquetas <- c("Not significant", "High-High", "Low-Low", "Low-High", "High-Low", "Not defined", "Isolated")
  shape_select$lisa_clusters <- lisa_clusters(lisa)
  shape_select$lisa_p <- lisa_pvalues(lisa)
  
  shape_select <<- shape_select
  
  plot(st_geometry(shape_select),
       col=sapply(shape_select$lisa_clusters, function(x){return(lisa_colors[[x+1]])}),
       border = "#333333", lwd=0.2)
  title(main = paste0("Spatial autocorrelation of ", file_name, " Food waste index"))
  legend('bottomleft', legend = etiquetas, fill = lisa_colors, border = "#eeeeee")
}

file_save <- function(file_name){
  folder <- c("~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/foodwastemexicocity/DATA/RESULTS/")
  file_name_save <- paste0(folder, file_name, ".shp", sep="")
  setwd("~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/foodwastemexicocity/DATA/RESULTS")
  st_write(shape_select, file_name_save)
}

# RUNNING CODE WITH DATA

lisa_function(denue_data_hood_aggregate, "all_outlets")
file_save(file_name)

lisa_function(denue_data_hood_aggregate_rest, "restaurants")
file_save(file_name)

# HAS ISSUES WITH HIGH-HIGH (CORRECTED USING RGEODA SOFTWARE FOR NOW)
lisa_function(denue_data_hood_aggregate_retail, "retail_stores")
file_save(file_name)

lisa_function(denue_data_hood_aggregate_wholesale, "wholesale_stores")
file_save(file_name)

# HAS ISSUES WITH HIGH-HIGH (CORRECTED USING RGEODA SOFTWARE FOR NOW)
lisa_function(denue_data_hood_aggregate_education, "education")
file_save(file_name)

lisa_function(denue_data_hood_aggregate_leisure, "leisure")
file_save(file_name)

# HAS ISSUES WITH HIGH-LOW (CORRECTED USING RGEODA SOFTWARE FOR NOW)
lisa_function(denue_data_hood_aggregate_hotel_motel, "hotelmotel")
file_save(file_name)