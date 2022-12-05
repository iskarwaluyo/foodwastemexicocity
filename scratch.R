folder <- c("~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/FOODWASTEMEXICOCITY/gis_data/RESULTS/")
categoria <- "denue_data_hood_aggregate"

library(sf)
library(rgeoda)

shape_name <- paste0(folder, categoria, ".shp", sep="")
shape_select <- st_read(denue_data_hood_aggregate)
weights <- queen_weights(denue_data_hood_aggregate)
attribute <- denue_data_hood_aggregate["FWI_score"]
lisa <- local_moran(weights, attribute)

lisa_colors <- lisa_colors(lisa)
etiquetas <- c("Not significant", "High-High", "Low-Low", "Low-High", "High-Low", "Not defined", "Isolated")
denue_data_hood_aggregate$lisa_clusters <- lisa_clusters(lisa)
denue_data_hood_aggregate$lisa_p <- lisa_pvalues(lisa)

plot(st_geometry(denue_data_hood_aggregate),
     col=sapply(denue_data_hood_aggregate$lisa_clusters, function(x){return(lisa_colors[[x+1]])}),
     border = "#333333", lwd=0.2)
title(main = paste0("Spatial autocorrelation of ", categoria, " Food waste index"))
legend('bottomleft', legend = etiquetas, fill = lisa_colors, border = "#eeeeee")