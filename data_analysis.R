# SET ENVIRONMENT TO WORKING DIRECTORY
# LOAD R DATA FROM DATA PREP PROCESSING
 load("~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/FOODWASTEMEXICOCITY/DATA/RData/spatial_data.RData")
 load("~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/FOODWASTEMEXICOCITY/DATA/RData/denue_data.RData")
 
setwd("~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/FOODWASTEMEXICOCITY")

# denue_data <- denue_dataB

# TRANSLATE VALUES TO ENGLISH
denue_data$CATEGORIA <- gsub("ALIMENTOS", "Restaurants", denue_data$CATEGORIA)
denue_data$CATEGORIA <- gsub("ALOJAMIENTO", "Hotels?Motels", denue_data$CATEGORIA)
denue_data$CATEGORIA <- gsub("COMERCIO AL POR MENOR", "Retail stores", denue_data$CATEGORIA)
denue_data$CATEGORIA <- gsub("COMERCIO AL POR MAYOR", "Wholesale stores", denue_data$CATEGORIA)
denue_data$CATEGORIA <- gsub("ESPARCIMIENTO", "Leisure time", denue_data$CATEGORIA)
denue_data$CATEGORIA <- gsub("SERVICIOS EDUCATIVOS", "Education services", denue_data$CATEGORIA)
denue_data$CATEGORIA <- gsub("OTRA ACTIVIDAD", "Other", denue_data$CATEGORIA)

denue_data$per_ocu <- gsub("0 a 5 personas", "1 (0 to 5)", denue_data$per_ocu)
denue_data$per_ocu <- gsub("6 a 10 persona", "2 (6 to 10)", denue_data$per_ocu)
denue_data$per_ocu <- gsub("11 a 30 personas", "3 (11 to 30)", denue_data$per_ocu)
denue_data$per_ocu <- gsub("31 a 50 personas", "4 (31 to 50)", denue_data$per_ocu)
denue_data$per_ocu <- gsub("51 a 100 personas", "5 (51 to 100)", denue_data$per_ocu)
denue_data$per_ocu <- gsub("101 a 250 personas", "6 (101 to 250)", denue_data$per_ocu)
denue_data$per_ocu <- gsub("251 y más personas", "7 (>251)", denue_data$per_ocu)

# SUBSET OUT "OTHER" OUTLETS NOT DIRECTLY RELATED TO FOODSCAPE
denue_data_foodscape <- subset(denue_data, denue_data$CATEGORIA != "Other")

denue_data_foodscape$ONSITE_CONSUMPTION = ""

for(n in 1:nrow(denue_data_foodscape)){
  if(denue_data_foodscape[n,]$FACTOR_SIT == "1"){denue_data_foodscape[n,]$ONSITE_CONSUMPTION = "Low"}
  if(denue_data_foodscape[n,]$FACTOR_SIT == "2"){denue_data_foodscape[n,]$ONSITE_CONSUMPTION = "Medium"}
  if(denue_data_foodscape[n,]$FACTOR_SIT == "3"){denue_data_foodscape[n,]$ONSITE_CONSUMPTION = "High"}
}

# SAVE R DATA FROM GLOBAL ENVIRONMENT FOR FASTER LOADING TIMES IN FUTURE
setwd("~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/FOODWASTEMEXICOCITY/DATA/RData")
save(denue_data_foodscape, file = "foodscape_data.RData")
setwd("~/sigdata/1BB596AA0B7E98241/archivos/sigdata/PROYECTOS/FOODWASTEMEXICOCITY")

library(dplyr)
# ONE VARIABLE MELTS
denue_data_borough <- denue_data_foodscape %>%
  group_by(municipio) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count)*100)

denue_data_category <- denue_data_foodscape %>%
  group_by(CATEGORIA) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count)*100)

denue_data_size <- denue_data_foodscape %>%
  group_by(per_ocu) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count)*100)

# TWO VARIABLE MELTS

denue_data_size_borough <- denue_data_foodscape %>%
  group_by(municipio, per_ocu) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count)*100)

denue_data_onsite_borough <- denue_data_foodscape %>%
  group_by(municipio, FACTOR_SIT) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count)*100)

denue_data_category_burough <- denue_data_foodscape %>%
  group_by(municipio, CATEGORIA) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count)*100)


# THREE VARIABLE MELTS
denue_data_size_category_burough <- denue_data_foodscape %>%
  group_by(municipio, CATEGORIA, per_ocu) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count)*100)

denue_data_onsite_category_burough <- denue_data_foodscape %>%
  group_by(municipio, CATEGORIA, FACTOR_SIT) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count))

# PLOTS
library(ggplot2)
library(RColorBrewer)

# 1 AND 2 VARIABLE PLOTS

ggplot(denue_data_category_burough, aes(x = factor(municipio), y = perc, fill = CATEGORIA)) +
  geom_bar(stat="identity", width = .7, position = position_fill(reverse = TRUE)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_flip() +
  labs(title="Percentage of outlets by category",
       subtitle="Outlets per category per borrough",
       y="Percentage of outlets (%)",
       x="Borough",
       caption = "Source: Author contribution with data from DENUE/INEGI") +
  theme(legend.position = "bottom") +
  # geom_text(aes(label = sprintf("%0.2f", round(perc*100, digits = 2))), colour = "black", size = 3,
  #           position=position_dodge(width=0.025), hjust = "bottom") +
  labs(fill='Classification') +
  scale_fill_brewer(palette = "Set3")

ggplot(denue_data_onsite_borough, aes(x = factor(municipio), y = perc, fill = FACTOR_SIT)) +
  geom_bar(stat="identity", width = .7, position = position_fill(reverse = TRUE)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_flip() +
  labs(title="Onsite consumption by borough",
       subtitle="Outlets with low, medium and high onsite consumption",
       y="Percentage of outlets (%)",
       x="Borough",
       caption = "Source: Author contribution with data from DENUE/INEGI") +
  theme(legend.position = "bottom") +
  # geom_text(aes(label = sprintf("%0.2f", round(perc*100, digits = 2))), colour = "black", size = 3,
  #           position=position_dodge(width=0.025), hjust = "bottom") +
  labs(fill='Classification') +
  scale_fill_brewer(palette = "Oranges")

# 3 VARIABLE PLOTS (FACET WRAP)
ggplot(denue_data_size_category_burough, aes(x = factor(municipio), y = perc, fill = per_ocu)) +
  geom_bar(stat="identity", width = .7, position = position_fill(reverse = TRUE)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_flip() +
  labs(title="Distribution of outlet size",
       subtitle="Percentage of outlets per category and borough",
       y="Outlets (%)",
       x="Borough",
       caption = "Source: Author contribution with data from DENUE/INEGI") +
  theme(legend.position = "bottom")  +
  labs(fill='Outlet size') +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(vars(CATEGORIA), ncol = 3)

# DOES NOT WORK CORRECTLY
ggplot(denue_data_onsite_category_burough, aes(x = factor(municipio), y = perc, fill = FACTOR_SIT)) +
  geom_bar(stat="identity", width = .7, position = position_fill(reverse = TRUE)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  coord_flip() +
  labs(title="Distribution of outlet size",
       subtitle="Percentage of outlets per category and borough",
       y="Outlets (%)",
       x="Borough",
       caption = "Source: Author contribution with data from DENUE/INEGI") +
  theme(legend.position = "bottom")  +
  labs(fill='Outlet size') +
  scale_fill_brewer(palette = "Set3") +
  facet_wrap(vars(CATEGORIA), ncol = 3)




