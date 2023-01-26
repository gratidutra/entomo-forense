library(spData)
library(raster)
library(tidyverse)
library(sf)
library(tidyverse)
library(geobr)
library(tmap)

df <- 
  read.csv('data/casos_3.csv')

cases_by_city <- df %>%
  group_by(mun= municipio)%>%
  summarise(n= n_distinct(ID_do_caso)) %>%
  mutate(mun = tolower(mun))
#        n = case_when(n>0 ~ 1))

mun <- read_municipality(code_muni="TO", year=2010)

mun <- mun %>% 
  mutate(name_muni_2 = tolower(name_muni))

sf_with_cases <- mun %>%
  left_join(cases_by_city, by = c("name_muni_2" = "mun")) %>% 
  replace_na(list(n = 0)) 

mapa_casos <- tm_shape(sf_with_cases) + 
  tm_fill(col ='n', title= expression("Número de cadáveres"), palette = 'Blues') +
  tm_borders()+
  tm_compass(type = '8star', position = c("right", "top"), size = 2.5) 
  #tm_scale_bar(breaks = c(0,50,100), text.size = 0.5, position = c("right", "bottom"))

tmap_mode("view")
mapa_casos


# casos ------------------------------------------------------------

casos <- read_csv('casos.csv')

casos$`data da coleta` <- as.POSIXct(casos$`data da coleta`, format = "%d/%m/%Y")

casos_por_mes <- casos %>% 
  group_by(mes = month(casos$`data da coleta`, label=TRUE)) %>%
  summarise(Abundância = sum(abundância))
  

ggplot(casos_por_mes, aes(mes, Abundância, fill = Abundância)) +
  geom_bar(binwidth = 1, stat = "identity") +
  theme_light() +
  #scale_fill_gradient(low = "lightgray", high = "black") +
  #facet_wrap(~espécie) +
  ylab("Abundance") +
  xlab("Month") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_polar()+
  theme_minimal()
