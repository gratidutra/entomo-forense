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
  summarise(`Number of cadavers`= n_distinct(ID_do_caso)) %>%
  mutate(mun = tolower(mun))
#        n = case_when(n>0 ~ 1))

mun <- read_municipality(code_muni="TO", year=2010)

mun <- mun %>% 
  mutate(name_muni_2 = tolower(name_muni))

sf_with_cases <- mun %>%
  left_join(cases_by_city, by = c("name_muni_2" = "mun")) %>% 
  replace_na(list(n = 0))

# 1 -----------------------------------------------------------------------


mapa_casos <- tm_shape(sf_with_cases) +
  tm_polygons(fill = 'n', col = NA, fill.legend = tm_legend(title = expression("Número de cadáveres"), palette = 'Blues')) +
  tm_borders() +
  tm_compass(type = '8star', position = c("left", "top"), size = 2.5) +
  tm_scalebar(breaks = c(0, 50, 100), text.size = 0.5, position = c("right", "bottom")) +
  tm_minimap()
  

#tmap_mode("view")
mapa_casos

# 2 -----------------------------------------------------------------------
brasil <- st_read("data/shapefiles/BR/BR_UF_2022.shp")

main_map <- ggplot(data = sf_with_cases) +
  geom_sf(aes(fill = `Number of cadavers`)) +
  scale_fill_gradient(low = "#fdf0d5", high = "red", na.value = 'lightgray') +
  #geom_sf(color = "black") +
  annotation_north_arrow(
    location = "tl", which_north = "true", 
    style = north_arrow_fancy_orienteering,
    height = unit(2.5, "cm")
  ) +
  annotation_scale(location = "br", width_hint = 0.5) +
  theme_minimal() +
  theme(legend.position = "right")

# Criar o minimapa
inset_map <- ggplot(data = brasil) +
  geom_sf() +
  theme_void()

# Combinar os mapas
combined_map <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset_map, x = 0.7, y = 0.7, width = 0.25, height = 0.25)

combined_map

ggsave('new_map.svg', height=10, width = 10, dpi=600)
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
