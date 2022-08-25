library(tmap)
library(rgeos)
library(geobr)
library(leaflet)
library(tidyverse)

df <- 
  read.csv('casos_mun.csv')

cases_by_city <- df %>%
  group_by(mun) %>% 
  count() %>%
  mutate(mun = tolower(mun))
  #        n = case_when(n>0 ~ 1))
  
mun <- read_municipality(code_muni="TO", year=2010)

mun <- mun %>% 
  mutate(name_muni_2 = tolower(name_muni))

sf_with_cases <- mun %>%
  left_join(cases_by_city, by = c("name_muni_2" = "mun")) %>% 
  replace_na(list(n = 0)) 

pal <- colorNumeric(
  palette = "red",
  domain = sf_with_cases$n)

leaflet(sf_with_cases) %>%
  addProviderTiles("Stamen.TonerHybrid") %>%
  addPolygons(stroke = F, 
              smoothFactor = 0.2, fillOpacity = 4,
              color = ~pal(n))

