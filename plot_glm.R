library(tidyverse)

df <- read.csv('data/1 - PLANILHA DE DADOS PARA RODAR NO R.csv', sep = ';')
df

tabela_corpos<-df %>% 
  group_by (ID_do_caso) %>%
  summarise(abundancia=sum(abundancia),riqueza=n_distinct(especie),
#CALCULANDO TODAS AS MEDIAS/MEDIANAS (TEMP, UMIDADE E CHUVA)
  temp_media=mean(temp_media_da_data_do_encontro),
  temp_mediana=median(temp_media_da_data_do_encontro),
  temp_media_7dias=mean(temp_med_7_dias_da_data_do_encontro),
  temp_mediana_7dias=median(temp_med_7_dias_da_data_do_encontro),
  umid_media=mean(umidade_media_da_data_do_encontro),
  umid_mediana=median(umidade_media_da_data_do_encontro),
  umid_media_7dias=mean(umd_med_7_dias_da_data_do_encontro),
  umid_mediana_7dias=median(umd_med_7_dias_da_data_do_encontro),
  chuva_acum_media=mean(chuva_acumulada_em_24h),
  chuva_acum_mediana=median(chuva_acumulada_em_24h),
  chuva_acum_7dias_media=mean(chuva_acumulada_7_dias),
  chuva_acum_7dias_mediana=median(chuva_acumulada_7_dias))


ggplot(tabela_corpos,aes(x=umid_mediana,y=abundancia))+
  geom_point() +
  geom_smooth(method="lm",formula=y~x+I(x^2), se=TRUE)

ggplot(tabela_corpos,aes(x=umid_mediana,y=abundancia))+
  geom_point() +
  geom_smooth(method="loess", se=TRUE)

ggplot(tabela_corpos,aes(x=umid_mediana,y=abundancia))+
  geom_point() +
  geom_smooth(method="glm", family="quasipoisson", se=TRUE)
