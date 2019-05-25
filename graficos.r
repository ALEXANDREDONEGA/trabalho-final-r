library(dplyr)
library(leaflet)
library(sf)
library(mapsBR) # Não é compativel com a versão 3.6.0
library(rgeos)
library(rgdal)
library(janitor)
library(highcharter)
library(tidyr)
library(ggplot2)
library(readr)

idese = readRDS("data/Idese Municipios 2007-2014.rds") %>% 
  clean_names() 

options(scipen = 999)
ideseTest = idese %>% 
  filter(cod != "4314548") %>% 
  select(cod,nome, ano, bloco_educacao,bloco_saude,bloco_renda,idese,populacao) %>% 
  gather(bloco,valor,bloco_educacao,bloco_saude,bloco_renda,idese,populacao) %>% 
  group_by(cod,bloco) %>% 
  #filter(ano !=2007) %>% #Botei 2007 de volta kkk não vi nada que pudesse prejudicar a analise.
  mutate(
    difference = valor - lag(valor)
  ) %>% 
  filter(!is.na(difference)) %>%
  group_by(cod,nome,bloco) %>% 
  summarise(
    sumDiff = sum(difference)
  ) %>% 
  spread(bloco, sumDiff)

top5= ideseTest %>% 
  arrange(desc(idese)) %>% 
  head(5)

top5lixo = ideseTest %>% 
  arrange(idese) %>% 
  head(5)

top5 %>% 
  left_join(
    idese,
    by = c("cod" = "cod")
  ) %>% 
  gather(bloco,valor,bloco_educacao.y,bloco_saude.y,bloco_renda.y,idese.y,populacao.y) %>% 
  ggplot(aes(x = ano, valor, color = nome.y)) +
  geom_line()+
  geom_point()+
  facet_wrap(~bloco, scales = "free_y")

top5lixo %>% 
  left_join(
    idese,
    by = c("cod" = "cod")
  ) %>% 
  gather(bloco,valor,bloco_educacao.y,bloco_saude.y,bloco_renda.y,idese.y,populacao.y) %>% 
  ggplot(aes(x = ano, valor, color = nome.y)) +
  geom_line()+
  geom_point()+
  facet_wrap(~bloco, scales = "free_y")
