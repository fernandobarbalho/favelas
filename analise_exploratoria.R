library(tidyverse)

favelas_trabalho <- readRDS("~/Github/favelas/favelas_trabalho.rds")
favelas_cor_trabalho <- readRDS("~/Github/favelas/favelas_cor_trabalho.rds")

mapas_favela_cor_seat<-
  mapa_municipios_seat %>%
  inner_join(
    favelas_cor_trabalho %>%
      mutate(code_muni= as.numeric(cod_ibge)) 
  )


mapas_favela_cor_seat %>%
  filter(percentual >=10) %>%
  mutate(cor_raca = factor(cor_raca, levels = c("Indígena", "Preta", "Branca","Parda"))) %>%
  ggplot() +
  geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes( fill = percentual), pch = 21 ,color = "black", size = 0.7 ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white")
  ) +
  facet_wrap(cor_raca~.) +
  labs(
    title =  "Cor/raça nas favelas brasileiras",
    subtitle =  "Dados do Censo de 2022",
    fill = "%",
    caption = "Fonte: IBGE. Elaboração própria"
  )




###Mapa mostrando as cidades com maior número de favelas
mapas_favela_seat<-
  mapa_municipios_seat %>%
  inner_join(
    favelas_trabalho %>%
      mutate(code_muni= as.numeric(cod_ibge)) 
  )




mapas_favela_seat %>%
  filter(perc_acumulado <=0.8007775) %>%
  ggplot() +
  geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes(size = quantidade), pch = 21 ,color = "black", fill= "white", alpha=0.5 ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    
  )+
  labs(
    title =  "As 130 cidades com maior número de favelas",
    subtitle =  "Dados do censo de 2022",
    #fill = "%",
    caption = "Fonte: IBGE. Elaboração própria"
  )



### Mapa mostrando a distribuição da população das favelas por cor/raça


mapas_favela_cor_seat<-
  mapa_municipios_seat %>%
  inner_join(
    favelas_cor_trabalho %>%
      mutate(code_muni= as.numeric(cod_ibge)) 
  )


mapas_favela_cor_seat %>%
  filter(percentual >=10) %>%
  mutate(cor_raca = factor(cor_raca, levels = c("Indígena", "Preta", "Branca","Parda"))) %>%
  ggplot() +
  geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes( fill = percentual), pch = 21 ,color = "black", size = 0.7 ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white")
  ) +
  facet_wrap(cor_raca~.) +
  labs(
    title =  "Cor/raça nas favelas brasileiras",
    subtitle =  "Dados do Censo de 2022",
    fill = "%",
    caption = "Fonte: IBGE. Elaboração própria"
  )

