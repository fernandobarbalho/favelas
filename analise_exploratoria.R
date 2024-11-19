library(tidyverse)

favelas_trabalho <- readRDS("~/Github/favelas/favelas_trabalho.rds")
favelas_cor_trabalho <- readRDS("~/Github/favelas/favelas_cor_trabalho.rds")
favelas_populacao_trabalho <- readRDS("~/Github/favelas/favelas_populacao_trabalho.rds")
municipios_cor_raca_trabalho <- readRDS("~/Github/favelas/municipios_cor_raca_trabalho.rds")
municipios_populacao_trabalho <- readRDS("~/Github/favelas/municipios_populacao_trabalho.rds")
municipios_cor_raca_perc_trabalho <- readRDS("~/Github/favelas/municipios_cor_raca_perc_trabalho.rds")


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

### Distribuição da população brasileira x população das favelas

municipios_cor_raca_perc_trabalho %>%
  ggplot(aes(x= cor_raca, y= percentual)) +
  geom_boxplot()


favelas_cor_trabalho %>%
  ggplot(aes(x= cor_raca, y= percentual)) +
  geom_boxplot()

df_modelo_municipios<-
  favelas_cor_trabalho %>%
  mutate(uf = str_sub(municipio, str_length(municipio)-2, str_length(municipio)))

modelo_lm_mun<- lm(percentual~cor_raca+uf+ cor_raca*uf , data = df_modelo_municipios)

summary(modelo_lm_mun)

df_modelo_favelas<-
  favelas_cor_trabalho %>%
  mutate(uf = str_sub(municipio, str_length(municipio)-2, str_length(municipio)))

modelo_lm<- lm(percentual~cor_raca+uf+ cor_raca*uf , data = df_modelo_favelas)


summary(modelo_lm)

municipios_favela<- unique(favelas_cor_trabalho$cod_ibge)
  
  

dados_favela_municipios<-
  municipios_cor_raca_perc_trabalho %>%
  filter(cod_ibge %in% municipios_favela) %>%
  mutate(agrupamento = "municipio") %>%
  bind_rows(
    favelas_cor_trabalho %>%
      mutate(agrupamento = "favela")
  )%>%
  filter(cor_raca %in% c("Branca", "Preta", "Parda"))  


favela_municipio_fake<- tibble(municipio = 1:100, favela= 1:100)

favela_municipio_fake %>%
  ggplot()aes(x=municipio, y=favela)+
  geom_smooth(method = "lm")


dados_favela_municipios %>%
  select(-municipio) %>%
  pivot_wider(names_from = agrupamento, values_from = percentual) %>%
  ggplot(aes(x=municipio, y=favela)) +
  geom_point( aes(fill = cor_raca), pch= 21, color = "black")+
  geom_smooth(data=favela_municipio_fake,method = "lm", color = "yellow", linetype = "dashed") +
  scale_fill_discrete_qualitative(palette = "Dark 2") +
  theme_light() +
  theme(
    panel.grid =  element_blank() ,
    panel.background = element_rect(fill = "black")
  )


dados_favela_municipios %>%
  ggplot(aes(x= cor_raca, y= percentual)) +
  geom_boxplot() +
  facet_wrap(agrupamento ~.)


mapa_favela_municipio_cor_seat<-
  mapa_municipios_seat %>%
  inner_join(
    dados_favela_municipios %>%
      mutate(code_muni= as.numeric(cod_ibge)) 
  )


mapa_favela_municipio_cor_seat_dif <-
  mapa_municipios_seat %>%
  inner_join(
    dados_favela_municipios %>%
      select(-municipio) %>%
      pivot_wider(names_from = agrupamento, values_from = percentual) %>%
      mutate(diferenca = municipio-favela) %>%
      mutate(code_muni= as.numeric(cod_ibge)) 
  )


mapa_favela_municipio_cor_seat_dif %>%
  ggplot() +
  geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes( fill = diferenca, size = abs(diferenca)), pch = 21, alpha = 1, color = "black"  ) +
  scale_fill_continuous_divergingx(palette = "RdBu") +
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



mapa_favela_municipio_cor_seat %>%
  filter(cor_raca== "Branca") %>%
  mutate(agrupamento = factor(agrupamento, levels = c("municipio", "favela"))) %>%
  ggplot() +
  geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes( fill = percentual), pch = 21  ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white")
  ) +
  facet_wrap(agrupamento~.) +
  labs(
    title =  "Cor/raça nas favelas brasileiras",
    subtitle =  "Dados do Censo de 2022",
    fill = "%",
    caption = "Fonte: IBGE. Elaboração própria"
  )


mapa_favela_municipio_cor_seat %>%
  filter(cor_raca== "Branca") %>%
  mutate(agrupamento = factor(agrupamento, levels = c("municipio", "favela"))) %>%
  ggplot() +
  geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes( fill = percentual), pch = 21, size =1.5  ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white")
  ) +
  facet_wrap(agrupamento~.) +
  labs(
    title =  "Cor/raça nas favelas brasileiras",
    subtitle =  "Dados do Censo de 2022",
    fill = "%",
    caption = "Fonte: IBGE. Elaboração própria"
  )


mapa_favela_municipio_cor_seat %>%
  filter(cor_raca== "Parda") %>%
  mutate(agrupamento = factor(agrupamento, levels = c("municipio", "favela"))) %>%
  ggplot() +
  geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes( fill = percentual), pch = 21, size =1.5  ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white")
  ) +
  facet_wrap(agrupamento~.) +
  labs(
    title =  "Cor/raça nas favelas brasileiras",
    subtitle =  "Dados do Censo de 2022",
    fill = "%",
    caption = "Fonte: IBGE. Elaboração própria"
  )



#Sul Comparar preta
mapa_favela_municipio_cor_seat %>%
  filter(cor_raca %in% c("Preta")) %>%
  filter(abbrev_state %in% c("RS","SC","PR")) %>%
  mutate(agrupamento = factor(agrupamento, levels = c("municipio", "favela"))) %>%
  ggplot() +
  #geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes( fill = percentual, size = percentual), pch = 21  ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white")
  ) +
  facet_wrap(agrupamento+cor_raca~.) +
  labs(
    title =  "Cor/raça nas favelas brasileiras",
    subtitle =  "Dados do Censo de 2022",
    fill = "%",
    caption = "Fonte: IBGE. Elaboração própria"
  )



####RJ comparar com branca e preta

mapa_favela_municipio_cor_seat %>%
  filter(cor_raca %in% c("Branca")) %>%
  filter(abbrev_state %in% c("RJ")) %>%
  mutate(agrupamento = factor(agrupamento, levels = c("municipio", "favela"))) %>%
  ggplot() +
  #geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes( fill = percentual, size = percentual), pch = 21  ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white")
  ) +
  facet_wrap(agrupamento+cor_raca~.) +
  labs(
    title =  "Cor/raça nas favelas brasileiras",
    subtitle =  "Dados do Censo de 2022",
    fill = "%",
    caption = "Fonte: IBGE. Elaboração própria"
  )


##SP Comparar branca + parda _preta
mapa_favela_municipio_cor_seat %>%
  filter(cor_raca %in% c("Preta")) %>%
  filter(abbrev_state %in% c("RJ")) %>%
  mutate(agrupamento = factor(agrupamento, levels = c("municipio", "favela"))) %>%
  ggplot() +
  #geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes( fill = percentual, size = percentual), pch = 21, alpha=0.5  ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white")
  ) +
  facet_wrap(agrupamento+cor_raca~.) +
  labs(
    title =  "Cor/raça nas favelas brasileiras",
    subtitle =  "Dados do Censo de 2022",
    fill = "%",
    caption = "Fonte: IBGE. Elaboração própria"
  )


mapa_favela_municipio_cor_seat %>%
  filter(cor_raca %in% c("Branca")) %>%
  filter(abbrev_state %in% c("SP")) %>%
  mutate(agrupamento = factor(agrupamento, levels = c("municipio", "favela"))) %>%
  ggplot() +
  #geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes( fill = percentual, size = percentual), pch = 21  ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white")
  ) +
  facet_wrap(agrupamento+cor_raca~.) +
  labs(
    title =  "Cor/raça nas favelas brasileiras",
    subtitle =  "Dados do Censo de 2022",
    fill = "%",
    caption = "Fonte: IBGE. Elaboração própria"
  )


mapa_favela_municipio_cor_seat %>%
  filter(cor_raca %in% c("Parda")) %>%
  filter(abbrev_state %in% c("SP")) %>%
  mutate(agrupamento = factor(agrupamento, levels = c("municipio", "favela"))) %>%
  ggplot() +
  #geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes( fill = percentual, size = percentual), pch = 21  ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white")
  ) +
  facet_wrap(agrupamento+cor_raca~.) +
  labs(
    title =  "Cor/raça nas favelas brasileiras",
    subtitle =  "Dados do Censo de 2022",
    fill = "%",
    caption = "Fonte: IBGE. Elaboração própria"
  )


mapa_favela_municipio_cor_seat %>%
  filter(cor_raca %in% c("Preta")) %>%
  filter(abbrev_state %in% c("SP")) %>%
  mutate(agrupamento = factor(agrupamento, levels = c("municipio", "favela"))) %>%
  ggplot() +
  #geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes(  size = percentual), pch = 21, alpha=0.5, fill= "white"  ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white")
  ) +
  facet_wrap(agrupamento+cor_raca~.) +
  labs(
    title =  "Cor/raça nas favelas brasileiras",
    subtitle =  "Dados do Censo de 2022",
    fill = "%",
    caption = "Fonte: IBGE. Elaboração própria"
  )
