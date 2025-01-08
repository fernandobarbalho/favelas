library(sidrar)
library(tidyverse)
library(geobr)
library(sf)
library(colorspace)


mapa_estados<- geobr::read_state()

mapa_municipios<- geobr::read_municipality()

mapa_municipios_seat<- geobr::read_municipal_seat()

geobr::read_census_tract()


############### Número de Favelas por cidade brasileira

info_sidra(9883, wb = TRUE)

favelas_municipios<-  
  get_sidra(x = 9883,
            #variable = c(11601,1607,11602), #12607 (número índice com ajustes sazonal), 11601 mês/mês anterior com ajustes sazonal, 11602 mês/mesmo mês do ano anterior 
            variable = 9910,
            #period = c("202301-202406"),
            #period = c("last" = 12),
            geo = "City",
            #geo.filter = "RS",
            #classific = "C544",
            #category =  list(c(129314 )), #, 72118,72119, 12046
            header = FALSE,
            format = 3)

favelas_trabalho<-
  favelas_municipios %>%
  select(c(4:6)) %>%
  rename(
    quantidade = V,
    cod_ibge = D1C,
    municipio = D1N
  )



mapas_favela<-
  mapa_municipios %>%
  inner_join(
    favelas_trabalho %>%
      mutate(code_muni= as.numeric(cod_ibge))
  )

favelas_trabalho<-
  favelas_trabalho %>%
  arrange(desc(quantidade))

numero_total<- sum(favelas_trabalho$quantidade)

favelas_trabalho$total_acumulado<- cumsum(favelas_trabalho$quantidade)

favelas_trabalho <-
  favelas_trabalho %>%
  mutate(code_muni= as.numeric(cod_ibge),
         perc_acumulado = total_acumulado/numero_total) 

saveRDS(favelas_trabalho, "favelas_trabalho.rds")

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


### Proporção de habitantes nas favelas por cor/raça

info_sidra(9884, wb = TRUE)


favelas_municipios_cor<-  
  get_sidra(x = 9884,
            #variable = c(11601,1607,11602), #12607 (número índice com ajustes sazonal), 11601 mês/mês anterior com ajustes sazonal, 11602 mês/mesmo mês do ano anterior 
            variable = 1009612,
            #period = c("202301-202406"),
            #period = c("last" = 12),
            geo = "City",
            #geo.filter = "RS",
            classific = c("C86"),
            category =  list(c(2776, 2777,2778,2779, 2780,  2781     )), #, 72118,72119, 12046
            header = FALSE,
            format = 3)

favelas_cor_trabalho<-
  favelas_municipios_cor %>%
  select(c(4:6,9)) %>%
  rename(
    percentual = V,
    cod_ibge = D1C,
    municipio = D1N,
    cor_raca = D4N
  )

saveRDS(favelas_cor_trabalho, "favelas_cor_trabalho.rds")

favelas_municipios_cor_numero<-  
  get_sidra(x = 9884,
            #variable = c(11601,1607,11602), #12607 (número índice com ajustes sazonal), 11601 mês/mês anterior com ajustes sazonal, 11602 mês/mesmo mês do ano anterior 
            variable = 9612,
            #period = c("202301-202406"),
            #period = c("last" = 12),
            geo = "City",
            #geo.filter = "RS",
            classific = c("C86"),
            category =  list(c(2776, 2777,2778,2779, 2780,  2781     )), #, 72118,72119, 12046
            header = FALSE,
            format = 3)

favelas_cor_numero_trabalho<-
  favelas_municipios_cor_numero %>%
  select(c(4:6,9)) %>%
  rename(
    populacao = V,
    cod_ibge = D1C,
    municipio = D1N,
    cor_raca = D4N
  )

saveRDS(favelas_cor_numero_trabalho, "favelas_cor_numero_trabalho.rds")



###Populações das favelas


info_sidra(9888, wb = TRUE)

favelas_populacao<-  
  get_sidra(x = 9888,
            #variable = c(11601,1607,11602), #12607 (número índice com ajustes sazonal), 11601 mês/mês anterior com ajustes sazonal, 11602 mês/mesmo mês do ano anterior 
            variable = 9612,
            #period = c("202301-202406"),
            #period = c("last" = 12),
            geo = "City",
            #geo.filter = list("City"= 2304400),
            #classific = c("C86"),
            #category =  list(c(2776, 2777,2778,2779, 2780,  2781     )), #, 72118,72119, 12046
            header = FALSE,
            format = 3)


favelas_populacao_trabalho<-
  favelas_populacao %>%
  select(c(4:6)) %>%
  rename(
    populacao = V,
    cod_ibge = D1C,
    municipio = D1N
  )

saveRDS(favelas_populacao_trabalho, "favelas_populacao_trabalho.rds")
info_sidra(9923, wb = TRUE)



municipios_populacao<-
  get_sidra(x = 9923,
            #variable = c(11601,1607,11602), #12607 (número índice com ajustes sazonal), 11601 mês/mês anterior com ajustes sazonal, 11602 mês/mesmo mês do ano anterior 
            variable = 93,
            #period = c("202301-202406"),
            #period = c("last" = 12),
            geo = "City",
            #geo.filter = list("City"= 2304400),
            classific = c("C1"),
            category =  list(c(6795)), #, 72118,72119, 12046
            header = FALSE,
            format = 3)

municipios_populacao_trabalho<-
  municipios_populacao %>%
  select(c(4:6)) %>%
  rename(
    populacao = V,
    cod_ibge = D1C,
    municipio = D1N
  )

saveRDS(municipios_populacao_trabalho, "municipios_populacao_trabalho.rds")


info_sidra(9605, wb = TRUE)

municipios_cor_raca<-
  get_sidra(x = 9605,
            #variable = c(11601,1607,11602), #12607 (número índice com ajustes sazonal), 11601 mês/mês anterior com ajustes sazonal, 11602 mês/mesmo mês do ano anterior 
            variable = 93,
            period = c("2022"),
            #period = c("last" = 12),
            geo = "City",
            #geo.filter = list("City"= 2304400),
            classific = c("C86"),
            category =  list(c(2776:2780)), #, 72118,72119, 12046
            header = FALSE,
            format = 3)

municipios_cor_raca_trabalho<-
  municipios_cor_raca %>%
  select(c(4:6,9)) %>%
  rename(
    populacao = V,
    cod_ibge = D1C,
    municipio = D1N,
    cor_raca = D4N
  )

saveRDS(municipios_cor_raca_trabalho, "municipios_cor_raca_trabalho.rds")

municipios_cor_raca_perc<-
  get_sidra(x = 9605,
            #variable = c(11601,1607,11602), #12607 (número índice com ajustes sazonal), 11601 mês/mês anterior com ajustes sazonal, 11602 mês/mesmo mês do ano anterior 
            variable = 1000093,
            period = c("2022"),
            #period = c("last" = 12),
            geo = "City",
            #geo.filter = list("City"= 2304400),
            classific = c("C86"),
            category =  list(c(2776:2780)), #, 72118,72119, 12046
            header = FALSE,
            format = 3)

municipios_cor_raca_perc_trabalho<-
  municipios_cor_raca_perc %>%
  select(c(4:6,9)) %>%
  rename(
    percentual = V,
    cod_ibge = D1C,
    municipio = D1N,
    cor_raca = D4N
  )

saveRDS(municipios_cor_raca_perc_trabalho, "municipios_cor_raca_perc_trabalho.rds")


municipios_cor_raca_perc_trabalho%>%
  readr::write_csv("municipios_cor_raca_perc_trabalho.csv")


favelas_cor_trabalho %>%
  readr::write_csv("favelas_cor_trabalho.csv")

##############Esses códigos abaixo resultaram em análises pouco produtivas.
# favela_perc_cor_raca_mun<-
#   favelas_cor_numero_trabalho %>%
#   inner_join(
#     municipios_cor_raca_trabalho %>%
#       select(-municipio),
#     by = c("cod_ibge", "cor_raca")
#   ) %>%
#   mutate(percentual = (populacao.x/populacao.y)*100)
# 
# cidades_sel<-
#   (favelas_trabalho %>%
#      filter(perc_acumulado <=0.8007775))$cod_ibge
# 
# 
# favela_perc_cor_raca_mun %>%
#   filter(cod_ibge %in% cidades_sel) %>%
#   ggplot() +
#   geom_boxplot(aes(x= cor_raca, y= percentual))
# 
# favela_perc_cor_raca_mun %>%
#   filter(cod_ibge %in% cidades_sel) %>%
#   summarise(mean( percentual, na.rm = TRUE),
#             median(percentual, na.rm = TRUE),
#             .by = cor_raca)
# 
# fab<-
#   favela_perc_cor_raca_mun %>%
#   filter(cod_ibge %in% cidades_sel)
# 
# 
# mapas_favela_mun_cor_seat<-
#   mapa_municipios_seat %>%
#   inner_join(
#     favela_perc_cor_raca_mun %>%
#       filter(cod_ibge %in% cidades_sel) %>%
#       mutate(code_muni= as.numeric(cod_ibge)) 
#   )
# 
# 
# mapas_favela_mun_cor_seat %>%
#   filter(cor_raca %in% c("Branca", "Parda", "Preta")) %>%
#   ggplot() +
#   geom_sf(data = mapa_estados, fill = NA) +
#   geom_sf(aes( size = percentual, ), pch = 21 ,color = "white", alpha= 1 ) +
#   scale_fill_continuous_sequential(palette= "Heat 2") +
#   theme_void() +
#   theme(
#     panel.background = element_rect(fill = "black"),
#     plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5, size = 12),
#     strip.background = element_rect(fill = "black"),
#     strip.text = element_text(color = "white")
#   ) +
#   facet_wrap(cor_raca~.) +
#   labs(
#     title =  "Cor/raça nas favelas brasileiras",
#     subtitle =  "Dados do Censo de 2022",
#     fill = "%",
#     caption = "Fonte: IBGE. Elaboração própria"
#   )
# 
# ggsave(filename = "cor_mun_raca_favela.jpg", dpi = 300)
