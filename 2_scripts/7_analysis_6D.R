#### Sase article - Data Analysis
#### Sase article - Data Analysis
#### Sase article - Data Analysis

# Start: 19/05/2023
# Author: Alexandre Stein

# R options and libraries -------------------------------------------------
options(scipen = 999L)

library(tidyverse)
library(data.table)
library(ggstatsplot)
library(ggthemes)
library(sf)
library(tmap)
library(classInt)
library(patchwork)
library(ggrepel)
library(geomtextpath)

rm(list = ls(all = TRUE))

# Side data -----------------------------------------------------------

colors <- c("#2D3A4E", "#798594", "#D9D5C7","#E6673D")

municipios <- read.csv("C:/Users/queir/Meu Drive/Economia/Outros/Tabelas de Compatibilizacoes/municipio.csv") %>%
    select(id_municipio, nome) %>%
    mutate(id_municipio = as.character(id_municipio))

cbos <- fread("C:/Users/queir/Meu Drive/Economia/Outros/Tabelas de Compatibilizacoes/cbo_2002.csv",
              colClasses = list(character = 1:10)) %>%
    select(cbo_2002, descricao) %>% 
    mutate(cbo_2002 = as.character(cbo_2002))

shape_muns <- geobr::read_municipality() %>% 
    mutate(code_muni = as.character(code_muni))

shape_estados <- geobr::read_state() %>% 
  mutate(reg = case_when(
           code_region == "1" ~ "North",
           code_region == "2" ~ "Northeast",
           code_region == "3" ~ "Southeast",
           code_region == "4" ~ "South",
           code_region == "5" ~ "Midwest"),
         reg = factor(reg,
                      levels = c(
                        "North",
                        "Northeast",
                        "Southeast",
                        "South",
                        "Midwest"
                      )))
shape_macros <- geobr::read_region()



# Data ----------------------------------------------------------------------------------------

cbos_interesse <- read.csv2("ocupacoes_interesse_6d.csv", colClasses = "character")


data <- readRDS("3_results_6d_cnae/densidade_media_municipios.RDS") %>% 
    mutate(reg = str_sub(country, 1, 1),
           reg = factor(case_when(reg == "1" ~ "North",
                                        reg == "2" ~ "Northeast",
                                        reg == "3" ~ "Southeast",
                                        reg == "4" ~ "South",
                                        reg == "5" ~ "Midwest"),
                        levels = c(
                            "North",
                            "Northeast",
                            "Southeast",
                            "South",
                            "Midwest"
                                   )),
           ano2 = as.integer(str_sub(ano, 3, 4))) %>% 
  filter(country != "1100205")



data2 <- read_rds("3_results_6d_cnae/densidade.RDS")

data2 %>% 
  mutate(interesse = ifelse(product %in% cbos_interesse$cbo_2002,
                            yes = 1,
                            no = 0)) %>%
  filter(interesse == 1) %>% 
  group_by(ano, country, interesse) %>% 
  summarise(rca_count_interesse = sum(rca_binario, na.rm = TRUE),
            rca_mean_interesse = mean(rca, na.rm = TRUE),
            densidade_media_interesse = mean(densidade, na.rm = TRUE)
            ) %>% 
  saveRDS("3_results_6d_cnae/densidade_interesse.RDS")




data.table::fwrite(data,
       "3_results/dados_digital_techs.CSV",
       scipen = getOption("scipen", 999L))


dados_municipios <- data %>% 
  group_by(ano, ano2, ano3, country, country_name, reg, n.empregos_tot_int, n.empregos_tot, 
         perc, n.rca.interesse, eci) %>% 
  summarise(media_densidade = mean(densidade, na.rm = TRUE)) %>%
  ungroup() 

# Density plot --------------------------------

media_brazil <- dados_municipios %>% 
  filter(ano %in% c("2006", "2021") & perc > 0 & perc < quantile(perc, 0.99, names = FALSE)) %>% 
  group_by(ano) %>% 
  summarise(media = median(perc, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pull(media)


texto <- data.frame(ano = c("2006", "2021"),
                    y = c(2.5, 2),
                    x = c(media_brazil[1], media_brazil[2]),
                    label = paste0(c("Median \n2006 = ", "Median \n2021 = "),
                                   round(c(media_brazil[1], media_brazil[2]), 3)))



dados_municipios %>% 
  filter(ano %in% c("2006", "2021") & perc > 0 & perc < quantile(perc, 0.99, names = FALSE)) %>% 
  ggplot()+
  geom_density(aes(x = perc, 
                   fill = ano),
               alpha = 0.7,
               color = NA)+
  geom_segment(aes(x = media_brazil[1], 
                   xend = media_brazil[1], 
                   y = 0, 
                   yend = 2.5), 
               color = colors[1], 
               size = 1.5)+
  geom_segment(aes(x = media_brazil[2], 
                   xend = media_brazil[2], 
                   y = 0, 
                   yend = 2), 
               color = colors[4], 
               size = 1.5)+
  geom_text_repel(data = texto,
                  aes(y = y, 
                      x = x,
                      label = label,
                      color = ano),
                  fontface = "bold",
                  direction = "x",
                  nudge_x = 0.1,
                  show.legend = FALSE)+
  scale_fill_manual(values = colors[c(1, 4)])+
  scale_color_manual(values = colors[c(1, 4)])+
  scale_y_continuous(expand = c(0,0))+
  theme_gdocs()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal")+
  labs(x = "Employment share",
       y = "Density",
       fill = NULL,
       color = NULL)

ggsave("3_results/analysis/empolyment_share_brazil.png", plot = last_plot(),
       height = 20, width = 30, units = "cm", dpi = 300)


# boxplot - regions --------------------------------

dados_brazil <- dados_municipios %>% 
  select(ano, ano3, reg, perc) %>% 
  filter(ano %in% c("2006", "2021") & perc > 0 & perc < quantile(perc, 0.99, names = FALSE)) %>% 
  mutate(reg = "Brazil")
  
dados_regioes <- dados_municipios %>%
  select(ano, ano3, reg, perc) %>% 
  filter(ano %in% c("2006", "2021") & perc > 0 & perc < quantile(perc, 0.99, names = FALSE)) 

dados_grafico <- rbind(dados_brazil, dados_regioes)

medias <- dados_grafico %>% 
  filter(ano %in% c("2006", "2021") & perc > 0 & perc < quantile(perc, 0.99, names = FALSE)) %>% 
  group_by(ano, ano3, reg) %>% 
  summarise(media = mean(perc, na.rm = TRUE)) %>% 
  ungroup()


dados_grafico %>% 
  ggplot()+
  geom_jitter(aes(y = reorder(ano, -ano3), 
                  x = perc,
                  color = ano), 
              height = 0.1, 
              size = 0.1,
              show.legend = FALSE)+
  geom_boxplot(aes(y = reorder(ano, -ano3), 
                   x = perc,
                   color = ano), 
               fill = NA, 
               alpha = 0.6, 
               outlier.shape = NA,
               show.legend = FALSE)+
  geom_text_repel(data = filter(medias, ano == "2006"), 
                  aes(x = media,
                      y = reorder(ano, -ano3),
                      label = round(media, 3),
                      color = ano),
                  nudge_y = 0.7,
                  show.legend = FALSE)+
  geom_text_repel(data = filter(medias, ano == "2021"), 
                  aes(x = media,
                      y = reorder(ano, -ano3),
                      label = round(media, 3),
                      color = ano),
                  nudge_y = -0.7,
                  show.legend = FALSE)+
  geom_point(data = medias,
             aes(x = media,
                 y = reorder(ano, -ano3)),
             shape=20, 
             size=5, 
             color="red", 
             fill="red") +
  scale_color_manual(values = colors[c(1, 4)])+
  theme_gdocs()+
  labs(x = "Employment share (percentage)",
       y = NULL)+
  facet_wrap(~reg)

#ggsave("3_results/analysis/empolyment_share_boxplot_v.png", plot = last_plot(),
#       height = 30, width = 15, units = "cm", dpi = 300)

ggsave("3_results/analysis/empolyment_share_boxplot.png", plot = last_plot(),
       height = 20, width = 30, units = "cm", dpi = 300)




# RCA - variation --------------------------------

dados_regioes <- dados_municipios %>%
  select(ano, country, reg, n.empregos_tot_int, n.rca.interesse, eci) %>% 
  filter(ano %in% c("2006", "2021") & n.empregos_tot_int > 0) %>% 
  rename(perc = n.rca.interesse) %>% 
  select(-n.empregos_tot_int)

dados_grafico <- dados_regioes %>%
  pivot_wider(names_from = ano,
              values_from = c(perc, eci),
              values_fill = 0) %>% 
  mutate(var_perc = perc_2021-perc_2006,
         var_eci = eci_2021-eci_2006)


medias <- dados_grafico %>% 
  summarise(media_06 = mean(perc_2006, na.rm = TRUE),
            media_21 = mean(perc_2021, na.rm = TRUE))

intervalos <- classIntervals(dados_grafico$eci_2006,
                             style = "quantile",
                             n = 3)$brks


dados_grafico <- dados_grafico %>% 
  mutate(variation = case_when(perc_2006 < medias$media_06[1] & perc_2021 < medias$media_21[1] ~ "Maintence-low",
                               perc_2006 < medias$media_06[1] & perc_2021 > medias$media_21[1] ~ "Improved",
                               perc_2006 > medias$media_06[1] & perc_2021 < medias$media_21[1] ~ "Decayed",
                               perc_2006 > medias$media_06[1] & perc_2021 > medias$media_21[1] ~ "Maintence-high"),
         nivel_complexidade = cut(eci_2006,
                                  breaks = intervalos,
                                  labels = c("Low", "Medium" ,"High"),
                                  include.lowest = TRUE))

ggplot(dados_grafico)+
  geom_jitter(aes(x = perc_2006, y = perc_2021, color = variation), 
              width = 0.2, 
              height = 0.2,
              size = 2,
              alpha = 0.8)+
  geom_textvline(label = paste0("avg = ", round(medias$media_06[1], 1)), 
                 xintercept = medias$media_06[1], hjust = 0.9, fontface = "bold") +
  geom_texthline(label = paste0("avg = ", round(medias$media_21[1], 1)), 
                 yintercept = medias$media_21[1], hjust = 0.9, fontface = "bold")+
  scale_color_manual(values = colors[c(2, 1, 4, 3)])+
  theme_gdocs()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal")+
  labs(y = "RCA count (2021)",
       x = "RCA count (2006)",
       color = NULL)+
  guides(colour = guide_legend(override.aes = list(size=5)))



# Relatedness Density and rca variation --------------------------------

dados_grafico <- dados_municipios %>% 
  filter(ano %in% as.character(seq(2006, 2021, 3)))


ggbetweenstats(data = dados_grafico,
               x = ano, 
               y = media_densidade,
               plot.type = "box",
               type = "nonparametric",
               pairwise.comparisons = FALSE)
  

dados_grafico <- data %>% 
  #filter(ano %in% as.character(c(2006, seq(2007, 2021, 2)))) %>% 
  filter(ano %in% as.character(c(2006, 2021))) %>% 
  group_by(ano, country, country_name, reg, ) %>% 
  summarise(densidade_media = mean(densidade, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_longer(cols = 5:last_col(),
               names_to = "variavel",
               values_to = "valor") %>%
  group_by(country, country_name, reg, variavel) %>% 
  mutate(variacao = valor-lag(valor)) %>% 
  ungroup() %>% 
  arrange(country, variavel, ano)
  




dados_grafico <- data %>% 
  filter(ano %in% as.character(c(2006, 2021))) %>% 
  group_by(ano, country, country_name, reg, n.rca.interesse, perc, n.empregos_tot_int, eci) %>% 
  summarise(densidade_media = mean(densidade, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = ano,
              values_from = c(densidade_media, n.rca.interesse, perc, n.empregos_tot_int, eci),
              values_fill = 0) %>% 
  mutate(var_densidade = densidade_media_2021-densidade_media_2006,
         n.rca.interesse_2006_2 = as.character(n.rca.interesse_2006),
         var_perc = perc_2021 - perc_2006,
         var_emp = n.empregos_tot_int_2021 - n.empregos_tot_int_2006)


ggplot(filter(dados_grafico, country_name != "São Paulo")) +
  geom_jitter(aes(x = reorder(n.rca.interesse_2006_2, n.rca.interesse_2006), y = log(var_emp)))+
  geom_smooth(aes(x = reorder(n.rca.interesse_2006_2, n.rca.interesse_2006), y = log(var_emp)))


ggplot(dados_grafico) +
  geom_jitter(aes(x = eci_2006, y = var_emp))+
  geom_smooth(aes(x = eci_2006, y = var_emp))+
  theme_gdocs()




# Variacao no emprego x nível complexidade --------------------------------





# CRESCIMENTO
dados_regioes <- dados_municipios %>%
  select(ano, country, reg, n.empregos_tot_int, perc, eci) %>% 
  filter(ano %in% c("2006", "2021")) 

dados_grafico <- dados_regioes %>%
  pivot_wider(names_from = ano,
              values_from = c(perc, eci, n.empregos_tot_int),
              values_fill = 0) %>%
  mutate(var_emp_abs = n.empregos_tot_int_2021 - n.empregos_tot_int_2006,
         var_emp = log(n.empregos_tot_int_2021) - log(n.empregos_tot_int_2006),
         var_eci_abs = eci_2021 - eci_2006,
         var_eci = ((eci_2021/eci_2006)-1)/eci_2006*100) %>% 
  filter(var_emp != Inf)
  
  
  


# Relação entre ECI e crescimento do emprego em tech ABSOLUTO
ggplot(dados_grafico)+
  geom_point(aes(y = log(var_emp_abs), x = eci_2006, color = reg))+
  geom_smooth(aes(y = log(var_emp_abs), x = eci_2006), method = "lm")+
  facet_wrap(~reg, scales = "free")



# Relação entre ECI e crescimento do emprego em tech - Relativo
ggplot(dados_grafico)+
  geom_point(aes(y = var_emp, x = eci_2006, color = reg))+
  geom_smooth(aes(y = var_emp, x = eci_2006), method = "lm")+
  facet_wrap(~reg, scales = "free")







# DECRESCIMENTO
dados_regioes <- dados_municipios %>%
  select(ano, country, reg, n.empregos_tot_int, perc, eci) %>% 
  filter(ano %in% c("2006", "2021")) 

dados_grafico <- dados_regioes %>%
  pivot_wider(names_from = ano,
              values_from = c(perc, eci, n.empregos_tot_int),
              values_fill = 0) %>%
  mutate(var_emp_abs = n.empregos_tot_int_2021 - n.empregos_tot_int_2006,
         var_emp = (((n.empregos_tot_int_2021/n.empregos_tot_int_2006)-1)/n.empregos_tot_int_2006)*100,
         var_eci_abs = eci_2021 - eci_2006,
         var_eci = ((eci_2021/eci_2006)-1)/eci_2006*100) %>% 
  filter(var_emp != Inf) %>% 
  mutate(var_emp_abs_index = var_emp_abs/(max(var_emp_abs) - min(var_emp_abs))) 
filter(var_emp_abs < 0)





# Relação entre ECI e crescimento do emprego em tech ABSOLUTO
ggplot(dados_grafico)+
  geom_point(aes(y = var_emp_abs_index, x = eci_2006, color = reg))+
  geom_smooth(aes(y = var_emp_abs_index, x = eci_2006), method = "lm")+
  facet_wrap(~reg, scales = "free")



# Relação entre ECI e crescimento do emprego em tech - Relativo
ggplot(dados_grafico)+
  geom_point(aes(y = log(var_emp), x = eci_2006, color = reg))+
  geom_smooth(aes(y = log(var_emp), x = eci_2006), method = "lm")+
  facet_wrap(~reg, scales = "free")



















medias <- dados_grafico %>% 
  summarise(media_06 = mean(var_emp, na.rm = TRUE),
            media_21 = mean(var_emp, na.rm = TRUE))



intervalos1 <- classIntervals(dados_grafico$eci_2006,
                              style = "quantile",
                              n = 3)$brks
intervalos2 <- classIntervals(dados_grafico$eci_2021,
                              style = "quantile",
                              n = 3)$brks


dados_grafico <- dados_grafico %>% 
  mutate(variation = case_when(perc_2006 < medias$media_06[1] & perc_2021 < medias$media_21[1] ~ "Maintence-low",
                               perc_2006 < medias$media_06[1] & perc_2021 > medias$media_21[1] ~ "Improved",
                               perc_2006 > medias$media_06[1] & perc_2021 < medias$media_21[1] ~ "Decayed",
                               perc_2006 > medias$media_06[1] & perc_2021 > medias$media_21[1] ~ "Maintence-high"),
         nivel_complexidade06 = cut(eci_2006,
                                    breaks = intervalos1,
                                    labels = c("Low", "Medium" ,"High"),
                                    include.lowest = TRUE),
         nivel_complexidade21 = cut(eci_2021,
                                    breaks = intervalos2,
                                    labels = c("Low", "Medium" ,"High"),
                                    include.lowest = TRUE))


ggbetweenstats(data = dados_grafico,
               x = eci_2006,
               y = log(var_emp_abs),
               plot.type = "box",
               type = "nonparametric",
               ggplot.component = ggplot2::scale_y_continuous(limits = (c(-100, max(dados_grafico$var_perc)))))








medias <- dados_grafico %>% 
  summarise(media_06 = mean(perc_2006, na.rm = TRUE),
            media_21 = mean(perc_2021, na.rm = TRUE))

intervalos1 <- classIntervals(dados_grafico$eci_2006,
                             style = "quantile",
                             n = 3)$brks
intervalos2 <- classIntervals(dados_grafico$eci_2021,
                             style = "quantile",
                             n = 3)$brks


dados_grafico <- dados_grafico %>% 
  mutate(variation = case_when(perc_2006 < medias$media_06[1] & perc_2021 < medias$media_21[1] ~ "Maintence-low",
                               perc_2006 < medias$media_06[1] & perc_2021 > medias$media_21[1] ~ "Improved",
                               perc_2006 > medias$media_06[1] & perc_2021 < medias$media_21[1] ~ "Decayed",
                               perc_2006 > medias$media_06[1] & perc_2021 > medias$media_21[1] ~ "Maintence-high"),
         nivel_complexidade06 = cut(eci_2006,
                                  breaks = intervalos1,
                                  labels = c("Low", "Medium" ,"High"),
                                  include.lowest = TRUE),
         nivel_complexidade21 = cut(eci_2021,
                                    breaks = intervalos2,
                                    labels = c("Low", "Medium" ,"High"),
                                    include.lowest = TRUE))

dados_brazil <- dados_grafico %>% 
  mutate(reg = "Brazil")

dados_grafico2 <- rbind(dados_grafico, dados_brazil)


ggplot(dados_grafico)+
  geom_point(aes(x = perc_2006, y = var_perc, color = reg, size = eci_2006))+
  geom_smooth(aes(x = perc_2006, y = var_perc), method = "lm")


ggbetweenstats(data = dados_grafico,
                       x = nivel_complexidade06,
                       y = var_perc,
                       plot.type = "box",
                       type = "nonparametric",
                       ggplot.component = ggplot2::scale_y_continuous(limits = (c(-100, max(dados_grafico$var_perc)))))





dados_grafico2 <- dados_grafico %>% 
  group_by(reg, variation) %>%
  summarise(n = n()) %>% 
  mutate(perc = n/sum(n)*100)
  
ggplot(dados_grafico2)+
  geom_col(aes(x = reg, y = perc, color = variation, fill = variation), position = "dodge")+
  facet_wrap(~reg, scales = "free_x")


ggplot(dados_grafico)+
  geom_jitter(aes(y = reg, 
                  x = var_perc,
                  color = reg), 
              height = 0.1, 
              size = 0.1,
              show.legend = FALSE)+
  geom_boxplot(aes(x = var_perc, y = reg, color = reg), fill = NA)
  facet_wrap(~reg, scales = "free_x")





  scale_color_manual(values = colors[c(2, 1, 4, 3)])+
  theme_gdocs()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal")+
  labs(y = "RCA count (2021)",
       x = "RCA count (2006)",
       color = NULL)






medias <- dados_grafico %>% 
  filter(ano %in% c("2006", "2021") & perc > 0 & perc < quantile(perc, 0.99, names = FALSE)) %>% 
  group_by(ano, ano3, reg) %>% 
  summarise(media = mean(perc, na.rm = TRUE)) %>% 
  ungroup()


dados_grafico %>% 
  ggplot()+
  geom_jitter(aes(y = reorder(ano, -ano3), 
                  x = perc,
                  color = ano), 
              height = 0.1, 
              size = 0.1,
              show.legend = FALSE)+
  geom_boxplot(aes(y = reorder(ano, -ano3), 
                   x = perc,
                   color = ano), 
               fill = NA, 
               alpha = 0.6, 
               outlier.shape = NA,
               show.legend = FALSE)+
  geom_text_repel(data = filter(medias, ano == "2006"), 
                  aes(x = media,
                      y = reorder(ano, -ano3),
                      label = round(media, 3),
                      color = ano),
                  nudge_y = 0.7,
                  show.legend = FALSE)+
  geom_text_repel(data = filter(medias, ano == "2021"), 
                  aes(x = media,
                      y = reorder(ano, -ano3),
                      label = round(media, 3),
                      color = ano),
                  nudge_y = -0.7,
                  show.legend = FALSE)+
  geom_point(data = medias,
             aes(x = media,
                 y = reorder(ano, -ano3)),
             shape=20, 
             size=5, 
             color="red", 
             fill="red") +
  scale_color_manual(values = colors[c(1, 4)])+
  theme_gdocs()+
  labs(x = "Employment share (percentage)",
       y = NULL)+
  facet_wrap(~reg)

#ggsave("3_results/analysis/empolyment_share_boxplot_v.png", plot = last_plot(),
#       height = 30, width = 15, units = "cm", dpi = 300)

ggsave("3_results/analysis/empolyment_share_boxplot.png", plot = last_plot(),
       height = 20, width = 30, units = "cm", dpi = 300)



# Testando deslocamento espacial


shape_sedes <- geobr::read_municipal_seat() %>% 
  mutate(code_muni = as.character(code_muni),
         reg = case_when(
           code_region == "1" ~ "North",
           code_region == "2" ~ "Northeast",
           code_region == "3" ~ "Southeast",
           code_region == "4" ~ "South",
           code_region == "5" ~ "Midwest"),
         reg = factor(reg,
                      levels = c(
                        "North",
                        "Northeast",
                        "Southeast",
                        "South",
                        "Midwest"
                      )))

coords_sedes <- shape_sedes %>% 
  mutate(coordx = map_dbl(geom, 1),
         coordy = map_dbl(geom, 2)) %>%
  st_drop_geometry() %>% 
  select(code_muni, coordx, coordy)
  

dados_emprego2 <- dados_municipios %>% 
  group_by(ano, reg) %>% 
  mutate(total = sum(n.empregos_tot_int)) %>% 
  ungroup() %>% 
  mutate(perc_emp = n.empregos_tot_int/total) %>% 
  select(ano, country, reg, perc_emp) %>% 
  left_join(coords_sedes, by = c("country" = "code_muni")) %>% 
  mutate(coordx_pond = perc_emp*coordx,
         coordy_pond = perc_emp*coordy) %>% 
  group_by(ano, reg) %>% 
  summarise(coordx = sum(coordx_pond, na.rm = TRUE),
            coordy = sum(coordy_pond, na.rm = TRUE)) %>% 
  ungroup()
  

ggplot()+
  geom_sf(data = shape_estados)+
  geom_point(data = dados_emprego2, aes(x = coordx, y = coordy))+
  facet_wrap(~reg)




teste <- dados_emprego %>% 
  group_by(ano) %>% 
  summarise(t = sum(perc_emp))

ggplot(shape_sedes2)+
  geom_point(aes(x = coordx, y = coordy))



# Employment map --------------------------------

selecionado <- dados_municipios %>% 
  filter(ano %in% c("2006", "2021")) 

intervalos <- classIntervals(selecionado$n.empregos_tot_int,
                             style = "fixed",
                             fixedBreaks = c(0, 10, 30, 100, 500, 2000, max(selecionado$n.empregos_tot_int)))$brks
  
  
selecionado <-selecionado %>% 
  mutate(emp.intervals = cut(n.empregos_tot_int,
                             breaks = intervalos,
                             labels = c("0-9", "10-29" ,"30-99", "100-500", "500-2,000", "2,000-55,000"),
                             include.lowest = TRUE))

shape_muns2 <- shape_muns %>% 
  left_join(selecionado,
            by = c("code_muni" = "country")) %>% 
  filter(!is.na(ano))
  
  

mapa <- ggplot()+
  geom_sf(data = shape_muns2, 
          aes(fill = emp.intervals),
          color = NA)+
  geom_sf(data = shape_estados,
          fill = NA,
          color = "grey70")+
  geom_sf(data = shape_macros,
          fill = NA,
          color = "black")+
  scale_fill_brewer(palette = "Blues")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 14),
        text = element_text(size = 12),
        legend.title = element_text(size = 12),
        title = element_text(size = 16),
        legend.position = "bottom")+
  labs(fill = "Employment \ncount",
       title = str_c("Digital techs employment count"))+
  facet_wrap(~ano)


ggsave("3_results/analysis/mapa_emprego_.png",
       plot = mapa,
       height = 20,
       width = 30,
       units = "cm",
       dpi = 300)























# Averiguando porto velho ---------------------------------------------------------------------


vinculos <- fread("1_dados/RAIS_VINC_ID_NORTE.txt", nrows = Inf, encoding = "Latin-1",
                  sep = ";", dec = ",",
                  select = c("Município",
                             "Vínculo Ativo 31/12",
                             "CPF",
                             "CEI Vinculado",
                             "CNPJ / CEI",
                             "CNPJ Raiz",
                             "CBO Ocupação 2002"),
                  colClasses = c(Município = "character",
                                 `CBO Ocupação 2002` = "character",
                                 `CEI Vinculado` = "character",
                                 `CNPJ / CEI` = "character",
                                 `CNPJ Raiz`  = "character" 
                                 )) %>% 
  janitor::clean_names()




vinculos_pv <- vinculos %>% 
  filter(municipio == "110020" & vinculo_ativo_31_12 == 1 & cbo_ocupacao_2002 == "317110")

a <- vinculos_pv %>% 
  group_by(cbo_ocupacao_2002, cnpj_cei) %>% 
  summarise(n = n())



  
  


n.sample <- length(unique(data$country))
random <- sample(1:length(unique(data$country)), n.sample)
countries_random <- unique(data$country)[random]

data_random <- data %>% 
    filter(country %in% countries_random & n.rca.interesse > 0)

a <- data_random %>% 
    select(ano2, ano3, country, reg, n.rca.interesse) %>%
    distinct()
    

plt <- ggplot(data = a)+
    geom_jitter(aes(y = reorder(ano2, -ano3), x = n.rca.interesse),
                alpha = 0.4,
                height = 0.1,
                size = 0.9)+
    geom_boxplot(aes(y = reorder(ano2, -ano3), x = n.rca.interesse), 
                 fill = NA,
                 linewidth = 1,
                 outlier.shape = NA)+
    facet_wrap(~reg, ncol = 1)+
    theme_gdocs()+
    labs(x = "RCA count",
         y = NULL)

ggsave("3_results/analysis/rca_count_boxplot.png",
       plot = plt,
       height = 40, width = 20, units = "cm")



ggstatsplot::grouped_ggbetweenstats(data = a,
                                    x = ano, 
                                    y = n.rca.interesse, 
                                    grouping.var = reg,
                                    pairwise_comparisons = FALSE,
                                    centrality.plotting = FALSE,
                                    package = "palettesForR",
                                    palette = "Gray",
                                    point.args = list(size = 0.1),
                                    violin.args = list(width = 0),
                                    type = "p")

    ggplot()+
    geom_histogram(aes(x = n.rca.interesse, fill = reg))
    
    
    
    

# Maps --------------------------------------------------------------------

  
    
    

# Maps of digital techs occupation employment  
shape_muns2 <- shape_muns %>% 
    left_join(emp_cbos_interesse_k1, by = c("code_muni" = "country"))  %>% 
    mutate(total_k1 = ifelse(is.na(perc),
                             yes = 0,
                             no = perc))

mapa <- ggplot()+
    geom_sf(data = shape_muns2, 
            aes(fill = perc),
            color = NA)+
    geom_sf(data = shape_estados,
            fill = NA,
            color = "grey70")+
    geom_sf(data = shape_macros,
            fill = NA,
            color = "black")+
    scale_fill_gradient(low = colors[3],
                        high = colors[1])+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_blank(),
          text = element_text(size = 12),
          legend.title = element_text(size = 12),
          title = element_text(size = 16),
          legend.position = "bottom")+
    labs(fill = "Employment \nshare",
         title = str_c("Digital techs employment share (", anos[k], ")"))

ggsave(str_c("3_results/mapas_analise/mapa_participacao_emprego_", anos[k], ".png"),
       plot = mapa,
       height = 30,
       width = 20,
       units = "cm",
       dpi = 300)



shape_muns2 <- shape_muns %>% 
    left_join(emp_cbos_interesse_k2, by = c("code_muni" = "country"))  %>% 
    mutate(total_k1 = ifelse(is.na(perc),
                             yes = 0,
                             no = perc))

mapa <- ggplot()+
    geom_sf(data = shape_muns2, 
            aes(fill = perc),
            color = NA)+
    geom_sf(data = shape_estados,
            fill = NA,
            color = "grey70")+
    geom_sf(data = shape_macros,
            fill = NA,
            color = "black")+
    scale_fill_gradient(low = colors[3],
                        high = colors[1])+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_blank(),
          text = element_text(size = 12),
          legend.title = element_text(size = 12),
          title = element_text(size = 16),
          legend.position = "bottom")+
    labs(fill = "Employment \nshare",
         title = str_c("Digital techs employment share (", anos_k2[k], ")"))

ggsave(str_c("3_results/mapas_analise/mapa_participacao_emprego_", anos_k2[k], ".png"),
       plot = mapa,
       height = 30,
       width = 20,
       units = "cm",
       dpi = 300)

















# RCA counts maps
shape_muns2 <- shape_muns %>% 
    left_join(n.rca.interesse, by = c("code_muni" = "country"))  %>% 
    mutate(total_k1 = ifelse(is.na(total_k1),
                             yes = 0,
                             no = total_k1),
           total_k2 = ifelse(is.na(total_k2),
                             yes = 0,
                             no = total_k2))

mapa <- ggplot()+
    geom_sf(data = shape_muns2, 
            aes(fill = total_k1),
            color = NA)+
    geom_sf(data = shape_estados,
            fill = NA,
            color = "grey70")+
    geom_sf(data = shape_macros,
            fill = NA,
            color = "black")+
    scale_fill_gradient(low = colors[3],
                        high = colors[4])+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_blank(),
          text = element_text(size = 12),
          legend.title = element_text(size = 12),
          title = element_text(size = 16),
          legend.position = "bottom")+
    labs(fill = "RCA\ncount",
         title = str_c("RCA count on digital techs occupations (", anos[k], ")"))

ggsave(str_c("3_results/mapas_analise/mapa_rca_count_", anos[k], ".png"),
       plot = mapa,
       height = 30,
       width = 20,
       units = "cm",
       dpi = 300)


mapa <- ggplot()+
    geom_sf(data = shape_muns2, 
            aes(fill = total_k2),
            color = NA)+
    geom_sf(data = shape_estados,
            fill = NA,
            color = "grey70")+
    geom_sf(data = shape_macros,
            fill = NA,
            color = "black")+
    scale_fill_gradient(low = colors[3],
                        high = colors[4])+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_blank(),
          text = element_text(size = 12),
          legend.title = element_text(size = 12),
          title = element_text(size = 16),
          legend.position = "bottom")+
    labs(fill = "RCA\ncount",
         title = str_c("RCA count on digital techs occupations (", anos_k2[k], ")"))

ggsave(str_c("3_results/mapas_analise/mapa_rca_count_", anos_k2[k], ".png"),
       plot = mapa,
       height = 30,
       width = 20,
       units = "cm",
       dpi = 300)






# Variation map
var.rca.interesse <- rca_var %>% 
    filter(cbo_interesse == 1) %>% 
    group_by(country) %>% 
    summarise(var_cbo_interesse = sum(var, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(country)


shape_muns2 <- shape_muns %>% 
    left_join(var.rca.interesse, by = c("code_muni" = "country")) %>% 
    filter(!is.na(var_cbo_interesse))



mapa <- ggplot()+
    geom_sf(data = shape_muns2, 
            aes(fill = var_cbo_interesse),
            color = NA)+
    geom_sf(data = shape_estados,
            fill = NA,
            color = "grey70")+
    geom_sf(data = shape_macros,
            fill = NA,
            color = "black")+
    scale_fill_gradient2(low = colors[2],
                         mid = colors[3],
                         high = colors[4],
                         midpoint = 0)+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_blank(),
          text = element_text(size = 12),
          legend.title = element_text(size = 12),
          title = element_text(size = 16),
          legend.position = "bottom")+
    labs(fill = "RCA\ncount",
         title = str_c("RCA variation on digital techs occupations (", anos[k], "-", anos_k2[k], ")"))

ggsave(str_c("3_results/mapas_analise/mapa_variacao_cbos_interesse_", anos[k], ".png"),
       plot = mapa,
       height = 30,
       width = 20,
       units = "cm",
       dpi = 300)



