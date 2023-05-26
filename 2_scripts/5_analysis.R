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

shape_estados <- geobr::read_state()
shape_macros <- geobr::read_region()



# Data ----------------------------------------------------------------------------------------

data <- readRDS("3_results/dados_digital_techs.RDS") %>% 
    mutate(reg = factor(reg,
                        levels = c(
                            "North",
                            "Northeast",
                            "Southeast",
                            "South",
                            "Midwest"
                                   )),
           ano2 = str_sub(ano, 3, 4),
           ano3 = as.integer(ano2)) %>% 
  filter(country != "1100205")

dados_municipios <- data %>% 
  select(ano, country, country_name, reg, n.empregos_tot_int, n.empregos_tot, perc, n.rca.interesse) %>% 
  distinct()

# Employment map

selecionado <- dados_municipios %>% 
  filter(!is.na(ano) & ano %in% c("2006", "2021")) 

intervalos <- classIntervals(selecionado$n.empregos_tot_int,
                             style = "fixed",
                             fixedBreaks = c(0, 10, 30, 100, 500, 2000, max(selecionado$n.empregos_tot_int)))$brks
  
  
selecionado <-selecionado %>% 
  mutate(emp.intervals = cut(n.empregos_tot_int,
                             breaks = intervalos,
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
        text = element_text(size = 12),
        legend.title = element_text(size = 12),
        title = element_text(size = 16),
        legend.position = "bottom")+
  labs(fill = "Employment count",
       title = str_c("Digital techs employment"))+
  facet_wrap(~ano)




ggsave("3_results/analysis/mapa_emprego_.png",
       plot = mapa,
       height = 20,
       width = 30,
       units = "cm",
       dpi = 300)



ggplot(data = selecionado)+
  geom_jitter(aes(x = ano, y = log(n.empregos_tot_int+1)), width = 0.05)+
  geom_boxplot(aes(x = ano, y = log(n.empregos_tot_int+1), color = reg), fill = NA, outlier.shape = NA)+
  facet_wrap(~reg)


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



