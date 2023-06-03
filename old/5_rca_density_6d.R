#### Sase article - Preparing data for analysis
#### Sase article - Preparing data for analysis
#### Sase article - Preparing data for analysis

# Start: 19/05/2023
# Author: Alexandre Stein



# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)
library(economiccomplexity)
library(EconGeo)
library(RCy3)
library(stringi)
library(dtplyr)


setwd("C:/Users/queir/Meu Drive/Economia/Outros/Gustavo Britto/sase_2023_rio/digital_techs")

rm(list = ls(all = TRUE))

# EconGeo Functions modified 

relatedness.density.int_mod <- function (mat, relatedness) 
{
    rel <- mat %*% relatedness
    reltot <- colSums(relatedness)
    reldens <- t(rel)/reltot
    reldens <- t(reldens) * 100
    reldens <- round(reldens, digits = 2) #modificamos a fun??o para deixar 2 d?gitos e n?o arredondar
    mat[mat == 0] <- NA
    reldensint <- reldens * mat
    return(reldensint)
}


relatedness.density.ext_mod <- function (mat, relatedness) 
{
    rel <- mat %*% relatedness
    reltot <- colSums(relatedness)
    reldens <- t(rel)/reltot
    reldens <- t(reldens) * 100
    reldens <- round(reldens, digits = 2) #modificamos a fun??o para deixar 2 d?gitos e n?o arredondar
    mat[mat == 1] <- NA
    mat[mat == 0] <- 1
    reldensext <- reldens * mat
    return(reldensext)
}




# Side data -----------------------------------------------------------

colors <- c("#2D3A4E", "#798594", "#D9D5C7","#E6673D")



municipios <- read.csv("C:/Users/queir/Meu Drive/Economia/Outros/Tabelas de Compatibilizacoes/municipio.csv") %>%
    select(id_municipio, nome) %>%
    mutate(id_municipio = as.character(id_municipio))

cbos <- fread("C:/Users/queir/Meu Drive/Economia/Outros/Tabelas de Compatibilizacoes/cbo_2002.csv",
              colClasses = list(character = 1:10)) %>%
    select(cbo_2002, descricao) %>% 
    mutate(cbo_2002 = as.character(cbo_2002))



# Selecting CBOS --------------------------------------------------------------------

palavras_chave <- c("digital", "programacao", "programador", "software", "hardware", 
                    "computacao", "computador", "computadores", "desenvolvedor", "desenvolvimento web", "web", 
                    "administrador de redes", "\\sdados", "internet", "data")

palavras_chave <- str_c(palavras_chave, collapse = "|")

cbos_interesse <- cbos %>% 
  mutate(descricao = tolower(descricao),
         descricao = stri_trans_general(str = descricao, 
                                        id = "Latin-ASCII"),
         interesse = ifelse(str_detect(descricao, 
                                       palavras_chave),
                            yes = 1,
                            no = 0)) %>% 
  filter(interesse == 1 & (!descricao %in% c("tecnico de planejamento e programacao da manutencao",
                                                            "analista de pcp (programacao e controle da producao)"))) %>% 
  pull(cbo_2002)


# Data --------------------------------------------------------------------

files_raw <- list.files(path = "1_dados", pattern = ".RDS")

files <- files_raw[1:length(files_raw)-1]
files_k2 <- files_raw[2:length(files_raw)]

anos <- as.character((2006:2020))
anos_k2 <- as.character((2007:2021))

lista_densidades <- list()
lista_empregos_total <- list()
lista_empregos_cbo_interesse <- list()
lista_rca <- list()
lista_rcanb <- list()
lista_rca_var <- list()
lista_qtd.rca.interesse <- list()

for (k in seq_along(files)){
  
  print(anos[k])
  print(anos_k2[k])
  
        # Preparing basic data  
  
        dados <- readRDS(paste0("1_dados/", files[k]))
        dados_k2 <- readRDS(paste0("1_dados/", files_k2[k]))
        
        
        data <- dados %>%
            select(id_municipio, cbo_2002, qtd_vinc) %>%
            rename(country = id_municipio,
                   product = cbo_2002,
                   value = qtd_vinc) %>%
            pivot_wider(names_from = product,
                        values_from = value,
                        values_fill = 0) %>%
            {.->> data_mat} %>%
            pivot_longer(cols = 2:last_col(),
                         values_to = "value",
                         names_to = "product")
        

        
        data_k2 <- dados_k2 %>%
            select(id_municipio, cbo_2002, qtd_vinc) %>%
            rename(country = id_municipio,
                   product = cbo_2002,
                   value = qtd_vinc) %>%
            pivot_wider(names_from = product,
                        values_from = value,
                        values_fill = 0) %>%
            pivot_longer(cols = 2:last_col(),
                         names_to = "product",
                         values_to = "value") %>%
            pivot_wider(names_from = product,
                        values_from = value,
                        values_fill = 0) %>%
            {.->> data_mat_k2} %>%
            pivot_longer(cols = 2:last_col(),
                         values_to = "value",
                         names_to = "product")
        

        data_mat <- data_mat %>%
            column_to_rownames(var = "country")
        
        nomes <- list(names1 = rownames(data_mat),
                      names2 = colnames(data_mat))
        
        
        data_mat <- as.matrix(x = data_mat)
        dimnames(data_mat) <- nomes
        
        
        data_mat_k2 <- data_mat_k2 %>%
            column_to_rownames(var = "country")
        
        
        nomes <- list(names1 = rownames(data_mat_k2),
                      names2 = colnames(data_mat_k2))
        
        
        data_mat_k2 <- as.matrix(x = data_mat_k2)
        dimnames(data_mat_k2) <- nomes
        
        
        
        
        # Digital techs occupation employment
        
        tot_emp_k1 <- data %>% 
            group_by(country) %>% 
            summarise(n.empregos_tot = sum(value, na.rm = TRUE)) %>% 
            ungroup()
        
        
        if (anos_k2[k] == "2021"){
        tot_emp_k2 <- data_k2 %>% 
            group_by(country) %>% 
            summarise(n.empregos_tot = sum(value, na.rm = TRUE)) %>% 
            ungroup()
        }
        
        data %>%
            mutate(value = ifelse(product %in% cbos_interesse,
                                  yes = value,
                                  no = 0)) %>% 
            group_by(country) %>%
            summarise(n.empregos_tot_int = sum(value, na.rm = TRUE)) %>%
            ungroup() %>% 
            left_join(tot_emp_k1) %>% 
            mutate(perc = n.empregos_tot_int/n.empregos_tot*100,
                   reg = substr(country, 1, 1),
                   reg = case_when(reg == 1 ~ "North",
                                   reg == 2 ~ "Northeast",
                                   reg == 3 ~ "Southeast",
                                   reg == 4 ~ "South",
                                   reg == 5 ~ "Midwest"),
                   ano = anos[k]) %>%
          select(ano, country, reg, n.empregos_tot, n.empregos_tot_int, perc)->
          lista_empregos_total[[anos[k]]]
        
        
        if (anos_k2[k] == "2021"){
        data_k2 %>%
            mutate(value = ifelse(product %in% cbos_interesse,
                                  yes = value,
                                  no = 0)) %>% 
            group_by(country) %>%
            summarise(n.empregos_tot_int = sum(value, na.rm = TRUE)) %>%
            ungroup() %>% 
            left_join(tot_emp_k2) %>% 
            mutate(perc = n.empregos_tot_int/n.empregos_tot*100,
                   reg = substr(country, 1, 1),
                   reg = case_when(reg == 1 ~ "North",
                                   reg == 2 ~ "Northeast",
                                   reg == 3 ~ "Southeast",
                                   reg == 4 ~ "South",
                                   reg == 5 ~ "Midwest"),
                   ano = anos_k2[k]) %>% 
          select(ano, country, reg, n.empregos_tot, n.empregos_tot_int, perc) ->
          lista_empregos_total[[anos_k2[k]]]
        }
        
        
        # Saving employement by occupation data for later boxplot
        
        data %>%
          filter(product %in% cbos_interesse) %>% 
          mutate(ano = anos[k]) %>% 
          rename(n.empregos = value)->
          lista_empregos_cbo_interesse[[anos[k]]]
        
        if (anos_k2[k] == "2021"){
        data_k2 %>%
          filter(product %in% cbos_interesse) %>% 
          mutate(ano = anos_k2[k]) %>% 
            rename(n.empregos = value)->
          lista_empregos_cbo_interesse[[anos_k2[k]]]
        }
        
        
        
        # RCAs K1
        
        mat.rca <- location.quotient(mat = data_mat, binary = TRUE)
        
        mat.rca_long <- mat.rca %>%
            as.data.frame() %>%
            rownames_to_column(var = "country") %>%
            pivot_longer(cols = 2:last_col(),
                         values_to = "rca_k1",
                         names_to = "product")
        
        
        coocorrencias <- co.occurrence(mat = t(mat.rca), diagonal = TRUE) #c?lculo da coocorr?ncias - exige a matriz transposta
        
        proximidades <- relatedness(mat = coocorrencias, method = "cosine")
        
        mcp <- Matrix(data = mat.rca, sparse = TRUE)
        
        
        location.quotient(mat = data_mat, binary = FALSE) %>% 
          as.data.frame() %>%
          rownames_to_column(var = "country") %>%
          pivot_longer(cols = 2:last_col(),
                       values_to = "rcanb",
                       names_to = "product") %>% 
          filter(product %in% cbos_interesse) -> lista_rcanb[[anos[k]]]
        
        # RCAs K2
        
        mat.rca_k2 <- location.quotient(mat = data_mat_k2, binary = TRUE)
        
        mat.rca_long_k2 <- mat.rca_k2 %>%
            as.data.frame() %>%
            rownames_to_column(var = "country") %>%
            pivot_longer(cols = 2:last_col(),
                         values_to = "rca_k2",
                         names_to = "product") 
        
        
        coocorrencias_k2 <- co.occurrence(mat = t(mat.rca_k2), diagonal = TRUE) #c?lculo da coocorr?ncias - exige a matriz transposta
        
        proximidades_k2 <- relatedness(mat = coocorrencias_k2, method = "cosine")
        
        mcp_k2 <- Matrix(data = mat.rca_k2, sparse = TRUE)
        

        
        
        if (anos_k2[k] == "2021"){
          
          location.quotient(mat = data_mat_k2, binary = FALSE) %>% 
            as.data.frame() %>%
            rownames_to_column(var = "country") %>%
            pivot_longer(cols = 2:last_col(),
                         values_to = "rcanb",
                         names_to = "product") %>% 
            filter(product %in% cbos_interesse) -> lista_rcanb[[anos_k2[k]]]
        }
        
        

        # RCA Variation computation
        rca_var <- mat.rca_long %>% 
            left_join(mat.rca_long_k2) %>% 
            mutate(rca_k2 = ifelse(is.na(rca_k2),
                                   yes = 0,
                                   no = rca_k2),
                   var = case_when(rca_k1 == 0 & rca_k2 == 0 ~ 0,
                                   rca_k1 == 1 & rca_k2 == 1 ~ 0,
                                   rca_k1 == 0 & rca_k2 == 1 ~ 1,
                                   rca_k1 == 1 & rca_k2 == 0 ~ -1),
                   cbo_interesse = ifelse(product %in% cbos_interesse,
                                        yes = 1, 
                                        no = 0)) 
        # Saving RCA variation
        rca_var %>%
          filter(cbo_interesse == 1) %>% 
          mutate(ano = anos_k2[k]) %>% 
          select(ano, country, product, var) ->
          lista_rca_var[[anos_k2[k]]]
        
        
        # Saving RCA
        rca_var %>% 
          filter(cbo_interesse == 1) %>% 
          mutate(ano = anos[k]) %>% 
          select(ano, country, product, rca_k1) %>% 
          rename(rca = rca_k1) ->
          lista_rca[[anos[k]]]
        
        
        if (anos_k2[k] == "2021"){
          rca_var %>% 
            filter(cbo_interesse == 1) %>% 
            mutate(ano = anos_k2[k]) %>% 
            select(ano, country, product, rca_k2) %>% 
            rename(rca = rca_k2) ->
            lista_rca[[anos_k2[k]]]
          
        }
        
        
        # RCA counts maps
        n.rca.interesse <- rca_var %>% 
            filter(cbo_interesse == 1) %>% 
            group_by(country) %>% 
            summarise(total_k1 = sum(rca_k1, na.rm = TRUE),
                      total_k2 = sum(rca_k2, na.rm = TRUE)) %>% 
            ungroup()
        
        
        n.rca.interesse %>% 
          select(country, total_k1) %>% 
          mutate(ano = anos[k]) %>% 
          select(ano, country, total_k1) %>% 
          rename(n.rca.interesse = total_k1) ->
          lista_qtd.rca.interesse[[anos[k]]]
        
        if (anos_k2[k] == "2021"){
          
          n.rca.interesse %>% 
              select(country, total_k2) %>% 
              mutate(ano = anos_k2[k]) %>% 
              select(ano, country, total_k2) %>% 
              rename(n.rca.interesse = total_k2) ->
              lista_qtd.rca.interesse[[anos_k2[k]]]
            
        }
          

        # General density between regions and occupations
        as.data.frame(relatedness.density(mat = mat.rca, relatedness = proximidades)) %>%
          rownames_to_column(var = "country") %>%
          pivot_longer(cols = c(2:last_col()),
                       names_to = "product",
                       values_to = "densidade") %>% 
          mutate(ano = anos[k],
                 value = densidade/100) %>% #dividindo por 100 para ficar entre 0 e 1 e manter coerência com outras medidas feitas
          select(ano, country, product, densidade) %>% 
          filter(product %in% cbos_interesse) ->
          lista_densidades[[anos[k]]]
        
        
        
        if (anos_k2[k] == "2021"){
          
          as.data.frame(relatedness.density(mat = mat.rca_k2, relatedness = proximidades_k2)) %>%
            rownames_to_column(var = "country") %>% 
            pivot_longer(cols = c(2:last_col()),
                         names_to = "product",
                         values_to = "densidade") %>% 
            mutate(ano = anos_k2[k],
                   value = densidade/100) %>% #dividindo por 100 para ficar entre 0 e 1 e manter coerência com outras medidas feitas
            select(ano, country, product, densidade) %>% 
            filter(product %in% cbos_interesse) ->
            lista_densidades[[anos_k2[k]]]
          
          }
        
        
   
}




# Binding all densities and employment data along years

densidades <- do.call(rbind, lista_densidades)
empregos_total <- do.call(rbind, lista_empregos_total)
empregos_interesse <- do.call(rbind, lista_empregos_cbo_interesse)
qtd.rca.interesse <- do.call(rbind, lista_qtd.rca.interesse)
rca <- do.call(rbind, lista_rca)
rcanb <- do.call(rbind, lista_rcanb)
rca.var <- do.call(rbind, lista_rca_var)


saveRDS(empregos_total, "3_results/emprego_digital_techs_06_21.RDS")
saveRDS(empregos_interesse, "3_results/emprego_digital_techs_interesse_06_21.RDS")
saveRDS(densidades, "3_results/densidades_RCA_digital_techs_06_21.RDS")
saveRDS(qtd.rca.interesse, "3_results/qtd_RCA_digital_techs_06_21.RDS")
saveRDS(rca, "3_results/rca_digital_techs_06_21.RDS")
saveRDS(rcanb, "3_results/rcaNB_digital_techs_06_21.RDS")
saveRDS(rca.var, "3_results/rca_var_digital_techs_06_21.RDS")


#rm(list=ls(all = TRUE))



densidades <- readRDS("3_results/densidades_RCA_digital_techs_06_21.RDS") %>% 
  ungroup()
empregos_total <- read_rds("3_results/emprego_digital_techs_06_21.RDS") %>% 
  ungroup()
empregos_interesse <- read_rds("3_results/emprego_digital_techs_interesse_06_21.RDS") %>% 
  ungroup()
qtd.rca.interesse <- readRDS("3_results/qtd_RCA_digital_techs_06_21.RDS") %>% 
  ungroup()
rca <- readRDS("3_results/rca_digital_techs_06_21.RDS") %>% 
  ungroup()
rcanb <- readRDS("3_results/rcaNB_digital_techs_06_21.RDS") %>% 
  ungroup()
rca.var <- readRDS("3_results/rca_var_digital_techs_06_21.RDS") %>% 
  ungroup()



# Loading ECI data

arquivos <- list.files(path = "3_results/",
                       pattern = "ECI_20+",
                       full.names = TRUE)

ECI <- map_dfr(arquivos, read_rds) %>% 
  ungroup()

arquivos <- list.files(path = "3_results/",
                       pattern = "PCI_20+",
                       full.names = TRUE)

PCI <- map_dfr(arquivos, readRDS) %>% 
  ungroup()




setDT(densidades)
setDT(empregos_total)
setDT(empregos_interesse)
setDT(qtd.rca.interesse)
setDT(rca.var)
setDT(rca)
setDT(rcanb)
setDT(ECI)
setDT(PCI)


setkeyv(x = densidades, cols = c("ano", "country", "product"))
setkeyv(x = rca, cols = c("ano", "country", "product"))
setkeyv(x = rcanb, cols = c("ano", "country", "product"))
setkeyv(x = rca.var, cols = c("ano", "country", "product"))
setkeyv(x = empregos_interesse, cols = c("ano", "country", "product"))
setkeyv(x = empregos_total, cols = c("ano", "country"))
setkeyv(x = qtd.rca.interesse, cols = c("ano", "country"))
setkeyv(x = ECI, cols = c("ano", "country"))
setkeyv(x = PCI, cols = c("ano", "product"))


dados <- densidades %>% 
  merge.data.table(rca.var, all.x = TRUE, all.y = FALSE) %>%
  merge.data.table(rca, all.x = TRUE, all.y = FALSE) %>%
  merge.data.table(rcanb, all.x = TRUE, all.y = FALSE) %>%
  merge.data.table(empregos_interesse, all.x = TRUE, all.y = FALSE) %>%
  merge.data.table(empregos_total, all.x = TRUE, all.y = FALSE) %>%
  merge.data.table(qtd.rca.interesse, all.x = TRUE, all.y = FALSE, allow.cartesian = TRUE) %>% 
  merge.data.table(ECI, all.x = TRUE, all.y = FALSE)

setkeyv(dados, cols = c("ano", "product"))

dados <- dados %>%
  merge.data.table(PCI, all.x = TRUE, all.y = FALSE)









dados <- dados %>% 
  select(ano, country, nome, reg, product, descricao, n.empregos, n.empregos_tot_int, n.empregos_tot, perc,
         rca, rcanb, var, diversificacao, eci, ubiquidade, pci, n.rca.interesse, densidade) %>% 
  rename(country_name = nome, 
         product_desc = descricao) %>% 
  arrange(ano, country, product)



saveRDS(dados, "3_results/dados_digital_techs.RDS")
















































