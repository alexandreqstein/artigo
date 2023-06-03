#### Sase article - data cleaning
#### Sase article - data cleaning
#### Sase article - data cleaning

# Start: 09/05/2023
# Author: Alexandre Stein



# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)
options(scipen = 999L)
rm(list = ls(all= TRUE))

here::here()


# Reading data ------------------------------------------------------------


files <- list.files(path = "C:/backup_arquivos/RAIS/dados_rais_cbo__cnae_06_2021", pattern = ".RDS")

lista_dados <- list()

for(i in files){
    
    lista_dados[[i]] <- read_rds(paste0("C:/backup_arquivos/RAIS/dados_rais_cbo__cnae_06_2021/", i))
    
}

dados <- do.call(rbind, lista_dados)


dados <- dados %>%
  select(ano, sigla_uf, id_municipio, cbo_2002, cnae_2_subclasse, qtd_vinc) %>% 
  filter(!is.na(cbo_2002) & !is.na(cnae_2_subclasse) & id_municipio != "1100205") %>%
  rename(cnae = cnae_2_subclasse)
  



anos <- unique(dados$ano)

for (i in anos){
  
  df <- dados %>% 
    filter(ano == i & cbo_2002 != "" & cnae != "")
  
  saveRDS(df, paste0("1_dados/emprego_municipio_cbo_cnae", i, ".RDS"))
  
}







