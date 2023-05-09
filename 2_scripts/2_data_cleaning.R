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

# Reading data ------------------------------------------------------------


files <- list.files(path = "C:/backup_arquivos/RAIS/dados_rais_cbo_06_2021", pattern = ".RDS")

lista_dados <- list()
i = files[1]

for(i in files){
    
    lista_dados[[i]] <- read_rds(paste0("C:/backup_arquivos/RAIS/dados_rais_cbo_06_2021/", i))
    
}

dados <- do.call(rbind, lista_dados)

cbos <- fread("1_dados/cbo_2002.csv", colClasses = c(CODIGO = "character")) %>% 
    rename(cbo_2002 = CODIGO, 
           cbo_desc = TITULO)


dados <- dados %>% 
    left_join(cbos) %>% 
    select(ano, sigla_uf, id_municipio, cbo_2002, cbo_desc, qtd_vinc) %>% 
    filter(!is.na(cbo_2002))


anos <- unique(dados$ano)

for (i in anos){
  
  df <- dados %>% 
    filter(ano == i & cbo_2002 != "")
  
  saveRDS(df, paste0("1_dados/emprego_municipio_cbo_", i, ".RDS"))
  
}







