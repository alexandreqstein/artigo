#### Sase article - Data Analysis
#### Sase article - Data Analysis
#### Sase article - Data Analysis

# Start: 19/05/2023
# Author: Alexandre Stein



# R options and libraries -------------------------------------------------
options(scipen = 999L)

library(tidyverse)
library(data.table)
library(foreach)
library(doParallel)
library(dtplyr)

rm(list = ls(all = TRUE))

# Side data -----------------------------------------------------------

cbos_interesse <- read.csv2("ocupacoes_interesse_6d.csv", colClasses = "character") %>% 
    pull(cbo_2002)


# Loading data -----------------------------
densidade <- readRDS("3_results_6d_cnae/densidade.RDS")
densidade <- lazy_dt(densidade)

# Defining paralellization ----------------------------------------------------

# Define number of clusters
parallel::detectCores()
n.cores <- parallel::detectCores() - 22

# Make a cluster
my.cluster <- parallel::makeCluster(
    n.cores, 
    type = "PSOCK"
)


print(my.cluster)

# Registering cluster
doParallel::registerDoParallel(cl = my.cluster)


# Checking 
foreach::getDoParRegistered()
foreach::getDoParWorkers()

# Data work ------------------------------------------------------------------

anos <- densidade %>% 
    select(ano) %>% 
    distinct() %>% 
    pull(ano)


k = 1

dados_completos <- foreach(k = seq_along(anos)) %dopar% {
    
    library(tidyverse)
    library(data.table)
    library(dtplyr)
    
   return(densidade %>%
              filter(ano == anos[k]) %>%
              group_by(ano, country) %>%
              summarise(densidade_media = mean(densidade, na.rm = TRUE),
                        rca_count = sum(rca_binario, na.rm = TRUE),
                        rca_mean = mean(rca, na.rm = TRUE)) %>% 
              ungroup() %>%
              as.data.frame())
   
    
}



dados_interesse <- foreach(k = seq_along(anos)) %dopar% {
    
    library(tidyverse)
    library(data.table)
    library(dtplyr)
    
    return(densidade %>%
        filter(ano == anos[k] & product %in% cbos_interesse) %>% 
        group_by(ano, country) %>% 
        summarise(densidade_media_int = mean(densidade, na.rm = TRUE),
                  rca_count_int = sum(rca_binario, na.rm = TRUE),
                  rca_mean_int = mean(rca, na.rm = TRUE)) %>% 
        ungroup() %>% 
            as.data.frame())
    
    
}

parallel::stopCluster(cl = my.cluster)


dados_completos <- do.call(rbind, dados_completos)
dados_interesse <- do.call(rbind, dados_interesse)


dados_densidade <- dados_completos %>% 
    left_join(dados_interesse)

rm(dados_completos, dados_interesse)

gc()

# Loading ECI

files <- list.files(path = "3_results_6d_cnae", pattern = "ECI", full.names = TRUE)

eci <- map_dfr(.x = files, .f = read_rds)


# Loading employment

emprego <- readRDS("3_results_6d_cnae/emprego.RDS") %>%  
    group_by(ano, sigla_uf, id_municipio, cbo_2002) %>% 
    summarise(qtd_vinc = sum(qtd_vinc, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(interesse = ifelse(cbo_2002 %in% cbos_interesse,
                              yes = "1",
                              no = "0"))

emprego_total <- emprego2 %>% 
    group_by(ano, sigla_uf, id_municipio) %>% 
    summarise(qtd_vinc = sum(qtd_vinc, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(country = id_municipio) %>% 
    mutate(ano = as.character(ano))

emprego_interesse <- emprego2 %>% 
    group_by(ano, id_municipio, interesse) %>% 
    summarise(qtd_vinc_int = sum(qtd_vinc, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(country = id_municipio) %>%
    mutate(qtd_vinc_int = ifelse(interesse == "0",
                                 yes = 0,
                                 no = qtd_vinc_int)) %>% 
    group_by(ano, country) %>% 
    summarise(qtd_vinc_int = sum(qtd_vinc_int, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(ano = as.character(ano))


# Joining everything

dados_finais <- dados_densidade %>% 
    left_join(eci) %>%
    left_join(emprego_total) %>% 
    left_join(emprego_interesse) %>% 
    select(ano, sigla_uf, country, nome, qtd_vinc_int, qtd_vinc, eci, everything()) %>% 
    select(-diversificacao)



saveRDS(dados_finais, "3_results_6d_cnae/variaveis_municipios.RDS")





