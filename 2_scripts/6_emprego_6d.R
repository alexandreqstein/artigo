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
library(stringi)
library(foreach)
library(doParallel)


setwd("C:/Users/queir/Meu Drive/Economia/Outros/Gustavo Britto/sase_2023_rio/digital_techs")

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
                                               "analista de pcp (programacao e controle da producao)",
                                               "coordenador de programacao"))) %>% 
    select(cbo_2002) %>% 
    distinct() %>% 
    pull(cbo_2002)  



# Metadata --------------------------------------------------------------------

files_cnae <- list.files(path = "1_dados", pattern = "cnae")

anos <- as.character(2006:2021)

# Defining paralellization ----------------------------------------------------

# Define number of clusters
parallel::detectCores()
n.cores <- parallel::detectCores() - 18

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

k = 1

dados_completos <- foreach( k = seq_along(files_cnae)) %dopar% {
    
    library(tidyverse)
    library(data.table)
    library(economiccomplexity)
    library(EconGeo)
    
    lista_emprego_cbo <-list()
    lista_emprego_cnae <-list()
    
    print(anos[k])
    
    # Preparing basic data  
    
    a <- readRDS(paste0("1_dados/", files_cnae[k])) %>% 
        mutate(interesse = ifelse(cbo_2002 %in% cbos_interesse,
                                  yes = 1,
                                  no = 0)) 
    
    return(a)
    
}

# Stop cluster work
parallel::stopCluster(cl = my.cluster)


dados_completos <- do.call(rbind, dados_completos)

saveRDS(dados_completos, "3_results_6d_cnae/emprego.RDS")




















