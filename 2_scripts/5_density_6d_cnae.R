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
files_muns <- list.files(path = "1_dados", pattern = "emprego_municipio_cbo_2")
files_densidade <- list.files(path = "3_results_6d_cnae", pattern = "similarities", full.names = TRUE)

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

dados_completos <- foreach( k = seq_along(files_densidade)) %dopar% {
    
    library(tidyverse)
    library(data.table)
    library(economiccomplexity)
    library(EconGeo)
    
    lista_densidades <-list()
    lista_rca <-list()
    lista_rcanb <- list()
    
    print(anos[k])
    
    # Preparing basic data  
    
    data_mat <- readRDS(paste0("1_dados/", files_muns[k])) %>%
        mutate(cbo_2002 = str_sub(cbo_2002, 1, 6)) %>% 
        select(id_municipio, cbo_2002, qtd_vinc) %>%
        group_by(id_municipio, cbo_2002) %>% 
        summarise(qtd_vinc = sum(qtd_vinc, na.rm = TRUE)) %>% 
        ungroup() %>% 
        rename(country = id_municipio,
               product = cbo_2002,
               value = qtd_vinc) %>%
        pivot_wider(names_from = product,
                    values_from = value,
                    values_fill = 0)
    
    data_mat <- data_mat %>%
        column_to_rownames(var = "country")
    
    nomes <- list(names1 = rownames(data_mat),
                  names2 = colnames(data_mat))
    
    
    data_mat <- as.matrix(x = data_mat)
    dimnames(data_mat) <- nomes
    
    # RCAs K1
    
    mat.rca <- location.quotient(mat = data_mat, binary = TRUE)
    
    coocorrencias <- co.occurrence(mat = t(mat.rca), diagonal = TRUE) #c?lculo da coocorr?ncias - exige a matriz transposta
    
    
    proximidades <- read_rds(files_densidade[k]) %>%
        pivot_wider(names_from = target,
                    values_from = weight,
                    values_fill = 0) %>% 
        column_to_rownames(var = "source") %>% 
        as.matrix()
    
    
    mat.rca %>%
        as.data.frame() %>%
        rownames_to_column(var = "country") %>%
        pivot_longer(cols = 2:last_col(),
                     values_to = "rca_binario",
                     names_to = "product") %>% 
        mutate(ano = anos[k]) ->
        lista_rca[[anos[k]]]
    
    location.quotient(mat = data_mat, binary = FALSE) %>% 
        as.data.frame() %>%
        rownames_to_column(var = "country") %>%
        pivot_longer(cols = 2:last_col(),
                     values_to = "rca",
                     names_to = "product") %>%
        mutate(ano = anos[k]) ->
        lista_rcanb[[anos[k]]]
    
    
    
    # General density between regions and occupations
    relatedness.density(mat = mat.rca, relatedness = proximidades) %>%
        as.data.frame() %>% 
        rownames_to_column(var = "country") %>%
        pivot_longer(cols = c(2:last_col()),
                     names_to = "product",
                     values_to = "densidade") %>% 
        mutate(ano = anos[k],
               densidade = densidade/100) %>% #dividindo por 100 para ficar entre 0 e 1 e manter coerência com outras medidas feitas
        select(ano, country, product, densidade) -> 
        lista_densidades[[anos[k]]]
    
    resultado <- list(
        
        lista_densidades,
        lista_rca,
        lista_rcanb
        
    )
    
    return(resultado)
    
    gc()
}

# Stop cluster work
parallel::stopCluster(cl = my.cluster)

lista_densidades <- list()
lista_rca <- list()
lista_rcanb <- list()

for (k in seq_along(1:(length(dados_completos)))){
    
    lista_densidades[[k]] <- dados_completos[[k]][[1]][[1]]
    lista_rca[[k]] <- dados_completos[[k]][[2]][[1]]
    lista_rcanb[[k]] <- dados_completos[[k]][[3]][[1]]
    
}




# Binding all densities and employment data along years

densidades <- do.call(rbind, lista_densidades)
rca <- do.call(rbind, lista_rca)
rcanb <- do.call(rbind, lista_rcanb)


dados_completos <- densidades %>% 
    left_join(rca) %>% 
    left_join(rcanb) %>% 
    select(ano, country, product, rca_binario, rca, densidade)



fwrite(dados_completos, "3_results_6d_cnae/densidade.csv",
       scipen = getOption("scipen", 999L))



saveRDS(dados_completos, "3_results_6d_cnae/densidade.RDS")





















