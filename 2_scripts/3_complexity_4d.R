#### Sase article - data cleaning
#### Sase article - data cleaning
#### Sase article - data cleaning

# Start: 09/05/2023
# Author: Alexandre Stein

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)
library(economiccomplexity)
library(EconGeo)
library(Matrix)
library(stringi)
library(foreach)
library(doParallel)


setwd("C:/Users/queir/Meu Drive/Economia/Outros/Gustavo Britto/sase_2023_rio/digital_techs")

rm(list = ls(all = TRUE))

# Side data -----------------------------------------------------------


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
    mutate(cbo_2002 = str_sub(cbo_2002, 1, 4)) %>% 
    select(cbo_2002) %>% 
    distinct() %>% 
    pull(cbo_2002)  


cbos <- fread("C:/Users/queir/Meu Drive/Economia/Outros/Tabelas de Compatibilizacoes/cbo_2002.csv",
              colClasses = list(character = 1:10)) %>%
    select(familia, descricao_familia) %>% 
    distinct() %>% 
    mutate(familia = as.character(familia),
           interesse = ifelse(familia %in% cbos_interesse,
                              yes = 1,
                              no = 0),
           descricao_familia = case_when(
               descricao_familia == "Técnicos de operação de emissoras de rádio" ~ "Técnicos de operação de registros sonoro/audiovisuais",
               descricao_familia == "Locutores, comentaristas e repórteres de rádio e televisão" ~ "Locutores, comentaristas e repórteres de mídias audiovisuais",
               descricao_familia == "Técnicos em operação de sistemas de televisão e de produtoras de vídeo" ~ "Supervisores operacionais e técnicos em mídias audiovisuais",
               TRUE ~ descricao_familia)
           ) %>% 
    rename(cbo_2002 = familia,
           descricao = descricao_familia) %>% 
    distinct()



# Data --------------------------------------------------------------------

files <- list.files(path = "1_dados", pattern = ".RDS")
anos <- str_sub(files, -8, -5)

# Defining paralellization ----------------------------------------------------

# Define number of clusters
parallel::detectCores()
n.cores <- parallel::detectCores() - 12

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


# Complaxity measures -----------------------------------------------------

foreach(k = seq_along(files)) %dopar% {
    
    library(tidyverse)
    library(data.table)
    library(economiccomplexity)
    library(EconGeo)
    library(Matrix)
    
    # Load data
    dados<- readRDS(paste0("1_dados/", files[k]))
    
    data <- dados %>%
        select(id_municipio, cbo_2002, qtd_vinc) %>%
        mutate(cbo_2002 = str_sub(cbo_2002, 1, 4)) %>% 
        group_by(id_municipio, cbo_2002) %>% 
        summarise(qtd_vinc = sum(qtd_vinc, na.rm = TRUE)) %>% 
        rename(country = id_municipio,
               product = cbo_2002,
               value = qtd_vinc) %>%
        pivot_wider(names_from = product,
                    values_from = value,
                    values_fill = 0) %>%
        pivot_longer(cols = 2:last_col(),
                     names_to = "product",
                     values_to = "value")
    
    # Removing some outliers products and countries
    procs_unicos <- data %>%
        group_by(country) %>%
        mutate(proc = ifelse(value > 0,
                             yes = 1,
                             no = 0)) %>%
        summarise(n.proc = sum(proc, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(n.proc < 0) %>%
        pull(country)
    
    
    
    muns_zero <- data %>%
        group_by(country) %>%
        summarise(total = sum(value, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(total < 0) %>%
        pull(country)
    
    
    products_zero <- data %>%
        group_by(product) %>%
        summarise(total = sum(value, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(total < 0) %>%
        pull(product)
    
    
    data <- data %>%
        filter(!country %in% muns_zero & !product %in% products_zero & !country %in% procs_unicos) %>%
        pivot_wider(names_from = product,
                    values_from = value,
                    values_fill = 0) %>%
        {.->> data_mat} %>%
        pivot_longer(cols = 2:last_col(),
                     values_to = "value",
                     names_to = "product")
    
    # Preparing data
    data_mat <- data_mat %>%
        column_to_rownames(var = "country")
    
    nomes <- list(names1 = rownames(data_mat),
                  names2 = colnames(data_mat))
    
    data_mat <- as.matrix(x = data_mat)
    dimnames(data_mat) <- nomes
    
    
    # Diversity and ubiquity --------------------------------------------------
    
    mat.rca <- location.quotient(mat = data_mat, binary = TRUE)
    
    coocorrencias <- co.occurrence(mat = t(mat.rca), diagonal = TRUE) #c?lculo da coocorr?ncias - exige a matriz transposta
    
    proximidades <- relatedness(mat = coocorrencias, method = "cosine")
    
    diversificacao <- data.frame(diversificacao = EconGeo::diversity(mat = mat.rca, RCA = FALSE)) %>%
        rownames_to_column(var = "country") %>%
        left_join(municipios, by = c("country" = "id_municipio")) %>%
        select(country, nome, diversificacao)
    
    ubiquidade <- data.frame(ubiquidade = EconGeo::ubiquity(mat = mat.rca, RCA = FALSE)) %>%
        rownames_to_column(var = "product") %>%
        left_join(cbos, by = c("product" = "cbo_2002"))
    
    
    mcp <- Matrix(data = mat.rca, sparse = TRUE)
    
    
    
    # Complexity indexes
    
    complexity <- complexity_measures(mcp, iterations = 20, method = "reflections")
    
    
    product_complexity <- data.frame(pci = complexity[[2]]) %>%
        mutate(pci = pci,
               ano = anos[k]) %>%
        rownames_to_column(var = "product") %>%
        left_join(ubiquidade) %>% 
        select(ano, everything()) %>% 
        ungroup()
    
    regions_complexity <- data.frame(eci = complexity[[1]]) %>%
        mutate(eci = eci,
               ano = anos[k]) %>% 
        rownames_to_column(var = "country") %>%
        left_join(diversificacao) %>% 
        filter(!is.na(nome)) %>% 
        select(ano, everything()) %>% 
        ungroup()
    
    saveRDS(regions_complexity, str_c("3_results_4d/ECI_", anos[k], ".RDS"))
    saveRDS(product_complexity, str_c("3_results_4d/PCI_", anos[k], ".RDS"))
    
    
    a <- ggplot()+
        geom_point(data = regions_complexity, aes(y = eci, x = diversificacao)) +
        theme_gray(base_size = 12)
    
    ggsave(str_c("3_results_4d/ECI_diversificacao_", anos[k], ".png"), plot = a,
           height = 20, width = 20, units = "cm")
    
    
    
    a <- ggplot()+
        geom_point(data = product_complexity, aes(y = pci, x = ubiquidade)) +
        theme_gray(base_size = 12)
    
    
    ggsave(str_c("3_results_4d/PCI_ubiquidade_", anos[k], ".png"), plot = a,
           height = 20, width = 20, units = "cm")
    
    
}

parallel::stopCluster(cl = my.cluster)
