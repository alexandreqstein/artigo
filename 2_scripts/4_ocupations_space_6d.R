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
library(RCy3)
library(Matrix)
library(foreach)
library(doParallel)

setwd("C:/Users/queir/Meu Drive/Economia/Outros/Gustavo Britto/sase_2023_rio/digital_techs")

rm(list = ls(all = TRUE))

source("2_scripts/function_create_product_space.R")

# Data --------------------------------------------------------------------

files <- list.files(path = "1_dados", pattern = ".RDS")
anos <- str_sub(files, -8, -5)


# Defining paralellization ----------------------------------------------------

# Define number of clusters
parallel::detectCores()
n.cores <- parallel::detectCores() - 16

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


foreach(k = seq_along(files)) %dopar% {
    
    library(tidyverse)
    library(data.table)
    library(economiccomplexity)
    library(EconGeo)
    library(Matrix)
    
    source("2_scripts/function_create_product_space.R")
    
    dados<- readRDS(paste0("1_dados/", files[k]))
    
    data <- dados %>%
        select(id_municipio, cbo_2002, qtd_vinc) %>%
        rename(country = id_municipio,
               product = cbo_2002,
               value = qtd_vinc) %>%
        pivot_wider(names_from = product,
                    values_from = value,
                    values_fill = 0) %>%
        pivot_longer(cols = 2:last_col(),
                     names_to = "product",
                     values_to = "value")
    
    
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
    
    data_mat <- data_mat %>%
        column_to_rownames(var = "country")
    
    nomes <- list(names1 = rownames(data_mat),
                  names2 = colnames(data_mat))
    
    data_mat <- as.matrix(x = data_mat)
    dimnames(data_mat) <- nomes
    
    
    # Diversity and ubiquity --------------------------------------------------
    
    mat.rca <- location.quotient(mat = data_mat, binary = TRUE)
    
    mcp <- Matrix(data = mat.rca, sparse = TRUE)
    
    complexity <- readRDS(paste0("3_results_6d/PCI_", anos[k], ".RDS"))
    
    
    
    # Proximities
    
    proximities <- economiccomplexity::proximity(balassa_index = mcp,
                                                 compute = "both")
    
    
    as.matrix(proximities[[2]]) %>%
        as.data.frame.table(stringsAsFactors = FALSE) %>% 
        rename(source = Var1,
               target = Var2,
               weight = Freq) %>% 
        saveRDS(str_c("3_results_6d/similarities_", anos[k], ".RDS"))
    
    
    product_network <- complexity_graph(proximity_mat = proximities[[2]], threshold = 0.3)
    
    vcount(product_network)
    
    edgelist_product <- as_edgelist(product_network)
    
    edgelist_product <- data.frame(source=V(product_network)[edgelist_product[,1]]$name,
                                   target=V(product_network)[edgelist_product[,2]]$name,
                                   weight=E(product_network)$weight,
                                   mst=E(product_network)$mst)
    
    
    
    nodes_list_product <- complexity %>% 
        rename(id = product)
    
    
    write.csv(edgelist_product, str_c("3_results_6d/OS_edgelist_", anos[k], ".csv"), row.names = FALSE)
    write.csv(nodes_list_product, str_c("3_results_6d/OS_nodelist_", anos[k], ".csv"), row.names = FALSE)
    
    
    gc()
    
}

parallel::stopCluster(cl = my.cluster)
