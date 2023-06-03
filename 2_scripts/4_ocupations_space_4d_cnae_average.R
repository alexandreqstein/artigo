#### Sase article - networks
#### Sase article - networks
#### Sase article - networks

# Start: 31/05/2023
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

# Side data -----------------------------------------------------------

cbos <- fread("C:/Users/queir/Meu Drive/Economia/Outros/Tabelas de Compatibilizacoes/cbo_2002.csv",
              colClasses = list(character = 1:10)) %>%
    select(cbo_2002, descricao) %>% 
    mutate(cbo_2002 = as.character(cbo_2002))


# Data --------------------------------------------------------------------

files_edges <- list.files(path = "3_results_4d_cnae", pattern = "similarities", full.names = TRUE)
files_nodes <- list.files(path = "3_results_4d_cnae", pattern = "OS_nodelist", full.names = TRUE)
anos <- str_sub(files_edges, -8, -5)
dados <- list()




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


# Complaxity measures -----------------------------------------------------

edges <- foreach(i = seq_along(files_edges)) %dopar% {
    
    library(tidyverse)

    dados <- readRDS(files_edges[i]) %>% 
        mutate(ano = anos[i])
    
    return(dados)

}


nodes <- foreach(i = seq_along(files_nodes)) %dopar% {
    
    library(data.table)
    library(tidyverse)
    
    dados <- fread(files_nodes[i],
                   colClasses = c(id = "character",
                                  interesse = "character"))
    
    return(dados)
    
}


parallel::stopCluster(cl = my.cluster)


# Preparing edgelist

edges <- do.call(rbind, edges)

edges2 <- edges %>% 
    group_by(source, target) %>% 
    summarise(weight = mean(weight, na.rm = TRUE)) %>% 
    ungroup()

edges3 <- edges2 %>%
    arrange(source, target) %>% 
    pivot_wider(names_from = target,
                values_from = weight,
                values_fill = 0) %>% 
    column_to_rownames(var = "source") %>% 
    as.matrix()

proximidades <- Matrix(edges3, sparse = TRUE)

product_network <- complexity_graph(proximity_mat = proximidades, threshold = 0.3)
    
edgelist_product <- as_edgelist(product_network)
    
edgelist_product <- data.frame(source=V(product_network)[edgelist_product[,1]]$name,
                               target=V(product_network)[edgelist_product[,2]]$name,
                               weight=E(product_network)$weight,
                               mst=E(product_network)$mst)

saveRDS(edgelist_product, "3_results_4d_cnae/edgelist_average_all_period.RDS")
fwrite(edgelist_product, "3_results_4d_cnae/edgelist_average_all_period.csv",
       scipen = getOption("scipen", 999L))


# Preparing nodes list

nodes2 <- do.call(rbind, nodes)


nodes_ano <- nodes2 %>% 
    select(id, descricao, interesse, everything()) %>% 
    pivot_wider(names_from = ano,
                values_from = c(pci, ubiquidade))

nodes_average <- nodes2 %>% 
    group_by(id, descricao, interesse) %>% 
    summarise(pci_medio = mean(pci, na.rm = TRUE),
              ubiquidade_media = mean(ubiquidade, na.rm = TRUE)) %>% 
    ungroup()

nodes_average <- nodes_average %>% 
    left_join(nodes_ano)


saveRDS(nodes_average, "3_results_4d_cnae/nodes_average_all_period.RDS")
fwrite(nodes_average, "3_results_4d_cnae/nodes_average_all_period.csv",
       scipen = getOption("scipen", 999L))


vertex.attributes(graph = product_network) <- nodes_average

cytoscapePing()

createNetworkFromIgraph(product_network, title = "Occupations Space 4d", collection = "Occupations Space")


nodes_average %>% 
    select(id, descricao, interesse) %>% 
    filter(interesse == "1") %>%
    write_excel_csv2("ocupacoes_interesse_4d_cnae.csv")


