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

rm(list = ls(all = TRUE))

source("2_scripts/function_create_product_space.R")


# Side data -----------------------------------------------------------


municipios <- read.csv("C:/Users/queir/Meu Drive/Economia/Outros/Tabelas de Compatibilizacoes/municipio.csv") %>%
    select(id_municipio, nome) %>%
    mutate(id_municipio = as.character(id_municipio))

cbos <- fread("C:/Users/queir/Meu Drive/Economia/Outros/Tabelas de Compatibilizacoes/cbo_2002.csv",
              colClasses = list(character = 1:10)) %>%
    select(cbo_2002, descricao) %>% 
    mutate(cbo_2002 = as.character(cbo_2002))


# Data --------------------------------------------------------------------

files <- list.files(path = "1_dados", pattern = ".RDS")
anos <- str_sub(files, -8, -5)

cytoscapePing()

for (k in seq_along(files)){
    
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
        filter(n.proc < 2) %>%
        pull(country)
    
    
    
    muns_zero <- data %>%
        group_by(country) %>%
        summarise(total = sum(value, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(total < 300) %>%
        pull(country)
    
    
    products_zero <- data %>%
        group_by(product) %>%
        summarise(total = sum(value, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(total < 100) %>%
        pull(product)
    
    
    data <- data %>%
        filter(!country %in% muns_zero & !product %in% products_zero & !country %in% procs_unicos)
    
    
    data <- data %>%
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
    
    mat.rca_long <- mat.rca %>%
        as.data.frame() %>%
        rownames_to_column(var = "country") %>%
        pivot_longer(cols = 2:last_col(),
                     values_to = "value",
                     names_to = "product")
    
    
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
        mutate(pci = pci) %>%
        rownames_to_column(var = "product") %>%
        left_join(ubiquidade)
    
    regions_complexity <- data.frame(eci = complexity[[1]]) %>%
        mutate(eci = eci) %>% 
        rownames_to_column(var = "country") %>%
        left_join(diversificacao) %>% 
        filter(!is.na(nome))
    
    write_excel_csv2(regions_complexity, str_c("3_results/ECI_", anos[k], ".csv"))
    write_excel_csv2(product_complexity, str_c("3_results/PCI_", anos[k], ".csv"))
    
    
    a <- ggplot()+
        geom_point(data = regions_complexity, aes(y = eci, x = diversificacao)) +
        theme_gray(base_size = 12)
    
    ggsave(str_c("3_results/ECI_diversificacao_", anos[k], ".png"), plot = a,
           height = 20, width = 20, units = "cm")
    
    
    
    a <- ggplot()+
        geom_point(data = product_complexity, aes(y = pci, x = ubiquidade)) +
        theme_gray(base_size = 12)
    
    
    ggsave(str_c("3_results/PCI_ubiquidade_", anos[k], ".png"), plot = a,
           height = 20, width = 20, units = "cm")
    
    
    # Proximities
    
    proximities <- economiccomplexity::proximity(balassa_index = mcp,
                                                 compute = "both")
    
    
    as.matrix(proximities[[2]]) %>%
        as.data.frame.table(stringsAsFactors = FALSE) %>% 
        rename(source = Var1,
               target = Var2,
               value = Freq) %>% 
        filter(source != target) %>% 
        saveRDS(str_c("3_results/similarities_", anos[k], ".RDS"))
    
    
    
    product_network <- complexity_graph(proximity_mat = proximities[[2]], threshold = 0.3)
    
    
    edgelist_product <- as_edgelist(product_network)
    
    edgelist_product <- data.frame(source=V(product_network)[edgelist_product[,1]]$name,
                                                 target=V(product_network)[edgelist_product[,2]]$name,
                                                 weight=E(product_network)$weight,
                                                 mst=E(product_network)$mst)
    
    
    
    nodes_list_product <- product_complexity %>% 
        rename(id = product)
    
    
    write.csv(edgelist_product, str_c("3_results/OS_edgelist_", anos[k], ".csv"), row.names = FALSE)
    write.csv(nodes_list_product, str_c("3_results/OS_nodelist_", anos[k], ".csv"), row.names = FALSE)
    
    
    createNetworkFromIgraph(product_network, title = str_c("Occupations Space ", anos[k]), collection = "Occupations Space")
    
    
    gc()

}
