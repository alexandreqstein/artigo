




dados_completos <- foreach( k = seq_along(files)) %dopar% {
    
    library(tidyverse)
    library(data.table)
    library(economiccomplexity)
    library(EconGeo)
    
    lista_densidades <-list()
    lista_empregos_total <-list()
    lista_empregos_cbo_interesse <-list()
    lista_rca <-list()
    lista_rcanb <-list()
    lista_rca_var <-list()
    lista_qtd.rca.interesse <-list()
    
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
    
    
    if (anos_k2[k] == "2021"){
        
        location.quotient(mat = data_mat_k2, binary = FALSE) %>%
            as.data.frame() %>%
            rownames_to_column(var = "country") %>%
            pivot_longer(cols = 2:last_col(),
                         values_to = "rcanb",
                         names_to = "product") %>%
            filter(product %in% cbos_interesse) -> 
            lista_rcanb[[anos_k2[k]]]
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
    
        relatedness.density(mat = mat.rca, relatedness = proximidades) %>%
            as.data.frame() %>% 
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
        
        relatedness.density(mat = mat.rca_k2, relatedness = proximidades_k2) %>%
            as.data.frame() %>% 
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
        
        
    resultado <- list(
        lista_densidades,
        lista_empregos_total,
        lista_empregos_cbo_interesse,
        lista_rca,
        lista_rcanb,
        lista_rca_var,
        lista_qtd.rca.interesse
        )
    
    
    return(resultado)
    
    
    gc()
}

