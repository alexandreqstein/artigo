#### Sase article - data collection
#### Sase article - data collection
#### Sase article - data collection

# Start: 09/05/2023
# Author: Alexandre Stein



#install.packages("basedosdados")

library(data.table)
library(basedosdados)





# Baixando dados da Base dos dados ----------------------------------------

basedosdados::set_billing_id("complexidade-sebrae")



basedosdados::get_dataset_description("basedosdados.br_me_rais.dicionario")


anos <- c('2006', '2007', '2008', '2009', '2010',
          '2011', '2012', '2013', '2014', '2015',
          '2016', '2017', '2018', '2019', "2020")
anos <- '2021'


estados <- c('AC', 'RR', 'RO', 'AM', 'PA', 'AP', 'TO', 'MA', 'PI', 'CE', 'RN', 'PB', 'PE', 'AL', 'SE', 'BA',
             'MG', 'ES', 'RJ', 'SP', 'PR', 'SC', 'RS', 'MS', 'MT', 'GO', 'DF')


for (i in seq_along(anos))
    for (j in seq_along(estados)){
        
        quest <- paste("SELECT ano, sigla_uf, id_municipio, vinculo_ativo_3112, cbo_2002, count(vinculo_ativo_3112) as qtd_vinc FROM `basedosdados.br_me_rais.microdados_vinculos` WHERE ano = ", 
                       anos[i], 
                       " AND sigla_uf = ",
                       "'",
                       estados[j],
                       "' AND vinculo_ativo_3112 = 1 ",
                       " GROUP BY ano, sigla_uf, id_municipio, vinculo_ativo_3112, cbo_2002", 
                       sep = "")
        
        
        arquivo <- paste("C:/backup_arquivos/RAIS/dados_rais_cbo_06_2021/",
                         estados[j], "_", anos[i], ".csv", sep = "")
        
        
        basedosdados::download(query = quest, path = arquivo)
        
    }


# Transformando em RDS ----------------------------------------------------


library(data.table)

setwd("C:/backup_arquivos/RAIS/dados_rais_cbo_06_2021")


files <- list.files(pattern = ".csv")

for (i in seq_along(files)){
    df <- fread(files[i], 
                colClasses = c(id_municipio = "character",
                               cbo_2002 = "character"))
    
    saveRDS(df, paste0(substr(files[i], 1, 7), ".RDS"))
}

files <- list.files(pattern = ".RDS")


# Testanto se as CBOS estão ok
for(i in files){
    
    teste <- readRDS(i)
    
    if (min(nchar(teste$cbo_2002)) < 6 & min(nchar(teste$cbo_2002)) > 0 | is.na(min(nchar(teste$cbo_2002)))){
        
        print(file[i])
    } else {
        print("no")
    }
    
}






