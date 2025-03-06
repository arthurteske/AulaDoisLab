## EXERCICIO AULA DE SEXTA FEIRA 28/02 ##

#PACOTES####
# install.packages('WDI') #PRIMEIRO A EXECUTAR
# library(WDI) # CARREGAR O PACOTE
# library(tidyverse)

#VARIAVEIS ####
# Importações de bens e serviços = NE.IMP.GNFS.CD
# Exportações de bens e serviços =  NE.EXP.GNFS.CD

variaveis <- c('NE.IMP.GNFS.CD', 'NE.EXP.GNFS.CD')

#base de dados de cortes transversal####
dados <- WDI(indicator = variaveis, 
             country= 'all', #todos = all
             start = 2023, end = 2023) 

# base completa do Brasil####
dadosBR_Com <- WDI(indicator = variaveis, 
                   country= 'BR', #codigo Iso2c
                   start = , end = ) 