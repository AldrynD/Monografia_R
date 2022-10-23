################################################################################
##### MONOGRAFIA #####

##### POLÍTICA MONETÁRIA E O FATOR CREDIBILIDADE DO BANCO CENTRAL BRASILEIRO EM TEMPOS DE COVID-19

##### ALDRYN DYLAN MAMANI QUISPE


################################################################################



#####---------------------------------------------------------------------------

### PACOTES UTILIZADOS ###
library(tidyverse)
library(openxlsx)



#####---------------------------------------------------------------------------

### BASE DE DADOS ###
#Base sem tratamento
data <- openxlsx::read.xlsx(xlsxFile = "basedados_monografia.xlsx", 
                            sheet = "BASEDADOS", 
                            detectDates = TRUE,
                            colNames = TRUE, 
                            na.strings = "-"
) %>% 
        as_tibble()


#Base com tratamento
database <- data %>% 
        pivot_longer(
                cols = -c(periodo),
                names_to = "variaveis",
                values_to = "valor"
        )










