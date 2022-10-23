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




teste <- data %>% 
        mutate(cred = case_when(
                data$exp_ipca <= data$inflacao_meta_central ~ 1,
                data$exp_ipca > data$inflacao_meta_central & 
                        data$exp_ipca < 20 ~ (1-(1/(20-data$inflacao_meta_central))*(data$exp_ipca - data$inflacao_meta_central)),
                data$exp_ipca >= 20 ~ 0
                )
        ) %>%
        select(periodo, cred)

ggplot(data = teste) + aes(x = periodo, y = cred) + geom_line() + scale_y_continuous(limits=c(0, 1))










