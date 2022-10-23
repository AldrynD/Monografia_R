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
        filter(data$periodo >= "2010-01-01") %>% 
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

ggplot(data = teste) + aes(x = periodo, y = cred) + 
        geom_line() + 
        scale_y_continuous(limits=c(0, 1), n.breaks = 6L) +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2022-10-01"))) +
        labs(y = "IC",
             x = "Período",
             title = "Índice de Credibilidade",
             subtitle = "Cecchetti e Krause (2002)",
             caption = "Feito pelo autor"
        ) +
        theme(plot.title = element_text(family = "Times")) +
        theme_bw() +
        theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted")) +
        theme(panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))
 










