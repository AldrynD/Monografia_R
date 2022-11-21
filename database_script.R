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

## BASEDADOS DE DADOS BRUTA
basedados <- openxlsx::read.xlsx(xlsxFile = "basedados_monografia.xlsx", 
                            sheet = "BASEDADOS", 
                            detectDates = TRUE,
                            colNames = TRUE, 
                            na.strings = "-"
) %>% 
        mutate(llr = case_when(
                basedados$exp_ipca < basedados$inflacao_meta_inf ~ (1/(exp(basedados$exp_ipca - basedados$inflacao_meta_inf) - (basedados$exp_ipca - basedados$inflacao_meta_inf))),
                basedados$exp_ipca >= basedados$inflacao_meta_inf & basedados$exp_ipca <= basedados$inflacao_meta_sup ~ 1,
                basedados$exp_ipca > basedados$inflacao_meta_sup ~ (1/(exp(basedados$exp_ipca - basedados$inflacao_meta_sup) - (basedados$exp_ipca - basedados$inflacao_meta_sup)))
        ) 
        ) %>%
        #filter(data$periodo >= "2010-01-01") %>% 
        as_tibble()


# BASEDADOS PIVOTADO
basedados_pivot <- basedados %>% 
        pivot_longer(
                cols = -c(periodo),
                names_to = "variaveis",
                values_to = "valor"
        )

# BASEDADOS PARA CONSTRUÇÃO DOS IC'S
basedados_ics <- basedados %>% 
        filter(periodo >= "2010-01-01")


######--------------------------------------------------------------------------

### ÍNDICES DE CREDIBILIDADE ###

## INDICE DE CREDIBILIDADE Cecchetti e Krause (2002)
cred_ck <- basedados_ics %>% 
        dplyr::mutate(ck = case_when(
                basedados_ics$exp_ipca <= basedados_ics$inflacao_meta_central ~ 1,
                basedados_ics$exp_ipca > basedados_ics$inflacao_meta_central & 
                        basedados_ics$exp_ipca < 20 ~ (1-(1/(20-basedados_ics$inflacao_meta_central))*(basedados_ics$exp_ipca - basedados_ics$inflacao_meta_central)),
                basedados_ics$exp_ipca >= 20 ~ 0
                )
        ) %>%
        select(periodo, ck)

## INDICE DE CREDIBILIDADE Mendonça e Guimarães (2009)
cred_mg <- basedados_ics %>% 
        mutate(mg = case_when(
                basedados_ics$exp_ipca >= basedados_ics$inflacao_meta_inf & basedados_ics$exp_ipca <= basedados_ics$inflacao_meta_sup ~ 1,
                basedados_ics$exp_ipca > basedados_ics$inflacao_meta_sup & basedados_ics$exp_ipca < 20 ~ (1-(1/(20-basedados_ics$inflacao_meta_sup))*(basedados_ics$exp_ipca - basedados_ics$inflacao_meta_sup)),
                basedados_ics$exp_ipca > 0 & basedados_ics$exp_ipca < basedados_ics$inflacao_meta_inf ~ (1-(1/(-basedados_ics$inflacao_meta_inf))*(basedados_ics$exp_ipca - basedados_ics$inflacao_meta_inf)),
                basedados_ics$exp_ipca >= 20 | basedados_ics$exp_ipca <= 0 ~ 0
        )
        ) %>%
        select(periodo, mg)


## INDICE DE CREDIBILIDADE LLR (2018):
cred_llr <- basedados_ics %>% 
        mutate(llr = case_when(
                basedados_ics$exp_ipca < basedados_ics$inflacao_meta_inf ~ (1/(exp(basedados_ics$exp_ipca - basedados_ics$inflacao_meta_inf) - (basedados_ics$exp_ipca - basedados_ics$inflacao_meta_inf))),
                basedados_ics$exp_ipca >= basedados_ics$inflacao_meta_inf & basedados_ics$exp_ipca <= basedados_ics$inflacao_meta_sup ~ 1,
                basedados_ics$exp_ipca > basedados_ics$inflacao_meta_sup ~ (1/(exp(basedados_ics$exp_ipca - basedados_ics$inflacao_meta_sup) - (basedados_ics$exp_ipca - basedados_ics$inflacao_meta_sup)))
        ) 
        ) %>% 
        select(periodo, llr)

#####---------------------------------------------------------------------------

### GRÁFICOS GERAIS

#GRÁFICO META DE INFLAÇÃO
data_meta <- basedados_pivot %>% 
        filter(variaveis %in% c("inflacao_meta_central", "inflacao_meta_sup", "inflacao_meta_inf", "ipca_acum_12m", "exp_ipca"))


grafico_meta <- ggplot(data = data_meta, aes(x = periodo, y = valor, color = variaveis, )) +
        geom_line() +
        labs(y = "%",
             x = "Período",
             title = "Meta de Inflação do BCB",
             subtitle = "Com intervalos de tolerância e Inflação acumulada vs Esperada",
             caption = "Fonte: BCB | Elaborado pelo autor"
        ) +
        theme_bw()

grafico_meta

#Grafico IC_CK
grafico_ck <- ggplot(data = cred_ck) + aes(x = periodo, y = ck) + 
        geom_line(size = 0.75) + 
        scale_y_continuous(limits=c(0, 1), n.breaks = 6L) +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2022-10-01"))) +
        labs(y = "IC",
             x = "Período",
             title = "Índice de Credibilidade",
             subtitle = "Cecchetti e Krause (2002)",
             caption = "Elaborado pelo autor"
        ) +
        theme(plot.title = element_text(family = "Times")) +
        theme_bw() +
        theme(panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))
 
#Grafico IC_MG
grafico_mg <- ggplot(data = cred_mg) + aes(x = periodo, y = mg) + 
        geom_line(size = 0.75) + 
        scale_y_continuous(limits=c(0, 1), n.breaks = 6L) +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2022-10-01"))) +
        labs(y = "IC",
             x = "Período",
             title = "Índice de Credibilidade",
             subtitle = "Mendonça e Guimarães (2009)",
             caption = "Elaborado pelo autor"
        ) +
        theme(plot.title = element_text(family = "Times")) +
        theme_bw() +
        theme(panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))


#Grafico IC_LLR
grafico_llr <- ggplot(data = cred_llr) + aes(x = periodo, y = llr) + 
        geom_line(size = 0.75) + 
        scale_y_continuous(limits=c(0, 1), n.breaks = 6L) +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2022-10-01"))) +
        labs(y = "IC",
             x = "Período",
             title = "Índice de Credibilidade",
             subtitle = "Levieuge, Lucotte e Ringuedé (2018)",
             caption = "Elaborado pelo autor"
        ) +
        theme(plot.title = element_text(family = "Times")) +
        theme_bw() +
        theme(panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))


