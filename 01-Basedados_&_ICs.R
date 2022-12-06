################################################################################
################################################################################

##### MONOGRAFIA
##### ALDRYN DYLAN MAMANI QUISPE
##### ORIENTADOR: ALEXSANDRO NASCIMENTO ORDONEZ
##### ORIENTADOR 2: MATHEUS ALBERGARIA DE MAGALHÃES

##### TEMA: POLÍTICA MONETÁRIA E O FATOR CREDIBILIDADE DO BANCO CENTRAL BRASILEIRO

################################################################################
################################################################################


##### 01-Basedados_&_ICs


#####---------------------------------------------------------------------------

### PACOTES UTILIZADOS ###
library(tidyverse)
library(openxlsx)

#####---------------------------------------------------------------------------


### CARREGANDO BASE DE DADOS BRUTA UTILIZADO PARA PLOTAR OS GRÁFICOS DOS IC'S:

#Base de dados bruta
basedados <- openxlsx::read.xlsx(xlsxFile = "basedados_monografia.xlsx", 
                                 sheet = "BASEDADOS", 
                                 detectDates = TRUE,
                                 colNames = TRUE, 
                                 na.strings = "-"
) %>% 
  as_tibble() %>% 
        mutate(log_selic = log(selic))


#Pivotando a base de dados bruta (basedados):
basedados_pivot <- basedados %>% 
  pivot_longer(
    cols = -c(periodo, per),
    names_to = "variaveis",
    values_to = "valores"
  )

#Filtrando "basedados" para plotar gráficos (periodo >= "2010-01-01"):
basedados_ics <- basedados %>% 
  filter(periodo >= "2010-01-01")




######--------------------------------------------------------------------------

### CÁLCULO DOS ÍNDICES DE CREDIBILIDADE ###

##Cálculo do ÍNDICE DE CREDIBILIDADE de Cecchetti e Krause (2002):
basedados_cred_ck <- basedados_ics %>% 
  dplyr::mutate(ck = case_when(
    basedados_ics$exp_ipca <= basedados_ics$inflacao_meta_central ~ 1,
    basedados_ics$exp_ipca > basedados_ics$inflacao_meta_central & 
      basedados_ics$exp_ipca < 20 ~ (1-(1/(20-basedados_ics$inflacao_meta_central))*(basedados_ics$exp_ipca - basedados_ics$inflacao_meta_central)),
    basedados_ics$exp_ipca >= 20 ~ 0
  )
  ) %>%
  select(periodo, ck)


##Cálculo do ÍNDICE DE CREDIBILIDADE de Mendonça e Guimarães (2009):
basedados_cred_mg <- basedados_ics %>% 
  mutate(mg = case_when(
    basedados_ics$exp_ipca >= basedados_ics$inflacao_meta_inf & basedados_ics$exp_ipca <= basedados_ics$inflacao_meta_sup ~ 1,
    basedados_ics$exp_ipca > basedados_ics$inflacao_meta_sup & basedados_ics$exp_ipca < 20 ~ (1-(1/(20-basedados_ics$inflacao_meta_sup))*(basedados_ics$exp_ipca - basedados_ics$inflacao_meta_sup)),
    basedados_ics$exp_ipca > 0 & basedados_ics$exp_ipca < basedados_ics$inflacao_meta_inf ~ (1-(1/(-basedados_ics$inflacao_meta_inf))*(basedados_ics$exp_ipca - basedados_ics$inflacao_meta_inf)),
    basedados_ics$exp_ipca >= 20 | basedados_ics$exp_ipca <= 0 ~ 0
  )
  ) %>%
  select(periodo, mg)


##Cálculo do ÍNDICE DE CREDIBILIDADE de Levieuge, Lucotte e Ringuedé (2018):
basedados_cred_llr <- basedados_ics %>% 
  mutate(llr = case_when(
    basedados_ics$exp_ipca < basedados_ics$inflacao_meta_inf ~ (1/(exp(basedados_ics$exp_ipca - basedados_ics$inflacao_meta_inf) - (basedados_ics$exp_ipca - basedados_ics$inflacao_meta_inf))),
    basedados_ics$exp_ipca >= basedados_ics$inflacao_meta_inf & basedados_ics$exp_ipca <= basedados_ics$inflacao_meta_sup ~ 1,
    basedados_ics$exp_ipca > basedados_ics$inflacao_meta_sup ~ (1/(exp(basedados_ics$exp_ipca - basedados_ics$inflacao_meta_sup) - (basedados_ics$exp_ipca - basedados_ics$inflacao_meta_sup)))
  ) 
  ) %>% 
  select(periodo, llr)



### META DE INFLAÇÃO BRASILEIRA:

#Base de dados para plotagem do gráfico de metas para inflação brasileira:
basedados_meta <- basedados_pivot %>% 
  filter(variaveis %in% c(
    "inflacao_meta_central", 
    "inflacao_meta_sup", 
    "inflacao_meta_inf", 
    "ipca_acum_12m",
    "exp_ipca")
    )


#####---------------------------------------------------------------------------

### GRÁFICO DE META DE INFLAÇÃO BRASILEIRA ###

basedados_meta_plot <- ggplot(data = basedados_meta, aes(x = periodo, y = valores, color = variaveis)) +
  geom_line() +
  labs(y = "%",
       x = "Período",
       title = "Meta de Inflação do BCB",
       subtitle = "Com intervalos de tolerância e Inflação acumulada vs Esperada",
       caption = "Fonte: BCB | Elaborado pelo autor"
  ) +
  theme_bw()

basedados_meta_plot



##############


a_grafico <- basedados %>% 
        ggplot(aes(x = periodo)) +  
        geom_line(aes(y = inflacao_meta_inf), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(y = inflacao_meta_central), color = "gray", size = 1.3) +
        geom_line(aes(y = inflacao_meta_sup), color = "black", linetype = "dashed", size = 1) +
        geom_line(aes(y = ipca_acum_12m), color = "black", size = 0.7) +
        geom_line(aes(y = exp_ipca), color = "black", linetype = "F1", size = 1.3) +
        scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
        labs(y = "Inflação, em %", x = "") +
        theme_bw() +
        theme_set(theme_bw(base_size = 13))
a_grafico

##############

a_juros <- basedados %>% 
        ggplot(aes(x = periodo)) + 
        geom_line(aes(y = exp_selic), color = "black", size = 0.7) +
        geom_line(aes(y = selic), color = "black", linetype = "F1", size = 1) +
        scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
        scale_y_continuous(limits=c(0, 30), n.breaks = 5L) +
        labs(y = "%", x = "") +
        theme(plot.title = element_text(family = "Times")) +
        theme_bw() +
        theme_set(theme_bw(base_size = 13))
a_juros

######












### GRÁFICOS DOS ÍNDICES DE CREDIBILIDADE ###


#Plotando gráfico do IC_CK
basedados_cred_ck_plot <- ggplot(data = basedados_cred_ck) + aes(x = periodo, y = ck) + 
  geom_line(size = 0.75) + 
  scale_y_continuous(limits=c(0, 1), n.breaks = 6L) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2022-06-01"))) +
  labs(y = "IC CK",
       x = ""#,
      #title = "Índice de Credibilidade (CK)",
       #subtitle = "Cecchetti e Krause (2002)",
       #caption = "Fonte: Elaborado pelo autor"
  ) +
  theme(plot.title = element_text(family = "Times")) +
  theme_bw()+
        theme_set(theme_bw(base_size = 13))#+
  #theme(panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))

basedados_cred_ck_plot


##Plotando gráfico do IC_MG:
basedados_cred_mg_plot <- ggplot(data = basedados_cred_mg) + aes(x = periodo, y = mg) + 
  geom_line(size = 0.75) + 
  scale_y_continuous(limits=c(0, 1), n.breaks = 6L) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2022-06-01"))) +
  labs(y = "IC MG",
       x = ""#,
       #title = "Índice de Credibilidade (MG)",
       #subtitle = "Mendonça e Guimarães (2009)",
       #caption = "Fonte: Elaborado pelo autor"
  ) +
  theme(plot.title = element_text(family = "Times")) +
  theme_bw()+
        theme_set(theme_bw(base_size = 13))# +
  #theme(panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))

basedados_cred_mg_plot


##Plotando gráfico do IC_LLR:
basedados_cred_llr_plot <- ggplot(data = basedados_cred_llr) + aes(x = periodo, y = llr) + 
  geom_line(size = 0.75) + 
  scale_y_continuous(limits=c(0, 1), n.breaks = 6L) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2010-01-01", "2022-06-01"))) +
  labs(y = "IC LLR",
       x = ""#,
       #title = "Índice de Credibilidade (LLR)",
       #subtitle = "Levieuge, Lucotte e Ringuedé (2018)",
       #caption = "Fonte: Elaborado pelo autor"
  ) +
  theme(plot.title = element_text(family = "Times")) +
  theme_bw()+
        theme_set(theme_bw(base_size = 13))# +
  #theme(panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"))

basedados_cred_llr_plot


################################################################################
################################################################################

