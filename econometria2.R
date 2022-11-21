
######## PREPARAÇÃO E ESTIMAÇÃO DO MODELO EGARCH ##########

###-----------------------------------------------------------------------------

### CARREGANDO BASE DE DADOS (BASEDADOS_3) - DADOS DE 2010-01 A 2021-09
# Foi utilizado um cálculo de expectativa de inflação médio ponderado, 
# como apresentado em Levieuge, Lucotte e Ringuedé (2018) [exp_ipca_pond]
basedados3 <- openxlsx::read.xlsx(xlsxFile = "basedados_monografia.xlsx", 
                                 sheet = "BASEDADOS_3", 
                                 detectDates = TRUE,
                                 colNames = TRUE, 
                                 na.strings = "-"
) %>% 
        filter(periodo >= "2010-01-01" & periodo <= "2021-09-01" ) %>% 
        mutate(llr = case_when(
                basedados3$exp_ipca_pond < basedados3$inflacao_meta_inf ~ (1/(exp(basedados3$exp_ipca_pond - basedados3$inflacao_meta_inf) - (basedados3$exp_ipca_pond - basedados3$inflacao_meta_inf))),
                basedados3$exp_ipca_pond >= basedados3$inflacao_meta_inf & basedados3$exp_ipca_pond <= basedados3$inflacao_meta_sup ~ 1,
                basedados3$exp_ipca_pond > basedados3$inflacao_meta_sup ~ (1/(exp(basedados3$exp_ipca_pond - basedados3$inflacao_meta_sup) - (basedados3$exp_ipca_pond - basedados3$inflacao_meta_sup)))
        ) 
        ) %>%
        dplyr::mutate(log_selic = log(selic)) %>%  #log da selic
        dplyr::mutate(difference = log_selic - lag(log_selic, 1)) %>% # aplicando diferença no log_selic 
        dplyr::mutate(llr_defasado = lag(llr, 1)) %>% #defasando o índice LLR em 1 período
        na.omit() #retirando as linhas que contém NA values. #Importante para estimar o eGARCH

basedados3_log_selic_ts <- as.ts(basedados3$log_selic) #tranformando log_selic em formato time series.

basedados3_log_selic_ts_df <- diff(basedados3_log_selic_ts) #aplicando a primeira diferença da série.


qqnorm(basedados3_log_selic_ts) #teste de normalidade da série.
qqline(basedados3_log_selic_ts)

#Função de autocorrelação (ACF): Ela nos mostra o quão forte o valor observado hoje está correlacionado com os 
#valores observados no passado e como choques hoje afetam valores futuros da variável estocástica.
forecast::Acf(basedados3_log_selic_ts) 

#Função de Autocorrelação Parcial (PACF): Teste de correlação entre a variável no instante t e uma de suas defasagens,
#retirado os efeitos das outras defasagens.
forecast::Pacf(basedados3_log_selic_ts) #Truncagem em 3, indicando AR(3)

#Teste Dickey-Fuller aumentado (ADF): Teste de estacionaridade
#H0: Possui raíz unitária --> processo não estacionário
#H1: Sem raíz unitária --> processo estacionário
tseries::adf.test(basedados3_log_selic_ts) #p-valor (0,07712) > 0,01 --> processo não estacionário.

#Verificação da quantidade de diferenciações para estacionarizar uma série:
ndiffs(basedados3_log_selic_ts) #Necessidade de 1 diferenciação


#anova_dif2 <- basedados3$difference^2
#forecast::Acf(anova_dif2)
#forecast::Pacf(anova_dif2)



#Estimando um ARIMA(3,1,0):
basedados3_arima <- forecast::Arima(y = basedados3_log_selic_ts, order = c(3,1,0), include.constant = TRUE)
summary(basedados3_arima)

#Calculando o p-valor dos parâmetros ARIMA (drift = constant):
basedados3_arima_pvalor <- (1-pnorm(abs(basedados3_arima$coef)/sqrt(diag(basedados3_arima$var.coef))))*2
basedados3_arima_pvalor #Todos os parâmetros são estatisticamente significantes a 1%.

#Teste Ljung-Box de normalidade dos resíduos do ARIMA estimado:
Box.test(x = basedados3_arima$residuals, type = "Ljung", lag = 1) #p-valor > 0,01 --> Aceitamos H0 que os resíduos são ruído branco.
forecast::Acf(basedados3_arima$residuals) 


### ESTIMANDO O MODELO EGARCH COM AS INFORMAÇÕES ADQUIRIDAS:

#Adicionando um regressor externo explicativo para a variância, o índice LLR defasado:
modelo_egarch <- ugarchspec(
        variance.model = list(model = "eGARCH", garchOrder = c(1,1), external.regressors = as.matrix(basedados3$llr_defasado)),
        mean.model = list(armaOrder = c(3,0))
)

modelo_egarch_fit <- ugarchfit(spec = modelo_egarch, data = basedados3$difference)
modelo_egarch_fit #Notemos que TODOS os parâmetros são significantes a 1%.
#Inclusive, conseguimos um parâmetro negativo para o regressor externo (como esperado)


### TESTE DE EFEITO ARCH 
aTSA::arch.test(arima(x = basedados3_log_selic_ts, order = c(3,1,0)))
arch.test(y = basedados3_log_selic_ts, arch = c("box","Lm"), alpha = 0.05,lag.max = 2)
#Para ambos os testes indicam presença de Efeito ARCH na série!!!

