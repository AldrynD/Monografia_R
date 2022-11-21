### RODANDO O MODELO EGARCH ###

################################################################################
#The solution is to use another package called rugarch.

install.packages("rugarch")
require(rugarch)
#Let's construct a (Tx2) matrix called data, where column 1 is Yt and column 2 is Xt.

teste <- cbind(rnorm(1000),rnorm(1000))
#We can then compute the GARCH(1,1) model that I described in my question:

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), 
submodel = NULL, external.regressors = NULL, variance.targeting = FALSE), 
mean.model = list(armaOrder = c(0, 0), external.regressors = matrix(teste[,2]), 
distribution.model = "norm", start.pars = list(), fixed.pars = list()))

garch <- ugarchfit(spec=spec,data=teste[,1],solver.control=list(trace=0))
#Comparing this to EViews: The R coefficient estimates that are greater than 0.01 differ 
#to the EViews estimates by approximately 1−2%, which is acceptable in my mind. 
#t-statistics differ by more but it doesn't effect inference.

rugarch::plot(garch)

################################################################################

### PACOTES UTILIZADOS ###
library(tidyverse)
library(openxlsx)
library(forecast)
library(tseries)
library(tsibble)
library(feasts)
library(e1071)
library(rugarch)
library(equatiomatic)
library(aTSA)
library(nortsTest)

### CARREGANDO BASEDADOS2
basedados2 <- openxlsx::read.xlsx(xlsxFile = "basedados_monografia.xlsx", 
                                  sheet = "BASEDADOS_2", 
                                  detectDates = TRUE,
                                  colNames = TRUE
) %>% 
        dplyr::mutate(log_selic = log(selic_media_mes)) %>% 
        dplyr::mutate(difference = log_selic - lag(log_selic, 1)) %>% 
        select(periodo, log_selic)

a_dif <- diff(basedados2$log_selic) %>% as.ts()
qqnorm(a_dif)
qqline(a_dif)
forecast::Acf(a_dif)
forecast::Pacf(a_dif)

a_dif2 <- a_dif^2
forecast::Acf(a_dif2)
forecast::Pacf(a_dif2)

a_arima <- forecast::Arima(y = a_dif, order = c(2,0,0), include.constant = TRUE)
summary(a_arima)

Box.test(x = a_arima$residuals, type = "Ljung", lag = 1) 
forecast::Acf(a_arima$residuals) 




a_modelo_egarch <- ugarchspec(
        variance.model = list(model = "eGARCH", garchOrder = c(1,1), external.regressors = as.matrix(basedados$llr)),
        mean.model = list(armaOrder = c(2,0))
)

a_gar_fit <- ugarchfit(spec = a_modelo_egarch, data = a_dif)




















#basedados2_ts <- ts(data = basedados2$selic_media_mes,
                  # start = c(2000,1),
                   #end = c(2022,9),
                   #frequency = 12
                   #)

#autoplot(basedados2_ts)

#selic_ <- basedados2$selic_media_mes %>% 
        #diff()


######--------------------------------------------------------------------------

### TESTES ESTATÍSTICOS ###


### GERANDO AUTORREGRESSIVO PARA SELIC (ARIMA)


#Verificando função de ACF e ACFP
forecast::Acf(a_dif)
forecast::Pacf(basedados2_ts) #Truncamento em 3 (indicando um autorregressivo de ordem 3 - AR(3))
ggtsdisplay(basedados2_ts)


#Verificando estacionaridade | Teste Dickey-Fuller
tseries::adf.test(basedados2_ts) #p-valor > 1% --> Série não estacionária

forecast::ndiffs(basedados2_ts) #Necessário primeira diferença para estacionarizar

#Estimando modelo ARI(3,1)
ARI <- forecast::Arima(y = basedados2_ts,
                       order = c(3,1,0),
                       include.constant = TRUE)
summary(ARI)


resid2 <- ARI_resid^2
Box.test(x = resid2, type = "Ljung") ## Rejeitamos a H0, então tem efeito ARCH
# e que devemos modelar a volatilidade.
forecast::Acf(resid2) 





#Calculando o p-valor dos parâmetros ARI (drift = constant)
ARI_pvalue <- (1-pnorm(abs(ARI$coef)/sqrt(diag(ARI$var.coef))))*2

##verificando se resíduos são ruído branco | Ljung-Box Test
ARI_resid <- ts(ARI$residuals)

Box.test(x = ARI_resid, type = "Ljung", lag = 1) #Não rejeitamos a H0, ou seja, o resíduo é ruído branco
forecast::Acf(ARI_resid)


######--------------------------------------------------------------------------

### ESTIMANDO MODELO EGARCH ###

modelo_egarch <- ugarchspec(
        variance.model = list(model = "eGARCH", garchOrder = c(1,1), external.regressors = as.matrix(cred_llr)),
        mean.model = list(armaOrder = c(3,1))
)

gar_fit <- ugarchfit(spec = modelo_egarch, data = selic_)


garch.spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(df$regressor)),
        mean.model = list(armaOrder = c(2, 0), include.mean = TRUE))
ugarchfit(garch.spec, df$dependent)



