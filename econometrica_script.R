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
library(fable)
library(fabletools)
library(feasts)


### Selecionando base de dados

bd <- openxlsx::read.xlsx(xlsxFile = "basedados_monografia.xlsx", 
                                  sheet = "BASEDADOS_2", 
                                  detectDates = TRUE,
                                  colNames = TRUE, 
)

bd_ts <- ts(data = bd$selic_media_mes, start = c(2000,1), end = c(2022,3), frequency = 12)
autoplot(bd_ts)


### GERANDO AUTORREGRESSIVO PARA SELIC

#Verificando função de ACF e ACFP
forecast::Acf(bd_ts)
forecast::Pacf(bd_ts) #Truncamento em 3 (autorregressivo de ordem 3)
ggtsdisplay(bd_ts)

#Verificando se a série é estacionária | Dickey-Fuller
tseries::adf.test(bd_ts) #p-valor > 1% --> Série não estacionária

forecast::ndiffs(bd_ts) #Necessário primeira diferença para estacionarizar

#Estimando modelo ARI(1,1)
AR <- forecast::Arima(y = bd_ts, order = c(1,1,0))
summary(AR)

#verificar se resíduos são ruído branco
resid_AR <- ts(AR$residuals)

#Teste Ljung-Box
Box.test(x = resid_AR, type = "Ljung", lag = 1) #Não rejeitamos a H0, ou seja, o resíduo é ruído branco
forecast::Acf(resid_AR)



