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

### CARREGANDO BASEDADOS2
basedados2 <- openxlsx::read.xlsx(xlsxFile = "basedados_monografia.xlsx", 
                                  sheet = "BASEDADOS_2", 
                                  detectDates = TRUE,
                                  colNames = TRUE, 
) %>% as.tibble()

basedados2_ts <- ts(data = basedados2$selic_media_mes,
                   start = c(2000,1), 
                   end = c(2022,9), 
                   frequency = 12
                   )

autoplot(basedados2_ts)

selic_ <- bd$selic100_media_mes %>% 
        diff()


######--------------------------------------------------------------------------

### TESTES ESTATÍSTICOS ###


### GERANDO AUTORREGRESSIVO PARA SELIC (ARIMA)


#Verificando função de ACF e ACFP
forecast::Acf(basedados2_ts)
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

#Calculando o p-valor dos parâmetros ARI (drift = constant)
(1-pnorm(abs(ARI$coef)/sqrt(diag(ARI$var.coef))))*2


forecast::auto.arima(basedados2_ts) #Retorna ARIMA(3,1,3)

#verificar se resíduos são ruído branco
resid_AR <- ts(AR$residuals)

#Teste Ljung-Box
Box.test(x = resid_AR, type = "Ljung", lag = 1) #Não rejeitamos a H0, ou seja, o resíduo é ruído branco
forecast::Acf(resid_AR)


gar <- ugarchspec(variance.model = list(model = "eGARCH", 
                                        garchOrder = c(1,1)),
                  mean.model = list(armaOrder = c(3,1,1)),
                  distribution.model = "norm")
gar_fit <- ugarchfit(spec = gar, data = selic_)






