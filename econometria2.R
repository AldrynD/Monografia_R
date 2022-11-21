anova_basedados <- openxlsx::read.xlsx(xlsxFile = "basedados_monografia.xlsx", 
                                 sheet = "BASEDADOS_3", 
                                 detectDates = TRUE,
                                 colNames = TRUE, 
                                 na.strings = "-"
) %>% 
        filter(periodo >= "2010-01-01" & periodo <= "2021-09-01" ) %>% 
        mutate(llr = case_when(
                anova_basedados$exp_ipca_pond < anova_basedados$inflacao_meta_inf ~ (1/(exp(anova_basedados$exp_ipca_pond - anova_basedados$inflacao_meta_inf) - (anova_basedados$exp_ipca_pond - anova_basedados$inflacao_meta_inf))),
                anova_basedados$exp_ipca_pond >= anova_basedados$inflacao_meta_inf & anova_basedados$exp_ipca_pond <= anova_basedados$inflacao_meta_sup ~ 1,
                anova_basedados$exp_ipca_pond > anova_basedados$inflacao_meta_sup ~ (1/(exp(anova_basedados$exp_ipca_pond - anova_basedados$inflacao_meta_sup) - (anova_basedados$exp_ipca_pond - anova_basedados$inflacao_meta_sup)))
        ) 
        ) %>%
        dplyr::mutate(var_selic = ((selic/lag(selic, 1))-1)*100) %>% 
        dplyr::mutate(log_selic = log(selic)) %>% 
        dplyr::mutate(difference = log_selic - lag(log_selic, 1)) %>% 
        dplyr::mutate(llr_defasado = lag(llr, 1)) %>% 
        na.omit()

anova_ts <- as.ts(anova_basedados$log_selic)

anova_ts_df <- diff(anova_ts)


qqnorm(anova_ts)
qqline(anova_ts)
forecast::Acf(anova_ts)
forecast::Pacf(anova_ts)

tseries::adf.test(anova_ts)
ndiffs(anova_ts)



anova_dif2 <- anova_basedados$difference^2
forecast::Acf(anova_dif2)
forecast::Pacf(anova_dif2)

anova_arima <- forecast::Arima(y = anova_ts, order = c(3,1,0), include.constant = TRUE)
summary(anova_arima)

#Calculando o p-valor dos parâmetros ARI (drift = constant)
anova_pvalor <- (1-pnorm(abs(anova_arima$coef)/sqrt(diag(anova_arima$var.coef))))*2
anova_pvalor

Box.test(x = anova_arima$residuals, type = "Ljung", lag = 1) 
forecast::Acf(anova_arima$residuals) 




a_modelo_egarch <- ugarchspec(
        variance.model = list(model = "eGARCH", garchOrder = c(1,1), external.regressors = as.matrix(anova_basedados$llr_defasado)),
        mean.model = list(armaOrder = c(3,0))
)

a_gar_fit <- ugarchfit(spec = a_modelo_egarch, data = anova_basedados$difference)
a_gar_fit


aTSA::arch.test(arima(x = anova_ts, order = c(2,1,0)))
arch.test(y = anova_ts, arch = c("box","Lm"), alpha = 0.05,lag.max = 2)


