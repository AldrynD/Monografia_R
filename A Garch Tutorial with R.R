###### 00-Prepare_Computer.R ########

# A Garch Tutorial with R - R Script for Preparing Computer
# Paper at <https://rac.anpad.org.br/index.php/rac/article/view/1420>
#
# This script will install all missing dependencies for the R code related to article
# "A Garch Tutorial in R" <link-to-paper>. If this is the first time running a R script, 
# make sure you got the right software: 
#
# 1) [required] Install latest R version <https://www.r-project.org/>
# 2) [optional] Install latest RStudio <https://rstudio.com/products/rstudio/download/>
# 
# Preferably, you should execute all scripts in RStudio. If you use other IDE (or none), 
# make sure to change the path for setwd() in all scripts.
#
# LINUX users: 1) Install additional libraries: libssl-dev, libxml2-dev, libcurl4-openssl-dev
#                 In terminal (control+alt+t): "sudo apt install libssl-dev libxml2-dev libcurl4-openssl-dev"
#              2) Just execute this script to install required libraries
#
# WINDOWS users: 1) Simply execute this script to install required libraries
#
# MAC users: 1) Simply execute this script to install required libraries
# 
# Alternatively, you can execute the code using RStudio Cloud at the following,
# public available, project: https://rstudio.cloud/project/1371589


# list of required packages
required_pkgs <- c('tidyverse'      , 'ggtext'    , 'rugarch', 
                   'BatchGetSymbols', 'GetBCBData', 'cowplot', 
                   'purrr'          , 'tidyr'     , 'FinTS'  , 
                   'scales'         , 'texreg'    , 'knitr'  , 
                   'kableExtra'     , 'forecast'  , 'writexl')

# finds installed pkgs from R session
installed_pkgs <- installed.packages()

# find missing packages
missing_pkgs <- required_pkgs[!(required_pkgs %in% installed_pkgs[, 1])]

if (length(missing_pkgs) == 0 ) {
        message("No missing dependencies. You are good to go!")
} else {
        install.packages(missing_pkgs)
        
        message("All packages installed. You are good to go!")
        
}

message("You should now execute the first script: 01-Get_Index_Data.R")



################################################################################
################################################################################

###### 01-Get_Index_Data.R ######


# A Garch Tutorial with R - Get Index data from Yahoo Finance
# Paper at <https://rac.anpad.org.br/index.php/rac/article/view/1420>
#
# This script will import price data for market index Ibovespa (or any other) from Yahoo Finance.
# 
# The resulting dataset is serialized (saved) in a rds file named data/RAC-GARCH-Data.rds,
# to be used in the next step.

## MAIN OPTIONS (fell free to edit it)

first_date <- '2000-01-01' # first date in sample ("2000-01-01" in paper)
last_date <- '2020-06-15' # set Sys.Date() for current date ("2020-06-01" in paper)
my_ticker <- '^BVSP' # Ibovespa ticker (fell free to change to any 
# other from YFinance: ^GSCP, ^FTSE, ITSA3.SA
# head over to https://finance.yahoo.com/ for more tickers
series_name <- 'Ibovespa' # Name of index/stock that will show up in all plots

## END OPTIONS

# load required libraries
library(BatchGetSymbols)
library(tidyverse)

# change directory to where the script located
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

# makes sure the directory "data" exists
if (!dir.exists('data')) dir.create('data')

# download price data for "my_ticker"
l_out <- BatchGetSymbols(tickers = my_ticker, 
                         first.date = first_date, 
                         last.date = last_date)

# select columns and calculated log_ret and arim_ret
df_prices <- l_out$df.tickers %>%
        select(ref.date, ticker, price.adjusted) %>%
        mutate(log_ret = log(price.adjusted/dplyr::lag(price.adjusted) ),
               arim_ret = price.adjusted/dplyr::lag(price.adjusted) - 1,
               series_name = series_name) %>%
        na.omit() # remove all NA values

# save data into file
rds_out <- 'RAC-GARCH-Data.rds'
write_rds(df_prices, rds_out)

################################################################################
################################################################################


# A Garch Tutorial with R - Create Descriptive Figure
# Paper at <https://rac.anpad.org.br/index.php/rac/article/view/1420>
#
# This script will use the financial data from previous script and, additionally, 
# import inflation data from the Brazilian Central Bank Database
# <https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries> 
# , producing Figure 01 at the end of its execution


# Functions usages

my_perc <- function(x) {
        require(scales)
        x <- scales::percent(x, accuracy = 0.01)
}


# OPTIONS
n_largest <- 10 # number of largest absolute returns to plot

# END OPTIONS

# load libraries
library(cowplot)
library(tidyverse)
library(GetBCBData)
library(forecast)

# close all existing plot windows
graphics.off()

# change directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

# make sure folder "fig" exists
if (!dir.exists('figs')) dir.create('figs')

# source functions 
source('fcts/garch_fcts.R')

# get price data
df_prices <- read_rds('RAC-GARCH-Data.rds')
series_name <- df_prices$series_name[1]

# get inflation data

log_ret <- log(diff())

df_inflation <- gbcbd_get_series(id = 433, first.date = min(df_prices$ref.date), 
                                 last.date = max(df_prices$ref.date)) %>%
        mutate(inf_index  = cumprod(1+value/100))

total_ibov_ret <- last(df_prices$price.adjusted)/first(df_prices$price.adjusted)-1
total_inflation <- last(df_inflation$inf_index)/first(df_inflation$inf_index) - 1 
n_years <- as.numeric(max(df_prices$ref.date) - min(df_prices$ref.date))/365
ret_ibov_year = (1+total_ibov_ret)^(1/n_years) - 1
ret_inflation_year = (1+total_inflation)^(1/n_years) - 1

real_ret_ibov <- (1+total_ibov_ret)/(1+total_inflation) - 1
real_ret_ibov_year <- (1+ret_ibov_year)/(1+ret_inflation_year) - 1

# create first plot
p1 <- ggplot(df_prices, aes(x = ref.date, y = price.adjusted)) + 
        geom_line() + 
        labs(title = paste0('Prices of ', series_name),
             subtitle = paste0('Total nominal arithmetic return equals to ', 
                               my_perc(total_ibov_ret),
                               ' (', my_perc(ret_ibov_year), ' per year)\n',
                               'Total real return, adjusted for inflation, equals to ',
                               my_perc(real_ret_ibov), 
                               ' (', my_perc(real_ret_ibov_year), ' per year)'),
             x = '',
             y = 'Index Value',
             caption = 'Data from Yahoo Finance') + 
        theme_bw(base_family = "TT Times New Roman") 

# calculate largest absolute price variations
largest_tab <- df_prices %>%
        group_by(ticker) %>%
        top_n(abs(log_ret), n = n_largest)

# create second plot
p2 <- ggplot(df_prices, 
             aes(x = ref.date, y = log_ret)) + 
        geom_line() + 
        labs(title = paste0('Nominal Daily Log Returns of '),#, series_name),
             subtitle = paste0('Red circles represent the largest #, n_largest, 
                               absolute price variations in the s,ample'),
             x = '',
             y = 'Log Returns',
             caption = 'Data from Yahoo Finance') + 
        theme_bw(base_family = "TT Times New Roman") +
        geom_point(data = largest_tab, aes(x = ref.date, y = log_ret), 
                   size = 3, color = 'red'  ) +
        scale_y_continuous(labels = scales::percent) + 
        labs(size = 'Absolute Price Variation') # + 
scale_color_brewer(palette = 'BrBG')

# bind plots together
p <- plot_grid(p1, p2, nrow = 2, 
               labels = 'AUTO')

# show and save
# ERROR in old Code: invalid 'bg' value
#x11() ; p ; ggsave(filename = paste0('figs/fig02_', series_name, '_prices_returns.png'), 
#                  plot = p) 

x11() ; p1 ; ggsave(filename = paste0('figs/fig02a_', series_name, '_prices.png'), 
                    plot = p1)
x11() ; p2 ; ggsave(filename = paste0('figs/fig02b_', series_name, '_returns.png'), 
                    plot = p2)

# build autocorrelagram
p <- ggAcf(x = df_prices$log_ret, lag.max = 10) +
        labs(title = paste0('Autocorrelogram for the Log Returns of ', series_name)) +
        theme_bw(base_family = "TT Times New Roman")

x11()  ; p ; ggsave('figs/fig03_autocorrelation_logret.png')