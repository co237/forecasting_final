library(tseries)
library(forecast)
library(vars)
library(tidyverse)
library(lubridate)

# Load Data
jobs_data <- read_csv("/Users/connorobrien/Downloads/jobs_data.csv")

ndiffs(jobs_data$Jobs)
ndiffs(jobs_data$CPI)
ndiffs(jobs_data$Chi_Fed_NFCI)
ndiffs(jobs_data$NBER_Recession)
ndiffs(jobs_data$T10Y2YM)
ndiffs(jobs_data$Ten_yr_Treasury)
ndiffs(jobs_data$Industrial_Production)
ndiffs(jobs_data$Population)
ndiffs(jobs_data$PCE)

# Format Date
jobs_data$Month <- as.Date(jobs_data$Month, "%m/%d/%Y")
# 1976 onwards 
jobs = jobs_data[-c(1:449),]
jobs <- jobs2[ c(1,2,3,4,5,6,8,9,10,11,12,13,14) ]
# Create Time Series
jobs_national <- ts(jobs$Jobs,
                    start = c(1976,1),
                    end = c(2020, 4),
                    frequency = 12)

tenyr <- ts(jobs$Ten_yr_Treasury,
             start = c(1976,1),
             end = c(2020, 4),
             frequency = 12)

recession <- ts(jobs$NBER_Recession,
                      start = c(1976,1),
                      end = c(2020, 4),
                      frequency = 12)

population <- ts(jobs$Population,
                start = c(1976,1),
                end = c(2020, 4),
                frequency = 12)

Ind_Production <- ts(jobs$Industrial_Production,
                 start = c(1976,1),
                 end = c(2020, 4),
                 frequency = 12)

financial_conditions <- ts(jobs$Chi_Fed_NFCI,
                     start = c(1976,1),
                     end = c(2020, 4),
                     frequency = 12)

cpi <- ts(jobs$CPI,
                           start = c(1976,1),
                           end = c(2020, 4),
                           frequency = 12)

treasury_spread <- ts(jobs$T10Y2YM,
          start = c(1976,1),
          end = c(2020, 4),
          frequency = 12)

# Plot Time Series
plot(jobs_national)
plot(tenyr)
plot(recession)
plot(population)
plot(Ind_Production)
plot(financial_conditions)
plot(cpi)
plot(treasury_spread)

ndiffs(cpi)
ndiffs(treasury_spread)
ndiffs(financial_conditions)
ndiffs(jobs_national)
ndiffs(recession)
ndiffs(tenyr)
ndiffs(population)

kpss.test(jobs_d2)
kpss.test(tenyr)

library(dynlm)

acf(tenyr)

VAR_EQ1 <- dynlm(jobs_national ~ L(jobs_national, 1:2) + L(tenyr, 1:2) + L(recession, 1:2) + L(population,1:2) + L(Ind_Production, 1:2) + L(financial_conditions, 1:2) + L(cpi, 1:2) + L(treasury_spread, 1:2), 
                 start = c(1976, 1), 
                 end = c(2021, 4))
VAR_EQ2 <- dynlm(jobs_national ~ L(jobs_national, 1:2) + L(tenyr, 1:2) + L(recession, 1:2) + L(population,1:2) + L(Ind_Production, 1:2) + L(financial_conditions, 1:2) + L(cpi, 1:2) + L(treasury_spread, 1:2), 
                 start = c(1976, 1), 
                 end = c(2021, 4))
VAR_EQ3 <- dynlm(jobs_national ~ L(jobs_national, 1:2) + L(tenyr, 1:2) + L(recession, 1:2) + L(population,1:2) + L(Ind_Production, 1:2) + L(financial_conditions, 1:2) + L(cpi, 1:2) + L(treasury_spread, 1:2), 
                 start = c(1976, 1), 
                 end = c(2021, 4))
VAR_EQ4 <- dynlm(jobs_national ~ L(jobs_national, 1:2) + L(tenyr, 1:2) + L(recession, 1:2) + L(population,1:2) + L(Ind_Production, 1:2) + L(financial_conditions, 1:2) + L(cpi, 1:2) + L(treasury_spread, 1:2), 
                 start = c(1976, 1), 
                 end = c(2021, 4))
VAR_EQ5 <- dynlm(jobs_national ~ L(jobs_national, 1:2) + L(tenyr, 1:2) + L(recession, 1:2) + L(population,1:2) + L(Ind_Production, 1:2) + L(financial_conditions, 1:2) + L(cpi, 1:2) + L(treasury_spread, 1:2), 
                 start = c(1976, 1), 
                 end = c(2021, 4))
VAR_EQ6 <- dynlm(jobs_national ~ L(jobs_national, 1:2) + L(tenyr, 1:2) + L(recession, 1:2) + L(population,1:2) + L(Ind_Production, 1:2) + L(financial_conditions, 1:2) + L(cpi, 1:2) + L(treasury_spread, 1:2), 
                 start = c(1976, 1), 
                 end = c(2021, 4))
VAR_EQ7 <- dynlm(jobs_national ~ L(jobs_national, 1:2) + L(tenyr, 1:2) + L(recession, 1:2) + L(population,1:2) + L(Ind_Production, 1:2) + L(financial_conditions, 1:2) + L(cpi, 1:2) + L(treasury_spread, 1:2), 
                 start = c(1976, 1), 
                 end = c(2021, 4))
VAR_EQ8 <- dynlm(jobs_national ~ L(jobs_national, 1:2) + L(tenyr, 1:2) + L(recession, 1:2) + L(population,1:2) + L(Ind_Production, 1:2) + L(financial_conditions, 1:2) + L(cpi, 1:2) + L(treasury_spread, 1:2), 
                 start = c(1976, 1), 
                 end = c(2021, 4))

names(VAR_EQ1$coefficients) <- c("Intercept","Jobs_t-1", 
                                 "Jobs_t-2", "Tyield_t-1", "Tyield_t-2")
names(VAR_EQ2$coefficients) <- names(VAR_EQ1$coefficients)

coeftest(VAR_EQ1, vcov. = sandwich)

coeftest(VAR_EQ2, vcov. = sandwich)

VAR_data <- window(ts.union(jobs_national, tenyr, recession, population, Ind_Production, financial_conditions, cpi, treasury_spread), start = c(1976, 1), end = c(2020, 4))
library(vars)
VAR_est <- VAR(VAR_data, p = 2)
VAR_est     
library(MTS)

VARMA_est = VARMA(y = VAR_data, p = 2, q = 2)
         
forecasts <- predict(VAR_est, h = 12)          
plot(forecasts)


