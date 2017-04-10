library(dplyr)
library(ggplot2)
library(readr)

# Data is a subset of the "Government Current Receipts and Expenditures" table
# From the Bureau of Economic Analysis
# https://www.bea.gov/iTable/iTable.cfm?ReqID=9&step=1#reqid=9&step=3&isuri=1&904=1929&903=86&906=a&905=2016&910=x&911=0
data <- read_csv("us_gov_receipts_expenditures.csv", col_types="idddddddd")

# Simplify labels
colnames(data) <- c(
  "year",
  "receipts",
  "receipts.personal_taxes",
  "receipts.production_taxes",
  "receipts.transfer_payments",
  "social_benefits",
  "social_benefits.to_persons",
  "social_benefits.to_world"
) 

# CPI is from Bureau of Labor Statistics series CUUR0000SA0 annual averages
# https://data.bls.gov/timeseries/CUUR0000SA0
cpi <- read_csv("cpi.csv", col_types="id")

cpi.base <- cpi[cpi$year == 2016,]$year

data <- data %>%
  left_join(cpi, by="year")

# US population totals are from census historical and intercensal estimates
population <- read_csv("population.csv", col_types="id")

data <- data %>%
  left_join(population, by="year")

###
### MEASURES OF VALUE
###

# Nominal
data$nominal <- data$receipts.personal_taxes

# Real
data$real <- data$nominal * cpi.base / data$cpi

# In units of TKTK (price of some common good, e.g. Big Macs)
# TKTK

# Real rate per capita
data$per.capita <- data$real / data$population

# Real deviation from mean
data$dev.from.mean <- data$real - mean(data$real)

# Real deviation from base period mean
# TKTK

# Z-Scores
# TKTK

###
### MEASURES OF CHANGE
###

# Real change
data <- data %>%
  mutate(change = real - first(real))

# Real year-over-year change
data <- data %>%
  mutate(yoy = real - lag(real))

# Real annual percent change
data <- data %>%
  mutate(pct.change = real / lag(real))

# Cumulative percent change
data <- data %>%
  mutate(cum.pct.change = real / first(real))

# Real index to initial year
data <- data %>%
  mutate(index = real / (first(real) / 100))

# Real index to low year
# TKTK

# Real index to high year
# TKTK

###
### MEASURES OF PROPORTION
###

# Share of receipts
data$share.of.receipts <- data$nominal / data$receipts * 100

# Change in share of receipts
data <- data %>%
  mutate(share.change = share.of.receipts - first(share.of.receipts))

# Year-over-year change in share of receipts
data <- data %>%
  mutate(share.yoy = share.of.receipts - lag(share.of.receipts))

###
### Other
###

# Volatility
# TKTK


###
### CHARTS
###

line.chart <- function(y) {
  ggplot(data=data, aes_string(x="year", y=y)) +
    geom_line()
  
  ggsave(paste("charts/", y, ".jpg", sep=""))
}

# Nominal
line.chart("nominal")

# Real
line.chart("real")

# Per capita
line.chart("per.capita")

# Change
line.chart("change")

# Percent change
line.chart("pct.change")

# Year-over-year change
line.chart("yoy")
