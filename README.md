# Assignment1
ECON4490

# remove
rm(plot_data_monthly)
# read + subset 2 times
econ23 <- read.csv('~/Downloads/econ1.csv')
econ23_subset <- subset(econ23, select= -c(SYMBOL))
econ23_subset <- subset(econ23, select= -c(GEO, TERMINATED, DECIMALS))
econ23_sub2 <- subset(econ23, select= c(REF_DATE, Products.and.product.groups, VALUE))
colnames(econ23)
econ23_sub2revamp <- subset(econ23_sub2$Products.and.product.groups == "All-items")
unique(econ23_sub2$Products.and.product.groups)
econ23_sub2revamp <- subset(econ23_sub2, select= c(econ23_sub2$Products.and.product.groups == "All-items"))

# install required packages
install.packages("tidyverse")
library(tidyverse)

# filter the data and extract All-items and All-item excluding food and energy
dataagain <- econ23_sub2 %>% filter(
  Products.and.product.groups %in% c("All-items", "All-items excluding food and energy" )
)
data_headline <- econ23_sub2 %>% filter(
  Products.and.product.groups %in% c("All-items")
)
data_core <- econ23_sub2 %>% filter(
  Products.and.product.groups %in% c("All-items excluding food and energy")
)

# plot headline and core together - monthly
par(mfrow = c(1,1))
plot.ts(data_core$VALUE, main= "TS Core Inflation")
lines(data_headline$VALUE, main= "TS Headline Inflation")
legend()
plot.ts(data_headline$VALUE, main= "TS Headline Inflation")

data_headline2 <- subset(data_headline, select = c("REF_DATE", "VALUE"))
data_core2 <- subset(data_core, select = c("REF_DATE", "VALUE"))

rm(data_headline)
rm(data_core)

# Convert 'REF_DATE' to Date type
data_headline2$REF_DATE <- as.Date(paste(data_headline2$REF_DATE, "01", sep = "-"), format = "%Y-%m-%d")
data_core2$REF_DATE <- as.Date(paste(data_core2$REF_DATE, "01", sep = "-"), format = "%Y-%m-%d")

data_headline2$REF_DATE <- as.Date(data_headline2$REF_DATE)
data_core2$REF_DATE <- as.Date(data_core2$REF_DATE)


# monthly frequency 

# Create time series objects
ts_headline <- ts(data_headline2$VALUE, start = start(data_headline2$REF_DATE), frequency = 12)
ts_core <- ts(data_core2$VALUE, start = start(data_core2$REF_DATE), frequency = 12)

library(dplyr)

# Calculate Year-on-Year rates using dplyr's lag()
yoy_monthly_headline <- 100 * (ts_headline / lag(ts_headline, order_by = data_headline2$REF_DATE) - 1)
yoy_monthly_core <- 100 * (ts_core / lag(ts_core, order_by = data_core2$REF_DATE) - 1)


# Plot the Year-on-Year rates at monthly frequency
par(mfrow = c(1, 1))
plot(data_headline2$REF_DATE, yoy_monthly_headline, type = "l", col = "blue", lwd = 2, main = "Monthly CPI Inflation (YoY)", ylab = "Inflation Rate")
lines(data_core2$REF_DATE, yoy_monthly_core, col = "red", lwd = 2)
legend("topright", legend = c("Headline Inflation", "Core Inflation"), col = c("blue", "red"), lty = 1)

library(dplyr)
install.packages(c("dplyr", "quantmod"))
library(dplyr)
library(quantmod)


# Convert 'REF_DATE' to Date type
data_headline2$REF_DATE <- as.Date(data_headline2$REF_DATE)
data_core2$REF_DATE <- as.Date(data_core2$REF_DATE)

# Create time series objects
ts_headline <- ts(data_headline2$VALUE, start = start(data_headline2$REF_DATE), frequency = 12)
ts_core <- ts(data_core2$VALUE, start = start(data_core2$REF_DATE), frequency = 12)

# Calculate Year-on-Year rates using dplyr's lag() - this worked after quantmod
yoy_monthly_headline <- 100 * (ts_headline / lag(ts_headline) - 1)
yoy_monthly_core <- 100 * (ts_core / lag(ts_core) - 1)

# Calculate Year-on-Year rates using delta
yoy_monthly_headline <- Delt(ts_headline, type = "arithmetic", k = 12) * 100
yoy_monthly_core <- delta(ts_core, type = "arithmetic", k = 12) * 100


# Plot the Year-on-Year rates at monthly frequency
par(mfrow = c(1, 1))
plot(data_headline2$REF_DATE, yoy_monthly_headline, type = "l", col = "blue", lwd = 2, main = "Monthly CPI Inflation (YoY)", ylab = "Inflation Rate")
lines(data_core2$REF_DATE, yoy_monthly_core, col = "red", lwd = 2)
legend("topright", legend = c("Headline Inflation", "Core Inflation"), col = c("blue", "red"), lty = 1)



#Plot the two Year-on-Year (headline and core) at monthly 
#frequency in the same chart.
# Plot the returns at monthly frequency
par(mfrow = c(1, 1))
plot.ts(yoy_monthly_headline, main= "Year-on-Year (headline and core) at monthly ", type= "l", col= "red", lwd= 1)
lines(yoy_monthly_core, col = "blue", lwd = 1)
legend("bottomright", legend = c("Headline Inflation", "Core Inflation"), col = c("red", "blue"), lty = 1)




# Create time series objects
ts_headline_q <- ts(data_headline2$VALUE, start = start(data_headline2$REF_DATE), frequency = 4)
ts_core_q <- ts(data_core2$VALUE, start = start(data_core2$REF_DATE), frequency = 4)


library(dplyr)
library(quantmod)

# Calculate Year-on-Year rates using delta
yoy_quarterly_headline_q <- Delt(ts_headline, type = "arithmetic", k = 4) * 100
yoy_quarterly_core_q <- Delt(ts_core, type = "arithmetic", k = 4) * 100

# Plot the Year-on-Year rates at quarterly frequency
par(mfrow = c(1, 1))
plot.ts(yoy_quarterly_headline_q, main= "Year-on-Year (headline and core) at Quarterly ", type= "l", col= "red", lwd= 1)
lines(yoy_quarterly_core_q, col = "blue", lwd = 1)
legend("bottomright", legend = c("Headline Inflation", "Core Inflation"), col = c("red", "blue"), lty = 1)

# Calculate mean and standard deviation
mean_headline <- mean(yoy_quarterly_headline, na.rm = TRUE)
sd_headline <- sd(yoy_quarterly_headline, na.rm = TRUE)

mean_core <- mean(yoy_quarterly_core, na.rm = TRUE)
sd_core <- sd(yoy_quarterly_core, na.rm = TRUE)

# Print the results
cat("Mean and Standard Deviation for Headline Inflation:\n")
cat("Mean: ", mean_headline, "\n")
cat("Standard Deviation: ", sd_headline, "\n\n")

cat("Mean and Standard Deviation for Core Inflation:\n")
cat("Mean: ", mean_core, "\n")
cat("Standard Deviation: ", sd_core, "\n")

# Calculate mean and standard deviation
mean_headline <- mean(yoy_quarterly_headline, na.rm = TRUE)
sd_headline <- sd(yoy_quarterly_headline, na.rm = TRUE)

mean_core <- mean(yoy_quarterly_core_q, na.rm = TRUE)
sd_core <- sd(yoy_quarterly_core_q, na.rm = TRUE)

# Print the results
cat("Mean and Standard Deviation for Headline Inflation:\n")
cat("Mean: ", mean_headline, "\n")
cat("Standard Deviation: ", sd_headline, "\n\n")

cat("Mean and Standard Deviation for Core Inflation:\n")
cat("Mean: ", mean_core, "\n")
cat("Standard Deviation: ", sd_core, "\n")
