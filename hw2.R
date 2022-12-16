# Candidate number: 54338
setwd('D:/Master_LSE/FM442/hw2')
library(reshape2)
library(lubridate)
data <- read.csv("DAL.csv")
head(data)
# Keeping the unadjusted prices in a new column
data$Unadjusted_PRC <- data$PRC

# Modifying the PRC column
data$Adjusted_PRC <- data$PRC / data$CFACPR

# Getting the date and Adjusted_PRC variables 
DAL <- data[data$PERMNO == 26112, c("date", "Adjusted_PRC")]

# Renaming Adjusted_PRC 
names(DAL)[2] <- "DAL"


PRC <- dcast(data, date ~ PERMNO, value.var = "Adjusted_PRC")
names(PRC) <- c("date", "DAL")
head(PRC)
RET <- dcast(data, date ~ PERMNO, value.var = "RET")
names(RET) <- c("date",  "DAL")
head(RET)

# And transform them into a new Y data frame
Y <- log(1 + RET[,1:2])
Y$date <- RET$date
head(Y)
class(Y$date)
# Save the original int type date to int_date
Y$int_date <- Y$date
# Use the function ymd() to transform the column into Dates
Y$date <- ymd(Y$date)



# Saving the data frame of returns
save(Y, file = "Y.RData")
head(Y)

############################## Specify tGARCH ##################################
library(rugarch)
library(tseries)
spec1 <- ugarchspec(
  variance.model = list(garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "std"
)
# Save the returns in a variable called y
y <- Y$DAL
# Simple plot
plot(Y["date"], y, col = "blue", type = "l", main = "Returns for DAL", ylab = "Return", xlab = "Date")
# Fit the model
tGARCH <- ugarchfit(spec = spec1, data = y)
# Create 1000 simulations from our fitted model
# Steps inside the function:
# tGARCH spec
# Probability, here 0.05
# Portfolio value, here 1000
# Estimation window, here 1000
S = 1000
T_sample = 1000
portfolio_value = 1000
p = 0.05

simulations <- ugarchsim(tGARCH, m.sim = S)

# Plot the simulated returns
matplot(simulations@simulation$seriesSim, type = "l",
        main = "Simulations of returns", ylab = "Return")
# Plot the simulated volatilities
matplot(simulations@simulation$sigmaSim, type = "l", 
        main = "Simulations of volatility", ylab = "Volatility")
# For every simulation and store the VAR and ES
VaR <- c()
Es <- c()
for (j in 1:S){
  #get the vector of returns of jth simulation
  y_simulation <- simulations@simulation$seriesSim[,j]
# Sort the values in y
  ys <- sort(y_simulation)
# Find the place of VaR
  place <- ceiling(T_sample*p)
# Allocate the VaR to the matrix
  VaR[j] <- -ys[place] * portfolio_value
# Allocate the ES to the matrix
  Es[j] <- -mean(ys[1:place]) * portfolio_value
}

# Hist the VaR and Es respectively
hist(VaR, breaks = 25, main = "Histogram of VaR", col = "lightgrey", ylim = c(0, 150))
hist(Es, breaks = 25, main = "Histogram of Es", col = "lightgrey", ylim = c(0, 150))

# Find the intervals for 95% confidence level
sort(VaR)
sort(Es)
confi_bound_VaR_low = VaR[25]
confi_bound_VaR_high = VaR[975]
confi_bound_Es_low = Es[25]
confi_bound_Es_high = Es[975]




















