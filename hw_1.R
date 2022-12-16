#Candidate number: 54338

library(reshape2)
library(lubridate)
library(tseries)
library(car)

setwd("D:/Master_LSE/FM442/hw1")
data <- read.csv("homework_1.csv")


#Basic definations
data$Unadjusted_PRC <- data$PRC
data$Adjusted_PRC <- data$PRC / data$CFACPR

PRC <- dcast(data, date ~ PERMNO, value.var = "Adjusted_PRC")
names(PRC) <- c("date", "SOHU", "NTES")

RET <- dcast(data, date ~ PERMNO, value.var = "RET")
names(RET) <- c("date", "SOHU", "NTES")

PRC$date <- ymd(PRC$date)

Y <- log(1 + RET[,2:3])
Y$date <- RET$date
Y$date <- ymd(Y$date)


#Plots of prices and returns
plot(PRC$date, PRC$SOHU, type = "l", main = "Prices for SOHU and NTES", 
     ylab = "Price", xlab = "Date", col = "red", ylim = c(0, 150))

lines(PRC$date, PRC$NTES, col = "blue")

legend("topright",legend = c("SOHU", "NTES"), col = c("red", "blue"), lty=1)

par(mfrow = c(1,2))
for (i in 1:2) { 
  plot(Y$date, Y[,i], type = "l", ylab = "Returns", xlab = "Date", col = "red",
       main = paste("Returns for", names(Y)[i]))
}
summary(PRC)
summary(Y)

SOHU <- subset(RET, select = "SOHU")
NTES <- subset(RET, select = "NTES")
names(SOHU) <- 'returns'
names(NTES) <- 'returns'
SOHU$date <- RET$date
NTES$date <- RET$date


# Rejection of normal distribution visually
SOHU_mean <- mean(SOHU$returns)
SOHU_sd <- sd(SOHU$returns)
paste0("Mean: ", round(SOHU_mean,3))
paste0("Standard Deviation: ", round(SOHU_sd,3))

hist(SOHU$returns, freq = FALSE, main = "Returns of SOHU", col = "lightgrey", breaks = 50)

x <- seq(-3,3,0.001)
lines(x, dnorm(x, mean = SOHU_mean, sd = SOHU_sd), lwd = 3, col = "red")

NTES_mean <- mean(NTES$returns)
NTES_sd <- sd(NTES$returns)
paste0("Mean: ", round(NTES_mean,3))
paste0("Standard Deviation: ", round(NTES_sd,3))

hist(NTES$returns, freq = FALSE, main = "Returns of NTES", col = "lightgrey", breaks = 50)

x <- seq(-3,3,0.001)
lines(x, dnorm(x, mean = NTES_mean, sd = NTES_sd), lwd = 3, col = "red")


# qqPlot of normal distribution and student-t distribution
par(mfrow = c(2,2))
qqPlot(SOHU$returns, distribution = "norm", envelope = FALSE, main = "Normal distribution")

qqPlot(SOHU$returns, distribution = "t", df = 2, envelope = FALSE,
       main = "2 Degrees of Freedom")
qqPlot(SOHU$returns, distribution = "t", df = 3, envelope = FALSE,
       main = "3 Degrees of Freedom")
qqPlot(SOHU$returns, distribution = "t", df = 4, envelope = FALSE,
       main = "4 Degrees of Freedom")

qqPlot(NTES$returns, distribution = "norm", envelope = FALSE, main = "Normal distribution")

qqPlot(NTES$returns, distribution = "t", df = 2, envelope = FALSE,
       main = "2 Degrees of Freedom")
qqPlot(NTES$returns, distribution = "t", df = 3, envelope = FALSE,
       main = "3 Degrees of Freedom")
qqPlot(NTES$returns, distribution = "t", df = 4, envelope = FALSE,
       main = "4 Degrees of Freedom")


