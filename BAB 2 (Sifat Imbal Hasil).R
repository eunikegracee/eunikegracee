#Import Library dan Data
library(readr)
library(tidyr)
library(tseries)
library(lmtest)
library(forecast)
library(FinTS)
library(nortest)
library(fGarch)
library(xts)
library(PerformanceAnalytics)
library(rugarch)
library(quantmod)
library(moments)
library(psych)
library(ggplot2)

# SAHAM BTC
# Mengambil data harga saham 
getSymbols("BTC-USD", src = "yahoo", from = "2021-02-21", to = "2024-02-21")
data_1 <- `BTC-USD`$`BTC-USD.Close`
head(data_1) # menampilkan 5 data pertama

# Plot harga saham
plot(1:length(data_1), data_1, type = "l", main = "Harga BTC",
     xlab = "t", ylab = expression(S[t]), col = "black")
#abline(h = mean(return_1), lwd = 2, lty = 2, col = "red")

# Return Harian
return_1_notclean <- diff(log(data_1))
return_1 <- na.omit(return_1_notclean)
plot(1:length(return_1), return_1, type = "l", main = "Imbal Hasil BTC",
     xlab = "t", ylab = expression(R[t]), col = "black")
#abline(h = mean(return_1), lwd = 2, lty = 2, col = "red")

# Return kuadrat
plot(1:length(return_1), return_1^2, type = "l", main = "Imbal Hasil Kuadrat BTC",
     xlab = "t", ylab = expression(R[t]^2), col = "black")
#abline(h = mean(return_1), lwd = 2, lty = 2, col = "red")

# Menghitung compound return
data_1 <- as.numeric(data_1)
compound_return1 <- numeric(length(data_1))
for (t in 2:length(data_1)) {
  compound_return1[t] <- compound_return1[t-1] + log(data_1[t] / data_1[t-1])
}
plot(1:length(compound_return1), compound_return1, type = "l", main = "Imbal Hasil Majemuk BTC",
     xlab = "t", ylab = expression(R[t]), col = "black")

# volatilitas
# Menghitung deviasi standar return harian
sigma_t1 <- sd(return_1)
cat("Standard deviation of daily returns:", sigma_t1, "\n")

# Menghitung rolling volatility (standar deviasi bergulir)
rolling_volatility1 <- runSD(return_1, n = 30)
rolling_volatility1

# Plot imbal hasil saham dan rolling volatility
n <- length(return_1)
t <- 1:n
plot(t, return_1, type = "l", col = "black", main = "Imbal Hasil dan Volatilitas BTC", ylab = expression(R[t]), xlab = "t")
lines(t, rolling_volatility1, col = "red", lwd = 4)
legend("topright", legend = c("Imbal Hasil", "Volatilitas"), col = c("black", "red"), lty = 1, lwd = 2)

# Statistik Deskriptif
summary(return_1_notclean)
return_1 <- na.omit(return_1_notclean)
str(return_1)

# Uji Hipotesis
ad.test(return_1) # Uji Andersen-Darling
ks.test(return_1, "pnorm") # Uji Kolmogorov-Smirnov

summary(return_1)
mean_value1 <- mean(return_1)
std_dev1 <- sd(return_1)
var1 <- var(return_1)
kurtosis_value1 <- kurtosis(return_1)
skewness_value1 <- skewness(return_1)

# Tampilkan hasil
cat("Mean:", mean_value1, "\n")
cat("Variance:", var1, "\n")
cat("Standard Deviation:", std_dev1, "\n")
cat("Kurtosis:", kurtosis_value1, "\n")
cat("Skewness:", skewness_value1, "\n")

# Menampilkan plot
plot(acf(return_1, plot=FALSE), main = "Fungsi Autokorelasi Imbal Hasil BTC", ylim = c(-0.10, 0.10), lwd = 3)
plot(acf(return_1^2, plot=FALSE), main="Fungsi Autokorelasi Imbal Hasil Kuadrat BTC", ylim = c(-0.15, 0.15), lwd = 3)

# Ljung box test
Box.test(return_1, lag = 1, type = "Ljung")
Box.test(return_1^2, lag = 1, type = "Ljung")

# QQ Plot
qqnorm(return_1_notclean)
qqline(return_1_notclean)

# SAHAM ETH
# Mengambil data harga saham 
getSymbols("ETH-USD", src = "yahoo", from = "2021-02-21", to = "2024-02-21")
data_2 <- `ETH-USD`$`ETH-USD.Close`
head(data_2) # menampilkan 5 data pertama

# Plot harga saham
plot(1:length(data_2), data_2, type = "l", main = "Harga ETH",
     xlab = "t", ylab = expression(S[t]), col = "black")
#abline(h = mean(return_1), lwd = 2, lty = 2, col = "red")

# Return Harian
return_2_notclean <- diff(log(data_2))
return_2 <- na.omit(return_2_notclean)
plot(1:length(return_2), return_2, type = "l", main = "Imbal Hasil ETH",
     xlab = "t", ylab = expression(R[t]), col = "black")
#abline(h = mean(return_1), lwd = 2, lty = 2, col = "red")

# Return kuadrat
plot(1:length(return_2), return_2^2, type = "l", main = "Imbal Hasil Kuadrat ETH",
     xlab = "t", ylab = expression(R[t]^2), col = "black")
#abline(h = mean(return_1), lwd = 2, lty = 2, col = "red")

# Menghitung compound return
data_2 <- as.numeric(data_2)
compound_return2 <- numeric(length(data_2))
for (t in 2:length(data_2)) {
  compound_return2[t] <- compound_return2[t-1] + log(data_2[t] / data_2[t-1])
}
plot(1:length(compound_return2), compound_return2, type = "l", main = "Imbal Hasil Majemuk ETH",
     xlab = "t", ylab = expression(R[t]), col = "black")

# volatilitas
# Menghitung deviasi standar return harian
sigma_t2 <- sd(return_2)
cat("Standard deviation of daily returns:", sigma_t2, "\n")

# Menghitung rolling volatility (standar deviasi bergulir)
rolling_volatility2 <- runSD(return_2, n = 30)

# Plot imbal hasil saham majemuk dan rolling volatility
n <- length(return_2)
t <- 1:n
plot(t, return_2, type = "l", col = "black", main = "Imbal Hasil dan Volatilitas ETH", ylab = expression(R[t]), xlab = "t")
lines(t, rolling_volatility2, col = "red", lwd = 4)
legend("topright", legend = c("Imbal Hasil", "Volatilitas"), col = c("black", "red"), lty = 1, lwd = 2)

# Statistik Deskriptif
summary(return_2_notclean)
return_2 <- na.omit(return_2_notclean)
str(return_2)

# Uji Hipotesis
ad.test(return_2) # Uji Andersen-Darling
ks.test(return_2, "pnorm") # Uji Kolmogorov-Smirnov

summary(return_2)
mean_value2 <- mean(return_2)
std_dev2 <- sd(return_2)
var2 <- var(return_2)
kurtosis_value2 <- kurtosis(return_2)
skewness_value2 <- skewness(return_2)

# Tampilkan hasil
cat("Mean:", mean_value2, "\n")
cat("Variance:", var2, "\n")
cat("Standard Deviation:", std_dev2, "\n")
cat("Kurtosis:", kurtosis_value2, "\n")
cat("Skewness:", skewness_value2, "\n")

# Menampilkan plot
plot(acf(return_2, plot=FALSE), main="Fungsi Autokorelasi Imbal Hasil ETH", ylim = c(-0.10, 0.10))
plot(acf(return_2^2, plot=FALSE), main="Fungsi Autokorelasi Imbal Hasil Kuadrat ETH", ylim = c(-0.15, 0.15))

# Ljung box test
Box.test(return_2, lag = 1, type = "Ljung")
Box.test(return_2^2, lag = 1, type = "Ljung")

# QQ Plot
qqnorm(return_2_notclean)
qqline(return_2_notclean)

# SAHAM LTC
# Mengambil data harga saham 
getSymbols("LTC-USD", src = "yahoo", from = "2021-02-21", to = "2024-02-21")
data_3 <- `LTC-USD`$`LTC-USD.Close`
head(data_3) # menampilkan 5 data pertama

# Plot harga saham
plot(1:length(data_3), data_3, type = "l", main = "Harga LTC",
     xlab = "t", ylab = expression(S[t]), col = "black")
#abline(h = mean(return_1), lwd = 2, lty = 2, col = "red")

# Return Harian
return_3_notclean <- diff(log(data_3))
return_3 <- na.omit(return_3_notclean)
plot(1:length(return_3), return_3, type = "l", main = "Imbal Hasil LTC",
     xlab = "t", ylab = expression(R[t]), col = "black")


# Return kuadrat
plot(1:length(return_3), return_3^2, type = "l", main = "Imbal Hasil Kuadrat LTC",
     xlab = "t", ylab = expression(R[t]^2), col = "black")
#abline(h = mean(return_1), lwd = 2, lty = 2, col = "red")

# Menghitung compound return
data_3 <- as.numeric(data_3)
compound_return3 <- numeric(length(data_3))
for (t in 2:length(data_3)) {
  compound_return3[t] <- compound_return3[t-1] + log(data_3[t] / data_3[t-1])
}
plot(1:length(compound_return3), compound_return3, type = "l", main = "Imbal Hasil Majemuk LTC",
     xlab = "t", ylab = expression(R[t]), col = "black")

# volatilitas
# Menghitung deviasi standar return harian
sigma_t3 <- sd(return_3)
cat("Standard deviation of daily returns:", sigma_t3, "\n")

# Menghitung rolling volatility (standar deviasi bergulir)
rolling_volatility3 <- runSD(return_3, n = 30)

# Plot imbal hasil saham majemuk dan rolling volatility
n <- length(return_3)
t <- 1:n
plot(t, return_3, type = "l", col = "black", main = "Imbal Hasil dan Volatilitas LTC", ylab = expression(R[t]), xlab = "t")
lines(t, rolling_volatility3, col = "red", lwd = 4)
legend("topright", legend = c("Imbal Hasil", "Volatilitas"), col = c("black", "red"), lty = 1, lwd = 2)

# Statistik Deskriptif
summary(return_3_notclean)
return_3 <- na.omit(return_3_notclean)
str(return_3)

# Uji Hipotesis
ad.test(return_3) # Uji Andersen-Darling
ks.test(return_3, "pnorm") # Uji Kolmogorov-Smirnov

summary(return_3)
mean_value3 <- mean(return_3)
std_dev3 <- sd(return_3)
var3 <- var(return_3)
kurtosis_value3 <- kurtosis(return_3)
skewness_value3 <- skewness(return_3)

# Tampilkan hasil
cat("Mean:", mean_value3, "\n")
cat("Variance:", var3, "\n")
cat("Standard Deviation:", std_dev3, "\n")
cat("Kurtosis:", kurtosis_value3, "\n")
cat("Skewness:", skewness_value3, "\n")

# Menampilkan plot
plot(acf(return_3, plot=FALSE), main="Fungsi Autokorelasi Imbal Hasil LTC", ylim = c(-0.10, 0.10))
plot(acf(return_3^2, plot=FALSE), main="Fungsi Autokorelasi Imbal Hasil Kuadrat LTC", ylim = c(-0.15, 0.15))

# Ljung box test
Box.test(return_3, lag = 1, type = "Ljung")
Box.test(return_3^2, lag = 1, type = "Ljung")

# QQ Plot
qqnorm(return_3_notclean)
qqline(return_3_notclean)

mean(return_1)
mean(return_2)
mean(return_3)
