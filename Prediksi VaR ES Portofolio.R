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

# SAHAM S&P500
# Mengambil data harga saham 
getSymbols("^GSPC", src = "yahoo", from = "2018-02-21", to = "2024-02-21")
data_1<- GSPC$GSPC.Close
head(data_1) # menampilkan 5 data pertama

# Return Harian
return_1_notclean <- diff(log(data_1))
plot(return_1_notclean, type = "l", main = "Harga Imbal Hasil S&P500",
     xlab = "Tanggal", ylab = "USD")
abline(h = mean(return_1_notclean), lwd = 2, lty = 2, col = "red")

# Statistik Deskriptif
summary(return_1_notclean)
return_1 <- na.omit(return_1_notclean)
str(return_1)

# Menghitung volatilitas harian (rolling standard deviation)
library(TTR)
rolling_volatility <- runSD(return_1, n = 30)  # 30 hari

# Mengkonversi objek xts ke data frame untuk digunakan di ggplot2
df <- data.frame(date = index(rolling_volatility), 
                 rolling_volatility = coredata(rolling_volatility))

# Plot menggunakan ggplot2
ggplot(df, aes(x = date, y = rolling_volatility)) +
  geom_line(color = "blue") +
  labs(title = "Volatilitas Imbal Hasil S&P500",
       x = "Tanggal",
       y = "Volatilitas") +
  theme_minimal()

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

# Histogram imbal hasil
df <- data.frame(return_1 = return_1)
p <- ggplot(df, aes(x = return_1)) + 
  geom_histogram(aes(y = ..density..), bins = 25, fill = "pink", color = "black", alpha = 0.7) + 
  stat_function(fun = dnorm, args = list(mean = mean, sd = sigma), color = "red", size = 1) +
  ggtitle(paste("Histogram Return Harian Saham S&P500")) + 
  xlab("Imbal Hasil Harian") + 
  ylab("Frekuensi") + 
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2))

p

# Membuat histogram dengan ggplot2
df_data <- data.frame(return_1 = return_1)
p <- ggplot(df_data, aes(x = return_1)) + 
  geom_histogram(aes(y = ..density..), bins = 25, fill = "pink", color = "black", alpha = 0.7) + 
  stat_function(fun = dnorm, args = list(mean = mean_value1, sd = std_dev1), color = "red", size = 1, linetype = "dashed") +
  stat_function(fun = function(x) dt((x - mean_value1) / std_dev1, df) / std_dev1, color = "blue", size = 1) +
  ggtitle("Histogram Return Harian Saham S&P500") + 
  xlab("Imbal Hasil Harian") + 
  ylab("Densitas") + 
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2))

# Menampilkan plot
print(p)

# Menampilkan plot
plot(acf(return_1, plot=FALSE), main="Fungsi Autokorelasi Imbal Hasil S&P500")
plot(acf(return_1^2, plot=FALSE), main="Fungsi Autokorelasi Imbal Hasil Kuadrat S&P500")

# Ljung box test
Box.test(return_1, lag = 1000, type = "Ljung")
Box.test(return_1^2, lag = 1000, type = "Ljung")

# QQ Plot
qqnorm(return_1)
qqline(return_1)

# Uji Hipotesis
ad.test(return_1) # Uji Andersen-Darling
ks.test(return_1, "pnorm") # Uji Kolmogorov-Smirnov

# Pemodelan GARCH(1,0) (estimasi parameter)
m1_0 <- garchFit(return_1 ~ garch(1,0), data = return_1,
                 trace = F)
summary(m1_0)

# Mendapatkan koefisien dari model
coefficients1_0 <- coef(m1_0)
mu10 <- coefficients1_0["mu"]
omega10 <- coefficients1_0["omega"]
alpha10 <- coefficients1_0["alpha1"]

# Menampilkan nilai parameter
mu10
omega10
alpha10

# estimasi sigma GARCH(1,0)
n <- length(return_1)
sigma_10 <- omega10 + alpha10*return_1[n]^(2)
sigma_10

#########################################################

# Pemodelan GARCH(1,1) (estimasi parameter)
m1_1 <- garchFit(return_1 ~ garch(1,1), data = return_1,
               trace = F)
summary(m1_1)

# Mendapatkan koefisien dari model
coefficients1_1 <- coef(m1_1)
mu11 <- coefficients1_1["mu"]
omega11 <- coefficients1_1["omega"]
alpha11 <- coefficients1_1["alpha1"]
beta11 <- coefficients1_1["beta1"]


# Menampilkan nilai parameter
mu11
omega11
alpha11
beta11

# estimasi sigma GARCH(1,1)
sigma_11 <- omega11 + alpha11*return_1[n]^(2) + beta11*(m1_1@sigma.t[n])^2
sigma_11

m1_1@sigma.t
m1_1@sigma.t[n]

################################################################

# Pemodelan GARCH(1,2) (estimasi parameter)
m1_2 <- garchFit(return_1 ~ garch(1,2), data = return_1,
                 trace = F)
summary(m1_2)

# Mendapatkan koefisien dari model
coefficients1_2 <- coef(m1_2)
mu12 <- coefficients1_2["mu"]
omega12 <- coefficients1_2["omega"]
alpha12 <- coefficients1_2["alpha1"]
beta12 <- coefficients1_2["beta1"]
beta122 <- coefficients1_2["beta2"]

# Menampilkan nilai parameter
mu12
omega12
alpha12
beta12
beta122

# estimasi sigma GARCH(1,2)
sigma_12 <- omega12 + alpha12*return_1[n]^(2) + beta12*(m1_2@sigma.t[n])^2 + beta122*(m1_2@sigma.t[n-1])^2
sigma_12

########################################################

# Pemodelan GARCH(2,1) (estimasi parameter)
m1_3 <- garchFit(return_1 ~ garch(2,1), data = return_1,
                 trace = F)
summary(m1_3)

# Mendapatkan koefisien dari model
coefficients1_3 <- coef(m1_3)
mu21 <- coefficients1_3["mu"]
omega21 <- coefficients1_3["omega"]
alpha21 <- coefficients1_3["alpha1"]
alpha212 <- coefficients1_3["alpha2"]
beta21 <- coefficients1_3["beta1"]

# Menampilkan nilai parameter
mu21
omega21
alpha21
alpha212
beta21

m1_3@sigma.t[n]

# estimasi sigma GARCH(2,1)
sigma_21 <- omega21 + alpha21*(return_1[n])^(2) + alpha212*(return_1[-1])^(2) + beta21*(m1_3@sigma.t[n])^2
sigma_21

##################################################
##################################################

# SAHAM Russell 2000
# Mengambil data harga saham 
getSymbols("^RUT", src = "yahoo", from = "2018-02-21", to = "2024-02-21")
data_2<- RUT$RUT.Close
head(data_2) # menampilkan 5 data pertama

# Return Harian
return_2_notclean <- diff(log(data_2))
plot(return_2_notclean, type = "l", main = "Harga Imbal Hasil Russell 2000",
     xlab = "Tanggal", ylab = "USD")
abline(h = mean(return_2_notclean), lwd = 2, lty = 2, col = "red")

# Statistik Deskriptif
summary(return_2_notclean)
return_2 <- na.omit(return_2_notclean)
str(return_2)

# Menghitung dan memplot fungsi autokorelasi
acf(return_2, main="ACF of Stock Log Returns")
acf(return_2^2, main="ACF of Stock Log Square Returns")

# Menampilkan plot
plot(acf(return_2, plot=FALSE), main="Fungsi Autokorelasi Imbal Hasil Russell 2000")

# Ljung box test
Box.test(return_2, lag = 1, type = "Ljung")
Box.test(return_2^2, lag = 1, type = "Ljung")

# QQ Plot
qqnorm(return_2)
qqline(return_2)

# Uji Hipotesis
ad.test(return_2) # Uji Andersen-Darling
ks.test(return_2, "pnorm") # Uji Kolmogorov-Smirnov

# Statistika Deskriptif
summary(return_2)
mean_value2 <- mean(return_2)
std_dev2 <- sd(return_2)
var2 <- var(return_2)
kurtosis_value2 <- kurtosis(return_2)
skewness_value2 <- skewness(return_2)

mean_value2
std_dev2
var2
kurtosis_value2
skewness_value2

m <- length(return_2)

# Pemodelan GARCH(1,0) (estimasi parameter)
m2_0 <- garchFit(return_2 ~ garch(1,0), data = return_2,
                 trace = F)
summary(m2_0)

# Mendapatkan koefisien dari model
coefficients2_0 <- coef(m2_0)
mu2_10 <- coefficients2_0["mu"]
omega2_10 <- coefficients2_0["omega"]
alpha2_10 <- coefficients2_0["alpha1"]

# Menampilkan nilai parameter
mu2_10
omega2_10
alpha2_10

# estimasi sigma GARCH(1,0)
sigma2_10 <- omega2_10 + alpha2_10*(return_2[m])^(2)
sigma2_10

#########################################################

# Pemodelan GARCH(1,1) (estimasi parameter)
m2_1 <- garchFit(return_2~ garch(1,1), data = return_2,
                 trace = F)
summary(m2_1)

# Mendapatkan koefisien dari model
coefficients2_1 <- coef(m2_1)
mu2_11 <- coefficients2_1["mu"]
omega2_11 <- coefficients2_1["omega"]
alpha2_11 <- coefficients2_1["alpha1"]
beta2_11 <- coefficients2_1["beta1"]

# Menampilkan nilai parameter
mu2_11
omega2_11
alpha2_11
beta2_11

# estimasi sigma GARCH(1,1)
sigma2_11 <- omega2_11 + alpha2_11*(return_2[m])^(2) + beta2_11*(m2_1@sigma.t[m])^2
sigma2_11

################################################################
# Pemodelan GARCH(1,2) (estimasi parameter)
m2_2 <- garchFit(return_2 ~ garch(1,2), data = return_2,
                 trace = F)
summary(m2_2)

# Mendapatkan koefisien dari model
coefficients2_2 <- coef(m2_2)
mu2_12 <- coefficients2_2["mu"]
omega2_12 <- coefficients2_2["omega"]
alpha2_12 <- coefficients2_2["alpha1"]
beta2_12 <- coefficients2_2["beta1"]
beta2_122 <- coefficients2_2["beta2"]

# Menampilkan nilai parameter
mu2_12
omega2_12
alpha2_12
beta2_12
beta2_122

# estimasi sigma GARCH(1,2)
sigma2_12 <- omega2_12 + alpha2_12*(return_2[m])^(2) + beta2_12*(m2_2@sigma.t[m])^2 + beta2_122*(m2_2@sigma.t[m-1])^2
sigma2_12

########################################################

# Pemodelan GARCH(2,1) (estimasi parameter)
m2_3 <- garchFit(return_2 ~ garch(2,1), data = return_2,
                 trace = F)
summary(m2_3)

# Mendapatkan koefisien dari model
coefficients2_3 <- coef(m2_3)
mu2_21 <- coefficients2_3["mu"]
omega2_21 <- coefficients2_3["omega"]
alpha2_21 <- coefficients2_3["alpha1"]
alpha2_212 <- coefficients2_3["alpha2"]
beta2_21 <- coefficients2_3["beta1"]

# Menampilkan nilai parameter
mu2_21
omega2_21
alpha2_21
alpha2_212
beta2_21

# estimasi sigma GARCH(2,1)
sigma2_21 <- omega2_21 + alpha2_21*(return_2[m])^(2) + alpha2_212*(return_2[-1])^(2) + beta2_21*(m2_3@sigma.t[m])^2
sigma2_21

#############################################
#############################################
#############################################

# Menghitung korelasi Pearson
correlation <- cor(return_1, return_2, method = "pearson")

# Menampilkan hasil
print(correlation)

# Parameter portofolio













