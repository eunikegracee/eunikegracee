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

# DATA SAHAM
# SAHAM S&P500
# Mengambil data harga saham 
getSymbols("^GSPC", src = "yahoo", from = "2021-02-21", to = "2024-02-21")
data_1<- GSPC$GSPC.Close
head(data_1) # menampilkan 5 data pertama


adf.test(data_1)

# Return Harian
return_1_notclean <- -diff(log(data_1))
return_1 <- na.omit(return_1_notclean)
head(return_1)
tail(return_1)
n <- length(return_1)

# SAHAM RUSSEL 2000
# Mengambil data harga saham 
getSymbols("^RUT", src = "yahoo", from = "2021-02-21", to = "2024-02-21")
data_2<- RUT$RUT.Close

# Menampilkan beberapa baris pertama untuk memverifikasi
head(data_2)

# Return Harian
return_2_notclean <- -diff(log(data_2))
return_2 <- na.omit(return_2_notclean)
head(return_2)
tail(return_2)
m <- length(return_2)

# Menghitung korelasi Pearson
correlation <- cor(return_1, return_2, method = "pearson")
correlation

# Pastikan corr_10 adalah skalar
corr <- as.numeric(correlation)
# Pastikan corr_10 diulang sepanjang garch10_sigma
corr <- rep(corr, length(return_1))

# Mengestimasi parameter GARCH(1,0)
# Menggunakan distribusi normal
library(fGarch)
m_10 <- garchFit(~ garch(1, 0), data = return_1, cond.dist = "norm")
summary(m_10)
coefficients1 <- coef(m_10)
mu_10 <- coefficients1["mu"]
omega1_10 <- coefficients1["omega"]
alpha1_10 <- coefficients1["alpha1"]

# Mengambil volatilitas yang diestimasi
garch10_sigma = m_10@sigma.t

m2_10 <- garchFit(~ garch(1, 0), data = return_2, cond.dist = "norm")
summary(m2_10)
coefficients2 <- coef(m2_10)
mu2_10 <- coefficients2["mu"]
omega2_10 <- coefficients2["omega"]
alpha21_10 <- coefficients2["alpha1"]

# Mengambil volatilitas yang diestimasi
a = 0.25
garch2_10_sigma = m2_10@sigma.t
garch10_sigma2 = a^2*garch10_sigma^2 + (1-a)^2*garch2_10_sigma^2 + 2*corr*garch10_sigma*garch2_10_sigma*a*(1-a)

# Memprediksi nilai VaR dan ES pada tingkat kepercayaan 0.95
# Menghitung VaR dan ES pada level kepercayaan 95%
alpha = 0.05

# VaR untuk distribusi normal
VaR <- qnorm(alpha) * sqrt(garch10_sigma2)

# ES untuk distribusi normal
ES <- (dnorm(qnorm(alpha)) / (1-alpha)) * sqrt(garch10_sigma2)

# Prediksi VaR dan ES
# Menghitung sigma
esigma_10 <- sqrt(omega1_10 + alpha1_10 *  return_1[n]^2)
esigma2_10 <- sqrt(omega2_10 + alpha21_10 *  return_2[m]^2)
esigma_tot10 <- a^2*esigma_10^2 + (1-a)^2*esigma2_10^2 + 2*correlation*esigma_10*esigma2_10*a*(1-a)
esigma_tot10

VaR_pred10 <- qnorm(alpha) * sqrt(esigma_tot10)
ES_pred10 <- (dnorm(qnorm(alpha)) / (1-alpha)) * sqrt(esigma_tot10)
VaR_pred10
ES_pred10

max(return_1)
max(return_2)

# Memprediksi nilai VaR dan ES pada tingkat kepercayaan 0.99
# Menghitung VaR dan ES pada level kepercayaan 99%
alpha2 = 0.99

# VaR untuk distribusi normal
VaR99 <- qnorm(alpha2) * sqrt(garch10_sigma2)

# ES untuk distribusi normal
ES99 <- (dnorm(qnorm(alpha2)) / (1-alpha2)) * sqrt(garch10_sigma2)

# Prediksi VaR dan ES
# Menghitung sigma
VaR_pred10 <- qnorm(alpha2) * sqrt(esigma_tot10)
ES_pred10 <- (dnorm(qnorm(alpha2)) / (1-alpha2)) * sqrt(esigma_tot10)
VaR_pred10
ES_pred10

# Plot data yang disimulasikan, VaR, dan ES
plot(1:length(return_1), a*return_1+(1-a)*return_2, type = "l", main = "Ukuran Risiko Portofolio Kerugian GARCH(1,0)",
     xlab = "t", ylab = expression(R[t]), col = "black")
#plot(a*return_1+(1-a)*return_2, type = "l", main = "Grafik Imbal Hasil dan VaR ES Portofolio Kerugian", ylab = expression(R[t]), xlab = "t")
lines(VaR, col = "red", lwd = 2)
lines(ES, col = "blue", lwd = 2)
lines(VaR99, col = "purple", lwd = 2)
lines(ES99, col = "green", lwd = 2)
legend("bottom", legend = c("VaR (95%)", "ES (95%)", "VaR (99%)", "ES (99%)"), col = c("red", "blue", "purple", "green"), lty = 1, lwd = 2, ncol = 2)

# histogram
hist(a*return_1+(1-a)*return_2, prob = TRUE, main = "Histogram Data", xlab = "Nilai", ylab = "Densitas", col = "lightblue", border = "black")
curve(dnorm(x, mean = mean(a*return_1+(1-a)*return_2), sd = sd(a*return_1+(1-a)*return_2)), col = "darkorange", lwd = 2, add = TRUE)
abline(v = VaR_pred10, col = "red", lwd = 2, lty = 2)
abline(v = ES_pred10, col = "blue", lwd = 2, lty = 2)

##########################################################################
# Mengestimasi parameter GARCH(1,1)
# Menggunakan distribusi normal
library(fGarch)
m_11 <- garchFit(~ garch(1, 1), data = return_1, cond.dist = "norm")
summary(m_11)
coefficients3 <- coef(m_11)
mu_11 <- coefficients3["mu"]
omega1_11 <- coefficients3["omega"]
alpha1_11 <- coefficients3["alpha1"]
beta1_11 <- coefficients3["beta1"]

# Mengambil volatilitas yang diestimasi
garch11_sigma = m_11@sigma.t

m2_11 <- garchFit(~ garch(1, 1), data = return_2, cond.dist = "norm")
summary(m2_11)
coefficients4 <- coef(m2_11)
mu2_11 <- coefficients4["mu"]
omega2_11 <- coefficients4["omega"]
alpha21_11 <- coefficients4["alpha1"]
beta21_11 <- coefficients4["beta1"]

# Mengambil volatilitas yang diestimasi
a = 0.25
garch2_11_sigma = m2_11@sigma.t
garch11_sigma2 = (a^2)*garch11_sigma^2 + ((1-a)^2)*garch2_11_sigma^2 + 2*corr*garch11_sigma*garch2_11_sigma*a*(1-a)

# Memprediksi nilai VaR dan ES pada tingkat kepercayaan 0.95
# Menghitung VaR dan ES pada level kepercayaan 95%
alpha = 0.95

# VaR untuk distribusi normal
VaR2 <- qnorm(alpha) * sqrt(garch11_sigma2)

# ES untuk distribusi normal
ES2 <- (dnorm(qnorm(alpha)) / (1-alpha)) * sqrt(garch11_sigma2)

# Prediksi VaR dan ES
# Menghitung sigma
esigma_11 <- sqrt(omega1_11 + alpha1_11 *  return_1[n]^2 + beta1_11 * m_11@sigma.t[n]^2)
esigma2_11 <- sqrt(omega2_11 + alpha21_11 * return_2[m]^2 + beta21_11 * m2_11@sigma.t[m]^2)
esigma_tot11 <- a^2*esigma_11^2 + (1-a)^2*esigma2_11^2 + 2*correlation*esigma_11*esigma2_11*a*(1-a)
esigma_tot11
0.0001908499
VaR_pred11 <- qnorm(alpha) * sqrt(esigma_tot11)
ES_pred11 <- (dnorm(qnorm(alpha)) / (1-alpha)) * sqrt(esigma_tot11)
VaR_pred11
ES_pred11


# Memprediksi nilai VaR dan ES pada tingkat kepercayaan 0.99
# Menghitung VaR dan ES pada level kepercayaan 99%
alpha2 = 0.99

# VaR untuk distribusi normal
VaR2_99 <- qnorm(alpha2) * sqrt(garch11_sigma2)

# ES untuk distribusi normal
ES2_99 <- (dnorm(qnorm(alpha2)) / (1-alpha2)) * sqrt(garch11_sigma2)

# Prediksi VaR dan ES
# Menghitung sigma
VaR99_pred11 <- qnorm(alpha2) * sqrt(esigma_tot11)
ES99_pred11 <- (dnorm(qnorm(alpha2)) / (1-alpha2)) * sqrt(esigma_tot11)
VaR99_pred11
ES99_pred11

# Plot data yang disimulasikan, VaR, dan ES
plot(1:length(return_1), a*return_1+(1-a)*return_2, type = "l", main = "Ukuran Risiko Portofolio Kerugian GARCH(1,1)",
     xlab = "t", ylab = expression(R[t]), col = "black")
#plot(a*return_1+(1-a)*return_2, type = "l", main = "Grafik Imbal Hasil dan VaR ES Portofolio Kerugian", ylab = expression(R[t]), xlab = "t")
lines(VaR2, col = "red", lwd = 2)
lines(ES2, col = "blue", lwd = 2)
lines(VaR2_99, col = "purple", lwd = 2)
lines(ES2_99, col = "green", lwd = 2)
legend("bottom", legend = c("VaR (95%)", "ES (95%)", "VaR (99%)", "ES (99%)"), col = c("red", "blue", "purple", "green"), lty = 1, lwd = 2, ncol = 2)

# histogram
hist(a*return_1+(1-a)*return_2, prob = TRUE, main = "Histogram Data", xlab = "Nilai", ylab = "Densitas", col = "lightblue", border = "black")
curve(dnorm(x, mean = mean(a*return_1+(1-a)*return_2), sd = sd(a*return_1+(1-a)*return_2)), col = "darkorange", lwd = 2, add = TRUE)
abline(v = VaR_pred11, col = "red", lwd = 2, lty = 2)
abline(v = ES_pred11, col = "blue", lwd = 2, lty = 2)


##########################################################################
# Mengestimasi parameter GARCH(1,2)
# Menggunakan distribusi normal
library(fGarch)
m_12 <- garchFit(~ garch(1, 2), data = return_1, cond.dist = "norm")
summary(m_12)
coefficients5 <- coef(m_12)
mu_12 <- coefficients5["mu"]
omega1_12 <- coefficients5["omega"]
alpha1_12 <- coefficients5["alpha1"]
beta1_12 <- coefficients5["beta1"]
beta2_12 <- coefficients5["beta2"]

# Mengambil volatilitas yang diestimasi
garch12_sigma = m_12@sigma.t

m2_12 <- garchFit(~ garch(1, 2), data = return_2, cond.dist = "norm")
summary(m2_12)
coefficients6 <- coef(m2_12)
mu2_12 <- coefficients6["mu"]
omega2_12 <- coefficients6["omega"]
alpha21_12 <- coefficients6["alpha1"]
beta21_12 <- coefficients6["beta1"]
beta22_12 <- coefficients6["beta2"]


# Mengambil volatilitas yang diestimasi
a = 0.25
garch2_12_sigma = m2_12@sigma.t
garch12_sigma2 = (a^2)*garch12_sigma^2 + ((1-a)^2)*garch2_12_sigma^2 + 2*corr*garch12_sigma*garch2_12_sigma*a*(1-a)

# Memprediksi nilai VaR dan ES pada tingkat kepercayaan 0.95
# Menghitung VaR dan ES pada level kepercayaan 95%
alpha = 0.95

# VaR untuk distribusi normal
VaR3 <- qnorm(alpha) * sqrt(garch12_sigma2)

# ES untuk distribusi normal
ES3 <- (dnorm(qnorm(alpha)) / (1-alpha)) * sqrt(garch12_sigma2)

# Prediksi VaR dan ES
# Menghitung sigma
esigma_12 <- sqrt(omega1_12 + alpha1_12 *  return_1[n]^2 + beta1_12 * m_12@sigma.t[n]^2 + beta2_12 * m_12@sigma.t[n-1]^2)
esigma2_12 <- sqrt(omega2_12 + alpha21_12 * return_2[m]^2 + beta21_12 * m2_12@sigma.t[m]^2 + beta22_12 * m2_12@sigma.t[m-1]^2)
esigma_tot12 <- a^2*esigma_12^2 + (1-a)^2*esigma2_12^2 + 2*correlation*esigma_12*esigma2_12*a*(1-a)

esigma_10
esigma_11
esigma_12
esigma_21

esigma2_10
esigma2_11
esigma2_12
esigma2_21

VaR_pred12 <- qnorm(alpha) * sqrt(esigma_tot12)
ES_pred12 <- (dnorm(qnorm(alpha)) / (1-alpha)) * sqrt(esigma_tot12)
VaR_pred12
ES_pred12

max(return_1)
max(return_2)

# Memprediksi nilai VaR dan ES pada tingkat kepercayaan 0.99
# Menghitung VaR dan ES pada level kepercayaan 99%
alpha2 = 0.99

# VaR untuk distribusi normal
VaR3_99 <- qnorm(alpha2) * sqrt(garch12_sigma2)

# ES untuk distribusi normal
ES3_99 <- (dnorm(qnorm(alpha2)) / (1-alpha2)) * sqrt(garch12_sigma2)

# Prediksi VaR dan ES
# Menghitung sigma
VaR99_pred12 <- qnorm(alpha2) * sqrt(esigma_tot12)
ES99_pred12 <- (dnorm(qnorm(alpha2)) / (1-alpha2)) * sqrt(esigma_tot12)
VaR99_pred12
ES99_pred12

# Plot data yang disimulasikan, VaR, dan ES
plot(1:length(return_1), a*return_1+(1-a)*return_2, type = "l", main = "Ukuran Risiko Portofolio Kerugian GARCH(1,2)",
     xlab = "t", ylab = expression(R[t]), col = "black")
#plot(a*return_1+(1-a)*return_2, type = "l", main = "Grafik Imbal Hasil dan VaR ES Portofolio Kerugian", ylab = expression(R[t]), xlab = "t")
lines(VaR3, col = "red", lwd = 2)
lines(ES3, col = "blue", lwd = 2)
lines(VaR3_99, col = "purple", lwd = 2)
lines(ES3_99, col = "green", lwd = 2)
legend("bottom", legend = c("VaR (95%)", "ES (95%)", "VaR (99%)", "ES (99%)"), col = c("red", "blue", "purple", "green"), lty = 1, lwd = 2, ncol = 2)

# histogram
hist(a*return_1+(1-a)*return_2, prob = TRUE, main = "Histogram Data", xlab = "Nilai", ylab = "Densitas", col = "lightblue", border = "black")
curve(dnorm(x, mean = mean(a*return_1+(1-a)*return_2), sd = sd(a*return_1+(1-a)*return_2)), col = "darkorange", lwd = 2, add = TRUE)
abline(v = VaR_pred12, col = "red", lwd = 2, lty = 2)
abline(v = ES_pred12, col = "blue", lwd = 2, lty = 2)


##########################################################################
# Mengestimasi parameter GARCH(2, 1)
# Menggunakan distribusi normal
library(fGarch)
m_21 <- garchFit(~ garch(2, 1), data = return_1, cond.dist = "norm")
summary(m_21)
coefficients7 <- coef(m_21)
mu_21 <- coefficients7["mu"]
omega1_21 <- coefficients7["omega"]
alpha1_21 <- coefficients7["alpha1"]
alpha2_21 <- coefficients7["alpha2"]
beta1_21 <- coefficients7["beta1"]


# Mengambil volatilitas yang diestimasi
garch21_sigma = m_21@sigma.t

m2_21 <- garchFit(~ garch(2, 1), data = return_2, cond.dist = "norm")
summary(m2_21)
coefficients8 <- coef(m2_21)
mu2_21 <- coefficients8["mu"]
omega2_21 <- coefficients8["omega"]
alpha21_21 <- coefficients8["alpha1"]
alpha22_21 <- coefficients8["alpha2"]
beta21_21 <- coefficients8["beta1"]

# Mengambil volatilitas yang diestimasi
a = 0.25
garch2_21_sigma = m2_21@sigma.t
garch21_sigma2 = (a^2)*garch21_sigma^2 + ((1-a)^2)*garch2_21_sigma^2 + 2*corr*garch21_sigma*garch2_21_sigma*a*(1-a)

# Memprediksi nilai VaR dan ES pada tingkat kepercayaan 0.95
# Menghitung VaR dan ES pada level kepercayaan 95%
alpha = 0.95

# VaR untuk distribusi normal
VaR4 <- qnorm(alpha) * sqrt(garch21_sigma2)

# ES untuk distribusi normal
ES4 <- (dnorm(qnorm(alpha)) / (1-alpha)) * sqrt(garch21_sigma2)

# Prediksi VaR dan ES
# Menghitung sigma
esigma_21 <- sqrt(omega1_21 + alpha1_21 *  return_1[n]^2 + alpha2_21 *  0.004815044^2 + beta1_21 * m_21@sigma.t[n]^2)
esigma2_21 <- sqrt(omega2_21 + alpha21_21 * return_2[m]^2 + alpha22_21 * 0.007957908^2 + beta21_21 * m2_21@sigma.t[m]^2)
esigma_tot21 <- a^2*esigma_21^2 + (1-a)^2*esigma2_21^2 + 2*correlation*esigma_21*esigma2_21*a*(1-a)

VaR_pred21 <- qnorm(alpha) * sqrt(esigma_tot21)
ES_pred21 <- (dnorm(qnorm(alpha)) / (1-alpha)) * sqrt(esigma_tot21)
VaR_pred21
ES_pred21

max(return_1)
max(return_2)

# Memprediksi nilai VaR dan ES pada tingkat kepercayaan 0.99
# Menghitung VaR dan ES pada level kepercayaan 99%
alpha2 = 0.99

# VaR untuk distribusi normal
VaR4_99 <- qnorm(alpha2) * sqrt(garch12_sigma2)

# ES untuk distribusi normal
ES4_99 <- (dnorm(qnorm(alpha2)) / (1-alpha2)) * sqrt(garch12_sigma2)

# Prediksi VaR dan ES
# Menghitung sigma
VaR99_pred21 <- qnorm(alpha2) * sqrt(esigma_tot21)
ES99_pred21 <- (dnorm(qnorm(alpha2)) / (1-alpha2)) * sqrt(esigma_tot21)
VaR99_pred21
ES99_pred21

# Plot data yang disimulasikan, VaR, dan ES
plot(1:length(return_1), a*return_1+(1-a)*return_2, type = "l", main = "Ukuran Risiko Portofolio Kerugian GARCH(2,1)",
     xlab = "t", ylab = expression(R[t]), col = "black")
#plot(a*return_1+(1-a)*return_2, type = "l", main = "Grafik Imbal Hasil dan VaR ES Portofolio Kerugian", ylab = expression(R[t]), xlab = "t")
lines(VaR4, col = "red", lwd = 2)
lines(ES4, col = "blue", lwd = 2)
lines(VaR4_99, col = "purple", lwd = 2)
lines(ES4_99, col = "green", lwd = 2)
legend("bottom", legend = c("VaR (95%)", "ES (95%)", "VaR (99%)", "ES (99%)"), col = c("red", "blue", "purple", "green"), lty = 1, lwd = 2, ncol = 2)

# histogram
hist(a*return_1+(1-a)*return_2, prob = TRUE, main = "Histogram Data", xlab = "Nilai", ylab = "Densitas", col = "lightblue", border = "black")
curve(dnorm(x, mean = mean(a*return_1+(1-a)*return_2), sd = sd(a*return_1+(1-a)*return_2)), col = "darkorange", lwd = 2, add = TRUE)
abline(v = VaR_pred21, col = "red", lwd = 2, lty = 2)
abline(v = ES_pred21, col = "blue", lwd = 2, lty = 2)


esigma_10
esigma2_10
esigma_11
esigma2_11
esigma_12
esigma2_12
esigma_21
esigma2_21


