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

# Mengambil data harga saham
getSymbols("^GSPC", src = "yahoo", from = "2021-02-21", to = "2024-02-21")
getSymbols("^RUT", src = "yahoo", from = "2021-02-21", to = "2024-02-21")

# Menghitung return harian
return_1 <- na.omit(-diff(log(Cl(GSPC))))
return_2 <- na.omit(-diff(log(Cl(RUT))))

# Memeriksa data untuk nilai yang hilang atau ekstrim
summary(return_1)
summary(return_2)

library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(MASS)

# GARCH(1,0)
# Fungsi untuk prediksi VaR dan ES
calculate_var_es10 <- function(a, return_1, return_2, corr, alpha) {
  esigma_10 <- sqrt(omega1_10 + alpha1_10 *  return_1[n]^2)
  esigma2_10 <- sqrt(omega2_10 + alpha21_10 *  return_2[m]^2)
  esigma_tot10 <- a^2*esigma_10^2 + (1-a)^2*esigma2_10^2 + 2*correlation*esigma_10*esigma2_10*a*(1-a)
  
  # Prediksi VaR
  VaR_preds10 <- qnorm(alpha) * sqrt(esigma_tot10)
  
  # Prediksi ES
  ES_preds10 <- (dnorm(qnorm(alpha)) / (1-alpha)) * sqrt(esigma_tot10)
  
  return(list(VaR10 = VaR_preds10, ES10 = ES_preds10))
}

# Inisialisasi array untuk menyimpan nilai a, VaR, ES
a_values <- seq(0.01, 0.99, length.out = 100) 
VaR10_values <- c()
ES10_values <- c()

# Iterasi melalui berbagai a dan hitung VaR ES
for (a in a_values) {
  R_1 <- return_1 * a
  R_2 <- return_2 * (1-a)
  res <- calculate_var_es10(a, R_1, R_2, corr, alpha)
  VaR10_values <- c(VaR10_values, res$VaR10)
  ES10_values <- c(ES10_values, res$ES10)
}

# Plot hasilnya
df <- data.frame(a = a_values, VaR10 = VaR10_values, ES10 = ES10_values)

ggplot(df, aes(x = a)) +
  geom_line(aes(y = VaR10, color = "VaR")) +
  geom_line(aes(y = ES10, color = "ES")) +
  labs(title = "VaR dan ES terhadap Proporsi a", x = "a", y = "Prediksi Ukuran Risiko") +
  scale_color_manual("", values = c("VaR" = "red", "ES" = "green")) +
  theme_minimal()













