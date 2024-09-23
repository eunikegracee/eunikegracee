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
n <- length(return_1)
m <- length(return_2)

# Memeriksa data untuk nilai yang hilang atau ekstrim
summary(return_1)
summary(return_2)

adf.test(return_1)
adf.test(return_2)

# Mengestimasi parameter GARCH(1,0)
# Menggunakan distribusi normal
m_10 <- garchFit(~ garch(1, 0), data = return_1, cond.dist = "norm")
summary(m_10)
coefficients1 <- coef(m_10)
omega1_10 <- coefficients1["omega"]
alpha1_10 <- coefficients1["alpha1"]

m2_10 <- garchFit(~ garch(1, 0), data = return_2, cond.dist = "norm")
summary(m2_10)
coefficients2 <- coef(m2_10)
omega2_10 <- coefficients2["omega"]
alpha21_10 <- coefficients2["alpha1"]

# Menghitung korelasi antara return
correlation <- cor(return_1, return_2)
correlation

# Define alpha level for VaR and ES
alpha <- 0.95

# Fungsi untuk prediksi VaR dan ES
calculate_var_es10 <- function(a, omega1_10, alpha1_10, omega2_10, alpha21_10, correlation, alpha) {
  esigma_10 <- sqrt(omega1_10 + alpha1_10 * return_1[n]^2)
  esigma2_10 <- sqrt(omega2_10 + alpha21_10 * return_2[m]^2)
  esigma_tot10 <- sqrt(a^2 * esigma_10^2 + (1-a)^2 * esigma2_10^2 + 2 * correlation * esigma_10 * esigma2_10 * a * (1-a))
  
  # Prediksi VaR
  VaR_preds10 <- qnorm(alpha) * esigma_tot10
  
  # Prediksi ES
  ES_preds10 <- (dnorm(qnorm(alpha)) / (1-alpha)) * esigma_tot10
  
  return(list(VaR10 = VaR_preds10, ES10 = ES_preds10))
}

# Inisialisasi array untuk menyimpan nilai a, VaR, ES
a_values <- seq(0.01, 0.99, length.out = 100) 
VaR10_values <- c()
ES10_values <- c()

# Iterasi melalui berbagai a dan hitung VaR ES
for (a in a_values) {
  res <- calculate_var_es10(a, omega1_10, alpha1_10, omega2_10, alpha21_10, correlation, alpha)
  VaR10_values <- c(VaR10_values, res$VaR10)
  ES10_values <- c(ES10_values, res$ES10)
}


# GARCH(1,1)
# Mengestimasi parameter GARCH(1,1)
# Menggunakan distribusi normal
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

# Define alpha level for VaR and ES
alpha <- 0.95

# Fungsi untuk prediksi VaR dan ES
calculate_var_es11 <- function(a, omega1_11, alpha1_11, beta1_11, omega2_11, alpha21_11, beta21_11, correlation, alpha) {
  esigma_11 <- sqrt(omega1_11 + alpha1_11 *  return_1[n]^2 + beta1_11 * m_11@sigma.t[n]^2)
  esigma2_11 <- sqrt(omega2_11 + alpha21_11 * return_2[m]^2 + beta21_11 * m2_11@sigma.t[m]^2)
  esigma_tot11 <- a^2*esigma_11^2 + (1-a)^2*esigma2_11^2 + 2*correlation*esigma_11*esigma2_11*a*(1-a)
  
  #prediksi ukuran risiko
  VaR_preds11 <- qnorm(alpha) * sqrt(esigma_tot11)
  ES_preds11 <- (dnorm(qnorm(alpha)) / (1-alpha)) * sqrt(esigma_tot11)
  
  return(list(VaR11 = VaR_preds11, ES11 = ES_preds11))
}

# Inisialisasi array untuk menyimpan nilai a, VaR, ES
a_values <- seq(0.01, 0.99, length.out = 100) 
VaR11_values <- c()
ES11_values <- c()

# Iterasi melalui berbagai a dan hitung VaR ES
for (a in a_values) {
  res2 <- calculate_var_es11(a, omega1_11, alpha1_11, beta1_11, omega2_11, alpha21_11, beta21_11, correlation, alpha)
  VaR11_values <- c(VaR11_values, res2$VaR11)
  ES11_values <- c(ES11_values, res2$ES11)
}

# GARCH(1,2)
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

# Define alpha level for VaR and ES
alpha <- 0.95

# Fungsi untuk prediksi VaR dan ES
calculate_var_es12 <- function(a, omega1_12, alpha1_12, beta1_12, beta2_12, omega2_12, alpha21_12, beta21_12, beta22_12, correlation, alpha) {
  esigma_12 <- sqrt(omega1_12 + alpha1_12 *  return_1[n]^2 + beta1_12 * m_12@sigma.t[n]^2 + beta2_12 * m_12@sigma.t[n-1]^2)
  esigma2_12 <- sqrt(omega2_12 + alpha21_12 * return_2[m]^2 + beta21_12 * m2_12@sigma.t[m]^2 + beta21_12 * m2_12@sigma.t[m-1]^2)
  esigma_tot12 <- a^2*esigma_12^2 + (1-a)^2*esigma2_12^2 + 2*correlation*esigma_12*esigma2_12*a*(1-a)
  
  #prediksi ukuran risiko
  VaR_preds12 <- qnorm(alpha) * sqrt(esigma_tot12)
  ES_preds12 <- (dnorm(qnorm(alpha)) / (1-alpha)) * sqrt(esigma_tot12)
  
  return(list(VaR12 = VaR_preds12, ES12 = ES_preds12))
}

# Inisialisasi array untuk menyimpan nilai a, VaR, ES
a_values <- seq(0.01, 0.99, length.out = 100) 
VaR12_values <- c()
ES12_values <- c()

# Iterasi melalui berbagai a dan hitung VaR ES
for (a in a_values) {
  res3 <- calculate_var_es12(a, omega1_12, alpha1_12, beta1_12, beta2_12, omega2_12, alpha21_12, beta21_12, beta22_12, correlation, alpha)
  VaR12_values <- c(VaR12_values, res3$VaR12)
  ES12_values <- c(ES12_values, res3$ES12)
}


# GARCH(2,1)
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

# Fungsi untuk prediksi VaR dan ES
calculate_var_es21 <- function(a, omega1_21, alpha1_21, alpha2_21, beta1_21, omega2_21, alpha21_21, alpha22_21, beta21_21, correlation, alpha) {
  esigma_21 <- sqrt(omega1_21 + alpha1_21 *  return_1[n]^2 + alpha2_21 *  0.004815044^2 + beta1_21 * m_21@sigma.t[n]^2)
  esigma2_21 <- sqrt(omega2_21 + alpha21_21 * return_2[m]^2 + alpha22_21 * 0.007957908^2 + beta21_21 * m2_21@sigma.t[m]^2)
  esigma_tot21 <- a^2*esigma_21^2 + (1-a)^2*esigma2_21^2 + 2*correlation*esigma_21*esigma2_21*a*(1-a)
  
  #prediksi ukuran risiko
  VaR_preds21 <- qnorm(alpha) * sqrt(esigma_tot21)
  ES_preds21 <- (dnorm(qnorm(alpha)) / (1-alpha)) * sqrt(esigma_tot21)
  
  return(list(VaR21 = VaR_preds21, ES21 = ES_preds21))
}

# Inisialisasi array untuk menyimpan nilai a, VaR, ES
a_values <- seq(0.01, 0.99, length.out = 100) 
VaR21_values <- c()
ES21_values <- c()

# Iterasi melalui berbagai a dan hitung VaR ES
for (a in a_values) {
  res4 <- calculate_var_es21(a, omega1_21, alpha1_21, alpha2_21, beta1_21, omega2_21, alpha21_21, alpha22_21, beta21_21, correlation, alpha)
  VaR21_values <- c(VaR21_values, res4$VaR21)
  ES21_values <- c(ES21_values, res4$ES21)
}




# Plot VaR hasilnya
df <- data.frame(a = a_values, VaR10 = VaR10_values, VaR11 = VaR11_values, VaR12 = VaR12_values, VaR21 = VaR21_values)

ggplot(df, aes(x = a)) +
  geom_line(aes(y = VaR10, color = "GARCH(1,0)"), lwd=1) +
  geom_line(aes(y = VaR11, color = "GARCH(1,1)"), lwd=1) +
  geom_line(aes(y = VaR12, color = "GARCH(1,2)"), lwd=1) +
  geom_line(aes(y = VaR21, color = "GARCH(2,1)"), lwd=1) +
  labs(title = "Pengaruh Proporsi a terhadap Prediksi VaR 0.95", x = "a", y = "VaR") +
  scale_color_manual("", values = c("GARCH(1,0)" = "red", "GARCH(1,1)" = "green", "GARCH(1,2)" = "blue", "GARCH(2,1)" = "purple")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.grid.major = element_blank(), # Menghilangkan garis bantu mayor
    panel.grid.minor = element_blank(), # Menghilangkan garis bantu minor
    panel.border = element_rect(colour = "black", fill = NA) # Menambahkan border hitam
  )

# Plot ES hasilnya
df2 <- data.frame(a = a_values, ES10 = ES10_values, ES11 = ES11_values, ES12 = ES12_values, ES21 = ES21_values)

ggplot(df2, aes(x = a)) +
  geom_line(aes(y = ES10, color = "GARCH(1,0)"), lwd=1) +
  geom_line(aes(y = ES11, color = "GARCH(1,1)"), lwd=1) +
  geom_line(aes(y = ES12, color = "GARCH(1,2)"), lwd=1) +
  geom_line(aes(y = ES21, color = "GARCH(2,1)"), lwd=1) +
  labs(title = "Pengaruh Proporsi a terhadap Prediksi ES 0.95", x = "a", y = "ES") +
  scale_color_manual("", values = c("GARCH(1,0)" = "red", "GARCH(1,1)" = "green", "GARCH(1,2)" = "blue", "GARCH(2,1)" = "purple")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.grid.major = element_blank(), # Menghilangkan garis bantu mayor
    panel.grid.minor = element_blank(), # Menghilangkan garis bantu minor
    panel.border = element_rect(colour = "black", fill = NA) # Menambahkan border hitam
  )


# Menghitung return harian
return_1 <- na.omit(-diff(log(Cl(GSPC))))
return_2 <- na.omit(-diff(log(Cl(RUT))))
n <- length(return_1)
m <- length(return_2)


# Menghitung korelasi antara return
correlation <- cor(return_1, return_2)
correlation

# Define alpha level for VaR and ES
alpha <- 0.95

# Fungsi untuk prediksi VaR dan ES
calculate_var_es <- function(a, mean, sigma1, sigma2, sigma, correlation, alpha) {
  mean <- 0
  sigma1 <- sd(return_1)
  sigma2 <- sd(return_2)
  sigma <- sqrt(a^2 * sigma1^2 + (1-a)^2 * sigma2^2 + 2 * correlation * sigma1 * sigma2 * a * (1-a))
  
  # Prediksi VaR
  VaR_preds <- qnorm(alpha) * sigma
  
  # Prediksi ES
  ES_preds <- (dnorm(qnorm(alpha)) / (1-alpha)) * sigma
  
  return(list(VaR = VaR_preds, ES = ES_preds))
}

# Inisialisasi array untuk menyimpan nilai a, VaR, ES
a_values <- seq(0.01, 0.99, length.out = 100) 
VaR_values <- c()
ES_values <- c()

correlation2 <- 0.345
# Iterasi melalui berbagai a dan hitung VaR ES
for (a in a_values) {
  res <- calculate_var_es(a, mean, sigma1, sigma2, sigma, -correlation2, alpha)
  VaR_values <- c(VaR_values, res$VaR)
  ES_values <- c(ES_values, res$ES)
}

df2 <- data.frame(a = a_values, VaR = VaR_values, ES = ES_values)
ggplot(df2, aes(x = a)) +
  geom_line(aes(y = VaR, color = "VaR")) +
  geom_line(aes(y = ES, color = "ES")) +
  labs(title = "Pengaruh Proporsi a terhadap Prediksi ES 0.99", x = "a", y = "ES") +
  scale_color_manual("", values = c("VaR" = "red", "ES" = "green")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.grid.major = element_blank(), # Menghilangkan garis bantu mayor
    panel.grid.minor = element_blank(), # Menghilangkan garis bantu minor
    panel.border = element_rect(colour = "black", fill = NA) # Menambahkan border hitam
  )

