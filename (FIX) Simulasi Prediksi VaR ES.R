# Instal dan muat paket rugarch jika belum diinstal
install.packages("rugarch")
library(rugarch)

# Set seed for reproducibility
set.seed(1234567)

# Number of data points
n = 1000

# GARCH(1,0)
# Spesifikasi model GARCH(1,0) untuk simulasi
spec_sim = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm",
  fixed.pars = list(omega = 0.0001, alpha1 = 0.04)
)

# Simulasi data menggunakan model GARCH(1,0)
sim_garch10 = ugarchpath(spec_sim, n.sim = n)

# Mengambil data yang disimulasikan
garch10_series = fitted(sim_garch10)

# Mengestimasi parameter GARCH(1,0)
# Menggunakan distribusi normal
library(fGarch)
m_10 <- garchFit(~ garch(1, 0), data = garch10_series, cond.dist = "norm")
summary(m_10)
coefficients1 <- coef(m_10)
mu_10 <- coefficients1["mu"]
omega1_10 <- coefficients1["omega"]
alpha1_10 <- coefficients1["alpha1"]

# Nilai parameter sebenarnya
true_omega <- 0.0001
true_alpha1 <- 0.04

# Taksiran parameter dari hasil estimasi
estimated_omega <- omega1_10
estimated_alpha1 <- alpha1_10

# Hitung bias
bias_omega <- estimated_omega - true_omega
bias_alpha1 <- estimated_alpha1 - true_alpha1

# Hitung MSE
mse_omega <- bias_omega^2
mse_alpha1 <- bias_alpha1^2

# Tampilkan hasil
cat("Bias untuk omega:", bias_omega, "\n")
cat("Bias untuk alpha1:", bias_alpha1, "\n")
cat("MSE untuk omega:", mse_omega, "\n")
cat("MSE untuk alpha1:", mse_alpha1, "\n")

# Mengambil volatilitas yang diestimasi
garch10_sigma = m_10@sigma.t

# Memprediksi nilai VaR dan ES pada tingkat kepercayaan 0.95
# Menghitung VaR dan ES pada level kepercayaan 95%
alpha = 0.95

# VaR untuk distribusi normal
VaR <- qnorm(alpha) * garch10_sigma

# ES untuk distribusi normal
ES <- (dnorm(qnorm(alpha)) / (1-alpha)) * garch10_sigma

# Prediksi VaR dan ES
# Menghitung sigma
esigma_10 <- sqrt(omega1_10 + alpha1_10 *  m_10@sigma.t[n]^2)
VaR_pred10 <- qnorm(alpha) * esigma_10
ES_pred10 <- (dnorm(qnorm(alpha)) / (1-alpha)) * esigma_10
VaR_pred10
ES_pred10

# Memprediksi nilai VaR dan ES pada tingkat kepercayaan 0.99
# Menghitung VaR dan ES pada level kepercayaan 99%
alpha2 = 0.99

# VaR untuk distribusi normal
VaR99 <- qnorm(alpha2) * garch10_sigma

# ES untuk distribusi normal
ES99 <- (dnorm(qnorm(alpha2)) / (1-alpha2)) * garch10_sigma

# Prediksi VaR dan ES
# Menghitung sigma
esigma_10 <- sqrt(omega1_10 + alpha1_10 *  m_10@sigma.t[n]^2)
VaR_pred10 <- qnorm(alpha2) * esigma_10
ES_pred10 <- (dnorm(qnorm(alpha2)) / (1-alpha2)) * esigma_10
VaR_pred10
ES_pred10

# Tata letak plot satu di atas yang lain
par(mfrow = c(1, 1))  # Atur ulang ke layout default (satu plot per jendela)
plot(garch10_series, type = "l", main = "Grafik Imbal Hasil dan VaR ES GARCH(1,0)", ylab = expression(R[t]), xlab = "t")
lines(VaR, col = "red", lwd = 2)
lines(ES, col = "blue", lwd = 2)
lines(VaR99, col = "purple", lwd = 2)
lines(ES99, col = "green", lwd = 2)
legend("bottom", legend = c("VaR (95%)", "ES (95%)", "VaR (99%)", "ES (99%)"), col = c("red", "blue", "purple", "green"), lty = 1, lwd = 2, ncol = 2)


# Plot data yang disimulasikan, VaR, dan ES
plot(garch10_series, type = "l", main = "Grafik Imbal Hasil dan VaR ES GARCH(1,0)", ylab = expression(R[t]), xlab = "t")
lines(VaR, col = "red", lwd = 2)
lines(ES, col = "blue", lwd = 2)
lines(VaR99, col = "purple", lwd = 2)
lines(ES99, col = "green", lwd = 2)
legend("bottom", legend = c("VaR (95%)", "ES (95%)", "VaR (99%)", "ES (99%)"), col = c("red", "blue", "purple", "green"), lty = 1, lwd = 2, ncol = 2)


######################################################################
# GARCH(1,1)
# Spesifikasi model GARCH(1,1) untuk simulasi
spec_sim11 = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm",
  fixed.pars = list(omega = 0.0001, alpha1 = 0.04, beta1 = 0.9)
)

n = 1000
# Simulasi data menggunakan model GARCH(1,1)
sim_garch11 = ugarchpath(spec_sim11, n.sim = n)

# Mengambil data yang disimulasikan
garch11_series = fitted(sim_garch11)

# Mengestimasi parameter GARCH(1,1)
# Menggunakan distribusi normal
library(fGarch)
m_11 <- garchFit(~ garch(1, 1), data = garch11_series, cond.dist = "norm")
summary(m_11)
coefficients2 <- coef(m_11)
omega1_11 <- coefficients2["omega"]
alpha1_11 <- coefficients2["alpha1"]
beta1_11 <- coefficients2["beta1"]

# Nilai parameter sebenarnya
true_omega <- 0.0001
true_alpha1 <- 0.04
true_beta1 <- 0.9

# Taksiran parameter dari hasil estimasi
estimated_omega <- omega1_11
estimated_alpha1 <- alpha1_11
estimated_beta1 <- beta1_11

# Hitung bias
bias_omega <- estimated_omega - true_omega
bias_alpha1 <- estimated_alpha1 - true_alpha1
bias_beta1 <- estimated_beta1 - true_beta1

# Hitung MSE
mse_omega <- bias_omega^2
mse_alpha1 <- bias_alpha1^2
mse_beta1 <- bias_beta1^2

# Tampilkan hasil
cat("Bias untuk omega:", bias_omega, "\n")
cat("Bias untuk alpha1:", bias_alpha1, "\n")
cat("Bias untuk beta1:", bias_beta1, "\n")
cat("MSE untuk omega:", mse_omega, "\n")
cat("MSE untuk alpha1:", mse_alpha1, "\n")
cat("MSE untuk beta1:", mse_beta1, "\n")

# Mengambil volatilitas yang diestimasi
garch11_sigma = m_11@sigma.t

# Memprediksi nilai VaR dan ES pada tingkat kepercayaan 0.95
# Menghitung VaR dan ES pada level kepercayaan 95%
alpha = 0.95

# VaR untuk distribusi normal
VaR_11 <- qnorm(alpha) * garch11_sigma

# ES untuk distribusi normal
ES_11 <- (dnorm(qnorm(alpha)) / (1-alpha)) * garch11_sigma

# Prediksi VaR dan ES
# Menghitung sigma
esigma_11 <- sqrt(omega1_11 + alpha1_11 * (garch11_series[n])^2 +beta1_11*(m_11@sigma.t[n])^2)
VaR_pred11 <- qnorm(alpha) * esigma_11
ES_pred11 <- (dnorm(qnorm(alpha)) / (1-alpha)) * esigma_11
VaR_pred11
ES_pred11

# Memprediksi nilai VaR dan ES pada tingkat kepercayaan 0.99
# Menghitung VaR dan ES pada level kepercayaan 99%
alpha2 = 0.99

# VaR untuk distribusi normal
VaR99_11 <- qnorm(alpha2) * garch11_sigma

# ES untuk distribusi normal
ES99_11 <- (dnorm(qnorm(alpha2)) / (1-alpha2)) * garch11_sigma

# Prediksi VaR dan ES
# Menghitung sigma
esigma_11 <- sqrt(omega1_11 + alpha1_11 * (garch11_series[n])^2 +beta1_11*(m_11@sigma.t[n])^2)
VaR99_pred11 <- qnorm(alpha2) * esigma_11
ES99_pred11 <- (dnorm(qnorm(alpha2)) / (1-alpha2)) * esigma_11
VaR99_pred11
ES99_pred11

# Plot data yang disimulasikan, VaR, dan ES
plot(garch11_series, type = "l", main = "Grafik Imbal Hasil dan VaR ES GARCH(1,1)", ylab = expression(R[t]), xlab = "t")
lines(VaR_11, col = "red", lwd = 2)
lines(ES_11, col = "blue", lwd = 2)
lines(VaR99_11, col = "purple", lwd = 2)
lines(ES99_11, col = "green", lwd = 2)
legend("bottom", legend = c("VaR (95%)", "ES (95%)","VaR (99%)", "ES (99%)"), col = c("red", "blue", "purple", "green"), lty = 1, lwd = 2, ncol=2)


######################################################################
# GARCH(1,2)
# Spesifikasi model GARCH(1,2) untuk simulasi
spec_sim12 = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 2)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm",
  fixed.pars = list(omega = 0.0001, alpha1 = 0.04, beta1 = 0.9, beta2 = 0.02)
)

n <- 1000
# Simulasi data menggunakan model GARCH(1,2)
sim_garch12 = ugarchpath(spec_sim12, n.sim = n)

# Mengambil data yang disimulasikan
garch12_series = fitted(sim_garch12)

# Mengestimasi parameter GARCH(1,2)
# Menggunakan distribusi normal
library(fGarch)
m_12 <- garchFit(~ garch(1, 2), data = garch12_series, cond.dist = "norm")
summary(m_12)
coefficients3 <- coef(m_12)
omega1_12 <- coefficients3["omega"]
alpha1_12 <- coefficients3["alpha1"]
beta1_12 <- coefficients3["beta1"]
beta2_12 <- coefficients3["beta2"]

# Nilai parameter sebenarnya
true_omega <- 0.0001
true_alpha1 <- 0.04
true_beta1 <- 0.9
true_beta2 <- 0.02

# Taksiran parameter dari hasil estimasi
estimated_omega <- omega1_12
estimated_alpha1 <- alpha1_12
estimated_beta1 <- beta1_12
estimated_beta2 <- beta2_12

# Hitung bias
bias_omega <- estimated_omega - true_omega
bias_alpha1 <- estimated_alpha1 - true_alpha1
bias_beta1 <- estimated_beta1 - true_beta1
bias_beta2 <- estimated_beta2 - true_beta2

# Hitung MSE
mse_omega <- bias_omega^2
mse_alpha1 <- bias_alpha1^2
mse_beta1 <- bias_beta1^2
mse_beta2 <- bias_beta2^2

# Tampilkan hasil
cat("Bias untuk omega:", bias_omega, "\n")
cat("Bias untuk alpha1:", bias_alpha1, "\n")
cat("Bias untuk beta1:", bias_beta1, "\n")
cat("Bias untuk beta2:", bias_beta2, "\n")
cat("MSE untuk omega:", mse_omega, "\n")
cat("MSE untuk alpha1:", mse_alpha1, "\n")
cat("MSE untuk beta1:", mse_beta1, "\n")
cat("MSE untuk beta2:", mse_beta2, "\n")

# Mengambil volatilitas yang diestimasi
garch12_sigma = m_12@sigma.t

# Memprediksi nilai VaR dan ES pada tingkat kepercayaan 0.95
# Menghitung VaR dan ES pada level kepercayaan 95%
alpha = 0.95

# VaR untuk distribusi normal
VaR_12 <- qnorm(alpha) * garch12_sigma

# ES untuk distribusi normal
ES_12 <- (dnorm(qnorm(alpha)) / (1-alpha)) * garch12_sigma

# Prediksi VaR dan ES
# Menghitung sigma
esigma_12 <- sqrt(omega1_12 + alpha1_12 * (garch12_series[n])^2 + beta1_12*(m_12@sigma.t[n])^2 + beta2_12*(m_12@sigma.t[n-1])^2)
esigma_12
VaR_pred12 <- qnorm(alpha) * esigma_12
ES_pred12 <- (dnorm(qnorm(alpha)) / (1-alpha)) * esigma_12
VaR_pred12
ES_pred12

# Memprediksi nilai VaR dan ES pada tingkat kepercayaan 0.99
# Menghitung VaR dan ES pada level kepercayaan 99%
alpha2 = 0.99

# VaR untuk distribusi normal
VaR99_12 <- qnorm(alpha2) * garch12_sigma

# ES untuk distribusi normal
ES99_12 <- (dnorm(qnorm(alpha2)) / (1-alpha2)) * garch12_sigma

# Prediksi VaR dan ES
# Menghitung sigma
esigma_12 <- sqrt(omega1_12 + alpha1_12 * (garch12_series[n])^2 + beta1_12*(m_12@sigma.t[n])^2 + beta2_12*(m_12@sigma.t[n-1])^2)
VaR99_pred12 <- qnorm(alpha2) * esigma_12
ES99_pred12 <- (dnorm(qnorm(alpha2)) / (1-alpha2)) * esigma_12
VaR99_pred12
ES99_pred12

# Plot data yang disimulasikan, VaR, dan ES
plot(garch12_series, type = "l", main = "Grafik Imbal Hasil dan VaR ES GARCH(1,2)", ylab = expression(R[t]), xlab = "t")
lines(VaR_12, col = "red", lwd = 2)
lines(ES_12, col = "blue", lwd = 2)
lines(VaR99_12, col = "purple", lwd = 2)
lines(ES99_12, col = "green", lwd = 2)
legend("bottom", legend = c("VaR (95%)", "ES (95%)","VaR (99%)", "ES (99%)"), col = c("red", "blue", "purple", "green"), lty = 1, lwd = 2, ncol=2)


######################################################################
# GARCH(2,1)
# Spesifikasi model GARCH(2,1) untuk simulasi
spec_sim21 = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(2, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm",
  fixed.pars = list(omega = 0.0001, alpha1 = 0.04, alpha2 = 0.02, beta1 = 0.9)
)

n <- 1000
# Simulasi data menggunakan model GARCH(1,2)
sim_garch21 = ugarchpath(spec_sim21, n.sim = n)

# Mengambil data yang disimulasikan
garch21_series = fitted(sim_garch21)

# Mengestimasi parameter GARCH(2,1)
# Menggunakan distribusi normal
library(fGarch)
m_21 <- garchFit(~ garch(2, 1), data = garch21_series, cond.dist = "norm")
summary(m_21)
coefficients4 <- coef(m_21)
omega1_21 <- coefficients4["omega"]
alpha1_21 <- coefficients4["alpha1"]
alpha2_21 <- coefficients4["alpha2"]
beta1_21 <- coefficients4["beta1"]

# Nilai parameter sebenarnya
true_omega <- 0.0001
true_alpha1 <- 0.04
true_alpha2 <- 0.02
true_beta1 <- 0.9

# Taksiran parameter dari hasil estimasi
estimated_omega <- omega1_21
estimated_alpha1 <- alpha1_21
estimated_alpha2 <- alpha2_21
estimated_beta1 <- beta1_21

# Hitung bias
bias_omega <- estimated_omega - true_omega
bias_alpha1 <- estimated_alpha1 - true_alpha1
bias_alpha2 <- estimated_alpha2 - true_alpha2
bias_beta1 <- estimated_beta1 - true_beta1

# Hitung MSE
mse_omega <- bias_omega^2
mse_alpha1 <- bias_alpha1^2
mse_alpha2 <- bias_alpha2^2
mse_beta1 <- bias_beta1^2

# Tampilkan hasil
cat("Bias untuk omega:", bias_omega, "\n")
cat("Bias untuk alpha1:", bias_alpha1, "\n")
cat("Bias untuk alpha1:", bias_alpha2, "\n")
cat("Bias untuk beta1:", bias_beta1, "\n")
cat("MSE untuk omega:", mse_omega, "\n")
cat("MSE untuk alpha1:", mse_alpha1, "\n")
cat("MSE untuk alpha1:", mse_alpha2, "\n")
cat("MSE untuk beta1:", mse_beta1, "\n")


# Mengambil volatilitas yang diestimasi
garch21_sigma = m_21@sigma.t

# Memprediksi nilai VaR dan ES pada tingkat kepercayaan 0.95
# Menghitung VaR dan ES pada level kepercayaan 95%
alpha = 0.95

# VaR untuk distribusi normal
VaR_21 <- qnorm(alpha) * garch21_sigma

# ES untuk distribusi normal
ES_21 <- (dnorm(qnorm(alpha)) / (1-alpha)) * garch21_sigma

# Prediksi VaR dan ES
# Menghitung sigma
esigma_21 <- sqrt(omega1_21+ alpha1_21 * (garch21_series[n])^2 + alpha2_21 * (garch21_series[n-1])^2 + beta1_21*(m_21@sigma.t[n])^2)
esigma_21
VaR_pred21 <- qnorm(alpha) * esigma_21
ES_pred21 <- (dnorm(qnorm(alpha)) / (1-alpha)) * esigma_21
VaR_pred21
ES_pred21

# Memprediksi nilai VaR dan ES pada tingkat kepercayaan 0.99
# Menghitung VaR dan ES pada level kepercayaan 99%
alpha2 = 0.99

# VaR untuk distribusi normal
VaR99_21 <- qnorm(alpha2) * garch21_sigma

# ES untuk distribusi normal
ES99_21 <- (dnorm(qnorm(alpha2)) / (1-alpha2)) * garch21_sigma

# Prediksi VaR dan ES
# Menghitung sigma
esigma_21 <- sqrt(omega1_21+ alpha1_21 * (garch21_series[n])^2 + alpha2_21 * (garch21_series[n-1])^2 + beta1_21*(m_21@sigma.t[n])^2)
VaR99_pred21 <- qnorm(alpha2) * esigma_21
ES99_pred21 <- (dnorm(qnorm(alpha2)) / (1-alpha2)) * esigma_21
VaR99_pred21
ES99_pred21

# Plot data yang disimulasikan, VaR, dan ES
plot(garch21_series, type = "l", main = "Grafik Imbal Hasil dan VaR ES GARCH(2,1)", ylab = expression(R[t]), xlab = "t")
lines(VaR_21, col = "red", lwd = 2)
lines(ES_21, col = "blue", lwd = 2)
lines(VaR99_21, col = "purple", lwd = 2)
lines(ES99_21, col = "green", lwd = 2)
legend("bottom", legend = c("VaR (95%)", "ES (95%)","VaR (99%)", "ES (99%)"), col = c("red", "blue", "purple", "green"), lty = 1, lwd = 2, ncol=2)
