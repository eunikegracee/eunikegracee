# Instal paket rugarch jika belum terinstal
if (!require(rugarch)) {
  install.packages("rugarch")
  library(rugarch)
}

# Mengatur seed untuk generator angka acak
set.seed(1234567)

# KONVERGEN
# Definisikan spesifikasi model GARCH(1,1) dengan parameter tetap
spec_garch11 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm",
  fixed.pars = list(omega = 0.001, alpha1 = 0.04, beta1 = 0.9)
)

# Definisikan spesifikasi model GARCH(1,0) dengan parameter tetap
spec_garch10 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm",
  fixed.pars = list(omega = 0.001, alpha1 = 0.9)
)

# Membuat simulasi model GARCH(1,1)
sim_garch11 <- ugarchpath(spec_garch11, n.sim = 1000)

# Membuat simulasi model GARCH(1,0)
sim_garch10 <- ugarchpath(spec_garch10, n.sim = 1000)

# Mendapatkan data simulasi
data_garch11 <- as.numeric(sim_garch11@path$seriesSim)
sigma_garch11 <- as.numeric(sim_garch11@path$sigmaSim)
data_garch10 <- as.numeric(sim_garch10@path$seriesSim)
sigma_garch10 <- as.numeric(sim_garch10@path$sigmaSim)

# Menghitung variansi tak bersyarat teoretis
unconditional_var_garch11 <- 0.001 / (1 - 0.04 - 0.9)
unconditional_var_garch10 <- 0.0007 / (1 - 0.9)

# Plot hasil simulasi GARCH(1,1)
#par(mfrow = c(2, 2))

# Plot data simulasi GARCH(1,1)
plot(data_garch11, type = 'l', col = 'black', ylab = expression(R[t]), xlab = 't', main = "Simulasi GARCH(1,1)")

# Plot mean GARCH(1,1)
mean_garch11 <- cumsum(data_garch11) / seq_along(data_garch11)
plot(mean_garch11, type = 'l', col = 'red', ylab = 'Mean', xlab = 't', main = "Mean GARCH(1,1)", lwd=3)

# Plot variansi GARCH(1,1) dengan variansi tak bersyarat
var_garch11 <- cumsum((data_garch11 - mean_garch11)^2) / seq_along(data_garch11)
plot(var_garch11, type = 'l', col = 'blue', ylab = 'Variansi', xlab = 't', main = "Variansi GARCH(1,1)", lwd=3)
abline(h = unconditional_var_garch11, col = 'black', lty = 2)

# Plot data, variansi, mean
plot(data_garch11, type = "l", ylab = expression(R[t]), xlab = "t", main = "Simulasi Data, Mean, dan Variansi GARCH(1,1)", col = "black")
lines(var_garch11, col = "blue", lwd=3)
lines(mean_garch11, col = "red", lwd=3)

# Plot data simulasi GARCH(1,0)
plot(data_garch10, type = 'l', col = 'black', ylab = expression(R[t]), xlab = 't', main = "Simulasi GARCH(1,0)")

# Plot mean GARCH(1,0)
mean_garch10 <- cumsum(data_garch10) / seq_along(data_garch10)
plot(mean_garch10, type = 'l', col = 'red', ylab = 'Mean', xlab = 't', main = "Mean GARCH(1,0)", lwd=3)

# Plot variansi GARCH(1,0) dengan variansi tak bersyarat
var_garch10 <- cumsum((data_garch10 - mean_garch10)^2) / seq_along(data_garch10)
plot(var_garch10, type = 'l', col = 'blue', ylab = 'Variansi', xlab = 't', main = "Variansi GARCH(1,0)", lwd=3)
abline(h = unconditional_var_garch10, col = 'black', lty = 2)

# Plot data, variansi, mean
plot(data_garch10, type = "l", ylab = expression(R[t]), xlab = "t", main = "Simulasi Data, Mean, dan Variansi GARCH(1,0)", col = "black")
lines(var_garch10, col = "blue", lwd=3)
lines(mean_garch10, col = "red", lwd=3)

# Menaksir parameter
# Menggunakan distribusi normal
library(fGarch)
m_10 <- garchFit(~ garch(1, 0), data = data_garch10, cond.dist = "norm")
summary(m_10)

m_11 <- garchFit(~ garch(1, 1), data = data_garch11, cond.dist = "norm")
summary(m_11)

# DIVERGEN 
# Mengatur seed untuk generator angka acak
#set.seed(1234567)

# Parameter model GARCH(1,1) yang tidak stasioner
omega <- 0.001
alpha1 <- 0.8
beta1 <- 0.4

# Panjang simulasi
n.sim <- 1000

# Vektor untuk menyimpan hasil simulasi
data_garch11_unstable <- numeric(n.sim)
sigma2_garch11_unstable <- numeric(n.sim)

# Inisialisasi nilai awal dengan nilai positif
sigma2_garch11_unstable[1] <- 0.1
data_garch11_unstable[1] <- rnorm(1, mean = 0, sd = sqrt(sigma2_garch11_unstable[1]))

# Simulasi model GARCH(1,1) yang tidak stasioner
for (t in 2:n.sim) {
  sigma2_garch11_unstable[t] <- omega + alpha1 * data_garch11_unstable[t-1]^2 + beta1 * sigma2_garch11_unstable[t-1]
  data_garch11_unstable[t] <- rnorm(1, mean = 0, sd = sqrt(sigma2_garch11_unstable[t]))
}

# Menghitung variansi empiris kumulatif
var_empirical_garch11_unstable <- cumsum((data_garch11_unstable - mean(data_garch11_unstable))^2) / seq_along(data_garch11_unstable)

# Menghitung mean empiris kumulatif
mean_empirical_garch11_unstable <- cumsum(data_garch11_unstable) / seq_along(data_garch11_unstable)

# Parameter model GARCH(1,0) yang tidak stasioner
omega <- 0.0007
alpha1 <- 1.3

# Vektor untuk menyimpan hasil simulasi
data_garch10_unstable <- numeric(n.sim)
sigma2_garch10_unstable <- numeric(n.sim)

# Inisialisasi nilai awal dengan nilai positif
sigma2_garch10_unstable[1] <- 0.1
data_garch10_unstable[1] <- rnorm(1, mean = 0, sd = sqrt(sigma2_garch10_unstable[1]))

# Simulasi model GARCH(1,0) yang tidak stasioner
for (t in 2:n.sim) {
  sigma2_garch10_unstable[t] <- omega + alpha1 * data_garch10_unstable[t-1]^2
  data_garch10_unstable[t] <- rnorm(1, mean = 0, sd = sqrt(sigma2_garch10_unstable[t]))
}

# Menghitung variansi empiris kumulatif
var_empirical_garch10_unstable <- cumsum((data_garch10_unstable - mean(data_garch10_unstable))^2) / seq_along(data_garch10_unstable)

# Menghitung mean empiris kumulatif
mean_empirical_garch10_unstable <- cumsum(data_garch10_unstable) / seq_along(data_garch10_unstable)


# Plot data simulasi GARCH(1,1) yang tidak stasioner
plot(data_garch11_unstable, type = 'l', col = 'black', ylab = expression(R[t]), xlab = 't', main = "Simulasi GARCH(1,1)")

# Plot mean kumulatif GARCH(1,1) yang tidak stasioner
plot(mean_empirical_garch11_unstable, type = 'l', col = 'red', ylab = 'Mean', xlab = 't', main = "Mean GARCH(1,1)", lwd=3)

# Plot variansi kumulatif GARCH(1,1) yang tidak stasioner
plot(var_empirical_garch11_unstable, type = 'l', col = 'blue', ylab = 'Variansi', xlab = 't', main = "Variansi GARCH(1,1)", lwd=3)

# Plot data simulasi GARCH(1,0) yang tidak stasioner
plot(data_garch10_unstable, type = 'l', col = 'black', ylab = expression(R[t]), xlab = 't', main = "Simulasi GARCH(1,0)")

# Plot mean kumulatif GARCH(1,0) yang tidak stasioner
plot(mean_empirical_garch10_unstable, type = 'l', col = 'red', ylab = 'Mean', xlab = 't', main = "Mean GARCH(1,0)", lwd=3)

# Plot variansi kumulatif GARCH(1,0) yang tidak stasioner
plot(var_empirical_garch10_unstable, type = 'l', col = 'blue', ylab = 'Variansi', xlab = 't', main = "Variansi GARCH(1,0)", lwd=3)

# Plot data, variansi, mean GARCH(1,0)
plot(data_garch10_unstable, type = "l", ylab = expression(R[t]), xlab = "t", main = "Simulasi Data, Mean, dan Variansi GARCH(1,0)", col = "black")
lines(var_empirical_garch10_unstable, col = "blue", lwd=3)
lines(mean_empirical_garch10_unstable, col = "red", lwd=3)

# Plot data, variansi, mean GARCH(1,1)
plot(data_garch11_unstable, type = "l", ylab = expression(R[t]), xlab = "t", main = "Simulasi Data, Mean, dan Variansi GARCH(1,1)", col = "black")
lines(var_empirical_garch11_unstable, col = "blue", lwd=3)
lines(mean_empirical_garch11_unstable, col = "red", lwd=3)

