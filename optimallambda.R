# Memasang dan memuat paket yang diperlukan
if (!require("MASS")) install.packages("MASS")
library(MASS)
if (!require("tseries")) install.packages("tseries")
library(tseries)
if (!require("readxl")) install.packages("readxl")
library(readxl)
if (!require("forecast")) install.packages("forecast")
library(forecast)

# Membaca data dari file Excel
data_ggrm <- read_excel("ggrm.xlsx")

# Memastikan kolom "Open" dan "Close" ada dalam data
if (!("Open" %in% names(data_ggrm)) || !("Close" %in% names(data_ggrm))) {
  stop("Kolom 'Open' atau 'Close' tidak ditemukan dalam data.")
}

# Membuat time series dari data "Open" dan "Close"
data_open_ts <- ts(data_ggrm$Open)
data_close_ts <- ts(data_ggrm$Close)

# Fungsi untuk transformasi Box-Cox dan differencing
transform_and_difference <- function(data_ts) {
  # Uji stasioneritas varian menggunakan Box-Cox dengan lambda dibatasi sampai 1
  bc <- boxcox(data_ts ~ 1, lambda = seq(-2, 1, by = 0.1), plotit = FALSE)
  lambda <- bc$x[which.max(bc$y)]
  
  # Jika lambda mendekati 1, gunakan nilai yang lebih mendekati 1
  if (lambda > 0.5) {
    lambda <- 1
  } else if (lambda < 0.5 && lambda > 0) {
    lambda <- 0.5
  } else {
    lambda <- 0  # default jika lambda tidak mendekati 1 atau positif
  }
  
  # Melakukan transformasi Box-Cox
  transformed_data <- if (lambda == 0) {
    log(data_ts)
  } else {
    (data_ts^lambda - 1) / lambda
  }
  
  # Melakukan differencing pada hasil transformasi
  diff_transformed_data <- diff(transformed_data)
  
  return(list(transformed_data = transformed_data, diff_transformed_data = diff_transformed_data, lambda = lambda, bc = bc))
}

# Transformasi dan differencing data "Open"
open_result <- transform_and_difference(data_open_ts)

# Transformasi dan differencing data "Close"
close_result <- transform_and_difference(data_close_ts)

# Plot hasil transformasi dan differencing data "Open"
plot(open_result$diff_transformed_data, type = "l", main = "Differenced Transformed Data - Open", xlab = "Index", ylab = "Differenced Transformed Data")
grid()

# Plot ACF dari data yang telah di-differencing dan di-transformasi - Open
acf(open_result$diff_transformed_data, main = "ACF dari Data yang Telah Di-differencing dan Di-transformasi - Open")

# Plot hasil transformasi dan differencing data "Close"
plot(close_result$diff_transformed_data, type = "l", main = "Differenced Transformed Data - Close", xlab = "Index", ylab = "Differenced Transformed Data")
grid()

# Plot ACF dari data yang telah di-differencing dan di-transformasi - Close
acf(close_result$diff_transformed_data, main = "ACF dari Data yang Telah Di-differencing dan Di-transformasi - Close")

# Memperkirakan model MA(1) pada data yang telah di-differencing dan di-transformasi - Open
ma1_model_open <- Arima(open_result$diff_transformed_data, order = c(0, 0, 1))

# Menampilkan hasil estimasi model MA(1) - Open
cat("Hasil Estimasi Model MA(1) untuk Open:\n")
print(summary(ma1_model_open))

# Menampilkan nilai Mean Squared Error (MSE), Log-Likelihood, dan AIC dari model MA(1) - Open
mse_open <- mean(residuals(ma1_model_open)^2)
log_likelihood_open <- logLik(ma1_model_open)
aic_open <- AIC(ma1_model_open)

cat("Mean Squared Error (MSE) untuk Open:\n")
print(mse_open)

cat("Log-Likelihood untuk Open:\n")
print(log_likelihood_open)

cat("Akaike Information Criterion (AIC) untuk Open:\n")
print(aic_open)

# Plot hasil transformasi dan differencing data "Open" dan model MA(1) - Open
plot(open_result$diff_transformed_data, type = "l", col = "black", main = "MA(1) Model - Open and Close", xlab = "Index", ylab = "Differenced Transformed Data")
lines(fitted(ma1_model_open), col = "blue")

# Memperkirakan model MA(1) pada data yang telah di-differencing dan di-transformasi - Close
ma1_model_close <- Arima(close_result$diff_transformed_data, order = c(0, 0, 1))

# Menampilkan hasil estimasi model MA(1) - Close
cat("Hasil Estimasi Model MA(1) untuk Close:\n")
print(summary(ma1_model_close))

# Menampilkan nilai Mean Squared Error (MSE), Log-Likelihood, dan AIC dari model MA(1) - Close
mse_close <- mean(residuals(ma1_model_close)^2)
log_likelihood_close <- logLik(ma1_model_close)
aic_close <- AIC(ma1_model_close)

cat("Mean Squared Error (MSE) untuk Close:\n")
print(mse_close)

cat("Log-Likelihood untuk Close:\n")
print(log_likelihood_close)

cat("Akaike Information Criterion (AIC) untuk Close:\n")
print(aic_close)

# Plot hasil transformasi dan differencing data "Close" dan model MA(1) - Close
lines(close_result$diff_transformed_data, col = "red")
lines(fitted(ma1_model_close), col = "green")

# Menambahkan legenda
legend("topright", legend = c("Open", "Close", "MA(1) Open", "MA(1) Close"), col = c("black", "red", "blue", "green"), lty = 1)

# Memperkirakan model MA(2) pada data yang telah di-differencing dan di-transformasi - Open
ma2_model_open <- Arima(open_result$diff_transformed_data, order = c(0, 0, 2))

# Menampilkan hasil estimasi model MA(2) - Open
cat("Hasil Estimasi Model MA(2) untuk Open:\n")
print(summary(ma2_model_open))

# Menampilkan nilai Mean Squared Error (MSE), Log-Likelihood, dan AIC dari model MA(2) - Open
mse_open_ma2 <- mean(residuals(ma2_model_open)^2)
log_likelihood_open_ma2 <- logLik(ma2_model_open)
aic_open_ma2 <- AIC(ma2_model_open)

cat("Mean Squared Error (MSE) untuk MA(2) - Open:\n")
print(mse_open_ma2)

cat("Log-Likelihood untuk MA(2) - Open:\n")
print(log_likelihood_open_ma2)

cat("Akaike Information Criterion (AIC) untuk MA(2) - Open:\n")
print(aic_open_ma2)

# Plot hasil transformasi dan differencing data "Open" serta model MA(2) - Open
plot(open_result$diff_transformed_data, type = "l", col = "black", main = "MA(2) Model - Open and Close", xlab = "Index", ylab = "Differenced Transformed Data")
lines(fitted(ma2_model_open), col = "blue")

# Memperkirakan model MA(2) pada data yang telah di-differencing dan di-transformasi - Close
ma2_model_close <- Arima(close_result$diff_transformed_data, order = c(0, 0, 2))

# Menampilkan hasil estimasi model MA(2) - Close
cat("\nHasil Estimasi Model MA(2) untuk Close:\n")
print(summary(ma2_model_close))

# Menampilkan nilai Mean Squared Error (MSE), Log-Likelihood, dan AIC dari model MA(2) - Close
mse_close_ma2 <- mean(residuals(ma2_model_close)^2)
log_likelihood_close_ma2 <- logLik(ma2_model_close)
aic_close_ma2 <- AIC(ma2_model_close)

cat("Mean Squared Error (MSE) untuk MA(2) - Close:\n")
print(mse_close_ma2)

cat("Log-Likelihood untuk MA(2) - Close:\n")
print(log_likelihood_close_ma2)

cat("Akaike Information Criterion (AIC) untuk MA(2) - Close:\n")
print(aic_close_ma2)

# Plot hasil transformasi dan differencing data "Close" serta model MA(2) - Close
lines(close_result$diff_transformed_data, col = "red")
lines(fitted(ma2_model_close), col = "green")

# Menambahkan legenda
legend("topright", legend = c("Open", "Close", "MA(2) Open", "MA(2) Close"), col = c("black", "red", "blue", "green"), lty = 1)

# Memperkirakan model AR(1) pada data yang telah di-differencing dan di-transformasi - Open
ar1_model_open <- Arima(open_result$diff_transformed_data, order = c(1, 0, 0))

# Menampilkan hasil estimasi model AR(1) - Open
cat("Hasil Estimasi Model AR(1) untuk Open:\n")
print(summary(ar1_model_open))

# Menampilkan nilai Mean Squared Error (MSE), Log-Likelihood, dan AIC dari model AR(1) - Open
mse_open_ar1 <- mean(residuals(ar1_model_open)^2)
log_likelihood_open_ar1 <- logLik(ar1_model_open)
aic_open_ar1 <- AIC(ar1_model_open)

cat("Mean Squared Error (MSE) untuk AR(1) - Open:\n")
print(mse_open_ar1)

cat("Log-Likelihood untuk AR(1) - Open:\n")
print(log_likelihood_open_ar1)

cat("Akaike Information Criterion (AIC) untuk AR(1) - Open:\n")
print(aic_open_ar1)

# Plot hasil transformasi dan differencing data "Open" serta model AR(1) - Open
plot(open_result$diff_transformed_data, type = "l", col = "black", main = "AR(1) Model - Open and Close", xlab = "Index", ylab = "Differenced Transformed Data")
lines(fitted(ar1_model_open), col = "blue")

# Memperkirakan model AR(1) pada data yang telah di-differencing dan di-transformasi - Close
ar1_model_close <- Arima(close_result$diff_transformed_data, order = c(1, 0, 0))

# Menampilkan hasil estimasi model AR(1) - Close
cat("\nHasil Estimasi Model AR(1) untuk Close:\n")
print(summary(ar1_model_close))

# Menampilkan nilai Mean Squared Error (MSE), Log-Likelihood, dan AIC dari model AR(1) - Close
mse_close_ar1 <- mean(residuals(ar1_model_close)^2)
log_likelihood_close_ar1 <- logLik(ar1_model_close)
aic_close_ar1 <- AIC(ar1_model_close)

cat("Mean Squared Error (MSE) untuk AR(1) - Close:\n")
print(mse_close_ar1)

cat("Log-Likelihood untuk AR(1) - Close:\n")
print(log_likelihood_close_ar1)

cat("Akaike Information Criterion (AIC) untuk AR(1) - Close:\n")
print(aic_close_ar1)

# Plot hasil transformasi dan differencing data "Close" serta model AR(1) - Close
lines(close_result$diff_transformed_data, col = "red")
lines(fitted(ar1_model_close), col = "green")

# Menambahkan legenda
legend("topright", legend = c("Open", "Close", "AR(1) Open", "AR(1) Close"), col = c("black", "red", "blue", "green"), lty = 1)

# Memperkirakan model AR(2) pada data yang telah di-differencing dan di-transformasi - Open
ar2_model_open <- Arima(open_result$diff_transformed_data, order = c(2, 0, 0))

# Menampilkan hasil estimasi model AR(2) - Open
cat("Hasil Estimasi Model AR(2) untuk Open:\n")
print(summary(ar2_model_open))

# Menampilkan nilai Mean Squared Error (MSE), Log-Likelihood, dan AIC dari model AR(2) - Open
mse_open_ar2 <- mean(residuals(ar2_model_open)^2)
log_likelihood_open_ar2 <- logLik(ar2_model_open)
aic_open_ar2 <- AIC(ar2_model_open)

cat("Mean Squared Error (MSE) untuk AR(2) - Open:\n")
print(mse_open_ar2)

cat("Log-Likelihood untuk AR(2) - Open:\n")
print(log_likelihood_open_ar2)

cat("Akaike Information Criterion (AIC) untuk AR(2) - Open:\n")
print(aic_open_ar2)

# Plot hasil transformasi dan differencing data "Open" serta model AR(2) - Open
plot(open_result$diff_transformed_data, type = "l", col = "black", main = "AR(2) Model - Open and Close", xlab = "Index", ylab = "Differenced Transformed Data")
lines(fitted(ar2_model_open), col = "blue")

# Memperkirakan model AR(2) pada data yang telah di-differencing dan di-transformasi - Close
ar2_model_close <- Arima(close_result$diff_transformed_data, order = c(2, 0, 0))

# Menampilkan hasil estimasi model AR(2) - Close
cat("\nHasil Estimasi Model AR(2) untuk Close:\n")
print(summary(ar2_model_close))

# Menampilkan nilai Mean Squared Error (MSE), Log-Likelihood, dan AIC dari model AR(2) - Close
mse_close_ar2 <- mean(residuals(ar2_model_close)^2)
log_likelihood_close_ar2 <- logLik(ar2_model_close)
aic_close_ar2 <- AIC(ar2_model_close)

cat("Mean Squared Error (MSE) untuk AR(2) - Close:\n")
print(mse_close_ar2)

cat("Log-Likelihood untuk AR(2) - Close:\n")
print(log_likelihood_close_ar2)

cat("Akaike Information Criterion (AIC) untuk AR(2) - Close:\n")
print(aic_close_ar2)

# Plot hasil transformasi dan differencing data "Close" serta model AR(2) - Close
lines(close_result$diff_transformed_data, col = "red")
lines(fitted(ar2_model_close), col = "green")

# Menambahkan legenda
legend("topright", legend = c("Open", "Close", "AR(2) Open", "AR(2) Close"), col = c("black", "red", "blue", "green"), lty = 1)

# Memperkirakan model ARMA(1,1) pada data yang telah di-differencing dan di-transformasi - Open
arma11_model_open <- Arima(open_result$diff_transformed_data, order = c(1, 0, 1))

# Menampilkan hasil estimasi model ARMA(1,1) - Open
cat("Hasil Estimasi Model ARMA(1,1) untuk Open:\n")
print(summary(arma11_model_open))

# Menampilkan nilai Mean Squared Error (MSE), Log-Likelihood, dan AIC dari model ARMA(1,1) - Open
mse_open_arma11 <- mean(residuals(arma11_model_open)^2)
log_likelihood_open_arma11 <- logLik(arma11_model_open)
aic_open_arma11 <- AIC(arma11_model_open)

cat("Mean Squared Error (MSE) untuk ARMA(1,1) - Open:\n")
print(mse_open_arma11)

cat("Log-Likelihood untuk ARMA(1,1) - Open:\n")
print(log_likelihood_open_arma11)

cat("Akaike Information Criterion (AIC) untuk ARMA(1,1) - Open:\n")
print(aic_open_arma11)

# Plot hasil transformasi dan differencing data "Open" serta model ARMA(1,1) - Open
plot(open_result$diff_transformed_data, type = "l", col = "black", main = "ARMA(1,1) Model - Open and Close", xlab = "Index", ylab = "Differenced Transformed Data")
lines(fitted(arma11_model_open), col = "blue")

# Memperkirakan model ARMA(1,1) pada data yang telah di-differencing dan di-transformasi - Close
arma11_model_close <- Arima(close_result$diff_transformed_data, order = c(1, 0, 1))

# Menampilkan hasil estimasi model ARMA(1,1) - Close
cat("\nHasil Estimasi Model ARMA(1,1) untuk Close:\n")
print(summary(arma11_model_close))

# Menampilkan nilai Mean Squared Error (MSE), Log-Likelihood, dan AIC dari model ARMA(1,1) - Close
mse_close_arma11 <- mean(residuals(arma11_model_close)^2)
log_likelihood_close_arma11 <- logLik(arma11_model_close)
aic_close_arma11 <- AIC(arma11_model_close)

cat("Mean Squared Error (MSE) untuk ARMA(1,1) - Close:\n")
print(mse_close_arma11)

cat("Log-Likelihood untuk ARMA(1,1) - Close:\n")
print(log_likelihood_close_arma11)

cat("Akaike Information Criterion (AIC) untuk ARMA(1,1) - Close:\n")
print(aic_close_arma11)

# Plot hasil transformasi dan differencing data "Close" serta model ARMA(1,1) - Close
lines(close_result$diff_transformed_data, col = "red")
lines(fitted(arma11_model_close), col = "green")

# Menambahkan legenda
legend("topright", legend = c("Open", "Close", "ARMA(1,1) Open", "ARMA(1,1) Close"), col = c("black", "red", "blue", "green"), lty = 1)

# Memperkirakan model ARIMA(1,1,0) pada data yang telah di-differencing dan di-transformasi - Open
arima110_model_open <- Arima(open_result$diff_transformed_data, order = c(1, 1, 0))

# Menampilkan hasil estimasi model ARIMA(1,1,0) - Open
cat("Hasil Estimasi Model ARIMA(1,1,0) untuk Open:\n")
print(summary(arima110_model_open))

# Menampilkan nilai Mean Squared Error (MSE), Log-Likelihood, dan AIC dari model ARIMA(1,1,0) - Open
mse_open_arima110 <- mean(residuals(arima110_model_open)^2)
log_likelihood_open_arima110 <- logLik(arima110_model_open)
aic_open_arima110 <- AIC(arima110_model_open)

cat("Mean Squared Error (MSE) untuk ARIMA(1,1,0) - Open:\n")
print(mse_open_arima110)

cat("Log-Likelihood untuk ARIMA(1,1,0) - Open:\n")
print(log_likelihood_open_arima110)

cat("Akaike Information Criterion (AIC) untuk ARIMA(1,1,0) - Open:\n")
print(aic_open_arima110)

# Plot hasil transformasi dan differencing data "Open" serta model ARIMA(1,1,0) - Open
plot(open_result$diff_transformed_data, type = "l", col = "black", main = "ARIMA(1,1,0) Model - Open and Close", xlab = "Index", ylab = "Differenced Transformed Data")
lines(fitted(arima110_model_open), col = "blue")

# Memperkirakan model ARIMA(1,1,0) pada data yang telah di-differencing dan di-transformasi - Close
arima110_model_close <- Arima(close_result$diff_transformed_data, order = c(1, 1, 0))

# Menampilkan hasil estimasi model ARIMA(1,1,0) - Close
cat("\nHasil Estimasi Model ARIMA(1,1,0) untuk Close:\n")
print(summary(arima110_model_close))

# Menampilkan nilai Mean Squared Error (MSE), Log-Likelihood, dan AIC dari model ARIMA(1,1,0) - Close
mse_close_arima110 <- mean(residuals(arima110_model_close)^2)
log_likelihood_close_arima110 <- logLik(arima110_model_close)
aic_close_arima110 <- AIC(arima110_model_close)

cat("Mean Squared Error (MSE) untuk ARIMA(1,1,0) - Close:\n")
print(mse_close_arima110)

cat("Log-Likelihood untuk ARIMA(1,1,0) - Close:\n")
print(log_likelihood_close_arima110)

cat("Akaike Information Criterion (AIC) untuk ARIMA(1,1,0) - Close:\n")
print(aic_close_arima110)

# Plot hasil transformasi dan differencing data "Close" serta model ARIMA(1,1,0) - Close
lines(close_result$diff_transformed_data, col = "red")
lines(fitted(arima110_model_close), col = "green")

# Menambahkan legenda
legend("topright", legend = c("Open", "Close", "ARIMA(1,1,0) Open", "ARIMA(1,1,0) Close"), col = c("black", "red", "blue", "green"), lty = 1)
