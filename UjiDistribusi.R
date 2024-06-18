# Mengimpor paket yang diperlukan
if (!require("fitdistrplus")) install.packages("fitdistrplus")
if (!require("readxl")) install.packages("readxl")
library(fitdistrplus)
library(readxl)

# Mengimpor data dari Excel
data_ggrm <- readxl::read_excel("ggrm.xlsx")

# Uji distribusi untuk data "Open"
fit_dist_open <- fitdist(data_ggrm$Open, "norm")  # Menggunakan distribusi normal

# Uji distribusi untuk data "Close"
fit_dist_close <- fitdist(data_ggrm$Close, "norm")  # Menggunakan distribusi normal

# Menampilkan hasil uji distribusi untuk data "Open"
print(summary(fit_dist_open))

# Menampilkan hasil uji distribusi untuk data "Close"
print(summary(fit_dist_close))

# Mendapatkan parameter distribusi normal dari hasil fitting
mean_open <- fit_dist_open$estimate[1]
sd_open <- fit_dist_open$estimate[2]
mean_close <- fit_dist_close$estimate[1]
sd_close <- fit_dist_close$estimate[2]

# Uji metode Kolmogorov-Smirnov untuk data "Open"
ks_result_open <- ks.test(data_ggrm$Open, "pnorm", mean_open, sd_open, alternative = "two.sided")

# Uji metode Kolmogorov-Smirnov untuk data "Close"
ks_result_close <- ks.test(data_ggrm$Close, "pnorm", mean_close, sd_close, alternative = "two.sided")

# Menampilkan hasil uji metode Kolmogorov-Smirnov untuk data "Open"
print(ks_result_open)

# Menampilkan hasil uji metode Kolmogorov-Smirnov untuk data "Close"
print(ks_result_close)
