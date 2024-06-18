# Menghitung ACF untuk data "Open"
acf_open <- acf(data_ggrm$Open, plot = FALSE,)

# Menghitung ACF untuk data "Close"
acf_close <- acf(data_ggrm$Close, plot = FALSE)

# Plot ACF untuk kedua data "Open" dan "Close"
par(mfrow=c(2, 1))  # Mengatur tata letak menjadi dua baris
plot(acf_open, main = "Autocorrelation Function (ACF) for Open Data")
plot(acf_close, main = "Autocorrelation Function (ACF) for Close Data")