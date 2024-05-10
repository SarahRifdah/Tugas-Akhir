library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

# Plot kurva Stress-Strain Triaxial CU
ggplot(RUN_R) +
  geom_line(aes(x = Strain, y = Stress, color = Conf, group = Sampel)) +
  labs(title = "Data Triaxial CU", x = "Strain (%)", y = "Stress (kPa)")

# Cleaning data
df <- slice(RUN_R, -c(2030:2031)) # Hapus data outliers

# Cek database terbaru
ggplot(df) +
  geom_line(aes(x = Strain, y = Stress, color = Conf,group = Sampel)) +
  labs(title = "Data Triaxial CU",x = "Strain (%)", y = "Stress (kPa)")

# Distribusi dan persebaran data
ggplot(df %>% 
         melt(id.vars = NULL),
         aes(x = value)) +
  geom_histogram(fill = "dodgerblue", color = "black") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Histograms Distribusi Data")

# Split data train dan test
# Menghitung jumlah baris unik dalam dataframe
jumlah_sampel_ols <- length(unique(df$Sampel))

# Jumlah data training yang diinginkan (70% dari total data)
jumlah_training_ols <- round(0.7 * jumlah_sampel_ols)

# Menetapkan seed untuk reproduktibilitas hasil acak
set.seed(123)

# Indeks acak untuk pemisahan data training dan testing
indeks_ols <- sample(1:jumlah_sampel_ols)

# Splitting data training dan testing berdasarkan indeks acak
data_training <- df[df$Sampel %in% 
                      unique(df$Sampel)
                    [indeks_ols[1:jumlah_training_ols]], ]
data_testing <- df[df$Sampel %in% 
                     unique(df$Sampel)
                   [indeks_ols[(jumlah_training_ols + 1):
                                 jumlah_sampel_ols]], ]

# Membuat plot data training
plot_train <- ggplot(data_training, aes(
  x = Strain, 
  y = Stress,
  color = "Data Train", 
  group = Sampel
  )) +
  geom_line(linewidth = 1) +
  labs(
    x = "Strain(%)", 
    y = "Stress(kPa)",
    title = "Plot Data Train dan Test") +
  scale_color_manual(
    values = c("Data Train" = "dodgerblue"), 
    name = "Data Type")

# Menambahkan plot data testing ke dalam plot data training
plot_combined <- plot_train +
  geom_line(data = data_testing, aes(
    x = Strain, 
    y = Stress,
    color = "Data Test", 
    group = Sampel), 
    linewidth = 1) +
  scale_color_manual(
    values = c("Data Train" = "dodgerblue", "Data Test" = "deeppink"))

# Menampilkan plot yang telah digabungkan
print(plot_combined)

####### MODELLING#########
# Training Ordinary Least Square Model Total Stress Strain
model_ols <- lm(
  Stress ~ Strain + Conf + e0 + Gs + PL + LL + PI + Cc + Sr + ϒm,
  data_training)
summary(model_ols)

# Testing & Prediction Ordinary Least Square Model
prediksi_ols <- predict(model_ols, newdata = data_testing)

#### Analisis Sensitivitas Model RF####

# Buat data frame baru untuk plot berisi nilai error dan prediksi
Prediksi_ols <- data_testing %>%
  mutate(Stress_ols = prediksi_ols,
         Error = Stress - prediksi_ols) %>%
  select(Sampel, Strain, Stress, Stress_ols, Error)

# Cek nilai RMSE OLS Model
RMSE_ols <- sqrt(mean((data_testing$Stress - prediksi_ols)^2))

# Hitung selisih absolut antara nilai aktual dan nilai prediksi
MAE_ols <- mean(abs(data_testing$Stress - prediksi_ols))

# Plot 1:1 Stress Sebenarnya vs Stress Prediksi
ggplot(Prediksi_ols, aes(x = Stress, y = Stress_ols)) +
  geom_point() + # Menambahkan titik-titik
  geom_abline(intercept = 0, 
              slope = 1, 
              color = "red", 
              linetype = "dashed") + # Menambahkan garis 1:1
  labs(x = "Stress (kPa)", y = "Prediksi Stress (kPa)") + 
  ggtitle("Plot Predicted Stress vs Stress OLS") + 
  geom_text(
    x = 20, 
    y = 130,
    label = paste("RMSE:", round(RMSE_ols, 2), "\n", "MAE:", round(MAE_ols, 2)),
    hjust = 1, 
    vjust = 0, 
    color = "black", 
    size = 4)

# Plot strain vs predicted OLS model
ggplot(Prediksi_ols, aes(x = Strain, y = Stress, group = Sampel)) +
  geom_point() + # Tambahkan titik data
  geom_line(aes(y = Stress_ols), color = "blue") + # Tambahkan garis regresi
  labs(
    title = "Plot Hasil Regresi Linear", 
    x = "Strain (%)", 
    y = "Stress (kPa)") +
  theme_minimal() +
  annotate(
    "text",
    x = 11, 
    y = c(0, 11),  # Mengatur posisi y untuk kedua teks
    label = c(paste("RMSE:", round(RMSE_ols, 2)),
              paste("MAE:", round(MAE_ols, 2))),
    hjust = 1,
    vjust = c(1, 0.9), 
    color = "black")

