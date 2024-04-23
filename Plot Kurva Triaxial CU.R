library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

#Plot kurva Stress-Strain Triaxial CU
ggplot(RUN_R) +
  geom_line(aes(x = Strain, y = Stress, color = Conf, group=Sampel))+
  labs(title = 'Data Triaxial CU',
       x = "Strain (%)", y = "Stress (kPa)")

#Cleaning data
df<-slice(RUN_R,-c(2030:2031)) #menghapus data di line 2030 dan 2031

#Cek database terbaru
ggplot(df) +
  geom_line(aes(x = Strain, y = Stress, color = Conf, 
                group=Sampel))+
  labs(title = 'Data Triaxial CU',
       x = "Strain (%)", y = "Stress (kPa)")

#Distribusi dan persebaran data
ggplot(df %>% melt(id.vars = NULL), 
       aes(x = value)) +
  geom_histogram(fill = "dodgerblue", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Histograms of Data Distribution")

#Split data train dan test
library(caTools)
jumlah_baris_unik <- length(unique(df$Sampel))
jumlah_training <- round(0.7 * jumlah_baris_unik)
set.seed(123)
indeks_acak <- sample(1:jumlah_baris_unik)
data_training <- df[df$Sampel %in% unique(df$Sampel)
                    [indeks_acak[1:jumlah_training]], ]
data_testing <- df[df$Sampel %in% unique(df$Sampel)
                   [indeks_acak[(jumlah_training + 1):jumlah_baris_unik]], ]

# Membuat plot data training
plot_train <- ggplot(data_training, aes(x = Strain, y = Stress, 
                                        color = "Data Train",group=Sampel)) +
  geom_line(size = 1) +labs(x = "Strain(%)", y = "Stress(kPa)", 
                            title = "Plot Data Train dan Test") +
  scale_color_manual(values = c("Data Train" = "dodgerblue"), name = "Data Type")

# Menambahkan plot data testing ke dalam plot data training
plot_combined <- plot_train +
  geom_line(data = data_testing, aes(x = Strain, y = Stress, 
                                     color = "Data Test",group=Sampel), size = 1) +
  scale_color_manual(values = c("Data Train" = "dodgerblue", "Data Test" = "deeppink")) 

# Menampilkan plot yang telah digabungkan
print(plot_combined)

#######MODELLING#########
#Training Ordinary Least Square Model
model_ols<-lm(Stress~Strain+Conf+e+Gs+PL+LL+Cc+Sr+ϒm,
              data_training)
summary(model_ols)

#Testing & Prediction Ordinary Least Square Model
prediksi_ols<- predict(model_ols, newdata = data_testing)
df_prediksi_ols <- data_testing %>% mutate(Pred = prediksi_ols)

####Analisis Sensitivitas Model OLS####

# Hitung selisih antara nilai prediksi dan nilai aktual
error <- prediksi_ols - data_testing$Stress

# Buat data frame baru untuk plot
error_df <- data.frame(Prediction = prediksi_ols, Error = error)

# Buat plot error
p <- ggplot(error_df, aes(x = Prediction, y = Error)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Garis referensi pada nilai nol
  labs(x = "Prediction", y = "Error", title = "Prediction Error Plot") +
  theme_bw()
print(p)

#Cek nilai RMSE OLS Model
rmse <- sqrt(mean((data_testing$Stress - prediksi_ols)^2))

#Cek nilai MAE OLS Model

#Hitung selisih absolut antara nilai aktual dan nilai prediksi
abs_diff <- abs(data_testing$Stress - prediksi_ols)

#Hitung nilai MAE
mae <- mean(abs_diff)
print(paste("MAE:", round(mae, 2)))

#Plot 1:1
ggplot(df_prediksi_ols) +
  geom_point(aes(x = Stress, y = Pred, 
                 color = as.character(Conf))) +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "black") +
  xlim(0, 200) + ylim(0, 200) + theme_bw() + coord_equal()+
  geom_text(x = 200, y = 160, 
            label = paste("RMSE:", round(rmse, 2), "\n", "MAE:", round(mae, 2)), 
            hjust = 1, vjust = 0, color = "black", size = 4)

#Plot strain vs predicted OLS model
ggplot(df_prediksi_ols, aes(x = Strain, y = Stress,group=Sampel)) +
  geom_point() +  # Tambahkan titik data
  geom_line(aes(y = Pred), color = "blue") +  # Tambahkan garis regresi
  labs(title = "Plot Hasil Regresi Linear", x = "Strain (%)", y = "Stress (kPa)") +
  theme_minimal()+
  annotate("text", x = 11, y = 0, label = paste("RMSE:", round(rmse, 2)), 
           hjust = 1, vjust = 1, color = "black") +
  annotate("text", x = 11, y = 11, label = paste("MAE:", round(mae, 2)), 
           hjust = 1, vjust = 0.9, color = "black")

#Training Random Forest model
library(ranger)
library(cowplot)
model_rf <- ranger(Stress~Strain+e+Gs+PL+LL+Cc+Sr+ϒm+Conf,
                   data_training,
                   num.trees=500,
                   splitrule = 'extratrees', importance = 'impurity')

#Prediction & testing RF model
pred_rf<-predict(model_rf, data=data_testing)$predictions
df_pred_rf<-data_testing %>% mutate(Predrf=pred_rf)

####Analisis Sensitivitas Model RF####

# Hitung selisih antara nilai prediksi dan nilai aktual
errorRF <- pred_rf - data_testing$Stress

# Buat data frame baru untuk plot
errorRF_df <- data.frame(PredictionRF = pred_rf, ErrorRF = errorRF)

# Buat plot error
p <- ggplot(errorRF_df, aes(x = PredictionRF, y = ErrorRF)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Garis referensi pada nilai nol
  labs(x = "Prediction", y = "Error", title = "Prediction Error Plot") +
  theme_bw()
print(p)

#Cek nilai RMSE RF Model
rmseRF <- sqrt(mean((data_testing$Stress - pred_rf)^2))

#Cek nilai MAE RF Model

#Hitung selisih absolut antara nilai aktual dan nilai prediksi
abs_diffRF <- abs(data_testing$Stress - pred_rf)

#Hitung nilai MAE
maeRF <- mean(abs_diffRF)
print(paste("MAE:", round(maeRF, 2)))

#Plot 1:1
ggplot(df_pred_rf) +
  geom_point(aes(x = Stress, y = Predrf, 
                 color = as.character(Conf))) +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "black") +
  xlim(0, 200) + ylim(0, 200) +theme_bw() +coord_equal()+
  geom_text(x = 150, y = 160, 
            label = paste("RMSE:", round(rmseRF, 2), "\n", "MAE:", round(maeRF, 2)), 
            hjust = 1, vjust = 0, color = "black", size = 4)

#Plot strain vs predicted RF model
ggplot(df_pred_rf, aes(x = Strain, y = Stress,group=Sampel)) +
  geom_point() +  # Tambahkan titik data
  geom_line(aes(y = Predrf), color = "blue") +  # Tambahkan garis random forest
  labs(title = "Plot Hasil Random Forest", 
       x = "Strain (%)", y = "Stress (kPa)") +theme_minimal()+
  annotate("text", x = 1, y = 140, label = paste("RMSE:", round(rmseRF, 2)), 
           hjust = 1, vjust = 1, color = "black") +
  annotate("text", x = 1, y = 135, label = paste("MAE:", round(maeRF, 2)), 
           hjust = 1, vjust = 0.9, color = "black")

####Cek variabel importance Random Forest####
importance_gini <- ranger::importance(model_rf) #Metode Gini Impurity
print(importance_gini)

# Membuat data frame untuk variabel importance Gini impurity
importance_gini_df <- data.frame(
  Feature = names(importance_gini),
  Importance_Gini = importance_gini
)

# Membuat bar plot
ggplot(importance_gini_df, 
       aes(x = reorder(Feature, Importance_Gini), y = Importance_Gini)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Fitur", y = "Importance Gini Impurity", 
       title = "Variabel Importance Gini Impurity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Model RF untuk feature importance permutation
rf_permutation <- ranger(Stress~Strain+e+Gs+PL+LL+Cc+Sr+ϒm+Conf,
                   data_training,
                   num.trees=500,
                   splitrule = 'extratrees', importance = 'permutation')

importance_permutation <- ranger::importance(rf_permutation) #Metode Gini Impurity
print(importance_permutation)

# Membuat data frame untuk variabel importance Permutation
importance_permu_df <- data.frame(
  Feature = names(importance_permutation),
  Importance_Permu = importance_permutation
)

# Membuat bar plot
ggplot(importance_permu_df, 
       aes(x = reorder(Feature, Importance_Permu), y = Importance_Permu)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(x = "Fitur", y = "Importance Permutation", 
       title = "Variabel Importance Permutation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#######Incremental model###########
#Membuat data baru (subset data df)
ggplot(IncrementR) +
  geom_line(aes(x = Strain, y = Stress, color = Conf, group=Sampel))+
  labs(title = 'Data Triaxial CU',
       x = "Strain (%)", y = "Stress (kPa)")

#Cleaning data
dfInc<-slice(IncrementR,-c(2030:2031)) #menghapus data di line 2030 dan 2031

#Splitting data training dan testing
baris_unik_inc <- length(unique(dfInc$Sampel))
training_inc <- round(0.7 * baris_unik_inc)
set.seed(123)
indeks_Inc <- sample(1:baris_unik_inc)
data_trainingInc <- dfInc[dfInc$Sampel %in% unique(dfInc$Sampel)
                    [indeks_Inc[1:training_inc]], ]
data_testingInc <- dfInc[dfInc$Sampel %in% unique(dfInc$Sampel)
                   [indeks_Inc[(training_inc + 1):baris_unik_inc]], ]

#Training Ordinary Least Square Model
ols_Inc<-lm(Stress~Strain+Conf+e+Gs+PL+LL+Cc+Sr+ϒm+Cc+Str+Ss+DelSt,
              data_trainingInc)
summary(ols_Inc)

#Testing & Prediction Ordinary Least Square Model
pred_ols<- predict(ols_Inc, newdata = data_testingInc)
df_pred_ols <- data_testingInc %>% mutate(PredInc = pred_ols)

#Hitung selisih antara nilai prediksi dan nilai aktual
errorOLS <- pred_ols - data_testingInc$Stress

#Buat data frame baru untuk plot
errorOLS_df <- data.frame(PredictionOLS = pred_ols, ErrorOLS = errorOLS)

#Buat plot error
p <- ggplot(errorOLS_df, aes(x = PredictionOLS, y = ErrorOLS)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Garis referensi pada nilai nol
  labs(x = "Prediction", y = "Error", title = "Prediction Error Plot") +
  theme_bw()
print(p)

#Cek nilai RMSE OLS ModelInc
rmseOLS <- sqrt(mean((data_testingInc$Stress - pred_ols)^2))

#Cek nilai MAE OLS ModelInc

#Hitung selisih absolut antara nilai aktual dan nilai prediksi
abs_diffOLS <- abs(data_testingInc$Stress - pred_ols)

#Hitung nilai MAE
maeOLS <- mean(abs_diffOLS)
print(paste("MAE:", round(maeOLS, 2)))

#Plot 1:1
ggplot(df_pred_ols) +
  geom_point(aes(x = Stress, y = PredInc, 
                 color = as.character(Conf))) +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "black") +
  xlim(0, 255) + ylim(0, 255) +theme_bw() +coord_equal()+
  geom_text(x = 150, y = 160, 
            label = paste("RMSE:", round(rmseOLS, 2), "\n", "MAE:", round(maeOLS, 2)), 
            hjust = 1, vjust = 0, color = "black", size = 4)+
  labs(title = "Plot Stress vs Predicted OLS", 
       x = "Stress", y = "Predicted Stress") 

#Plot strain vs predicted OLS model
ggplot(df_pred_ols, aes(x = Strain, y = Stress,group=Sampel)) +
  geom_point() +  # Tambahkan titik data
  geom_line(aes(y = PredInc), color = "blue") +  # Tambahkan garis OLS
  labs(title = "Plot Hasil OLS Increment", 
       x = "Strain (%)", y = "Stress (kPa)") +theme_minimal()+
  annotate("text", x = 1, y = 140, label = paste("RMSE:", round(rmseOLS, 2)), 
           hjust = 1, vjust = 1, color = "black") +
  annotate("text", x = 1, y = 135, label = paste("MAE:", round(maeOLS, 2)), 
           hjust = 1, vjust = 0.9, color = "black")

#Training Random Forest model
library(ranger)
library(cowplot)
rf_inc <- ranger(Stress~Strain+Conf+e+Gs+PL+LL+Cc+Sr+ϒm+Cc+Str+Ss+DelSt,
                 data_trainingInc,
                 num.trees=500,
                 splitrule = 'extratrees', importance = 'impurity')

#Prediction & testing RF model
prediksi_rf<-predict(rf_inc, data=data_testingInc)$predictions
df_prediksi_rf<-data_testingInc %>% mutate(Prediksirf=prediksi_rf)

#Hitung selisih antara nilai prediksi dan nilai aktual
errorRFInc <- prediksi_rf - data_testingInc$Stress

#Buat data frame baru untuk plot
errorRFInc_df <- data.frame(PredictionRFInc = prediksi_rf, ErrorRFInc = errorRFInc)

#Buat plot error
p <- ggplot(errorRFInc_df, aes(x = PredictionRFInc, y = ErrorRFInc)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Garis referensi pada nilai nol
  labs(x = "Prediction", y = "Error", title = "Prediction Error Plot") +
  theme_bw()
print(p)

#Cek nilai RMSE RF ModelInc
rmseRFInc <- sqrt(mean((data_testingInc$Stress - prediksi_rf)^2))

#Cek nilai MAE RF ModelInc

#Hitung selisih absolut antara nilai aktual dan nilai prediksi
abs_diffRFInc <- abs(data_testingInc$Stress - prediksi_rf)

#Hitung nilai MAE
maeRFInc <- mean(abs_diffRFInc)
print(paste("MAE:", round(maeRFInc, 2)))

#Plot 1:1
ggplot(df_prediksi_rf) +
  geom_point(aes(x = Stress, y = Prediksirf, 
                 color = as.character(Conf))) +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "black") +
  xlim(0, 255) + ylim(0, 255) + theme_bw() + coord_equal()+
  geom_text(x = 150, y = 160, 
            label = paste("RMSE:", round(rmseRFInc, 2), "\n", "MAE:", round(maeRFInc, 2)), 
            hjust = 1, vjust = 0, color = "black", size = 4)+
  labs(title = "Plot Stress vs Predicted Random Forest", 
       x = "Stress", y = "Predicted Stress") 

#Plot strain vs predicted RF model
ggplot(df_prediksi_rf, aes(x = Strain, y = Stress,group=Sampel)) +
  geom_point() +  # Tambahkan titik data
  geom_line(aes(y = Prediksirf), color = "blue") +  # Tambahkan garis random forest
  labs(title = "Plot Hasil Random Forest", 
       x = "Strain (%)", y = "Stress (kPa)") +theme_minimal()+
  annotate("text", x = 1, y = 140, label = paste("RMSE:", round(rmseRFInc, 2)), 
           hjust = 1, vjust = 1, color = "black") +
  annotate("text", x = 1, y = 135, label = paste("MAE:", round(maeRFInc, 2)), 
           hjust = 1, vjust = 0.9, color = "black")
