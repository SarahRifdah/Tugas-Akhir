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
set.seed(123)
proporsi_training<-0.7
jumlah_training<-floor(proporsi_training*nrow(df))
indeks_training<-sample(seq_len(nrow(df)),size=jumlah_training)
data_training<-df[indeks_training,]
data_testing<-df[-indeks_training,]

#######MODELLING#########
#Training Ordinary Least Square Model
model_ols<-lm(Stress~Strain+Conf+e+Gs+PL+LL+Cc+Sr+ϒm,
              data_training)
summary(model_ols)

#Testing & Prediction Ordinary Least Square Model
prediksi_ols<- predict(model_ols, newdata = data_testing)
df_prediksi_ols <- data_testing %>% mutate(Pred = prediksi_ols)

#Cek nilai MSE OLS Model
selisih <- data_testing$Stress - prediksi_ols
kuadrat_selisih <- selisih^2
mse<-mean(kuadrat_selisih)^2
print(mse)

#Plot 1:1
ggplot(df_prediksi_ols) +
  geom_point(aes(x = Stress, y = Pred, 
                 color = as.character(Conf))) +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "black") +
  xlim(0, 150) + ylim(0, 150) +
  theme_bw() +
  coord_equal()

#Plot strain vs predicted OLS model
ggplot(df_prediksi_ols, aes(x = Strain, y = Stress,group=Sampel)) +
  geom_point() +  # Tambahkan titik data
  geom_line(aes(y = Pred), color = "blue") +  # Tambahkan garis regresi
  labs(title = "Plot Hasil Regresi Linear", 
       x = "Strain (%)", y = "Stress (kPa)") +
  theme_minimal()

#Training Random Forest model
library(ranger)
library(cowplot)
library(randomForest)
model_rf <- ranger(Stress~Strain+e+Gs+PL+LL+Cc+Sr+ϒm+Conf,
                   data_training,
                   num.trees=500,
                   splitrule = 'extratrees')

#Cek nilai MSE Random Forest model
model_rf

#Prediction & testing RF model
pred_rf<-predict(model_rf, data=data_testing)$predictions
df_pred_rf<-data_testing %>% mutate(Predrf=pred_rf)

#Plot 1:1
ggplot(df_pred_rf) +
  geom_point(aes(x = Stress, y = Stress, 
                 color = as.character(Conf))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  xlim(0, 100) + ylim(0, 100) +
  theme_bw() +
  coord_equal()

#Plot strain vs predicted RF model
ggplot(df_pred_rf, aes(x = Strain, y = Stress,group=Sampel)) +
  geom_point() +  # Tambahkan titik data
  geom_line(aes(y = Predrf), color = "blue") +  # Tambahkan garis random forest
  labs(title = "Plot Hasil Random Forest", 
       x = "Strain (%)", y = "Stress (kPa)") +
  theme_minimal()
