# Nama : Ni Luh  Putu Indah Juliantari 
# NIM : 2415091041
#Kls : SI 1 Dps

# Simulasi Sederhana: Regresi Linear Sederhana di R

# 1. Membuat Data Simulasi
# Variabel X (independen) dan Y (dependen)
set.seed(987)  # Untuk hasil yang konsisten tapi unik
n <- 120  # Jumlah sampel yang sedikit lebih besar
X <- rnorm(n, mean = 5, sd = 2)  # Variabel independen (acak normal dengan mean 5 dan sd 2)
Y <- 3 * X - 1.5 + rnorm(n, mean = 0, sd = 3)  # Variabel dependen dengan noise acak

# Membentuk data frame
data <- data.frame(X = X, Y = Y)

# 2. Uji Asumsi
# a. Linearitas
plot(data$X, data$Y, main = "Scatter Plot X vs Y dengan Garis Regresi", 
     xlab = "X (Independen)", ylab = "Y (Dependen)", pch = 19, col = "darkgreen")
abline(lm(Y ~ X, data = data), col = "blue", lwd = 2)

# b. Normalitas Residual
model <- lm(Y ~ X, data = data)
residuals <- resid(model)
hist(residuals, main = "Distribusi Residual", 
     xlab = "Residual", col = "orange", breaks = 12)
shapiro.test(residuals)  # Shapiro-Wilk Test

# c. Homoskedastisitas
plot(fitted(model), residuals, main = "Plot Fitted Values vs Residuals", 
     xlab = "Fitted Values", ylab = "Residuals", pch = 19, col = "purple")
abline(h = 0, col = "red", lwd = 2)

# d. Uji Independensi Residual - Durbin-Watson Test
install.packages("lmtest")
library(lmtest)
dw_test <- dwtest(model)
cat("Uji Durbin-Watson untuk menguji independensi residual: \n")
cat("p-value dari uji Durbin-Watson:", dw_test$p.value, "\n")
if(dw_test$p.value < 0.05) {
  cat("Terdapat indikasi autokorelasi dalam residual (p-value < 0.05).\n")
} else {
  cat("Tidak terdapat indikasi autokorelasi dalam residual (p-value >= 0.05).\n")
}

# e. Uji Normalitas untuk Variabel Independen X
shapiro_test_X <- shapiro.test(X)
cat("Uji Shapiro-Wilk untuk normalitas X: \n")
cat("p-value dari uji Shapiro-Wilk:", shapiro_test_X$p.value, "\n")
if(shapiro_test_X$p.value < 0.05) {
  cat("Variabel X tidak terdistribusi normal (p-value < 0.05).\n")
} else {
  cat("Variabel X terdistribusi normal (p-value >= 0.05).\n")
}

# 3. Analisis: Regresi Linear Sederhana
summary(model)  # Hasil analisis regresi

# 4. Visualisasi Hasil
library(ggplot2)
plot_regresi <- ggplot(data, aes(x = X, y = Y)) +
  geom_point(color = "darkred", size = 2) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(title = "Model Regresi Linear: Hubungan X dan Y", 
       x = "Variabel Independen (X)", 
       y = "Variabel Dependen (Y)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(plot_regresi)

# 5. Interpretasi
cat("Interpretasi: \n")
cat("1. Berdasarkan koefisien regresi, setiap kenaikan 1 unit X rata-rata akan \n")
cat("   meningkatkan Y sebesar", round(coef(model)[2], 2), "unit.\n")
cat("2. Uji F menunjukkan apakah model ini signifikan secara statistik.\n")
cat("3. Hasil uji asumsi memberikan kesimpulan sebagai berikut:\n")
cat("   - Linearitas: Scatter plot menunjukkan hubungan linear yang konsisten.\n")
cat("   - Normalitas: Histogram residual menunjukkan pola yang mendekati normal \n")
cat("     dengan p-value dari Shapiro-Wilk test memberikan hasil yang relevan.\n")
cat("   - Homoskedastisitas: Plot Fitted vs Residuals menunjukkan distribusi residual \n")
cat("     yang tidak berpola jelas, mendukung asumsi homoskedastisitas.\n")
cat("   - Independensi Residual: Uji Durbin-Watson menunjukkan bahwa\n")
cat("     p-value dari uji Durbin-Watson", dw_test$p.value, "\n")
cat("     yang menunjukkan apakah terdapat indikasi autokorelasi.\n")
cat("   - Normalitas X: Uji Shapiro-Wilk menunjukkan bahwa\n")
cat("     p-value dari uji Shapiro-Wilk", shapiro_test_X$p.value, "\n")
cat("     yang menunjukkan apakah variabel independen X terdistribusi normal.\n")

