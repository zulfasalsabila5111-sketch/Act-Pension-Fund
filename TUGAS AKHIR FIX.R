#Membangkitkan packages
library(openxlsx)
library(MortalityLaws)
library(Metrics)

#Input data
Data_male = read.xlsx("C:/Users/ASUS/Downloads/SEMESTER 8/TUGAS AKHIR/OUTPUT TA/TUGAS AKHIR FIXX.xlsx", sheet = "Data UN_Male")
Data_female = read.xlsx("C:/Users/ASUS/Downloads/SEMESTER 8/TUGAS AKHIR/OUTPUT TA/TUGAS AKHIR FIXX.xlsx", sheet = "Data UN_Female")

#Gamma Gompertz
##Laki-laki usia 30 tahun
###Estimasi parameter
l_30_ggo = MortalityLaw(x=Data_male$Usia, mx=Data_male$`Miu(x)`, law="ggompertz", opt.method="LF2", fit.this.x = Data_male$Usia[Data_male$Usia >= 30])
coef(l_30_ggo)
summary(l_30_ggo)
A = l_30_ggo$coefficients[[1]]
B = l_30_ggo$coefficients[[2]]
C = l_30_ggo$coefficients[[3]]

###Penghitungan miu x dan tpx dengan asumsi ggo
mux.l30g = function(x){
  (A*exp(B*x))/(1+(A*C*(exp(B*x)-1)/B))
}
tpx.l30g = function(x, t) {
  ((1+(C*A*(exp(B*(x+t))-1)/B))/(1+(C*A*(exp(B*(x))-1)/B)))^(-1/C)
}

###Menghitung tpx untuk rentang usia 0 hingga 100
hasil_tpx.l30g = tpx.l30g(x = 0:100, t = 1)
hasil_mux.l30g = mux.l30g(x = 0:100)

###Menggabungkan hasil tpx dan mux ke dalam satu data frame
Tabel_laki_30g = data.frame(
  x = 0:100,
  tpx.l30g = hasil_tpx.l30g,
  tqx.130g = 1 - hasil_tpx.l30g,
  mux.l30g = hasil_mux.l30g
)
Tabel_laki_30g

###Ukuran Akurasi
#RSE
rse(Data_male$`Miu(x)`, hasil_mux.l30g)
rse(Data_male$L, hasil_tpx.l30g)
#SMAPE
smape(Data_male$`Miu(x)`, hasil_mux.l30g)
smape(Data_male$L, hasil_tpx.l30g)
#RMSE
rmse(Data_male$`Miu(x)`, hasil_mux.l30g)
rmse(Data_male$L, hasil_tpx.l30g)
#MAPE
mape(Data_male$`Miu(x)`, hasil_mux.l30g)
mape(Data_male$L, hasil_tpx.l30g)
###Menghitung RSE, SMAPE, dan RMSE
actual_mux = Data_male$`Miu(x)`
actual_tpx = Data_male$L
####hitung residuals
residual_mux = actual_mux - hasil_mux.l30g
residual_tpx = actual_tpx - hasil_tpx.l30g
####RSE
rse_mux_l30g = sqrt(sum(residual_mux^2) / (length(actual_mux) - 2))
cat("RSE :", rse_mux_l30g, "\n")
rse_tpx_l30g = sqrt(sum(residual_tpx^2) / (length(actual_tpx) - 2))
cat("RSE :", rse_tpx_l30g, "\n")
####SMAPE
smape_mux_l30g = (mean((2*(abs(hasil_mux.l30g - actual_mux)))/((abs(actual_mux))+(abs(hasil_mux.l30g)))))*100
cat("SMAPE :", smape_mux_l30g, "\n")
smape_tpx_l30g = (mean((2*(abs(hasil_tpx.l30g - actual_tpx)))/((abs(actual_tpx))+(abs(hasil_tpx.l30g)))))*100
cat("SMAPE :", smape_tpx_l30g, "\n")
####RMSE
rmse_mux_l30g = sqrt(mean(residual_mux^2))
cat("RMSE :", rmse_mux_l30g, "\n")
rmse_tpx_l30g = sqrt(mean(residual_tpx^2))
cat("RMSE :", rmse_tpx_l30g, "\n")
#### MAPE
mape_mux_l30g = (mean(abs((residual_mux) / actual_mux))) * 100
cat("MAPE :", mape_mux_l30g, "\n")
mape_tpx_l30g = (mean(abs((residual_tpx) / actual_tpx))) * 100
cat("MAPE :", mape_tpx_l30g, "\n")
valid_indices = actual_tpx != 0
mape_tpx_l30g = (mean(abs((residual_tpx[valid_indices]) / actual_tpx[valid_indices]))) * 100
cat("MAPE :", mape_tpx_l30g, "\n")


##Perempuan usia 30 tahun
###Estimasi parameter
p_30_ggo = MortalityLaw(x=Data_female$Usia, mx=Data_female$`Miu(x)`, law="ggompertz", opt.method="LF2", fit.this.x = Data_female$Usia[Data_female$Usia >= 30])
coef(p_30_ggo)
summary(p_30_ggo)
A = p_30_ggo$coefficients[[1]]
B = p_30_ggo$coefficients[[2]]
C = p_30_ggo$coefficients[[3]]

###Penghitungan miu x dan tpx dengan asumsi ggo
mux.p30g = function(x){
  (A*exp(B*x))/(1+(A*C*(exp(B*x)-1)/B))
}
tpx.p30g = function(x, t) {
  ((1+(C*A*(exp(B*(x+t))-1)/B))/(1+(C*A*(exp(B*(x))-1)/B)))^(-1/C)
}

###Menghitung tpx untuk rentang usia 0 hingga 100
hasil_tpx.p30g = tpx.p30g(x = 0:100, t = 1)
hasil_mux.p30g = mux.p30g(x = 0:100)

###Menggabungkan hasil tpx dan mux ke dalam satu data frame
Tabel_perempuan_30g = data.frame(
  x = 0:100,
  tpx.p30g = hasil_tpx.p30g,
  tqx.p30g = 1 - hasil_tpx.p30g,
  mux.p30g = hasil_mux.p30g
)
Tabel_perempuan_30g

###Ukuran Akurasi
#RSE
rse(Data_female$`Miu(x)`, hasil_mux.p30g)
rse(Data_female$P, hasil_tpx.p30g)
#SMAPE
smape(Data_female$`Miu(x)`, hasil_mux.p30g)
smape(Data_female$P, hasil_tpx.p30g)
#RMSE
rmse(Data_female$`Miu(x)`, hasil_mux.p30g)
rmse(Data_female$P, hasil_tpx.p30g)
#MAPE
mape(Data_female$`Miu(x)`, hasil_mux.p30g)
mape(Data_female$P, hasil_tpx.p30g)
###Menghitung RSE, SMAPE, dan RMSE
actual_mux = Data_female$`Miu(x)`
actual_tpx = Data_female$P
####hitung residuals
residual_mux = actual_mux - hasil_mux.p30g
residual_tpx = actual_tpx - hasil_tpx.p30g
####RSE
rse_mux_p30g = sqrt(sum(residual_mux^2) / (length(actual_mux) - 2))
cat("RSE :", rse_mux_p30g, "\n")
rse_tpx_p30g = sqrt(sum(residual_tpx^2) / (length(actual_tpx) - 2))
cat("RSE :", rse_tpx_p30g, "\n")
####SMAPE
smape_mux_p30g = (mean((2*(abs(hasil_mux.p30g - actual_mux)))/((abs(actual_mux))+(abs(hasil_mux.p30g)))))*100
cat("SMAPE :", smape_mux_p30g, "\n")
smape_tpx_p30g = (mean((2*(abs(hasil_tpx.p30g - actual_tpx)))/((abs(actual_tpx))+(abs(hasil_tpx.p30g)))))*100
cat("SMAPE :", smape_tpx_p30g, "\n")
####RMSE
rmse_mux_p30g = sqrt(mean(residual_mux^2))
cat("RMSE :", rmse_mux_p30g, "\n")
rmse_tpx_p30g = sqrt(mean(residual_tpx^2))
cat("RMSE :", rmse_tpx_p30g, "\n")
#### MAPE
mape_mux_p30g = (mean(abs((residual_mux) / actual_mux))) * 100
cat("MAPE :", mape_mux_p30g, "\n")
mape_tpx_p30g = (mean(abs((residual_tpx) / actual_tpx))) * 100
cat("MAPE :", mape_tpx_p30g, "\n")
valid_indices = actual_tpx != 0
mape_tpx_p30g = (mean(abs((residual_tpx[valid_indices]) / actual_tpx[valid_indices]))) * 100
cat("MAPE :", mape_tpx_p30g, "\n")


##Laki-laki usia 40 tahun
###Estimasi parameter
l_40_ggo = MortalityLaw(x=Data_male$Usia, mx=Data_male$`Miu(x)`, law="ggompertz", opt.method="LF2", fit.this.x = Data_male$Usia[Data_male$Usia >= 40])
coef(l_40_ggo)
summary(l_40_ggo)
A = l_40_ggo$coefficients[[1]]
B = l_40_ggo$coefficients[[2]]
C = l_40_ggo$coefficients[[3]]

###Penghitungan miu x dan tpx dengan asumsi ggo
mux.l40g = function(x){
  (A*exp(B*x))/(1+(A*C*(exp(B*x)-1)/B))
}
tpx.l40g = function(x, t) {
  ((1+(C*A*(exp(B*(x+t))-1)/B))/(1+(C*A*(exp(B*(x))-1)/B)))^(-1/C)
}

###Menghitung tpx untuk rentang usia 0 hingga 100
hasil_tpx.l40g = tpx.l40g(x = 0:100, t = 1)
hasil_mux.l40g = mux.l40g(x = 0:100)

###Menggabungkan hasil tpx dan mux ke dalam satu data frame
Tabel_laki_40g = data.frame(
  x = 0:100,
  tpx.l40g = hasil_tpx.l40g,
  tqx.l40g = 1 - hasil_tpx.l40g,
  mux.l40g = hasil_mux.l40g
)
Tabel_laki_40g

###Ukuran Akurasi
#RSE
rse(Data_male$`Miu(x)`, hasil_mux.l40g)
rse(Data_male$L, hasil_tpx.l40g)
#SMAPE
smape(Data_male$`Miu(x)`, hasil_mux.l40g)
smape(Data_male$L, hasil_tpx.l40g)
#RMSE
rmse(Data_male$`Miu(x)`, hasil_mux.l40g)
rmse(Data_male$L, hasil_tpx.l40g)
#MAPE
mape(Data_male$`Miu(x)`, hasil_mux.l40g)
mape(Data_male$L, hasil_tpx.l40g)
###Menghitung RSE, SMAPE, dan RMSE
actual_mux = Data_male$`Miu(x)`
actual_tpx = Data_male$L
####hitung residuals
residual_mux = actual_mux - hasil_mux.l40g
residual_tpx = actual_tpx - hasil_tpx.l40g
####RSE
rse_mux_l40g = sqrt(sum(residual_mux^2) / (length(actual_mux) - 2))
cat("RSE :", rse_mux_l40g, "\n")
rse_tpx_l40g = sqrt(sum(residual_tpx^2) / (length(actual_tpx) - 2))
cat("RSE :", rse_tpx_l40g, "\n")
####SMAPE
smape_mux_l40g = (mean((2*(abs(hasil_mux.l40g - actual_mux)))/((abs(actual_mux))+(abs(hasil_mux.l40g)))))*100
cat("SMAPE :", smape_mux_l40g, "\n")
smape_tpx_l40g = (mean((2*(abs(hasil_tpx.l40g - actual_tpx)))/((abs(actual_tpx))+(abs(hasil_tpx.l40g)))))*100
cat("SMAPE :", smape_tpx_l40g, "\n")
####RMSE
rmse_mux_l40g = sqrt(mean(residual_mux^2))
cat("RMSE :", rmse_mux_l40g, "\n")
rmse_tpx_l40g = sqrt(mean(residual_tpx^2))
cat("RMSE :", rmse_tpx_l40g, "\n")
#### MAPE
mape_mux_l40g = (mean(abs((residual_mux) / actual_mux))) * 100
cat("MAPE :", mape_mux_l40g, "\n")
mape_tpx_l40g = (mean(abs((residual_tpx) / actual_tpx))) * 100
cat("MAPE :", mape_tpx_l40g, "\n")
valid_indices = actual_tpx != 0
mape_tpx_l40g = (mean(abs((residual_tpx[valid_indices]) / actual_tpx[valid_indices]))) * 100
cat("MAPE :", mape_tpx_l40g, "\n")


##Perempuan usia 40 tahun
###Estimasi parameter
p_40_ggo = MortalityLaw(x=Data_female$Usia, mx=Data_female$`Miu(x)`, law="ggompertz", opt.method="LF2", fit.this.x = Data_female$Usia[Data_female$Usia >= 40])
coef(p_40_ggo)
summary(p_40_ggo)
A = p_40_ggo$coefficients[[1]]
B = p_40_ggo$coefficients[[2]]
C = p_40_ggo$coefficients[[3]]

###Penghitungan miu x dan tpx dengan asumsi ggo
mux.p40g = function(x){
  (A*exp(B*x))/(1+(A*C*(exp(B*x)-1)/B))
}
tpx.p40g = function(x, t) {
  ((1+(C*A*(exp(B*(x+t))-1)/B))/(1+(C*A*(exp(B*(x))-1)/B)))^(-1/C)
}

###Menghitung tpx untuk rentang usia 0 hingga 100
hasil_tpx.p40g = tpx.p40g(x = 0:100, t = 1)
hasil_mux.p40g = mux.p40g(x = 0:100)

###Menggabungkan hasil tpx dan mux ke dalam satu data frame
Tabel_perempuan_40g = data.frame(
  x = 0:100,
  tpx.p40g = hasil_tpx.p40g,
  tqx.p40g = 1 - hasil_tpx.p40g,
  mux.p40g = hasil_mux.p40g
)
Tabel_perempuan_40g

###Ukuran Akurasi
#RSE
rse(Data_female$`Miu(x)`, hasil_mux.p40g)
rse(Data_female$P, hasil_tpx.p40g)
#SMAPE
smape(Data_female$`Miu(x)`, hasil_mux.p40g)
smape(Data_female$P, hasil_tpx.p40g)
#RMSE
rmse(Data_female$`Miu(x)`, hasil_mux.p40g)
rmse(Data_female$P, hasil_tpx.p40g)
#MAPE
mape(Data_female$`Miu(x)`, hasil_mux.p40g)
mape(Data_female$P, hasil_tpx.p40g)
###Menghitung RSE, SMAPE, dan RMSE
actual_mux = Data_female$`Miu(x)`
actual_tpx = Data_female$P
####hitung residuals
residual_mux = actual_mux - hasil_mux.p40g
residual_tpx = actual_tpx - hasil_tpx.p40g
####RSE
rse_mux_p40g = sqrt(sum(residual_mux^2) / (length(actual_mux) - 2))
cat("RSE :", rse_mux_p40g, "\n")
rse_tpx_p40g = sqrt(sum(residual_tpx^2) / (length(actual_tpx) - 2))
cat("RSE :", rse_tpx_p40g, "\n")
####SMAPE
smape_mux_p40g = (mean((2*(abs(hasil_mux.p40g - actual_mux)))/((abs(actual_mux))+(abs(hasil_mux.p40g)))))*100
cat("SMAPE :", smape_mux_p40g, "\n")
smape_tpx_p40g = (mean((2*(abs(hasil_tpx.p40g - actual_tpx)))/((abs(actual_tpx))+(abs(hasil_tpx.p40g)))))*100
cat("SMAPE :", smape_tpx_p40g, "\n")
####RMSE
rmse_mux_p40g = sqrt(mean(residual_mux^2))
cat("RMSE :", rmse_mux_p40g, "\n")
rmse_tpx_p40g = sqrt(mean(residual_tpx^2))
cat("RMSE :", rmse_tpx_p40g, "\n")
#### MAPE
mape_mux_p40g = (mean(abs((residual_mux) / actual_mux))) * 100
cat("MAPE :", mape_mux_p40g, "\n")
mape_tpx_p40g = (mean(abs((residual_tpx) / actual_tpx))) * 100
cat("MAPE :", mape_tpx_p40g, "\n")
valid_indices = actual_tpx != 0
mape_tpx_p40g = (mean(abs((residual_tpx[valid_indices]) / actual_tpx[valid_indices]))) * 100
cat("MAPE :", mape_tpx_p40g, "\n")


#Makeham
##Laki-laki usia 30 tahun
###Estimasi parameter
l_30_mk = MortalityLaw(x=Data_male$Usia, mx=Data_male$`Miu(x)`, law="makeham", opt.method="LF2", fit.this.x = Data_male$Usia[Data_male$Usia >= 30])
coef(l_30_mk)
summary(l_30_mk)
A = l_30_mk$coefficients[[1]]
B = l_30_mk$coefficients[[2]]
C = l_30_mk$coefficients[[3]]

###Penghitungan miu x dan tpx dengan asumsi mk
mux.l30m = function(x){
  (A*exp(B*x))+C
}
tpx.l30m = function(x, t) {
  exp((-C*t)-((A*exp(B*(x))/B)*((exp(B*t))-1)))
}

###Menghitung tpx untuk rentang usia 0 hingga 100
hasil_tpx.l30m = tpx.l30m(x = 0:100, t = 1)
hasil_mux.l30m = mux.l30m(x = 0:100)

###Menggabungkan hasil tpx dan mux ke dalam satu data frame
Tabel_laki_30m = data.frame(
  x = 0:100,
  tpx.l30m = hasil_tpx.l30m,
  tqx.130m = 1 - hasil_tpx.l30m,
  mux.l30m = hasil_mux.l30m
)
Tabel_laki_30m

###Ukuran Akurasi
#RSE
rse(Data_male$`Miu(x)`, hasil_mux.l30m)
rse(Data_male$L, hasil_tpx.l30m)
#SMAPE
smape(Data_male$`Miu(x)`, hasil_mux.l30m)
smape(Data_male$L, hasil_tpx.l30m)
#RMSE
rmse(Data_male$`Miu(x)`, hasil_mux.l30m)
rmse(Data_male$L, hasil_tpx.l30m)
#MAPE
mape(Data_male$`Miu(x)`, hasil_mux.l30m)
mape(Data_male$L, hasil_tpx.l30m)
###Menghitung RSE, SMAPE, dan RMSE
actual_mux = Data_male$`Miu(x)`
actual_tpx = Data_male$L
####hitung residuals
residual_mux = actual_mux - hasil_mux.l30m
residual_tpx = actual_tpx - hasil_tpx.l30m
####RSE
rse_mux_l30m = sqrt(sum(residual_mux^2) / (length(actual_mux) - 2))
cat("RSE :", rse_mux_l30m, "\n")
rse_tpx_l30m = sqrt(sum(residual_tpx^2) / (length(actual_tpx) - 2))
cat("RSE :", rse_tpx_l30m, "\n")
####SMAPE
smape_mux_l30m = (mean((2*(abs(hasil_mux.l30m - actual_mux)))/((abs(actual_mux))+(abs(hasil_mux.l30m)))))*100
cat("SMAPE :", smape_mux_l30m, "\n")
smape_tpx_l30m = (mean((2*(abs(hasil_tpx.l30m - actual_tpx)))/((abs(actual_tpx))+(abs(hasil_tpx.l30m)))))*100
cat("SMAPE :", smape_tpx_l30m, "\n")
####RMSE
rmse_mux_l30m = sqrt(mean(residual_mux^2))
cat("RMSE :", rmse_mux_l30m, "\n")
rmse_tpx_l30m = sqrt(mean(residual_tpx^2))
cat("RMSE :", rmse_tpx_l30m, "\n")
#### MAPE
mape_mux_l30m = (mean(abs((residual_mux) / actual_mux))) * 100
cat("MAPE :", mape_mux_l30m, "\n")
mape_tpx_l30m = (mean(abs((residual_tpx) / actual_tpx))) * 100
cat("MAPE :", mape_tpx_l30m, "\n")
valid_indices = actual_tpx != 0
mape_tpx_l30m = (mean(abs((residual_tpx[valid_indices]) / actual_tpx[valid_indices]))) * 100
cat("MAPE :", mape_tpx_l30m, "\n")


##Perempuan usia 30 tahun
###Estimasi parameter
p_30_mk = MortalityLaw(x=Data_female$Usia, mx=Data_female$`Miu(x)`, law="makeham", opt.method="LF2", fit.this.x = Data_female$Usia[Data_female$Usia >= 30])
coef(p_30_mk)
summary(p_30_mk)
A = p_30_mk$coefficients[[1]]
B = p_30_mk$coefficients[[2]]
C = p_30_mk$coefficients[[3]]

###Penghitungan miu x dan tpx dengan asumsi mk
mux.p30m = function(x){
  (A*exp(B*x))+C
}
tpx.p30m = function(x, t) {
  exp((-C*t)-((A*exp(B*(x))/B)*((exp(B*t))-1)))
}

###Menghitung tpx untuk rentang usia 0 hingga 100
hasil_tpx.p30m = tpx.p30m(x = 0:100, t = 1)
hasil_mux.p30m = mux.p30m(x = 0:100)

###Menggabungkan hasil tpx dan mux ke dalam satu data frame
Tabel_perempuan_30m = data.frame(
  x = 0:100,
  tpx.p30m = hasil_tpx.p30m,
  tqx.p30m = 1 - hasil_tpx.p30m,
  mux.p30m = hasil_mux.p30m
)
Tabel_perempuan_30m

###Ukuran Akurasi
#RSE
rse(Data_female$`Miu(x)`, hasil_mux.p30m)
rse(Data_female$P, hasil_tpx.p30m)
#SMAPE
smape(Data_female$`Miu(x)`, hasil_mux.p30m)
smape(Data_female$P, hasil_tpx.p30m)
#RMSE
rmse(Data_female$`Miu(x)`, hasil_mux.p30m)
rmse(Data_female$P, hasil_tpx.p30m)
#MAPE
mape(Data_female$`Miu(x)`, hasil_mux.p30m)
mape(Data_female$P, hasil_tpx.p30m)
###Menghitung RSE, SMAPE, dan RMSE
actual_mux = Data_female$`Miu(x)`
actual_tpx = Data_female$P
####hitung residuals
residual_mux = actual_mux - hasil_mux.p30m
residual_tpx = actual_tpx - hasil_tpx.p30m
####RSE
rse_mux_p30m = sqrt(sum(residual_mux^2) / (length(actual_mux) - 2))
cat("RSE :", rse_mux_p30m, "\n")
rse_tpx_p30m = sqrt(sum(residual_tpx^2) / (length(actual_tpx) - 2))
cat("RSE :", rse_tpx_p30m, "\n")
####SMAPE
smape_mux_p30m = (mean((2*(abs(hasil_mux.p30m - actual_mux)))/((abs(actual_mux))+(abs(hasil_mux.p30m)))))*100
cat("SMAPE :", smape_mux_p30m, "\n")
smape_tpx_p30m = (mean((2*(abs(hasil_tpx.p30m - actual_tpx)))/((abs(actual_tpx))+(abs(hasil_tpx.p30m)))))*100
cat("SMAPE :", smape_tpx_p30m, "\n")
####RMSE
rmse_mux_p30m = sqrt(mean(residual_mux^2))
cat("RMSE :", rmse_mux_p30m, "\n")
rmse_tpx_p30m = sqrt(mean(residual_tpx^2))
cat("RMSE :", rmse_tpx_p30m, "\n")
#### MAPE
mape_mux_p30m = (mean(abs((residual_mux) / actual_mux))) * 100
cat("MAPE :", mape_mux_p30m, "\n")
mape_tpx_p30m = (mean(abs((residual_tpx) / actual_tpx))) * 100
cat("MAPE :", mape_tpx_p30m, "\n")
valid_indices = actual_tpx != 0
mape_tpx_p30m = (mean(abs((residual_tpx[valid_indices]) / actual_tpx[valid_indices]))) * 100
cat("MAPE :", mape_tpx_p30m, "\n")


##Laki-laki usia 40 tahun
###Estimasi parameter
l_40_mk = MortalityLaw(x=Data_male$Usia, mx=Data_male$`Miu(x)`, law="makeham", opt.method="LF2", fit.this.x = Data_male$Usia[Data_male$Usia >= 40])
coef(l_40_mk)
summary(l_40_mk)
A = l_40_mk$coefficients[[1]]
B = l_40_mk$coefficients[[2]]
C = l_40_mk$coefficients[[3]]

###Penghitungan miu x dan tpx dengan asumsi mk
mux.l40m = function(x){
  (A*exp(B*x))+C
}
tpx.l40m = function(x, t) {
  exp((-C*t)-((A*exp(B*(x))/B)*((exp(B*t))-1)))
}

###Menghitung tpx untuk rentang usia 0 hingga 100
hasil_tpx.l40m = tpx.l40m(x = 0:100, t = 1)
hasil_mux.l40m = mux.l40m(x = 0:100)

###Menggabungkan hasil tpx dan mux ke dalam satu data frame
Tabel_laki_40m = data.frame(
  x = 0:100,
  tpx.l40m = hasil_tpx.l40m,
  tqx.140m = 1 - hasil_tpx.l40m,
  mux.l40m = hasil_mux.l40m
)
Tabel_laki_40m

###Ukuran Akurasi
#RSE
rse(Data_male$`Miu(x)`, hasil_mux.l40m)
rse(Data_male$L, hasil_tpx.l40m)
#SMAPE
smape(Data_male$`Miu(x)`, hasil_mux.l40m)
smape(Data_male$L, hasil_tpx.l40m)
#RMSE
rmse(Data_male$`Miu(x)`, hasil_mux.l40m)
rmse(Data_male$L, hasil_tpx.l40m)
#MAPE
mape(Data_male$`Miu(x)`, hasil_mux.l40m)
mape(Data_male$L, hasil_tpx.l40m)
###Menghitung RSE, SMAPE, dan RMSE
actual_mux = Data_male$`Miu(x)`
actual_tpx = Data_male$L
####hitung residuals
residual_mux = actual_mux - hasil_mux.l40m
residual_tpx = actual_tpx - hasil_tpx.l40m
####RSE
rse_mux_l40m = sqrt(sum(residual_mux^2) / (length(actual_mux) - 2))
cat("RSE :", rse_mux_l40m, "\n")
rse_tpx_l40m = sqrt(sum(residual_tpx^2) / (length(actual_tpx) - 2))
cat("RSE :", rse_tpx_l40m, "\n")
####SMAPE
smape_mux_l40m = (mean((2*(abs(hasil_mux.l40m - actual_mux)))/((abs(actual_mux))+(abs(hasil_mux.l40m)))))*100
cat("SMAPE :", smape_mux_l40m, "\n")
smape_tpx_l40m = (mean((2*(abs(hasil_tpx.l40m - actual_tpx)))/((abs(actual_tpx))+(abs(hasil_tpx.l40m)))))*100
cat("SMAPE :", smape_tpx_l40m, "\n")
####RMSE
rmse_mux_l40m = sqrt(mean(residual_mux^2))
cat("RMSE :", rmse_mux_l40m, "\n")
rmse_tpx_l40m = sqrt(mean(residual_tpx^2))
cat("RMSE :", rmse_tpx_l40m, "\n")
#### MAPE
mape_mux_l40m = (mean(abs((residual_mux) / actual_mux))) * 100
cat("MAPE :", mape_mux_l40m, "\n")
mape_tpx_l40m = (mean(abs((residual_tpx) / actual_tpx))) * 100
cat("MAPE :", mape_tpx_l40m, "\n")
valid_indices = actual_tpx != 0
mape_tpx_l40m = (mean(abs((residual_tpx[valid_indices]) / actual_tpx[valid_indices]))) * 100
cat("MAPE :", mape_tpx_l40m, "\n")


##Perempuan usia 40 tahun
###Estimasi parameter
p_40_mk = MortalityLaw(x=Data_female$Usia, mx=Data_female$`Miu(x)`, law="makeham", opt.method="LF2", fit.this.x = Data_female$Usia[Data_female$Usia >= 40])
coef(p_40_mk)
summary(p_40_mk)
A = p_40_mk$coefficients[[1]]
B = p_40_mk$coefficients[[2]]
C = p_40_mk$coefficients[[3]]

###Penghitungan miu x dan tpx dengan asumsi mk
mux.p40m = function(x){
  (A*exp(B*x))+C 
}
tpx.p40m = function(x, t) {
  exp((-C*t)-((A*exp(B*(x))/B)*((exp(B*t))-1)))
}

###Menghitung tpx untuk rentang usia 0 hingga 100
hasil_tpx.p40m = tpx.p40m(x = 0:100, t = 1)
hasil_mux.p40m = mux.p40m(x = 0:100)

###Menggabungkan hasil tpx dan mux ke dalam satu data frame
Tabel_perempuan_40m = data.frame(
  x = 0:100,
  tpx.p40m = hasil_tpx.p40m,
  tqx.p40m = 1 - hasil_tpx.p40m,
  mux.p40m = hasil_mux.p40m
)
Tabel_perempuan_40m

###Ukuran Akurasi
#RSE
rse(Data_female$`Miu(x)`, hasil_mux.p40m)
rse(Data_female$P, hasil_tpx.p40m)
#SMAPE
smape(Data_female$`Miu(x)`, hasil_mux.p40m)
smape(Data_female$P, hasil_tpx.p40m)
#RMSE
rmse(Data_female$`Miu(x)`, hasil_mux.p40m)
rmse(Data_female$P, hasil_tpx.p40m)
#MAPE
mape(Data_female$`Miu(x)`, hasil_mux.p40m)
mape(Data_female$P, hasil_tpx.p40m)
###Menghitung RSE, SMAPE, dan RMSE
actual_mux = Data_female$`Miu(x)`
actual_tpx = Data_female$P
####hitung residuals
residual_mux = actual_mux - hasil_mux.p40m
residual_tpx = actual_tpx - hasil_tpx.p40m
####RSE
rse_mux_p40m = sqrt(sum(residual_mux^2) / (length(actual_mux) - 2))
cat("RSE :", rse_mux_p40m, "\n")
rse_tpx_p40m = sqrt(sum(residual_tpx^2) / (length(actual_tpx) - 2))
cat("RSE :", rse_tpx_p40m, "\n")
####SMAPE
smape_mux_p40m = (mean((2*(abs(hasil_mux.p40m - actual_mux)))/((abs(actual_mux))+(abs(hasil_mux.p40m)))))*100
cat("SMAPE :", smape_mux_p40m, "\n")
smape_tpx_p40m = (mean((2*(abs(hasil_tpx.p40m - actual_tpx)))/((abs(actual_tpx))+(abs(hasil_tpx.p40m)))))*100
cat("SMAPE :", smape_tpx_p40m, "\n")
####RMSE
rmse_mux_p40m = sqrt(mean(residual_mux^2))
cat("RMSE :", rmse_mux_p40m, "\n")
rmse_tpx_p40m = sqrt(mean(residual_tpx^2))
cat("RMSE :", rmse_tpx_p40m, "\n")
#### MAPE
mape_mux_p40m = (mean(abs((residual_mux) / actual_mux))) * 100
cat("MAPE :", mape_mux_p40m, "\n")
mape_tpx_p40m = (mean(abs((residual_tpx) / actual_tpx))) * 100
cat("MAPE :", mape_tpx_p40m, "\n")
valid_indices = actual_tpx != 0
mape_tpx_p40m = (mean(abs((residual_tpx[valid_indices]) / actual_tpx[valid_indices]))) * 100
cat("MAPE :", mape_tpx_p40m, "\n")

#Export Excel
write.xlsx(Tabel_laki_30g, file = "C:/Users/ASUS/Downloads/SEMESTER 8/TUGAS AKHIR/OUTPUT TA/L30ggo100.xlsx", rowNames = FALSE)
write.xlsx(Tabel_perempuan_30g, file = "C:/Users/ASUS/Downloads/SEMESTER 8/TUGAS AKHIR/OUTPUT TA/P30ggo100.xlsx", rowNames = FALSE)
write.xlsx(Tabel_laki_40g, file = "C:/Users/ASUS/Downloads/SEMESTER 8/TUGAS AKHIR/OUTPUT TA/L40ggo100.xlsx", rowNames = FALSE)
write.xlsx(Tabel_perempuan_40g, file = "C:/Users/ASUS/Downloads/SEMESTER 8/TUGAS AKHIR/OUTPUT TA/P40ggo100.xlsx", rowNames = FALSE)
write.xlsx(Tabel_laki_30m, file = "C:/Users/ASUS/Downloads/SEMESTER 8/TUGAS AKHIR/OUTPUT TA/L30mk100.xlsx", rowNames = FALSE)
write.xlsx(Tabel_perempuan_30m, file = "C:/Users/ASUS/Downloads/SEMESTER 8/TUGAS AKHIR/OUTPUT TA/P30mk100.xlsx", rowNames = FALSE)
write.xlsx(Tabel_laki_40m, file = "C:/Users/ASUS/Downloads/SEMESTER 8/TUGAS AKHIR/OUTPUT TA/L40mk100.xlsx", rowNames = FALSE)
write.xlsx(Tabel_perempuan_40m, file = "C:/Users/ASUS/Downloads/SEMESTER 8/TUGAS AKHIR/OUTPUT TA/P40mk100.xlsx", rowNames = FALSE)
