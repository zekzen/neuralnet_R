############################################
## Author: Dr. Zeki OZEN                  ##
############################################

# oncelikle MASS paketini R diline kuruyoruz.
# MASS paketi kurulu ise alttaki satiri calistirmamiza gerek yok.
# install.packages("MASS")
# MASS paketini calismamiza dahil ediyoruz.
library(MASS)

# MASS paketi icerisinde yer alan Pima.tr ve Pima.te veri setlerini
# data komutu ile sistemimize aktariyoruz.
data("Pima.tr")
data("Pima.te")

#Veri setlerini head komutu ile kullanilabilir hale getiriyoruz.
head(Pima.tr)
head(Pima.te)

# dim(Pima.tr)
# levels(Pima.tr$type)
# summary(Pima.tr)


# Normalizasyon icin gerekli clusterSim paketini yuklu degilse yukluyoruz.
# install.packages("clusterSim")
# Yukledigimiz clusterSim paketini calismamiza dahil ediyoruz.
library(clusterSim)


# Veri seti normalize edilirken type kolonu normalizasyona sokulmuyor.
# type="n4" parametresi, nitelik alanlarinin Min-Max normalizasyon kuralina
# gore normalize edilmesini saglar.
Pima.tr[! names(Pima.tr) %in% c("type")] <- lapply(1, function(x) clusterSim::data.Normalization(Pima.tr[! names(Pima.tr) %in% c("type")],  type="n4", normalization="column"))[[1]]

Pima.te[! names(Pima.te) %in% c("type")] <- lapply(1, function(x) clusterSim::data.Normalization(Pima.te[! names(Pima.te) %in% c("type")],  type="n4", normalization="column"))[[1]]


# Hedef niteligimiz iki siniftan olustugu icin 
# her bir sinifa ait bir nitelik olusturuyoruz.
Pima.tr$hasta = ifelse(Pima.tr$type=="Yes", 1, 0)
Pima.tr$saglikli <- ifelse(Pima.tr$type=="No", 1, 0)

# Backpropagation YSA algoritmasini kullanmak icin
# neuralnet paketi kurulu degilse kuruyoruz.
# install.packages("neuralnet")
library(neuralnet)

# YSA egitiliyor.
ysa_model <- NULL
ysa_model <- neuralnet(formula = as.formula(hasta + saglikli ~ npreg + glu + bp + skin + bmi + ped + age),
                       data = as.matrix( Pima.tr[, ! (names(Pima.tr) %in% c( "type")) ]), 
                       hidden = c(80),
                       err.fct = "sse",
                       learningrate = 0.01,
                       algorithm = "backprop",
                       act.fct = "logistic",
                       linear.output = FALSE,
                       stepmax=1e6
)

# YSA yapisi grafik olarak cizdiriliyor.
plot(ysa_model)

# YSA ciktilari degiskenlerde tutuluyor
# ysa_model_error  <- ysa_model$result.matrix["error", 1]
# ysa_model_steps  <- ysa_model$result.matrix["steps", 1]
# ysa_model_covariate <- ysa_model$covariate
# ysa_model_list_response <- ysa_model$model.list$response
# ysa_model_list_variables <- ysa_model$model.list$variables
# ysa_model_reached_threshold  <- ysa_model$result.matrix["reached.threshold", 1]

  

# Yapay sinir aginin ciktisi islenme asamalarina geciliyor.
tahminler <- NULL
tahminler <- neuralnet::compute (x =   ysa_model,
                                 covariate =  Pima.te[, ! (names(Pima.te) %in% c( "type")) ], rep = 1 )$net.result

# tahmin sonuclari hakkinda kisa bir fikir edinmek icin 
head(tahminler)

# tahmin sonuclari iki kolan halinde bir ornegin hangi sinifa ait oldugunu soylemektedir.
# Biz yapay sinir agini egitirken yapay sinir agi cikti siralamasini 
# as.formula(hasta + saglikli ~ ifadesi ile vermistik.
# Bu nedenle tahmin ciktilari da bu siralamada olacaktir.
# Yani ilk sutun kisinin hasta olma olasiligini
# ikinci sutun ise saglam olma olasiligini vermektedir.
# Asagidaki kod blogu ile olasiliklardan ilk sutun buyuk ise
# kisiyi hasta , ikinci sutun buyuk ise saglam olarak
# kategorik_tahmin degiskeninde sakliyoruz.

kategorik_tahmin <- NULL
for (i in 1:nrow(tahminler)) {
  if ( which.max(tahminler[i,]) == 1) kategorik_tahmin[i] <- "Yes" else kategorik_tahmin[i] <- "No"
}

# Table fonksiyonu ile kategorik olarak YSA modelin sonuclarini gorebiliriz.
table(kategorik_tahmin, Pima.te$type)


# Performans hesaplamak icin bir diger gelismis yontem ise
# caret kutuphanesi icindeki confusionMatrix fonksiyonudur
# caret paketi kurulu degilse kuruyoruz.
# install.packages("neuralnet")
library(caret)

# confusionMatrix fonksiyonu kategorik karsilastirma yapmak icin
# tahmin sonuclarinda ve asil degerlerde bir sinifin referans alinmasini istemektedir
# Bu nedenle hem tamnin sonuclarinda hem de asil degerlerde 
# kisinin hasta olma durumunu ifade eden Yes sinifini
# referans aliyoruz.

kategorik_tahmin <- as.factor(kategorik_tahmin)
kategorik_tahmin <- relevel(kategorik_tahmin, ref="Yes")
  
Pima.te$type <- as.factor(Pima.te$type)
Pima.te$type <- relevel(Pima.te$type, ref="Yes")

# ConfusionMatrix ve buna bagli degiskenler hesaplaniyor  
cMatris<- NULL
cMatris<- caret::confusionMatrix(data =  kategorik_tahmin, reference = Pima.te$type, positive ="Yes" )

cMatris

#Confussion Matrix Grafigi
fourfoldplot(cMatris$table, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Confusion Matrix")

