getwd()
setwd("D:/UVG/2022/Semestre 1 2022/Mineria de datos/HDT7")
df_test <- read.csv("test.csv")
df_train<- read.csv("train.csv")
df_test2 <- read.csv("sample_submission.csv")
df_test['SalePrice']<-df_test2$SalePrice

set.seed(123)
library(ggplot2)
library (dplyr)


library(caret)
library(e1071)


##limites de var categorica tipo de casa barata/ mediana / cara
summary(df_train$SalePrice)
priceRange <- max(df_train$SalePrice)-min(df_train$SalePrice)
baratoMax<- min(df_train$SalePrice)+(priceRange/3)
medianoMax <- baratoMax+(priceRange/3)
caroMax<-max(df_train$SalePrice)
min(df_train$SalePrice)
max(df_test$SalePrice)
(medianoMax)
baratoMax

df_train['tipoDeCasa']<- ifelse(df_train$SalePrice<baratoMax,"BARATA",ifelse(df_train$SalePrice>=baratoMax & df_train$SalePrice<medianoMax,"MEDIA","CARA"))
## Usar la variable de clasificacion tipodecasa2 con 1 para barato, 2 para mediano, 3 para caro
df_train['tipoDeCasa2']<- ifelse(df_train$SalePrice<baratoMax,1,ifelse(df_train$SalePrice>=baratoMax & df_train$SalePrice<medianoMax,2,3))
str(df_train_filtered)
df_train_filtered$tipoDeCasa <- as.factor(df_train_filtered$tipoDeCasa)
df_train_filtered<-df_train[,c(2,19,20,35,45,48,52,71,81,82,83)]

modelosvm <- svm(tipoDeCasa~., data = df_train_filtered, type= "C-classification" , kernel = 'linear')
modelosvm<-svm(tipoDeCasa~. , data = df_train_filtered, scale = T)


str(df_train_filtered)
summary(modelosvm)

modelosvm$index

plot(modelosvm,df_train_filtered , MSSubClass~YearBuilt)


modeloSVM_L<-svm(tipoDeCasa~., data=df_train_filtered, cost=2^5, kernel="linear")
