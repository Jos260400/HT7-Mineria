getwd()
setwd("~/estudios/mineria de datos/HT7-Mineria")


df_test <- read.csv("test.csv")
df_train<- read.csv("train.csv")
df_test2 <- read.csv("sample_submission.csv")
df_test['SalePrice']<-df_test2$SalePrice

# combine by row
df_union <- rbind(df_train, df_test)
# update ID column
df_union$Id <- 1:nrow(df_union)

str(df_union)


porcentaje<-0.7
datos<-df_union
set.seed(123)


corte <- sample(nrow(datos),nrow(datos)*porcentaje)
df_train<-datos[corte,]
df_test<-datos[-corte,]



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

summary(df_test$SalePrice)


df_train['tipoDeCasa']<- ifelse(df_train$SalePrice<baratoMax,"BARATA",ifelse(df_train$SalePrice>=baratoMax & df_train$SalePrice<medianoMax,"MEDIA","CARA"))
## Usar la variable de clasificacion tipodecasa2 con 1 para barato, 2 para mediano, 3 para caro
df_test['tipoDeCasa']<- ifelse(df_test$SalePrice<baratoMax,"BARATA",ifelse(df_test$SalePrice>=baratoMax & df_test$SalePrice<medianoMax,"MEDIA","CARA"))

#df_train['tipoDeCasa2']<- ifelse(df_train$SalePrice<baratoMax,1,ifelse(df_train$SalePrice>=baratoMax & df_train$SalePrice<medianoMax,2,3))
#str(df_train_filtered)

df_train_filtered<-df_train[,c(2,19,20,35,45,48,52,71,81,82)]
df_test_filtered<-df_test[,c(2,19,20,35,45,48,52,71,81,82)]

df_train_filtered$tipoDeCasa <- as.factor(df_train_filtered$tipoDeCasa)
df_test_filtered$tipoDeCasa <- as.factor(df_test_filtered$tipoDeCasa)

##modelos
modelosvm <- svm(tipoDeCasa~., data = df_train_filtered, type= "C-classification" , kernel = 'linear')
modelosvm<-svm(tipoDeCasa~. , data = df_train_filtered, scale = T)



modelosvm$index

plot(modelosvm,df_train_filtered , MSSubClass~YearBuilt)


modeloSVM_L<-svm(tipoDeCasa~., data=df_train_filtered, cost=2^-5, kernel="linear")

modeloSVM_L
df_test
modeloSVM_L<-svm(tipoDeCasa~., data=df_train_filtered, cost=2^5, kernel="linear") #98%
modeloSVM_L<-svm(tipoDeCasa~., data=df_train_filtered, cost=2^-5, kernel="linear") #88%
modeloSVM_L<-svm(tipoDeCasa~., data=df_train_filtered, cost=0.5, kernel="linear")#95%
modeloSVM_R<-svm(tipoDeCasa~., data=df_train_filtered, gamma=2^-5, kernel="radial")
modeloSVM_R<-svm(tipoDeCasa~., data=df_train_filtered, gamma=2^1, kernel="radial")
prediccionL<-predict(modeloSVM_L,newdata=df_test_filtered[,1:9])
prediccionR<-predict(modeloSVM_R,newdata=df_test_filtered)

#matrix
str(prediccionL)
confusionMatrix(df_test_filtered$tipoDeCasa,prediccionL)
confusionMatrix(df_test_filtered$tipoDeCasa,prediccionR)
str(df_test_filtered$tipoDeCasa)
str(prediccionL)
