getwd()
setwd("D:/UVG/2022/Semestre 1 2022/Mineria de datos/HDT7")
df_test <- read.csv("test.csv")
df_train<- read.csv("train.csv")
df_test2 <- read.csv("sample_submission.csv")
df_test['SalePrice']<-df_test2$SalePrice

set.seed(123)
library(ggplot2)
library (dplyr)
library(naivebayes)
library(psych)


library(caret)
library(e1071)
library(klaR)

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

## Definicion de variables dicotomicas trainig dataset
df_train['esBarata']<- ifelse(df_train$SalePrice<baratoMax,1,ifelse(df_train$SalePrice>=baratoMax & df_train$SalePrice<medianoMax,0,0))
df_train['esMediana']<- ifelse(df_train$SalePrice<baratoMax,0,ifelse(df_train$SalePrice>=baratoMax & df_train$SalePrice<medianoMax,1,0))
df_train['esCara']<- ifelse(df_train$SalePrice<baratoMax,0,ifelse(df_train$SalePrice>=baratoMax & df_train$SalePrice<medianoMax,0,1))

## Definicion de variables dicotomicas testing dataset
df_test['esBarata']<- ifelse(df_test$SalePrice<baratoMax,1,ifelse(df_test$SalePrice>=baratoMax & df_test$SalePrice<medianoMax,0,0))
df_test['esMediana']<- ifelse(df_test$SalePrice<baratoMax,0,ifelse(df_test$SalePrice>=baratoMax & df_test$SalePrice<medianoMax,1,0))
df_test['esCara']<- ifelse(df_test$SalePrice<baratoMax,0,ifelse(df_test$SalePrice>=baratoMax & df_test$SalePrice<medianoMax,0,1))


