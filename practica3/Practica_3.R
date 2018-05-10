library(caret)
library(glmnet)

#Lectura de los datos, Falta introducir la lectura del conjunto de test
opt.tra = read.csv("optdigits_tra.csv")
airfoil.tra = read.csv("airfoil_self_noise.csv")

#Configuracion de parémetros
seed = 674155357
set.seed(seed = seed)
numberOfPartes = 5

#Creacion de variables iniciales







#Describimos los datos del train
summary(opt.tra)
summary(airfoil.tra)


#Division de los datos en etiquetas y carasterísticas y iniciamos el cross validation
labels = airfoil.tra[,"X0.26"]
names.opt = colnames(airfoil.tra)
predict.opt = airfoil.tra[,match(names.opt, colnames(airfoil.tra))]

cv.opt <- createFolds(labels, k = numberOfPartes, returnTrain = TRUE)

#for (indice in 1:numberOfPartes) {
  indice = 1

  indexToSelect = switch(indice
                          ,cv.opt$Fold1
                          ,cv.opt$Fold2
                          ,cv.opt$Fold3
                          ,cv.opt$Fold4
                          ,cv.opt$Fold5)

  #Particionado de los datos
  labels.train = labels[indexToSelect]
  labels.test =  labels[-indexToSelect]
  predict.opt.train = predict.opt[indexToSelect,]
  predict.opt.test  = predict.opt[-indexToSelect,]


  #Inicio del preprocesamiento de los datos
  grid <- expand.grid(alpha = c(0,.2,.4,.6,.8,1),
                      lambda = seq(.01,.2, length.out = 10))
  
  glmnet_ctrl <- trainControl(method = "cv", number = numberOfPartes)
  glmnet_fit <- train(X0.26 ~ ., data = predict.opt.train,
                      method = "LogitBoost",
                      preProcess = c("center", "scale"),
                      metric = "Accuracy",
                      tuneGrid = grid,
                      trControl = glmnet_ctrl)


  
 # }


