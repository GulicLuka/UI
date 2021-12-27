#Vnos podatkov ucne in testne mnozice
ucna <- read.table(file="ucnaSem1.txt", sep=",", header=TRUE)
testna <- read.table(file="testnaSem1.txt", sep=",", header=TRUE)


#
#
##  1. NALOGA - VIZUALIZACIJA PODATKOV
#
#

#namembnost stavbe - njena povrsina
len <- length(table(ucna$namembnost))
headNamembnost <- c("izobrazevalna", "javno_storitvena", "kulturno_razvedrilna", "poslovna", "stanovanjska")
sel <- c()
for (i in 1:len){
  sel[i] <- mean(ucna$povrsina[ucna$namembnost == headNamembnost[i]])
}

names(sel) <- c("izobrazevalna", "javno_storitvena", "kulturno_razvedrilna", "poslovna", "stanovanjska")
barplot(sel, ylab="povprecna povrsina", main="Povprecna povrsina glede na namembnost")


#namembnost stavbe - poraba energije
porabapovp <-
i <-1
for (i in 1:len){
  porabapovp[i] <- mean(ucna$poraba[ucna$namembnost == headNamembnost[i]])
}
names(porabapovp) <- c("izobrazevalna", "javno_storitvena", "kulturno_razvedrilna", "poslovna", "stanovanjska")
barplot(porabapovp, ylab="povprecna poraba", main="Povprecna poraba glede na namembnost")


#oblacnost pritisk
boxplot(pritisk ~ oblacnost, data=ucna, main = "skatla z brki stopnje oblacnosti glede na pritisk")


#oblacnost - poraba
boxplot(poraba ~ oblacnost, data=ucna, main = "skatla z brki porabe glede na stopnjo oblacnosti")


#delez his v neki regiji . his v neki regiji glede na namembnost
tabV <- table(ucna$namembnost[ucna$regija == "vzhodna"])
tabZ <- table(ucna$namembnost[ucna$regija == "zahodna"])
stV <- sum(tabV)
stZ <- sum(tabZ)
normTabV <- tabV / stV
normTabZ <- tabZ / stZ
barplot(rbind(normTabV,normTabZ), beside = T, main = "Delez namembnosti poslopij glede na regijo")
barplot(rbind(tabV,tabZ), beside = T, main = "Namembnosti poslopij glede na regijo")

#
#
## 2. NALOGA   OCENJEVANJE IN KONSTRUKCIJA ATRIBUTOV
#
#

#Dodajanje atributov
#YYYY, MM, DD
#ucna
library("data.table")
dt <- as.data.table(ucna)
ymd <- ucna$datum
ucna <- dt[, c("YYYY", "MM", "DD") := tstrsplit(ymd, "-", fixed=TRUE)]

#testna
library("data.table")
dt <- as.data.table(testna)
ymd <- testna$datum
testna <- dt[, c("YYYY", "MM", "DD") := tstrsplit(ymd, "-", fixed=TRUE)]


#vikend
#ucna
library("chron")
ucna$vikend <- is.weekend(ucna$datum)

#testna
testna$vikend <- is.weekend(testna$datum)


#razvrstitev glede na letne case
#ucna
WS <- as.Date("2016-12-21", format = "%Y-%m-%d")
PO <- as.Date("2016-03-20",  format = "%Y-%m-%d")
SU <- as.Date("2016-06-20",  format = "%Y-%m-%d")
JE <- as.Date("2016-09-22",  format = "%Y-%m-%d")
for (i in seq_len(nrow(ucna)))
{
  d <- as.Date(strftime(ucna$datum[i], format="2016-%m-%d"))
  ifelse (d >= WS | d < PO, ucna$letnicas[i] <- "Zima",
          ifelse (d >= PO & d < SU, ucna$letnicas[i] <- "Pomlad",
                  ifelse (d >= SU & d < JE, ucna$letnicas[i] <- "Poletje", ucna$letnicas[i] <- "Jesen")))
}

#testna
for (i in seq_len(nrow(testna)))
{
  d <- as.Date(strftime(testna$datum[i], format="2016-%m-%d"))
  ifelse (d >= WS | d < PO, testna$letnicas[i] <- "Zima",
          ifelse (d >= PO & d < SU, testna$letnicas[i] <- "Pomlad",
                  ifelse (d >= SU & d < JE, testna$letnicas[i] <- "Poletje", testna$letnicas[i] <- "Jesen")))
}


# Povprecna poraba predhodni mesec
#ucna
library(stringr)
months <- str_pad(1:12, pad = 0,width = 2 , "left")

mn <- min(ucna$stavba)
mx <- max(ucna$stavba)

povpPorabaMesec <- NULL

for(m in 1:12) {
  for(i in mn:mx) {
    povpPorabaMesec[i] <- mean(ucna$poraba[ucna$stavba == i & ucna$MM == months[m]])
    if (m == 12) {
      ucna$povp_poraba_mesec_predhodni[ucna$stavba == i & ucna$MM == months[1]] <- povpPorabaMesec[i]
    }
    else {
      ucna$povp_poraba_mesec_predhodni[ucna$stavba == i & ucna$MM == months[m+1]] <- povpPorabaMesec[i]
    }
  }
}

#testna
mn <- min(testna$stavba)
mx <- max(testna$stavba)

povpPorabaMesec <- NULL

for(m in 1:12) {
  for(i in mn:mx) {
    povpPorabaMesec[i] <- mean(testna$poraba[testna$stavba == i & testna$MM == months[m]])
    if (m == 12) {
      testna$povp_poraba_mesec_predhodni[testna$stavba == i & testna$MM == months[1]] <- povpPorabaMesec[i]
    }
    else {
      testna$povp_poraba_mesec_predhodni[testna$stavba == i & testna$MM == months[m+1]] <- povpPorabaMesec[i]
    }
  }
}


#Povprecna poraba na teden
#ucna
library("lubridate")
wknum <- lubridate::week(ymd(ucna$datum))
ucna$week_num <- wknum

mn <- min(ucna$stavba)
mx <- max(ucna$stavba)
povpPorabaTeden <- NULL

for(t in 1:max(ucna$week_num)) {
  for(i in mn:mx) {
    povpPorabaTeden[i] <- mean(ucna$poraba[ucna$stavba == i & ucna$week_num == t])
    if (is.nan(povpPorabaTeden[i])){
      povpPorabaTeden[i] <- 0
    }
    if (t == max(ucna$week_num)) {
      ucna$povp_poraba_teden_predhodni[ucna$stavba == i & ucna$week_num == 1] <- povpPorabaTeden[i]
    }else {
      ucna$povp_poraba_teden_predhodni[ucna$stavba == i & ucna$week_num == t+1] <- povpPorabaTeden[i]
    }
  }
}

#testna
wknum <- lubridate::week(ymd(testna$datum))
testna$week_num <- wknum

mn <- min(testna$stavba)
mx <- max(testna$stavba)
povpPorabaTeden <- NULL

for(t in 1:max(testna$week_num)) {
  for(i in mn:mx) {
    povpPorabaTeden[i] <- mean(testna$poraba[testna$stavba == i & testna$week_num == t])
    if (is.nan(povpPorabaTeden[i])){
      povpPorabaTeden[i] <- 0
    }
    if (t == max(testna$week_num)) {
      testna$povp_poraba_teden_predhodni[testna$stavba == i & testna$week_num == 1] <- povpPorabaTeden[i]
    }else {
      testna$povp_poraba_teden_predhodni[testna$stavba == i & testna$week_num == t+1] <- povpPorabaTeden[i]
    }
  }
}


#Brisanje
#ucna
ucna$DD <- NULL
ucna$MM <- NULL
ucna$YYYY <- NULL
ucna$week_num <- NULL
ucna$stavba <- NULL

#testna
testna$DD <- NULL
testna$MM <- NULL
testna$YYYY <- NULL
testna$week_num <- NULL
testna$stavba <- NULL

## Faktoriziranje
#letni cas
ucna$letnicas <- factor(ucna$letnicas, levels = c("Zima", "Pomlad", "Poletje", "Jesen"))
testna$letnicas <- factor(testna$letnicas, levels = c("Zima", "Pomlad", "Poletje", "Jesen"))

#oblacnost
ucna$oblacnost <- factor(ucna$oblacnost, levels = c(0,1,2,3,4,5,6,7,8,9,10))
testna$oblacnost <- factor(testna$oblacnost, levels = c(0,1,2,3,4,5,6,7,8,9,10))

#namembnost
ucna$namembnost <- factor(ucna$namembnost, levels = c("izobrazevalna", "javno_storitvena", "kulturno_razvedrilna", "poslovna", "stanovanjska"))
testna$namembnost <- factor(testna$namembnost, levels = c("izobrazevalna", "javno_storitvena", "kulturno_razvedrilna", "poslovna", "stanovanjska"))

#regija
ucna$regija <- factor(ucna$regija, levels = c("vzhodna", "zahodna"))
testna$regija <- factor(testna$regija, levels = c("vzhodna", "zahodna"))

#datum
datumi <- ucna$datum
datumDate <- as.Date(datumi, "%Y-%m-%d")
ucna$datum <- datumDate

datumi <- testna$datum
datumDate <- as.Date(datumi, "%Y-%m-%d")
testna$datum <- datumDate

#
## OCENJEVANJE ATRIBUTOV
#

install.packages("CORElearn") 
library(CORElearn)

# KLASIFIKACIJSKO OCENJEVANJE
# INFGAIN pred dodanimi atributi
sort(attrEval(namembnost ~ datum + regija + stavba + poraba + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra, ucna, "InfGain"), decreasing = T)

# INFGAIN po dodanih atributih
sort(attrEval(namembnost ~ ., ucna, "InfGain"), decreasing = T)

# GINI pred dodanimi atributi
sort(attrEval(namembnost ~ datum + regija + stavba + poraba + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra, ucna, "Gini"), decreasing = T)

# GINI po dodanih atributih
sort(attrEval(namembnost ~ ., ucna, "Gini"), decreasing = T)

# GAINRATIO pred dodanimi atributi
sort(attrEval(namembnost ~ datum + regija + stavba + poraba + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra, ucna, "GainRatio"), decreasing = TRUE)

# GAINRATIO po dodanih atributih
sort(attrEval(namembnost ~ ., ucna, "GainRatio"), decreasing = TRUE)

# RELIEFF pred dodanimi atributi
sort(attrEval(namembnost ~ datum + regija + stavba + poraba + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra, ucna, "ReliefFequalK"), decreasing = TRUE)

# RELIEFF po dodanih atributih
sort(attrEval(namembnost ~ ., ucna, "ReliefFequalK"), decreasing = TRUE)


# REGRESIJSKO OCENJEVANJE
# MSEofMean
sort(attrEval(poraba ~ ., ucna, "MSEofMean"), decreasing = TRUE)
# RReliefFexpRank
sort(attrEval(poraba ~ ., ucna, "RReliefFexpRank"), decreasing = TRUE)


#
#
## 3.MODELIRANJE 
#
#

#
## KLASIFIKACIJA
#

install.packages("nnet")
library(nnet)

#Klasifikacijska tocnost
CA <- function(observed, predicted)
{
  mean(observed == predicted)
}

#Brierjeva mera
brier.score <- function(observedMatrix, predictedMatrix)
{
  sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}

observed <- testna$namembnost
obsMat <- class.ind(testna$namembnost)


#
#
## NAIVNI BAYESOV KLASIFIKATOR
#
#

# Vsi atributi
library(CORElearn)
nb <- CoreModel(namembnost ~ ., data = ucna, model="bayes")
predicted <- predict(nb, testna, type="class")
CA(observed, predicted)

predMat <- predict(nb, testna, type = "prob")
brier.score(obsMat, predMat)

# ReliefF
library(CORElearn)
nb <- CoreModel(namembnost ~ povrsina + regija + leto_izgradnje, data = ucna, model="bayes")
predicted <- predict(nb, testna, type="class")
CA(observed, predicted)

predMat <- predict(nb, testna, type = "prob")
brier.score(obsMat, predMat)

# Gain Ratio
library(CORElearn)
nb <- CoreModel(namembnost ~ povp_poraba_mesec_predhodni + povrsina + povp_poraba_teden_predhodni + poraba + regija + leto_izgradnje, data = ucna, model="bayes")
predicted <- predict(nb, testna, type="class")
CA(observed, predicted)

predMat <- predict(nb, testna, type = "prob")
brier.score(obsMat, predMat)

# InfGain
library(CORElearn)
nb <- CoreModel(namembnost ~ leto_izgradnje + povrsina + regija, data = ucna, model="bayes")
predicted <- predict(nb, testna, type="class")
CA(observed, predicted)

predMat <- predict(nb, testna, type = "prob")
brier.score(obsMat, predMat)

# Gini
library(CORElearn)
nb <- CoreModel(namembnost ~ povrsina + leto_izgradnje, data = ucna, model="bayes")
predicted <- predict(nb, testna, type="class")
CA(observed, predicted)

predMat <- predict(nb, testna, type = "prob")
brier.score(obsMat, predMat)

# Wrapper
library(CORElearn)
nb <- CoreModel(namembnost ~  leto_izgradnje + povrsina + regija, data = ucna, model="bayes")
predicted <- predict(nb, testna, type="class")
CA(observed, predicted)

predMat <- predict(nb, testna, type = "prob")
brier.score(obsMat, predMat)


#
#
# ODLOCITVENO DREVO
#
#

# Vsi atributi
library(CORElearn)
dt <- CoreModel(namembnost ~ . , data = ucna, model="tree")
predicted <- predict(dt, testna, type="class")
CA(observed, predicted)

predMat <- predict(dt, testna, type = "prob")
brier.score(obsMat, predMat)

# ReliefF
library(CORElearn)
dt <- CoreModel(namembnost ~ povrsina + regija + povp_poraba_mesec_predhodni + regija + povp_poraba_teden_predhodni + poraba , data = ucna, model="tree")
predicted <- predict(dt, testna, type="class")
CA(observed, predicted)

predMat <- predict(dt, testna, type = "prob")
brier.score(obsMat, predMat)

# Gain Ratio
library(CORElearn)
dt <- CoreModel(namembnost ~ povp_poraba_mesec_predhodni + povrsina + povp_poraba_teden_predhodni + poraba + regija + leto_izgradnje , data = ucna, model="tree")
predicted <- predict(dt, testna, type="class")
CA(observed, predicted)

predMat <- predict(dt, testna, type = "prob")
brier.score(obsMat, predMat)

# InfGain
library(CORElearn)
dt <- CoreModel(namembnost ~ leto_izgradnje + povrsina + regija , data = ucna, model="tree")
predicted <- predict(dt, testna, type="class")
CA(observed, predicted)

predMat <- predict(dt, testna, type = "prob")
brier.score(obsMat, predMat)

# Gini
library(CORElearn)
dt <- CoreModel(namembnost ~ povrsina + leto_izgradnje , data = ucna, model="tree")
predicted <- predict(dt, testna, type="class")
CA(observed, predicted)

predMat <- predict(dt, testna, type = "prob")
brier.score(obsMat, predMat)

# Wrapper
library(CORElearn)
dt <- CoreModel(namembnost ~ povrsina, data = ucna, model="tree")
predicted <- predict(dt, testna, type="class")
CA(observed, predicted)

predMat <- predict(dt, testna, type = "prob")
brier.score(obsMat, predMat)


#
#
# SVM
#
#

# Vsi atributi
library(e1071)
set.seed(0)
sm <- svm(namembnost ~ ., data = ucna)
predicted <- predict(sm, testna, type="class")
CA(observed, predicted)

sm <- svm(namembnost ~ ., ucna, probability = T)
pred <- predict(sm, testna, probability = T)
predMat <- attr(pred, "probabilities")

predMat <- predMat[,colnames(obsMat)]

brier.score(obsMat, predMat)

# Wrapper
library(e1071)
set.seed(0)
sm <- svm(namembnost ~ povrsina + leto_izgradnje + regija, data = ucna)
predicted <- predict(sm, testna, type="class")
CA(observed, predicted)

sm <- svm(namembnost ~ povrsina + leto_izgradnje + regija, ucna, probability = T)
pred <- predict(sm, testna, probability = T)
predMat <- attr(pred, "probabilities")

predMat <- predMat[,colnames(obsMat)]

brier.score(obsMat, predMat)


#
## REGRESIJA
#

# linearna regresija
model <- lm(poraba ~ ., ucna)
predicted <- predict(model, testna)
observed <- testna$poraba

# srednja absolutna napaka
mae <- function(obs, pred) {
  mean(abs(obs - pred))
}

# srednja kvadratna napaka
mse <- function(obs, pred) {
  mean((obs - pred)^2)
}

mae(observed, predicted)
mse(observed, predicted)

# relativna srednja absolutna napaka
rmae <- function(obs, pred, mean.val) {
  sum(abs(obs - pred)) / sum(abs(obs - mean.val))
}

# relativna srednja kvadratna napaka
rmse <- function(obs, pred, mean.val) {
  sum((obs - pred)^2) / sum((obs - mean.val)^2)
}


#
#
## REGRESIJSKO DREVO
#
#


library(rpart)
library(rpart.plot)

#Vsi atributi
set.seed(0)
rt.model <- rpart(poraba ~ ., data = ucna)

predicted <- predict(rt.model, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna$poraba))

rt.model <- rpart(poraba ~ ., data = ucna, cp = 0)

tab <- printcp(rt.model)

row <- which.min(tab[, "xerror"])
th <- mean(c(tab[row, "CP"], tab[row - 1, "CP"]))

rt.model <- prune(rt.model, cp = th)

predicted <- predict(rt.model, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna$poraba))
mse(testna$poraba, predicted)
rmse(testna$poraba, predicted, mean(ucna$poraba))

# RReliefF / MSEofMean ; dobimo iste predlagane atribute
set.seed(0)
rt.model <- rpart(poraba ~ povp_poraba_mesec_predhodni + povp_poraba_teden_predhodni, data = ucna)

predicted <- predict(rt.model, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna$poraba))

rt.model <- rpart(poraba ~ povp_poraba_mesec_predhodni + povp_poraba_teden_predhodni, data = ucna, cp = 0)

tab <- printcp(rt.model)

row <- which.min(tab[, "xerror"])
th <- mean(c(tab[row, "CP"], tab[row - 1, "CP"]))

rt.model <- prune(rt.model, cp = th)

predicted <- predict(rt.model, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna$poraba))
mse(testna$poraba, predicted)
rmse(testna$poraba, predicted, mean(ucna$poraba))

# Wrapper
set.seed(0)
rt.model <- rpart(poraba ~ povp_poraba_teden_predhodni + povrsina, data = ucna)
predicted <- predict(rt.model, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna$poraba))

rt.model <- rpart(poraba ~ povp_poraba_teden_predhodni + povrsina, data = ucna, cp = 0)

tab <- printcp(rt.model)

row <- which.min(tab[, "xerror"])
th <- mean(c(tab[row, "CP"], tab[row - 1, "CP"]))

rt.model <- prune(rt.model, cp = th)

predicted <- predict(rt.model, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna$poraba))
mse(testna$poraba, predicted)
rmse(testna$poraba, predicted, mean(ucna$poraba))


#
#
## K-NAJBLIZJIH SOSEDOV
#
#


# Vsi atributi; k = 5
set.seed(0)
library(kknn)
modelFull <- train.kknn(poraba ~ ., ucna, ks = 5)
predicted <- predict(modelFull, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna$poraba))
mse(testna$poraba, predicted)
rmse(testna$poraba, predicted, mean(ucna$poraba))


# Vsi atributi; k = 10
set.seed(0)
library(kknn)
modelFull <- train.kknn(poraba ~ ., ucna, ks = 10)
predicted <- predict(modelFull, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna$poraba))
mse(testna$poraba, predicted)
rmse(testna$poraba, predicted, mean(ucna$poraba))

# Wrapper; k = 5 !!!
set.seed(0)
library(kknn)
modelFull <- train.kknn(poraba ~ povp_poraba_mesec_predhodni + vikend + letnicas + povp_poraba_teden_predhodni + datum + povrsina + temp_zraka + namembnost + leto_izgradnje, ucna, ks = 5)
predicted <- predict(modelFull, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna$poraba))
mse(testna$poraba, predicted)
rmse(testna$poraba, predicted, mean(ucna$poraba))


# Wrapper; k = 10
set.seed(0)
library(kknn)
modelFull <- train.kknn(poraba ~ povp_poraba_mesec_predhodni + vikend + leto_izgradnje + datum + povrsina + regija, ucna, ks = 10)
predicted <- predict(modelFull, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna_Vzhodna$poraba))
mse(testna$poraba, predicted)
rmse(testna$poraba, predicted, mean(ucna_Vzhodna$poraba))

#
#
## NAKLJUCNI GOZD
#
#

# Vsi atributi
library(randomForest)
rf.model <- randomForest(poraba ~ ., ucna)
predicted <- predict(rf.model, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna$poraba))
mse(testna$poraba, predicted)
rmse(testna$poraba, predicted, mean(ucna$poraba))

# RReliefF
library(randomForest)
rf.model <- randomForest(poraba ~ povp_poraba_mesec_predhodni + povp_poraba_teden_predhodni, ucna)
predicted <- predict(rf.model, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna$poraba))
mse(testna$poraba, predicted)
rmse(testna$poraba, predicted, mean(ucna$poraba))


#
#
## KOMBINIRANJE
#
#

set.seed(0)
library(CORElearn)
CA <- function(observed, predicted) {
  mean(observed == predicted)
}


#
## GLASOVANJE
#

# zgradimo tri razlicne modele
modelDT <- CoreModel(namembnost ~ ., ucna, model = "tree")
modelNB <- CoreModel(namembnost ~ ., ucna, model = "bayes")
modelKNN <- CoreModel(namembnost ~ ., ucna, model = "knn", kInNN = 5)

#ovrednotimo
predDT <- predict(modelDT, testna, type = "class")
caDT <- CA(testna$namembnost, predDT)
caDT

predNB <- predict(modelNB, testna, type = "class")
caNB <- CA(testna$namembnost, predNB)
caNB

predKNN <- predict(modelKNN, testna, type = "class")
caKNN <- CA(testna$namembnost, predKNN)
caKNN

# zdruzimo napovedi posameznih modelov v en podatkovni okvir
pred <- data.frame(predDT, predNB, predKNN)

# testni primer klasificiramo v razred z najvec glasovi
voting <- function(predictions) {
  res <- vector()
  for (i in seq_len(nrow(predictions))) {
    vec <- unlist(predictions[i, ])
    res[i] <- names(which.max(table(vec)))
  }
  res
}

predClass <- voting(pred)

predicted <- factor(predClass, levels = levels(ucna$namembnost))

CA(testna$namembnost, predicted)


#
## UTEZENO GLASOVANJE
#

# verjetnostne napovedi po razredih
predDT.prob <- predict(modelDT, testna, type = "prob")
predNB.prob <- predict(modelNB, testna, type = "prob")
predKNN.prob <- predict(modelKNN, testna, type = "prob")

# sestejemo napovedane verjetnosti s strani razlicnih modelov
predProb <- predDT.prob + predNB.prob + predKNN.prob

# izberemo razred z najvecjo verjetnostjo
predClass <- colnames(predProb)[max.col(predProb)]
predicted <- factor(predClass, levels(ucna$namembnost))

CA(testna$namembnost, predicted)


library(ipred)

#zgradimo model
mymodel <- function(formula, data, target.model) {
  CoreModel(formula, data, model = target.model)
}

#napovedi modela
mypredict <- function(object, newdata) {
  pred <- predict(object, newdata, type = "class")
  destroyModels(object)
  pred
}

# PRVI
res <- errorest(namembnost ~ ., ucna, model = mymodel, predict = mypredict, target.model = "tree")

# to??nost
caDT.cv <- 1 - res$error

# DRUGI
res <- errorest(namembnost ~ ., ucna, model = mymodel, predict = mypredict, target.model = "bayes")
caNB.cv <- 1 - res$error

#TRETJI
mymodelKNN <- function(formula, data, valK) {
  CoreModel(formula, data, model = "knn", kInNN = valK)
}
res <- errorest(namembnost ~ ., ucna, model = mymodelKNN, predict = mypredict, valK = 5)
caKNN.cv <- 1 - res$error

# napovedana verjetnost 
predProb <- caDT.cv * predDT.prob + caNB.cv * predNB.prob + caKNN.cv * predKNN.prob

# izberemo stolpce z najvi??jo vrednostjo
predClass <- colnames(predProb)[max.col(predProb)]
predicted <- factor(predClass, levels(ucna$namembnost))

CA(testna$namembnost, predicted)


#
## BAGGING FUNKCIJA
#

# vsakic bo drugacen rezultat - ker nakljucno izbiramo primere
library(ipred)
bag <- bagging(namembnost ~ ., ucna, nbagg = 30)
predicted <- predict(bag, testna, type = "class")

CA(testna$namembnost, predicted)


#
## RANDOM FOREST
#

# dodatno se izbiramo podmnozico atributov, ki nastopajo kot kandidati, ki jih obravnavamo, ko gradimo drevo
library(randomForest)
rf <- randomForest(namembnost ~ ., ucna)
predicted <- predict(rf, testna, type = "class")

CA(testna$namembnost, predicted)


#
## BOOSTING FUNKCIJA
#

library(adabag)
bm <- boosting(namembnost ~ ., ucna, mfinal = 100)
predictions <- predict(bm, testna)
names(predictions)

predicted <- predictions$class

CA(testna$namembnost, predicted)




#
#
## PRIMERJANJE PO REGIJAH VZHODNA / ZAHODNA
#
#


#KLASIFIKACIJA
#ucenje iz ene same regije
ucna_Vzhodna <- ucna[ucna$regija == "vzhodna"]
ucna_zahodna <- ucna[ucna$regija == "zahodna"]

library(nnet)
observed <- testna$namembnost
obsMat <- class.ind(testna$namembnost)


#
#
## NAIVNI BAYESOV KLASIFIKATOR
#
#

# Vzhodna regija
set.seed(0)
library(CORElearn)
nb <- CoreModel(namembnost ~ povrsina + regija + povp_poraba_mesec_predhodni + regija + povp_poraba_teden_predhodni + poraba, data = ucna_Vzhodna, model="bayes")
predicted <- predict(nb, testna, type="class")
CA(observed, predicted)

predMat <- predict(nb, testna, type = "prob")
brier.score(obsMat, predMat)


# Zahodna regija
set.seed(0)
library(CORElearn)
nb <- CoreModel(namembnost ~ povrsina + regija + povp_poraba_mesec_predhodni + regija + povp_poraba_teden_predhodni + poraba, data = ucna_zahodna, model="bayes")
predicted <- predict(nb, testna, type="class")
CA(observed, predicted)

predMat <- predict(nb, testna, type = "prob")
brier.score(obsMat, predMat)


#
#
## ODLOCITVENO DREVO
#
#

# Vzhodna regija
library(CORElearn)
set.seed(0)
dt <- CoreModel(namembnost ~ povrsina + regija + povp_poraba_mesec_predhodni + regija + povp_poraba_teden_predhodni + poraba , data = ucna_Vzhodna, model="tree")
predicted <- predict(dt, testna, type="class")
CA(observed, predicted)

predMat <- predict(dt, testna, type = "prob")
brier.score(obsMat, predMat)

# Zahodna regija
library(CORElearn)
set.seed(0)
dt <- CoreModel(namembnost ~ povrsina + regija + povp_poraba_mesec_predhodni + regija + povp_poraba_teden_predhodni + poraba, data = ucna_zahodna, model="tree")
predicted <- predict(dt, testna, type="class")
CA(observed, predicted)

predMat <- predict(dt, testna, type = "prob")
brier.score(obsMat, predMat)


#
#
## SVM
#
#

# Vzhodna regija
library(e1071)
set.seed(0)
sm <- svm(namembnost ~ ., data = ucna_Vzhodna)
predicted <- predict(sm, testna, type="class")
CA(observed, predicted)

sm <- svm(namembnost ~ ., ucna_Vzhodna, probability = T)
pred <- predict(sm, testna, probability = T)
predMat <- attr(pred, "probabilities")

colnames(obsMat)
colnames(predMat)

predMat <- predMat[,colnames(obsMat)]

brier.score(obsMat, predMat)

# Zahodna regija
library(e1071)
set.seed(0)
sm <- svm(namembnost ~ ., data = ucna_zahodna)
predicted <- predict(sm, testna, type="class")
CA(observed, predicted)

sm <- svm(namembnost ~ ., ucna_zahodna, probability = T)
pred <- predict(sm, testna, probability = T)
predMat <- attr(pred, "probabilities")

colnames(obsMat)
colnames(predMat)

predMat <- predMat[,colnames(obsMat)]

brier.score(obsMat, predMat)



# REGRESIJA

#
#
## KKNN
#
#

# Vzhodna regija
set.seed(0)
library(kknn)
modelFull <- train.kknn(poraba ~ ., ucna_Vzhodna, ks = 10)
predicted <- predict(modelFull, testna)
rmae(testna$poraba, predicted, mean(ucna_Vzhodna$poraba))
rmse(testna$poraba, predicted, mean(ucna_Vzhodna$poraba))

# Zahodna regija
set.seed(0)
modelFull <- train.kknn(poraba ~ ., ucna_zahodna, ks = 10)
predicted <- predict(modelFull, testna)
rmae(testna$poraba, predicted, mean(ucna_zahodna$poraba))
rmse(testna$poraba, predicted, mean(ucna_zahodna$poraba))


#
#
## REGRESIJSKO DREVO
#
#

library(rpart)
library(rpart.plot)

# Vzhodna regija
set.seed(0)
rt.model <- rpart(poraba ~ povp_poraba_mesec_predhodni + povp_poraba_teden_predhodni, data = ucna_Vzhodna)

predicted <- predict(rt.model, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna_Vzhodna$poraba))
mse(testna$poraba, predicted)
rmse(testna$poraba, predicted, mean(ucna_Vzhodna$poraba))

rt.model <- rpart(poraba ~ povp_poraba_mesec_predhodni + povp_poraba_teden_predhodni, data = ucna_Vzhodna, cp = 0)

tab <- printcp(rt.model)

row <- which.min(tab[, "xerror"])
th <- mean(c(tab[row, "CP"], tab[row - 1, "CP"]))

rt.model <- prune(rt.model, cp = th)

predicted <- predict(rt.model, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna_Vzhodna$poraba))
mse(testna$poraba, predicted)
rmse(testna$poraba, predicted, mean(ucna_Vzhodna$poraba))


# Zahodna regija
set.seed(0)
rt.model <- rpart(poraba ~ povp_poraba_mesec_predhodni + povp_poraba_teden_predhodni, data = ucna_zahodna)

predicted <- predict(rt.model, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna_zahodna$poraba))
mse(testna$poraba, predicted)
rmse(testna$poraba, predicted, mean(ucna_zahodna$poraba))

rt.model <- rpart(poraba ~ povp_poraba_mesec_predhodni + povp_poraba_teden_predhodni, data = ucna_zahodna, cp = 0)

tab <- printcp(rt.model)

row <- which.min(tab[, "xerror"])
th <- mean(c(tab[row, "CP"], tab[row - 1, "CP"]))

rt.model <- prune(rt.model, cp = th)

predicted <- predict(rt.model, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna_zahodna$poraba))
mse(testna$poraba, predicted)
rmse(testna$poraba, predicted, mean(ucna_zahodna$poraba))


#
#
## NAKLJUCNI GOZD
#
#

# Vzhodna regija
install.packages("randomForest")
library(randomForest)
rf.model <- randomForest(poraba ~ povp_poraba_mesec_predhodni + povp_poraba_teden_predhodni, ucna_Vzhodna)
predicted <- predict(rf.model, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna_Vzhodna$poraba))
mse(testna$poraba, predicted)
rmse(testna$poraba, predicted, mean(ucna_Vzhodna$poraba))

# Zahodna regija
install.packages("randomForest")
library(randomForest)
rf.model <- randomForest(poraba ~ povp_poraba_mesec_predhodni + povp_poraba_teden_predhodni, ucna_zahodna)
predicted <- predict(rf.model, testna)
mae(testna$poraba, predicted)
rmae(testna$poraba, predicted, mean(ucna_zahodna$poraba))
mse(testna$poraba, predicted)
rmse(testna$poraba, predicted, mean(ucna_zahodna$poraba))


#
#
## KLASIFIKACIJSKI WRAPPERJI
#
#

#
## NAIVNI BAYES
#

source("wrapper.R")
library(CORElearn)

myTrainFunc <- function(formula, traindata)
{
  CoreModel(formula, traindata, model="bayes")
}

myPredictFunc <- function(model, testdata)
{
  predict(model, testdata, type="class")
}

myEvalFunc <- function(predicted, observed, trained)
{
  1.0 - mean(observed == predicted)	
}

set.seed(0)
wrapper(namembnost ~ ., ucna, myTrainFunc, myPredictFunc, myEvalFunc, cvfolds=10)

#
## ODLOCITVENO DREVO
#

source("wrapper.R")
library(CORElearn)

myTrainFunc <- function(formula, traindata)
{
  CoreModel(formula, traindata, model="tree")
}

myPredictFunc <- function(model, testdata)
{
  predict(model, testdata, type="class")
}

myEvalFunc <- function(predicted, observed, trained)
{
  1.0 - mean(observed == predicted)	
}

set.seed(0)
wrapper(namembnost ~ ., ucna, myTrainFunc, myPredictFunc, myEvalFunc, cvfolds=10)

#
## SVM
#

source("wrapper.R")
library(e1071)

myTrainFunc <- function(formula, traindata)
{
  svm(formula, traindata)
}

myPredictFunc <- function(model, testdata)
{
  predict(model, testdata, type="class")
}

myEvalFunc <- function(predicted, observed, trained)
{
  1.0 - mean(observed == predicted)	
}

set.seed(0)
wrapper(namembnost ~ ., ucna, myTrainFunc, myPredictFunc, myEvalFunc, cvfolds=10)




#
#
## REGRESIJSKI WRAPPERJI
#
#

#
## REGRESIJSKO DREVO
#

source("wrapper.R")
library(rpart)
library(rpart.plot)

myTrainFuncReg <- function(formula, traindata) {
  rpart(formula, traindata)
}

myPredictFuncReg <- function(model, testdata) {
  predict(model, testdata)
}

myEvalFuncRMSE <- function(predicted, observed, trained) {
  sum((observed - predicted)^2) / sum((observed - mean(trained))^2)
}

set.seed(0)
wrapper(poraba ~ ., ucna, myTrainFuncReg, myPredictFuncReg, myEvalFuncRMSE, cvfolds = 10)


#
## K-NAJBLIZJIH SOSEDOV (KKNN)
#

# k = 5
source("wrapper.R")

myTrainFuncReg <- function(formula, traindata) {
  train.kknn(formula, traindata, ks = 5)
}

myPredictFuncReg <- function(model, testdata) {
  predict(model, testdata)
}

myEvalFuncRMSE <- function(predicted, observed, trained) {
  sum((observed - predicted)^2) / sum((observed - mean(trained))^2)
}

set.seed(0)
wrapper(poraba ~ ., ucna, myTrainFuncReg, myPredictFuncReg, myEvalFuncRMSE, cvfolds = 10)

# k = 10
source("wrapper.R")

myTrainFuncReg <- function(formula, traindata) {
  train.kknn(formula, traindata, ks = 10)
}

myPredictFuncReg <- function(model, testdata) {
  predict(model, testdata)
}

myEvalFuncRMSE <- function(predicted, observed, trained) {
  sum((observed - predicted)^2) / sum((observed - mean(trained))^2)
}

set.seed(0)
wrapper(poraba ~ ., ucna, myTrainFuncReg, myPredictFuncReg, myEvalFuncRMSE, cvfolds = 10)






