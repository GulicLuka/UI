## Kombiniranje algoritmov strojnega ucenja
set.seed(0)
#delez pravilno napovedanih
library(CORElearn)
CA <- function(observed, predicted) {
  mean(observed == predicted)
}

### GLASOVANJE
# imamo tri modele. Lahko bi jih imeli tudi več. Vsak model, ko mu pokažemo testni primer glasuje za en razred.
# zberemo glasove. In testni model klasificiramo v tisti razred, ki je dobil največ glasov.
# če je enako. Izberemo naključno

# zgradimo tri razlicne modele
modelDT <- CoreModel(namembnost ~ ., ucna, model = "tree")
modelNB <- CoreModel(namembnost ~ ., ucna, model = "bayes")
modelKNN <- CoreModel(namembnost ~ ., ucna, model = "knn", kInNN = 5)

#ovrednotimo
#dobimo procent!! vidomo kateri je najboljsi
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
# stolpci = napovedi posameznih modelov
# vrstice = predstavljajo testne primere
pred <- data.frame(predDT, predNB, predKNN)

# testni primer klasificiramo v razred z najvec glasovi
# ni nujno da izboljšamo rezultat
voting <- function(predictions) {
  res <- vector()
  for (i in seq_len(nrow(predictions))) {
    vec <- unlist(predictions[i, ])
    res[i] <- names(which.max(table(vec)))
  }
  res
}

# dobivamo string(pomankljivost)
predClass <- voting(pred)
#faktoriziramo
predicted <- factor(predClass, levels = levels(train$Class))

# klasifikacijska točnost
CA(testna$namembnost, predicted)




### UTEZENO GLASOVANJE
# verjetnostne napovedi po razredih
predDT.prob <- predict(modelDT, testna, type = "prob")
predNB.prob <- predict(modelNB, testna, type = "prob")
predKNN.prob <- predict(modelKNN, testna, type = "prob")

# delez glasu, ki ga model poda posameznemu razredu - zvezno glasovanje(razdeli po razredih)

# sestejemo napovedane verjetnosti s strani razlicnih modelov
predProb <- predDT.prob + predNB.prob + predKNN.prob

# izberemo razred z najvecjo verjetnostjo
predClass <- colnames(predProb)[max.col(predProb)]
predicted <- factor(predClass, levels(vehicle$Class))

# klasifikacijska točnost
CA(testna$namembnost, predicted)



## Pri utezenem glasovanju lahko upostevamo tudi tocnosti modelov
# točnost modelov ocenimo na učnih podatkih
# Uporabimo precno preverjanje, ki nam oceni kvaliteto modela na učnih podatkih
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
# Izvede precno preverjanje
# target.model - zanima nas odlocitveno drevo
# K=10 (default)
# help page errorest -> za ostale parametre
# vrne nam napako
res <- errorest(namembnost ~ ., ucna, model = mymodel, predict = mypredict, target.model = "tree")

#zanima nas točnost
#1- napaka
caDT.cv <- 1 - res$error
# caDT.cv # prečno preverjanje, utež za glasovanje

# DRUGI
# Naivni Bayes
res <- errorest(namembnost ~ ., ucna, model = mymodel, predict = mypredict, target.model = "bayes")
caNB.cv <- 1 - res$error

#TRETJI
mymodelKNN <- function(formula, data, valK) {
  CoreModel(formula, data, model = "knn", kInNN = valK)
}
res <- errorest(namembnost ~ ., ucna, model = mymodelKNN, predict = mypredict, valK = 5)
caKNN.cv <- 1 - res$error

# sedaj pri sestevanju napovedane verjetnosti utezimo s pricakovano tocnostjo modela
# vsako verjetnostno napoved posameznega modela zmozimo z ocenjeno tocnostjo modela
# glas posamezenega modela dodatno utezili se z ocenjeno kvaliteto tega modela
# bolj kvalitetni modeli imajo mocnejsi glas pri tem glasovanju
predProb <- caDT.cv * predDT.prob + caNB.cv * predNB.prob + caKNN.cv * predKNN.prob

# izberemo stolpce z najvišjo vrednostjo
predClass <- colnames(predProb)[max.col(predProb)]
predicted <- factor(predClass, levels(vehicle$Class))

# klasifikacijska točnost
CA(testna$namembnost, predicted)


### BAGGING FUNKCIJA
# vsakic bo drugacen rezultat - ker nakljucno izbiramo primere
library(ipred)
bag <- bagging(namembnost ~ ., ucna, nbagg = 30)
predicted <- predict(bag, testna, type = "class")
# klasifikacijska točnost
CA(testna$namembnost, predicted)




### RANDOM FOREST
# Nakljucni gozd je inacica bagginga
# dodatno se izbiramo podmnozico atributov, ki nastopajo kot kandidati, ki jih obravnavamo, ko gradimo drevo
library(randomForest)
rf <- randomForest(namembnost ~ ., ucna)
predicted <- predict(rf, testna, type = "class")
# klasifikacijska točnost
CA(testna$namembnost, predicted)




### BOOSTING FUNKCIJAA
library(adabag)

bm <- boosting(namembnost ~ ., ucna, mfinal = 100)
predictions <- predict(bm, testna)
names(predictions)

predicted <- predictions$class

# klasifikacijska točnost
CA(testna$namembnost, predicted)