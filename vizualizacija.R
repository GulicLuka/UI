#Vnos podatkov ucne in testne mnozice
ucna <- read.table(file="ucnaSem1.txt", sep=",", header=TRUE)
testna <- read.table(file="testnaSem1.txt", sep=",", header=TRUE)

#faktoriziramo regijo in namembnost
ucna$regija <- as.factor(ucna$regija)
summary(ucna)

ucna$namembnost <- as.factor(ucna$namembnost)
summary(ucna)

##  1. NALOGA - VIZUALIZACIJA PODATKOV
#namembnost stavbe - njena povrsina
len <- length(table(ucna$namembnost))
headNamembnost <- c("izobrazevalna", "javno_storitvena", "kulturno_razvedrilna", "poslovna", "stanovanjska")

for (i in 1:len){
  sel[i] <- mean(ucna$povrsina[ucna$namembnost == headNamembnost[i]])
}

names(sel) <- c("izobrazevalna", "javno_storitvena", "kulturno_razvedrilna", "poslovna", "stanovanjska")
barplot(sel, ylab="povprecna povrsina", main="Povprecna povrsina glede na namembnost")


#namembnost stavbe - poraba energije
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
#kolicina padavin glede na raven pritiska
#namembnost stavbe - poraba energije - njena povrsina
#temperatura - poraba
#poraba energije glede na letne case / mesece