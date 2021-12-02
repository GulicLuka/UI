#Vnos podatkov ucne in testne mnozice
ucna <- read.table(file="ucnaSem1.txt", sep=",", header=TRUE)
testna <- read.table(file="testnaSem1.txt", sep=",", header=TRUE)

#faktoriziramo regijo in namembnost
ucna$regija <- as.factor(ucna$regija)
summary(ucna)

ucna$namembnost <- as.factor(ucna$namembnost)
summary(ucna)

ucna$oblacnost <- as.factor(ucna$oblacnost)
summary(ucna)

#ucna$datum <- as.factor(ucna$datum)
#summary(ucna)



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



##2. NALOGA   OCENJEVANJE IN KONSTRUKCIJA ATRIBUTOV
#dodajanje atributov
install.packages("data.table")
library("data.table")
dt = as.data.table(ucna)
ymd <- c(ucna$datum)
ucna <- dt[, c("YYYY", "MM", "DD") := tstrsplit(ymd, "-", fixed=TRUE)]
head(ucna)
summary(ucna)

#KONSTRUKCIJA ATRIBUTOV

#dodan atribut vikend - vrne true ali false glede na to ali je bil podani datum vikend ali ne
install.packages("chron")
library("chron")
ucna$vikend <- c(is.weekend(ucna$datum))


#razvrstitev glede na letne case
WS <- as.Date("2016-12-21", format = "%Y-%m-%d") # Winter Solstice
PO <- as.Date("2016-03-20",  format = "%Y-%m-%d") # Spring Equinox
SU <- as.Date("2016-06-20",  format = "%Y-%m-%d") # Summer Solstice
JE <- as.Date("2016-09-22",  format = "%Y-%m-%d") # Fall Equinox
for (i in 1:nrow(ucna))
{
  d <- as.Date(strftime(ucna$datum[i], format="2016-%m-%d"))
  ifelse (d >= WS | d < PO, ucna$letnicas[i] <- "Zima",
          ifelse (d >= PO & d < SU, ucna$letnicas[i] <- "Pomlad",
                  ifelse (d >= SU & d < JE, ucna$letnicas[i] <- "Poletje", ucna$letnicas[i] <- "Jesen")))
}

ucna$letnicas <- as.factor(ucna$letnicas)
summary(ucna)

#Smeri neba
#for (i in 1:nrow(ucna)) 
#{
#  if (ucna$smer_vetra[i] == 0){
#    ucna$smer[i] <- "Brezveterje"
#  }else {
#    cifra <- floor((ucna$smer_vetra[i] / 45 ) + 0.5)
#    konc <- cifra %% 8
#    ifelse (konc == 0, ucna$smer[i] <- "S",
#            ifelse (konc == 1,ucna$smer[i] <- "SV",
#                    ifelse (konc == 2,ucna$smer[i] <- "V",
#                            ifelse (konc == 3,ucna$smer[i] <- "JV",
#                                    ifelse (konc == 4,ucna$smer[i] <- "J",
#                                            ifelse (konc == 5,ucna$smer[i] <- "JZ",
#                                                    ifelse (konc == 6,ucna$smer[i] <- "Z",
#                                                            ifelse (konc == 7,ucna$smer[i] <- "SZ"))))))))
#  }
#}
#ucna$smer <- as.factor(ucna$smer)
#summary(ucna)
# poraba predhodnji mesec in teden
# poraba

# poraba

library(stringr)
months <- str_pad(1:99, pad = 0,width = 2 , "left")

mn <- min(ucna$stavba)
mx <- max(ucna$stavba)

povpPorabaMesec <- c()

for(m in 1:12) 
{
  for(i in mn:mx)
  {
    povpPorabaMesec[i] <- mean(ucna$poraba[ucna$stavba == i & ucna$MM == months[m]])
    
    if (m == 12)
    {
      ucna$povp_poraba_mesec_predhodni[ucna$stavba == i & ucna$MM == months[1]] <- povpPorabaMesec[i]
    }
    else
    {
      ucna$povp_poraba_mesec_predhodni[ucna$stavba == i & ucna$MM == months[m+1]] <- povpPorabaMesec[i]
    }
  }
}

head(ucna)
ucna


#teden povp poraba
install.packages("lubridate")
library("lubridate")
wknum <- lubridate::week(ymd(ucna$datum)) 
ucna$week_num <- wknum


mn <- min(ucna$stavba)
mx <- max(ucna$stavba)
povpPorabaTeden <- c()


for(t in 1:max(ucna$week_num))
{
  for(i in mn:mx)
  {
    povpPorabaTeden[i] <- mean(ucna$poraba[ucna$stavba == i & ucna$week_num == t])
    
    if (t == max(ucna$week_num))
    {
      ucna$povp_poraba_teden_predhodni[ucna$stavba == i & ucna$week_num == 1] <- povpPorabaTeden[i]
    }
    else
    {
      ucna$povp_poraba_teden_predhodni[ucna$stavba == i & ucna$week_num == t+1] <- povpPorabaTeden[i]
    }
  }
}

ucna$DD <- NULL
ucna$MM <- NULL
ucna$YYYY <- NULL
ucna$week_num <- NULL


#ocenjevanje
install.packages("CORElearn") 
library(CORElearn)

# INFGAIN pred dodanimi atributi
sort(attrEval(poraba ~ datum + regija + stavba + namembnost + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra, ucna, "InfGain"), decreasing = T)

# INFGAIN po dodanih atributih
sort(attrEval(poraba ~ ., ucna, "InfGain"), decreasing = T)

# GINI pred dodanimi atributi
sort(attrEval(poraba ~ datum + regija + stavba + namembnost + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra, ucna, "Gini"), decreasing = T)

# GINI po dodanih atributih
sort(attrEval(poraba ~ ., ucna, "Gini"), decreasing = T)

# GAINRATIO pred dodanimi atributi
sort(attrEval(poraba ~ datum + regija + stavba + namembnost + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra, ucna, "GainRatio"), decreasing = TRUE)

# GAINRATIO po dodanih atributih
sort(attrEval(poraba ~ ., ucna, "GainRatio"), decreasing = TRUE)

# RELIEFF pred dodanimi atributi
sort(attrEval(poraba ~ datum + regija + stavba + namembnost + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine + pritisk + smer_vetra + hitrost_vetra, ucna, "ReliefFequalK"), decreasing = TRUE)

# RELIEFF po dodanih atributih
sort(attrEval(poraba ~ ., ucna, "ReliefFequalK"), decreasing = TRUE)










