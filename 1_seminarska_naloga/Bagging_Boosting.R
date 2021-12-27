### BAGGING  ## KAKO DELUJE
# način za gradnjo tako da večkrat zgradimo odločitveno drevo. Izmed vseh n izberemo n vrstic z ponavljanjem. -> bootstrap (2/3 približno izberemo, 1/3 pa ne)
# vsako odločitveno drevo gradimo na drugačni učni množici

#koliko imamo vrstic
n <- nrow(ucna)
# m = koliko ucnih modelov bomo zgradili(dreves)
m <- 30

# shranjevali jih bomo v seznamu
models <- list()

#zgradi teh m dreves.
for (i in 1:m)
{
  # nakljucno izberemo n primerov z vracanjem
  # posamezno vrstico lahko izberemo veckrat. replace = T
  sel <- sample(1:n, n, replace = T)
  bootstrap <- ucna[sel, ]
  # minNodeWeightTree = 1 -> globoka drevesa
  models[[i]] <- CoreModel(Class ~ ., bootstrap, model = "tree", minNodeWeightTree = 1)
}

pred <- NULL
# sprehodimo se cez vse modele
for (i in 1:m) {
  # vzamemo i-ti model, testiramo ga na testni množici. Zanimajo nas razredi
  # prevedem v string in ga pripnem PREP-U kot stolpec
  pred <- cbind(pred, as.character(predict(models[[i]], test, type = "class")))
}

# Sesetejemo glasove
predClass <- voting(pred)
predicted <- factor(predClass, levels = levels(train$Class))

# klasifikacijska točnost
CA(testna$namembnost, predicted)






### BOOSTING ## KAKO DELUJE
# modele gradimo zaporedno - vsak nasledjni model bo bolj baziran na tistih težjih primerih, ki jih prejsni modeli niso znali pravilno napovedat  (pri baggingu neodvisno enega od drugega)
## IDEJA:
# Zaporedno gradimo modele
# Zaporedno ocenjujemo težavnost v učnem primeru
# vsak naslednji model se uči na tistih bolj teških primerih

models <- list()
n <- nrow(train)

# na zacetku imajo vsi primeri enako utez
# v prvi iteraciji imajo vsi enako verjetnost, da bodo izbrani
w <- rep(1 / n, n)

#zgradimo 100 primerov
m <- 100

# gremo cez vse modele
i <- 1
while (i <= m) {
  # nakljucno izberemo primere glede na utezi
  # prob=w -> upostevam utez ucnega primera. Za vsako vrstic odefinira verjetnost, da jo bomo izbrali
  # tezje primere bomo z vecjo verjetnostjo izbiral
  sel <- sample(1:n, n, prob = w, replace = T)

  # zgradimo model na podlagi izbranih primerov
  hyp <- CoreModel(namembnost ~ ., ucna[sel, ], model = "tree")

  # uporabimo model za klasifikacijo vseh primerov v ucni mnozici
  pred <- predict(hyp, ucna, type = "class")

  # kateri primeri so pravilno klasificirani
  correct <- pred == ucna$namembnost

  # napaka modela je vsota utezi napacno klasificiranih primerov
  err <- sum(w[!correct])

  # ce je napaka prevelika, ponovi iteracijo
  if (err > 0.5) {
    next
  }

  # stevilka med 0 in 1. Boljsi kot je model, nizja kot je napaka bo BETA nizja 0.
  beta <- err / (1 - err)

  # shranimo model in njegovo utez pri glasovanju (vecjo utez dobijo modeli z nizjo napako)
  models[[i]] <- list(model = hyp, quality = log(1 / beta))

  # znizamo utez pravilno klasificiranim primerom
  # ker je beta manjsa mnozimo z nizjo vrednostjo in bo zato utez manjsa
  w[correct] <- w[correct] * beta

  # normaliziramo utezi, da dobimo verjetnostjo porazdelitev
  w <- w / sum(w)

  print(paste("Zakljucil korak", i, "izmed", m))
  flush.console()

  # gremo na naslednji model
  i <- i + 1
}

# izpisimo utez pri glasovanju posameznih modelov
# visja kot je vrednost bolj je kvaliteten model
for (i in seq_along(models)) {
  print(models[[i]]$quality)
}


# pred glasovanjem pripravimo matriko (st. vrstic = st. testnih primerov, st. stolpcev = st. razredov)
predMat <- matrix(0, nrow = nrow(testna), ncol = nlevels(ucna$namembnost))
colnames(predMat) <- levels(ucna$namembnost)

# vsak model klasificira testne primere in glasuje za izbrane razrede v skladu s svojo kvaliteto
for (i in seq_along(models))
{
  pred <- as.character(predict(models[[i]]$model, testna, type = "class"))
  for (j in seq_along(pred)) {
    predMat[j, pred[j]] <- predMat[j, pred[j]] + models[[i]]$quality
  }
}

predClass <- colnames(predMat)[max.col(predMat)]
#vehicle$Class -> ucna in testna. ker on je pol to razdelil na TRAIN in TEST
predicted <- factor(predClass, levels(vehicle$Class))

# klasifikacijska točnost
CA(testna$namembnost, predicted)