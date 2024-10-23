######################### Dossier Econométrie spatiale #########################

##chargement package:
library(rgdal)
library(data.table)
library(readxl)
library(cartography)
library(raster)
library(RColorBrewer)
library(sp)
library(spdep)
library(spatialreg)
library(car)
library(dplyr)

##Création de la base de données  : 

#données projet 3 :
base <- read.csv2("C:/Users/Admi/Desktop/Dossier_econometrie_spatiale/Donnees_Projet3.csv")
colSums(is.na(base))

#prix m2 moyen:
DVF_Communes <- read.csv2('C:/Users/Admi/Desktop/Dossier_econometrie_spatiale/dvf2014.csv', sep=',')
DVF_gironde <- subset(DVF_Communes, grepl("^33", INSEE_COM))
colSums(is.na(DVF_gironde))

#revenu median:
revenu_median <- read_excel('C:/Users/Admi/Desktop/Dossier_econometrie_spatiale/revenu_median.xlsx')
revenu_median_gironde <- subset(revenu_median, grepl("^33", CODGEO))

#retraités:
retraite <- read_excel('C:/Users/Admi/Desktop/Dossier_econometrie_spatiale/retraite.xlsx')
retraite_gironde <- subset(retraite, grepl("^33", CODGEO))
retraite_gironde$age_retraite <- retraite_gironde$P14_POP6074+retraite_gironde$P14_POP7589+retraite_gironde$P14_POP90P

#Autres données:
df <- read_excel('C:/Users/Admi/Desktop/M2 ECAP/Econométrie spatiale/MDB-INSEE-V2.xls')
df_gironde <- subset(df, grepl("^33", CODGEO))
df_gironde$nb_entreprises <- df_gironde$`Nb Entreprises Secteur Commerce`+df_gironde$`Nb Entreprises Secteur Construction`+
  df_gironde$`Nb Entreprises Secteur Industrie`+df_gironde$`Nb Entreprises Secteur Services`

##Fusion base de données:
base$CODGEO <- as.character(base$CODGEO)
base$Rurale <- as.factor(base$Rurale)
base_csv <- merge(base, DVF_gironde[,c("INSEE_COM","Prixm2Moyen")], by.x = 'CODGEO', by.y='INSEE_COM', all.x=TRUE)

base_csv <- merge(base_csv, revenu_median_gironde[,c("CODGEO","MED14")], by.x = 'CODGEO', by.y='CODGEO', all.x=TRUE)

base_csv <- merge(base_csv, retraite_gironde[,c("CODGEO","C14_POP15P_CS7")], by.x = 'CODGEO', by.y='CODGEO', all.x=TRUE)

base_csv <- merge(base_csv, df_gironde[,c("CODGEO","Densité Médicale BV","Score équipement de santé BV","Nb Mineurs",
                                          "Nb Etudiants","nb_entreprises","Score Urbanité")], by.x='CODGEO', by.y='CODGEO', all.x = TRUE)

#Renommer les variables :
base_csv <- base_csv %>%
  rename(Revenu_median = MED14,
         Retraite = C14_POP15P_CS7,
         Densité_médicale_BV = `Densité Médicale BV`,
         Score_équipement_santé = `Score équipement de santé BV`,
         Nb_mineurs = `Nb Mineurs`,
         Nb_etudiants = `Nb Etudiants`,
         Score_urbanité = `Score Urbanité`)

#vérification des valeurs manquantes:
colSums(is.na(base_csv))
39/542
#7% de valeurs manquantes pur Prixm2Moyen
13/542
#2,% de valeurs manquantes pour Revenu_ median
2/542
#0.4% de valeurs manquantes pour Retraite

#imputation des valeurs manquantes :
str(base_csv)
base_csv$Prixm2Moyen <- as.numeric(base_csv$Prixm2Moyen)
library(missForest)
base_csv_sans_na <- missForest(base_csv[2:13])
colSums(is.na(base_csv_sans_na$ximp))
base_gironde <- cbind(base_csv[,1],base_csv_sans_na$ximp)
colSums(is.na(base_gironde))

base_gironde <- base_gironde %>%
  rename(CODGEO = `base_csv[, 1]`)

#création du csv :
write.csv(base_gironde, "C:/Users/Admi/Desktop/Dossier_econometrie_spatiale/base_gironde.csv", row.names = FALSE)


#-------------------------------------------------------------------------------

#Chargment fichier shp:
shp_gironde <- readOGR("commune_Gironde.shp")
str(shp_gironde@data)
colSums(is.na(shp_gironde@data))

#Chargement base_gironde :
base_gironde <- read.csv2('C:/M2 ECAP/Econométrie spatiale/Dossier/base_gironde.csv', sep=',')
str(base_gironde)
base_gironde <- base_gironde %>%
  mutate_all(as.numeric)
base_gironde$Rurale <- as.factor(base_gironde$Rurale)
base_gironde$CODGEO <- as.character(base_gironde$CODGEO)
base_gironde$Score_équipement_santé <- as.factor(base_gironde$Score_équipement_santé)
table(base_gironde$Score_équipement_santé)

#fusion base_gironde et shp :
shp_gironde<- merge(shp_gironde, base_gironde, by.x = 'INSEE_COM', by.y='CODGEO', all.x=TRUE)
View(shp_gironde@data)


##Analyse des corrélation :
var_quanti <- base_gironde[,c(2,3,5,6,7,8,10:13)]

correlation <- cor(var_quanti, method="spearman")
correlation
library(corrplot)
corrplot(correlation)

corrplot(correlation, method = 'number', type = "upper", tl.cex = 0.7, addCoef.col = "black")

#représentation sous forme d'histogramme et de boxplot la part des actifs salariés:
hist(var_quanti$Part_actif_salaries, main="Répartition de la part des actifs salariés des commune en 2014",
     xlab="Part des actifs salariés", ylab="Fréquence")
boxplot(var_quanti$Part_actif_salaries, main="Part des actifs salariés")

library(EnvStats)
rosnerTest(shp_gironde@data$Part_actif_salaries, k = 10, alpha = 0.05)
#pas d'outliers

#Histogramme et boxplot population:
hist(var_quanti$Population, main="Répartition de la population des communes en 2014", xlab="Population", ylab="Fréquence")
boxplot(var_quanti$Population, main="Population")

#Histogramme et boxplot Prixm2Moyen:
hist(var_quanti$Prixm2Moyen, main="Répartition du prix du m² moyen en 2014", xlab="Prix du m² moyen", ylab="Fréquence")
boxplot(var_quanti$Prixm2Moyen, main="Prix du m² moyen")

#Histogramme et boxplot Revenu Median:
hist(var_quanti$Revenu_median, main="Répartition du revenu médian des communes en 2014", xlab="Revenu Median", ylab="Fréquence")
boxplot(var_quanti$Revenu_median, main="Revenu Médian")

#Histogramme et boxplot Retraite:
hist(var_quanti$Retraite, main="Répartition du nombre de retraités en 2014", xlab="Nombre de retraités", ylab="Fréquence")
boxplot(var_quanti$Retraite, main="Retraité")

#Histogramme et boxplot Densité médicale:
hist(var_quanti$Densité_médicale_BV, main="Répartition de la densité médicale en 2014", xlab="Densité médicale", ylab="Fréquence")
boxplot(var_quanti$Densité_médicale_BV, main="Densité médicale")

#Histogramme et boxplot Nb mineurs:
hist(var_quanti$Nb_mineurs, main="Répartition du nombre de mineurs", xlab="Nb mineurs", ylab="Fréquence")
boxplot(var_quanti$Nb_mineurs, main="Nb mineurs")

#Histogramme et boxplot Nb étudiants:
hist(var_quanti$Nb_etudiants, main="Répartition du nombre d'étudiants", xlab="Nb étudiants", ylab="Fréquence")
boxplot(var_quanti$Nb_etudiants, main="Nb étudiants")

#Histogramme et boxplot nb entreprises:
hist(var_quanti$nb_entreprises, main="Répartition du nombre d'entreprises", xlab="Nb entreprises", ylab="Fréquence")
boxplot(var_quanti$nb_entreprises, main="Nb entreprises")

#Histogramme et boxplot Score urbanité:
hist(var_quanti$Score_urbanité, main="Répartition du score d'urbanité", xlab="Score urbanité", ylab="Fréquence")
boxplot(var_quanti$Score_urbanité, main="Score d'urbanité")


#Vérification du référentiel utilisé :
crs(shp_gironde)

#-------------------------------------------------------------------------------

#Représentation cartographique de la part des actifs salariés
choroLayer(spdf=shp_gironde, dfid = "INSEE_COM",var = "Part_actif_salaries", method="quantile",
           nclass = 6,col = carto.pal(pal1 = "red.pal", n1 = 6),legend.pos="bottomleftextra",
           legend.title.txt = "Legende")
layoutLayer(title = "Part des actifs salariés en 2014 - Gironde, méthode quantile",
            author = "Yohann, Ida & Souleymane", sources = "Données 2014",scale = NULL, north = FALSE,frame = TRUE, tabtitle = TRUE)

#Représentation cartographique de la Population
choroLayer(spdf=shp_gironde, dfid = "INSEE_COM",var = "Population", method="quantile",
           nclass = 6,col = carto.pal(pal1 = "red.pal", n1 = 6),legend.pos="bottomleftextra",
           legend.title.txt = "Legende")
layoutLayer(title = "Population en 2014 - Gironde, méthode quantile",
            author = "Yohann, Ida & Souleymane", sources = "Données 2014",scale = NULL, north = FALSE,frame = TRUE, tabtitle = TRUE)

#Représentation cartographique de la part des actifs salariés
?choroLayer
choroLayer(spdf=shp_gironde, dfid = "INSEE_COM",var = "Rurale", method="equal",
           nclass = 2,col = c("red", "blue"),legend.pos="bottomleftextra",
           legend.title.txt = "Legende")
layoutLayer(title = "Ruralité des communes en 2014 - Gironde, méthode quantile",
            author = "", sources = "",scale = NULL, north = FALSE,frame = TRUE, tabtitle = TRUE)

#Représentation cartographique de la Prix m² moyen:
choroLayer(spdf=shp_gironde, dfid = "INSEE_COM",var = "Prixm2Moyen", method="quantile",
           nclass = 6,col = carto.pal(pal1 = "red.pal", n1 = 6),legend.pos="bottomleftextra",
           legend.title.txt = "Legende")
layoutLayer(title = "Prix m² moyen en 2014 - Gironde, méthode quantile",
            author = "Yohann, Ida & Souleymane", sources = "Données 2014",scale = NULL, north = FALSE,frame = TRUE, tabtitle = TRUE)

#Représentation cartographique de la Revenu median:
choroLayer(spdf=shp_gironde, dfid = "INSEE_COM",var = "Revenu_median", method="quantile",
           nclass = 6,col = carto.pal(pal1 = "red.pal", n1 = 6),legend.pos="bottomleftextra",
           legend.title.txt = "Legende")
layoutLayer(title = "Revenu médian en 2014 - Gironde, méthode quantile",
            author = "Yohann, Ida & Souleymane", sources = "Données 2014",scale = NULL, north = FALSE,frame = TRUE, tabtitle = TRUE)

#Représentation cartographique des Retraités:
choroLayer(spdf=shp_gironde, dfid = "INSEE_COM",var = "Retraite", method="quantile",
           nclass = 6,col = carto.pal(pal1 = "red.pal", n1 = 6),legend.pos="bottomleftextra",
           legend.title.txt = "Legende")
layoutLayer(title = "Retraité en 2014 - Gironde, méthode quantile",
            author = "", sources = "",scale = NULL, north = FALSE,frame = TRUE, tabtitle = TRUE)

#Représentation cartographique de la Densité médicale
choroLayer(spdf=shp_gironde, dfid = "INSEE_COM",var = "Densité_médicale_BV", method="quantile",
           nclass = 4,col = carto.pal(pal1 = "red.pal", n1 = 4),legend.pos="bottomleftextra",
           legend.title.txt = "Legende")
layoutLayer(title = "Densité médicale en 2014 - Gironde, méthode quantile",
            author = "", sources = "",scale = NULL, north = FALSE,frame = TRUE, tabtitle = TRUE)

#Représentation cartographique du score équipement de santé :
choroLayer(spdf=shp_gironde, dfid = "INSEE_COM",var = "score_équipement_santé", method="quantile",
           nclass = 4,col = carto.pal(pal1 = "red.pal", n1 = 4),legend.pos="bottomleftextra",
           legend.title.txt = "Legende")
layoutLayer(title = "Score des équipemenents de santé en 2014 - Gironde, méthode quantile",
            author = "", sources = "",scale = NULL, north = FALSE,frame = TRUE, tabtitle = TRUE)

#Représentation cartographique du nombre de mineurs:
choroLayer(spdf=shp_gironde, dfid = "INSEE_COM",var = "Nb_mineurs", method="quantile",
           nclass = 4,col = carto.pal(pal1 = "red.pal", n1 = 4),legend.pos="bottomleftextra",
           legend.title.txt = "Legende")
layoutLayer(title = "Nb mineurs en 2014 - Gironde, méthode quantile",
            author = "", sources = "",scale = NULL, north = FALSE,frame = TRUE, tabtitle = TRUE)

#Représentation cartographique du nombre d'étudiants
choroLayer(spdf=shp_gironde, dfid = "INSEE_COM",var = "Nb_etudiants", method="quantile",
           nclass = 4,col = carto.pal(pal1 = "red.pal", n1 = 4),legend.pos="bottomleftextra",
           legend.title.txt = "Legende")
layoutLayer(title = "Nb étudiants en 2014 - Gironde, méthode quantile",
            author = "", sources = "",scale = NULL, north = FALSE,frame = TRUE, tabtitle = TRUE)

#Représentation cartographique du nombre d'entreprises:
choroLayer(spdf=shp_gironde, dfid = "INSEE_COM",var = "nb_entreprises", method="quantile",
           nclass = 4,col = carto.pal(pal1 = "red.pal", n1 = 4),legend.pos="bottomleftextra",
           legend.title.txt = "Legende")
layoutLayer(title = "Nb entreprises en 2014 - Gironde, méthode quantile",
            author = "", sources = "",scale = NULL, north = FALSE,frame = TRUE, tabtitle = TRUE)

#Représentation cartographique du score d'urbanité
choroLayer(spdf=shp_gironde, dfid = "INSEE_COM",var = "Score_urbanité", method="quantile",
           nclass = 8,col = carto.pal(pal1 = "red.pal", n1 = 8),legend.pos="bottomleftextra",
           legend.title.txt = "Legende")
layoutLayer(title = "Score d'urbanité en 2014 - Gironde, méthode quantile",
            author = "", sources = "",scale = NULL, north = FALSE,frame = TRUE, tabtitle = TRUE)

#-------------------------------------------------------------------------------
#Définition des critère de contiguité d'ordre 1:

#Type reine:
nb_queen <- poly2nb(shp_gironde, row.names=shp_gironde$INSEE_COM, queen = TRUE)
WQueen<-nb2listw(nb_queen,style="W",zero.policy=TRUE)
plot(shp_gironde, border = 'lightgrey')
plot(nb_queen, coordinates(shp_gironde), add=TRUE, col='red')

#Type Tour:
nb_Tour <- poly2nb(shp_gironde, row.names=shp_gironde$INSEE_COM, queen = FALSE )
WTour<-nb2listw(nb_Tour,style="W",zero.policy=TRUE)
plot(shp_gironde, border = 'lightgrey')
plot(nb_Tour, coordinates(shp_gironde), add=TRUE, col='red')

#1 Proche voisin:
coords <- coordinates(shp_gironde)
k1 <- knn2nb(knearneigh(coords,k=1))
PPV1<- nb2listw(k1,style="W",zero.policy=TRUE)
plot(shp_gironde, border = 'lightgrey')
plot(k1, coordinates(shp_gironde), add=TRUE, col='orange')

#3 proche voisin:
k3 <- knn2nb(knearneigh(coords,k=3))
PPV3<- nb2listw(k3,style="W",zero.policy=TRUE)
plot(shp_gironde, border = 'lightgrey')
plot(k3, coordinates(shp_gironde), add=TRUE, col='blue')


#Calcul de l'indice de Moran pour les trois matrices de poids:

#WQueen:
globalMoran <- moran.test(shp_gironde$Part_actif_salaries,WQueen, zero.policy=TRUE,randomisation=TRUE)
globalMoran

set.seed(1234)
Moranperm = moran.mc(shp_gironde$Part_actif_salaries, WQueen, nsim = 999, zero.policy = TRUE)
Moranperm

#WTour:
globalMoranT <- moran.test(shp_gironde$Part_actif_salaries,WTour, zero.policy=TRUE,randomisation=TRUE)
globalMoranT

set.seed(1234)
MoranpermT = moran.mc(shp_gironde$Part_actif_salaries, WTour, nsim = 999, zero.policy = TRUE)
MoranpermT

#PPV1:
globalMoran2 <- moran.test(shp_gironde$Part_actif_salaries,PPV1, zero.policy=TRUE,randomisation=TRUE)
globalMoran2

set.seed(1234)
Moranperm2 = moran.mc(shp_gironde$Part_actif_salaries, PPV1, nsim = 999, zero.policy = TRUE)
Moranperm2

#PPV3
globalMoran3 <- moran.test(shp_gironde$Part_actif_salaries,PPV3, zero.policy=TRUE,randomisation=TRUE)
globalMoran3

set.seed(1234)
Moranperm3 = moran.mc(shp_gironde$Part_actif_salaries, PPV3, nsim = 999, zero.policy = TRUE)
Moranperm3

#Réalisation du diagramme de Moran pour les 3 matrices de poids:

#WQueen
shp_gironde$Part_actif_salaries_std <- scale(shp_gironde$Part_actif_salaries)
mp <- moran.plot(as.vector(shp_gironde$Part_actif_salaries_std), WQueen, xlab="Part des actifs salariés en 2014 -
Gironde", ylab="Lag Part des actifs salariés en 2014 - Gironde",
                 main="Matrice type Reine",labels=as.character(shp_gironde$NOM_COM))

#WTour
mpT <- moran.plot(as.vector(shp_gironde$Part_actif_salaries_std), WTour, xlab="Part des actifs salariés en 2014 -
Gironde", ylab="Lag Part des actifs salariés en 2014 - Gironde",
                  main="Matrice type Tour",labels=as.character(shp_gironde$NOM_COM))

#PPV1
mp2 <- moran.plot(as.vector(shp_gironde$Part_actif_salaries_std), PPV1, xlab="Part des actifs salariés en 2014 -
Gironde", ylab="Lag Part des actifs salariés en 2014 - Gironde",
                  main="Matrice - PPV1",labels=as.character(shp_gironde$NOM_COM))

#PPV3
mp3 <- moran.plot(as.vector(shp_gironde$Part_actif_salaries_std), PPV3, xlab="Part des actifs salariés en 2014 - Gironde",
                  ylab="Lag Part des actifs salariés en 2014 - Gironde",
                  main="Matrice - PPV3",labels=as.character(shp_gironde$NOM_COM))

#-------------------------------------------------------------------------------
#MCO :
shp_gironde$Score_équipement_santé <- as.factor(shp_gironde$Score_équipement_santé)
fit1 <- lm(Part_actif_salaries ~ Population+Rurale+Revenu_median+Densité_médicale_BV, data=shp_gironde)
summary(fit1)

vif(fit1)

#calcul de l'indice de Moran
#WQueen
moran.lm<-lm.morantest(fit1, WQueen, alternative="two.sided")
print(moran.lm)

#WTour
moran.lmt<-lm.morantest(fit1, WTour, alternative="two.sided")
print(moran.lmt)

#PPV1:
moran.lm2<-lm.morantest(fit1, PPV1, alternative="two.sided")
print(moran.lm2)

#PPV3:
moran.lm3<-lm.morantest(fit1, PPV3, alternative="two.sided")
print(moran.lm3)

#-------------------------------------------------------------------------------
#Test de Lagrange pour déterminer le modèle à estimer

#Queen:
LM<-lm.LMtests(fit1, WQueen, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
print(LM)

#Tour:
LMT<-lm.LMtests(fit1, WTour, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
print(LMT)

#PPV1:
LM2<-lm.LMtests(fit1, PPV1, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
print(LM2)

#PPV3:
LM3<-lm.LMtests(fit1, PPV3, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
print(LM3)

#dans tous les cas on choisit un modèle SAR

#-------------------------------------------------------------------------------
#Modélisation de modèles SAR pour chaque matrice de poids:

#WQueen:
library(spatialreg)
sar<-lagsarlm(Part_actif_salaries ~ Population+Rurale+Revenu_median+Densité_médicale_BV, data=shp_gironde, WQueen)
summary(sar)
#AIC: 3517
impacts.sar<-impacts(sar, listw=WQueen)
impacts.sar

#WTour:
sarT<-lagsarlm(Part_actif_salaries ~ Population+Rurale+Revenu_median+Densité_médicale_BV, data=shp_gironde, WTour)
summary(sarT)
#AIC: 3517.5
impacts.sarT<-impacts(sarT, listw=WTour)
impacts.sarT

#PPV1:
sar2<-lagsarlm(Part_actif_salaries ~ Population+Rurale+Revenu_median+Densité_médicale_BV, data=shp_gironde, PPV1)
summary(sar2)
#3640.9
impacts.sar2<-impacts(sar2, listw=PPV1)
impacts.sar2

#PPV3:
sar3<-lagsarlm(Part_actif_salaries ~ Population+Rurale+Revenu_median+Densité_médicale_BV, data=shp_gironde, PPV3)
summary(sar3)
#AIC : 3554.1
impacts.sar3<-impacts(sar3, listw=PPV3)
impacts.sar3

#Selon les tests du multiplicateur de Lagrange, nous obtenons au moins un modèle significatifs, on compare donc les modèle SAR avec les SDM.

#-------------------------------------------------------------------------------
#Comparaison modèle SAR et SDM

#WQueen:
sdm<-lagsarlm(Part_actif_salaries ~ Population+Rurale+Revenu_median+Densité_médicale_BV, data=shp_gironde, WQueen, type="mixed")
summary(sdm)
#AIC:3503.6
impacts.sdm<-impacts(sdm, listw=WQueen)
impacts.sdm

TestSDM_SAR<-LR.Sarlm(sdm,sar)
print(TestSDM_SAR)
#SDM

#WTour:
sdmt<-lagsarlm(Part_actif_salaries ~ Population+Rurale+Revenu_median+Densité_médicale_BV, data=shp_gironde, WTour, type="mixed")
summary(sdmt)
#3504.6
impacts.sdmt<-impacts(sdmt, listw=WTour)
impacts.sdmt

TestSDMT_SART<-LR.Sarlm(sdmt,sarT)
print(TestSDMT_SART)
#SDM

#PPV1:
sdm2<-lagsarlm(Part_actif_salaries ~ Population+Rurale+Revenu_median+Densité_médicale_BV, data=shp_gironde, PPV1, type="mixed")
summary(sdm2)
#3641.4
impacts.sdm2<-impacts(sdm2, listw=PPV1)
impacts.sdm2

TestSDM2_SAR2<-LR.Sarlm(sdm2,sar2)
print(TestSDM2_SAR2)
#Pas de différence
#PPV3:
sdm3<-lagsarlm(Part_actif_salaries ~ Population+Rurale+Revenu_median+Densité_médicale_BV, data=shp_gironde, PPV3, type="mixed")
summary(sdm3)
#AIC : 3550.9
impacts.sdm3<-impacts(sdm3, listw=PPV3)
impacts.sdm3

TestSDM3_SAR3<-LR.Sarlm(sdm3,sar3)
print(TestSDM3_SAR3)
#SDM