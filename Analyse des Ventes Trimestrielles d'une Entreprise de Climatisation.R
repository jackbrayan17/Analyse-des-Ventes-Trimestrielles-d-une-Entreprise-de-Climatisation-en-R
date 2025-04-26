library(dplyr)

#a - creer un tableau de donnees en R :
donnees <- data.frame (
  annee = rep(2023:2024, each = 4),
  trimestre = rep(1:4, times = 2),
  ventes = c(20,30,60,25,22,33,66,27)
)
head(donnees)

#b - creer une variable temps: 
#'$' nous permet de creer une colonne 'time' dans le jeu de donnees 'donnees' 
donnees$time <- seq.Date(from = as.Date("2023-01-01"), by = "quarter", length.out = nrow(donnees))
head(donnees)

#c - creer une serie temporelle trimestrielle
#Ici on cree un serie ts qui commence du 1 Jan 2022
serie_ts <- ts(donnees$ventes, start = c(2023,1), frequency = 4)
head(serie_ts)

#d - visualiser la serie et detecter la saisonalite
plot(serie_ts, main="Series trimestrielle ventes", col="blue")
plot(serie_ts, main="Series trimestrielle ventes",ylab="Ventes (en millier)" ,col="red")

#ETAPE 4 : ESTIMER LA TENDANCE PAR REGRESSION
#a - creation d'une variable temps numerique (1,2,3....12)
temps <- 1:length(serie_ts)

#b - regression lineaire : ventes-temps
modele_tendance <- lm(serie_ts ~ temps)

#c - Resume du modele
summary(modele_tendance)
#Pour avoir les valeurs de Beta-1 et Beta-0, Beta-0 sur la colonne 'Estimate' 
#donc Beta-0(a) = 27.607 et Beta-1(b) = 1.726

#Pour donner la valeur de la 5eme periode si on a la valeur de B1 et B0 on devra prendre la valeur de la 5eme periode
#at + b + E -> B0(t) + B1 + E(5eme Periode)

#d - Tracer la serie avec la tendance 
plot(serie_ts , main="Serie avec tendance" , ylab = "Ventes" , xlab = "Temps" , col = "blue")
abline(modele_tendance , col="blue" , lwd=2)

#e - visualiser la tendance
plot(serie_ts, main="Serie avec tendance", ylab="Ventes",col="darkgray")
abline(modele_tendance , col = "red" , lwd=2)

#f - residus
residus <- residuals(modele_tendance)
residus

#g - visualiser les residus
plot(residus, type = "o" , col = "purple" , main = "Residus de la tendance", ylab = "Residus" , xlab = "Trimestre")
abline(h=0 , lty = 2 , col = "gray")

#ETAPE 5 : EXTRAIRE LES COMPOSANTES (tendance & saison)
#a - decomposition additive
decomp <- decompose(serie_ts , type = "additive")
saisonnalite <- decomp$seasonal
#b - visualisaton de la decomposition
plot(saisonnalite, main = "Composante saisoniere ")

#ETAPE 6 : DESAISONALISATION
#a - serie desaisonalisee
serie_desaisonalisee <- serie_ts - saisonnalite

#b - tracer la serie desaisonalisee
plot(serie_desaisonalisee, type = "o" ,main = "Serie desaisonalisee", col = "green", ylab = "Ventes")

#c - visualisation
head(serie_desaisonalisee)


#ETAPE 7 : RECONSTITUER LES VALEURS ATTENDUES
#a - reconstituer les valeurs attendues la ou la tendance est non nulle
valeurs_attendues <- decomp$trend + decomp$seasonal

#b - creer un vecteur logique pour reperer les valeurs non manquantes
valide <- !is.na(valeurs_attendues)

#c - tracer la serie observee (complete)
plot(serie_ts , type = "o" ,
     col = "blue" , 
     main = "Serie observee vvs attendues" ,
     ylab = "Ventes" , 
     xlab = "Temps")

#d - ajouter la serie attendue uniquement la ou elle est definie
lines(time(serie_ts)[valide], valeurs_attendues[valide], type = "o" , col = "red")

#e - legende
legend("topleft", legend = c("Observe","Reconstitue"), col= c("blue","red"), lty = 1)

#f - visualisation
head(valeurs_attendues)

#ETAPE 8 : LISSER AVEC LES MOYENNES MOBILES
mm <- rollmean(serie_ts, k = 4 , align = "center", fill = NA)
lines(mm, col = "orange", lwd = 2)


#ETAPE 9 : PREVOIR T1 & T2
temps_futur <- data.frame(temps = 9:10)
previsions_tendance <- predict(modele_tendance, newdata = temps_futur)

#a - Ajouter les composantes
coef_sais <- saisonnalite
#coef_sais <- donnees$trimestre
coef_sais
saisons <- tapply(saisonnalite, coef_sais, mean,  na.rm = TRUE)
sais_T1 <- saisons[1]
sais_T2 <- saisons[2]

#b - Les previsions
previsions_T1_2025 <- previsions_tendance[1] + sais_T1
previsions_T2_2025 <- previsions_tendance[2] + sais_T2
previsions_T1_2025
previsions_T2_2025

#ETAPE 10 : VISUALISATION DES PREVISIONS
ts_future <- ts(c(serie_ts, previsions_T1_2025, previsions_T2_2025), start = c(2023,1), frequency = 4)
plot(ts_future, type = "o", col = "blue" , main = "Serie + Previsions T1/T2 2025")
abline(v = c(2025), col = "red" , lty=2)
