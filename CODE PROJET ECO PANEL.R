library(AER)
library(dplyr)
library(ggplot2)
library(wooldridge)
install.packages("psych")
library(psych)
library(dplyr)
install.packages("tidyverse")
install.packages("mosaic")
library(tidyverse)
library(plm)
library(mosaic)
library(corrplot)
library(reshape2)
install.packages("forecast")
library(forecast)
install.packages("lmtest")  # Pour le test RESET
library(lmtest)
rm(list=ls())
data("Grunfeld")

head(dat)
help("crime4")
dat <- dat %>% arrange(year)
data("crime4")
df <- crime4
attach(df)
df <- df %>% arrange(year)
df<- df%>% arrange(county)
summary(crime4)


columns_names <- colnames(df)
columns_names
#NETTOYAGE DATASET
str(df)

#Présence de NA dans plusieurs variables, on va les remplacer par la moyenne générale.
mean_clcrmrte <- mean(df$clcrmrte, na.rm = TRUE)
# Remplacer les NA par la moyenne
df$clcrmrte <- replace_na(df$clcrmrte, mean_clcrmrte)
# Vérifier que les NA ont été remplacés
summary(df)

df2 <- pdata.frame(df, index = c("county", "year"),
                   drop.index = TRUE, row.names = TRUE)
# Calculer la moyenne de prbarr
moyenne_prbconv <- mean(df$prbconv, na.rm = TRUE)

# Remplacer les valeurs supérieures à 1 par la moyenne
df$prbconv[df$prbconv > 1] <- moyenne_prbconv

# Afficher le dataframe modifié
df
#STATISTIQUES DESCRIPTIVES
summary(df)
head(df)
mean(df$crmrte)
max(df$crmrte)
median(crmrte)
quantile(crmrte)
median(prbarr)
mean(prbconv)
mean(prbpris)
median(prbconv)
summary(df$avgsen)
summary(df$prbconv)
plot(prbarr)
summary(density)
summary(avgsen)
#Par Année
df4 <- df %>% filter(df$year == 86)
mean(df4$crmrte)

#Matrice de corrélation
df3 <- df[,c("crmrte", "prbarr", "prbconv", "prbpris", "avgsen",
             "polpc", "density", "taxpc","urban", "wcon", "wtuc")]
cor_mat <-cor(df3)
ggplot(data = melt(cor_mat), aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6),  # Adjust 'angle' parameter
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 6)) +  # Vertical orientation
  coord_fixed()


#Description des variables par catégories de taux de crime.
#On divise les taux de crimes en 5 :
df <- mutate(df, crmrte_cat = crmrte)

seuil_1 <- 0.015
seuil_2 <- 0.030
seuil_3 <- 0.050
seuil_4 <- 0.075

df <- mutate(df,
             crmrte_cat = case_when(
               crmrte <= seuil_1 ~ "Très Petit Taux",
               crmrte <= seuil_2 & crmrte > seuil_1 ~ "Petit Taux",
               crmrte <= seuil_3 & crmrte > seuil_2 ~ "Taux Moyen",
               crmrte <= seuil_4 & crmrte > seuil_3 ~ "Haut Taux",
               crmrte > seuil_4 ~ "Très Haut Taux",
               TRUE ~ NA_character_
             )
)

#Compte le nombre d'occurence de chacune des variables crmrte_cat en fonction des années
table_crim_rate<- table(df$year, df$crmrte_cat)
table_crim_rate
#Compte le nombre d'occurence de chacune des variables
table(df$crmrte_cat)
summary(crmrte_cat)


df$crmrte_cat <- factor(df$crmrte_cat, levels = c("Très Petit Taux", "Petit Taux", "Taux Moyen", "Haut Taux", "Très Haut Taux"))
ggplot(df, aes(x = crmrte_cat)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution des catégories de taux de crime",
       x = "Catégorie de taux de crime",
       y = "Fréquence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

crime_des <-describeBy(df, group=df$crmrte_cat)
crime_des
#Boxplot raté
crim_per_year <- boxplot(crmrte, col = c("red"), main = "Taux de crime par annnée", ylab = year)

#Taux de crime par année
plot(crmrte, year)
plot(prbarr,year)

columns_names
#GRAPHIQUE DE CRIME PAR RAPPORT A LA DENSITE AVEC EN ROUGE LES PTS QUI CONRESPONDENT AU TX DE CRIME SUPERIEUX A LA MOYENNE ANNUEL RESPECTIVE

#Taux de crime par rapport à la densité 
plot(lcrmrte,ldensity)
plot(crmrte,lpctmin)
plot(crmrte,ltaxpc)
plot(crmrte, lprbconv)
plot(crmrte,year)
plot(crmrte, county)
plot(crmrte, lpolpc)
plot(crmrte, avgsen)
# Scatter plot entre la variable dépendante (Y) et la variable indépendante (X)
plot(df$lcrmrte,df$lprbarr, main = "log(Crim) VS log(Density)", xlab = "log Taux de criminalité", ylab = "log Densité")

# Calculer la moyenne de crmrte pour chaque année
mean_crmrte_by_year <- aggregate(crmrte ~ year, data = df, FUN = mean)
# Calculer la moyenne de crmrte sur toutes les années
overall_mean_crmrte <- mean(df$crmrte)
# Identifier les points où crmrte est supérieur à la moyenne de crmrte pour cette année
points_above_mean <- df$crmrte > mean_crmrte_by_year$crmrte[match(df$year, mean_crmrte_by_year$year)]
# Créer un graphique avec plot() pour crmrte vs. density
plot(df$crmrte, df$density)
# Ajouter les points en rouge
points(df$crmrte[points_above_mean], df$density[points_above_mean], col = "red")





#Résultats des estimations et commentaires

#Méthode pooling
#Ne prend pas en compte les variations individuelles et temporelles
# La régression pooling est sans effets fixes ou aléatoires, ce qui signifie que l'intercept (L'ordonée à l'origine de la droite de regression)
#et la pente seront estimés sans prendre en compte les différences entre les individus et entre les périodes.

pooledmethod =plm(crmrte ~ prbarr + prbconv + prbpris + avgsen + 
                     pctymle + density + taxpc + wfir + wmfg, data = df, model = "pooling")
pooledmethod2 =plm(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + 
                    lpctymle + ldensity + ltaxpc + lpctmin+ lwfir + lwmfg, data = df, model = "pooling")
summary(pooledmethod2)
""" 
Residuals:
      Min.    1st Qu.     Median    3rd Qu.       Max. 
-1.2391009 -0.1555330 -0.0060146  0.1422085  2.2011593 

Coefficients:
             Estimate Std. Error  t-value  Pr(>|t|)    
(Intercept) -4.837205   0.384606 -12.5771 < 2.2e-16 ***
lprbarr     -0.429026   0.034508 -12.4328 < 2.2e-16 ***
lprbconv    -0.320520   0.023662 -13.5457 < 2.2e-16 ***
lprbpris    -0.090554   0.058669  -1.5435  0.123226    
lavgsen     -0.045122   0.045326  -0.9955  0.319883    
lpctymle     0.065269   0.069649   0.9371  0.349062    
ldensity     0.313075   0.022572  13.8703 < 2.2e-16 ***
ltaxpc       0.120401   0.045876   2.6245  0.008893 ** 
lpctmin      0.219180   0.013592  16.1259 < 2.2e-16 ***
lwfir       -0.059901   0.050840  -1.1782  0.239162    
lwmfg       -0.011971   0.060482  -0.1979  0.843173    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    206.38
Residual Sum of Squares: 58.996
R-Squared:      0.71414
Adj. R-Squared: 0.70952
F-statistic: 154.638 on 10 and 619 DF, p-value: < 2.22e-16"""
bptest(pooledmethod)


#Poolability Test
pooltest(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + 
           lpctmin + lpctymle + ldensity + ltaxpc+ lwfir + lwmfg, data = df, model = "within")
"""Erreur dans FUN(X[[i]], ...) : insufficient number of observations
H0 : Les memes coeff s'appliquent à tous les individus 
Ha : Les coeff ne s'appliquent pas à tous les individus
L'objectif était de savoir si les coefficients s'appliquent à tous les individus,
si non, alors la pooled OLS est instable"""

#The Fixed Effects Models (Fixed Effect method)

#Le principe d'un modèle a effet fixes fixe est d'explorer la relation entre les variables explicatives et l'outcome
#dans notre base le taux de criminalité) pour les différents comptés de Caroline du Nord.
# Chaque individu ou agent (compté ici) a ses propres caracteristiques individuelles qui peuvent influencer les variables
# explicatives, ainsi en utilisant des données de panel avec effets fixes,
# on suppose en effet que quelque choses au sein de l' individu / propre a l' individu peut impacter les variables explicatives
#et la variables à expliquer,on doit donc controler pour cela!
# En d'autres termes, on assume que la corrélation entre les variables explicatives et l'erreur est differentes de 0.
# Les modeles à effets fixes permettent "d'enlever" ces effet individuels qui ne varient pas dans le temps 
#comme ça on peut estimer l'effet "net" des variable explicatives

femethod = plm(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + 
                 lpctymle + lpctmin + ldensity + ltaxpc + lwfir + lwmfg, data = df, model = "within", index = c("county","year"))
summary(femethod)
"""Balanced Panel: n = 90, T = 7, N = 630

Residuals:
      Min.    1st Qu.     Median    3rd Qu.       Max. 
-0.9730873 -0.0801824  0.0019242  0.0782787  1.0830007 

Coefficients:
            Estimate  Std. Error t-value  Pr(>|t|)    
lprbarr  -0.22996132  0.03754422 -6.1251 1.766e-09 ***
lprbconv -0.13712982  0.02210232 -6.2043 1.107e-09 ***
lprbpris -0.13127050  0.03929014 -3.3411 0.0008933 ***
lavgsen   0.02347735  0.03071216  0.7644 0.4449495    
lpctymle  0.15842294  0.32376739  0.4893 0.6248236    
ldensity  0.83009236  0.34212302  2.4263 0.0155857 *  
ltaxpc    0.10967273  0.05239368  2.0932 0.0368013 *  
lwfir    -0.00036737  0.03518464 -0.0104 0.9916733    
lwmfg    -0.39054712  0.10463727 -3.7324 0.0002101 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    17.991
Residual Sum of Squares: 15.747
R-Squared:      0.12474
Adj. R-Squared: -0.036791
F-statistic: 8.40883 on 9 and 531 DF, p-value: 8.8132e-12"""
#On utilise la commande plm pour ajouter des effets individuels et de temps => within c'est à dire que c'est l'effet
#moyen pour la variable pourr chaque individu (pour chaque états)

#Within avec une dichotomiques du temps
femethod_twoway = plm(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + 
                 lpctymle + lpctmin+ ldensity + ltaxpc + lwfir + lwmfg, data = df, 
                 model = "within", 
                 index = c("county","year"),
                 effect ="twoways")
summary(femethod_twoway)
"""

Balanced Panel: n = 90, T = 7, N = 630

Residuals:
      Min.    1st Qu.     Median    3rd Qu.       Max. 
-0.8932743 -0.0672509  0.0022907  0.0656683  1.1101547 

Coefficients:
            Estimate  Std. Error t-value  Pr(>|t|)    
lprbarr  -0.19636047  0.03677438 -5.3396 1.389e-07 ***
lprbconv -0.11224543  0.02177575 -5.1546 3.605e-07 ***
lprbpris -0.09609942  0.03855339 -2.4926  0.012987 *  
lavgsen  -0.02314008  0.03151213 -0.7343  0.463080    
lpctymle  1.23500159  0.43537583  2.8366  0.004735 ** 
ldensity  0.73474312  0.34039934  2.1585  0.031344 *  
ltaxpc    0.04993141  0.05380792  0.9280  0.353857    
lwfir    -0.00037956  0.03419514 -0.0111  0.991148    
lwmfg    -0.57284595  0.13316109 -4.3019 2.020e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    16.123
Residual Sum of Squares: 14.36
R-Squared:      0.10938
Adj. R-Squared: -0.06705
F-statistic: 7.16398 on 9 and 525 DF, p-value: 7.8167e-10
"""

#Random Effect method
#random effect, l'effet individuel est une variable aléatoire qui n'est pas corrélé aux variables explicatives
# et permet d'estimer l'impact des variables qui ne bougent pas de temps.

remethod = plm(crmrte ~ prbarr + prbconv + prbpris + avgsen + 
                 polpc + density + taxpc + wfir +wmfg, data = df, model = "random", index=c("county","year"))

remethod2 = plm(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lpctymle + lpctmin +
                  ldensity + ltaxpc + lwfir + lwmfg, data = df, model = "random", index=c("county","year"))
summary(remethod2)
bptest(remethod2)
bptest(remethod)
summary(remethod)

"""Balanced Panel: n = 90, T = 7, N = 630

Effects:
                  var std.dev share
idiosyncratic 0.02965 0.17220 0.322
individual    0.06247 0.24994 0.678
theta: 0.748

Residuals:
      Min.    1st Qu.     Median    3rd Qu.       Max. 
-1.1173738 -0.0799691  0.0019003  0.0832178  1.3527570 

Coefficients:
              Estimate Std. Error z-value  Pr(>|z|)    
(Intercept) -3.2286240  0.3975161 -8.1220 4.586e-16 ***
lprbarr     -0.2721686  0.0347074 -7.8418 4.441e-15 ***
lprbconv    -0.1679453  0.0210214 -7.9893 1.358e-15 ***
lprbpris    -0.1393079  0.0389905 -3.5729 0.0003531 ***
lavgsen      0.0158810  0.0307641  0.5162 0.6057021    
lpctymle     0.1836434  0.1346702  1.3637 0.1726770    
lpctmin      0.2128363  0.0293176  7.2597 3.880e-13 ***
ldensity     0.4250662  0.0406773 10.4497 < 2.2e-16 ***
ltaxpc       0.1124836  0.0453605  2.4798 0.0131466 *  
lwfir       -0.0094112  0.0348109 -0.2704 0.7868884    
lwmfg       -0.2653646  0.0747373 -3.5506 0.0003843 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    29.955
Residual Sum of Squares: 19.037
R-Squared:      0.36449
Adj. R-Squared: 0.35422
Chisq: 355.023 on 10 DF, p-value: < 2.22e-16"""

remethod_twoways = plm(crmrte ~ prbarr + prbconv + prbpris + avgsen + 
                 polpc + density + taxpc + wfir +wmfg, data = df, model = "random", index=c("county","year"), effect="twoways")
"""Erreur dans swar_Between_check(estm[[3L]], method) : 
  model not estimable: 10 coefficient(s) (incl. intercept) to be estimated but only 7 time(s) in data for the between model necessary for Swamy-Arora random-effect model estimation
De plus : Message d'avis :
Dans summary.lm(object, ...) :
  ajustement pratiquement parfait : le résumé n’est peut-être pas fiable"""

#Test for INDIVIDUAL and TIME effect 
plmtest(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + 
          lpctmin+ lpctymle + ldensity + ltaxpc + lwfir + lwmfg, data = df, effect ="twoways", type="ghm")
""" H0: No significant and time effects
Ha: Significant individual and time effects
Lagrange Multiplier Test - two-ways effects (Gourieroux, Holly
	and Monfort)

data:  crmrte ~ prbarr + prbconv + prbpris + avgsen + polpc + density +  ...
chibarsq = 716.08, df0 = 0.00, df1 = 1.00, df2 = 2.00, w0 = 0.25,
w1 = 0.50, w2 = 0.25, p-value < 2.2e-16
alternative hypothesis: significant effects
H0 est rejetée, on accepte Ha"""

#Comparaison de pool effect et fixed effect
pFtest(femethod,pooledmethod2)
"""H0 : pooled ols model is consistant, Ha: fixed effect model is consistent
F test for individual effects

data:  crmrte ~ prbarr + prbconv + prbpris + avgsen + polpc + density +  ...
F = 16.819, df1 = 89, df2 = 533, p-value < 2.2e-16
alternative hypothesis: significant effects"""

#Le test D'Hausman

#le test d'hausman est un test statistiques qui permet de selectionner si le fixed effect ou le random effect est le plus 
#approprier à utiliser
# Ho = Random (RE) , H1 = Within (FE)
phtest(femethod, remethod2)
"""
Le modèle fixed effect est consistant
Hausman Test

data:  crmrte ~ prbarr + prbconv + prbpris + avgsen + polpc + density +  ...
chisq = 32.224, df = 7, p-value = 3.691e-05
alternative hypothesis: one model is inconsistent"""
phtest(femethod, remethod, data = df, method = "aux", vcov = vcovHC)
"""Hausman Test

data:  crmrte ~ prbarr + prbconv + prbpris + avgsen + polpc + density +  ...
chisq = 13.88, df = 9, p-value = 0.1267
alternative hypothesis: one model is inconsistent"""

#L'estimation de White sur du pannel 
# Calculer la matrice de covariance robuste à l'aide de la méthode de White
robust_cov <- vcovHC(remethod2, method = "white1")
# Extraire les erreurs standard robustes à partir de la matrice de covariance
RobustSE_Within <- list(sqrt(diag(robust_cov)))
# Résumé du modèle en utilisant les erreurs standard robustes
summary(remethod2, se = RobustSE_Within)
  
#Si on veux obverser l'intercept qui est spécifique a chaque individu et qui n'est pas aléatoire , on utilise la commande fixef
fixef(femethod)
library(nlme)
m <- model(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + 
             lpctymle + lpctmin + ldensity + ltaxpc + lwfir + lwmfg, data = df)
gls.model <- gls(m, model = "random", 
                 correlation = corAR1(form = ~year))

gls_model <- gls(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + 
                   lpctymle + lpctmin + ldensity + ltaxpc + lwfir + lwmfg, 
                 data = df, model = "random")

summary(gls.model)



#Transformation Within
crmrteW <- Within(df2$crmrte, effect = "indiv", na.rm = TRUE)
prbarrW <- Within(df2$prbarr, effect = "indiv")
prbconvW <- Within(df2$prbconv, effect = "indiv")
prbprisW <- Within(df2$prbpris, effect = "indiv")
avgsenW <- Within(df2$avgsen, effect = "indiv")
polpcW <- Within(df2$polpc, effect = "indiv")
densityW <- Within(df2$density, effect = "indiv")
taxpcW <- Within(df2$taxpc, effect = "indiv")
lpctymleW <- Within(df2$lpctymle, effect = "indiv")
lpctminW <- Within(df2$lpctmin, effect = "indiv")
wfirW <- Within(df2$wfir, effect = "indiv")
wmfgW <- Within(df2$wmfg, effect = "indiv")

beta_within_i <- lm(crmrteW ~ prbarrW + prbconvW + prbprisW + avgsenW +
                       lpctymleW + lpctminW + densityW + taxpcW, data = df2)
summary(beta_within_i)
AIC(beta_within_i)
"""
Residuals:
      Min        1Q    Median        3Q       Max 
-0.040197 -0.002587 -0.000358  0.002238  0.066422 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -8.285e-21  2.271e-04   0.000 1.000000    
prbarrW     -7.871e-03  2.285e-03  -3.445 0.000609 ***
prbconvW    -1.024e-03  1.797e-04  -5.696 1.89e-08 ***
prbprisW    -1.315e-03  3.335e-03  -0.394 0.693435    
avgsenW      2.630e-04  1.046e-04   2.514 0.012205 *  
polpcW       2.060e+00  1.484e-01  13.885  < 2e-16 ***
densityW     4.281e-04  4.239e-03   0.101 0.919592    
taxpcW       3.282e-05  3.687e-05   0.890 0.373765    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.0057 on 622 degrees of freedom
Multiple R-squared:  0.2437,	Adjusted R-squared:  0.2352 
F-statistic: 28.63 on 7 and 622 DF,  p-value: < 2.2e-16
"""

lcrmrtetW <- Within(df2$lcrmrte, effect = "time", na.rm = TRUE)
lprbarrtW <- Within(df2$lprbarr, effect = "time")
lprbconvtW <- Within(df2$lprbconv, effect = "time")
lprbpristW <- Within(df2$lprbpris, effect = "time")
lavgsentW <- Within(df2$lavgsen, effect = "time")
lpolpctW <- Within(df2$lpolpc, effect = "time")
ldensitytW <- Within(df2$ldensity, effect = "time")
ltaxpctW <- Within(df2$ltaxpc, effect = "time")
lwfirtW <- Within(df2$lwfir, effect = "time")
lwmfgtW <- Within(df2$lwmfg, effect = "time")
lpctymletW <- Within(df2$lpctymle, effect = "time")
lpctmintW <- Within(df2$lpctmin, effect = "time")

beta_within_t2 <- lm(lcrmrtetW ~ lprbarrtW + lprbconvtW + lprbpristW + lavgsentW +
                      lpctymletW + lpctmintW + ldensitytW + ltaxpctW, data = df2)
summary(beta_within_t)
bptest(beta_within_t)
summary(beta_within_t2)
"""Call:
Residuals:
     Min       1Q   Median       3Q      Max 
-1.19196 -0.14387 -0.00298  0.14631  2.22414 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.62624    0.04054 -15.448  < 2e-16 ***
lprbarrtW   -0.42816    0.03401 -12.591  < 2e-16 ***
lprbconvtW  -0.31449    0.02340 -13.439  < 2e-16 ***
lprbpristW  -0.08045    0.05815  -1.383 0.167067    
lavgsentW   -0.08550    0.04674  -1.829 0.067839 .  
lpctymletW   0.05835    0.06922   0.843 0.399560    
ldensitytW   0.30804    0.01985  15.521  < 2e-16 ***
ltaxpctW     0.18009    0.04764   3.781 0.000172 ***
lpctmin      0.21495    0.01328  16.189  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3044 on 621 degrees of freedom
Multiple R-squared:  0.7187,	Adjusted R-squared:  0.715 
F-statistic: 198.3 on 8 and 621 DF,  p-value: < 2.2e-16
"""
AIC(beta_within_t2)
bptest(beta_within_t2)
"""studentized Breusch-Pagan test

data:  beta_within_t2
BP = 73.741, df = 8, p-value = 8.81e-13"""

beta_between_ = plm(crmrte ~ prbarr + prbconv + prbpris + avgsen + 
                                        polpc + density + taxpc + wfir +wmfg, data = df, model = "between", index=c("county","year"))
beta_between_2 = plm(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lpctymle +
                       ldensity + lpctmin + ltaxpc + lwfir +lwmfg, data = df, model = "between", index=c("county","year"))
summary(beta_between_2)
"""Oneway (individual) effect Between Model

Call:
Balanced Panel: n = 90, T = 7, N = 630
Observations used in estimation: 90

Residuals:
     Min.   1st Qu.    Median   3rd Qu.      Max. 
-0.535471 -0.127292 -0.016982  0.107541  1.201307 

Coefficients:
             Estimate Std. Error t-value  Pr(>|t|)    
(Intercept) -5.771664   1.268418 -4.5503 1.912e-05 ***
lprbarr     -0.529346   0.096206 -5.5022 4.490e-07 ***
lprbconv    -0.430989   0.068717 -6.2720 1.773e-08 ***
lprbpris     0.267462   0.271764  0.9842  0.328036    
lavgsen     -0.174772   0.183204 -0.9540  0.343005    
lpctymle     0.077346   0.167325  0.4623  0.645171    
ldensity     0.210152   0.061911  3.3944  0.001078 ** 
lpctmin      0.200973   0.033558  5.9889 5.915e-08 ***
ltaxpc       0.230031   0.134529  1.7099  0.091209 .  
lwfir        0.033124   0.223267  0.1484  0.882435    
lwmfg        0.080234   0.157521  0.5094  0.611926    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    26.913
Residual Sum of Squares: 5.2697
R-Squared:      0.80419
Adj. R-Squared: 0.77941
F-statistic: 32.4461 on 10 and 79 DF, p-value: < 2.22e-16"""
bptest(beta_between_2)
""" bptest(beta_between_2)

	studentized Breusch-Pagan test

data:  beta_between_2
BP = 16.441, df = 9, p-value = 0.05823
"""
AIC(beta_between_2)
first_diff = plm(crmrte ~ prbarr + prbconv + prbpris + avgsen + 
                                   polpc + density + taxpc + wfir +wmfg, data = df, model = "fd", index=c("county","year"))
summary(first_diff)
"""Residuals:
       Min.     1st Qu.      Median     3rd Qu.        Max. 
-6.1121e-02 -2.3801e-03 -6.0622e-05  2.3107e-03  1.0780e-01 

Coefficients:
               Estimate  Std. Error t-value  Pr(>|t|)    
(Intercept) -2.2479e-04  5.4907e-04 -0.4094  0.682404    
prbarr      -7.6320e-03  2.5129e-03 -3.0371  0.002506 ** 
prbconv     -1.2664e-03  1.6881e-04 -7.5018 2.672e-13 ***
prbpris     -1.6153e-03  3.3357e-03 -0.4842  0.628409    
avgsen      -1.3863e-05  1.1280e-04 -0.1229  0.902232    
polpc        2.6430e+00  2.0923e-01 12.6323 < 2.2e-16 ***
density      8.6381e-03  1.2273e-02  0.7038  0.481846    
taxpc       -1.7617e-05  4.9524e-05 -0.3557  0.722191    
wfir        -5.8379e-06  1.1876e-05 -0.4916  0.623240    
wmfg         1.8209e-05  2.1891e-05  0.8318  0.405876    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    0.043448
Residual Sum of Squares: 0.03261
R-Squared:      0.24943
Adj. R-Squared: 0.23669
F-statistic: 19.5701 on 9 and 530 DF, p-value: < 2.22e-16"""

#Les tests statistiques :

#Distribution des résidus
residus <- resid(remethod2)
# Histogramme des résidus
hist(residus, main = "Histogramme des résidus", xlab = "Résidus", breaks = 30)

# QQ plot des résidus
qqnorm(residus)
qqline(residus)

# Test de Shapiro-Wilk pour la normalité des résidus
shapiro.test(residus)



# Supposons que 'modele' est votre modèle

# Calculer les résidus
residus <- resid(remethod2)

# Calculer les valeurs prédites
valeurs_predites <- fitted(remethod2)

ggplot(data.frame(valeurs_predites = valeurs_predites, residus = residus),
       aes_string(x = "valeurs_predites", y = "residus")) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Valeurs prédites", y = "Résidus")
#Absence d'homoscedasticité, corroboré par les bptest


# Tracer le correlogramme des résidus
acf(residus)

# AUTOCORELATION DES RESIDUS
install.packages("car")
library(car)
residus <- resid(remethod2)

# Convertir les résidus en un vecteur
residus_vector <- as.vector(residus)
# Effectuer le test de Durbin-Watson
durbinWatsonTest(residus_vector)

#LINEARITE DE LA RELATION
# Appliquer le test RESET
resettest(remethod2, power = 1, type = "fitted")


#QUALITE AJUSTEMENT DU MODELE
#Test AIC
log_likelihood <- -0.5 * (nobs(pooledmethod2)) * (1 + log(2 * pi)) - 0.5 * (nobs(pooledmethod2)) * log(sum(residuals(pooledmethod2)^2/nobs(pooledmethod2)))

# Calculer le nombre de paramètres dans le modèle
nb_parametres <- length(coef(pooledmethod2))

# Calculer l'AIC
AIC_value <- -2 * log_likelihood + 2 * nb_parametres

# Afficher l'AIC
print(AIC_value)
AIC(beta_between_)
