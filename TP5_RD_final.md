
-----

<center>

<!-- Permet de centrer le texte jusqu'au </center> plus bas -->

Raphaël Deragon
<!-- Le fait de laisser une ligne entre les deux parties du premier bloc de texte place la deuxième partie sur une seconde ligne -->

111 153 173

<br><br><br>
<!-- Permet l'ajout de 3 lignes vides séparant les deux blocs de texte -->

Analyse et modélisation d’agroécosystèmes - GAA-7007

Hiver 2020

<br><br><br>

**Travail 5 : Séries temporelles et reproductibilité**

<br><br><br>

Présenté à M. Serge-Étienne Parent

<br><br><br>

Département des sols et de génie agroalimentaire

Faculté des sciences de l’agriculture et de l’alimentation

Université Laval

<br><br><br>

Remis le 30 avril 2020

</center>

-----

Ce document se trouve dans mon [dépôt
GitHub](https://github.com/Derag12/Remise_de_travaux).

### Étape 1 : Importer et explorer le jeu de données

  - On charge les librairies pour la session de travail
  - On importe les données
  - On explore avec `str` et `summary`.

<!-- end list -->

``` r
library("tidyverse")
library("ggplot2")
library("remedy")

hawai <- read.csv("hawai.csv")

str(hawai)  
```

    ## 'data.frame':    526 obs. of  2 variables:
    ##  $ time: num  1958 1958 1958 1958 1958 ...
    ##  $ CO2 : num  316 317 317 317 316 ...

``` r
summary(hawai)
```

    ##       time           CO2       
    ##  Min.   :1958   Min.   :313.4  
    ##  1st Qu.:1969   1st Qu.:324.0  
    ##  Median :1980   Median :337.9  
    ##  Mean   :1980   Mean   :339.6  
    ##  3rd Qu.:1991   3rd Qu.:354.5  
    ##  Max.   :2002   Max.   :373.8

<!-- Pour faire disparaître le bloc de code du fichier final et faire apparaître seulement le résultats```{r, echo = FALSE}  -->

<!-- ```{r message=FALSE} fait disparaître l'info que la librairie a aussi loadé tel ou tel truc -->

  - Les résultats montrent que la colonne *time* a une structure de type
    numérique et non de type date.

<!-- end list -->

``` r
plot(x = hawai$time, y = hawai$CO2, type = 'l', lty = 1, lwd = 1, 
     main = "Évolution du CO2 en fonction du temps", xlab = "Temps (année)", ylab = "CO2 (ppm)")
```

![](TP5_RD_final_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

  - Avec le graphique exploratoire précédent, il est possible de
    remarquer la présence d’une fluctuation saisonnière et d’une
    tendance générale à la hausse. Il ne semble pas y avoir de valeur
    extrême. On peut remarquer un cycle qui sort de l’ordinaire vers
    1965.
  - La pente de la tendance générale se stabilise entre 1990 et 1993
    avant de remonter par la suite. Il faudra y porter attention dans la
    prédiction.

-----

### Étape 2 - Créer une série temporelle à partir du jeu de données

  - En analysant le jeu de données, il est possible de remarquer que la
    colonne *time* est composée de nombres de type *année.décimale* où
    les décimales sont des douzièmes d’année (donc des mois). En
    utilisant la cheatsheet du module `lubridate`, la fonction qui se
    rapproche le plus de notre format actuel est `date_decimal(decimal,
    tz = "UTC")`. Voici le résultat lorsqu’on exécute la fonction :

<!-- end list -->

``` r
library("lubridate")

test <- date_decimal(hawai$time, tz = "UTC") 
head(test, n = 20)
```

    ##  [1] "1958-03-02 20:00:01 UTC" "1958-04-02 06:00:00 UTC"
    ##  [3] "1958-05-02 16:00:00 UTC" "1958-06-02 02:00:01 UTC"
    ##  [5] "1958-07-02 12:00:00 UTC" "1958-08-01 22:00:00 UTC"
    ##  [7] "1958-09-01 08:00:01 UTC" "1958-10-01 18:00:00 UTC"
    ##  [9] "1958-11-01 04:00:00 UTC" "1958-12-01 14:00:01 UTC"
    ## [11] "1959-01-01 00:00:00 UTC" "1959-01-31 10:00:00 UTC"
    ## [13] "1959-03-02 20:00:01 UTC" "1959-04-02 06:00:00 UTC"
    ## [15] "1959-05-02 16:00:00 UTC" "1959-06-02 02:00:01 UTC"
    ## [17] "1959-07-02 12:00:00 UTC" "1959-08-01 22:00:00 UTC"
    ## [19] "1959-09-01 08:00:01 UTC" "1959-10-01 18:00:00 UTC"

  - Le résultat est peu pratique dans notre cas. Par exemple, on peut
    voir que le mois de janvier 1959 est présent à deux reprises et que
    le mois de février est absent… De plus, des jours et des heures sont
    présents sans que l’information ne soit désirée ni réelle (artéfacts
    des décimales arrondies du jeu de données original).
  - Par conséquent, il faut transformer en format date d’une autre
    façon. Une méthode simple en trois étapes est utilisée :
      - On crée une fonction pour ne conserver que les décimales de
        l’année que l’on multiplie par 12 mois pour avoir un entier
        correspondant au mois de l’année. Il faut ajouter 1 pour ne pas
        avoir de mois 0 et avoir 12 mois. La fonction `floor()` est
        utilisée afin d’arrondir l’entier (retirer les décimales).
      - Ensuite, on utilise la fonction `floor()` à nouveau, cette
        fois-ci afin de conserver l’année.
      - Finalement, on crée un nouveau tableau avec les colonnes *year*
        (obtenue au point 2) et *month* (obtenue au point 1). On peut
        utiliser `paste0()` afin de coller l’année et le mois dans un
        format que comprendra `lubridate`. Il faut s’assurer que les
        mois soient composés de deux chiffres (format \#\#). On ajoute
        donc des 0 devant les mois 1-9. On ajoute aussi *01* pour
        symboliser les jours et permettre le passage en format date.

<!-- end list -->

``` r
revtrunc <- function(x) { x - floor(x) }  
Decimal <- revtrunc(hawai$time)
Month <- (12 * Decimal) + 1


Data <- data.frame(year = floor(hawai$time),
                   month = Month)

Data<- Data %>% 
  mutate(Date = paste0(Data$year, 
                       ifelse(signif(Data$month, digits = 2) < 10, #Si le mois arrondi à 2 chiff. sign. est < 10
                                         paste0("0", signif(Data$month, digits = 2)),   # Vrai : colle 0, mois
                                         signif(Data$month, digits = 2)),               #Faux : colle mois
                       "01"))


str(Data)#glimpse(data)
```

    ## 'data.frame':    526 obs. of  3 variables:
    ##  $ year : num  1958 1958 1958 1958 1958 ...
    ##  $ month: num  3 4 5 6 7 ...
    ##  $ Date : chr  "19580301" "19580401" "19580501" "19580601" ...

  - Il est maintenant possible de passer en format date et de créer le
    tableau de données avec la colonne CO2.
  - Pour finir, on crée la série temporelle *serietemp\_ts* avec la
    fonction `ts()` et l’objet *serietemp*. On peut l’observer avec la
    fontion `autoplot()`.

<!-- end list -->

``` r
serietemp <- data.frame(Date = ymd(Data$Date),
                        CO2 = hawai$CO2)

head(serietemp, n = 20)
```

    ##          Date      CO2
    ## 1  1958-03-01 316.1000
    ## 2  1958-04-01 317.2000
    ## 3  1958-05-01 317.4333
    ## 4  1958-06-01 317.4333
    ## 5  1958-07-01 315.6250
    ## 6  1958-08-01 314.9500
    ## 7  1958-09-01 313.5000
    ## 8  1958-10-01 313.5000
    ## 9  1958-11-01 313.4250
    ## 10 1958-12-01 314.7000
    ## 11 1959-01-01 315.5000
    ## 12 1959-02-01 316.7000
    ## 13 1959-03-01 316.7333
    ## 14 1959-04-01 317.6750
    ## 15 1959-05-01 318.3250
    ## 16 1959-06-01 318.0250
    ## 17 1959-07-01 316.5250
    ## 18 1959-08-01 314.9000
    ## 19 1959-09-01 313.8250
    ## 20 1959-10-01 313.4000

``` r
# Tout est OK, ça a bien fonctionné 
serietemp_ts <- ts(serietemp %>% dplyr::select(CO2), # Ne pas prendre la colonne Date pour créer une ts
                   start = c(serietemp$Date[1] %>% year(), 3), # Commence dès la première ligne du tableau qui     est le 3e mois en utilisant les années
                   frequency = 12) # 12 périodes (mois) par cycle (année)

library("forecast")
autoplot(serietemp_ts)  # Visualiser le résultat
```

![](TP5_RD_final_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

  - Il est possible d’explorer la série temporelle sur plusieurs années
    et par mois à l’aide du module `cowplot` et de trois fonctions de
    `forecast` :

<!-- end list -->

``` r
ggA
```

![](TP5_RD_final_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

  - Sur cette première figure, il est effectivement possible de
    constater une hausse annuelle de la teneur en CO2 dans l’air. Les
    courbes se superposent. De plus, la fluctuation saisonnière est
    clairement visible. Elle prend la forme d’une courbe sinusoïdale où
    le maximum annuel semble se trouver vers mai et le minimum annuel
    vers septembre.

<!-- end list -->

``` r
ggB
```

![](TP5_RD_final_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

  - Sur cette deuxième figure, l’information est similaire au graphique
    précédent, mais les variations de CO2 sont continues à défaut d’être
    représentées à l’aide d’une courbe par an. Ce faisant, on peut
    remarquer que la courbe ne se recoupe pratiquement jamais pour un
    même mois. La teneur en CO2 augmente donc constamment. On peut
    aussi observer les années de forte augmentation lorsque la courbe
    est espacée pour un même mois entre deux années consécutives, tels
    les anneaux de croissance d’un arbre.

<!-- end list -->

``` r
ggC
```

![](TP5_RD_final_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

  - Pour terminer, cette troisième figure regroupe les années par mois,
    permettant par exemple de constater l’évolution de la teneur en CO2
    pour le mois de janvier d’année en année. La ligne horizontale
    correspond à la moyenne du mois au fil des ans. Cela confirme que le
    mois de mai voit les hausses les plus importantes et le mois de
    septembre (et octobre) montre les concentrations les plus faibles.

  - Il est aussi possible de confirmer la présence de structures dans la
    série temporelle et de s’assurer que ce ne soit pas un bruit blanc :

<!-- end list -->

``` r
gglagplot(serietemp_ts) + ggtitle("CO2 : Lag plot")
```

![](TP5_RD_final_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

  - Le lag plot montre des droites très proches de la diagonale, ce qui
    témoigne d’une forte tendance dans les données.
  - Vérifions maintenant la probabilité que la série temporelle ne soit
    que du bruit blanc avec une intervalle de confiance de 0,95 :

<!-- end list -->

``` r
ggAcf(serietemp_ts, ci = 0.95) + ggtitle("CO2 : Autocorrélation")
```

![](TP5_RD_final_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
Box.test(serietemp_ts, lag = 27, type = "Ljung-Box") # Teste la probabilité que la série soit du bruit blanc
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  serietemp_ts
    ## X-squared = 12216, df = 27, p-value < 2.2e-16

  - Selon la figure précédente, il semble que toutes les valeurs soient
    fortement corrélées entre elles de manière significative.
  - Le test de Ljung-Box montre que la probabilité que la série
    temporelle soit du bruit blanc est presque nulle (p-value très
    faible).

-----

### Étape 3 - Séparer le jeu de données en partie d’entraînement (70%) et en partie test

  - Le jeu de données est composé de 526 observations (mois) tel que vu
    dans le résultat de `str(Data)`.
  - Par un simple calcul, si on désire conserver 70% des données, on
    doit conserver les 368 premières observations : \(526*0.7 = 368.2\),
    soit jusqu’au 1988-10-01.

<!-- end list -->

``` r
CO2_ts <- serietemp_ts[, 1]
CO2_ts_train <- window(CO2_ts, start = 1958.167, end = 1988.750)
CO2_ts_test <- window(CO2_ts, start = 1988.833)
```

-----

### Étape 4 - Créer une modèle ETS et projeter les prévisions sur les données test

  - Tel que demandé, la méthode SES sera utilisée. Elle donne des poids
    exponentiellement moins importants aux valeurs précédentes. Le
    modèle demande plusieurs paramètres, dont certains en rapport avec
    la tendance, la saison et l’erreur. En utilisant `forecast::ets()`,
    R optimise la valeur des paramètres et le modèle en soit:

<!-- end list -->

``` r
CO2_modele <- ets(CO2_ts_train)
CO2_modele
```

    ## ETS(M,Ad,M) 
    ## 
    ## Call:
    ##  ets(y = CO2_ts_train) 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 0.7538 
    ##     beta  = 0.0348 
    ##     gamma = 2e-04 
    ##     phi   = 0.9761 
    ## 
    ##   Initial states:
    ##     l = 314.6724 
    ##     b = 0.025 
    ##     s = 1.004 1.0018 1 0.9973 0.994 0.9907
    ##            0.9912 0.9963 1.0022 1.0068 1.0086 1.0072
    ## 
    ##   sigma:  0.001
    ## 
    ##      AIC     AICc      BIC 
    ## 1398.208 1400.173 1468.504

  - Le modèle retenu est **ETS(M, Ad, M)**, ce qui signifie que l’erreur
    est multiplicative, la tendance est additive et la saison est
    multiplicative.
  - Il est possible d’observer l’effet des composantes du modèle sur
    l’observation

<!-- end list -->

``` r
autoplot(CO2_modele)
```

![](TP5_RD_final_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

  - On remarque que *level* augmente constamment (la tendance) et qu’il
    y a bien présence de fluctuations saisonnières (*season*).

  - Il est maintenant possible de prédire les valeurs retirées du jeu de
    données original avec `forecast()`. Les données utilisées pour le
    test seront superposées afin d’évaluer visuellement la prédiction.

<!-- end list -->

``` r
CO2_fc <- CO2_modele %>% forecast(h = 158)  # On demande de prédire 158 données afin de couvrir l'ensemble des    données de test.

autoplot(CO2_fc) +
  autolayer(CO2_ts_test, color = "black") + #ajoute la portion de données brutes mises de côté pour le test en    noir en guise de comparaison
  labs(x = "Année", y = "CO2 (ppm)")
```

![](TP5_RD_final_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

  - La prévision semble sous-estimer les erreurs en s’éloignant de la
    dernière donnée du jeu d’entraînement.
  - De plus, la médiane est horizontale au lieu de suivre la tendance
    initiale croissante. Cela veut dire que le modèle prévoit que 50%
    des données seront en deçà Cela fait en sorte que, selon le modèle,
    la teneur en CO2 de l’air pourrait diminuer dans le futur, alors que
    cette tendance n’a jamais été observée auparavant.
  - Pour corriger le tir, il pourrait être juste de prétransformer les
    données. On optimise le paramètre lambda avec `BoxCox.lambda()`.
    Ensuite, on lance une nouvelle prédiction avec le nouveau modèle ETS
    créé.

<!-- end list -->

``` r
BoxCox.lambda(CO2_ts_train)  # -0.6080674, près de -1 alors on tente la transformation log naturel (valeur de     lambda)
```

    ## [1] -0.6080674

``` r
CO2_ln_fc <- CO2_ts_train %>% 
  ets(lambda = -1) %>% 
  forecast(h = 157) 

autoplot(CO2_ln_fc) +
  autolayer(CO2_ts_test, color = "black") +
  labs(x = "Année", y = "CO2 (ppm)")
```

![](TP5_RD_final_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

  - Le paramètre lambda choisi est de -1. R transforme automatiquement
    les données avec un ln avant de créer le modèle.
  - À noter que le modèle a changé pour un **ETS(A, A, A)**, ce qui veut
    dire que la saison, l’erreur et la tendance sont toutes additives.
  - En observant la figure produite, on remarque que la prévision est
    toujours à la hausse, ce qui se rapproche davantage du jeu de
    données original.
  - Ce modèle a plutôt tendance à surestimer les erreurs plus la
    prédiction est éloignée dans le temps. En effet, la prédiction et
    les données test collent pour trois à quatre années avant de
    s’éloigner de la médiane, tout en demeurant dans l’intervalle de
    confiance de la prédiction.

-----

### Étape 5 - Effectuer une analyse des résidus

  - L’analyse des résidus est exécutée sur les deux modèles afin de les
    comparer.

<!-- end list -->

``` r
checkresiduals(CO2_fc)
```

![](TP5_RD_final_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ETS(M,Ad,M)
    ## Q* = 48.079, df = 7, p-value = 3.437e-08
    ## 
    ## Model df: 17.   Total lags used: 24

``` r
checkresiduals(CO2_ln_fc)
```

![](TP5_RD_final_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ETS(A,A,A)
    ## Q* = 52.176, df = 8, p-value = 1.556e-08
    ## 
    ## Model df: 16.   Total lags used: 24

  - Les deux modèles montrent une autocorélation entre 3 mesures,
    indiquant la présence d’un cycle significatif aux 16 lags environ.
    Ils montrent aussi des valeurs extrêmes dans leur histogramme des
    résidus et ont un p-value significatif selon le testde Ljung-Box.
    Cela veut dire qu’il est peu probable que les résidus aient été
    générés par du bruit blanc. Comme un patron existe toujours, cela
    signifie que le modèle n’est pas parfait et qu’il ne capte pas tous
    les mécanismes et cycles impliqués dans la série temporelle du CO2.

  - La pédiode choisie (mensuelle) limite peut-être la précision du
    modèle et constitue une période trop grossière masquant les
    influences hebdomadaires par exemple.

  - Cela dit, les modèles ne sont pas à rejeter pour autant.Les
    histogrammes sont très près de la courbe normale de référence. Une
    dernière analyse permettra de définir le meilleur modèle.

-----

### Étape 6 - Analyse de la fiabilité du modèle et amélioration

  - Plusieurs critères peuvent être utilisés afin d’évaluer la précision
    de la prédiction d’un modèle en se comparant aux données test. La
    fonction `accuracy()` permet d’obtenir la valeur de ces critères
    d’erreur pour le modèle (calibré sur les données d’entraînement)
    et pour la prédiction (comparée aux données test).

<!-- end list -->

``` r
accuracy(CO2_fc, CO2_ts)
```

    ##                      ME      RMSE      MAE        MPE       MAPE      MASE
    ## Training set 0.05221402 0.3310028 0.247852 0.01557531 0.07533287 0.2026326
    ## Test set     5.14916794 7.0637151 5.210695 1.40463918 1.42205401 4.2600285
    ##                     ACF1 Theil's U
    ## Training set -0.06517604        NA
    ## Test set      0.98156427     5.463

``` r
accuracy(CO2_ln_fc, CO2_ts)
```

    ##                       ME      RMSE       MAE         MPE       MAPE
    ## Training set  0.04027907 0.3272458 0.2480239  0.01220679 0.07530402
    ## Test set     -3.61068960 4.0040255 3.6154078 -0.99324535 0.99458496
    ##                   MASE      ACF1 Theil's U
    ## Training set 0.2027731 0.2326735        NA
    ## Test set     2.9557940 0.9604418  3.127581

  - La moyenne absolue échelonnée (MASE) est le critère suggéré pour
    l’analyse de la précision.

  - Pour les deux modèles testés, la MASE est pratiquement égale quant à
    la précision du modèle.Il s’agit d’une très faible valeur, ce qui
    est désiré.

  - Le critère montre des valeurs élevées pour les deux prévisions.
    Cependant, la prévision utilisant les données prétraitées avec le
    logarithme naturel montre une valeur beaucoup plus faible. Plus
    cette valeur se rapproche de zéro, plus le modèle est précis. Donc,
    le meilleur modèle semble être **CO2\_ln\_fc**, soit celui
    prétransformant les données.

  - Le modèle pourrait être amélioré. D’autres transformations
    pourraient être testées.

  - Tel que mentionné plus haut, l’analyse des résidus montre une
    structure résiduelle dans les données. Il serait intéressant de
    varier la période utilisée et collecter les données deux fois par
    mois ou même hebdomadairement afin de capter davantage de
    fluctuations saisonnières.
