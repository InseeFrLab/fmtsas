
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->
[![Project Status: Work in progress](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![pipeline status](https://gitlab.insee.fr/xkfzv9/fmtsas/badges/master/pipeline.svg)](https://gitlab.insee.fr/xkfzv9/fmtsas/-/pipelines)
[![coverage report](https://gitlab.insee.fr/xkfzv9/fmtsas/badges/master/coverage.svg)](https://gitlab.insee.fr/xkfzv9/fmtsas/-/commits/master)
[![CRAN status](https://www.r-pkg.org/badges/version/fmtsas)](https://cran.r-project.org/package=fmtsas)
<!-- badges: end -->

![](https://gitlab.insee.fr/uploads/-/system/project/avatar/1134/visuel_fmt_sas.png?width=64)

# fmtsas

Le package R **fmtsas** permet d’importer dans R des données servant à
construire des formats SAS.

Pour cela, il construit une liste de vecteurs, à partir de tables SAS ou
de programmes SAS. Cette liste pourra ensuite être utilisée pour
effectuer des conversions ou agrégations.

*Le package ne gère (pour l’instant) que des formats caractères
comportant un nombre défini de modalités.*

## Installation

  - **version stable la plus récente** (recommandé)

<!-- end list -->

``` r
install.packages("fmtsas", repos = "https://nexus.insee.fr/repository/r-public")
```

  - **version de développement**

<!-- end list -->

``` r
# install.packages("remotes")
remotes::install_gitlab(
  repo    = "xkfzv9/fmtsas",
  host    = "gitlab.insee.fr",
  upgrade = "never"
)
```

## Chargement

``` r
library(fmtsas)
```

## Exemples d’import des données

### Depuis une table : `from_tab`

En entrée un data.frame qui proviendra typiquement d’une table SAS
servant dans une `proc format` avec l’option `CNTLIN=`.

``` r
format_data <- 
  data.frame(
    FMTNAME = c( "fmt1_", "fmt1_", "fmt1_", "fmt1_",  "sexe",  "sexe"),
    TYPE    = c(     "C",     "C",     "C",     "C",     "C",     "C"),
    START   = c(     "A",     "B",     "C",      NA,     "1",     "2"),
    LABEL   = c(     "A",    "BC",    "BC", "ERROR", "Homme", "Femme"),
    HLO     = c(      NA,      NA,      NA,     "O",      NA,      NA)
  )

conv_t <- from_tab(format_data)

conv_t
#> $fmt1_
#> # a character `fmtsas` object
#>    A    B    C 
#>  "A" "BC" "BC" 
#> [other] "ERROR"
#> $sexe
#> # a character `fmtsas` object
#>       1       2 
#> "Homme" "Femme"
```

[Documentation détaillée de la
fonction](https://xkfzv9.gitlab-pages.insee.fr/fmtsas/reference/from_tab.html).
<!-- lien en dur, trouver un moyen de rendre cela portable -->

### Depuis un programme : `from_pgm`

En entrée un programme SAS contenant une ou plusieurs `proc format` avec
`value(s)`.

``` r
test_pgm <-
  'PROC FORMAT;
     VALUE $ sexe "1"="Homme" "2"="Femme" ;
     value vnum
       0-99 = "petit" 100-high = "grand" ; 
     value $rega /* (geographie) */
       "01", "02", "03", "04", "06" = "Outre-mer"
       "11", "24", "27", "28", "32",
       "44", "52", "53", "75", "76",
       "84", "93", "94"             = "Métropole"
       other                        = "ERREUR" ;
   RUN;'

conv_p <- from_pgm(test_pgm)
#> Warning in from_pgm(test_pgm): 
#> Format(s) numerique(s) ignore(s) :
#>   vnum

conv_p
#> $sexe
#> # a character `fmtsas` object
#>       1       2 
#> "Homme" "Femme" 
#> 
#> $rega
#> # a character `fmtsas` object
#>          01          02          03          04          06          11 
#> "Outre-mer" "Outre-mer" "Outre-mer" "Outre-mer" "Outre-mer" "Métropole" 
#>          24          27          28          32          44          52 
#> "Métropole" "Métropole" "Métropole" "Métropole" "Métropole" "Métropole" 
#>          53          75          76          84          93          94 
#> "Métropole" "Métropole" "Métropole" "Métropole" "Métropole" "Métropole" 
#> [other] "ERREUR"
```

[Documentation détaillée de la
fonction](https://xkfzv9.gitlab-pages.insee.fr/fmtsas/reference/from_pgm.html).
<!-- lien en dur, trouver un moyen de rendre cela portable -->

## Utilisation après import

### Format de l’objet

Les fonctions `from_tab` et `from_pgm` retournent toutes deux une liste
de vecteurs :

  - les noms de la liste correspondent aux noms des formats SAS ;
  - les éléments de la liste sont des vecteurs contenant les relations
    entre valeurs initiales et valeurs converties ;
  - chaque élément a un éventuel attribut `"other"` ;
  - chaque élément est un objet de type `fmtsas_c`, ce qui permet
    notamment d’utiliser
    [`[]`](https://xkfzv9.gitlab-pages.insee.fr/fmtsas/reference/extract.fmtsas_c.html)
    avec prise en compte des valeurs par défaut (`other`). Cf. infra
    pour plus de détails sur cet opérateur.

### Exemple d’utilisation

Par exemple, si l’on a un jeu de données avec des codes pour lesquels on
souhaite avoir une correspondance en clair et/ou une agrégation.

``` r
donnees <-
  data.frame(
    ID   = c("001", "002", "003", "004", "005"),
    SEXE = c(  "1",   "2",   "1",   "2",   "#"),
    REG  = c( "04",  "11",  "##",    NA,  "24")
  )
```

On utilise les listes générées par les fonctions `from_*` et l’opérateur
`[]` pour objet “fmtsas” :

``` r
donnees$SEXE_LIB <- conv_t$sexe[donnees$SEXE]
donnees$REG2 <- conv_p$rega[donnees$REG]

donnees
#>    ID SEXE  REG SEXE_LIB      REG2
#> 1 001    1   04    Homme Outre-mer
#> 2 002    2   11    Femme Métropole
#> 3 003    1   ##    Homme    ERREUR
#> 4 004    2 <NA>    Femme    ERREUR
#> 5 005    #   24        # Métropole
```

> Utiliser `[]` sur un élément d’une liste créée par `from_tab` ou
> `from_pgm` fonctionne comme une sélection sur des vecteurs *normaux*,
> à la différence que d’éventuels `NA` seront remplacés par :
> 
>   - la valeur contenue dans l’attribut “other”, si cet attribut est
>     présent ;
>   - la valeur initiale, s’il n’y a pas d’attribut “other”.
> 
> Ces modifications ont pour but de reproduire le comportement d’un
> format SAS.
> 
> Pour que cet opérateur dédié soit utilisé, il faut que **le package
> soit chargé**. Dans le cas contraire, l’opérateur `[]` de base sera
> utilisé.

### Conversion de code SAS

Le package offre également la possibilité de rechercher dans un
programme SAS les instructions qui créent des variables à partir de
formats (de la forme `NEW = put(OLD, $var.);`), pour générer le code R
correspondant :

``` r
pgm_sas <-
  "data t2 ; set t1 ;
     SEXE_LIB = put(SEXE, $sexe.) ;
     REG2 = put(REG, $rega.) ;
   run ;"

convert_put(pgm_sas, fmt_list = "conv_p", style = "dplyr")
#> mutate(
#>   SEXE_LIB = conv_p$sexe[SEXE],
#>   REG2 = conv_p$rega[REG]
#> )
convert_put(pgm_sas, fmt_list = "conv_p", style = "base")
#> <donnees>$SEXE_LIB <- conv_p$sexe[<donnees>$SEXE]
#> <donnees>$REG2 <- conv_p$rega[<donnees>$REG]
```

Il ne reste plus qu’à copier ces instructions dans un programme R (après
quelques éventuels ajustements).

## Améliorations

Package en cours de développement.

[Contribuez](https://xkfzv9.gitlab-pages.insee.fr/xkfzv9/fmtsas),
notamment en signalant des bugs ou des cas où le package ne fonctionne
pas correctement (de préference
[ici](https://gitlab.insee.fr/xkfzv9/fmtsas/-/issues)).

Idées pour la suite :

  - gestion des formats numériques
  - traduction et soumission CRAN
