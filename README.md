
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->
[![Project Status: Work in
progress](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![pipeline
status](https://git.stable.innovation.insee.eu/xkfzv9/fmtsas/badges/master/pipeline.svg)](https://git.stable.innovation.insee.eu/xkfzv9/fmtsas/pipelines)
[![coverage
report](https://git.stable.innovation.insee.eu/xkfzv9/fmtsas/badges/master/coverage.svg)](https://git.stable.innovation.insee.eu/xkfzv9/fmtsas/commits/master)
[![CRAN
status](https://www.r-pkg.org/badges/version/fmtsas)](https://cran.r-project.org/package=fmtsas)
<!-- badges: end -->

![](https://git.stable.innovation.insee.eu/uploads/-/system/project/avatar/3136/visuel_fmt_sas.png?width=64)

# fmtsas

Le package R *fmtsas* permet d’importer dans R des données servant à
construire des formats SAS.

Pour cela, il construit une liste de vecteurs, à partir de tables SAS ou
de programmes SAS. Cette liste pourra ensuite être utilisée pour
effectuer des conversions ou agrégations.

Le package ne gère (pour l’instant) que des formats caractères
comportant un nombre défini de modalités.

## Installation

Pour installer le package sur un poste Insee ou sur AUS :

``` r
# install.packages("remotes", type = "source")
remotes::install_gitlab(
  repo    = "xkfzv9/fmtsas",
  host    = "git.stable.innovation.insee.eu",
  upgrade = "never",
  build   = FALSE
)
```

(sur AUS, il est possible qu’il faille répéter cette opération deux
fois)

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
#>    A    B    C 
#>  "A" "BC" "BC" 
#> attr(,"other")
#> [1] "ERROR"
#> 
#> $sexe
#>       1       2 
#> "Homme" "Femme" 
#> attr(,"other")
#> [1] NA
```

[Documentation détaillée de la
fonction](http://xkfzv9.pages.innovation.insee.eu/fmtsas/reference/from_tab.html).
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
       "01", "02", "03", "04", "05" = "Outre-mer"
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
#>       1       2 
#> "Homme" "Femme" 
#> attr(,"other")
#> [1] NA
#> 
#> $rega
#>          01          02          03          04          05          11 
#> "Outre-mer" "Outre-mer" "Outre-mer" "Outre-mer" "Outre-mer" "Métropole" 
#>          24          27          28          32          44          52 
#> "Métropole" "Métropole" "Métropole" "Métropole" "Métropole" "Métropole" 
#>          53          75          76          84          93          94 
#> "Métropole" "Métropole" "Métropole" "Métropole" "Métropole" "Métropole" 
#> attr(,"other")
#> [1] "ERREUR"
```

[Documentation détaillée de la
fonction](http://xkfzv9.pages.innovation.insee.eu/fmtsas/reference/from_pgm.html).
<!-- lien en dur, trouver un moyen de rendre cela portable -->

## Utilisation

La liste de vecteurs nommés que retournent les deux fonctions peuvent
être utilisés pour convertir des variables caractères d’un data.frame.

Par exemple, si l’on a un jeu de données avec des codes pour lesquels on
souhaite avoir une correspondance en clair et/ou une agrégation.

``` r
donnees <-
  data.frame(
    ID   = c("001", "002", "003", "004"),
    SEXE = c(  "1",   "2",   "1",   "2"),
    REG  = c( "04",  "11",  "24",  "27"),
    stringsAsFactors = FALSE # la conversion ne marche pas sur des facteurs
  )
```

On utilise les listes générées par le package :

``` r
donnees$SEXE_LIB <- conv_t$sexe[donnees$SEXE]
donnees$REG2 <- conv_p$rega[donnees$REG]

donnees
#>    ID SEXE REG SEXE_LIB      REG2
#> 1 001    1  04    Homme Outre-mer
#> 2 002    2  11    Femme Métropole
#> 3 003    1  24    Homme Métropole
#> 4 004    2  27    Femme Métropole
```

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

## Amélioration

Package en cours de développement. [Contribuez
\!](https://git.stable.innovation.insee.eu/xkfzv9/fmtsas)

  - signaler des bugs ou des cas où le package ne fonctionne pas
    correctement (de préference
    [ici](https://git.stable.innovation.insee.eu/xkfzv9/fmtsas/issues))
  - proposer une amélioration du code
