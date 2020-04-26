# Extrait les instructions du type
#     GEO2 = put(REG2016, $reg.) ;
#     A13 = put(A21, $a13.) ;
# dans une matrice de la forme
#        new    old       fmt
#   [1,] "GEO2" "REG2016" "reg"
#   [2,] "A13"  "A21"     "a13"

extract_put <- function(sas_pgm) {

  stopifnot(length(sas_pgm) == 1 && is.character(sas_pgm))

  # supprime espaces
  sas_pgm <- gsub("\\s", "", sas_pgm)

  res <-
    stringr::str_match_all(
      sas_pgm,
      "(\\w+)=put\\((\\w+),\\$(\\w+)\\.\\);"
    )[[1]]

  res <- res[ , -1, drop = FALSE]
  colnames(res) <- c("new", "old", "fmt")

  res

}

# Transforme un vecteur de type
#    c(new = "GEO2", old = "REG2016", fmt = "reg")
# en une chaîne de type
#    "<new> = <fmt_list>$<old>[<fmt>]" (sytle dplyr)

stylise <- function(x, fmt_list, style) {

  pattern <- switch(
    style,
    "dplyr" = "%s = %s$%s[%s]",
    "base"  = "<donnees>$%s <- %s$%s[<donnees>$%s]"
  )

  sprintf(
    pattern,
    x["new"], fmt_list, x["fmt"], x["old"]
  )

}

#' Convertit du code SAS (put) en code R
#'
#' Convertit les instructions de la forme `NEW = put(OLD, $var.);` d'un
#' programme SAS en leur équivalent en R.
#'
#' La fonction recherche le motif ci-dessus quel que soit l'endroit où il
#' apparaît dans le programme, hormis dans un commentaire. Les espaces et sauts
#' de lignes sont sont sans importance dans la détection des instructions.
#'
#' Deux styles sont possibles pour le code R généré :
#'
#' - `"dplyr"` génère des instructions de type
#'   `mutate(NEW = <fmt>$var[OLD], ...)`
#' - `"base"` génère une syntaxe standard
#'   `<donnees>$NEW <- <fmt>$var[<donnees>$OLD]`
#'
#' @inheritParams from_pgm
#' @param style forme du code généré : `"dplyr"` (défaut) ou `"base"`. Voir
#'   section 'Details'.
#' @param fmt_list nom de la liste créée auparavant par [from_tab] ou [from_pgm]
#'   (sous forme de chaîne de caractères).
#' @param file par défaut, le résultat est affiché dans la console. Spécifier
#'   un nom de fichier pour une sauvegarde disque.
#' @param quiet pour désactiver certains avertissements.
#'
#' @return La fonction écrit le résultat dans la console ou dans un fichier. La
#'   chaîne de caractères correspondante est de plus retournée sous forme de
#'   vecteur caractère (de manière invisible).
#'
#' @export
#'
#' @examples
#' test_pgm <- readLines(
#'   system.file("extdata", "pgm_format_test.sas", package = "fmtsas")
#' )
#' cat(tail(test_pgm), sep = "\n")
#'
#' convert_put(test_pgm, style = "dplyr")
#' convert_put(test_pgm, style = "base")

convert_put <- function(sas_pgm,
                        style = c("dplyr", "base"),
                        fmt_list = "<fmt>",
                        file = NULL,
                        quiet = FALSE) {

  style <- match.arg(style)

  if (is.null(file)) file <- "" # -> console

  sas_pgm <- paste(sas_pgm, collapse = "\n") # collapse si vecteur longueur > 1
  sas_pgm <- rm_sas_comments(sas_pgm) # suppr comments

  # donnees instructions put

  data_put <- extract_put(sas_pgm)

  if (!nrow(data_put)) {
    stop(
      "aucune instruction de type ",
      "`NEW = put(OLD, $fmt.);` ",
      "dans le programme"
    )
  }

  # formate chaque conversion

  res_vect <-
    apply(
      data_put,
      MARGIN   = 1,
      FUN      = stylise,
      fmt_list = fmt_list,
      style    = style
    )

  # finalise instruction

  if (style == "dplyr") {

    res <-
      sprintf(
        "mutate(\n  %s\n)\n",
        paste(res_vect, collapse = ",\n  ")
      )

  } else if (style == "base") {

    res <-
      sprintf(
        "%s\n",
        paste(res_vect, collapse = "\n")
      )

  }

  if (!quiet && !exists(fmt_list)) {
    warning(
      "le code genere ne pourra probablement pas s'executer en l'etat car `",
      fmt_list, "` n'existe pas\n  ",
      "(pour creer cette liste, utiliser `from_tab` ou `from_pgm`)"
    )
  }

  # resultat

  cat(res, file = file)
  invisible(res)

}
