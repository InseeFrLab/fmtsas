# Constructeurs -----------------------------------------------------------

#' Objet de classe S3 "fmtsas_c"
#'
#' Construit un objet représentant en R un format caractère SAS, à partir de
#' ses composantes (vecteur d'associations et modalité par défaut).
#'
#' La classe S3 `"fmtsas_c"` représente un format caractère SAS. Cette classe
#' hérite de la classe `"fmtsas"`, qui représente tout type de format
#' (caractère, numérique...).
#'
#' Un objet `"fmtsas_c"` est un vecteur nommé contenant les relations entre
#' valeurs initiales et valeurs converties. Il possède en outre un attribut
#' "other", où est stockée une valeur par défaut. Cette valeur sera utilisée
#' lors d'une conversion avec l'[opérateur de sélection][extract.fmtsas_c]
#' (`[]`).
#'
#' Le nom du format n'est pas stocké dans ce type d'objet. Il pourra être
#' spécifié comme un des noms d'un liste de `fmtsas`. Ces listes sont en
#' pratique générées par [from_tab] ou [from_pgm].
#'
#' @param x vecteur caractère (dont tous les éléments sont nommés) représentant
#'   un format SAS (association _valeur(s) entrée = valeur sortie_).
#' @param other modalité par défaut (paramètre optionnel). Un vecteur caractère
#'   de longueur 1.
#'
#' @return Un objet de classe `"fmtsas_c"` (et donc `"fmtsas"`), c'est-à-dire un
#'   vecteur nommé possédant un éventuel attribut "other".
#'
#' @export
#'
#' @examples
#' fmtsas_c(c("A" = "LIBA", "B" = "LIB_B"))
#' fmtsas_c(c("A" = "LIBA", "B" = "LIB_B"), other = "??")

fmtsas_c <- function(x, other = NULL) {

  if (!is.character(x)) {
    stop("`x` doit etre un vecteur caractere")
  }
  if (is.null(names(x)) || any(names(x) == "") || anyNA(names(x))) {
    stop("tous les elements doivent avoir un nom")
  }
  if (!is.null(other) && (!is.character(other) || length(other) != 1L)) {
    stop("`other` doit etre un vecteur caractere de longueur 1")
  }

  structure(
    x,
    other = other,
    class = c("fmtsas_c", "fmtsas")
  )

}


# Getters -----------------------------------------------------------------

other <- function(x) UseMethod("other")

other.fmtsas <- function(x) attr(x, "other")


# Setters -----------------------------------------------------------------

`other<-` <- function(x, value) UseMethod("other<-")

`other<-.fmtsas` <- function(x, value) {

  err_msg <- "`other` doit etre un vecteur caractere de longueur 1"

  if (!is.null(value)) {
    if (length(value) != 1) stop(err_msg)
    if (is.na(value) && is.logical(value)) { # pour autoriser ecriture NA (lgl)
      value <- NA_character_
    }
    if (!is.character(value)) stop(err_msg)
  }

  attr(x, "other") <- value
  x

}
