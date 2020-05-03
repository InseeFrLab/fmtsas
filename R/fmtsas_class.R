# Constructeurs -----------------------------------------------------------

#' Constructeur d'objet S3 fmtsas_c
#'
#' Construit un objet représentant un format caractère SAS en R, à partir de
#' ses composantes (vecteur d'associations et modalité par défaut).
#'
#' @param x vecteur caractère dont tous les éléments sont nommés représentant
#'   un format SAS (association valeur entrée = valeur sortie).
#' @param other modalité par défaut. Un vecteur caractère de longueur 1.
#'
#' @return Un objet de classes `"fmtsas_c"` (et `"fmtsas"`). Ces objets seront
#'   en pratique créés par [from_tab] ou [from_pgm] (ces fonctions génèrent
#'   une liste d'objets de classe `fmtsas_c`).
#'
#' @export

fmtsas_c <- function(x, other = NA_character_) {

  if (!is.character(x)) {
    stop("`x` doit etre un vecteur caractere")
  }
  if (is.null(names(x)) || any(names(x) == "") || anyNA(names(x))) {
    stop("tous les elements doivent avoir un nom")
  }
  if (!is.character(other) || length(other) != 1) {
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
#' @export
other.fmtsas <- function(x) attr(x, "other")


# Setters -----------------------------------------------------------------

'other<-' <- function(x, value) UseMethod("other<-")

#' @export
'other<-.fmtsas' <- function(x, value) {

  err_msg <- "`other` doit etre un vecteur caractere de longueur 1"

  if (length(value) != 1) stop(err_msg)
  if (is.na(value)) value <- NA_character_ # pour autoriser ecriture NA (lgl)
  if (!is.character(value)) stop(err_msg)

  attr(x, "other") <- value
  x

}
