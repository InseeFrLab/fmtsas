#' Opérateur `[]` pour objet fmtsas_c
#'
#' Variante de `[]` pour objet fmtsas_c, prenant en compte une possible valeur
#' par défaut.
#'
#' L'opérateur `[]` pour un objet "fmtsas_c" s'utilise de la même façon que le
#' `[]` habituel. La différence est qu'il remplace les valeurs inconnues par la
#' valeur définie dans l'attribut `"other"`. Si cet attribut n'est pas présent,
#' les valeurs initiales sont conservées telles quelles.
#'
#' Autres particularités :
#'
#' - il ne s'utilise qu'avec un vecteur caractère en paramètre (pas d'entiers,
#'   ni de booléens). Un facteur est aussi possible, il sera transformé en
#'   caractère ;
#' - le résultat est dépourvu de noms d'éléments.
#'
#' @param x objet sur lequel on veut procéder à une extraction/conversion.
#' @param i noms des éléments à extraire ou convertir.
#' @param keep_na conserver les valeurs manquantes telles quelles. Par défaut
#'   `FALSE` : les `NA` seront remplacés par la valeur `other` (comme dans SAS).
#'
#' @return Un vecteur caractère non nommé, de même taille que le vecteur passé
#'   en paramètre `i`.
#'
#' @export
#' @name extract.fmtsas_c
#'
#' @examples
#' # construit un objet fmtsas_c
#' conv <- fmtsas_c(c("A" = "LIB_A", "B" = "LIB_B"), other = "?")
#'
#' # effectue une conversion
#' conv[c("A", "A", "B", "C", NA)]
#' conv[c("A", "A", "B", "C", NA), keep_na = TRUE]

`[.fmtsas_c` <- function(x, i, keep_na = FALSE) {

  # pour message erreur si `i` pas caractere
  x_str <- deparse(substitute(x), nlines = 1)
  i_str <- deparse(substitute(i), nlines = 1)

  if (is.numeric(i) || is.logical(i)) {
    stop(
      "seule la selection par noms est autorisee pour un objet `fmtsas_c`,\n  ",
      sprintf(
        "pour une utilisation classique de [] : `unclass(%s)[%s]` ",
        x_str, i_str
      )
    )
  }

  if (is.factor(i)) {
    i <- as.character(i) # transforme caractere, sinon probleme conversion
  }

  if (keep_na) {
    na_pos <- which(is.na(i)) # memorise positions NA
  }

  res <- unclass(x)[i] # extraction standard

  if (is.null(other(x))) {
    res[is.na(res)] <- i[is.na(res)] # valeurs initiales
  } else {
    res[is.na(res)] <- attr(x, "other") # remplace NA par attr other
  }

  if (keep_na) {
    res[na_pos] <- NA_character_ # remet les NA initiaux à NA
  }

  unname(res) # supprime noms

}
