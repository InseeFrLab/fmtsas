# ..............................................................................
# Specifications
# https://documentation.sas.com/?docsetId=proc&docsetTarget=n1e19y6lrektafn1kj6nbvhus59w.htm&docsetVersion=9.4
# ..............................................................................

#' Convertit une table pour format SAS
#'
#' Convertit les données utilisées pour une proc format (option CNTLIN=) en une
#' liste de vecteurs qui pourra servir à effectuer des conversions.
#'
#' La table en entrée est un data.frame, à importer dans R à partir d'une table
#' SAS (par exemple avec le package **haven**).
#'
#' Cette fonction ne gère pour l'instant que des formats caractère. De plus,
#' s'il y a une colonne `END` dans la table en entrée, elle doit être égale à la
#' colonne `START`.
#'
#' @param sas_data un data.frame importé depuis une table SAS pour proc format.
#'   Ce data.frame doit contenir a minima les colonnes `FMTNAME`, `START`,
#'   `LABEL` et `TYPE` (majuscule ou minuscule).
#'
#' @return Une liste contenant autant d'éléments que de formats si les données
#'   avaient été générées par une proc format.
#'   - les noms de la liste correspondent aux noms des formats (`FMTNAME`) ;
#'   - les éléments de la liste sont des vecteurs contenant les relations entre
#'   valeurs initiales et valeurs converties.
#'
#'   Voir les exemples pour l'utilisation de cette liste.
#'
#' @export
#'
#' @seealso [from_pgm] pour importer les formats contenus dans un programme SAS.
#'
#' @examples
#' # donnees en entree (provenant d'une table SAS)
#' format_data <-
#'   data.frame(
#'     FMTNAME = c("v1fmt", "v1fmt", "v1fmt", "v2fmt", "v2fmt"),
#'     TYPE    = c(    "C",     "C",     "C",     "C",     "C"),
#'     START   = c(    "1",     "2",     "3",     "A",     "B"),
#'     LABEL   = c("Lib 1", "Lib23", "Lib23", "Lib A", "Lib B")
#'   )
#'
#' conv <- from_tab(format_data)
#'
#' # Utilisation :
#'
#' # soit un jeu de donnees contenant des codes a convertir en libelles
#' donnees <-
#'   data.frame(
#'     VAR1_CODE = c("1", "2", "3", "1", "Z"),
#'     stringsAsFactors = FALSE # la conversion ne marche pas sur des facteurs
#'   )
#'
#' # pour remplacer les codes par les libelles (pour VAR1)
#' donnees$VAR1_LIB <- conv$v1fmt[donnees$VAR1_CODE]
#'
#' donnees

from_tab <- function(sas_data) {

  # TODO :
  # [ ] other/HLO (voir specif)

  stopifnot(is.data.frame(sas_data))

  # transforme colonne facteurs en caractere
  fct <- vapply(sas_data, is.factor, TRUE)
  sas_data[fct] <- lapply(sas_data[fct], as.character)

  # noms en majuscule pour eviter tout probleme de casse
  names(sas_data) <- toupper(names(sas_data))

  # verifs
  needed <- c("FMTNAME", "START", "LABEL", "TYPE")
  if (any(miss <- !needed %in% names(sas_data))) {
    stop("\nColonne(s) manquante(s) : ", paste(needed[miss], collapse = ", "))
  }
  if ("END" %in% names(sas_data) && any(sas_data$START != sas_data$END)) {
    stop("les colonnes START et END doivent etre egales")
  }

  # exclut formats numériques
  sas_data_chr <- sas_data[sas_data$TYPE == "C", ]
  if (nrow(sas_data_chr) != nrow(sas_data)) {
    warning("formats hors type caractere (C) exclus")
  }

  # creation liste
  with(
    sas_data_chr,
    tapply(setNames(as.vector(LABEL), START), FMTNAME, list)
  )

}
