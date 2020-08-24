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
#' La modalité SAS `other` (valeur par défaut), présente dans la table si une
#' colonne `HLO` existe et vaut `"O"`, est sauvegardée dans l'attribut
#' "other" pour les éléments de la liste concernés.
#'
#' @param sas_data un data.frame importé depuis une table SAS pour proc format.
#'   Ce data.frame doit contenir a minima les colonnes `FMTNAME`, `START`,
#'   `LABEL` et `TYPE` (majuscule ou minuscule).
#'
#' @return Une liste contenant autant d'éléments que de formats si les données
#'   avaient été générées par une proc format.
#'   - les noms de la liste correspondent aux noms des formats (`FMTNAME`) ;
#'   - les éléments de la liste sont des vecteurs contenant les relations entre
#'   valeurs initiales et valeurs converties ;
#'   - chaque élément a un éventuel attribut `"other"` ;
#'   - chaque élément est un objet de type [`fmtsas_c`], ce qui permet
#'     d'utiliser l'[opérateur de sélection][extract.fmtsas_c] avec prise en
#'     compte d'une valeur par défaut (`other`).
#'
#'   Voir les exemples pour l'utilisation de cette liste.
#'
#' @importFrom stats setNames
#' @export
#'
#' @seealso [from_pgm] pour importer les formats contenus dans un programme SAS.
#'
#' @examples
#' ## Donnees en entree (provenant d'une table SAS) :
#'
#' format_data <-
#'   data.frame(
#'     FMTNAME = c( "fmt1_", "fmt1_", "fmt1_", "fmt1_",  "sexe",  "sexe"),
#'     TYPE    = c(     "C",     "C",     "C",     "C",     "C",     "C"),
#'     START   = c(     "A",     "B",     "C",      NA,     "1",     "2"),
#'     LABEL   = c(     "A",    "BC",    "BC", "ERROR", "Homme", "Femme"),
#'     HLO     = c(      NA,      NA,      NA,     "O",      NA,      NA)
#'   )
#'
#' ## Conversion des formats :
#'
#' conv <- from_tab(format_data)
#' conv
#'
#' ## Utilisation :
#'
#' # soit un jeu de donnees contenant des codes a convertir en libelles
#' donnees <-
#'   data.frame(
#'     VAR1_CODE = c("A", "B", "A", "C", "Z",  NA),
#'     SEXE_CODE = c("1", "2", "1", "2",  NA, "Z")
#'   )
#'
#' # pour remplacer les codes par les libelles (pour VAR1)
#' donnees$VAR1_LIB  <- conv$fmt1_[donnees$VAR1_CODE]
#' donnees$VAR1_LIB2 <- conv$fmt1_[donnees$VAR1_CODE, keep_na = TRUE]
#' donnees$SEXE_LIB  <- conv$sexe[donnees$SEXE_CODE]
#'
#' donnees

from_tab <- function(sas_data) {

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
  if ("END" %in% names(sas_data)) {
    start_diff_end <-
      with(
        sas_data,
        any(START[!is.na(START)] != END[!is.na(START)])
      )
    if (start_diff_end) stop("les colonnes START et END doivent etre egales")
  }

  # exclut formats numériques
  sas_data_chr <- sas_data[sas_data$TYPE == "C", ]
  if (nrow(sas_data_chr) != nrow(sas_data)) {
    warning("formats hors type caractere (C) exclus")
  }

  # other (HLO)
  fmtnames <- unique(sas_data_chr$FMTNAME)
  others <-
    setNames(
      rep(NA_character_, length(fmtnames)),
      fmtnames
    )
  others_hlo <- character(0)
  if (any(names(sas_data_chr) == "HLO")) {
    others_hlo <-
      with(
        sas_data_chr[with(sas_data_chr, !is.na(HLO) & HLO == "O"), ],
        setNames(LABEL, FMTNAME)
      )
  }
  others[names(others_hlo)] <- others_hlo

  # creation liste
  res <-
    with(
      sas_data_chr,
      tapply(
        setNames(as.vector(LABEL), START),
        FMTNAME,
        function(x) list(x[!is.na(names(x))]) #(!is.na retire possibles `other`)
      )
    )

  # ajouts attributs other
  lapply(
    setNames(names(res), names(res)),
    function(nm) {
      oth <- unname(others[nm])
      fmtsas_c(
        res[[nm]],
        other = if (is.na(oth)) NULL else oth
      )
    }
  )

}
