# ..............................................................................
# Specifications
# https://documentation.sas.com/?docsetId=proc&docsetTarget=p1upn25lbfo6mkn1wncu4dyh9q91.htm&docsetVersion=9.4
# ..............................................................................

# couple_regex
#
# Expression régulière pour détecter une association de modalités dans une proc
# format.
#
# _Fonction auxiliaire non exportée._
#
# `(?:\"[^"]*\"\\s*[,-]\\s*)*\"[^"]*\"\\s*=\\s*"[^"]*\"`
#
# Détectera :
#   - "A" = "LIBA"
#   - "A","B" = "LIBAB"
#   - "A"-"Z" = "LIBAZ"
#
# Insensible aux espace et sauts de lignes surnumeraires.

couple_regex <- '(?:"[^"]*"\\s*[,-]\\s*)*"[^"]*"\\s*=\\s*"[^"]*"'

# rm_sas_comments
#
# Supprime commentaires d'un programme SAS
#
# _Fonction auxiliaire non exportée._
#
# @param sas_pgm programme sas, vecteur de caractères de longueur 1.
#
# Gère pour l'instant les commentaires de type `/* ... */` (correctement) et
# `* ... ;` (mal).

rm_sas_comments <- function(sas_pgm) {

  # type /* ... */
  sas_pgm <- gsub("/\\*.+?(\\*/|$)", "", sas_pgm) # (+? pour laziness)

  # type * ... ;
  gsub("\\*[^;]+(;|$)", "", sas_pgm)
  # TODO
  #  [ ] pb si * ou ; dans chaine de caracteres exemple '*value "a;b"="x";'

}

# Intervertit guillemets simples et doubles
#
# @param sas_pgm programme sas, vecteur de caractères de longueur 1.

reverse_simple_double_quote <- function(sas_pgm) {
  # intervertit guillemets simples et doubles

  temp_car <- "^~^" # peu de chances d'apparaitre dans pgm sas
  res <- gsub("\"", temp_car, sas_pgm)
  res <- gsub("'", "\"", res)
  gsub(temp_car, "'", res)

}

# Transforme un format (programme) en vecteur
#
# Transforme un format sous forme de programme SAS en vecteur nommé.
#
# _Fonction auxiliaire non exportée._
#
# Gère plusieurs valeurs à gauche séparées par virgules, mais pas d'intervalles
# caractères ("A"-"B") car contenant un nombre indéfini de modalités possibles.
# Seules les deux bornes sont conservées dans le résultat (comme si tiret
# remplacé par virgule).
#
# @param value_txt chaîne de texte de la forme `value $nomformat "1","2" = "x"
#   "3" = "y"` (peut contenir espaces surnuméraires, sauts de lignes,
#   commentaires...)
#
# @return Un vecteur de type `c("1" = "x", "2" = "x", "3" = "y")`.

value_to_vect <- function(value_txt) {

  # extract equivalences par regex (detecte plusieurs valeurs à gauche)
  equiv <- unlist(
    stringr::str_extract_all(
      value_txt,
      sprintf("(%s)", couple_regex)
    )
  )

  mbs <- strsplit(equiv, "=")
  if (!length(mbs)) return(NULL) # probablement format numerique

  ins <-
    stringr::str_match_all(
      lapply(mbs, function(x) x[1]),
      '"([^"]+)"'
    )
  ins <- lapply(ins, function(x) x[ , 2])

  outs <-
    stringr::str_match(
      sapply(mbs, function(x) x[2]),
      '"([^"]+)"'
    )[ , 2]

  rep_outs <- rep(outs, lengths(ins))
  stats::setNames(rep_outs, unlist(ins))

}

#' Convertit un programme de formats SAS
#'
#' Convertit les données contenues dans les `proc format` d'un programme SAS en
#' une liste de vecteurs qui pourra servir à effectuer des conversions.
#'
#' La fonction ne recherche que les formats de type caractère (`value $nom`).
#' Les formats numériques sont ignorés (un message d'avertissement dresse une
#' liste de ces formats, s'ils sont présents).
#'
#' Le programme peut se présenter sous la forme d'une chaîne unique de
#' caractères mais aussi d'un vecteur de plusieurs chaînes (typiquement le
#' résultat de la lecture d'un fichier par [readLines]). Les commentaires,
#' espaces et sauts de lignes surnuméraires sont autorisés. La casse du code SAS
#' (majuscule ou minuscule) est sans importance. Le programme peut en outre
#' contenir autre chose que des `proc format`.
#'
#' La fonction détecte plusieurs valeurs séparées par des virgules à gauche du
#' signe `=`. En revanche, les intervalles de caractères (type `"A"-"Z"`) ne
#' sont pas gérés car ils contiennent un nombre indéfini de modalités possibles.
#' Les bornes de l'intervalle seront toutefois prises en compte (comme si
#' `"A","Z"` était écrit). La syntaxe sans les guillements (`A-Z`) est aussi
#' permise par SAS. Elle n'est **pas prise en compte** et ces intervalles seront
#' ignorés.
#'
#' Il est possible de choisir le type de guillemets (double ou simple) entourant
#' les valeurs. Un programme contenant un mélange de guillemets simples et
#' doubles ne détectera qu'un type et pas l'autre.
#'
#' Bien qu'elle ne puisse pas être utilisée directement pour faire des
#' conversions (nombre de modalités indéfini), la modalité SAS `other` (valeur
#' par défaut) est aussi sauvegardée dans le résultat de fonction. Le nom
#' associé est par défaut `"."`. Il peut-être modifié.
#'
#' @param sas_pgm un programme SAS sous la forme d'un vecteur de chaînes de
#'   caractères.
#' @param other nom associé à la modalité spéciale `other`, par défaut `"."`
#'   (voir section 'Details').
#' @param quote type de guillemet. SAS autorise deux types de guillemets pour
#'   décrire une chaîne de caractère. La fonction suppose que des guillemets
#'   doubles sont utilisés ("). Dans le cas contraire ('), spécifier
#'   `quote = "simple"`.
#' @param source conserver le code SAS dans un attribut `"source"` de l'objet
#'   en sortie.
#'
#' @return Une liste contenant autant d'éléments que de formats si les données
#'   avaient été générées par SAS via une `proc format`.
#'   - les noms de la liste correspondent aux noms des formats (`value $...`) ;
#'   - les éléments de la liste sont des vecteurs contenant les relations entre
#'   valeurs initiales et valeurs converties.
#'
#'   Voir les exemples pour l'utilisation de cette liste.
#'
#' @export
#'
#' @seealso [from_tab] pour importer les formats contenus dans une table SAS.
#'
#' @examples
#' test_pgm <- readLines(
#'   system.file("extdata", "pgm_format_test.sas", package = "fmtsas")
#' )
#' cat(test_pgm, sep = "\n")
#' conv <- from_pgm(test_pgm)
#'
#' # Utilisation :
#'
#' # soit un jeu de donnees contenant des codes a convertir en libelles
#' donnees <-
#'   data.frame(
#'     ACT_CODE = c("B", "C", "I", "W", "H"),
#'     stringsAsFactors = FALSE # la conversion ne marche pas sur des facteurs
#'   )
#'
#' # pour remplacer les codes par les libelles (pour ACT_CODE)
#' donnees$ACT_LIB <- conv$a13_[donnees$ACT_CODE]
#'
#' donnees

from_pgm <- function(sas_pgm,
                     other = ".",
                     quote = c("double", "simple"),
                     source = FALSE) {

  quote <- match.arg(quote)

  # collapse si vecteur longueur > 1
  sas_pgm <- paste(sas_pgm, collapse = "\n")
  if (source) orig <- sas_pgm

  # intervertit simples et doubles guillemets si besoin
  if (quote == "simple") {
    sas_pgm <- reverse_simple_double_quote(sas_pgm)
  }

  # remplace modalite speciale `other` par valeur definie par utilisateur
  sas_pgm <-
    stringr::str_replace_all(
      sas_pgm,
      'other(\\s*=\\s*"[^"]*")',
      paste0('"', other, '"\\1')
    )

  # suppr comments
  sas_pgm_nocom <- rm_sas_comments(sas_pgm)

  # extrait noms des formats + couples separes par "="
  values <-
    stringr::str_match_all(
      sas_pgm_nocom,
      sprintf('(?i)value\\s+\\$\\s*(\\w+)\\s+((?:%s\\s*)+)', couple_regex)
    )[[1]]

  if (!length(values)) {
    warning("aucun format (de type caractere) dans sas_pgm")
  }

  fmtnames <- values[ , 2]
  couples  <- values[ , 3] # ensemble des couples (séparés plus loin)

  # applique value_to_vect a chaque "value"
  assoc <- lapply(couples, value_to_vect)

  # avertit numeriques (ignore)
  fmtnum <- stringr::str_match_all(sas_pgm_nocom, '(?i)value\\s+(\\w+)')[[1]]
  if (length(fmtnum)) {
    warning(
      "\nFormat(s) numerique(s) ignore(s) :\n  ",
      paste(fmtnum[ , 2], collapse = ", ")
    )
  }

  # avertit character range
  chr_range <- grepl('"[^"]*"\\s*-\\s*)*"[^"]*"\\s*=', couples)
  if (any(chr_range)) {
    warning(
      "\n",
      "Presence d'intervalles caracteres (\"A\"-\"Z\"). ",
      "Seules les bornes ont ete prises en compte :\n  ",
      paste(fmtnames[chr_range], collapse = ", ")
    )
  }

  res <- stats::setNames(assoc, fmtnames)
  if (source) attr(res, "source") <- orig

  res

}
