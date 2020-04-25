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

switch_quote <- function(sas_pgm) {
  # intervertit guillemets simples et doubles

  temp_car <- "!~!" # peu de chance d'apparaitre dans pgm sas
  res <- gsub("\"", temp_car, sas_pgm)
  res <- gsub("'", "\"", res)
  gsub(temp_car, "'", res)

}
