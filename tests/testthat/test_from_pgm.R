# :::value_to_vect --------------------------------------------------------

context(":::value_to_vect")

test_that("pas de couples", {

  expect_null(value_to_vect(''))

  expect_null(value_to_vect('a'))

  expect_null(value_to_vect('0-4 = "N"')) # numérique

  expect_null(value_to_vect('A-D = "AD"')) # syntaxe pas gérée

})

test_that("un seul =", {

  expect_equal(
    value_to_vect('"a"="A"'),
    c("a" = "A")
  )

  # avec blancs surnuméraires
  expect_equal(
    value_to_vect('  "a"\n =\n  "A" '),
    c("a" = "A")
  )

  # virgule
  expect_equal(
    value_to_vect('"a1","a2"="A"'),
    c("a1" = "A", "a2" = "A")
  )

  # tiret
  expect_equal(
    value_to_vect('"a1"-"a2"="A"'),
    c("a1" = "A", "a2" = "A")
  )

})

test_that("plusieurs =", {

  expect_equal(
    value_to_vect('"a"="A" "b"="B"'),
    c("a" = "A", "b" = "B")
  )

  # virgules, tirets
  expect_equal(
    value_to_vect('"a1","a2"="A" "b1"-"b3"="B" "c"="C"'),
    c("a1" = "A", "a2" = "A", "b1" = "B", "b3" = "B", "c" = "C")
  )

  # virgules, tirets + blancs intercalés
  expect_equal(
    value_to_vect('
      "a1", "a2" = "A"
      "b1"-"b3"  = "B"
      "c"        = "C"
    '),
    c(
      "a1" = "A", "a2" = "A",
      "b1" = "B", "b3" = "B",
      "c" = "C"
    )
  )

})


# from_pgm ----------------------------------------------------------------

context("from_pgm")

test_that("exemple complet", {

  test_pgm <-
    'options mprint ;
     PROC FORMAT;
       VALUE $ sexe "1"="Homme" "2"="Femme" ;
       vALue vnum
         0-99 = "petit" 100-high = "grand" ;
       value $rega /* (geographie) */
         "11", "24" = "Métropole"
         "01", "02",
         "03"       = "Outre-mer" ;
       value $az "A"-"Z" = "AZ" ;
       value $az_ A-Z = "AZ" ;
     RUN;'

  expect_warning(
    res <- from_pgm(test_pgm),
    "numerique.+vnum$"
  )

  expect_warning(
    res <- from_pgm(test_pgm),
    "intervalles caracteres.+az$"
  )

  expect_equal(
    res,
    list(
      sexe = c("1" = "Homme", "2" = "Femme"),
      rega = c(
        "11" = "Métropole", "24" = "Métropole",
        "01" = "Outre-mer", "02" = "Outre-mer", "03" = "Outre-mer"
      ),
      az = c("A" = "AZ", "Z" = "AZ")
    )
  )

})

test_that("quote", {

  test_pgm <-
    "PROC FORMAT;
       VALUE $ sexe \"1\"=\"Homme\" \"2\"=\"Femme\" ;
       value $rega
         '11', '24' = 'Métropole'
         '01', '02', '03' = 'Outre-mer';
     RUN;"

  expect_equal(
    from_pgm(
      test_pgm,
      quote = "double" # [+]
    ),
    list(sexe = c("1" = "Homme", "2" = "Femme"))
  )

  expect_equal(
    from_pgm(
      test_pgm,
      quote = "simple" # [+]
    ),
    list(
      rega = c(
        "11" = "Métropole", "24" = "Métropole",
        "01" = "Outre-mer", "02" = "Outre-mer", "03" = "Outre-mer"
      )
    )
  )

})

test_that("other", {

  test_pgm <- 'VALUE $sexe "1"="Homme" "2"="Femme" other = "?"'

  expect_equal(
    from_pgm(test_pgm),
    list(sexe = c("1" = "Homme", "2" = "Femme", "." = "?"))
  )

  expect_equal(
    from_pgm(
      test_pgm,
      other = "#" # [+]
    ),
    list(sexe = c("1" = "Homme", "2" = "Femme", "#" = "?"))
  )

  expect_equal(
    from_pgm(
      paste(
        test_pgm,
        'VALUE $sexe_ "1"="Homme" "2"="Femme" other = "Inconnu"' # [+]
      ),
      other = "#"
    ),
    list(
      sexe  = c("1" = "Homme", "2" = "Femme", "#" = "?"),
      sexe_ = c("1" = "Homme", "2" = "Femme", "#" = "Inconnu")
    )
  )

})

test_that("source", {

  test_pgm <-
    c(
      "VALUE $ sexe \"1\"=\"Homme\" \"2\"=\"Femme\" ;",
       "value $rega",
       "  '11', '24' = 'Métropole'",
       "  '01', '02', '03' = 'Outre-mer';"
    )

  expect_equal(
    from_pgm(
      test_pgm,
      source = TRUE # [+]
    ),
    structure(
      list(sexe = c("1" = "Homme", "2" = "Femme")),
      source = paste(test_pgm, collapse = "\n")
    )
  )

})

test_that("vide", {

  expect_warning(
    from_pgm(character(0)),
    "aucun format .de type caractere."
  )

  expect_warning(
    from_pgm(""),
    "aucun format .de type caractere."
  )

  expect_warning(
    from_pgm("data t2 ; set t1 ; run"),
    "aucun format .de type caractere."
  )

  expect_warning(
    from_pgm('value $az_ A-Z = "AZ" ;'),
    "aucun format .de type caractere."
  )

})
