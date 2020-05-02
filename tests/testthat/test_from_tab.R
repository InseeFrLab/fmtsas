context("from_tab")

test_that("example", {

  test_tab <-
    data.frame(
      FMTNAME = c("v1fmt", "v1fmt", "v1fmt", "v2fmt", "v2fmt"),
      TYPE    = c(    "C",     "C",     "C",     "C",     "C"),
      START   = c(    "1",     "2",     "3",     "A",     "B"),
      END     = c(    "1",     "2",     "3",     "A",     "B"),
      LABEL   = c("Lib 1", "Lib23", "Lib23", "Lib A", "Lib B")
    )

  expect_equal(
    from_tab(test_tab),
    list(
      v1fmt = fmtsas_c(c("1" = "Lib 1", "2" = "Lib23", "3" = "Lib23")),
      v2fmt = fmtsas_c(c("A" = "Lib A", "B" = "Lib B"))
    )
  )

})

test_that("erreurs input", {

  expect_error(
    from_tab(data.frame()),
    "Colonne.s. manquante.s."
  )

  test_tab <-
    data.frame(
      FMTNAME = c("v1fmt"),
      TYPE    = c(    "C"),
      START   = c(    "1"),
      END     = c( "-UN-"), # != START
      LABEL   = c("Lib 1")
    )
   expect_error(
    from_tab(test_tab),
    "colonnes START et END doivent etre egales"
  )

})

test_that("formats non caractere", {

  test_tab <-
    data.frame(
      FMTNAME = c("v1fmt",  "fnum"),
      TYPE    = c(    "C",     "N"), # N
      START   = c(    "1",     "0"),
      LABEL   = c("Lib 1", "Lib 2")
    )
  expect_warning(
    res <- from_tab(test_tab),
    "formats hors type caractere"
  )
  expect_equal(
    res,
    list(v1fmt = fmtsas_c(c("1" = "Lib 1")))
  )

})

test_that("other", {

  test_tab <-
    data.frame(
      FMTNAME = c("v1fmt", "v1fmt", "v1fmt", "v2fmt", "v2fmt", "v2fmt"),
      TYPE    = c(    "C",     "C",     "C",     "C",     "C",     "C"),
      START   = c(    "1",     "2",     "3",     "A",     "B",      NA),
      END     = c(    "1",     "2",     "3",     "A",     "B",      NA),
      LABEL   = c("Lib 1", "Lib23", "Lib23", "Lib A", "Lib B", "ERROR"),
      HLO     = c(     "",      NA,      NA,  "RIEN",      NA,     "O")
    )

  expect_equal(
    from_tab(test_tab),
    list(
      v1fmt = fmtsas_c(c("1" = "Lib 1", "2" = "Lib23", "3" = "Lib23")),
      v2fmt = fmtsas_c(c("A" = "Lib A", "B" = "Lib B"), other = "ERROR")
    )
  )

})
