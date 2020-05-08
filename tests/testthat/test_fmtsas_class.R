# fmtsas_c --------------------------------------------------------------

context("fmtsas_c")

test_that("erreurs", {

  expect_error(
    fmtsas_c(1:2),
    "caractere"
  )

  expect_error(
    fmtsas_c(c("A" = "LIB_A", "LIB_B")),
    "tous les elements doivent avoir un nom"
  )

  expect_error(
    fmtsas_c(c("LIB_A", "LIB_B")),
    "tous les elements doivent avoir un nom"
  )

  expect_error(
    fmtsas_c(c("A" = "LIB_A", "B" = "LIB_B"), other = 4),
    "vecteur caractere de longueur 1"
  )

  expect_error(
    fmtsas_c(c("A" = "LIB_A", "B" = "LIB_B"), other = c("#", "?")),
    "vecteur caractere de longueur 1"
  )

})

test_that("constructeur", {

  expect_equal(
    fmtsas_c(c("A" = "LIB_A", "B" = "LIB_B")),
    structure(
      c("A" = "LIB_A", "B" = "LIB_B"),
      class = c("fmtsas_c", "fmtsas")
    )
  )

  expect_equal(
    fmtsas_c(c("A" = "LIB_A", "B" = "LIB_B"), other = "?"),
    structure(
      c("A" = "LIB_A", "B" = "LIB_B"),
      other = "?",
      class = c("fmtsas_c", "fmtsas")
    )
  )

})


# getters/setters ---------------------------------------------------------

conv1 <-
  fmtsas_c(
    c("A" = "LIB_A", "B" = "LIB_B")
  )

conv2 <-
  fmtsas_c(
    c("A" = "LIB_A", "B" = "LIB_B"),
    other = "?"
  )

context("other.fmtsas")

test_that("get", {

  expect_equal(other(conv1), NULL)

  expect_equal(other(conv2), "?")

})

test_that("set", {

  expect_error(
    other(conv1) <- 1,
    "vecteur caractere de longueur 1"
  )

  expect_error(
    other(conv1) <- 1:5,
    "vecteur caractere de longueur 1"
  )

  expect_error(
    other(conv1) <- c("ERR", "Inconnu"),
    "vecteur caractere de longueur 1"
  )

  expect_error(
    other(conv2) <- NA_integer_,
    "vecteur caractere de longueur 1"
  )

  other(conv1) <- NULL
  expect_null(attr(conv1, "other"))

  other(conv2) <- NULL
  expect_null(attr(conv2, "other"))

  other(conv1) <- "ERR"
  expect_equal(attr(conv1, "other"), "ERR")

  other(conv2) <- NA_character_
  expect_equal(attr(conv2, "other"), NA_character_)

  other(conv2) <- NA
  expect_equal(attr(conv2, "other"), NA_character_)

})
