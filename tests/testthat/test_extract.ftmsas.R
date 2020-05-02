# `[.fmtsas_c` ----------------------------------------------------------

context("[.fmtsas_c")

conv <-
  fmtsas_c(
    c("A" = "LIB_A", "B" = "LIB_B"),
    other = "?"
  )

test_that("existe", {

  expect_equal(
    conv["A"],
    "LIB_A"
  )

  expect_equal(
    conv["B"],
    "LIB_B"
  )

  expect_equal(
    conv[c("A", "B")],
    c("LIB_A", "LIB_B")
  )

  expect_equal(
    conv[c("A", "B", "A")],
    c("LIB_A", "LIB_B", "LIB_A")
  )

})

test_that("other", {

  expect_equal(
    conv["Z"],
    "?"
  )

  expect_equal(
    conv[c("A", "Z")],
    c("LIB_A", "?")
  )

})

test_that("NA", {

  expect_equal(
    conv[NA_character_],
    "?"
  )

  expect_equal(
    conv[NA_character_, keep_na = TRUE],
    NA_character_
  )

})

test_that("exemple mix", {

  expect_equal(
    conv[c("A", "Z", NA, "B", "A")],
    c("LIB_A", "?", "?", "LIB_B", "LIB_A")
  )

  expect_equal(
    conv[c("A", "Z", NA, "B", "A"), keep_na = TRUE],
    c("LIB_A", "?", NA, "LIB_B", "LIB_A")
  )

})

test_that("i facteur", {

  expect_equal(
    conv[factor(c("A", "N", NA))],
    c("LIB_A", "?", "?")
  )

  expect_equal(
    conv[factor(c("A", "N", NA)), keep_na = TRUE],
    c("LIB_A", "?", NA)
  )

})

test_that("erreur", {

  expect_error(
    conv[1:3],
    "selection par noms"
  )

  expect_error(
    conv[-1],
    "selection par noms"
  )

  expect_error(
    conv[1000],
    "selection par noms"
  )

  expect_error(
    conv[c(TRUE, FALSE)],
    "selection par noms"
  )

})
