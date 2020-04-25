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
