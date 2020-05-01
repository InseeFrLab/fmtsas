# :::extract_put ----------------------------------------------------------

context(":::extract_put")

test_that("extract_put", {

  matrix_put <- function(data) {
    # (matrix avec parametres prerenseignes)
    matrix(data, ncol = 3, dimnames = list(NULL, c("new", "old", "fmt")))
  }

  # 0
  expect_equal(
    extract_put("options mprint ;"),
    matrix_put(character(0))
  )

  # 1
  expect_equal(
    extract_put("GEO2 = put(REG2016, $reg.) ;"),
    matrix_put(c("GEO2", "REG2016", "reg"))
  )

  # 2
  expect_equal(
    extract_put("
      GEO2 = put(REG2016, $reg.) ;
      A13=put(A21,$a13_.);
    "),
    matrix_put(c("GEO2", "A13", "REG2016", "A21", "reg", "a13_"))
  )

})


# :::stylise --------------------------------------------------------------

context(":::stylise")

test_that("style = dplyr", {

  expect_equal(
    stylise(
      c(new = "NEW", old = "OLD", fmt = "new_old"),
      fmt_list = "fmt",
      style = "dplyr"
    ),
    "NEW = fmt$new_old[OLD]"
  )

  expect_equal(
    stylise(
      c(new = "NEW", old = "OLD", fmt = "new_old"),
      fmt_list = "conv", # [+]
      style = "dplyr"
    ),
    "NEW = conv$new_old[OLD]"
  )

})

test_that("style = base", {

  expect_equal(
    stylise(
      c(new = "NEW", old = "OLD", fmt = "new_old"),
      fmt_list = "fmt",
      style = "base"
    ),
    "<donnees>$NEW <- fmt$new_old[<donnees>$OLD]"
  )

  expect_equal(
    stylise(
      c(new = "NEW", old = "OLD", fmt = "new_old"),
      fmt_list = "conv", # [+]
      style = "base"
    ),
    "<donnees>$NEW <- conv$new_old[<donnees>$OLD]"
  )

})


# convert_put -------------------------------------------------------------

context("convert_put")

test_pgm <- c(
  "data t1 ;",
  "  set t2 ;",
  "  GEO2 = put(REG2016, $reg.) ;",
  "  A13 = put(A21, $a13_.) ;",
  "run ;"
)

test_that("rien", {

  expect_error(
    convert_put("options mprint ;"),
    "aucune instruction.+put"
  )

})

test_that("style inconnu", {

  expect_error(
    convert_put("", style = "mutate"),
    "dplyr.+base"
  )

})

test_that("fmt_list / quiet", {

  # teste quiet = FALSE, pour les tests suivants vaudra toujours TRUE
  expect_warning(
    capture_output(
      convert_put(test_pgm)
    ),
    "le code genere ne pourra probablement pas s'executer"
  )

  expect_equal(
    capture_output(
      convert_put(
        test_pgm,
        fmt_list = "conv",
        quiet = TRUE
      )
    ),
    "mutate(\n  GEO2 = conv$reg[REG2016],\n  A13 = conv$a13_[A21]\n)"
  )

})

test_that("1 put", {

  expect_equal(
    capture_output(
      convert_put("GEO2 = put(REG2016, $reg.) ;", quiet = TRUE)
    ),
    "mutate(\n  GEO2 = <fmt>$reg[REG2016]\n)"
  )

})


test_that("example", {

  # dplyr
  expect_equal(
    capture_output(
      convert_put(
        test_pgm,
        style = "dplyr",
        quiet = TRUE
      )
    ),
    "mutate(\n  GEO2 = <fmt>$reg[REG2016],\n  A13 = <fmt>$a13_[A21]\n)"
  )

  # base
  expect_equal(
    capture_output(
      convert_put(
        test_pgm,
        style = "base",
        quiet = TRUE
      )
    ),
    paste0(
      "<donnees>$GEO2 <- <fmt>$reg[<donnees>$REG2016]\n",
      "<donnees>$A13 <- <fmt>$a13_[<donnees>$A21]"
    )
  )

})
