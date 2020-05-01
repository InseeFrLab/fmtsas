# :::rm_sas_comments ---------------------------------------------------------

context(":::rm_sas_comments")

test_that("type `/* */`", {

  expect_equal(
    rm_sas_comments("options /*1234*/mprint ;"),
    "options mprint ;"
  )

  # multiligne
  expect_equal(
    rm_sas_comments("options /*12\n34*/mprint ;"),
    "options mprint ;"
  )

  # plusieurs (lazy)
  expect_equal(
    rm_sas_comments("options /*1234*/mprint /*567*/;"),
    "options mprint ;"
  )

  # commentaire final (sans fermeture)
  expect_equal(
    rm_sas_comments("options mprint ;/*1234"),
    "options mprint ;"
  )

})

test_that("type `* ;`", {

  expect_equal(
    rm_sas_comments("*1234 ;options mprint ;"),
    "options mprint ;"
  )

  # plusieurs
  expect_equal(
    rm_sas_comments("*1234;options mprint ;*567; run ;"),
    "options mprint ; run ;"
  )

  # multiligne
  expect_equal(
    rm_sas_comments("*12\n34 ;options mprint ;"),
    "options mprint ;"
  )

  # commentaire final (sans fermeture)
  expect_equal(
    rm_sas_comments("options mprint ;*1234"),
    "options mprint ;"
  )

  expect_equal(
    rm_sas_comments('*value "a*b"="x";'),
    ""
  )

  expect_equal(
    rm_sas_comments('*value "a*b"="x";options mprint ;'),
    "options mprint ;"
  )

  skip("`;` ou `*` entre guillemets") ## a corriger

  expect_equal(
    rm_sas_comments('*value "a;b"="x";'),
    ""
  )

  expect_equal(
    rm_sas_comments('*value "a;b"="x";options mprint ;'),
    "options mprint ;"
  )



})

test_that("mix types", {

  expect_equal(
    rm_sas_comments("*1234 ;options mprint ;/*567*/ run ;"),
    "options mprint ; run ;"
  )

  # imbrication (1)
  expect_equal(
    rm_sas_comments("/*1234 ;*567 ;89*/options mprint ;"),
    "options mprint ;"
  )

  # imbrication (2)
  expect_equal(
    rm_sas_comments("*1234 /*options mprint*/;options mprint ;"),
    "options mprint ;"
  )

})


# :::switch_quote ---------------------------------------------------------

context(":::switch_quote")

test_that("switch_quote", {

  expect_equal(
    switch_quote("'a'b\"c\""),
    "\"a\"b'c'"
  )

  expect_equal(
    switch_quote("'a\"b\"c'"),
    "\"a'b'c\""
  )

})
