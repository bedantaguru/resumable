test_that("trails works", {

  f0 <- function(x){
    print("calc")
    x^2
  }

  tf <- tempfile()

  fr <- resumable(f0, tf)

  capture_output(lapply(1:5, fr))

  fr2 <- resumable(root_path = tf)

  unlink(list.files(tf, "fr", full.names = TRUE))

  fr3 <- resumable(root_path = tf)

  expect_true(
    length(
    list.files(tf, "fr", full.names = TRUE)) == 0)

  expect_failure(expect_output(lapply(1:5, fr)))
  expect_output(fr(6), "calc")
  capture_output(expect_equal(fr(5), f0(5L)))

  expect_failure(expect_output(lapply(1:6, fr2)))
  expect_output(fr2(7), "calc")
  expect_true(all.equal(fr2, fr))


  expect_failure(expect_output(lapply(1:7, fr3)))
  expect_error(fr3(8), "already cached")


})
