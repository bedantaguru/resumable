test_that("basic things works", {

  all_backends <- alternatives(object_cache)

  f0 <- function(x){
    print("calc")
    x^2
  }

  for(bn in all_backends$name){
    alternatives(object_cache, use = bn)

    nowb <- alternatives(object_cache)

    # alternatives working as expected
    expect_equal(nowb$name[nowb$in_use], bn)

    fr <- resumable(f0, clean_root_path_on_creation = TRUE)

    # caching mechanism
    o1 <- capture_output(fr(2))
    o2 <- capture_output(fr(2))
    expect_true(grepl("calc", o1))
    expect_false(grepl("calc", o2))

    # value match with real function
    rnd <- round(runif(3)*100)+2
    capture_output(
      expect_equal(fr(rnd), f0(rnd))
    )

  }

})

