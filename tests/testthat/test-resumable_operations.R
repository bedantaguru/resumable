test_that("resumable_operations works", {
  ro <- resumable()

  f0 <- function(x){
    print("calc")
    x^2
  }

  fr <- resumable(f0)

  capture_output(lapply(2:5,fr))

  ro$list_cached_args(fr)

  expect_equal(2:5,
               sort(unique(unlist(ro$list_cached_args(fr)))))

  expect_equal((2:5)^2,
               sort(unique(unlist(ro$list_cached_function_values(fr)))))

  expect_equal(
    (as.numeric(unlist(ro$list_cached_args(fr))))^2,
    as.numeric(unlist(ro$list_cached_function_values(fr))))

  fr2 <- resumable(f0, with_meta_info = TRUE)

  capture_output(lapply(6:12,fr2))

  expect_output(print(fr2, details = TRUE),
                paste0(
                  "With meta info. \\(few keys can be obsolete\\)","\n",
                  "With 7 cached arguments","\n",
                  "Glimpse of the function"
                ))

  expect_equal(6:12,
               sort(unique(unlist(ro$list_cached_args(fr2)))))

  expect_equal((6:12)^2,
               sort(unique(unlist(ro$list_cached_function_values(fr2)))))

  expect_equal(
    (as.numeric(unlist(ro$list_cached_args(fr2))))^2,
    as.numeric(unlist(ro$list_cached_function_values(fr2))))


  frr <- ro$eraser(fr)

  expect_output(
    print(frr),
    "Eraser Function"
  )

  lapply(50:60, frr)

  expect_equal(
    sort(unique(unlist(ro$list_cached_args(frr)))),
    2:5
  )

  expect_equal(
    ro$list_cached_args(frr),
    ro$list_cached_args(fr)
  )

  frr(5)

  expect_equal(
    sort(unique(unlist(ro$list_cached_args(frr)))),
    2:4
  )

  expect_output(fr(5),"calc")


  ro$forget(fr(3))

  expect_output(fr(3),"calc")
  expect_failure(expect_output(fr(2),"calc"))


  ro$forget(fr)

  expect_output(
    print(fr, details = TRUE),
    "With 0 cached arguments"
  )

  all_backends <- alternatives(object_cache)
  all_backends <- all_backends[all_backends$is_usable,]

  if(nrow(all_backends)>1){

    now_bk <- all_backends[all_backends$in_use,]

    fr3 <- ro$convert(fr, now_bk$name)

  }

})
