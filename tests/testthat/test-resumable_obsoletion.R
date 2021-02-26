test_that("obsoletion works", {

  all_backends <- alternatives(object_cache)
  all_backends <- all_backends[all_backends$is_usable,]

  f0 <- function(x){
    print("calc")
    x^2
  }

  for(bn in all_backends$name){
    alternatives(object_cache, use = bn)
    fr <- resumable(f0, obsolete_if = function( ct, nu, lt, ...){
      nu > 2
    })

    expect_output(fr(5),"calc")
    expect_failure(expect_output(fr(5),"calc"))
    expect_failure(expect_output(fr(5),"calc"))
    expect_output(fr(5),"calc")
    expect_failure(expect_output(fr(5),"calc"))
    expect_failure(expect_output(fr(5),"calc"))
    expect_output(fr(5),"calc")



    if(is_available("rlang")){

      skip_on_cran()

      # based on time since creation
      fr <- resumable(
        f0,
        obsolete_if = ~difftime(Sys.time(),.x, units = "sec")>0.1)
      expect_output(fr(5),"calc")
      expect_failure(expect_output(fr(5),"calc"))
      Sys.sleep(0.15)
      expect_output(fr(5),"calc")
      expect_failure(expect_output(fr(5),"calc"))

    }


  }

})
