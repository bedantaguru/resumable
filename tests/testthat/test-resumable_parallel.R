test_that("parallel works", {

  cl <- parallel::makeCluster(2L)

  pids <- parallel::clusterApply(cl, 1:2, function(x) Sys.getpid())

  # make sure these are actually 3 process (including this process)
  expect_equal(length(unique(c(unlist(pids), Sys.getpid()))), 3)

  all_backends <- alternatives(object_cache)

  f1 <- function(x){
    print("calc")
    x^2
  }

  for(bn in all_backends$name){
    if(bn!="thor"){
      alternatives(object_cache, use = bn)

      fr <- resumable(f1, root_path = ".test",
                      clean_root_path_on_creation = TRUE)

      parallel::clusterApply(cl, 1:10, fr)

      oc <- environment(fr)$`_fun_oc`
      expect_equal(as.numeric(sort(unlist(oc$list_keys()))), 1:10)

      o0 <- capture_output(expect_equal(sapply(1:10, fr), (1:10)^2))
      o1 <- capture_output(fr(11))

      expect_true(grepl("calc", o1))
      expect_false(grepl("calc", o0))

    }
  }


})
