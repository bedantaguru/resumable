test_that("basic things works", {

  all_backends <- alternatives(object_cache)
  all_backends <- all_backends[all_backends$is_usable,]

  f0 <- function(x){
    print("calc")
    x^2
  }

  f0_NL <- function(x){
    print("calc")
    if(x==1){
      NULL
    }else{
      if(x>0.01){
        Sys.sleep(x)
      }
      x
    }
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

    # if root path is deleted it should be able to recreate it
    # assuming all cached values are lost (this is only for two inbuilt)

    if(bn %in% c("POS","SF")){
      tf <- tempfile()

      fr1.2 <- resumable(f0,
                         root_path = tf,
                         clean_root_path_on_creation = TRUE)

      expect_output(fr1.2(1), "calc")

      unlink(tf, recursive = TRUE, force = TRUE)

      expect_false(dir.exists(tf))
      expect_output(fr1.2(1), "calc")
      expect_failure(expect_output(fr1.2(1), "calc"))

    }

    # check skip if

    if(is_available("rlang")){
      fr2.1 <- resumable(f0_NL, skip_if = ~is.null(.x))
    }else{
      fr2.1 <- resumable(
        f0_NL,
        skip_if = function(fval, fevaltime) is.null(fval))
    }

    o1 <- capture_output(fr2.1(1))
    o2 <- capture_output(fr2.1(1))
    expect_true(grepl("calc", o1))
    expect_true(grepl("calc", o2))

    # normal things works normally
    o1 <- capture_output(fr2.1(0.01))
    o2 <- capture_output(fr2.1(0.01))
    expect_true(grepl("calc", o1))
    expect_false(grepl("calc", o2))

    # eval time based
    # for time save test this only for POS
    if(bn=="POS"){
      fr2.2 <- resumable(
        f0_NL,
        skip_if = function(fval, fevaltime){
          fevaltime<0.1
        })

      o1 <- capture_output(fr2.2(10^-10))
      o2 <- capture_output(fr2.2(10^-10))
      expect_true(grepl("calc", o1))
      expect_true(grepl("calc", o2))

      # normal things works normally
      o1 <- capture_output(fr2.1(0.3))
      o2 <- capture_output(fr2.1(0.3))
      expect_true(grepl("calc", o1))
      expect_false(grepl("calc", o2))
    }

    # few other tests only for POS (base)
    if(bn == "POS"){
      # test no_function_footprint
      # simple root_path linking (user will take care function body changes)
      if(file.exists(".test")){
        unlink(normalizePath(".test"), recursive = TRUE, force = TRUE)
      }

      fr3.1 <- resumable(f0,
                         no_function_footprint = TRUE,
                         root_path = ".test")

      # call once to save
      expect_output(fr3.1(1), "calc")

      fr3.2 <- resumable(f0_NL,
                         no_function_footprint = TRUE,
                         root_path = ".test")
      o1 <- capture_output(fr3.2(1))
      expect_false(grepl("calc", o1))
      expect_equal(fr3.2(1), 1)

      # call to save
      expect_output(fr3.2(0.01), "calc")

      o1 <- capture_output(fr3.1(0.01))
      expect_false(grepl("calc", o1))
      expect_equal(fr3.1(0.01), 0.01)
      capture_output(expect_true(f0(0.01)!=fr3.1(0.01)))

      unlink(normalizePath(".test"), recursive = TRUE, force = TRUE)
    }


  }

})

