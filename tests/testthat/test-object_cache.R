test_that("trails works", {

  all_backends <- alternatives(object_cache)
  all_backends <- all_backends[all_backends$is_usable,]

  if(nrow(all_backends)<=1){
    skip("Not sufficient backends")
  }

  tf <- tempfile()
  oc1 <- object_cache(tf, use = "POS")

  oc1$set("hi","hello")

  for(bn in all_backends$name){
    if(bn=="POS"){
      expect_failure(expect_warning(
        oc2 <- object_cache(tf, use = bn)))
    }else{
      expect_warning(
        oc2 <- object_cache(tf, use = bn),
        "While the old object_cache in the same path is")
    }


    expect_equal(oc1$list_keys(), oc2$list_keys())
    expect_equal(oc1$list_values(), oc2$list_values())
    expect_equal(oc1$meta(), oc2$meta())
  }


  expect_equal(oc1$list_keys()[[1]], "hi")

  set.seed(1)

  for(bn in all_backends$name){

    oc1$destroy()

    oc1 <- object_cache(tf, use = bn)

    expect_failure(expect_warning(
      object_cache(tf, use = bn)))

    other_bn <- setdiff(all_backends$name, bn)
    other_bn <- sample(other_bn)[1]

    expect_warning(
      oc2 <- object_cache(tf, use = other_bn),
      "While the old object_cache in the same path is")

    if(bn!="thor"){
      expect_equal(oc1$list_keys(), oc2$list_keys())
      expect_equal(oc1$list_values(), oc2$list_values())
      expect_equal(oc1$meta(), oc2$meta())
    }

  }

})
