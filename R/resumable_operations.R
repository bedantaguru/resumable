

resumable_operations <- function(){
  lo <- list()

  lo$list_cached_args <- function(rfun){
    "This is to list cached arguments"
    ro_list_cached_args(rfun)
  }

  lo$list_cached_function_values <- function(rfun){
    "This is for listing cached values"
    ro_list_cached_function_values(rfun)
  }

  lo$eraser <- function(rfun){
    "It will return another resumable function which will remove cached values"
    ro_eraser(rfun)
  }

  lo$forget <- function(rf_exp){
    "Either function or a function with argument can be passed to reset"
    ro_forget(enquote(rf_exp), environment())
  }

  lo$aggregate <- function(rfun, aggregator = lapply){
    "It will aggregate all cached output by the resumable function"
    ro_aggregate(rfun, aggregator)
  }

  lo$destroy <- function(rfun){
    "It will destroy resumable function folder"
    ro_destroy(rfun)
  }

  lo$transfer <- function(rfun, dest_oc, new_root){
    "It will transfer resumable function to new OC or new root_path"
    ro_transfer(rfun, dest_oc, new_root)
  }

  lo$convert <- function(rfun, new_oc){
    "This will convert the storage backend"
    ro_convert(rfun, new_oc)
  }
  lo
}

ro_list_cached_args <- function(resf){
  ee <- environment(resf)
  if(isTRUE(ee$`_fun_with_meta`)){
    ee$`_fun_meta_list_args`()
  }else{
    ee$`_fun_oc`$list_keys()
  }
}

ro_list_cached_function_values <- function(resf){
  ee <- environment(resf)
  if(isTRUE(ee$`_fun_with_meta`)){
    lv <- ee$`_fun_meta_list_args`(values = TRUE)
  }else{
    lv <- ee$`_fun_oc`$list_values()
  }
  lapply(lv, `[[`, "value")
}

ro_eraser <- function(resf){

  # this is {patch}-style edit which is very strict and will change with changes
  # in resumable

  # remove meta
  body(resf)[[14]][[3]][[3]][[3]] <- substitute({meta$remove()})
  # change get to remove
  body(resf)[[14]][[3]][[4]][[3]][[2]][[3]][[1]][[3]] <- substitute(remove)
  # change execution part
  body(resf)[[15]][[3]] <- substitute("The argument passed is not cached!")
  # no return
  body(resf)[[16]] <- substitute(invisible(0))

  # modify info for printing
  attr(resf, "resumable")<- TRUE
  attr(resf, "resumable_eraser")<- TRUE
  class(resf) <- c("resumable","function")
  resf
}

ro_forget <- function(rf_exp, env = environment()){
  fe <- substitute(rf_exp, env)
  fec <- deparse(fe)
  if(grepl("\\(",fec)){
    # remove specific argument
    de <- substitute(ro_eraser(f)(arg))
    de[[1]][[2]] <- fe[[1]]
    de[[2]] <- fe[[2]]
    eval(de)
  }else{
    rf_get <- eval(fe)
    # reset whole function
    if(is_resumable(rf_get) & is.function(rf_get)){
      ee <- environment(rf_get)
      ee$`_fun_oc`$reset()
    }
  }
}

ro_aggregate <- function(resf, aggregator = lapply){
  ee <- environment(resf)
  aggregator(
    ee$`_fun_oc`$list_values(),
    identity)
}

ro_destroy <- function(resf){
  ee <- environment(resf)
  ee$`_fun_oc`$destroy()
  unlink(ee$`_fun_root_path`, recursive = TRUE, force = TRUE)
  invisible(0)
}

ro_transfer <- function(resf, dest_oc, new_root){
  if(missing(dest_oc) & missing(new_root)){
    stop("Either dest_oc or new_root has to be specified.", call. = FALSE)
  }
  if(!missing(new_root)){
    if(new_root==environment(resf)$`_fun_root_path`){
      stop("New root_path is same with supplied root_path!" ,call. = FALSE)
    }
    # in this case new root
    new_res_f <- resumable(environment(resf)$`_fun`, root_path = new_root)
  }else{
    # simple use old root
    new_res_f <- resf
  }

  if(!missing(dest_oc)){
    # in this case underlying oc gets changed (value gets copied)

    if(!is_object_cache(dest_oc)){
      stop("Supplied dest_oc is not an object_cache!", call. = FALSE)
    }

    old_oc <- environment(resf)$`_fun_oc`
    if(!missing(new_root)) environment(new_res_f)$`_fun_oc`$destroy()
    environment(new_res_f)$`_fun_oc` <- dest_oc
    object_cache_copy(old_oc, dest_oc)
    old_oc$destroy()

  }else{
    # in this case old oc is used on new oc (value gets moved if required)
    old_oc <- environment(resf)$`_fun_oc`
    environment(new_res_f)$`_fun_oc`$destroy()
    environment(new_res_f)$`_fun_oc` <- environment(resf)$`_fun_oc`
    environment(new_res_f)$`_fun_oc`$relocate(
      move_to = environment(new_res_f)$`_fun_oc_path`)

  }

  unlink(environment(resf)$`_fun_root_path`, recursive = TRUE, force = TRUE)

  return(new_res_f)

}

ro_convert<-function(resf, new_oc){

  tfoc <- tempfile(pattern = "temp_rf_oc")

  if(is.function(new_oc)){
    new_oc <- new_oc(tfoc)
  }else{
    if(is.character(new_oc)){
      altn <- alternatives("object_cache")
      usable_altn <- altn[altn$is_usable,]
      if(!(new_oc %in% usable_altn$name)){
        stop(paste0("The alternative (for object_cache) named:", new_oc,
                    " is not available!"), call. = FALSE)
      }
      new_oc <- object_cache(tfoc, use = new_oc)
    }else{
      stop(
        paste0("The argument new_oc ",
               "should be either a object_cache usable alternative name,",
               " or it can be a function which generates an object_cache ",
               "taking path as argument"),
        call. = FALSE
      )
    }
  }

  tf <- tempfile(pattern = "temp_rf")

  resf_new <- ro_transfer(resf, dest_oc = new_oc, new_root = tf)
  resf_final <- ro_transfer(resf_new,
                            new_root = environment(resf)$`_fun_root_path`)
  resf_final

}
