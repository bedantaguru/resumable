#' @export
resumable <- function(fun,
                      root_path,
                      env = environment(fun),
                      eval_args_before_caching = TRUE,
                      impactless_args = NULL,
                      clean_root_path_on_creation = FALSE,
                      no_function_footprint = FALSE,
                      skip_if = function(
                        fun_val,
                        fun_eval_time){FALSE},
                      obsolete_if = function(
                        create_time,
                        num_times_used,
                        last_used_time,
                        fun_val,
                        fun_eval_time){FALSE}) {

  if(missing(fun)){
    # No functions supplied returning resumable operations
    return(resumable_operations())
  }

  if(is_available("rlang")){
    fun <- rlang::as_function(fun)
    skip_if <- rlang::as_function(skip_if)
    obsolete_if <- rlang::as_function(obsolete_if)
  }

  if(!is.function(fun)){
    stop("resumable is meant for a function.", call. = FALSE)
  }

  lval <- as.list(body(obsolete_if))
  lval <- lval[[length(lval)]]

  with_meta_info_in_oc <- TRUE
  if(lval==FALSE){
    with_meta_info_in_oc <- FALSE
  }

  if(is_resumable(fun)){
    cat("Already resumable\n")
    return(fun)
  }

  fun_formals <- formals(fun)

  # re-construct resumable function
  # Note: this has to sync ro_eraser edits
  fmod <- function(...){
    actualcall <- match.call()
    encl_env <- parent.env(environment())

    f_called_args <- as.list(actualcall)[-1]

    f_default_args <- encl_env$`_fun_default_args`

    f_default_args <- f_default_args[
      setdiff(names(f_default_args), names(f_called_args))
    ]

    f_called_args[
      intersect(encl_env$`_fun_impactless_args`, names(f_called_args))
    ] <- NULL
    f_default_args[
      intersect(encl_env$`_fun_impactless_args`, names(f_default_args))
    ] <- NULL

    if(encl_env$`_fun_eval_args`){
      final_args <- c(lapply(f_called_args, eval, parent.frame()),
                      lapply(f_default_args, eval, envir = environment()))

    }else{
      final_args <- c(f_called_args,
                      f_default_args)

    }

    with_meta <- FALSE
    meta <- list()
    if(encl_env$`_fun_with_meta`){
      with_meta <- TRUE
      # function with meta info
      meta <- encl_env$`_fun_meta_info`(final_args)
      check_obsolete_function <- encl_env$`_fun_meta_obsolete_if`
    }

    fresh_required <- FALSE

    if(encl_env$`_fun_oc`$key_exists(final_args)){
      fetch <- TRUE
      if(with_meta){
        minfo <- meta$get()
        is_obsolete <- check_obsolete_function(
          minfo$create_time,
          minfo$num_times_used,
          minfo$last_used_time,
          minfo$extra$val,
          minfo$extra$eval_time
        )
        if(is_obsolete){
          fetch <- FALSE
        }else{
          fetch <- TRUE
          meta$set(used = TRUE)
        }
      }

      if(fetch){
        out <- encl_env$`_fun_oc`$get(final_args)
        fresh_required <- FALSE
      }else{
        fresh_required <- TRUE
      }
    }else{
      fresh_required <- TRUE
    }

    if(fresh_required){
      actualcall[[1L]] <- encl_env$`_fun`
      etime <- system.time({
        et <- tryCatch(
          out <- withVisible(eval(actualcall, parent.frame())),
          error = function(e) e
        )
      })

      if(inherits(et, "error")) stop(et)

      skip_if_chk <- tryCatch({
        encl_env$`_fun_skip_if`(out$value, etime["elapsed"])
      }, error = function(e) FALSE)

      if(!is.logical(skip_if_chk)) skip_if_chk <- FALSE

      if(length(skip_if_chk)!=1) skip_if_chk <- isTRUE(skip_if_chk[1])

      if(!skip_if_chk){
        encl_env$`_fun_oc`$set(final_args, out)
        if(with_meta){
          meta$set(
            ext_info = list(val = out$value, eval_time = etime["elapsed"])
          )
        }
      }

    }

    if (out$visible) {
      out$value
    } else {
      invisible(out$value)
    }
  }

  formals(fmod) <- fun_formals

  if(no_function_footprint){
    fh <- "rf"
  }else{
    fh <- function_hash(fun)
  }


  if(missing(root_path)){
    root_path <- tempfile(pattern = "resumable_")
  }

  if(clean_root_path_on_creation){
    unlink(root_path, recursive = TRUE)
  }

  path <- file.path(root_path, fh)

  foc <- object_cache(path)

  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  if(with_meta_info_in_oc){

    # function for storing/accessing/comparing meta information
    meta_info <- function(arg){
      # this has to match with same expression as in meta_list_args
      argk <- list(`_args` = hash_it(arg))
      l <- list()

      get <- function(fresh = FALSE, ext_info = list()){
        if(foc$key_exists(argk) & !fresh){
          info <- foc$get(argk)
        }else{
          info <- list()
          info$create_time <- Sys.time()
          info$num_times_used <- 1
          info$last_used_time <- Sys.time()
          info$extra <- ext_info
        }
        info
      }

      l$set <- function(ext_info = list(), used = FALSE){
        info <- get(fresh = !used, ext_info = ext_info)
        if(!foc$key_exists(argk) | !used){
          foc$set(argk, info)
        }else{
          if(used){
            info$num_times_used <- info$num_times_used+1
            info$last_used_time <- Sys.time()
            foc$set(argk, info)
          }
        }
      }

      l$remove <- function(){
        if(foc$key_exists(argk)){
          foc$remove(argk)
        }
      }

      l$get <- get

      l
    }

    meta_list_args <- function(){
      ks <- foc$list_keys()
      ksh <- lapply(ks, hash_it)
      is_key <- lapply(ks, function(x){
        # this has to match with same expression as in meta_info
        argk <- list(`_args` = hash_it(x))
        any(ksh==hash_it(argk))
      })
      ks[unlist(is_key)]
    }

  }

  res_fun_env <- new.env(parent = env)
  res_fun_env$`_fun_root_path` <- root_path
  res_fun_env$`_fun_oc` <- foc
  res_fun_env$`_fun_oc_path` <- path
  res_fun_env$`_fun` <- fun
  res_fun_env$`_fun_default_args` <- Filter(
    function(x) !identical(x, quote(expr = )), fun_formals)
  res_fun_env$`_fun_impactless_args` <- impactless_args
  res_fun_env$`_fun_eval_args` <- eval_args_before_caching
  res_fun_env$`_fun_skip_if` <- skip_if
  res_fun_env$`_fun_with_meta` <- with_meta_info_in_oc

  if(with_meta_info_in_oc){
    res_fun_env$`_fun_meta_info` <- meta_info
    res_fun_env$`_fun_meta_list_args` <- meta_list_args
    res_fun_env$`_fun_meta_obsolete_if` <- obsolete_if
  }


  environment(fmod) <- res_fun_env
  class(fmod) <- c("resumable","function")
  attr(fmod, "resumable") <- TRUE

  fmod

}


head_and_tail_of_fn <- function(fun, th_n = 5){
  bd <- deparse(fun)
  if(length(bd)>th_n+3){
    bd <- bd[c(1:3,3,(length(bd)-2:0))]
    bd[4] <- "..."
    bd
  }else{
    bd
  }
}


#' @export
print.resumable <- function(x, ..., details = FALSE) {
  if(isTRUE(attr(x, "resumable_eraser"))){
    cat("Resumable Remover / Eraser Function:\n")
  }else{
    cat("Resumable Function:\n")
  }

  tryCatch(
    {
      if(details){
        if(environment(x)$`_fun_with_meta`){
          ks <- environment(x)$`_fun_meta_list_args`()
          cat("With meta info. (few keys can be obsolete)\n")
        }else{
          oc <- environment(x)$`_fun_oc`
          ks <- oc$list_keys()
        }
        cat(paste0("With ",length(ks)," cached arguments\n"))
        disp <- head_and_tail_of_fn(environment(x)$`_fun`)
        if(is_available("prettycode")){
          disp <- tryCatch(prettycode::highlight(disp),
                           error = function(e) disp)
        }
        cat("Glimpse of the function:\n")
        cat(disp, sep="\n")
      }else{
        if(is_available("prettycode")){
          pcns <- asNamespace("prettycode")
          pcns$print.function(environment(x)$`_fun`)
        }else{
          print(environment(x)$`_fun`)
        }
      }
    },
    error = function(e){
      stop("No function found / session terminated / cache corrupted!",
           call. = FALSE)
    })
}


is_resumable <- function(x){
  inherits(x,"resumable") & isTRUE(attr(x, "resumable"))
}
