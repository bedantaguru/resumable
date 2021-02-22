#' @export
resumable <- function(fun,
                      root_path,
                      env = environment(fun),
                      eval_args_before_caching = TRUE,
                      impactless_args = NULL,
                      clean_root_path_on_creation = FALSE) {

  if(is_available("rlang")){
    fun <- rlang::as_function(fun)
  }

  if(!is.function(fun)){
    stop("resumable is meant for a function.", call. = FALSE)
  }

  if(is_resumable(fun)){
    cat("Already resumable\n")
    return(fun)
  }

  fun_formals <- formals(fun)

  # re-construct resumable function
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


    if(encl_env$`_fun_oc`$key_exists(final_args)){
      out <- encl_env$`_fun_oc`$get(final_args)
    }else{
      actualcall[[1L]] <- encl_env$`_fun`
      out <- withVisible(eval(actualcall, parent.frame()))
      encl_env$`_fun_oc`$set(final_args, out)
    }

    if (out$visible) {
      out$value
    } else {
      invisible(out$value)
    }
  }

  formals(fmod) <- fun_formals
  attr(fmod, "resumable") <- TRUE

  fh <- function_hash(fun)

  if(missing(root_path)){
    root_path <- tempfile(pattern = "resumable_")
  }

  if(clean_root_path_on_creation){
    unlink(root_path, recursive = TRUE)
  }

  path <- file.path(root_path, fh)

  foc <- object_cache(path)

  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  res_fun_env <- new.env(parent = env)
  res_fun_env$`_fun_oc` <- foc
  res_fun_env$`_fun_oc_path` <- path
  res_fun_env$`_fun` <- fun
  res_fun_env$`_fun_default_args` <- Filter(
    function(x) !identical(x, quote(expr = )), fun_formals)
  res_fun_env$`_fun_impactless_args` <- impactless_args
  res_fun_env$`_fun_eval_args` <- eval_args_before_caching




  environment(fmod) <- res_fun_env
  class(fmod) <- c("resumable","function")

  fmod

}


#' @export
print.resumable <- function(x, ..., details = FALSE) {
  cat("Resumable Function:\n")
  tryCatch(
    {
      if(details){
        oc <- environment(x)$`_fun_oc`
        ks <- oc$list_keys()
        cat(paste0("With ",length(ks)," cached arguments\n"))
      }
      print(environment(x)$`_fun`)
    },
    error = function(e){
      stop("No function found / session terminated!", call. = FALSE)
    })
}


is_resumable <- function(x){
  inherits(x,"resumable") & isTRUE(attr(x, "resumable"))
}
