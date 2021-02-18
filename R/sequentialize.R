

#' @export
sequentialize <- function(fun, root_path, env = environment(fun)){

  if(is_available("rlang")){
    fun <- rlang::as_function(fun)
  }

  if(!is.function(fun)){
    stop("sequentialize is meant for a function.", call. = FALSE)
  }

  if(is_sequentialized(fun)){
    cat("Already sequentialized\n")
    return(fun)
  }

  fun_formals <- formals(fun)

  fmod <- function(...){
    actualcall <- match.call()
    encl_env <- parent.env(environment())

    fl <- filelock::lock(encl_env$`_fun_lockfile`)

    on.exit(filelock::unlock(fl))

    actualcall[[1L]] <- encl_env$`_fun`
    out <- withVisible(eval(actualcall, parent.frame()))


    if (out$visible) {
      out$value
    } else {
      invisible(out$value)
    }
  }

  formals(fmod) <- fun_formals
  attr(fmod, "sequentialized") <- TRUE

  # creation of lock file

  ff <- function_hash(fun)

  if(missing(root_path)){
    root_path <- tempfile(pattern = "sequentialize_")
  }

  dir.create(root_path, showWarnings = FALSE, recursive = TRUE)

  fflock <- file.path(root_path, ff)
  if(!file.exists(fflock)){
    file.create(fflock)
  }

  seq_fun_env <- new.env(parent = env)
  seq_fun_env$`_fun_lockfile` <- fflock
  seq_fun_env$`_fun` <- fun



  environment(fmod) <- seq_fun_env
  class(fmod) <- c("sequentialized","function")

  fmod

}

#' @export
print.sequentialized <- function(x, ...) {
  cat("Sequentialized Function:\n")
  tryCatch(
    {
      suppressMessages(
        suppressWarnings(
          fl <- filelock::lock(environment(x)$`_fun_lockfile`, timeout = 0)
        )
      )
      print(environment(x)$`_fun`)
      if(is.null(fl)){
        cat("Possibly in use.\n")
      }else{
        cat("Possibly Not in use.\n")
      }
      on.exit(filelock::unlock(fl))
    },
    error = function(e){
      stop("No function found / session terminated!", call. = FALSE)
    })
}

is_sequentialized <- function(x){
  inherits(x,"sequentialized") & isTRUE(attr(x, "sequentialized"))
}
