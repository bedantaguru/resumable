#' Create a resumable function
#'
#' @param .f a function or a formula similar to what is used in purrr package
#' @param path a local path where and object_storr will be created for storing the function outcomes
#' @param enable_functions_footprinting Logical value. If set to \code{TRUE} the resumable function will be liked to source code of the input function. Default value in \code{FALSE}.
#' @param trace_only Logical value. If set to \code{TRUE} the resumable function does not store any output (just NULL or 0). It may be useful for situation where only extrenal activity is performed. Default value in \code{FALSE}.
#' @param clean_on_start Logical value. If set to \code{TRUE} then the cached values will be cleaned on start. Default value in \code{FALSE}.
#' @param skip_if a function or a formula similar to what is used in purrr package which should return a locgical scaler. If this returns \code{TRUE} the value will not be stored. Ignore if not required.
#' @param details Logical value indicating whether the object_storr and resumable function both are required or not. Default value in \code{FALSE}.
#' @param ... other params to be passed to \code{.f} (at present this is not working properly. Directly use purrr type formula.)
#'
#' @return If \code{details} is \code{TRUE} it returns a list with resumable function and corresponding object_storr. Otherwise it will return only resumable function.
#' @details This function is fully compatible in parallel processing. This is similar to \link[storr]{storr_external}. However, this is applicable to arbitrary user defined functions. Note here numeric and integer will be treated as different entity as input. (Means f(10) and f(10L) (f is a resumable function) both will evalute first even though the input function is indifferent.)
#' @export
#'
#' @examples
#' slow <- function(x){
#'   print("calc")
#'   x^2
#' }
#'
#' slowr <- resumable(slow, "test")
#'
#' # first time the values will be caluculated and stored
#' purrr::map(seq(10), slowr)
#'
#' # this time the cached values are coming
#' purrr::map(seq(10), slowr)
#'
#'
#' @seealso \link[storr]{storr_external}
resumable <- function(.f,
                      path,
                      enable_functions_footprinting = F,
                      trace_only = F,
                      clean_on_start = F,
                      skip_if,
                      details = F,
                      ...) {
  str <- get_object_storr(path)

  if (clean_on_start) {
    str$destroy()
    str <- get_object_storr(path)
  }
  .f <- purrr::as_mapper(.f, ...)
  # check for no argument function
  empty_arg_function <- F
  fer<-function(){}
  if(identical(args(fer), args(.f))){
    empty_arg_function <- T
  }

  if (enable_functions_footprinting) {
    ff <- str$hash_object(.f)
  } else{
    ff <- str$storr_details$key_storr$default_namespace
  }

  if (!missing(skip_if)) {
    skip_if <- purrr::as_mapper(skip_if)
  } else{
    skip_if <- function(...)
      FALSE
  }

  .f_resume <- function(...) {
    x <- list(...)
    if(empty_arg_function) x <- "NULL"

    if (str$exists(x, ff)) {
      out <- str$get(x, ff)
    } else{

      if(empty_arg_function){
        out <- .f()
      }else{
        out <- do.call(.f, x)
      }

      if (!skip_if(out)) {
        if (trace_only) {
          str$set(x, 0, ff)
        } else{
          str$set(x, out, ff)
        }
      }

    }
    out
  }

  .f_mod <- function(...) {
    .f_resume(...)
  }

  if (details) {
    .ls_key <- function() {
      str$list(namespace = ff)
    }
    list(
      f = .f_mod,
      st = str,
      ns = ff,
      ls_cached = function()
        .ls_key()
    )
  } else{
    .f_mod
  }

}
