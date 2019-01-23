


#' get an object_storr
#'
#' @param path the local path where object_storr will be created
#'
#' @return The function returns a storr like object (a list) with less functions and a node named "storr_details" which contains two storr_rds objects used for object_storr
#' @export
#'
#' @examples
#' st <- get_object_storr("test")
#' st$set(sin, cos)
#' st$list()
#' st$get(sin)(pi)
#' st$destroy()
#'
#'
#' @seealso \link[storr]{storr_rds}
get_object_storr <- function(path) {
  dir.create(path, showWarnings = F)

  stk <- storr::storr_rds(file.path(path, "keys"))
  stv <- storr::storr_rds(file.path(path, "values"))

  st <- list()

  st$exists <- function(key, namespace) {
    if (missing(namespace))
      namespace <- stv$default_namespace
    key_h <- stv$hash_object(key)
    stv$exists(key_h, namespace) & stk$exists(key_h, namespace)
  }

  st$set <- function(key, value, namespace) {
    if (missing(namespace))
      namespace <- stv$default_namespace
    key_h <- stv$hash_object(key)
    stk$set(key_h, key, namespace)
    stv$set(key_h, value, namespace)
  }

  st$get <- function(key, namespace) {
    if (missing(namespace))
      namespace <- stv$default_namespace
    key_h <- stv$hash_object(key)
    stv$get(key_h, namespace)
  }

  st$list <- function(namespace) {
    if (missing(namespace))
      namespace <- stv$default_namespace
    stk$mget(stv$list(namespace), namespace)
  }

  st$del <- function(key, namespace) {
    if (missing(namespace))
      namespace <- stv$default_namespace
    key_h <- stv$hash_object(key)
    stk$del(key_h, namespace)
    stv$del(key_h, namespace)
  }

  st$destroy <- function() {
    stk$destroy()
    stv$destroy()
    unlink(path, recursive = T, force = T)
  }

  st$hash_object <- stv$hash_object

  st$storr_details <- list(key_storr = stk, value_storr = stv)

  return(st)

}



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

  .f_resume <- function(x) {
    if (str$exists(x, ff)) {
      out <- str$get(x, ff)
    } else{
      out <- .f(x)

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

  .f_mod <- function(x) {
    .f_resume(x)
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
