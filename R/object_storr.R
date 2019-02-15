


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



