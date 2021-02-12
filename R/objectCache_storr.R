



# need to pur sequentialize here
get_object_storr <- function(
  path = tempfile(pattern = "object_storr"),
  key_storr = function(path){
    storr::storr_rds(file.path(path, "keys"))
  },
  value_storr =  function(path){
    storr::storr_rds(file.path(path, "values"))
  })
{


  stk <- NULL
  stv <- NULL

  create <- function(){
    dir.create(path, showWarnings = FALSE)

    stk <<- key_storr(path)
    stv <<- value_storr(path)
  }

  create()

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

  st$reset <- function(){
    stk$destroy()
    stv$destroy()
    create()
  }

  st$hash_object <- stv$hash_object

  st$storr_details <- list(key_storr = stk, value_storr = stv)

  return(st)

}

# to use thor
# get_object_storr(
#   path,
#   key_storr = function(path){
#     thor::storr_thor(thor::mdb_env(file.path(path, "keys")))
#   },
#   value_storr = function(path){
#     thor::storr_thor(thor::mdb_env(file.path(path, "values")))
#   }
# )
#
