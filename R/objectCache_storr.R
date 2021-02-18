



# need to put sequentialize here
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
    dir.create(path, showWarnings = FALSE, recursive = TRUE)

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
    unlink(path, recursive = TRUE, force = TRUE)
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

adapter_object_cache_from_object_storr <- function(object_storr){

  oc <- object_cache_empty()

  oc$key_exists <- object_storr$exists
  oc$set <- object_storr$set
  oc$get <- object_storr$get
  oc$list_keys <- object_storr$list
  oc$remove <- object_storr$del
  oc$destroy <- object_storr$destroy
  oc$reset <- object_storr$reset

  oc
}



object_cache_alt_storr <- function(path){
  "storr"
}

object_cache_altMeta_storr <- function(){
  list(

    dep = list(
      packages = c("storr")
    ),

    desc = paste0(
      "Based on {storr} : 'Simple Key Value Stores'. ",
      "Functions like storr::storr_* can be used"
    )

  )
}



object_cache_alt_thor <- function(path){
"thor"
}

object_cache_altMeta_thor <- function(){
  list(

    dep = list(
      packages = c("thor")
    ),

    type = c("suggested","recommended"),

    desc = paste0(
      "Based on {thor} : 'Simple Key Value Stores'. ",
      "Functions like thor::storr_* can be used"
    )

  )
}


