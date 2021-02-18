

get_object_cachem <- function(
  path = tempfile(pattern = "object_cachem"),
  key_cachem_cache = function(path){
    cachem::cache_disk(file.path(path, "keys"))
  },
  value_cachem_cache = function(path){
    cachem::cache_disk(file.path(path, "values"))
  }
){

  cam <- list()

  camk <- NULL
  camv <- NULL

  create <- function(){
    dir.create(path, showWarnings = FALSE, recursive = TRUE)

    camk <<- key_cachem_cache(path)
    camv <<- value_cachem_cache(path)
  }

  create()

  cam$exists <- function(key){
    key_h <- hash_it(key)
    camk$exists(key_h) &
      camv$exists(key_h)
  }

  cam$set <- function(key, value){
    key_h <- hash_it(key)
    camk$set(key_h, key)
    camv$set(key_h, value)
  }

  cam$get <- function(key){
    key_h <- hash_it(key)
    camv$get(key_h)
  }

  cam$keys <- function(){
    lapply(camv$keys(), camk$get)
  }

  cam$remove <- function(key){
    key_h <- hash_it(key)
    camk$remove(key_h)
    camv$remove(key_h)
  }

  cam$destroy <- function(){
    camk$destroy()
    camv$destroy()
    unlink(path, recursive = TRUE, force = TRUE)
  }

  cam$reset <- function(){
    camk$reset()
    camv$reset()
  }

  cam


}


adapter_object_cache_from_object_cachem <- function(object_cachem){

  oc <- object_cache_empty()

  oc$key_exists <- object_cachem$exists
  oc$set <- object_cachem$set
  oc$get <- object_cachem$get
  oc$list_keys <- object_cachem$keys
  oc$remove <- object_cachem$remove
  oc$destroy <- object_cachem$destroy
  oc$reset <- object_cachem$reset

  oc

}



object_cache_alt_cachem <- function(path){
  adapter_object_cache_from_object_cachem(
    get_object_cachem(path)
  )
}

object_cache_altMeta_cachem <- function(){
  list(

    dep = list(
      packages = c("cachem")
    ),

    desc = paste0(
      "Based on {cachem} : 'Cache R Objects with Automatic Pruning'"
    )

  )
}
