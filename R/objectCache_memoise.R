


get_object_memoise <- function(
  path = tempfile(pattern = "object_memoise"),
  key_memoise_cache = function(path){
    memoise::cache_filesystem(file.path(path, "keys"))
  },
  value_memoise_cache = function(path){
    memoise::cache_filesystem(file.path(path, "values"))
  }
){

  mem <- list()

  memk <- NULL
  memv <- NULL

  create <- function(){
    if(!is.null(path)){
      dir.create(path, showWarnings = FALSE, recursive = TRUE)
    }


    memk <<- key_memoise_cache(path)
    memv <<- value_memoise_cache(path)
  }

  create()

  mem$has_key <- function(key){
    key_h <- memk$digest(key)
    memk$has_key(key_h) &
      memv$has_key(key_h)
  }

  mem$set <- function(key, value){
    key_h <- memk$digest(key)
    memk$set(key_h, key)
    memv$set(key_h, value)
  }

  mem$get <- function(key){
    key_h <- memk$digest(key)
    memv$get(key_h)
  }

  mem$keys <- function(){
    lapply(memv$keys(), memk$get)
  }

  # mem$remove method not present

  mem$destroy <- function(){
    if(!is.null(path)){
      unlink(path, recursive = TRUE, force = TRUE)
    }
  }

  mem$reset <- function(){
    memk$reset()
    memv$reset()
  }

  mem


}


adapter_object_cache_from_object_memoise <- function(object_memoise){
  oc <- object_cache_empty()

  oc$key_exists <- object_memoise$has_key
  oc$set <- object_memoise$set
  oc$get <- object_memoise$get
  oc$list_keys <- object_memoise$keys
  # remove method not present : oc$remove <- object_memoise$remove
  oc$destroy <- object_memoise$destroy
  oc$reset <- object_memoise$reset

  oc
}



object_cache_alt_memoise <- function(path){
  adapter_object_cache_from_object_memoise(
    get_object_memoise(path)
  )
}

object_cache_altMeta_memoise <- function(){
  list(

    dep = list(
      packages = c("memoise")
    ),

    desc = paste0(
      "Based on {memoise} : 'Memoisation of Functions'. ",
      "Functions like memoise::cache_* can be used"
    )

  )
}
