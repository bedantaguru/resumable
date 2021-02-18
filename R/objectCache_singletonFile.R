

# specific singleton file based object cache

get_object_singleton_file_based <- function(
  path = tempfile(pattern = "object_singleton_file")
){

  sf <- list()

  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  kf <- file.path(path, "keys")
  vf <- file.path(path, "values")

  saveRDS(list(), kf)
  saveRDS(list(), vf)

  sf$key_exists <- function(key){
    key_h <- hash_it(key)
    lv <- readRDS(vf)
    !is.null(lv[[key_h]])
  }
  sf_set <- function(key, value){
    key_h <- hash_it(key)
    lk <- readRDS(kf)
    lv <- readRDS(vf)
    lk[[key_h]] <- key
    lv[[key_h]] <- value
    saveRDS(lk, kf)
    saveRDS(lv, vf)
  }
  sf$set <- sequentialize(sf_set, root_path = file.path(path, "seq"))
  sf$get <- function(key){
    key_h <- hash_it(key)
    lv <- readRDS(vf)
    lv[[key_h]]
  }
  sf$list_keys <- function(){
    lk <- readRDS(kf)
    lv <- readRDS(vf)
    lk[names(lv)]
  }
  sf_remove <- function(key){
    key_h <- hash_it(key)
    lk <- readRDS(kf)
    lv <- readRDS(vf)
    lk[[key_h]] <- NULL
    lv[[key_h]] <- NULL
    saveRDS(lk, kf)
    saveRDS(lv, vf)
  }
  sf$remove <- sequentialize(sf_remove, root_path = file.path(path, "seq"))
  sf$destroy <- function(){
    unlink(path, recursive = TRUE, force = TRUE)
  }
  sf_reset <- function(){
    saveRDS(list(), kf)
    saveRDS(list(), vf)
  }
  sf$reset <- sequentialize(sf_reset, root_path = file.path(path, "seq"))

  sf


}



object_cache_alt_singletonFile <- function(path){
  get_object_singleton_file_based(path)
}

object_cache_altMeta_singletonFile <- function(){
  list(

    dep = list(
      alt = NULL,
      packages = NULL,
      suggests = c("digest"),
      sys = NULL,
      other = NULL),

    desc = paste0(
      "Single file based"
    )

  )
}
