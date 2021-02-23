

# specific singleton file based object cache

get_object_singleton_file_based <- function(
  path = tempfile(pattern = "object_singleton_file")
){

  sf <- list()

  kf <- file.path(path, "keys")
  vf <- file.path(path, "values")

  create <- function(){
    if(!dir.exists(path)) {
      dir.create(path, showWarnings = FALSE, recursive = TRUE)
    }
    if(!file.exists(kf)) save_rds(list(), kf)
    if(!file.exists(vf)) save_rds(list(), vf)
  }

  read_rds <- function(file){
    if(!file.exists(file)) return(list())
    readRDS(file)
  }


  save_rds <- function(val, file){
    if(!dir.exists(path)) {
      create()
    }
    saveRDS(val, file)
  }


  sf$key_exists <- function(key){
    key_h <- hash_it(key)
    lv <- read_rds(vf)
    !is.null(lv[[key_h]])
  }
  sf_set <- function(key, value){
    key_h <- hash_it(key)
    lk <- read_rds(kf)
    lv <- read_rds(vf)
    lk[[key_h]] <- key
    lv[[key_h]] <- value
    save_rds(lk, kf)
    save_rds(lv, vf)
  }
  sf$set <- sequentialize(sf_set, root_path = file.path(path, "seq"))
  sf$get <- function(key){
    key_h <- hash_it(key)
    lv <- read_rds(vf)
    lv[[key_h]]
  }
  sf$list_keys <- function(){
    lk <- read_rds(kf)
    lv <- read_rds(vf)
    lk[names(lv)]
  }
  sf_remove <- function(key){
    key_h <- hash_it(key)
    lk <- read_rds(kf)
    lv <- read_rds(vf)
    lk[[key_h]] <- NULL
    lv[[key_h]] <- NULL
    save_rds(lk, kf)
    save_rds(lv, vf)
  }
  sf$remove <- sequentialize(sf_remove, root_path = file.path(path, "seq"))
  sf$destroy <- function(){
    unlink(path, recursive = TRUE, force = TRUE)
  }
  sf_reset <- function(){
    save_rds(list(), kf)
    save_rds(list(), vf)
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
      packages = c("filelock"),
      suggests = c("digest"),
      sys = NULL,
      other = NULL),

    desc = paste0(
      "Single file based"
    )

  )
}
