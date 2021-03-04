

# specific SF:singleton file based object cache

get_object_SF <- function(
  path = tempfile(pattern = "object_SF")
){

  sf <- list()

  kf <- NULL
  vf <- NULL


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

  create <- function(){
    if(!dir.exists(path)) {
      dir.create(path, showWarnings = FALSE, recursive = TRUE)
    }
    kf <<- file.path(path, "keys")
    vf <<- file.path(path, "values")
    if(!file.exists(kf)) save_rds(list(), kf)
    if(!file.exists(vf)) save_rds(list(), vf)
  }

  create()

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
  # special set case not using sequentialize
  sf$multi_set <- function(key, value){
    key_h <- unlist(lapply(key, hash_it))
    lk <- read_rds(kf)
    lv <- read_rds(vf)
    oldnm <- setdiff(names(lk), key_h)
    names(key) <- key_h
    names(value) <- key_h
    lkn <- c(lk[oldnm],key)
    lvn <- c(lv[oldnm],value)
    save_rds(lkn, kf)
    save_rds(lvn, vf)
  }

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
  sf$list_values <- function(){
    read_rds(vf)
  }
  sf$relocate <- function(move_to){
    dir.create(move_to, showWarnings = FALSE, recursive = TRUE)
    fls <- list.files(path,
                      all.files = TRUE)
    fls <- setdiff(fls, c(".",".."))
    ffrom <- file.path(path, fls)
    fto <- file.path(move_to, fls)

    file.rename(from = ffrom, to = fto)

    old_path <- path
    path <<- move_to
    create()
    unlink(old_path, recursive = TRUE, force = TRUE)
    invisible(0)
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

  sf$meta <- function(){
    list(type = "SF")
  }

  sf


}



object_cache_alt_SF <- function(path){
  get_object_SF(path)
}

object_cache_altMeta_SF <- function(){
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
