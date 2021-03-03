
# POS : PersistentObjectStore
# dep: "R/module_PersistentObjectStore.R"

# modified POS for
#
# 1) Object keys (keys can be any R object)
#
# 2) use any R object as value (R_object = TRUE)
get_object_POS <- function(
  path = tempfile(pattern = "object_POS")
){

  pos <- list()

  posk <- NULL
  posv <- NULL

  create <- function(){
    dir.create(path, showWarnings = FALSE, recursive = TRUE)

    posk <<- persistent_object_store(store_path = file.path(path, "keys"))
    posv <<- persistent_object_store(store_path = file.path(path, "values"))
  }

  create()

  pos$key_exists <- function(key){
    key_h <- hash_it(key)
    posk$key_exists(key_h, R_object = TRUE) &
      posv$key_exists(key_h, R_object = TRUE)
  }

  pos$write <- function(key, value){
    key_h <- hash_it(key)
    posk$write(key_h, key, R_object = TRUE)
    posv$write(key_h, value, R_object = TRUE)
  }

  pos$read <- function(key){
    key_h <- hash_it(key)
    posv$read(key_h, R_object = TRUE, uniform_output = TRUE)[[1]]
  }

  pos$list_keys <- function(){
    posk$read(
      posv$list_keys(R_object = TRUE),
      R_object = TRUE, uniform_output = TRUE)
  }

  pos$list_values <- function(){
    posv$read(
      posv$list_keys(R_object = TRUE),
      R_object = TRUE, uniform_output = TRUE)
  }

  pos$relocate <- function(move_to){
    dir.create(move_to, showWarnings = FALSE, recursive = TRUE)
    fls <- list.files(path,
                      all.files = TRUE)
    fls <- setdiff(fls, c(".",".."))
    ffrom <- file.path(path, fls)
    fto <- file.path(move_to, fls)

    file.rename(from = ffrom, to = fto)

    old_path <- path
    posk$destroy()
    posv$destroy()
    path <<- move_to
    create()
    unlink(old_path, recursive = TRUE, force = TRUE)
    invisible(0)
  }

  pos$remove <- function(key){
    key_h <- hash_it(key)
    posk$remove(key_h, R_object = TRUE)
    posv$remove(key_h, R_object = TRUE)
  }

  pos$destroy <- function(){
    posk$destroy()
    posv$destroy()
    unlink(path, recursive = TRUE, force = TRUE)
  }

  pos$reset <- function(){
    posk$reset()
    posv$reset()
  }

  pos


}



adapter_object_cache_from_object_POS <- function(object_POS){

  oc <- object_cache_empty()

  oc$key_exists <- object_POS$key_exists
  oc$set <- object_POS$write
  oc$get <- object_POS$read
  oc$list_keys <- object_POS$list_keys
  oc$list_values <- object_POS$list_values
  oc$remove <- object_POS$remove
  oc$destroy <- object_POS$destroy
  oc$reset <- object_POS$reset
  oc$relocate <- object_POS$relocate

  oc
}


object_cache_alt_POS <- function(path){
  adapter_object_cache_from_object_POS(get_object_POS(path))
}

object_cache_altMeta_POS <- function(){
  list(

    dep = list(
      alt = NULL,
      packages = NULL,
      suggests = c("digest"),
      sys = NULL,
      other = NULL),

    type = c("default","base","minimal"),

    desc = paste0(
      "Based on inbuilt module : Persistent Object Store."
    )

  )
}
