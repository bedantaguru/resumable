



# need to put sequentialize here
get_object_storr <- function(
  path = tempfile(pattern = "object_storr"),
  key_storr = function(path){
    storr::storr_rds(file.path(path, "keys"))
  },
  value_storr =  function(path){
    storr::storr_rds(file.path(path, "values"))
  },
  sequentialize_set = FALSE)
{

  stk <- NULL
  stv <- NULL

  create <- function(){
    if(!is.null(path)){
      dir.create(path, showWarnings = FALSE, recursive = TRUE)
    }

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

  stk_set <- stk$set
  stv_set <- stv$set
  if(sequentialize_set & !is.null(path)){
    seq_path <- file.path(path, "seq")
    stk_set <- sequentialize(stk_set, seq_path)
    stv_set <- sequentialize(stv_set, seq_path)
  }

  st$set <- function(key, value, namespace) {
    if (missing(namespace))
      namespace <- stv$default_namespace
    key_h <- stv$hash_object(key)

    stk_set(key_h, key, namespace)
    stv_set(key_h, value, namespace)
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

  st$list_values <- function(namespace){
    if (missing(namespace))
      namespace <- stv$default_namespace
    stv$mget(stv$list(namespace), namespace)
  }

  st$relocate <- function(move_to){
    dir.create(move_to, showWarnings = FALSE, recursive = TRUE)
    fls <- list.files(path,
                      all.files = TRUE)
    fls <- setdiff(fls, c(".",".."))
    ffrom <- file.path(path, fls)
    fto <- file.path(move_to, fls)

    file.rename(from = ffrom, to = fto)

    old_path <- path
    stk$destroy()
    stv$destroy()
    path <<- move_to
    create()
    unlink(old_path, recursive = TRUE, force = TRUE)
    invisible(0)
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
    if(!is.null(path)){
      unlink(path, recursive = TRUE, force = TRUE)
    }
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

adapter_object_cache_from_object_storr <- function(
  object_storr, meta_type = "storr"){

  oc <- object_cache_empty()

  oc$key_exists <- object_storr$exists
  oc$set <- object_storr$set
  oc$get <- object_storr$get
  oc$list_keys <- object_storr$list
  oc$list_values <- object_storr$list_values
  oc$remove <- object_storr$del
  oc$destroy <- object_storr$destroy
  oc$reset <- object_storr$reset
  oc$relocate <- object_storr$relocate
  if(meta_type == "thor"){
    oc$meta = function(){
      list(type = "thor")
    }
  }else{
    oc$meta = function(){
      list(type = "storr")
    }
  }


  oc
}



object_cache_alt_storr <- function(path){

  trails_object_cache(
    path,
    ocf = function(path){
      adapter_object_cache_from_object_storr(
        get_object_storr(path)
      )
    } ,
    oc_type = "storr")

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

  trails_object_cache(
    path,
    ocf = function(path){
      adapter_object_cache_from_object_storr(
        get_object_storr(
          path,
          key_storr = function(path){
            thor::storr_thor(thor::mdb_env(file.path(path, "keys")))
          },
          value_storr = function(path){
            thor::storr_thor(thor::mdb_env(file.path(path, "values")))
          },
          sequentialize_set = TRUE
        ),
        meta_type = "thor"
      )
    } ,
    oc_type = "thor")

}

object_cache_altMeta_thor <- function(){
  list(

    dep = list(
      packages = c("thor")
    ),

    # Note: It is yet to discover if this is a correct approach to use {thor}
    # Now it does not work in parrallel sessions
    type = c("Sequential"),

    desc = paste0(
      "Based on {thor} : 'Simple Key Value Stores'. ",
      "Functions like thor::storr_* can be used"
    )

  )
}


