

# candidates
# {storr}, {cachem}, {memoise}, {thor}

object_cache_empty <- function(){
  list(
    key_exists = function(key){
      invisible(NULL)
    },
    set = function(key, value){
      invisible(NULL)
    },
    get = function(key){
      invisible(NULL)
    },
    list_keys = function(){
      invisible(NULL)
    },
    list_values = function(){
      invisible(NULL)
    },
    remove = function(key){
      invisible(NULL)
    },
    destroy = function(){
      invisible(NULL)
    },
    reset = function(){
      invisible(NULL)
    },
    relocate = function(move_to){
      invisible(NULL)
    },
    meta = function(){
      list(type = "empty")
    }
  )
}



#' @export
object_cache <- function(path = tempfile("object_cache"), use, ...){
  use_alternatives("object_cache")
}


object_cache_copy <- function(oc_src, oc_dest, iterator = lapply){
  ks <- oc_src$list_keys()
  if(isTRUE(oc_dest$meta()$type=="SF")){
    # special case of SF
    oc_dest$multi_set(ks, oc_src$list_values())
  }else{
    iterator(ks, function(x){
      oc_dest$set(x, oc_src$get(x))
    })
  }
  invisible(0)
}

is_object_cache <- function(x){
  oce <- object_cache_empty()
  nt <- names(oce)
  res <- tryCatch(
    {
      nx <- names(x)
      chk <- length(setdiff(nt, nx))==0
      if(chk){
        cmn <- intersect(nx, nt)
        for(nn in cmn){
          if(is.function(x[[nn]])){
            if(length(setdiff(
              names(formals(oce[[nn]])),
              names(formals(x[[nn]]))
            ))>0){
              chk <- FALSE
              break()
            }
          }else{
            chk <- FALSE
            break()
          }
        }

      }
      chk
    },
    error = function(e) FALSE
  )
  res
}


trails_getset_object_cache <- function(path, oc){
  ocn <- file.path(path, "oc")
  if(file.exists(ocn)){

    ot <- tryCatch(
      readRDS(ocn),
      error = function(e) NULL
    )

    if(is_object_cache(ot)){
      return(ot)
    }else{
      stop(paste0(
        "Old OC file may be corrupted/wrongly configured.",
        " Clear the path:",path
      ), call. = FALSE)
    }

  }else{
    if(!missing(oc)){
      saveRDS(oc, ocn)
    }
  }
  invisible("no_trail")
}


trails_object_cache <- function(path, ocf, oc_type){
  toc <- trails_getset_object_cache(path)
  if(identical(toc,"no_trail")){
    oc <- ocf(path)
    trails_getset_object_cache(path, oc)
  }else{

    if(isTRUE(getOption("resumable_log_level")=="info")){
      cat(paste0(
        "Old object_cache found in the path: ", path,
        " - using the same.\n"
      ))
    }


    if(toc$meta()$type!=oc_type){
      warning(
        paste0("Desired object_cache in path:",
               path, " is ", oc_type,
               "\nWhile the old object_cache in the same path is:",
               toc$meta()$type,
               "\nMake sure that this is what you expected.\n",
               "You may clean old object_cache by destroying it.\n"),
        call. = FALSE
      )
    }

    oc <- toc
  }

  oc
}
