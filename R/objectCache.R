

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
    }
  )
}



#' @export
object_cache <- function(path = tempfile("object_cache"), use, ...){
  use_alternatives("object_cache")
}



object_cache_copy <- function(oc_src, oc_dest, iterator = lapply){
  ks <- oc_src$list_keys()
  iterator(ks, function(x){
    oc_dest$set(x, oc_src$get(x))
  })
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
