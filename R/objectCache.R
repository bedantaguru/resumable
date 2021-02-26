

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
    remove = function(key){
      invisible(NULL)
    },
    destroy = function(){
      invisible(NULL)
    },
    reset = function(){
      invisible(NULL)
    }
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
