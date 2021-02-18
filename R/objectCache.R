

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
  )
}



#' @export
object_cache <- function(path = tempfile("object_cache"), ...){
  use_alternatives("object_cache")
}
