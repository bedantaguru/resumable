




to_char <- function(x){
  UseMethod("to_char")
}

#' @export
to_char.default <- function(x){
  tryCatch(
    paste0(trimws(as.character(x)), collapse = ";"),
    error = function(e) "")
}

#' @export
to_char.list <- function(x){
  v <- lapply(x, to_char)
  paste0(names(x),"=",trimws(unlist(v)), collapse = ";")
}

#' @export
to_char.function <- function(x){
  # well it can be done using methods:::.BasicFunsList
  # but it is not worth it I think
  if(is.primitive(x)) warning("to_char on Primitive does not work yet")
  ar <- as.list(formals(x))
  ar <- ar[sort(names(ar))]
  ar <- to_char(ar)
  bd <- to_char(as.character(body(x)))
  ar <- paste0(trimws(ar), collapse = ";")
  bd <- paste0(trimws(bd), collapse = ";")
  paste0(ar,"~fbdy~", bd)
}


# really it's not a a hash
no_hash_actually <- function(x){
  ns <- as.integer(serialize(x , connection = NULL))
  ns[ns==0] <- 0.5
  paste0("dummy_sum_",round(sum(ns*seq_along(ns))+sum(ns*seq_along(ns)^2)))
}


hash_it <- function(x){
  if(is_available("digest")){
    digest::digest(x)
  }else{
    if(is_available("rlang")){
      rlang::hash(x)
    }else{
      warning(
        paste0("Either {digest} or {rlang} required for hashing!\n",
               "As none of them is available the hashing is not done correctly.\n",
               "Please install either of them to disable this warning."),
        call. = FALSE)
      no_hash_actually(x)
    }
  }
}

function_hash <- function(fun){
  hash_it(to_char(fun))
}
