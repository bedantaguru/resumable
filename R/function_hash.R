




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

function_hash <- function(fun){
  # alternative rlang::hash
  digest::digest(to_char(fun))
}
