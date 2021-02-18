



resumable::get_object_storr()


thor::storr_thor()


# to_char <- function(x){
#   UseMethod("to_char")
# }
#
# to_char.default <- function(x){
#   tryCatch(
#     paste0(trimws(as.character(x)), collapse = ";"),
#     error = function(e) "")
# }
#
# to_char.list <- function(x){
#   v <- lapply(x, to_char)
#   paste0(names(x),"=",trimws(unlist(v)), collapse = ";")
# }
#
# to_char.function <- function(x){
#   # well it can be done using methods:::.BasicFunsList
#   # but it is not worth it I think
#   if(is.primitive(x)) warning("to_char on Primitive does not work yet")
#   ar <- as.list(formals(x))
#   ar <- ar[sort(names(ar))]
#   ar <- to_char(ar)
#   bd <- to_char(as.character(body(x)))
#   ar <- paste0(trimws(ar), collapse = ";")
#   bd <- paste0(trimws(bd), collapse = ";")
#   paste0(ar,"~fbdy~", bd)
# }
#
# function_hash <- function(fun){
#   # alternative rlang::hash
#   digest::digest(to_char(fun))
# }
#
#
#
# sequentialize <- function(fun, root_path, env = environment(fun)){
#
#   fun <- rlang::as_function(fun)
#
#
#   fun_formals <- formals(fun)
#
#   fmod <- function(...){
#     actualcall <- match.call()
#     encl_env <- parent.env(environment())
#
#     fl <- filelock::lock(encl_env$`_fun_lockfile`)
#
#     on.exit(filelock::unlock(fl))
#
#     actualcall[[1L]] <- encl_env$`_fun`
#     out <- withVisible(eval(actualcall, parent.frame()))
#
#
#     if (out$visible) {
#       out$value
#     } else {
#       invisible(out$value)
#     }
#   }
#
#   formals(fmod) <- fun_formals
#   attr(fmod, "sequentialized") <- TRUE
#
#   # creation of lock file
#
#   ff <- function_hash(fun)
#
#   if(missing(root_path)){
#     root_path <- tempfile(pattern = "sequentialize_")
#   }
#
#   dir.create(root_path, showWarnings = FALSE, recursive = TRUE)
#
#   fflock <- file.path(root_path, ff)
#   if(!file.exists(fflock)){
#     file.create(fflock)
#   }
#
#   seq_fun_env <- new.env(parent = env)
#   seq_fun_env$`_fun_lockfile` <- fflock
#   seq_fun_env$`_fun` <- fun
#
#
#
#   environment(fmod) <- seq_fun_env
#   class(fmod) <- c("sequentialized","function")
#
#   fmod
#
# }
#
#
# print.sequentialized <- function(x, ...) {
#   cat("Sequentialized Function:\n")
#   tryCatch(
#     {
#       print(environment(x)$`_fun`)
#       fl <- filelock::lock(environment(x)$`_fun_lockfile`, timeout = 0)
#       if(is.null(fl)){
#         cat("\nPossibly in use.\n")
#       }else{
#         cat("\nPossibly Not in use.\n")
#       }
#       on.exit(filelock::unlock(fl))
#     },
#     error = function(e) stop("No function found!", call. = FALSE))
# }
#
#

cl <- parallel::makeCluster(parallel::detectCores())

parallel::clusterApply(cl, 1:10, function(x) Sys.getpid())


f0 <- function(x){
  write(as.character(x), file = "test", append = TRUE)
}



write("0", file = "test")

parallel::clusterApply(cl, 1:10, f0)

readLines("test")

f1 <- sequentialize(f0)

f1 <- sequentialize(~write(as.character(.x), file = "test", append = TRUE))


write("0", file = "test")

parallel::clusterApply(cl, 1:10, f1)

readLines("test")


