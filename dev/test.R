
unlink("test", recursive = T, force = T)
unlink("test2", recursive = T, force = T)

f <- resumable(function(x){
  print("calc")
  x
},"test")

f(1)
f(2)

g <- ro_convert(f, new_oc = "SF")

g <- ro_transfer(f, new_root = "test2")

# dest_oc and new root both is required
# new_root has to be given always (otherwise meaningless)
g <- ro_transfer(f, dest_oc = object_cache_alt_cachem("test2"))

g <- ro_transfer(f, dest_oc = object_cache_alt_cachem("test2/rf"), new_root = "test2")


ft <- function(x){
  environment(x)$`_fun_etc` < "hi"
  invisible(0)
}


f0 <- function(x=4,
               y=4){
  if(T){
    T
  }else F
}

ft <- function(x){
  # mc <- match.call()
  # length(mc[[2]])
  y <- deparse(substitute(x))
  print(y)
  print(match.call())
  browser()
  grepl("\\(",y)
}


rb <- callr::r_session$new()

cl <- parallel::makeCluster(2)


f0 <- function(x) {print("calc");x^2}

f1 <- resumable(f0, obsolete_if = ~.y>5)

f2 <- memoise::memoise(f0, cache = memoise::cache_filesystem(tempfile()))


parallel::clusterApply(cl, 1:2, f1)

# user  system elapsed
# 0.02    0.00    0.09

parallel::clusterApply(cl, 1:2, f2)

# user  system elapsed
# 0.00    0.00    0.03

parallel::clusterApply(cl, 1:2, function(x) sessionInfo())

parallel::stopCluster(cl)


f0 <- function() {print("calc");100}

f1 <- resumable(f0, root_path = "test")

rb$run(sessionInfo)


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

long_function_n <- function(n){
  flong <- eval(
    parse(text =
            paste0("function(x){\n",
                   paste0("x", 1:n, " <- ", 1:n, collapse = "\n"),"\nx\n}")))
  flong
}


fr <- resumable(long_function_n(100))


cl <- parallel::makeCluster(parallel::detectCores())

parallel::clusterApply(cl, 1:10, function(x) Sys.getpid())

#require(resumable)

resumable:::alternatives(resumable::object_cache, use = "cachem")
resumable:::alternatives(resumable::object_cache, use = "memoise")
resumable:::alternatives(resumable::object_cache, use = "POS")
resumable:::alternatives(resumable::object_cache, use = "storr")
resumable:::alternatives(resumable::object_cache, use = "thor")
#resumable:::alternatives(resumable::object_cache, use = "singletonFile")
# singletonFile renamed to SF
resumable:::alternatives(resumable::object_cache, use = "SF")


parallel::clusterApply(cl, 1:3, function(x){

  resumable:::alternatives(resumable::object_cache)
})


unlink(".test", recursive = T, force = T)
parallel::clusterApply(cl, 1:10, function(x){
  oc <- resumable::object_cache(".test")
  oc$set(x, x)
})

oc <- resumable::object_cache(".test")
identical(sort(unlist(oc$list_keys())), 1:10)

parallel::clusterApply(cl, 1:100, function(x){
  oc <- resumable::object_cache(".test")
  oc$set(x, x)
})

oc <- resumable::object_cache(".test")
identical(sort(unlist(oc$list_keys())), 1:100)

setdiff(1:100, sort(unlist(oc$list_keys())))



f0 <- function(x){
  x
}

f1 <- resumable(f0, root_path = ".test")

parallel::clusterApply(cl, 1:100, f1)
parallel::clusterApply(cl, 1:100, function(x, f){try(f(x))}, f=f1)

parallel::clusterApply(cl, 1:100, function(x){

  f0 <- function(x){
    x
  }



  tryCatch({
    f1 <- resumable::resumable(f0, root_path = ".test")
    f1(x)
  }, error =function(e) e)
})


parallel::clusterApply(cl, 1:100, function(x, ff){
  tryCatch({
    # f1 <- resumable::resumable(f0, root_path = ".test")
    # f1(x)
    ff(x)
  }, error =function(e) e)
}, ff = f1)



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


#
# == Testing test-resumable_parallel.R ===========================================
#   [ FAIL 0 | WARN 0 | SKIP 0 | PASS 1 ][1] "POS"
# elapsed
# 0.92
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 5 ][1] "cachem"
# elapsed
# 1.18
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 9 ][1] "memoise"
# elapsed
# 0.55
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 13 ][1] "singletonFile"
# elapsed
# 1.06
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 17 ][1] "storr"
# elapsed
# 2.08
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 21 ] Done!
#
