


#>> Embedded Module: {alternatives}
#>> Depends on (shallow): {PersistentObjectStore}
#>> Note: Prototype
#>> Author: Indranil Gayen
#>> Version: 0.0.2
#>> Files: c("R/module_alternatives.R")


# environment kept for storing "alternatives" for methods
alternatives_env <- new.env()

# end user function
alternatives <- function(method_name,
                         implement,
                         use,
                         install,
                         register,
                         # scope will define the location of saving
                         scope) {
  if(!is.character(method_name)){
    method_name <- as.character(substitute(method_name))
  }

  if(!missing(implement)){
    alternatives_implement(method_name, implement)
    return(invisible(0))
  }
  if(!missing(use)){
    alternatives_use(method_name, use)
    return(invisible(0))
  }
  if(!missing(install)){
    alternatives_install(method_name, install)
    return(invisible(0))
  }

  if(!missing(register)){
    alternatives_register(method_name, register)
    return(invisible(0))
  }
  alts <- get_alternatives(method_name)

  alts[c("name","desc")]
}

alternatives_install <- function(method_name, alt_name) {
    alts <- get_alternatives(method_name)
    alts_target <- alts[alts$name == alt_name,]
    to_inst <- c(
      unlist(alts_target$meta.dep.packages),
      unlist(alts_target$meta.dep.suggests)
    )
    #TODO
  }

alternatives_use <- function(method_name, alt_name) {

  alts_valid <- get_alternatives(method_name, only_uable = TRUE)

  if(!(alt_name %in% alts_valid$name)){
    stop(paste0("There is no alternatives for method ",method_name,
                " with the name ", alt_name,
                " (which is either installed/configured or registered)."),
         call. = FALSE)
  }

  if (exists("persistent_object_store",
             envir = parent.env(environment()))) {
    # in this case custom method will be implemented in all subsequent sessions
    # unless changed again

    pos_alt <- persistent_object_store(appname = "alternatives")
    pos_alt$write(method_name, alt_name)

  }

  # by default store in session environment only
  assign(paste0(method_name,"_alternatives_in_use"), alt_name,
         envir = alternatives_env)

}

alternatives_implement <- function(method_name, alt_name) {
  alternatives_install(method_name, alt_name)
  alternatives_use(method_name, alt_name)
}

alternatives_register <- function(method_name, alternative_details){
  #TODO for 3rd party integration
}

use_alternatives <- function(method_name) {
  calt <- get_configured_alternatives(method_name)
  do.call(calt, args = as.list(sys.frame(sys.parent())))
}

alternatives_dispatch_style_naming <- function(method_name) {
  list(
    alt_name = paste0("^", method_name, "_alt_"),
    alt_meta_name = paste0("^", method_name, "_altMeta_")
  )
}

search_alternatives <- function(method_name, env) {
  if (missing(env)) {
    env <- parent.env(environment())
  }

  alt_name_style <- alternatives_dispatch_style_naming(method_name)

  alts <- lapply(alt_name_style,
                 function(an) {
                   ls(pattern = an, envir = env)
                 })

  alts2 <- lapply(names(alts),
                  function(ann) {
                    d <- data.frame(temp = alts[[ann]])
                    colnames(d) <- ann
                    d$name <- gsub(alt_name_style[[ann]], "", alts[[ann]])
                    d
                  })

  altd <-
    Reduce(function(x, y)
      merge(x, y, by = "name", all = TRUE), alts2)

  for (ann in names(alt_name_style)) {
    altd[[paste0(ann, "_fn")]] <- lapply(altd[[ann]],
                                         function(annn) {
                                           if (exists(annn, envir = env)) {
                                             get(annn, envir = env)
                                           } else{
                                             # blank function
                                             function(...) {

                                             }
                                           }
                                         })
  }

  altd

}

# this is the guy who parse alt data
elaborate_alternatives_data <- function(alts) {
  alts$alt_meta_lst <-
    lapply(alts$alt_meta_name_fn, function(fn)
      fn())
  alts$meta.dep <- lapply(alts$alt_meta_lst, `[[`, "dep")
  alts$meta.dep.alt <- lapply(alts$meta.dep, `[[`, "alt")
  alts$meta.dep.packages <- lapply(alts$meta.dep, `[[`, "packages")
  alts$meta.dep.suggests <- lapply(alts$meta.dep, `[[`, "suggests")
  alts$meta.dep.sys <- lapply(alts$meta.dep, `[[`, "sys")
  alts$meta.dep.other <- lapply(alts$meta.dep, `[[`, "other")
  alts$meta.type <- lapply(alts$alt_meta_lst, `[[`, "type")
  alts$meta.desc <- lapply(alts$alt_meta_lst, `[[`, "desc")
  alts$desc <-
    unlist(lapply(alts$meta.desc, paste0, collapse = ", "))

  alts$is_base <- unlist(lapply(alts$meta.type,
                                function(x) "base" %in% x))
  if(!any(alts$is_base)){
    # no base found. so pick the one with minimum dependency
    # TODO recursive dependency can be checked here
    pkdep <- unlist(lapply(alts$meta.dep.packages, length))
    pksugdep <- unlist(lapply(alts$meta.dep.suggests, length))
    pkfinaldep <- pkdep+0.5*pksugdep
    alts$is_base[pkfinaldep==min(pkfinaldep)] <- TRUE
  }

  alts$is_recommended <- unlist(lapply(alts$meta.type,
                                       function(x) "recommended" %in% x))
  if(!any(alts$is_recommended)){
    alts$is_recommended <- alts$is_base
  }

  alts
}


register_alternatives_self <- function(method_name) {
  if (!isTRUE(alternatives_env[[paste0(method_name, "_self_searched")]])) {
    alts <- search_alternatives(method_name)

    alts <- elaborate_alternatives_data(alts)

    alts$provider <- "self"

    assign(paste0(method_name, "_self_searched"),
           TRUE,
           envir = alternatives_env)

    assign(paste0(method_name, "_alts"),
           alts,
           envir = alternatives_env)

  } else{
    alts <- get(paste0(method_name, "_alts"), envir = alternatives_env)

  }

  alts

}

# for registration on the fly by third party
# TODO register_alternatives_3rd_party

register_alternatives <- function(method_name) {
  register_alternatives_self(method_name)
  # TODO register_alternatives_3rd_party
}

check_alternatives_data <- function(alts){

  alts$meta.dep.packages_is_installed <-
    unlist(lapply(alts$meta.dep.packages, is_available))
  # TODO many tuning can be done here
  alts$is_usable <- alts$meta.dep.packages_is_installed

  alts$select_score <- ifelse(alts$is_usable, 1, 0)*(
    alts$is_recommended*5+alts$is_base*2
  )
  alts
}

auto_select_alternatives <- function(method_name){
  alts_valid <- get_alternatives(method_name, only_uable = TRUE)

  alt_name_by_score <-
    alts_valid$name[order(alts_valid$select_score, decreasing = TRUE)][1]
  # store it for faster access
  assign(paste0(method_name,"_alternatives_in_use"), alt_name_by_score,
         envir = alternatives_env)
  alt_name_by_score
}

get_alternatives <- function(method_name, only_uable = FALSE) {
  alts <- register_alternatives(method_name)
  alts <- check_alternatives_data(alts)

  if(only_uable){
    alts <- alts[alts$select_score>0,]
    if(nrow(alts)==0){
      stop(paste0("No alternative backend can be used for ",method_name,
                  ". Please install dependency for at least one alternatives."),
           call. = FALSE)
    }
  }

  alts
}

get_configured_alternatives <- function(method_name) {

  alts_valid <- get_alternatives(method_name, only_uable = TRUE)

  found_alt <- FALSE
  alt_name <- ""


  if(!found_alt){
    if(exists(
      paste0(method_name,"_alternatives_in_use"),
      envir = alternatives_env
    )){

      alt_name_try <- get(paste0(method_name,"_alternatives_in_use"),
                          envir = alternatives_env)

      if(alt_name_try %in% alts_valid$name){
        found_alt <- TRUE
        alt_name <- alt_name_try
      }

    }
  }

  if(!found_alt){
    if (exists("persistent_object_store",
               envir = parent.env(environment()))) {
      # in this case custom method will be implemented in all subsequent sessions
      # unless changed again

      pos_alt <- persistent_object_store(appname = "alternatives")
      alt_name_try <- pos_alt$read(method_name)

      if(is.null(alt_name_try)) alt_name_try <- ""

      if(alt_name_try %in% alts_valid$name){
        found_alt <- TRUE
        alt_name <- alt_name_try

        # store it for faster access
        assign(paste0(method_name,"_alternatives_in_use"), alt_name,
               envir = alternatives_env)
      }

    }
  }

  if(!found_alt){
    alt_name_try <- auto_select_alternatives(method_name)
    if(alt_name_try %in% alts_valid$name){
      found_alt <- TRUE
      alt_name <- alt_name_try
    }
  }

  if(found_alt){
    alts_valid[alts_valid$name==alt_name,]$alt_name_fn[[1]]
  }else{
    stop("No alternatives found.", call. = FALSE)
  }

}
