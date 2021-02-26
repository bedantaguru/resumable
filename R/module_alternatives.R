


#>> Embedded Module: {alternatives}
#>> Depends on (shallow): {PersistentObjectStore}
#>> Note: Prototype
#>> Author: Indranil Gayen
#>> Version: 0.0.2
#>> Files: c("R/module_alternatives.R")


# environment kept for storing "alternatives" for methods
alternatives_env <- new.env()

### end user function ###

#' @title Alternatives framework
#'
#' @description For some functions (or functionalities) there can be multiple
#'   number of R packages or even within a package multiple ways to do same or
#'   similar things. Alternatives framework tries to unify the vareity
#'   introduced by different approaches (may be referred as _alternatives_ for
#'   that function) adopted by several authors / packages. It is like
#'   `update-alternatives` command in linux (maybe you have used it to switch
#'   between Java versions). But here it is more focused towards similar work
#'   being done by various packages / functions. There are few related concepts
#'   to it. Like adapters and converters. However, these all are now in
#'   conceptual stage.
#'
#' @param method_name The function (either the direct function or name of the
#'   function) which has multiple back-ends or implementation (which you want to
#'   alter or install)
#' @param implement This is the combination of `install` and `use`.
#' @param use Use one of the _alternatives_ which is already installed (name of
#'   the intended _alternative_)
#' @param install Install dependency to an _alternative_ (mainly R-packages, but
#'   potentially we can expand the API for other type of installation too). But
#'   it will not use it.
#' @param register Mainly kept for registration of new _alternative_ to the same
#'   function.
#' @param scope Scope where the changes of _alternatives_ will take place.
#'
#' @return  If *no argument expect* `method_name` is supplied then details about
#'   _alternatives_ for the function will be returned. Otherwise it does not
#'   return anything useful.
#' @export
alternatives <- function(method_name,
                         implement,
                         use,
                         install,
                         register,
                         # scope will define the location of saving
                         scope) {
  if(!is.character(method_name)){
    method_name <- as.character(substitute(method_name))
    if(method_name[1]=="::"){
      # many things need to be done here
      # but leaving with simple things here
      method_name <- method_name[3]
    }
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

  alts <- alts[c("name","desc","is_usable")]
  aln <- get_configured_alternative(method_name, details = TRUE)
  alts$in_use <- alts$name == aln$name
  alts
}

alternatives_install <- function(method_name, alt_name, silent = FALSE) {
  alts <- get_alternatives(method_name)
  # special alt_names:
  # "all" "best"
  if("all" %in% tolower(alt_name)) alt_name <- alts$name

  if("best" %in% alt_name){
    alt_name <- alts$name[alts$is_recommended]
  }

  in_type_chk <- unlist(
    lapply(alts$meta.type, function(x) length(intersect(x, alt_name))>0)
  )

  alts_target <- alts[(alts$name %in% alt_name)|in_type_chk,]
  if(nrow(alts_target)==0){
    stop("Probably invalid alternative name specification!", call. = FALSE)
  }
  to_inst <- c(
    unlist(alts_target$meta.dep.packages),
    unlist(alts_target$meta.dep.suggests)
  )
  is_inst_chk <- unlist(lapply(to_inst, is_available))
  if(is.null(is_inst_chk)) is_inst_chk <- logical(0)
  if(any(!is_inst_chk)){
    to_inst <- to_inst[!is_inst_chk]
    cm <- getOption("repos")
    if(is.null(cm)){
      cm <- "https://cloud.r-project.org/"
    }
    utils::install.packages(
      pkgs = to_inst,
      repos = cm)
  }else{
    if(!silent){
      cat("\nAll dependency already installed\n")
    }
  }
}

alternatives_use <- function(method_name, alt_name) {

  if(length(alt_name)!=1){
    stop("Only one alternative can be used", call. = FALSE)
  }

  alts_valid <- get_alternatives(method_name, only_uable = TRUE)
  # specific case
  if(alt_name == "best"){
    alt_name <- alts_valid$name[alts_valid$is_recommended]
  }

  if(!(alt_name %in% alts_valid$name)){
    # search in type
    in_type_chk <- unlist(
      lapply(alts_valid$meta.type, function(x) length(intersect(x, alt_name))>0)
    )

    if(any(in_type_chk)){
      alt_name <- alts_valid$name[in_type_chk]
      alt_name <- alt_name[1]
    }

  }


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
  alternatives_install(method_name, alt_name, silent = TRUE)
  alternatives_use(method_name, alt_name)
}

alternatives_register <- function(method_name, alternative_details){
  #TODO for 3rd party integration
  stop("Not implemented  yet", call. = FALSE)
}

use_alternatives <- function(method_name, use_alt_arg_name = "use") {
  c_args <- as.list(sys.frame(sys.parent()))

  uc <- get_alternative_through_direct_call(
    method_name,
    c_args,
    use_alt_arg_name)

  c_args[[use_alt_arg_name]] <- NULL

  if(uc$use_configured){
    calt <- get_configured_alternative(method_name)
  }else{
    calt <- uc$choosen_alt
  }

  do.call(calt, args = c_args)
}

get_alternative_through_direct_call <- function(
  method_name,
  called_arg,
  use_alt_arg_name){
  use_configured <- TRUE

  # use alt on the fly
  if(use_alt_arg_name %in% names(called_arg)){

    on_the_fly_failed <- FALSE
    calt <- NULL


    # on the fly choose mode
    use_on_the_fly <- called_arg[[use_alt_arg_name]]

    if(length(use_on_the_fly)==1 & is.character(use_on_the_fly)){

      alts <- get_alternatives(method_name, only_uable = TRUE)

      sel1 <- alts$name==use_on_the_fly
      if(!any(sel1)){
        in_type_chk <- unlist(
          lapply(alts$meta.type,
                 function(x) length(intersect(x, use_on_the_fly))>0)
        )
        sel1 <- in_type_chk
      }

      if(any(sel1)){
        alts_sel <- alts[sel1,]
        calt <- alts_sel$alt_name_fn[[1]]
        use_configured <- FALSE
      }else{
        on_the_fly_failed <- TRUE
      }

    }else{
      on_the_fly_failed <- TRUE
    }

    if(on_the_fly_failed){
      warning(paste0("Unable to use: ",
                     use_on_the_fly,
                     "\nFalling back to configured alternative."),
              call. = FALSE)
    }

  }

  list(choosen_alt = calt,
       use_configured = use_configured)
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
    alts$is_recommended*5+alts$is_base*2+1
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

get_configured_alternative <- function(method_name, details = FALSE) {

  alts_valid <- get_alternatives(method_name, only_uable = TRUE)

  found_alt <- FALSE
  alt_name <- ""

  priority <- c("file_system", "session")
  # priority can be altered easily by priority <- c("session", "file_system")
  # TODO maybe that can be kept as a configuration to the alternatives itself.


  while(length(priority)>0){

    if(!found_alt & priority[1] == "session"){
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

    if(!found_alt & priority[1] == "file_system"){
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

    priority <- priority[-1]

  }



  if(!found_alt){
    alt_name_try <- auto_select_alternatives(method_name)
    if(alt_name_try %in% alts_valid$name){
      found_alt <- TRUE
      alt_name <- alt_name_try
    }
  }

  if(found_alt){
    if(details){
      alts_valid[alts_valid$name==alt_name,]
    }else{
      alts_valid[alts_valid$name==alt_name,]$alt_name_fn[[1]]
    }

  }else{
    stop("No alternatives found.", call. = FALSE)
  }

}
