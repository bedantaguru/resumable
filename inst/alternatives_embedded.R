

# this is for alternatives auto-complete using {patch} (NON-CRAN)

if(dir.exists(system.file(package = "patch"))){
  
  if(exists(".rs.addFunction")){
    enable_all <- function() {
      auto_complete_alternatives <- function() {
        .rs.addJsonRpcHandler(
          "get_completions",
          patch::patch_function(.rs.rpc.get_completions,
                         ".rs.getCompletionsEnvironmentVariables",
                         # addition portion
                         if (length(string) &&
                             # package(s) which ships alternatives  
                             ("resumable" %in% .packages()) &&
                             string[[1]] == "alternatives" &&
                             numCommas[[1]] == 0) {
                           
                           # package(s) which ships alternatives
                           nsr <- asNamespace("resumable")
                           nsexp <- getNamespaceExports(nsr)
                           fns <- unlist(
                             lapply(nsexp, 
                                    function(x) is.function(nsr[[x]])))
                           sels <- nsexp[fns]
                           sels <- setdiff(sels, "alternatives")
                           
                           using_alt <- unlist(
                             lapply(sels, 
                                  function(fxn){
                                    any(grepl("use_alternatives",
                                              as.character(body(nsr[[fxn]]))))
                                  }))
                           
                           sels <- sels[using_alt]
                           
                           candidates <- sels
                           results <- .rs.selectFuzzyMatches(candidates, token)
                           
                           return(.rs.makeCompletions(
                             token = token,
                             results = results,
                             quote = FALSE,
                             type = .rs.acCompletionTypes$VECTOR
                           ))
                         },
                         chop_locator_to = 1
          )
        )
      }
      
      auto_complete_alternatives_args <- function() {
        .rs.addFunction(
          "getCompletionsArgument",
          patch::patch_function(.rs.getCompletionsArgument,
                         '"knitr"',
                         if(as.character(functionCall)[1]=="alternatives"){
                           
                           altfn <- .rs.getAnywhere("alternatives", envir)
                           
                           altcallon <- as.character(functionCall[2])
                           
                           alts <- tryCatch(
                             altfn(altcallon),
                             error = function(e) NULL
                           )
                           
                           if(is.data.frame(alts)){
                             if(nrow(alts)>0){
                               
                               if (activeArg == "implement" | 
                                   activeArg == "install" |
                                   activeArg == "use") {
                                 if(activeArg == "implement"){
                                   choices <- c("best", alts$name) 
                                 }else{
                                   if(activeArg == "use"){
                                     choices <- c("best", alts$name[alts$is_usable])
                                   }else{
                                     # activeArg == "install"
                                     choices <- c("all", "best", alts$name[!alts$is_usable])
                                   }
                                 }
                                 
                                 results <- .rs.selectFuzzyMatches(choices, token)
                                 
                                 return(.rs.makeCompletions(
                                   token = token,
                                   results = results,
                                   quote = TRUE,
                                   type = .rs.acCompletionTypes$STRING
                                 ))
                               }
                               
                             }
                           }
                           
                         },
                         chop_locator_to = 1
          )
        )
      }
      
      # not called yet
      auto_complete_reset <- function() {
        # reset
        .rs.addJsonRpcHandler(
          "get_completions",
          patch::patch_function(.rs.rpc.get_completions)
        )
        
        .rs.addFunction(
          "getCompletionsArgument",
          patch::patch_function(.rs.getCompletionsArgument)
        )
      }
      
      
      # implement
      auto_complete_alternatives()
      auto_complete_alternatives_args()
    }
    enable_all()
    rm(enable_all)
  }else{
    cat("\nThese are designed for R-Studio\n")
  }
  
}else{
  cat(
    "This is for alternatives auto-complete using {patch} (NON-CRAN)", 
    "This requires {patch} which can be installed as described in:",
    "https://github.com/r-rudra/patch",
    "Also this is meant for RStudio but may cause issues with it.",
    "If this happens it is better to not use this.",
    sep = "\n")
}
