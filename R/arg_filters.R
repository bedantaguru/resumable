

arg_filter<- function(args){

  # these functions has to be inside arg_filter for parallel processing
  # all argument passed as 5 will be treated as integer 5 etc.
  arg_filter_numeric <- function(inarg){
    outarg <- inarg
    if(is.numeric(inarg)){
      chk <- unlist(lapply(inarg, function(x){
        x == as.integer(x)
      }))
      if(all(chk)){
        outarg <- as.integer(inarg)
      }
    }
    outarg
  }

  oargs <- lapply(args, arg_filter_numeric)
  oargs
}
