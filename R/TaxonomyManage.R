#'#################################################################
#' This file has funtions for managing taxonomy in a tree structure
#'#################################################################


#'#################################################################
#' function to add a word to the taxonomy
#'
#' @param taxtree - the taxonomy tree to which word needs to be added
#' @param child - word to be added to taxonomy tree
#' @param parent - parent to which term will be added as child,
#' if parent is NULL then word is added to Root of tree
#'
#' @return updated taxonomy tree
#'
#' @export
addTerm <- function(taxtree, child, parent=NULL){

  if(is.null(parent)){
    taxtree$AddChild(child)
  }else{
    parent.node <- taxtree$Climb(parent)
    parent.node$AddChild(child)
  }

  return(taxtree)

}


#'#################################################################
#' function to create a list of list from a data.tree that can be
#' displayed using shinyTree package
#'
#' @param taxtree - the taxonomy tree to which word needs to be added
#'
#' @return taxonomy as list of lists
#'
#' @export
toShinyTreeList <- function (x)
{
  self <- x
  res <- list()

  if(x$isRoot){
    l_nameName <- self$name
    res[l_nameName] <- self$name
  }

  fields <- self$fields
  fields <- fields[!is.function(fields) && !is.environment(fields)]

  for (fieldName in fields) res[fieldName] <- self[[fieldName]]

  if (!self$isLeaf) {
    kids <- lapply(self$children, FUN = function(x) toShinyTreeList(x))

    res <- c(res, kids)

  }else{
    if(!x$isRoot) res <- self$name
  }

  return(res)
}
