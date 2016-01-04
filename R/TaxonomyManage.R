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
#'
addTerm <- function(taxtree, child, parent=NULL){

  if(is.null(parent)){
    taxtree$AddChild(child)
  }else{
    parent.node <- taxtree$Climb(parent)
    parent.node$AddChild(child)
  }

  return(taxtree)

}

