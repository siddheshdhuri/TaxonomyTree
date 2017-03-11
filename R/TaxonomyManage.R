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
addTerm <- function(taxtree, child, path=NULL){
  child <- stringr::str_trim(child)
  if(is.null(path)){
    taxtree$AddChild(child)
  }else{
    parent.node <- taxtree
    for(elem in path){
      parent.node <- parent.node$Climb(elem)
    }
    parent.node$AddChild(child)
  }

  return(taxtree)

}


#'#################################################################
#' function to create a list of list from a data.tree that can be
#' displayed using shinyTree package
#'
#' @param taxtree - the taxonomy data.tree
#'
#' @return taxonomy as list of lists
#'
#' @export
toShinyTreeList <- function (taxtree)
{
  self <- taxtree
  res <- list()

  if(taxtree$isRoot){
    l_nameName <- self$name
    res[l_nameName] <- self$name
  }
#
#   fields <- self$fields
#   fields <- fields[!is.function(fields) && !is.environment(fields)]

#  for (fieldName in fields) res[fieldName] <- self[[fieldName]]

  if (!self$isLeaf) {
    kids <- lapply(self$children, FUN = function(x) toShinyTreeList(x))

    res <- c(res, kids)

  }else{
    if(!taxtree$isRoot) res <- self$name
  }

  return(res)
}

#'###############################################################################################
#' this functions turns a list prepared for shinyTree package into a json string to be fed directly into treejs.
#'
#' @param list of list that constitute the tree structure
#'
#' @return string that can be used to directly pain the tree
#'
#' @export
toTreeJSON = function(list){
  outString = '['
  for (i in 1:length(list)){
    outString %<>% paste0("{'text' : '",  names(list)[i], "'")
    attribs = attributes(list[[i]])

    stateAttribs = attribs[grepl('opened|disabled|selected',names(attribs))]
    children = attribs[grepl('names',names(attribs))]
    others = attribs[!grepl('opened|disabled|selected|names',names(attribs))]

    if (length(stateAttribs) >0){
      outString %<>% paste0(", 'state' : {")
      for (j in 1:length(stateAttribs)){
        outString %<>% paste0("'",gsub('st','',names(stateAttribs)[j]),"' : ", tolower(stateAttribs[j]))
        if (j < length(stateAttribs)){
          outString %<>% paste0(",")
        }
      }
      outString %<>% paste('}')
    }

    if(length(others)>0){
      for (j in 1:length(others)){
        outString %<>% paste0( ", '",gsub('st','',names(others)[j]),"' : '", others[j],"'")
      }
    }

    if (class(list[[i]]) == 'list'){
      outString %<>% paste0(", 'children' : ",toTreeJSON(list[[i]]))
    }

    outString %<>% paste0("}")
    if (i < length(list)){
      outString %<>% paste0(",")
    }

  }
  outString %<>% paste0(']')

  return(outString)

}



#'#################################################################
#' function to create a data.tree from a list of list that can be
#' used for data.tree operations
#'
#' @param x taxonomy as list of lists
#'
#' @return data.tree
#'
#' @export
fromShinyTreeList <- function (x){

  #fist item of list is the root node
  root.node <- Node$new(names(x[1]))

  # other items are branches which will be used to grow the tree
  tree.list <- x[-1]

  #recursive function to grow rest of the tree from the tree.list
  growTree <- function(node){

    parent.name <- names(node)

    parent.node <- Node$new(parent.name)

    #if the current node's value is a list then it has children
    # we need to create subtree for the children
    if(is.list(node[[1]])){

      sub.parent <- node[[1]]
      children.names <- names(sub.parent)

      for(child in children.names){

        sub.tree <- growTree(sub.parent[child])

        parent.node$AddChildNode(sub.tree)
      }
    }

    return(parent.node)

  }


  #running loop for every branch to grow the tree.
  for(i in 1:length(tree.list)){
    sub.tree <- growTree(tree.list[i])
    root.node$AddChildNode(sub.tree)
  }

  return(root.node)

}


#'#############################################################################################
#' Function to compute frequency for all terms
#'
#' @param treedf tree as a data frame with column pathString
#' @param the text corpus in which to look for term
#'
#' @return data.frame with updated frequencies
#'
#' @export
computeFrequency <- function(treedf, corpus){

  term.count <- unlist(lapply(treedf$pathString, function(x) {
                                                      term <- tail(unlist(strsplit(x,"/")),1)

                                                      tocount <- paste0("\\b",term,"\\b")

                                                      count <- stringr::str_count(corpus,stringr::regex(tocount,ignore_case = TRUE))

                                                      return(count)

                                                      }
                      )
                )

  treedf$freq <- term.count

  return(treedf)

}



#'#################################################################
#' function to get all children of selected nodes
#'
#' @param taxtree - the taxonomy tree
#' @param selected.nodes vector of selected tree nodes
#'
#' @return list of all node and its children
#'
#' @export
getSelectedNodeChildren <- function(taxtree, selected.nodes){

  selected.paths <- list()

  for(leaf in selected.nodes) {
    #' get ancestors of the selected node of shinyTree
    ancestors <- attr(leaf, "ancestry")
    
    selected.path <- c(ancestors,leaf)

    #' climb the tree to the selected node
    parent.node <- taxtree
    for(elem in selected.path){
      parent.node <- parent.node$Climb(elem)
    }
  
    #' get leaf children of node
    leaves <- as.character(parent.node$Get('name'))

    selected.paths[[leaf]] <- leaves
  }

  return(selected.paths)

}
