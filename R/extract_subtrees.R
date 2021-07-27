#' extract_subtrees
#'
#' \code{extract_subtrees} extracts unconnected subtrees given a graph and a list of nodes
#'
#' This function requires the \code{\link[topGO]} and \code{\link[graph]} packages to be installed.
#'
#' @param graph a graphNEL object
#' @param nodes a character vector of node labels to produce subtrees for
#'
#' @return a list of graphNEL objects
#'
#' @examples
#' listOfSubTrees <- extract_subtrees(graph, nodes)
#'
#' @export
extract_subtrees <- function(graph, nodes){
  # create induced graph from the supplied nodes
  inducedG <- topGO::inducedGraph(graph, nodes)
  totalNodes <- numNodes(inducedG)

  # build levels of induced graph
  inducedGLevels <- topGO::buildLevels(inducedG)

  # create reversed graph of induced graph (same nodes, reversed direction of edges)
  reversedG <- topGO::reverseArch(inducedG)

  # create an environment to keep a track of which nodes we have visited
  nodesVisited <- new.env(hash = T, parent = emptyenv())

  # start at the root of the induced graph and continue until all nodes have been seen
  subtreeList <- vector('list', length = length(nodes))
  i <- 1
  for( level in as.character(seq_len(inducedGLevels$noOfLevels)) ){
    # break out of the loop if we have seen all the supplied nodes already
    if( length(ls(nodesVisited)) == totalNodes ){
      break
    } else{
      for (node in inducedGLevels$level2nodes[[level]]){
        # cat(node, "\n")
        if ( length(ls(nodesVisited)) == totalNodes ){
          break
        } else if (exists(node, envir = nodesVisited)) {
          next
        } else {
          # if the node is in the nodes list then get the induced graph from that nodes to the leaves using the reversed graph
          # if not carry on
          if( node %in% nodes ){
            subtree <- topGO::inducedGraph(reversedG, node)
            # add any nodes in this induced graph to the nodes visited environment
            nodesInSubtree <- topGO::nodesInInducedGraph(reversedG, node)
            invisible( lapply(nodesInSubtree, function(node){ nodesVisited[[node]] <- TRUE }) )

            # if induced subtree has more than one node reverse it and add to subtrees list
            if (numNodes(subtree) > 1){
              subtreeList[[i]] <- topGO::reverseArch(subtree)
            } else {
              subtreeList[[i]] <- subtree
            }
            i <- i + 1
          } else {
            nodesVisited[[node]] <- TRUE
          }
        }
      }
    }
  }
  # tidy up and return subtrees list
  numTrees <- sum( sapply(subtreeList, function(x){ !is.null(x) } ) )
  subTrees <- vector('list', length = numTrees)
  for( i in seq_len(numTrees) ){
    subTrees[[i]] <- subtreeList[[i]]
  }
  return(subTrees)
}
