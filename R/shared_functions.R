node.settings <- function(nodes, nodeparams){

  paramlabs = c("color",
                "size",
                "shape",
                "alpha",
                "stroke",
                "label",
                "label.hjust",
                "label.vjust",
                "label.size",
                "label.color",
                "label.alpha",
                "label.angle",
                "label.family",
                "label.fontface")

  for( i in 1:length((paramlabs))){
    if(!(paramlabs[i] %in% colnames(nodes))){
      if(length(nodeparams[[i]]) != 1 & length(nodeparams[[i]]) != nrow(nodes) ){
        stop(paste("The length of nodes.", paramlabs[i], " must be 1 or equal to the number of nodes", sep = ""))
      }
      else if(length(nodeparams[[i]]) == 1){
        temp = data.frame(X = rep(nodeparams[[i]], nrow(nodes)))
      }
      else{
        temp = data.frame(X = nodeparams[[i]])
      }
      colnames(temp) = paramlabs[i]
      nodes <- cbind(nodes, temp)
    }
  }
  return(nodes)
}

edge.settings<-function(edges, edgeparams){

  paramlabs = c("alpha",
                "color",
                "linetype",
                "width",
                "lineend",
                "linejoin",
                "arrow",
                "arrow.length",
                "arrow.unit",
                "arrow.angle",
                "arrow.ends",
                "arrow.type",
                "shape",
                "curvature",
                "curve.angle",
                "ncp",
                "label",
                "label.hjust",
                "label.vjust",
                "label.size",
                "label.color",
                "label.alpha",
                "label.angle",
                "label.family",
                "label.fontface")

  for( i in 1:length((paramlabs))){
    if(!(paramlabs[i] %in% colnames(edges))){
      if(length(edgeparams[[i]]) != 1 & length(edgeparams[[i]]) != nrow(edges) ){
        stop(paste("The length of edges.", paramlabs[i], " must be 1 or equal to the number of edges", sep = ""))
      }
      else if(length(edgeparams[[i]]) == 1){
        temp = data.frame(X = rep(edgeparams[[i]], nrow(edges)))
      }
      else{
        temp = data.frame(X = edgeparams[[i]])
      }
      colnames(temp) = paramlabs[i]
      edges <- cbind(edges, temp)
    }
  }

  return(edges)
}
