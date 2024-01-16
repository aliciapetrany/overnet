library(ggplot2)

#todos
#add edge only plotting
#add adjacency matrix input
#add automatic coordinate placing with fancy algorithms
#add style presets
#legends?
overnet.custom.network <- function(nodes, edges, plot = NULL,
                         edges.nodelabels=c("root", "sink"),
                         edges.weightlabel=NULL,
                         nodes.nodelabel="node",
                         nodes.coords=c("x","y"),
                         nodes.color = "black",
                         nodes.size = 1,
                         nodes.shape = 16,
                         nodes.alpha = 1,
                         nodes.stroke = NA,
                         nodes.label = T,
                         nodes.label.hjust = 0,
                         nodes.label.vjust = 0,
                         nodes.label.size = 1,
                         nodes.label.color = "black",
                         nodes.label.alpha = 1,
                         nodes.label.angle = 0,
                         nodes.label.family = "sans",
                         nodes.label.fontface = "plain",
                         edges.alpha = 1,
                         edges.color = "black",
                         edges.linetype = "solid",
                         edges.width = 2,
                         edges.lineend = "round",
                         edges.linejoin = "round",
                         edges.arrow = F,
                         edges.arrow.length = 1,
                         edges.arrow.unit = "cm",
                         edges.arrow.angle = 30,
                         edges.arrow.ends = "last",
                         edges.arrow.type = "closed",
                         edges.shape = "straight",
                         edges.curvature = 30,
                         edges.curve.angle = 30,
                         edges.ncp = 10,
                         edges.label = T,
                         edges.label.hjust = 0,
                         edges.label.vjust = 0,
                         edges.label.size = 1,
                         edges.label.color = "black",
                         edges.label.alpha = 1,
                         edges.label.angle = 0,
                         edges.label.family = "sans",
                         edges.label.fontface = "plain"
                         ){

  addon = T
  if(is.null(plot)){
    plot = ggplot()
    addon = F
  }
  g = plot

  nodeparams <- list(nodes.color,
                    nodes.size,
                    nodes.shape,
                    nodes.alpha,
                    nodes.stroke,
                    nodes.label,
                    nodes.label.hjust,
                    nodes.label.vjust,
                    nodes.label.size,
                    nodes.label.color,
                    nodes.label.alpha,
                    nodes.label.angle,
                    nodes.label.family,
                    nodes.label.fontface)

  edgeparams <- list(edges.alpha,
                  edges.color,
                  edges.linetype,
                  edges.width,
                  edges.lineend,
                  edges.linejoin,
                  edges.arrow,
                  edges.arrow.length,
                  edges.arrow.unit,
                  edges.arrow.angle,
                  edges.arrow.ends,
                  edges.arrow.type,
                  edges.shape,
                  edges.curvature,
                  edges.curve.angle,
                  edges.ncp,
                  edges.label,
                  edges.label.hjust,
                  edges.label.vjust,
                  edges.label.size,
                  edges.label.color,
                  edges.label.alpha,
                  edges.label.angle,
                  edges.label.family,
                  edges.label.fontface)

  nodes <- node.settings(nodes, nodeparams)
  edges <- edge.settings(edges, edgeparams)

  colnames(edges)[colnames(edges) %in% edges.nodelabels[1]] = "root"
  colnames(edges)[colnames(edges) %in% edges.nodelabels[2]] = "sink"
  colnames(nodes)[colnames(nodes) %in% nodes.coords[1]] = "x"
  colnames(nodes)[colnames(nodes) %in% nodes.coords[2]] = "y"
  colnames(nodes)[colnames(nodes) %in% nodes.nodelabel] = "node"

  if(!is.null(edges.weightlabel)){
    colnames(edges)[colnames(edges) %in% edges.weightlabel] = "weight"
  }


  g <- plot_edges(g, edges, nodes)
  g <- plot_nodes(g, nodes)

  g <- plot_node_labels(g, nodes)
  g <- plot_edge_labels(g, edges, nodes)

  return(g)
}

plot_nodes <- function(g, nodes){
  g <- g + geom_point(data = nodes, aes(x = x, y = y),
                        color = nodes$color,
                        size = nodes$size,
                        shape = nodes$shape,
                        alpha = nodes$alpha,
                        stroke = nodes$stroke)
  return(g)
}

plot_node_labels <- function(g, nodes){
  temp = nodes[nodes$label,]
  g <-  g + annotate("text",
                 x = temp$x + temp$label.hjust,
                 y = temp$y + temp$label.vjust,
                 label = temp$node,
                 fontface = temp$label.fontface,
                 family = temp$label.family,
                 size = temp$label.size,
                 color = temp$label.color,
                 alpha = temp$label.alpha,
                 angle = temp$label.angle)
  return(g)
}

plot_edge_labels <- function(g, edges, nodes){
  for(i in 1:nrow(edges)){
    node1.x = nodes$x[nodes$node %in% edges[i,]$root]
    node1.y = nodes$y[nodes$node %in% edges[i,]$root]
    node2.x = nodes$x[nodes$node %in% edges[i,]$sink]
    node2.y = nodes$y[nodes$node %in% edges[i,]$sink]

    if(edges$label[i]){
      g <- g + annotate("text",
                        x = (node1.x + node2.x)/2 + edges$label.hjust[i],
                        y = (node1.y + node2.y)/2 + edges$label.vjust[i],
                        label = edges$weight[i],
                        fontface = edges$label.fontface[i],
                        family = edges$label.family[i],
                        size = edges$label.size[i],
                        color = edges$label.color[i],
                        alpha = edges$label.alpha[i],
                        angle = edges$label.angle[i])
    }
  }
  return(g)
}
plot_edges <- function(g, edges, nodes){
  for(i in 1:nrow(edges)){
    node1.x = nodes$x[nodes$node %in% edges[i,]$root]
    node1.y = nodes$y[nodes$node %in% edges[i,]$root]
    node2.x = nodes$x[nodes$node %in% edges[i,]$sink]
    node2.y = nodes$y[nodes$node %in% edges[i,]$sink]


    if(edges$arrow[i]){

      if(edges$shape[i] == "straight"){
        g <- g + geom_segment(aes(x = !!node1.x,
                                  y = !!node1.y,
                                  xend = !!node2.x,
                                  yend = !!node2.y),
                              color = edges$color[i],
                              linewidth = edges$width[i],
                              alpha = edges$alpha[i],
                              linetype = edges$linetype[i],
                              lineend = edges$lineend[i],
                              linejoin = edges$linejoin[i],
                              arrow = arrow(length = unit(edges$arrow.length[i],
                                                          edges$arrow.unit[i]),
                                            angle = edges$arrow.angle[i],
                                            ends = edges$arrow.ends[i],
                                            type = edges$arrow.type[i]))
      } else{
        g <- g + geom_curve(aes(x = !!node1.x,
                                y = !!node1.y,
                                xend = !!node2.x,
                                yend = !!node2.y),
                            color = edges$color[i],
                            linewidth = edges$width[i],
                            alpha = edges$alpha[i],
                            linetype = edges$linetype[i],
                            lineend = edges$lineend[i],
                            linejoin = edges$linejoin[i],
                            arrow = arrow(length = unit(edges$arrow.length[i],
                                                        edges$arrow.unit[i]),
                                          angle = edges$arrow.angle[i],
                                          ends = edges$arrow.ends[i],
                                          type = edges$arrow.type[i]),
                            curvature = edges$curvature[i],
                            angle = edges$curve.angle[i],
                            ncp = edges$ncp[i])
      }

    }else{
      if(edges$shape[i] == "straight"){
        g <- g + geom_segment(aes(x = !!node1.x,
                                  y = !!node1.y,
                                  xend = !!node2.x,
                                  yend = !!node2.y),
                              color = edges$color[i],
                              linewidth = edges$width[i],
                              alpha = edges$alpha[i],
                              linetype = edges$linetype[i],
                              lineend = edges$lineend[i],
                              linejoin = edges$linejoin[i])
      } else{
        g <- g + geom_curve(aes(x = !!node1.x,
                                y = !!node1.y,
                                xend = !!node2.x,
                                yend = !!node2.y),
                            color = edges$color[i],
                            linewidth = edges$width[i],
                            alpha = edges$alpha[i],
                            linetype = edges$linetype[i],
                            lineend = edges$lineend[i],
                            linejoin = edges$linejoin[i],
                            curvature = edges$curvature[i],
                            angle = edges$curve.angle[i],
                            ncp = edges$ncp[i])
      }
    }
  }

  return(g)
}



