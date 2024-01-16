library(ggplot2)
nodess = data.frame(node = c("A", "B", "C", "D"),
                   x = c(1,0,3,-1),
                   y = c(1,2,3,4))
edgess = data.frame(root = c("A","C", "C", "D", "D"),
                   sink = c("B", "A", "D", "B", "A"),
                   weight = c(1,2,3,4,5))
plot_network(nodes, edges,
             nodes.size = c(4,4,4,4),
             nodes.shape = 1,
             nodes.stroke = 3,
             nodes.color = "orange",
             edges.color = "purple",
             edges.alpha = 1,
             edges.width =1,
             nodes.label = T,
             nodes.label.color = "blue",
             nodes.label.size = 2,
             edges.label = c(T,T,F,F,T),
             edges.label.size =10,
             edges.label.color ="red")

