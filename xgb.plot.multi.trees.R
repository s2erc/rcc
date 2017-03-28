multi.trees <- function (model, feature_names = NULL, features_keep = 5, plot_width = NULL, 
          plot_height = NULL, ...) 
{
  #check.deprecation(...)
  require(plyr)
  require(dplyr)
  tree.matrix <- xgb.model.dt.tree(feature_names = feature_names, 
                                   model = model)
  root.nodes <- tree.matrix[stringi::stri_detect_regex(ID, "\\d+-0"), 
                            ID]
  tree.matrix[ID %in% root.nodes, `:=`(abs.node.position, root.nodes)]
  precedent.nodes <- root.nodes
  while (tree.matrix[, sum(is.na(abs.node.position))] > 0) {
    yes.row.nodes <- tree.matrix[abs.node.position %in% precedent.nodes & 
                                   !is.na(Yes)]
    no.row.nodes <- tree.matrix[abs.node.position %in% precedent.nodes & 
                                  !is.na(No)]
    yes.nodes.abs.pos <- yes.row.nodes[, abs.node.position] %>% 
      paste0("_0")
    no.nodes.abs.pos <- no.row.nodes[, abs.node.position] %>% 
      paste0("_1")
    tree.matrix[ID %in% yes.row.nodes[, Yes], `:=`(abs.node.position, 
                                                   yes.nodes.abs.pos)]
    tree.matrix[ID %in% no.row.nodes[, No], `:=`(abs.node.position, 
                                                 no.nodes.abs.pos)]
    precedent.nodes <- c(yes.nodes.abs.pos, no.nodes.abs.pos)
  }
  tree.matrix[!is.na(Yes), `:=`(Yes, paste0(abs.node.position, 
                                            "_0"))]
  tree.matrix[!is.na(No), `:=`(No, paste0(abs.node.position, 
                                          "_1"))]
  remove.tree <- . %>% stringi::stri_replace_first_regex(pattern = "^\\d+-", 
                                                replacement = "")
  tree.matrix[, `:=`(abs.node.position = remove.tree(abs.node.position), 
                     Yes = remove.tree(Yes), No = remove.tree(No))]
  nodes.dt <- tree.matrix[, .(Quality = sum(Quality)), by = .(abs.node.position, 
                                                              Feature)][, .(Text = paste0(Feature[1:min(length(Feature), 
                                                                                                        features_keep)], " (", Quality[1:min(length(Quality), 
                                                                                                                                             features_keep)], ")") %>% paste0(collapse = "\n")), by = abs.node.position]
  edges.dt <- tree.matrix[Feature != "Leaf", .(abs.node.position, 
                                               Yes)] %>% list(tree.matrix[Feature != "Leaf", .(abs.node.position, 
                                                                                               No)]) %>% data.table::rbindlist() %>% data.table::setnames(c("From", "To")) %>% 
    .[, .N, .(From, To)] %>% .[, `:=`(N, NULL)]
  # my code
  all.levels <- levels(factor(union(edges.dt$From, edges.dt$To)))
  edges.dt$From <- factor(edges.dt$From, levels = all.levels)
  edges.dt$To <- factor(edges.dt$To, levels = all.levels)
  
  nodes <- DiagrammeR::create_node_df(n = nrow(nodes.dt),
                                      #label = nodes.dt[, abs.node.position], 
                                    label = nodes.dt[, Text],
                                    style = "filled", color = "DimGray", 
                                    fillcolor = "Beige", shape = "oval", fontname = "Helvetica")
  edges <- DiagrammeR::create_edge_df(from = edges.dt[, From], 
                                    to = edges.dt[, To], color = "DimGray", arrowsize = "1.5", 
                                    arrowhead = "vee", fontname = "Helvetica", rel = "leading_to")
  graph <- DiagrammeR::create_graph(nodes_df = nodes, edges_df = edges, 
                                    attr_theme = 'default')
  DiagrammeR::render_graph(graph, width = plot_width, height = plot_height, output = 'graph')
  #return(edges.dt)
}