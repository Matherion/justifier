create_justifierGraph <- function(dTree) {

  tryCatch({

    dTree$Do(function(node) {

      lbl <-
        ifelse(is.null(node$label),
               node$name,
               node$label);

      lbl <- justifier::sanitize_for_DiagrammeR(lbl);

      lbl <- paste0(strwrap(lbl, 40), collapse = "\n");

      data.tree::SetNodeStyle(node,
                              label = lbl);

    });

    dTreeGraph <-
      data.tree::ToDiagrammeRGraph(dTree);

    dTreeGraph <-
      justifier::apply_graph_theme(
        dTreeGraph,
        c("layout", "dot", "graph"),
        c("rankdir", "LR", "graph"),
        c("outputorder", "edgesfirst", "graph"),
        c("fixedsize", "false", "node"),
        c("shape", "box", "node"),
        c("style", "rounded,filled", "node"),
        c("color", "#000000", "node"),
        c("margin", "0.2,0.2", "node"),
        c("color", "#888888", "edge"),
        c("dir", "none", "edge"),
        c("headclip", "false", "edge"),
        c("tailclip", "false", "edge"),
        c("fillcolor", "#FFFFFF", "node"),
        c("fontname", "Arial", "node")
      )

  },
  error = function(e) {
    warning(
      "Error issued when converting decision tree to a decision graph: ",
      e$message,
      "\n\nClass and content:\n\n",
      paste0(utils::capture.output(print(class(
        dTree
      ))),
      collapse = "\n"),
      "\n",
      paste0(utils::capture.output(print(dTree)),
             collapse = "\n")
    )

  });

  if (is.null(dTreeGraph)) {
    dTreeGraph <- NA;
  }

  class(dTreeGraph) <-
    c("justifierDecisionGraph", class(dTreeGraph));

  return(dTreeGraph);

}
