sanitize_for_DiagrammeR <- function(x,
                                    regExReplacements = list(c("\\\"", "`"),
                                                             c("\\'", "`"),
                                                             c("\\\\", "/"))) {
  for (i in seq_along(regExReplacements)) {
    x <- gsub(regExReplacements[[i]][1],
              regExReplacements[[i]][2],
              x);
  }
  return(x);
}

