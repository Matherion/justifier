has_justifierElement <- function(x,
                                 type) {
  return(
    !(is.null(x[[type]]) || all(is.na(x[[type]])) || all((length(x[[type]]) == 0)))
  );
}
