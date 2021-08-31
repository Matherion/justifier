#' Concatenate two or more structured justifier objects
#'
#' @param ... Structured justifier objects
#'
#' @return Invisibly, the concatenated list
#' @export
#'
#' @method c justifierStructured
#' @export
#' @rdname structuredJustifications
c.justifierStructured <- function(...) {

  ### Get arguments in a list
  dots <- list(...);

  dots <- dots[!is.null(dots)];

  if (!(all(unlist(lapply(dots, inherits, "justifierStructured"))))) {
    stop("I can only concatenate objects with class 'justifierStructured', ",
         "but you passed at least one object with a different class!");
  }

  res <-
    list(
      sources = do.call(c, lapply(dots, function(x) return(x$sources))),
      assertions = do.call(c, lapply(dots, function(x) return(x$assertions))),
      justifications = do.call(c, lapply(dots, function(x) return(x$justifications))),
      decisions = do.call(c, lapply(dots, function(x) return(x$decisions))),
      justifier = do.call(c, lapply(dots, function(x) return(x$justifier)))
    );

  class(res) <-
    c("justifier", "justifierStructured", "list");

  return(invisible(res));

}
