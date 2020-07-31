#' Programmatically constructing justifier elements
#'
#' These functions can be used to programmatically construct justifications.
#'
#' @param label A human-readable label for the `decision`, `justification`,
#' `assertion`, or `source`.
#' @param description A human-readable description.
#' @param xdoi,type For `source`s, XDOI identifier and type can also be
#' specified.
#' @param source In assertions, the source (or sources) that the assertion
#' is based on can be specified using `srce()`.
#' @param assertion In justifications, the assertion (or assertions) that
#' the justification is based on can be specified using `asrt()`.
#' @param justification In decisions, the justification (or justifications)
#' that the decision is based on can be specified using `jstf()`.
#' @param id The identifier (randomly generated if omitted).
#' @param ... Additional fields and values to store in the element.
#'
#' @rdname constructingJustifications
#' @aliases dcsn jstf asrt srce
#' @return The generated object.
#'
#' @examples ### Programmatically create a simple justification object
#' justifierObject <-
#'   justifier::asrt(
#'     "assertion",
#'     source = c(
#'       justifier::srce('source1'),
#'       justifier::srce('source2')));
#' @export srce
source <-
  srce <- function(label,
                   description = NULL,
                   xdoi = NULL,
                   type = NULL,
                   id = NULL,
                   ...) {
  return(justifierObjectConstructor(justifierType = "S",
                                    id=id,
                                    label = label,
                                    description = description %||% "",
                                    xdoi = xdoi %||% "",
                                    type = type %||% "",
                                    ...));
}

#' @export assert asrt
#' @rdname constructingJustifications
assert <-
  asrt <-
          function(label,
                   description = "",
                   id = NULL,
                   source = NULL,
                   ...) {
  return(justifierObjectConstructor(justifierType = "A",
                                    id=id,
                                    label = label,
                                    description = description %||% "",
                                    source = source %||% NULL,
                                    ...));
}

#' @export justify jstf
#' @rdname constructingJustifications
justify <-
   jstf <- function(label,
                    description = "",
                    id = NULL,
                    assertion = NULL,
                    ...) {
  return(justifierObjectConstructor(justifierType = "J",
                                    id=id,
                                    label = label,
                                    description = description %||% "",
                                    assertion = assertion %||% NULL,
                                    ...));
}

#' @export dcsn
#' @rdname constructingJustifications
dcsn <- function(label,
                 description = NULL,
                 id = NULL,
                 justification = NULL,
                 ...) {
  return(justifierObjectConstructor(justifierType = "D",
                                    id=id,
                                    label = label,
                                    description = description %||% "",
                                    justification = justification %||% "",
                                    ...));
}

#' @method c justifierElement
#' @export
#' @rdname constructingJustifications
c.justifierElement <- function(...) {

  ### Get arguments in a list
  res <- list(...);

  elementType <-
    unlist(lapply(res,
                  function(x) {
                    return(head(class(x), 1));
                  }));

  if (length(unique(elementType)) != 1) {
    stop("All elements to concatenate must be of the same type! ",
         "So either all decisions, or all justifications, or all ",
         "assertions, or all sources - you passed elements of types ",
         vecTxtQ(elementType), ", respectively.");
  }

  elementType <- unique(elementType);

  ### If any of the arguments does itself have multiple elements,
  ### we need to place the single elements in lists.
  if (any(unlist(lapply(res, class)) == "multipleJustifierElements")) {
    res <-
      lapply(res,
             function(x) {
               return(ifelseObj("singleJustifierElement" %in% class(x),
                                structure(list(x),
                                          class = c(elementType,
                                                    "singleJustifierElements",
                                                    "justifierElement",
                                                    "justifier")),
                                x));
             });
    ### ... And then remove one level of lists
    res <- unlist(res,
                  recursive = FALSE);
  }

  ### Set names to identifiers
  ### We can't do this until the parser can handle it.
  # names(res) <-
  #   unlist(lapply(res,
  #                 function(x) {
  #                   return(x$id);
  #                 }));

  class(res) <- c(elementType, "multipleJustifierElements", "justifierElement", "justifier");
  return(res);
}

#' @export
#' @method print singleJustifierElement
print.singleJustifierElement <- function(x, ...) {
  cat0("Justifier element of type '",
       class(x)[1], "' and with id '",
       x$id,
       "'.");
  return(invisible(x));
}

#' @export
#' @method print multipleJustifierElements
print.multipleJustifierElements <- function(x, ...) {
  cat0("A list of ", length(x), " justifier elements of ",
       "type ", class(x[[1]])[1], " and with identifiers ",
       vecTxtQ(unlist(lapply(x, function(y) return(y$id)))));
  return(invisible(x));
}

justifierObjectConstructor <-
  function(justifierType,
           id = NULL,
           ...) {

    justifierType <- toupper(justifierType);
    if (length(justifierType) > 1) {
      stop("I can only generate one justifier object!");
    }
    if (!any(c("D", "J", "A", "S") == justifierType)) {
      stop("I can only generate an object of type D(ecision), ",
           "J(ustifier), A(ssertion), or S(ource)!");
    }

    if (is.null(id)) {
      id <- generate_id(type=justifierType)
    }

    res <- c(list(id = id),
             list(...));

    justifierClasses <-
      list(D = "justifierDecision",
           J = "justifierJustification",
           A = "justifierAssertion",
           S = "justifierSource");

    class(res) <-
      c(justifierClasses[justifierType],
        "singleJustifierElement",
        "justifierElement",
        "justifier");

    return(res);

  }
