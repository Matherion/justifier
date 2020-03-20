#' Programmatically constructing justifier elements
#'
#' These functions can be used to programmatically construct justifications.
#'
#' @param label A human-readable label for the `decision`, `justification`,
#' `assertion`, or `source`.
#' @param description A human-readable description.
#' @param xdoi,type For `source`s, XDOI identifier and type can also be
#' specified.
#' @param id The identifier (randomly generated if omitted).
#' @param ... Additional fields and values to store in the element.
#'
#' @rdname constructingJustifications
#' @return The generated object.
#' @export
#'
#' @examples ### Programmatically create a simple justification object
#' justifierObject <-
#'   justifier::asrt(
#'     "assertion",
#'     source = c(
#'       justifier::srce('source1'),
#'       justifier::srce('source2')));
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

#' @export
#' @rdname constructingJustifications
asrt <- function(label,
                 description = NULL,
                 id = NULL,
                 source = NULL,
                 ...) {
  return(justifierObjectConstructor(justifierType = "A",
                                    id=id,
                                    label = label,
                                    description = description %||% "",
                                    source = source %||% "",
                                    ...));
}

#' @export
#' @rdname constructingJustifications
jstf <- function(label,
                 description = NULL,
                 id = NULL,
                 assertion = NULL,
                 ...) {
  return(justifierObjectConstructor(justifierType = "J",
                                    id=id,
                                    label = label,
                                    description = description %||% "",
                                    assertion = assertion %||% "",
                                    ...));
}

#' @export
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
  res <- list(...);
  names(res) <-
    unlist(lapply(res,
                  function(x) {
                    return(x$id);
                  }));
  return(res);
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
        "justifierElement",
        "justifier");

    return(res);

  }
