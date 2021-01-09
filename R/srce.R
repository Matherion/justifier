#' Programmatically constructing justifier elements
#'
#' These functions can be used to programmatically construct justifications.
#'
#' @param label A human-readable label for the `decision`, `justification`,
#' `assertion`, or `source`. Labels are brief summaries of the core of the
#' decision, justification, assertion, or source. More details, background
#' information, context, and other comments can be placed in the description.
#' @param description A human-readable description. This can be used to
#' elaborate on the label. Note that the label should be reader-friendly and
#' self-contained; but because they also have to be as short as possible,
#' descriptions can be used to provide definitions, context, background
#' information, or add any other metadata or comments.
#' @param type Types are used when working with a framework. Frameworks define
#' type identifiers, consisting of letters, digits, and underscores. By
#' specifying these identifiers the type of a decision, justification,
#' assertion, or source. Source types can be, for example, types of documents
#' or other data providers, such as "empirical evidence', 'expert consensus',
#' 'personal opinion', or 'that one meeting that we had in May'. Assertion
#' types can be, for example, data types or types of facts, such as 'number',
#' 'prevalence', 'causal relationship', or 'contact information'.
#' Justification types can be, for example, types of reasoning or logical
#' expressions, such as 'deduction', 'induction', or 'intersection'. Decision
#' types are the most framework-specific, heavily depend on the specific
#' context of the decision, and are used by frameworks to organise the
#' decisions in a project. Examples of decision types are the decision to
#' recruit a certain number of participants in a scientific study; the decision
#' to target a certain belief in a behavior change intervention; the decision
#' to merge two codes in a qualitative study; the decision to hire a staff
#' member; or the decision to make a certain purchase.
#' @param xdoi For `source`s, XDOI identifier (a DOI, or, if that does not
#' exist, ISBN or other unique identifier of the source).
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
                   type = NULL,
                   id = NULL,
                   xdoi = NULL,
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
                   type = NULL,
                   id = NULL,
                   source = NULL,
                   ...) {
  return(justifierObjectConstructor(justifierType = "A",
                                    id=id,
                                    label = label,
                                    description = description %||% "",
                                    type = type %||% "",
                                    source = source %||% NULL,
                                    ...));
}

#' @export justify jstf
#' @rdname constructingJustifications
justify <-
   jstf <- function(label,
                    description = "",
                    type = NULL,
                    id = NULL,
                    assertion = NULL,
                    ...) {
  return(justifierObjectConstructor(justifierType = "J",
                                    id=id,
                                    label = label,
                                    description = description %||% "",
                                    type = type %||% "",
                                    assertion = assertion %||% NULL,
                                    ...));
}

#' @export dcsn
#' @rdname constructingJustifications
decide <-
  dcsn <- function(label,
                   description = NULL,
                   type = NULL,
                   id = NULL,
                   justification = NULL,
                   ...) {
  return(justifierObjectConstructor(justifierType = "D",
                                    id=id,
                                    label = label,
                                    description = description %||% "",
                                    type = type %||% "",
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
                    return(utils::head(class(x), 1));
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
