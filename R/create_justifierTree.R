create_justifierTree <- function(x) {

  ### Call 'buildListWithChildren' from the right starting point; then
  ### that function will recurse to structure data.tree's 'explicit list'
  ### properly

  targetElement <-
    names(parentChildRelationships)[
      which(names(parentChildRelationships) %in% names(x))[1]
    ];

  x <-
    buildExplicitDataTreeList(
      x,
      targetElement = targetElement,
      childElement = parentChildRelationships[targetElement]
    );

  if ("id" %in% names(x)) {
    x <- list(x);
    names(x) <- x[[1]]$id;
  }

  res <-
    lapply(names(x),
           function(decisionId) {
             res <-
               data.tree::FromListExplicit(explicitList = x[[decisionId]],
                                           nameName="id",
                                           childrenName="children",
                                           nodeName=decisionId);
             return(res);
           });

  return(res);

}

parentChildRelationships = c(
  decision = 'justification',
  justification = 'assertion',
  assertion = 'source',
  source = NULL
)

justifierClasses = c(
  decision = 'justifierDecisionList',
  justification = 'justifierJustificationList',
  assertion = 'justifierAssertionList',
  source = 'justifierSourceList'
)

buildExplicitDataTreeList <- function(x,
                                      targetElement,
                                      childElement = NULL) {

  ### If this is a vector (e.g. a source with just an id), return it.
  if (is.atomic(x)) {
    return(x);
  }

  ### If this is a list without the indicated children, return it unclassed.
  if (!(targetElement %in% names(x))) {
    return(unclass(x));
  }

  ### Data Tree can create a tree of an 'explicit list', which
  ### basically wants the children to be in an element called
  ### 'children'.
  if (!is.null(x$children)) {
    x$children_old <- x$children;
  }

  x$children <- x[[targetElement]];

  if (all(unlist(lapply(x$children, is.list)))) {
    ### Only in this case, `lapply` through the lists; otherwise, we
    ### have only one child without the 'intermediate list', so introduce
    ### that
    x$children <-
      lapply(x$children,
             function(child) {
               # class(child) <- justifierClasses[targetElement];
               # child$justifierType <- justifierClasses[targetElement];
               return(child);
             });
  } else {
    ### Add the 'intermediate list'
    x$children <- list(x$children);
    # class(x$children) <- justifierClasses[targetElement];
    # x$children$justifierType <- justifierClasses[targetElement];
  }

  x[targetElement] <- NULL;

  if (!is.null(childElement)) {
    x$children <-
      lapply(
        x$children,
        buildExplicitDataTreeList,
        targetElement = childElement,
        childElement = parentChildRelationships[childElement]
      );
  }

  names(x$children) <-
    unlist(lapply(x$children, function(y) {
      if (is.atomic(y)) {
        return(y['id']);
      } else {
        return(y$id);
      }
    }));

  return(unclass(x));
}
