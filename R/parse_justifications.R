parse_justifications <- function(x) {

  res <- list(raw = x);

  ### Process all justifications and create four organised lists,
  ### where id's are used as names
  justNames <- tolower(names(x));
  res$structured <-
    list(sources = x[which(justNames == 'source')],
         assertions = x[which(justNames == 'assertion')],
         justifications = x[which(justNames == 'justification')],
         decisions = x[which(justNames == 'decision')]);

  ### Set names
  res$structured <- lapply(res$structured, function(elementList) {
    elementIds <-
      lapply(elementList, function(singleElement) {
        return(ifelse(is.null(singleElement$id),
                      "noID",
                      singleElement$id));
      });
    ### Set id's for elements without an id (shouldn't happen;
    ### may want to throws an error instead)
    if (any(elementIds=="noID")) {
      warning("Some elements don't have an ID!");
    }
    elementIds[elementIds=="noID"] <-
      paste0("id_",
             1:length(elementIds[elementIds=="noID"]));
    names(elementList) <-
      elementIds;
    return(elementList);
  });

  ### Vectors with singular, plural, and parents of elements
  sType <- c("justification", "assertion", "source");
  pType <- c("justifications", "assertions", "sources");
  parent <- c("decisions", "justifications", "assertions");
  for (i in 1:3) {
    ### Check whether the justifications in the decisions are
    ### all referenced, or whether some are also *specified*
    ### there. If some are specified, copy them to the
    ### 'justifications list'.
    for (j in seq_along(res$structured[[parent[i]]])) {
      ### Make copy for convenience and to still understand
      ### the code
      currentElement <-
        res$structured[[parent[i]]][[j]];
      if (length(currentElement[[sType[i]]]) > 1) {
        ### If length is 0 or 1, it can never be a complete specification
        if (length(names(currentElement[[sType[i]]])) > 1) {
          ### Either it's a vector with id's or it's a specified
          ### justification
          if (!is.null(currentElement[[sType[i]]]$id)) {
            ### It's a specification of a justification
            if (currentElement[[sType[i]]]$id %in% names(res$structured[[pType[i]]])) {
              ### Check whether it already exists as root element. If it does,
              ### check all fields, and then synchronize them; if not, copy it over.

              for (currentField in union(names(res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]]),
                                         names(currentElement[[sType[i]]]))) {
                if (!is.null(res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]]) &&
                    !is.null(currentElement[[sType[i]]][[currentField]])) {
                  ### Both fields are set
                  if (res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]] !=
                      currentElement[[sType[i]]][[currentField]]) {
                    ### Combine them, with warning
                    ### First store in currentElement
                    currentElement[[sType[i]]][[currentField]] <-
                      paste0("In ", currentElement$id, ": '",
                             currentElement[[sType[i]]][[currentField]],
                             "'; in root specification: '",
                             res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]],
                             "'.");
                    ### Also copy in `res` object, instead of the 'copy-on-modify' currentElement version
                    res$structured[[parent[i]]][[j]][[sType[i]]][[currentField]] <-
                      currentElement[[sType[i]]][[currentField]];
                    ### Then copy to root
                    res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]] <-
                      currentElement[[sType[i]]][[currentField]]
                    warning("Inconsistent content found: ",
                            currentElement[[sType[i]]][[currentField]]);
                  }
                } else if (is.null(res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]])) {
                  res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]] <-
                    currentElement[[sType[i]]][[currentField]];
                } else if (is.null(currentElement[[sType[i]]][[currentField]])) {
                  currentElement[[sType[i]]][[currentField]] <-
                    res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]];
                  ### Also copy in `res` object, instead of the 'copy-on-modify' currentElement version
                  res$structured[[parent[i]]][[j]][[sType[i]]][[currentField]] <-
                    currentElement[[sType[i]]][[currentField]];
                } else {
                  error("This should be impossible and never occur.");
                }
              }

              ### To ensure consistency, we copy it to a list of its own, one
              ### level 'within' the overarching specification.
              res$structured[[parent[i]]][[j]][[sType[i]]] <-
                structure(list(res$structured[[parent[i]]][[j]][[sType[i]]]),
                          names=res$structured[[parent[i]]][[j]][[sType[i]]]$id);

            } else {
              ### Simply copy it over
              res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]] <-
                currentElement[[sType[i]]];
            }
          }
        }
      }
    }
  }

  # ### Check whether the justifications in the decisions are
  # ### all referenced, or whether some are also *specified*
  # ### there. If some are specified, copy them to the
  # ### 'justifications list'.
  # for (currentElement in res$structured$decisions) {
  #   if (length(currentElement$justification) > 1) {
  #     ### If length is 0 or 1, it can never be a complete specification
  #     if (length(names(currentElement$justification)) > 1) {
  #       ### Either it's a vector with id's or it's a specified
  #       ### justification
  #       if (!is.null(currentElement$justification$id)) {
  #         ### It's a specification of a justification
  #         if (currentElement$justification$id %in% names(res$structured$justifications)) {
  #           ### Check whether it already exists; if not, copy it over;
  #           ### if it does, just set fields that were unset in both
  #           for (currentField in union(names(res$structured$justifications[[currentElement$justification$id]]),
  #                                      names(currentElement$justification))) {
  #             if (!is.null(res$structured$justifications[[currentElement$justification$id]][[currentField]]) &&
  #                 !is.null(currentElement$justification[[currentField]])) {
  #               ### Both fields are set
  #               if (res$structured$justifications[[currentElement$justification$id]][[currentField]] !=
  #                   currentElement$justification[[currentField]]) {
  #                 ### Combine, with warning
  #                 currentElement$justification[[currentField]] <-
  #                   paste0("In ", currentElement$id, ": '",
  #                          currentElement$justification[[currentField]],
  #                          "'; in root specification: '",
  #                          res$structured$justifications[[currentElement$justification$id]][[currentField]],
  #                          "'.");
  #                 res$structured$justifications[[currentElement$justification$id]][[currentField]] <-
  #                   currentElement$justification[[currentField]]
  #                 warning("Inconsistent content found: ",
  #                         currentElement$justification[[currentField]]);
  #               }
  #             } else if (is.null(res$structured$justifications[[currentElement$justification$id]][[currentField]])) {
  #               res$structured$justifications[[currentElement$justification$id]][[currentField]] <-
  #                 currentElement$justification[[currentField]];
  #             } else if (is.null(currentElement$justification[[currentField]])) {
  #               currentElement$justification[[currentField]] <-
  #                 res$structured$justifications[[currentElement$justification$id]][[currentField]];
  #             } else {
  #               error("This should be impossible and never occur.");
  #             }
  #           }
  #         } else {
  #           ### Simply copy it over
  #           res$structured$justifications[[currentElement$justification$id]] <-
  #             currentElement$justification;
  #         }
  #       }
  #     }
  #   }
  # }
  #
  # ### Next, repeat this 'trick' for the justifications and assertions
  # for (currentElement in res$structured$justifications) {
  #   if (length(currentElement$assertion) > 1) {
  #     ### If length is 0 or 1, it can never be a complete specification
  #     if (length(names(currentElement$assertion)) > 1) {
  #       ### Either it's a vector with id's or it's a specified
  #       ### justification
  #       if (!is.null(currentElement$assertion$id)) {
  #         ### It's a specification of a justification
  #         if (currentElement$assertion$id %in% names(res$structured$assertions)) {
  #           ### Check whether it already exists; if not, copy it over;
  #           ### if it does, just set fields that were unset in both
  #           for (currentField in union(names(res$structured$assertions[[currentElement$assertion$id]]),
  #                                      names(currentElement$assertion))) {
  #             if (!is.null(res$structured$assertions[[currentElement$assertion$id]][[currentField]]) &&
  #                 !is.null(currentElement$assertion[[currentField]])) {
  #               ### Both fields are set
  #               if (res$structured$assertions[[currentElement$assertion$id]][[currentField]] !=
  #                   currentElement$assertion[[currentField]]) {
  #                 ### Combine, with warning
  #                 currentElement$assertion[[currentField]] <-
  #                   paste0("In ", currentElement$id, ": '",
  #                          currentElement$assertion[[currentField]],
  #                          "'; in root specification: '",
  #                          res$structured$assertions[[currentElement$assertion$id]][[currentField]],
  #                          "'.");
  #                 res$structured$assertions[[currentElement$assertion$id]][[currentField]] <-
  #                   currentElement$assertion[[currentField]]
  #                 warning("Inconsistent content found: ",
  #                         currentElement$assertion[[currentField]]);
  #               }
  #             } else if (is.null(res$structured$assertions[[currentElement$assertion$id]][[currentField]])) {
  #               res$structured$justifications[[currentElement$assertion$id]][[currentField]] <-
  #                 currentElement$assertion[[currentField]];
  #             } else if (is.null(currentElement$assertion[[currentField]])) {
  #               currentElement$assertion[[currentField]] <-
  #                 res$structured$assertions[[currentElement$assertion$id]][[currentField]];
  #             } else {
  #               error("This should be impossible and never occur.");
  #             }
  #           }
  #         } else {
  #           ### Simply copy it over
  #           res$structured$assertions[[currentElement$assertion$id]] <-
  #             currentElement$assertion;
  #         }
  #
  #
  #
  #
  #
  #         res$structured$assertions[[currentElement$assertion$id]] <-
  #           currentElement$assertion;
  #       }
  #     }
  #   }
  # }
  #
  # ### Finally, for the assertions and sources
  # for (currentElement in res$structured$assertions) {
  #   ### If length is 0 or 1, it can never be a complete specification
  #   if (length(currentElement$source) > 1) {
  #     ### Either it's a vector with id's or it's a specified
  #     ### justification
  #     if (length(names(currentElement$source)) > 1) {
  #       ### It's a specification of a justification
  #       if (!is.null(currentElement$source$id)) {
  #         res$structured$sources[[currentElement$source$id]] <-
  #           currentElement$source;
  #       }
  #     }
  #   }
  # }

  ### Now we processed all specifications, and we are certain that
  ### all specifications have been copied to the root elements in
  ### the structured object. We are also certain that all fields have
  ### been synchronized. Therefore, we can now use this as an
  ### exhaustive list.

  ### Walk through this list and collect missing information
  ### where other elements were referenced by ID, in the opposite
  ### order (sources are now already complete; now complete the
  ### assertions; then the justifications; then the decisions.

  res$supplemented <- res$structured;

  for (currentElement in names(res$supplemented$assertions)) {
    if (length(res$supplemented$assertions[[currentElement]]$source) > 0) {
      ### There are four options. Either this is a character
      ### vector named 'id' that references one or more sources;
      ### or it is one specified source, with the id stored in
      ### 'id' but with other fields set, as well; or it is
      ### a list of multiple specified sources.
      if ('id' %in% names(res$supplemented$assertions[[currentElement]]$source)) {
        ### Can't be a list of specified sources. Check whether it's one or more
        ### referenced sources; otherwise it's one specified source and we don't
        ### have to do anything.
        if (length(names(res$supplemented$assertions[[currentElement]]$source)) == 1) {
          for (i in res$supplemented$assertions[[currentElement]]$source$id) {
            if (i %in% names(res$supplemented$sources)) {
              res$supplemented$assertions[[currentElement]]$source[[i]] <-
                res$supplemented$sources[[i]];
            } else {
              warning("Assertion '", currentElement,
                      "' references source with id '", i,
                      "', but I cannot find it.");
              res$supplemented$assertions[[currentElement]]$source[[i]] <-
                list(id = i);
            }
          }
        }
        ### No else necessary; we don't need to do anything in this case;
        ### unless at some point, we want to supplement information from
        ### the root sources list
        ### -------------
        ### Note - this is superfluous now that we synchronize everything above.
      }
      ### Same here
    }
  }

  for (currentElement in names(res$supplemented$justifications)) {
    if (length(res$supplemented$justifications[[currentElement]]$assertion) > 0) {
      if ('id' %in% names(res$supplemented$justifications[[currentElement]]$assertion)) {
        if (length(names(res$supplemented$justifications[[currentElement]]$assertion)) == 1) {
          for (i in res$supplemented$justifications[[currentElement]]$assertion$id) {
            if (i %in% names(res$supplemented$assertions)) {
              res$supplemented$justifications[[currentElement]]$assertion[[i]] <-
                res$supplemented$assertion[[i]];
            } else {
              warning("Justification '", currentElement,
                      "' references assertion with id '", i,
                      "', but I cannot find it.");
              res$supplemented$justifications[[currentElement]]$assertion[[i]] <-
                list(id = i);
            }
          }
        }
      }
    }
  }

  for (currentElement in names(res$supplemented$decisions)) {
    if (length(res$supplemented$decisions[[currentElement]]$justification) > 0) {
      if ('id' %in% names(res$supplemented$decisions[[currentElement]]$justification)) {
        if (length(names(res$supplemented$decisions[[currentElement]]$justification)) == 1) {
          for (i in res$supplemented$decisions[[currentElement]]$justification$id) {
            if (i %in% names(res$supplemented$justifications)) {
              res$supplemented$decisions[[currentElement]]$justification[[i]] <-
                res$supplemented$justification[[i]];
            } else {
              warning("Decision '", currentElement,
                      "' references justification with id '", i,
                      "', but I cannot find it.");
              res$supplemented$decisions[[currentElement]]$justification[[i]] <-
                list(id = i);
            }
          }
        }
      }
    }
  }

  res$fullTree <-
    data.tree::FromListSimple(res$supplemented);

  class(res) <- 'justifications';
  return(res);
}

