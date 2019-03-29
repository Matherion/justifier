#' Parsing justifications
#'
#' While there is some flexibility in how justifications can be specified,
#' they are most easily processed further if they all follow the same
#' conventions. This function ensures this. The convention is as follows:
#' - all specifications are provided in four 'flat' lists, named after the
#'   types of elements they contain;
#' - all elements have  a unique identifier
#' - all references to other elements are indeed only references to the other
#'   elements' id's in these 'flat lists'
#'
#' @param x The `justifier` object that has just been loaded by `load_justifications`.
#'
#' @return The parsed `justifier` object
#' @export
#'
#' @examples
parse_justifications <- function(x) {

  res <- list(raw = x);

  ### Process all justifications and create four organised lists,
  ### where id's are used as names
  justNames <- tolower(names(x));
  res$structured <-
    list(sources = to_specList(x[which(justNames == 'source')],
                               types="sources",
                               type="source"),
         assertions = to_specList(x[which(justNames == 'assertion')],
                                  types="assertions",
                                  type="assertion"),
         justifications = to_specList(x[which(justNames == 'justification')],
                                      types="justifications",
                                      type="justification"),
         decisions = to_specList(x[which(justNames == 'decision')],
                                 types="decisions",
                                 type="decision"));

  ### Set names
  res$structured <- lapply(res$structured, function(elementList) {
    elementIds <-
      lapply(elementList, function(singleElement) {
        return(ifelse(is.null(singleElement$id),
                      "noID",
                      singleElement$id));
      });
    ### Set ids for elements without an id (shouldn't happen;
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

  ### Check all decisions for justifications that were specified there,
  ### instead of simply being references that were specified as
  ### 'root' elements
  for (i in seq_along(res$structured$decisions)) {
    if (!is.null(res$structured$decisions[[i]]$justification)) {
      ### Flatten this justification, in case it is a list with a redundant level
      res$structured$decisions[[i]]$justification <-
        to_specList(res$structured$decisions[[i]]$justification,
                    types="justifications", type="justification");
      ### Process the elements one by one
      for (j in seq_along(res$structured$decisions[[i]]$justification)) {
        ### Check whether it is a specification (or one or more references)
        if (is_spec(res$structured$decisions[[i]]$justification[[j]])) {
          ### Check whether it already exists in the root
          if ((length(names(res$structured$justifications)) > 0) &&
              res$structured$decisions[[i]]$justification[[j]]$id %in%
                names(res$structured$justifications)) {
            ### If so, merge them and store the result in the root
            res$structured$justifications[[res$structured$decisions[[i]]$justification[[j]]$id]] <-
              merge_specs(res$structured$justifications[[res$structured$decisions[[i]]$justification[[j]]$id]],
                          res$structured$decisions[[i]]$justification[[j]]);
          } else {
            ### If not, copy this to the root
            if (is.null(res$structured$decisions[[i]]$justification[[j]]$id)) {
              stop("Error: no id for:\n\n",
                   paste0(capture.output(print(res$structured$decisions[[i]]$justification[[j]])),
                          collapse="\n"));
            }
            res$structured$justifications[[res$structured$decisions[[i]]$justification[[j]]$id]] <-
              res$structured$decisions[[i]]$justification[[j]];
          }
        }
      }
      ### Once they are all copied and/or merged, replace them with references
      if (is.list(res$structured$decisions[[i]]$justification)) {
        if (!is.null(names(res$structured$decisions[[i]]$justification)) &&
            (length(names(res$structured$decisions[[i]]$justification)) == 1) &&
            (names(res$structured$decisions[[i]]$justification) == "id")) {
          res$structured$decisions[[i]]$justification <-
            structure(res$structured$decisions[[i]]$justification$id,
                      class="justifierRef");
        } else {
          res$structured$decisions[[i]]$justification <-
            structure(unname(purrr::map_chr(res$structured$decisions[[i]]$justification, "id")),
                      class="justifierRef");
        }
      }
    }
  }

  ### Repeat the exact same procedure for justifications and assertions.
  ### Tihs could be done more efficiently, but becomes much harder to read and
  ### maintain. So, I'll defer to Hadley Wickham: He who gives up code safety
  ### for code speed deserves neither :-)
  ### https://twitter.com/hadleywickham/status/504368538874703872
  for (i in seq_along(res$structured$justifications)) {
    if (!is.null(res$structured$justifications[[i]]$assertion)) {
      ### Flatten this justification, in case it is a list with a redundant level
      res$structured$justifications[[i]]$assertion <-
        to_specList(res$structured$justifications[[i]]$assertion,
                    types="assertions", type="assertion");
      ### Process the elements one by one
      for (j in seq_along(res$structured$justifications[[i]]$assertion)) {
        ### Check whether it is a specification (or one or more references)
        if (is_spec(res$structured$justifications[[i]]$assertion[[j]])) {
          ### Check whether it already exists in the root
          if ((length(names(res$structured$assertions)) > 0) &&
                res$structured$justifications[[i]]$assertion[[j]]$id %in%
              names(res$structured$assertions)) {
            ### If so, merge them and store the result in the root
            res$structured$assertions[[res$structured$justifications[[i]]$assertion[[j]]$id]] <-
              merge_specs(res$structured$assertions[[res$structured$justifications[[i]]$assertion[[j]]$id]],
                          res$structured$justifications[[i]]$assertion[[j]]);
          } else {
            ### If not, copy this to the root
            res$structured$assertions[[res$structured$justifications[[i]]$assertion[[j]]$id]] <-
              res$structured$justifications[[i]]$assertion[[j]];
          }
        }
      }
      ### Once they are all copied and/or merged, replace them with references
      if (is.list(res$structured$justifications[[i]]$assertion)) {
        if (!is.null(names(res$structured$justifications[[i]]$assertion)) &&
            (length(names(res$structured$justifications[[i]]$assertion)) == 1) &&
            (names(res$structured$justifications[[i]]$assertion) == "id")) {
          res$structured$justifications[[i]]$assertion <-
            structure(res$structured$justifications[[i]]$assertion$id,
                      class="justifierRef");
        } else {
          res$structured$justifications[[i]]$assertion <-
            structure(unname(purrr::map_chr(res$structured$justifications[[i]]$assertion, "id")),
                      class="justifierRef");
        }
      }
    }
    ###
    ### Justifications can also contain *other* justifications. They're the only
    ### 'recursive type' in that sense.
    ###
    if (!is.null(res$structured$justifications[[i]]$justification)) {
      ### Flatten this justification, in case it is a list with a redundant level
      res$structured$justifications[[i]]$justification <-
        to_specList(res$structured$justifications[[i]]$justification,
                    types="justifications", type="justification");
      ### Process the elements one by one
      for (j in seq_along(res$structured$justifications[[i]]$justification)) {
        ### Check whether it is a specification (or one or more references)
        if (is_spec(res$structured$justifications[[i]]$justification[[j]])) {
          ### Check whether it already exists in the root
          if ((length(names(res$structured$justifications)) > 0) &&
                res$structured$justifications[[i]]$justification[[j]]$id %in%
              names(res$structured$justifications)) {
            ### If so, merge them and store the result in the root
            res$structured$justifications[[res$structured$justifications[[i]]$justification[[j]]$id]] <-
              merge_specs(res$structured$justifications[[res$structured$justifications[[i]]$justification[[j]]$id]],
                          res$structured$justifications[[i]]$justification[[j]]);
          } else {
            ### If not, copy this to the root
            res$structured$justifications[[res$structured$justifications[[i]]$justification[[j]]$id]] <-
              res$structured$justifications[[i]]$justification[[j]];
          }
        }
      }
      ### Once they are all copied and/or merged, replace them with references
      if (is.list(res$structured$justifications[[i]]$justification)) {
        if (!is.null(names(res$structured$justifications[[i]]$justification)) &&
            (length(names(res$structured$justifications[[i]]$justification)) == 1) &&
            (names(res$structured$justifications[[i]]$justification) == "id")) {
          res$structured$justifications[[i]]$justification <-
            structure(res$structured$justifications[[i]]$justification$id,
                      class="justifierRef");
        } else {
          res$structured$justifications[[i]]$justification <-
            structure(unname(purrr::map_chr(res$structured$justifications[[i]]$justification, "id")),
                      class="justifierRef");
        }
      }
    }

  }

  ### Then again for the assertions and sources.
  for (i in seq_along(res$structured$assertions)) {
    if (!is.null(res$structured$assertions[[i]]$source)) {
      ### Flatten this justification, in case it is a list with a redundant level
      res$structured$assertions[[i]]$source <-
        to_specList(res$structured$assertions[[i]]$source,
                    types="sources", type="source");
      ### Process the elements one by one
      for (j in seq_along(res$structured$assertions[[i]]$source)) {
        ### Check whether it is a specification (or one or more references)
        if (is_spec(res$structured$assertions[[i]]$source[[j]])) {
          ### Check whether it already exists in the root
          if ((length(names(res$structured$sources)) > 0) &&
              res$structured$assertions[[i]]$source[[j]]$id %in%
                names(res$structured$sources)) {
            ### If so, merge them and store the result in the root
            res$structured$sources[[res$structured$assertions[[i]]$source[[j]]$id]] <-
              merge_specs(res$structured$sources[[res$structured$assertions[[i]]$source[[j]]$id]],
                          res$structured$assertions[[i]]$source[[j]]);
          } else {
            ### If not, copy this to the root
            res$structured$sources[[res$structured$assertions[[i]]$source[[j]]$id]] <-
              res$structured$assertions[[i]]$source[[j]];
          }
        }
      }
      ### Once they are all copied and/or merged, replace them with references
      if (is.list(res$structured$assertions[[i]]$source)) {
        if (!is.null(names(res$structured$assertions[[i]]$source)) &&
            (length(names(res$structured$assertions[[i]]$source)) == 1) &&
            (names(res$structured$assertions[[i]]$source) == "id")) {
          res$structured$assertions[[i]]$source <-
            structure(res$structured$assertions[[i]]$source$id,
                      class="justifierRef");
        } else {
          res$structured$assertions[[i]]$source <-
            structure(unname(purrr::map_chr(res$structured$assertions[[i]]$source, "id")),
                      class="justifierRef");
        }
      }
    }
  }

  res$supplemented <- res$structured;

  ### Now all elements have been copied to the root. Now, we replace
  ### all references with these completed (merged) elements

  ### First for the sources in the assertions
  for (i in seq_along(res$supplemented$assertions)) {
    if (!is.null(res$supplemented$assertions[[i]]$source)) {
      sourceIds <-
        res$supplemented$assertions[[i]]$source;
      res$supplemented$assertions[[i]]$source <-
        lapply(sourceIds,
               function(j) {
                 return(res$supplemented$sources[[j]]);
               });
      names(res$supplemented$assertions[[i]]$source) <-
        sourceIds;
    }
  }

  ### Then for the assertions in the justifications
  for (i in seq_along(res$supplemented$justifications)) {
    if (!is.null(res$supplemented$justifications[[i]]$assertion)) {
      assertionIds <-
        res$supplemented$justifications[[i]]$assertion;
      res$supplemented$justifications[[i]]$assertion <-
        lapply(assertionIds,
               function(j) {
                 return(res$supplemented$assertions[[j]]);
               });
      names(res$supplemented$justifications[[i]]$assertion) <-
        assertionIds;
    }
  }

  ### Then for the justifications in the justifications
  for (i in seq_along(res$supplemented$justifications)) {
    if (!is.null(res$supplemented$justifications[[i]]$justification)) {
      justificationIds <-
        res$supplemented$justifications[[i]]$justification;
      res$supplemented$justifications[[i]]$justification <-
        lapply(justificationIds,
               function(j) {
                 return(res$supplemented$justifications[[j]]);
               });
      names(res$supplemented$justifications[[i]]$justification) <-
        justificationIds;
    }
  }

  ### Then for the justifications in the decisions
  for (i in seq_along(res$supplemented$decisions)) {
    if (!is.null(res$supplemented$decisions[[i]]$justification)) {
      justificationIds <-
        res$supplemented$decisions[[i]]$justification;
      res$supplemented$decisions[[i]]$justification <-
        lapply(justificationIds,
               function(j) {
                 return(res$supplemented$justifications[[j]]);
               });
      names(res$supplemented$decisions[[i]]$justification) <-
        justificationIds;
    }
  }

  ### Now create one decision tree for each decision
  res$decisionTrees <-
    lapply(res$supplemented$decisions,
           function(d) {
             return(lapply(d$justification,
                           function(j) {
                             return(lapply(j$assertion,
                                             function(a) {
                                               return(a$source);
                                             }));
                           }));
           });

  res$decisionTrees <-
    lapply(names(res$decisionTrees),
           function(decisionId) {
             data.tree::FromListSimple(simpleList = res$decisionTrees[[decisionId]],
                                       nodeName=decisionId);
           });
  names(res$decisionTrees) <-
    names(res$supplemented$decisions);

  class(res) <- 'justifications';
  return(res);

}



  # ### In the structured list, if an element contains multiple
  # ### specifications, give them the name of their id and remove
  # ### superfluous lists
  # res$structured <-
  #   lapply(res$structured,
  #          function(x) {
  #            print(length(x));
  #            print(names(x));
  #            if ((length(x) > 0) && (is.null(names(x)))) {
  #              return(structured(unlist(x, recursive=FALSE),
  #                                list(names = purrr::map_chr(x, "id"))));
  #            }
  #            else (return(x));
  #          });
  ### The bit above is now done by a call to 'flatten_list'

  ### Vectors with singular, plural, and parents of elements. Note that we process
  ### these from smaller to bigger - assertions can only reference sources,
  ### justifications can reference both of those (i.e. sources only through assertions),
  ### and decisions can reference anything (i.e. through justifications).
  ###
  # allTypes <- c("decision", "justification", "assertion", "source");
  # sType <- c("justification", "assertion", "source");         ### `rev` because that makes
  # pType <- c("justifications", "assertions", "sources");      ### more sense now that we
  # parent <- c("decisions", "justifications", "assertions");   ### walk the lists twice
  # child <- c("assertion", "source", NA);
  ### Check whether elements only contain references to lower-level
  ### elements, or whether they also contain specifications of
  ### lower-level elements. If they also contain specification,
  ### copy those to the 'root' for that elemnt type (if they don't
  ### exit yet) or synchronize the fields (if they do exist already).
  ###
  ### The problem here is that lower level elements (e.g. from the
  ### perspective of assertions, sources) can be specified both in
  ### the root and in higher-level elements. There are two ways to
  ### deal with this. First, we can walk the list of objects twice (after
  ### the first pass, the lists of elements in their 'root lists' is
  ### complete, so then we can repeat the process and synchronzie
  ### all elements again to make them all complete). Second, we can
  ### only walk the list once, but also check higher-level elements
  ### and synchronize 'both ways'.
  ###
  ### I'm going to choose the easy/slow way out: simply repeat the
  ### process. This keeps the code a bit cleaner and more understandable
  ### (it's barely understandably as it is), and this function isn't
  ### doing to be used to much that any speed benefits will pay off,
  ### I think.
  ###
  ### So, after the first three cycles (1:3, so processing all
  ### justifications in each decision; then processing all
  ###
  ###
  ###
  ###
  ### So, what to do with a decision that contains a justification
  ### that is also present in the jsutifications root?
  ###
  ### Or a justification that contains an assertion that is also
  ### present in the assertions root?
  ###
  ###
  ###
  ###

  # for (i in c(1:3, 2:1)) {
  #   ### So we go 1-3 and back. We can afford to do 3 only once -
  #   ### once we get there the first time, all lower-level elements
  #   ### have already been copied/synced to the root.
  #   for (j in seq_along(res$structured[[parent[i]]])) {
  #     ### This for loop simply loops through all elements
  #     ### Make copy for convenience and to still understand
  #     ### the code
  #     currentElement <-
  #       res$structured[[parent[i]]][[j]];
  #     if (length(currentElement[[sType[i]]]) > 1) {
  #       ### If length is 0 or 1, it can never be a complete specification
  #       if (length(names(currentElement[[sType[i]]])) > 1) {
  #         ### Either it's a vector with id's or it's a specified
  #         ### justification
  #         if (!is.null(currentElement[[sType[i]]]$id)) {
  #           ### If if has no ID, it cannot be a complete specification, so
  #           ### this has to be a specification of a justification
  #           if (currentElement[[sType[i]]]$id %in% names(res$structured[[pType[i]]])) {
  #             ### Check whether it already exists as root element. If it does,
  #             ### check all fields, and then synchronize them; if not, copy it over.
  #             for (currentField in union(names(res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]]),
  #                                        names(currentElement[[sType[i]]]))) {
  #               if (!is.null(res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]]) &&
  #                   !is.null(currentElement[[sType[i]]][[currentField]])) {
  #                 ### Both fields are set
  #                 if (!identical(res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]],
  #                     currentElement[[sType[i]]][[currentField]])) {
  #                   if (sType[i] %in% allTypes) {
  #                     ### In this case we can skip it; this field represents an element
  #                     ### that will be copied anyway
  #                   } else {
  #
  #                     ### In this case we have a field that already has a value in the root element;
  #                     ###
  #
  #                     ### Look at the types of the objects in the fields. If atomic vectors of length 1,
  #                     ### this approach works. Otherwise, combine in lists.
  #                     ###
  #                     ### Or. Maybe always combine them in lists.
  #
  #                     thisElementName <-
  #                       paste0("child (type=",
  #                              sType[i],
  #                              ", id=",
  #                              currentElement[[sType[i]]]$id,
  #                              ")");
  #
  #                     if (class(res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]]) ==
  #                         "multipleJustifierFields") {
  #                       if (!(thisElementName %in%
  #                             names(res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]]))) {
  #                         ### If the name of this element already exists, we've already synchronized this one.
  #                         res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]] <-
  #                           c(res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]],
  #                             structure(list(currentElement[[sType[i]]][[currentField]]),
  #                                       names=thisElementName));
  #                       }
  #                     } else {
  #                       res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]] <-
  #                         structure(list(res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]],
  #                                        currentElement[[sType[i]]][[currentField]]),
  #                                   names = c(paste0("root (type=",
  #                                                    pType[i],
  #                                                    ", id=",
  #                                                    currentElement[[sType[i]]]$id,
  #                                                    ")"),
  #                                             paste0("child (type=",
  #                                                    sType[i],
  #                                                    ", id=",
  #                                                    currentElement[[sType[i]]]$id,
  #                                                    ")")));
  #                     }
  #
  #                     # ### Combine them, with a warning.
  #                     # ### First, store in 'currentElement'.
  #                     # currentElement[[sType[i]]][[currentField]] <-
  #                     #   paste0("In ", currentElement$id, ": '",
  #                     #          currentElement[[sType[i]]][[currentField]],
  #                     #          "'; in root specification: '",
  #                     #          res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]],
  #                     #          "'.");
  #                     #
  #                     # ### Also copy in `res` object, instead of the 'copy-on-modify' currentElement version
  #                     # res$structured[[parent[i]]][[j]][[sType[i]]][[currentField]] <-
  #                     #   currentElement[[sType[i]]][[currentField]];
  #                     # ### Then copy to root
  #                     # res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]] <-
  #                     #   currentElement[[sType[i]]][[currentField]]
  #                     warning("Inconsistent content found: ",
  #                             currentElement[[sType[i]]][[currentField]]);
  #                   }
  #                 }
  #               } else if (is.null(res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]])) {
  #                 res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]] <-
  #                   currentElement[[sType[i]]][[currentField]];
  #               } else if (is.null(currentElement[[sType[i]]][[currentField]])) {
  #                 currentElement[[sType[i]]][[currentField]] <-
  #                   res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]][[currentField]];
  #                 ### Also copy in `res` object, instead of the 'copy-on-modify' currentElement version
  #                 res$structured[[parent[i]]][[j]][[sType[i]]][[currentField]] <-
  #                   currentElement[[sType[i]]][[currentField]];
  #               } else {
  #                 error("This should be impossible and never occur.");
  #               }
  #             }
  #
  #             ### To ensure consistency, we copy it to a list of its own, one
  #             ### level 'within' the overarching specification.
  #             res$structured[[parent[i]]][[j]][[sType[i]]] <-
  #               structure(list(res$structured[[parent[i]]][[j]][[sType[i]]]),
  #                         names=res$structured[[parent[i]]][[j]][[sType[i]]]$id);
  #
  #           } else {
  #             ### Simply copy it over
  #             res$structured[[pType[i]]][[currentElement[[sType[i]]]$id]] <-
  #               currentElement[[sType[i]]];
  #           }
  #         }
  #       }
  #     }
  #   }
  # }




  ### Hulpfuncties -
  ###
  ###   - is+_refernce
  ###   - merge_specs





  ### Check whether the justifications in the decisions are
  ### all referenced, or whether some are also *specified*
  ### there. If some are specified, copy them to the
  ### 'justifications list'.
  # for (i in seq_along(res$structured$decisions)) {
  #   if (length(res$structured$decisions[i]$justification) > 1) {
  #     ### If length is 0 or 1, it can never be a complete specification
  #     if (length(names(res$structured$decisions[i]$justification)) > 1) {
  #       ### Either it's a vector with id's or it's a specified
  #       ### justification
  #       if (!is.null(res$structured$decisions[i]$justification$id)) {
  #         ### It's a specification of a justification
  #         if (res$structured$decisions[i]$justification$id %in% names(res$structured$justifications)) {
  #           ### Check whether it already exists; if not, copy it over;
  #           ### if it does, just set fields that were unset in both
  #           for (currentField in union(names(res$structured$justifications[[res$structured$decisions[i]$justification$id]]),
  #                                      names(res$structured$decisions[i]$justification))) {
  #             if (!is.null(res$structured$justifications[[res$structured$decisions[i]$justification$id]][[currentField]]) &&
  #                 !is.null(res$structured$decisions[i]$justification[[currentField]])) {
  #               ### Both fields are set
  #               if (res$structured$justifications[[res$structured$decisions[i]$justification$id]][[currentField]] !=
  #                   res$structured$decisions[i]$justification[[currentField]]) {
  #                 ### Combine, with warning
  #                 res$structured$decisions[i]$justification[[currentField]] <-
  #                   paste0("In ", res$structured$decisions[i]$id, ": '",
  #                          res$structured$decisions[i]$justification[[currentField]],
  #                          "'; in root specification: '",
  #                          res$structured$justifications[[res$structured$decisions[i]$justification$id]][[currentField]],
  #                          "'.");
  #                 res$structured$justifications[[res$structured$decisions[i]$justification$id]][[currentField]] <-
  #                   res$structured$decisions[i]$justification[[currentField]]
  #                 warning("Inconsistent content found: ",
  #                         res$structured$decisions[i]$justification[[currentField]]);
  #               }
  #             } else if (is.null(res$structured$justifications[[res$structured$decisions[i]$justification$id]][[currentField]])) {
  #               res$structured$justifications[[res$structured$decisions[i]$justification$id]][[currentField]] <-
  #                 res$structured$decisions[i]$justification[[currentField]];
  #             } else if (is.null(res$structured$decisions[i]$justification[[currentField]])) {
  #               res$structured$decisions[i]$justification[[currentField]] <-
  #                 res$structured$justifications[[res$structured$decisions[i]$justification$id]][[currentField]];
  #             } else {
  #               error("This should be impossible and never occur.");
  #             }
  #           }
  #         } else {
  #           ### Simply copy it over
  #           res$structured$justifications[[res$structured$decisions[i]$justification$id]] <-
  #             currentElement$justification;
  #         }
  #       }
  #     }
  #   }
  # }
  #
  # ### Next, repeat this 'trick' for the justifications and assertions
  # for (i in seq_along(res$structured$justifications)) {
  #   if (length(res$structured$justifications[i]$assertion) > 1) {
  #     ### If length is 0 or 1, it can never be a complete specification
  #     if (length(names(res$structured$justifications[i]$assertion)) > 1) {
  #       ### Either it's a vector with id's or it's a specified
  #       ### assertion
  #       if (!is.null(res$structured$justifications[i]$assertion$id)) {
  #         ### It's a specification of an assertion
  #         if (res$structured$justifications[i]$assertion$id %in% names(res$structured$assertions)) {
  #           ### Check whether it already exists; if not, copy it over;
  #           ### if it does, just set fields that were unset in both
  #           for (currentField in union(names(res$structured$assertions[[res$structured$justifications[i]$assertion$id]]),
  #                                      names(res$structured$justifications[i]$assertion))) {
  #             if (!is.null(res$structured$assertions[[res$structured$justifications[i]$assertion$id]][[currentField]]) &&
  #                 !is.null(res$structured$justifications[i]$assertion[[currentField]])) {
  #               ### Both fields are set
  #               if (res$structured$assertions[[res$structured$justifications[i]$assertion$id]][[currentField]] !=
  #                   res$structured$justifications[i]$assertion[[currentField]]) {
  #                 ### Combine, with warning
  #                 res$structured$justifications[i]$assertion[[currentField]] <-
  #                   paste0("In ", res$structured$justifications[i]$id, ": '",
  #                          res$structured$justifications[i]$assertion[[currentField]],
  #                          "'; in root specification: '",
  #                          res$structured$assertions[[res$structured$justifications[i]$assertion$id]][[currentField]],
  #                          "'.");
  #                 res$structured$assertions[[res$structured$justifications[i]$assertion$id]][[currentField]] <-
  #                   res$structured$justifications[i]$assertion[[currentField]]
  #                 warning("Inconsistent content found: ",
  #                         res$structured$justifications[i]$assertion[[currentField]]);
  #               }
  #             } else if (is.null(res$structured$assertions[[res$structured$justifications[i]$assertion$id]][[currentField]])) {
  #               res$structured$justifications[[res$structured$justifications[i]$assertion$id]][[currentField]] <-
  #                 res$structured$justifications[i]$assertion[[currentField]];
  #             } else if (is.null(res$structured$justifications[i]$assertion[[currentField]])) {
  #               res$structured$justifications[i]$assertion[[currentField]] <-
  #                 res$structured$assertions[[res$structured$justifications[i]$assertion$id]][[currentField]];
  #             } else {
  #               error("This should be impossible and never occur.");
  #             }
  #           }
  #         } else {
  #           ### Simply copy it over
  #           res$structured$assertions[[currentElement$assertion$id]] <-
  #             currentElement$assertion;
  #         }
  #       }
  #     }
  #   }
  # }
  #
  # ### Finally, for the assertions and sources
  # for (i in seq_along(res$structured$assertions)) {
  #   ### If length is 0 or 1, it can never be a complete specification
  #   if (length(res$structured$assertions[i]$source) > 1) {
  #     ### Either it's a vector with id's or it's a specified
  #     ### justification
  #     if (length(names(res$structured$assertions[i]$source)) > 1) {
  #       ### It's a specification of a justification
  #       if (!is.null(res$structured$assertions[i]$source$id)) {
  #         res$structured$sources[[res$structured$assertions[i]$source$id]] <-
  #           res$structured$assertions[i]$source;
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

  # for (currentElement in names(res$supplemented$assertions)) {
  #   if (length(res$supplemented$assertions[[currentElement]]$source) > 0) {
  #     ### There are four options. Either this is a character
  #     ### vector named 'id' that references one or more sources;
  #     ### or it is one specified source, with the id stored in
  #     ### 'id' but with other fields set, as well; or it is
  #     ### a list of multiple specified sources.
  #     if ('id' %in% names(res$supplemented$assertions[[currentElement]]$source)) {
  #       ### Can't be a list of specified sources. Check whether it's one or more
  #       ### referenced sources; otherwise it's one specified source and we don't
  #       ### have to do anything.
  #       if (length(names(res$supplemented$assertions[[currentElement]]$source)) == 1) {
  #         for (i in res$supplemented$assertions[[currentElement]]$source$id) {
  #           if (i %in% names(res$supplemented$sources)) {
  #             res$supplemented$assertions[[currentElement]]$source[[i]] <-
  #               res$supplemented$sources[[i]];
  #           } else {
  #             warning("Assertion '", currentElement,
  #                     "' references source with id '", i,
  #                     "', but I cannot find it.");
  #             res$supplemented$assertions[[currentElement]]$source[[i]] <-
  #               list(id = i);
  #           }
  #         }
  #       }
  #       ### No else necessary; we don't need to do anything in this case;
  #       ### unless at some point, we want to supplement information from
  #       ### the root sources list
  #       ### -------------
  #       ### Note - this is superfluous now that we synchronize everything above.
  #     }
  #     ### Same here
  #   }
  # }
#
#   for (currentElement in names(res$supplemented$justifications)) {
#     if (length(res$supplemented$justifications[[currentElement]]$assertion) > 0) {
#       if ('id' %in% names(res$supplemented$justifications[[currentElement]]$assertion)) {
#         if (length(names(res$supplemented$justifications[[currentElement]]$assertion)) == 1) {
#           for (i in res$supplemented$justifications[[currentElement]]$assertion$id) {
#             if (i %in% names(res$supplemented$assertions)) {
#               res$supplemented$justifications[[currentElement]]$assertion[[i]] <-
#                 res$supplemented$assertion[[i]];
#             } else {
#               warning("Justification '", currentElement,
#                       "' references assertion with id '", i,
#                       "', but I cannot find it.");
#               res$supplemented$justifications[[currentElement]]$assertion[[i]] <-
#                 list(id = i);
#             }
#           }
#         }
#       }
#     }
#   }
#
#   for (currentElement in names(res$supplemented$decisions)) {
#     if (length(res$supplemented$decisions[[currentElement]]$justification) > 0) {
#       if ('id' %in% names(res$supplemented$decisions[[currentElement]]$justification)) {
#         if (length(names(res$supplemented$decisions[[currentElement]]$justification)) == 1) {
#           for (i in res$supplemented$decisions[[currentElement]]$justification$id) {
#             if (i %in% names(res$supplemented$justifications)) {
#               res$supplemented$decisions[[currentElement]]$justification[[i]] <-
#                 res$supplemented$justification[[i]];
#             } else {
#               warning("Decision '", currentElement,
#                       "' references justification with id '", i,
#                       "', but I cannot find it.");
#               res$supplemented$decisions[[currentElement]]$justification[[i]] <-
#                 list(id = i);
#             }
#           }
#         }
#       }
#     }
#   }

