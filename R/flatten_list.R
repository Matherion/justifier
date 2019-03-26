flatten_list <- function(x,
                         type) {
  if (!is.list(x) || (length(x) == 0) || !is.null(names(x))) {
    return(x);
  } else {
    ### Check whether all list elements have an identifier set
    elementIds <-
      unlist(lapply(x,
                    function(subX) ifelse(is.null(subX$id), "", subX$id)));
    names(x) <- elementIds;
    return(x);
  #   print("=================");
  #   print(elementIds);
  #   print(x);
  #   print("-----------------");
  #   if (all(nchar(elementIds) > 0)) {
  #     res <- x;
  #     # elementIds <-
  #     #   purrr::map_chr(unlist(x, recursive=FALSE),
  #     #                  "id");
  #     # print(elementIds);
  #     # if (length(x) > 1) {
  #     #   res <- structure(x,
  #     #                    names = elementIds);
  #     # } else {
  #     #   res <- structure(unlist(x, recursive=FALSE),
  #     #                    names = elementIds);
  #     # }
  #     # res <- unlist(x, recursive=FALSE);
  #     # if ((length(x) == 1) && (length(res) != 1)) {
  #     #   res <- list(res);
  #     # }
  #     # print("=================");
  #     # print(x);
  #     # print("-----------------");
  #     # print(res);
  #     # print(elementIds);
  #     # res <- structure(res,
  #     #                  names = elementIds);
  #     return(res);
  #   } else {
  #     warning("While trying to clean up the provided justifier specifications, ",
  #             "I encountered a list where the mandatory identifier ('id') seems ",
  #             "to have been omitted from one or more specifications. I'm trying to ",
  #             "flatten this object:\n\n",
  #             paste0(capture.output(str(x)), collapse="\n"), "\n");
  #     return(x);
  #   }
  }
}


