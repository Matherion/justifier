selective_flattening <- function(x,
                                 type,
                                 recursionLevel = 0,
                                 silent = justifier::opts$get("silent")) {

  msg(spc(recursionLevel),
      "Selectively flattening an object called '",
      deparse(substitute(x)), "' only selecting ",
      type, " specifications.\n",
      silent = silent);

  justifierClass <-
    paste0(
      "justifier",
      tools::toTitleCase(type)
    );

  justifierPlural <-
    paste0(tolower(type), "s");

  if ((inherits(x, justifierClass)) && (inherits(x, "singleJustifierElement"))) {

    msg(spc(recursionLevel),
        "Single justifier element present with identifier '",
        x$id, "'.\n",
        silent = silent);

    res <-
      list(
        sources = list(),
        assertions = list(),
        justifications = list(),
        decisions = list(),
        justifier = list()
      );

    res[[justifierPlural]] <- list(x);

    names(res[[justifierPlural]]) <-
      get_ids_from_structured_justifierElements(
        res[[justifierPlural]]
      );

    class(res) <- c("justifier", "justifierStructured", "list");

    return(res);

  } else if ((inherits(x, justifierClass)) && (inherits(x, "multipleJustifierElements"))) {

    msg(spc(recursionLevel),
        length(x), " elements present, calling myself recursively to structure them.\n",
        silent = silent);

    res <-
      do.call(
        c,
        lapply(
          x,
          selective_flattening,
          type = type,
          recursionLevel = recursionLevel + 1,
          silent = silent
        )
      );

    class(res) <- c("justifier", "justifierStructured", "list");

    return(res);

  } else {

    stop("You passed an object that isn't a singleJustifierElement of ",
         "class ", justifierClass, " or a list of ",
         "multipleJustifierElements. Instead, it has class(es) ",
         vecTxtQ(class(x)), "...");

  }

}
