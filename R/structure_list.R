structure_list <- function(x) {

  res <- list();

  ### Process all specifications and create five organised lists,
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
                                 type="decision"),
         justifier = to_specList(x[which(justNames == 'justifier')],
                                 types="justifiers",
                                 type="justifier"));

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

  return(res$structured);

}
