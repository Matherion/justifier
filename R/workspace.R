#' Show your workspace contents
#'
#' @param silent Whether to be chatty or silent.
#'
#' @return The workspace contents.
#' @export
#'
#' @examples clean_workspace(force = TRUE, silent=FALSE);
#' log_decision("First we start using `justifier`.",
#'              silent=FALSE);
#' log_decision(paste0("Then we start documenting our ",
#'                     "decisions and justifications."),
#'              silent=FALSE);
#' log_decision("Then we start learning from ourselves.",
#'              silent=FALSE);
#' workspace();
workspace <- function(silent=justifier::opts$get('silent')) {

  res <-
    getOption(justifier::opts$get('workspace'),
              NULL);

  if (is.null(res)) {
    cat("Your workspace is empty!");
    return(invisible(NULL));
  }

  return(res);

}
