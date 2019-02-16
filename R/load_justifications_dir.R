#' @rdname load_justifications
#' @export
load_justifications_dir <- function(path,
                                    recursive = TRUE,
                                    extension = "jmd",
                                    regex,
                                    justificationContainer = "justifier",
                                    delimiterRegEx = "^---$",
                                    ignoreOddDelimiters = FALSE,
                                    encoding="UTF-8",
                                    silent=TRUE) {

  if (!dir.exists(path)) {
    stop("Directory '",
         path,
         "' does not exist!");
  }

  if (missing(regex)) {
    regex <- paste0("^(.*)\\.", extension, "$");
  }

  ###--------------------------------------------------------------------------
  ### Load the YAML fragments containing the DCT specifications
  ###--------------------------------------------------------------------------

  justificationList <-
    yum::load_yaml_dir(path=path,
                       recursive=recursive,
                       fileRegexes = regex,
                       select=justificationContainer,
                       delimiterRegEx = delimiterRegEx,
                       ignoreOddDelimiters = ignoreOddDelimiters,
                       encoding = encoding,
                       silent=silent);

  ### Remove 'file' level
  justifications <-
    unlist(justificationList,
           recursive=FALSE);

  ###--------------------------------------------------------------------------
  ### Parse DCT specifications and return result
  ###--------------------------------------------------------------------------

  res <-
    parse_justifications(justifications);

  return(res);

}
