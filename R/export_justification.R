
# a<-justifier::load_justifications(strsplit(export_justification(asrt("assertion", source=c(srce('bla'), srce('bla2'))), file=NULL), "\n")[[1]])

export_justification <- function(x,
                                 file,
                                 encoding = "UTF-8",
                                 append = TRUE,
                                 preventOverwriting = TRUE) {

  if ("justifierSource" %in% class(x)) {
    justifierType <- "source";
  } else if ("justifierAssertion" %in% class(x)) {
    justifierType <- "assertion";
  } else if ("justifierJustification" %in% class(x)) {
    justifierType <- "justification";
  } else if ("justifierDecision" %in% class(x)) {
    justifierType <- "decision";
  } else {
    stop("The object you passed with argument `x` does not have a valid class!");
  }

  x <- yaml::as.yaml(x);
  x <- gsub("\n", "\n  ", x);
  x <- paste0("---\n",
              justifierType,
              ":\n  ",
              x,
              "\n---\n");

  if (is.null(file)) {
    return(x);
  } else {
    if (!dir.exists(dirname(file))) {
      stop("The directory specified where the output file '",
           basename(file), "' is supposed to be written ('",
           dirname(file),
           "') does not exist.");
    }
    if (file.exists(file)) {
      if (append) {
        con <- file(description=file,
                    open="a",
                    encoding=encoding);
        writeLines(text=c("\n\n",
                          x),
                   con=con);
        close(con);
      } else if (preventOverwriting) {
        if (!silent) {
          message("File '",
                  file, "' exists, and `preventOverwriting` was `TRUE`, so I did not ",
                  "write the justification to disk.");
        }
      }
    } else {
      con <- file(description=file,
                  open="w",
                  encoding=encoding);
      writeLines(text=x,
                 con=con);
      close(con);
    }
    if (!silent) {
      message("I just wrote a justification to file '",
              file,
              "'.");
    }
    invisible(res);
  }


}
