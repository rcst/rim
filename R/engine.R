utils::globalVariables(c("engine", "mx", "plots"))
#' \code{knitr} maxima engine
#'
#' Functions to process Maxima code chunks by \code{knitr}.
#'
#' Upon attachment, i.e. \code{library(rim)} function \code{maxima.engine} is registered as a \code{knitr} engine. Thus, \code{maxima.engine()} is called by \code{knit()} to evaluate Maxima code chunks. When called upon the first code chunk of a document it starts Maxima in a separate process in server mode. This means that a single Maxima session is used for all Maxima code chunks of an \code{RMarkdown} document. Inputs and outputs can thus be used across chunks (e.g. by using Maxima reference labels). \code{maxima.options(engine.format = ..., engine.label = ...)} configures the output format and whether or not output reference labels should be printed.
#'
#' The purpose of \code{maxima.inline} is to insert Maxima results as inline text, i.e. on the same line of the preceding text, if it is actually written on the same line of the \code{RMarkdown} file. It uses the same running Maxima process as \code{maxima.engine}. The output format for inline results can be configured separately from the settings of \code{maxima.engine}, i.e. \code{maxima.options(inline.format = ..., inline.label = ...)}.
#'
#' @param options named \code{list} of \code{knitr} options. Supported options are \code{echo}, \code{eval}, \code{include} and \code{output.var}. To change the output format of the Maxima engine set the option \code{maxima.options(engine.format)} to either \code{"linear"} (default), \code{"ascii"}, \code{"latex"} or \code{"mathml"}.
#'
#' @return This functions prints the resulting output from maxima together with it's cod
maxima.engine <- function(options) {
  maxima.engine.start()

  out_code <- options$code
  ccode <- character(0)
  output.data <- list()
  out_res <- character(0)
  ll <- list()

  ov <- !is.null(varname <- options$output.var)
  cmds <- dissect_chunk(out_code)

  if (options$eval) {
    for (i in 1:length(cmds)) {
      pc <- paste0(out_code[cmds[[i]]], collapse = "\n")
      # if plotting command, then it needs to end with ";"
      if (grepl(pattern = "^(?:plot|draw)(?:2d|3d)?\\([[:print:]|[:space:]]+\\)[[:space:]]*\\$$", x = pc)) {
        pc <- gsub(pattern = "\\$", replacement = ";", x = pc)
      }
      tt <- maxima.env$mx$get(pc)

      attr(tt, "from_engine") <- TRUE

      out_code[cmds[[i]]] <- unlist(strsplit(x = iprint(tt), split = '\n', fixed = TRUE))
      ccode <- append(ccode, unlist(strsplit(x = iprint(tt), split = '\n', fixed = TRUE)))
      if (!attr(tt, "suppressed")) {
        ll <- append(ll, list(structure(list(src = ccode), class = "source")))
        if (ov) 
          output.data[[substring(attr(tt, "output.label"), 2L)]] <- attr(tt, "parsed")

        # +++ FIGURE OUTPUT +++
        if (grepl(pattern = "^(?:plot|draw)(?:2d|3d)?\\([[:print:]|[:space:]]+\\)[[:space:]]*;", x = pc) &
            !is.null(knitr::all_labels(engine == "maxima"))) {
          tt$wol$ascii <- paste0(tt$wol$ascii, collapse = "")
          pm <- regexec(pattern = "\\[?([[:graph:]]*/?\\.?(?:plot|draw)(?:2d|3d)?-[a-z0-9]+\\.(?:png|pdf))\\]$", text = tt$wol$ascii)
          pm <- trim(unlist(regmatches(m = pm, x = tt$wol$ascii))[2])

          # possibly image not yet written to disk
          pm <- retry_include_graphics(pm)
          ll <- append(ll, list(pm))
          out_res <- c(out_res, list(pm))
          maxima.env$plots <- append(maxima.env$plots, normalizePath(pm, mustWork = FALSE))
        } else {
        # +++ TEXT OUTPUT +++
          ll <- append(ll, ttt <- print(tt))
          out_res <- c(out_res, ttt)
        }
        ccode <- character(0)
      }
    }

    if (ov) {
      assign(varname, output.data, envir = rim_global())
    }
  }

  # called_from_fn("knit") not needed since engine is ALWAYS called from knit
  if (last_label(options$label) & called_from_fn("knit")) {
    maxima.engine.stop()
  }

  # special handling for RStudio
  if(is_interactive()) {
    knitr::engine_output(knitr::opts_current$merge(list(engine = 'maxima',
                                                        lang = 'maxima',
                                                        results = 'markup')),
                         code = NULL,
                         out = out_res)
  } else {
    knitr::engine_output(knitr::opts_current$merge(list(engine = 'maxima',
                                                        lang = 'maxima',
                                                        results = maxima.options$engine.results)), 
                         out = ll)
  }
}

maxima.engine.start <- function() {
  if(is.null(knitr::all_labels(engine == "maxima"))) {
    maxima.env$mx <- maxima.env$maxima
  } else {
    if (!exists("mx", envir = maxima.env)) {
      maxima.env$mx <- RMaxima$new(
                                   display = maxima.options$display,
                                   preload = maxima.options$preload[knitr::is_latex_output() + 1]
      )
      maxima.env$plots <- character()
    }
  }
}

maxima.engine.stop <- function() {
  if(!is.null(knitr::all_labels(engine == "maxima"))) {
    maxima.env$mx$stop()
    e <- sys.frame(which = 1)
    do.call("on.exit", list(quote(if (exists("maxima.env")) file.remove(maxima.env$plots)), add = TRUE), envir = e)
    do.call("on.exit", list(quote(if (exists("maxima.env")) rm(plots, envir = maxima.env)), add = TRUE), envir = e)
    rm(mx, envir = maxima.env)
  }
}

last_label <- function(label = knitr::opts_current$get("label")) {
  # if (knitr:::child_mode()) return(FALSE)
  if(knitr::opts_knit$get("child")) {
    return(FALSE)
  }

  if(is_interactive()) {
    return(TRUE)
  }

  labels <- knitr::all_labels(engine == "maxima")
  utils::tail(labels, 1) == label
}

#' @describeIn maxima.engine This function can be used to insert maxima outputs as inline.
#' @param command character string containing the Maxima command to be executed.
#' @return character string containing the maxima result printed according options set by \code{maxima.options(inline.format = ..., inline.label = ...)}.
#' @export
#' @examples
#' if (maxima.isInstalled()) {
#'   maxima.inline("2+2;")
#'   maxima.stop(engine = TRUE)
#' }
maxima.inline <- function(command) {
  maxima.engine.start()
  x <- maxima.env$mx$get(command)

  switch(maxima.options$inline.label + 1,
    paste0(c(x[["wol"]][[maxima.options$inline.format]], ""), collapse = "\n"),
    paste0(c(x[["wtl"]][[maxima.options$inline.format]], ""), collapse = "\n")
  )
}
