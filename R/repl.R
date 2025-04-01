#' Run a Maxima REPL
#'
#' This function provides a Maxima REPL in the \R session, which can be used 
#' to interactively run Maxima code. All code executed within the REPL is
#' run within the interactive Maxima session that is started when rim is 
#' attached and any generated Maxima objects will persist in the Maxima
#' session after the REPL is detached.
#'
#' When working with R and Maxima scripts interactively, one can activate
#' the Python REPL with `maxima.repl()`, run Maxima code, and later run `exit;`
#' to return to the \R console.
#'
#' Note that, inside the REPL, multiple commands are allowed.
#'
#' The output format displayed inside the REPL is controlled by `maxima.options(repl.format = ...)`.
#'
#' @param history_filename A (optional) filename to which all Maxima commands
#' during the repl session are saved. If not provided (default), command history
#' lookup will still be available (if supported by OS) in the same way as in the
#' R session.
#'
#' @export
maxima.repl <- function(history_filename = NA_character_) {
  stopifnot(is.character(history_filename), 
            length(history_filename) == 1L)

  cat("Starting Maxima REPL... Type 'exit' to detach.\n")

  # check to see if the current environment supports history
  # (check for case where working directory not writable)
  use_history <-
    !"--vanilla" %in% commandArgs() &&
    !"--no-save" %in% commandArgs() &&
    !is.null(getwd()) &&
    tryCatch(
             { utils::savehistory(tempfile()); TRUE },
             error = function(e) FALSE
    )

    if (use_history) {
      # if we have history, save and then restore the current
      # R history
      utils::savehistory()
      on.exit(utils::loadhistory(), add = TRUE)

      # file to be used for command history during sessions
      histfile <- ifelse(missing(history_filename) | is.na(history_filename),
                         file.path(tempdir(), ".maxima_repl_history.mac"),
                         file.path(history_filename))

      # load history (create empty file if none exists yet)
      if (!file.exists(histfile))
        file.create(histfile)
      utils::loadhistory(histfile)
    }

  # Start a loop for the REPL
  repeat {
    prmpt <- paste0("(", maxima.env$maxima$getCurrentInputLabel(), ") ")
    # Prompt the user for input
    user_input <- readline(prompt = prmpt)

    # Exit condition
    if (user_input == "exit") {
      cat("Exiting Maxima REPL...\n")
      break
    }

    # splice and send the commands to Maxima and evaluate
    xx <- dissect_repl_input(user_input)
    for(i in 1:length(xx)) {
      tryCatch({
        x <- maxima.env$maxima$get(xx[[i]])
        print(x)
      }, error = function(e) {
        # Handle any errors (e.g., invalid Maxima commands)
        cat("\nError:", e$message, sep = '\n')
      })
    }

    # update history file
    if (use_history) {
      cat(user_input, file = histfile, sep = "\n", append = TRUE)
      utils::loadhistory(histfile)
    }
  }
}
