#' Return and write any system errors and warnings for an expression to the working directory README
#' @description This function is meant to catch errors to trace back to the steps in a workflow where things bottlenecked.
#' @return If there are errors and warnings to report, they are returned in the console as well as in the working directory README.md.
#' @param expr Expression to evaluate
#' @importFrom secretary typewrite_error
#' @importFrom secretary typewrite_warning
#' @export

write_warn_or_error_ticket <- function(expr)
{
    W <- NULL
    w.handler <- function(w){ # warning handler
        W <<- w
        invokeRestart("muffleWarning")
    }

    output <-
        list(
            error = withCallingHandlers(tryCatch(expr,
                                                  error = function(e) e),
                                                    warning = w.handler),
             warning = W)

    if (("error" %in% class(output$error))) {
        error_message <- secretary::typewrite_error(output$error$message)
    }

    if (("warning" %in% class(output$warning))) {
        warning_message <- secretary::typewrite_warning(output$warning$message)
    }
}
