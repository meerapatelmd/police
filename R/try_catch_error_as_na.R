#' Skip error messages, records NA, and continues to loop on the expression
#' @param expr expression
#' @export
try_catch_error_as_na <-
        function(expr) {
                tryCatch(expr, error = function(e) NA)
        }
