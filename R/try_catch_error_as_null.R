#' Skip error messages and continues to loop on the expression
#' @param expr expression
#' @export
try_catch_error_as_null <-
        function(expr) {
                tryCatch(expr, error = function(e) NULL)
        }
