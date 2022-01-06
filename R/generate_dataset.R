#' Generate a crafted dataset of spurious importance
#'
#' Generate a crafted dataset as an example of important predictors with spuriously low importance.
#'
#' @param n the number of rows
#'
#' @examples
#' ds <- generate_dataset(1000)
#'
#' @export
generate_dataset <- function(n = 500) {
    v <- stats::rnorm(n)
    w <- 1-v
    x <- stats::rnorm(n)
    y <- stats::rnorm(n)
    z <- stats::rnorm(n)
    r <- 0.5 * v + 0.1 * x + 0.01 * y
    s <- factor(ifelse(r < mean(r), "A", "B"))
    data.frame(v = v,
               w = w,
               x = x,
               y = y,
               z = z,
               r = r,
               s = s)
}
