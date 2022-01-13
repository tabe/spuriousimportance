## -*- mode: R -*-
##
## Copyright (C) 2022 Takeshi Abe
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
    s <- factor(ifelse(r < stats::median(r), "A", "B"))
    data.frame(v = v,
               w = w,
               x = x,
               y = y,
               z = z,
               r = r,
               s = s)
}
