# This is to draw circles in a plot (where to cut in a circular dendrogram)
# TODO: should be nice to do similar function for other symbols too in SciViews
#' Draw circles in a plot
#'
#' @description Add a circle in a base R plot, given its center (x and y
#' coordinates) and its diameter. In {exploreit}, this function can be used to
#' cut a circular dendrogram, see example.
#'
#' @param x The x coordinate of the center of the circle.
#' @param y The y coordinate of the center of the circle.
#' @param d The diameter of the circle.
#' @param col The color of the border of the circle.
#' @param lwd The width of the circle border.
#' @param lty The line type to use to draw the circle.
#' @param ... More arguments passed to [symbols()].
#'
#' @return This function returns `NULL`. It is invoked for it side effect of
#' adding a circle in a base R plot.
#' @export
#' @seealso [symbols()]
#'
#' @examples
#' plot(x = 0:2, y = 0:2)
#' circle(x = 1, y = 1, d = 1, col = "red", lwd = 2, lty = 2)
circle <- function(x = 0, y = 0, d = 1, col = 0, lwd = 1, lty = 1, ...) {
  symbols(x = x, y = y, circles = d / 2, fg = col, lwd = lwd, lty = lty,
    inches = FALSE, add = TRUE, ...)
}
