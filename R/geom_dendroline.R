# To indicate where to cut in the dendrogram, one could use `geom_hline()`,
# but when the dendrogram is horizontal or circular, this is suprizing. So,
# I define geom_dendroline(h = ....)

#' Draw a line to cut a dendrogram
#'
#' @description Add a line (horizontal, vertical, or circular, depending on the
#' dendrogram type) at height `h` to depict where it is cut into groups.
#' @param h The height to cut the dendrogram.
#' @param ... Further arguments passed to [geom_hline()] (this is really a convenience function that builds on it).
#'
#' @export
#' @seealso [geom_hline()]
#'
#' @examples
#' SciViews::R
#' iris <- read("iris", package = "datasets")
#' iris %>.%
#'   select(., -species) %>.%
#'   dissimilarity(.) %>.%
#'   cluster(.) ->
#'   iris_cluster
#' chart(iris_cluster) +
#'   geom_dendroline(h = 3, color = "red")
geom_dendroline <- function(h, ...) {
  geom_hline(yintercept = h, ...)
}
