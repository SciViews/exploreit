#' K-means clustering
#'
#' @description Perform a k-means clustering analysis using the
#'   [stats::kmeans()] function in {stats} but creating a **k_means** object
#'   that possibly embeds the original data with the analysis for a richer set
#'   of methods.
#'
#' @param x A data frame or a matrix with numeric data
#' @param k The number of clusters to create, or a set of initial cluster
#'   centers. If a number, a random set of initial centers are computed first.
#' @param centers Idem (`centers` is synonym to `k`)
#' @param iter.max Maximum number of iterations (10 by default)
#' @param nstart If `k` is a number, how many random sets should be chosen?
#' @param algorithm The algorithm to use. May be abbreviated. See
#'   [stats::kmeans()] for more details about available algorithms.
#' @param trace Logical or integer. Should process be traced. Higher value
#'   produces more tracing information.
#' @param keep.data Do we keep the data in the object? If `TRUE` (by default),
#'   a richer set of methods could be applied to the resulting object, but it
#'   takes more space in memory. Use `FALSE` if you want to save RAM.
#' @param fun The kmeans clustering function to use, `kmeans()` by default.
#' @param method The method used in [profile_k()]: `"wss"` (by default, total
#'   within sum of square), `"silhouette"` (average silhouette width) or
#'   `"gap_stat"` (gap statistics).
#' @param k.max Maximum number of clusters to consider (at least two). If not
#'   provided, a reasonable default is calculated.
#' @param ... Other arguments transmitted to [factoextra::fviz_nbclust()].
#' @param object The *k_means** object
#' @param data The original data frame
#' @param y Not used
#' @param choices The axes (variables) to plot (first and second by default)
#' @param col Color to use
#' @param c.shape The shape to represent cluster centers
#' @param c.size The size of the shape representing cluster centers
#' @param alpha Semi-transparency to apply to points
#' @param theme The ggplot theme to apply to the plot
#' @param use.chart If `TRUE` use [chart()], otherwise, use [ggplot()].
#' @param type Not used here
#' @param env Not used here
#'
#' @return [k_means()] creates an object of classes **k_means** and **kmeans**.
#'   [profile_k()] is used for its side-effect of creating a plot that should
#'   help to chose the best value for `k`.
#' @export
#'
#' @examples
#' # TODO...
k_means <- function(x, k, centers = k, iter.max = 10L, nstart = 1L,
algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), trace = FALSE,
keep.data = TRUE) {
  # k and centers are synonyms
  res <- kmeans(x, centers = centers, iter.max = iter.max, nstart = nstart,
    algorithm = algorithm, trace = trace)
  if (isTRUE(keep.data))
    res$data <- as.data.frame(x)
  class(res) <- unique(c("k_means", class(res)))
  res
}

#' @export
#' @rdname k_means
profile_k <- function(x, fun = kmeans, method = "wss", k.max = NULL, ...) {
  # This is a reworked version of factoextra::fviz_nbclust() to help chosing the
  # number of clusters for k_means()
  if (NROW(x) < 2)
    stop("You must provide an data frame or matrix with at least two rows")
  if (is.null(k.max))
    k.max <- min(nrow(x) - 1, 10) # Avoid error with very small datasets in fviz_nbclust()
  # factoextra::fviz_nbclust
  fviz_nbclust(x, FUNcluster = fun, method = method, k.max = k.max, ...)
}

#' @export
#' @rdname k_means
augment.kmeans <- function(x, data, ...) {
  # broom::augment.kmeans() seems buggy when data is called 'x'
  res <- fix_data_frame(data, newcol = ".rownames")
  res$.cluster <- factor(x$cluster)
  res
}

#' @export
#' @rdname k_means
predict.k_means <- function(object, ...) {
  # No predict() method for kmeans, but we add one for k_means
  factor(object$cluster)
}


#' @export
#' @rdname k_means
plot.k_means <- function(x, y, data = x$data, choices = 1L:2L,
col = NULL, c.shape = 8, c.size = 3, ...) {
  # There is no plot, autoplot and chart methods for kmeans objects => make them
  # for k_means objects, because we have both the k-means results and the data
  # Since data is no contained in the kmeans object, one has to provide it also
  nclust <- nrow(x$centers)
  if (is.null(col))
    col <- 1:nclust
  plot(as.data.frame(data)[, choices], col = col[x$cluster], ...)
  points(as.data.frame(x$centers)[, choices], col = col[1:nclust],
    pch = c.shape, cex = c.size)
}

#' @export
#' @rdname k_means
autoplot.k_means <- function(object, data = object$data, choices = 1L:2L,
alpha = 1, c.shape = 8, c.size = 3, theme = NULL, use.chart = FALSE, ...) {
  data <- as.data.frame(data)
  vars <- choices
  if (is.numeric(choices))
    vars <- colnames(data)[choices]
  var_x <- as.name(vars[1])
  var_y <- as.name(vars[2])
  centers <- tidy(object, col.names = colnames(data))
  cluster <- factor(object$cluster)
  if (isTRUE(use.chart)) {
    fun <- chart
  } else {
    fun <- ggplot
  }
  res <- fun(data = data, mapping = aes(x = {{var_x}}, y = {{var_y}},
    col  = cluster)) +
    geom_point(alpha = alpha) +
    geom_point(data = centers, size = c.size, shape = c.shape)
  if (!is.null(theme))
    res <- res + theme
  res
}

#' @export
#' @rdname k_means
chart.k_means <- function(data, ..., type = NULL, env = parent.frame())
  autoplot(data, type = type, theme = theme_sciviews(), use.chart = TRUE, ...)
