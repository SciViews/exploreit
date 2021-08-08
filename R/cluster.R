# TODO: Other methods by transforming into dendrogram: rev, reorder, order, [[

#' Hierarchical Clustering Analysis
#'
#' @description Hierarchical clustering is an agglomerative method that uses a
#' dissimilarity matrix to group individuals. It is represented by a dendrogram
#' that can be cut at a certain level to form the final clusters.
#'
#' @param x A `Dissimilarity` object.
#' @param ... Further arguments for the methods (see their respective manpages).
#'
#' @return A `Cluster` object inheriting from `hclust`. Specific methods are: [str()] (compact display of the object content), [labels()] (get the labels for the observations), [nobs()] (number of observations), [predict()] (get the clusters, given a cutting level), [augment()] (add the groups to the original data frame or tibble), [plot()] (create a dendrogram as base R plot), [autoplot()] (create a dendrogram as a ggplot2), and [chart()] (create a dendrogram as a chart variant of a ggplot2).
#' @export
#' @seealso [dissimilarity()], [stats::hclust()], [fastcluster::hclust()]
#'
#' @examples
#' SciViews::R
#' iris <- read("iris", package = "datasets")
#' iris_num <- select(iris, -species) # Only numeric columns from iris
#' # Cluster the 150 flowers
#' iris_dis <- dissimilarity(iris_num, method = "euclidean", scale = TRUE)
#' (iris_clust <- cluster(iris_dis, method = "complete"))
#' str(iris_clust) # More useful
#' str(iris_clust, max.level = 3L) # Only the top of the dendrogram
#'
#' # Dendrogram with base R graphics
#' plot(iris_clust)
#' plot(iris_clust, labels = FALSE, hang = 0.1)
#' abline(h = 3.5, col = "red")
#' # Horizontal dendrogram
#' plot(iris_clust, type = "horizontal", labels = FALSE)
#' abline(v = 3.5, col = "red")
#' # Circular dendrogram
#' plot(iris_clust, type = "circular", labels = FALSE)
#' circle(d = 3.5, col = "red")
#'
#' # Chart version of the dendrogram
#' chart(iris_clust) +
#'   geom_dendroline(h = 3.5, color = "red")
#' # Horizontal dendrogram and without labels
#' chart$horizontal(iris_clust, labels = FALSE) +
#'   geom_dendroline(h = 3.5, color = "red")
#' # Circular dendrogram with labels
#' chart$circ(iris_clust, circ.text.size = 3) + # Abbreviate type and change size
#'   geom_dendroline(h = 3.5, color = "red")
#'
#' # Get the clusters
#' predict(iris_clust, h = 3.5)
#' # Four clusters
#' predict(iris_clust, k = 4)
#' # Add the clusters to the data (.fitted column added)
#' augment(data = iris, iris_clust, k = 4)
cluster <- function(x, ...) {
  # cluster object (inheriting from hclust) but with different/more methods
  UseMethod("cluster")
}

#' @export
#' @rdname cluster
cluster.default <- function(x, ...) {
  stop("No method for object of class ", class(x)[1])
}

# Cluster uses hclust() by default, ... but it looks first for a faster
# implementation in either {fastcluster} or {flashClust} before falling back
# to the {stats} version.
# The functions cluster::agnes() and cluster::diana() should be compatible too,
# as well as any function that returns an object convertible into hclust
# by as.hclust() (but not tested yet)
# Also, a version where the raw data are provided and the disimilarity matrix
# is internally calculated should be also provided (see cluster::agnes)
# See also {ape} for phylogenetic trees methods

#' @export
#' @rdname cluster
#' @param method The agglomeration method used. `"complete"` by default. Other
#' options depend on the function `fun =` used. For the default one, you can
#' also use `"single"`, `"average"`, `"mcquitty"`, `"ward.D"`, `"ward.D2"`,
#' `"centroid"`, or `"median"`.
#' @param fun The function to use to do the calculation. By default, it is
#' [fastcluster::hclust()], an fast and memory-optimized version of the default
#' R function [stats::hclust()]. You can also use [flashClust::hclust()],
#' [cluster::agnes()], [cluster::diana()], as well as, any other function that
#' returns an `hclust`object, or something convertible to `hclust` with
#' [as.hclust()]. The default (`NULL`) means that the **fastcluster**
#' implementation is used.
cluster.dist <- function(x, method = "complete", fun = NULL, ...) {
  if (is.null(fun)) {
    fun <- fastcluster::hclust
    # We try fastcluster, then flashClust, then stats
    #fun <- try(fastcluster::hclust, silent = TRUE)
    #if (inherits(fun, "try-error"))
    #  fun <- try(flashClust::hclust, silent = TRUE)
    #if (inherits(fun, "try-error"))
    #  fun <- try(stats::hclust, silent = TRUE)
  }
  clst <- fun(x, method = method, ...)
  clst <- as.hclust(clst)
  clst$call <- match.call()
  # hclust has to give a different name to the distance metric: dist.method
  # but we use metric. Again, keep both for maximum compatibility
  clst$metric <- clst$dist.method
  # If the original data were scaled or transposed, get the info also
  clst$rownames.col <- attr(x, "rownames.col")
  clst$scale <- attr(x, "scale")
  clst$transpose <- attr(x, "transpose")
  class(clst) <- unique(c("Cluster", class(clst)))
  clst
}

#' @export
#' @rdname cluster
#' @param object A `cluster` object.
#' @param max.level The maximum level to present.
#' @param digits.d The number of digits to print.
str.Cluster <- function(object, max.level = NA, digits.d = 3L, ...) {
  # str() method is gathered from a dendrogram object
  str(as.dendrogram(object), max.level = max.level, digits.d = digits.d, ...)
}

#' @export
#' @rdname cluster
labels.Cluster <- function(object, ...) {
  object$labels
}

#' @export
#' @rdname cluster
nobs.Cluster <- function(object, ...) {
  length(object$order)
}

#' @export
#' @rdname cluster
#' @param k The number of clusters to get.
#' @param h The height where the dendrogram should be cut (give either `k =` or
#' `h = `, but not both at the same time).
predict.Cluster <- function(object, k = NULL, h = NULL, ...) {
  # cutree() is an explicit name, but it does not follow the rule of using
  # known methods... and here, it really something that predict() is made for,
  # except it cannot handle newdata =, that argument is not in its definition
  cutree(object, k = k, h = h)
}

#' @export
#' @rdname cluster
#' @param data The original dataset
augment.Cluster <- function(x, data, k = NULL, h = NULL, ...) {
  # There is no broom::glance() or broom::tidy() yet (what to put in it?),
  # but broom:augment() should be nice: add clusters as .fitted in the tibble
  # We depend on generics for the definition of augment
  # Should we transpose the data (note: this is against augment() rules, but...)
  if (isTRUE(x$transpose)) {
    # We first have to make sure rownames are correct before the transposition
    if (!is.matrix(data) && !is.null(data[[x$rownames.col]])) {
      rownames(data) <- data[[x$rownames.col]]
      data[[x$rownames.col]] <- NULL
    }
    data <- t(data)
    msg <- "transposed data"
  } else {
    msg <- "data"
  }
  data <- as_tibble(data)

  # Get clusters
  clst <- predict(x, k = k, h = h, ...)
  if (nrow(data) != length(clst)) {
    stop("Different number of items in ", msg, " (",nrow(data) ,
      ") and in the clusters (", length(clst), ")")
  }
  tibble::add_column(data, .fitted = clst)
}

#' @export
#' @rdname cluster
#' @param y Do not use it.
#' @param labels Should we show the labels (`TRUE` by default).
#' @param hang The fraction of the plot height at which labels should hang below
#' (by default, -1 meaning labels are all placed at the extreme of the plot).
#' @param check The validity of the `cluster` object is verified first to avoid
#' crashing R. You can put it at `FALSE` to speed up computation if you are
#' really sure your object is valid.
#' @param type The type of dendrogram, by default, `"vertical"`. It could also
#' be `"horizontal"` (more readable when there are many observations), or
#' `"circular"` (even more readable with many observations, but more difficult
#' to chose the cutting level).
#' @param lab The label of the y axis (vertical) or x axis (horizontal), by
#' default `"Height"`.
plot.Cluster <- function(x, y, labels = TRUE, hang = -1, check = TRUE,
type = "vertical", lab = "Height", ...) {
  # Instead of the default plot.hclust(), we prefer plot.dendrogram()
  # that allows for more and better variations of the dendrogram (horizontal or
  # circular), see http://www.sthda.com/english/wiki
  # /beautiful-dendrogram-visualizations-in-r-5-must-known-methods
  # -unsupervised-machine-learning
  type <- match.arg(type[1], c("vertical", "horizontal", "circular"))
  # type == "circular" is special because we need to transform as ape::phylo
  if (type == "circular") {
    if (!missing(hang))
      warning("'hang' is not used with a circular dendrogram")
    phylo <- ape::as.phylo(x)
    plot(phylo, type = "fan", font = 1, show.tip.label = labels, ...)
  } else {# Use plot.dendrogram() instead
    # We first convert into dendrogram objet, then we plot it
    # (better that plot.hclust())
    if (isTRUE(labels)) leaflab <- "perpendicular" else leaflab <- "none"
    dendro <- as.dendrogram(x, hang = hang, check = check)
    if (type == "horizontal") {
      plot(dendro, horiz = TRUE, leaflab = leaflab, xlab = lab, ...)
    } else {
      plot(dendro, horiz = FALSE, leaflab = leaflab, ylab = lab, ...)
    }
  }
}

#' @export
#' @rdname cluster
#' @param circ.text.size Size of the text for a circular dendrogram
#' @param theme The ggplot2 theme to use, by default, it is [theme_sciviews()].
#' @param xlab Label of the x axis (nothing by default)
#' @param ylab Label of the y axis, by default `"Height"`.
autoplot.Cluster <- function(object, labels = TRUE, type = "vertical",
circ.text.size = 3, theme = theme_sciviews(), xlab = "", ylab = "Height", ...) {
  # TODO: make sure the dendrogram is correct with different ggplot2 themes
  if (is.null(type))
    type <- "vertical"
  type <- match.arg(type[1], c("vertical", "horizontal", "circular"))

  # Create the dendrogram
  ddata <- ggdendro::dendro_data(object, type = "rectangle")
  # This is to avoid a problem in functions dependencies in R CMD check
  # but these are not used indeed
  x <- y <- xend <- yend <- NULL
  dendro <- ggplot(ggdendro::segment(ddata)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    theme + xlab(xlab) + ylab(ylab)

  if (type == "circular") {
    if (isTRUE(labels)) {
      # Get labels (need one more to avoid last = first!)
      label_df <- tibble::tibble(labels = c(labels(object)[object$order], ""))
      xmax <- nobs(object) + 1
      label_df$id <- 1:xmax
      angle <-  360 * (label_df$id - 0.5) / xmax
      # Left or right?
      label_df$hjust <- ifelse(angle < 270 & angle > 90, 1, 0)
      # Angle for more readable text
      label_df$angle <- ifelse(angle < 270 & angle > 90, angle + 180, angle)
    }

    # Make the dendrogram circular
    dendro <- dendro +
      scale_x_reverse() +
      scale_y_reverse() +
      coord_polar(start = pi/2)
    if (isTRUE(labels))
      # This is to avoid a problem in functions dependencies in R CMD check
      # but these are not used indeed
      id <- hjust <- NULL
      dendro <- dendro +
      geom_text(data = label_df,
        aes(x = id, y = -0.02, label = labels, hjust = hjust),
        size = circ.text.size, angle = label_df$angle, inherit.aes = FALSE)
    dendro <- dendro +
      theme(panel.border = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks.y = element_blank()) +
      ylab("")

  } else if (type == "vertical") {# Vertical dendrogram
    dendro <- dendro +
      scale_x_continuous(breaks = seq_along(ddata$labels$label),
        labels = ddata$labels$label) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
      theme(panel.border = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
    if (!isTRUE(labels))
      dendro <- dendro +
        theme(axis.text.x = element_blank())

  } else {# Horizontal dendrogram
    dendro <- dendro +
      scale_x_continuous(breaks = seq_along(ddata$labels$label),
        labels = ddata$labels$label, position = "top") +
      scale_y_reverse(expand = expansion(mult = c(0.05, 0))) +
      coord_flip() +
      theme(panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())
    if (!isTRUE(labels))
      dendro <- dendro +
        theme(axis.text.y = element_blank())
  }
  dendro
}

#' @export
#' @rdname cluster
#' @param env The environment where to evaluate formulas. If you don't
#'   understand this, it means you should not touch it!
chart.Cluster <- function(data, ...,
type = NULL, env = parent.frame()) {
  p <- autoplot(data, type = type, ...)
  class(p) <- unique(c("Chart", class(p)))
  p
}
