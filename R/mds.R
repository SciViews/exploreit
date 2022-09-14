#' Multidimensional scaling or principal coordinates analysis
#'
#' @description Perform a PCoA (`type = "metric"). or other forms of MDS.
#'
#' @param dist A **dist** object from [stats::dist()] or other compatible
#'   functions like [vegan::vegdist()], or a **Dissimilarity** object, see
#'   [dissimilarity()].
#' @param k The dimensions of the space for the representation, usually `k = 2`
#'   (by default). It should be possible to use also `k = 3` with extra care and
#'   custom plots.
#' @param type The type of MDS: `"metric"` (= PCoA) = `"wcmdscale"`,
#'   `"nonmetric"` = `"metaMDS`, `"cmdscale"`, `"isoMDS"`, `"monoMDS"`, or
#'   `"sammon"`. Apart for `"metric"` and `"nonmetric"` which are synonyms, the
#'   other types refer to the name of the function in packages {stats}, {MASS}
#'   or {vegan}. See [vegan::wcmdscale()], [vegan::metaMDS()],
#'   [stats::cmdscale()], [MASS::isoMDS()], [vegan::monoMDS()] or
#'   [MASS::sammon()] for more details and additional arguments.
#' @param p For types `"nonmetric"`, `"metaMDS"`, `"isoMDS"`, `"monoMDS"` and
#'   `"sammon"`, a Shepard plot is also precalculated. `p`is the power for Minkowski distance in the configuration scale. By default, `p = 2`. Leave it like that if you don't understand what it means see [MASS::Shepard()].
#' @param ... More arguments (see respective `type`s or functions)
#' @param object An **mds** object
#' @param mds Idem
#' @param x Idem
#' @param y Not used
#' @param labels Points labels on the plot (optional)
#' @param col Points color (optional)
#' @param type Not used
#' @param env Not used
#' @param l.col Color of the line in the Shepard's plot (red by default)
#' @param l.lwd Width of the line in the Shepard"s plot (1 by default)
#' @param alpha Alpha transparency for points (0.5 by default, meaning 50%
#'   transparency)
#' @param xlab Label for the X axis (a default value exists)
#' @param ylab Idem for the Y axis
#' @param data A data frame to augment with columns from the MDS analysis
#'
#' @return A **mds** object, which is a list containing all components from the
#'   corresponding function, plus possibly `Shepard` if the Shepard plot is
#'   precalculated.
#' @export
#'
#' @examples
#' library(chart)
#' data(iris, package = "datasets")
#' iris_num <- iris[, -5] # Only numeric columns
#' iris_dis <- dissimilarity(iris_num, method = "euclidean")
#'
#' # Metric MDS
#' iris_mds <- mds$metric(iris_dis)
#' chart(iris_mds, labels = 1:nrow(iris), col = iris$Species)
#'
#' # Non-metric MDS
#' iris_nmds <- mds$nonmetric(iris_dis)
#' chart(iris_nmds, labels = 1:nrow(iris), col = iris$Species)
#' glance(iris_nmds) # Good R^2
#' iris_sh <- shepard(iris_dis, iris_nmds)
#' chart(iris_sh) # Excellent matching + linear -> metric MDS is OK here
mds <- function(dist, k = 2, type = c("metric", "nonmetric", "cmdscale",
  "wcmdscale", "sammon", "isoMDS", "monoMDS", "metaMDS"), p = 2, ...) {
  type <- match.arg(type)
  res <- switch(type,
    metric = ,
    wcmdscale = structure(vegan::wcmdscale(d = dist, k = k, eig = TRUE, ...),
      class = c("wcmdscale", "mds", "list")),
    cmdscale = structure(stats::cmdscale(d = dist, k = k, eig = TRUE, ...),
      class = c("cmdscale", "mds", "list")),
    nonmetric = ,
    metaMDS = structure(vegan::metaMDS(comm = dist, k = k, ...),
      class = c("metaMDS", "monoMDS", "mds", "list")),
    isoMDS = structure(MASS::isoMDS(d = dist, k = k, ...),
      class = c("isoMDS", "mds", "list")),
    monoMDS = structure(vegan::monoMDS(dist = dist, k = k, ...),
      class = c("monoMDS", "mds", "list")),
    sammon = structure(MASS::sammon(d = dist, k = k, ...),
      class = c("sammon", "mds", "list")),
    stop("Unknown 'mds' type ", type)
  )
  # For non-metric MDS, we add also data required for the Shepard plot
  if (type %in% c("nonmetric", "sammon", "isoMDS", "monoMDS", "metaMDS"))
    res$Shepard <- MASS::Shepard(d = dist, x = res$points, p = p)
  res
}
class(mds) <- c("function", "subsettable_type")

#' @export
#' @rdname mds
plot.mds <- function(x, y, ...) {
  plot.mds : MDS2 ~ MDS1
  points <- as_tibble(x$points, .name_repair = "minimal")
  colnames(points) <- paste0("mds", 1:ncol(points))

  plot(data = points, mds2 ~ mds1,...)
}

#' @export
#' @rdname mds
autoplot.mds <- function(object, labels, col, ...) {
  points <- as_tibble(object$points, .name_repair = "minimal")
  colnames(points) <- paste0("mds", 1:ncol(points))

  if (!missing(col))
    points$.colors <- col

  if (!missing(labels)) {
    if (length(labels) != nrow(points))
      stop("You must provide a character vector of length ", nrow(points),
        " for 'labels'")
    points$.labels <- labels
    if (!missing(col)) {
      chart(points, mds2 ~ mds1 %col=% .colors %label=% .labels, ...) +
        geom_point() +
        geom_text_repel() + # ggrepel::geom_text_repel()
        coord_fixed(ratio = 1)
    } else {# col not provided
      chart(points, mds2 ~ mds1 %label=% .labels, ...) +
        geom_point() +
        geom_text_repel() + # ggrepel::geom_text_repel()
        coord_fixed(ratio = 1)
    }

  } else {# Plot without labels
    if (!missing(col)) {
      chart(points, mds2 ~ mds1 %col=% .colors, ...) +
        geom_point() +
        coord_fixed(ratio = 1)
    } else {
      chart(points, mds2 ~ mds1, ...) +
        geom_point() +
        coord_fixed(ratio = 1)
    }
  }
}

#' @export
#' @rdname mds
chart.mds <- function(data, labels, col, ..., type = NULL, env = parent.frame())
  autoplot(data, labels = labels, col, type = type, ...)

#' @export
#' @rdname mds
shepard <- function(dist, mds, p = 2)
  structure(MASS::Shepard(d = dist, x = mds$points, p = p),
    class = c("shepard", "list"))

#' @export
#' @rdname mds
plot.shepard <- function(x, y, l.col = "red", l.lwd = 1,
  xlab = "Observed Dissimilarity", ylab = "Ordination Distance", ...) {
  she <- as_tibble(x, .name_repair = "minimal")

  plot(data = she, y ~ x, xlab = xlab, ylab = ylab, ...)
  lines(data = she, yf ~ x, type = "S", col = l.col, lwd = l.lwd)
}

#' @export
#' @rdname mds
autoplot.shepard <- function(object, alpha = 0.5, l.col = "red", l.lwd = 1,
  xlab = "Observed Dissimilarity", ylab = "Ordination Distance", ...) {
  she <- as_tibble(object)

  chart(data = she, y ~ x) +
    geom_point(alpha = alpha) +
    geom_step(f_aes(yf ~ x), direction = "vh", col = l.col, lwd = l.lwd) +
    labs(x = xlab, y = ylab)
}

#' @export
#' @rdname mds
chart.shepard <- function(data, alpha = 0.5, l.col = "red", l.lwd = 1,
  xlab = "Observed Dissimilarity", ylab = "Ordination Distance", ..., type = NULL,
  env = parent.frame())
  autoplot(data, alpha = alpha, l.col = l.col, l.lwd = l.lwd,
    xlab = xlab, ylab = ylab, type = type, ...)

#' @export
#' @rdname mds
augment.mds <- function(x, data, ...){
  if (is.null(rownames(x$points)) || all(rownames(x) == 1:NROW(x$points))) {
    points <- as_tibble(x$points)
  } else {
    points <- as_tibble(x$points,
      rownames = getOption("SciViews.dtx.rownames", default = ".rownames"))
  }
  colnames(points) <- paste0(".mds", 1:ncol(points))
  cbind(data, points) # bind_cols
}

#' @export
#' @rdname mds
glance.mds <- function(x, ...){
  if ("GOF" %in% names(x)) {# Probably cmdscale() or wcmdscale() => metric MDS
    tibble(GOF1 = x$GOF[1], GOF2 = x$GOF[2])
  } else {# Non metric MDS
    # Calculate linear and non linear R^2 from the Shepard (stress) plot
    tibble(
      linear_R2 = cor(x$Shepard$y, x$Shepard$yf)^2,
      nonmetric_R2 = 1 - sum(goodness(x)^2) # vegan::goodness()
    )
  }
}
