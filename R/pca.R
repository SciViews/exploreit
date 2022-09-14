#' Principal Component Analysis (PCA)
#'
#' @param x A formula or a data frame with numeric columns, for `as.prcomp()`,
#'   an object to coerce into **prcomp**.
#' @param ... For `pca()`, further arguments passed to [SciViews::pcomp()],
#'   notably, `data=`  associated with a formula, `subset=`(optional),
#'   `na.action=`, `method=` that can be `"svd"` or `"eigen"`. See
#'   [SciViews::pcomp()] for more details on these arguments.
#' @param object A **pcomp** object
#' @param type The type of plot to produce: `"screeplot"` or `"altscreeplot"`
#'   for two versions of the screeplot, `"loadings"`, `"correlations"`, or
#'   `"scores"` for the different views of the PCA, or a combined `"biplot"`.
#' @param choices Vector of two positive integers. The two axes to plot, by
#' default first and second axes.
#' @param name The name of the object (automatically defined by default)
#' @param ar.length The length of the arrow head on the plot, 0.1 by default
#' @param circle.col The color of the circle on the plot, gray by default
#' @param col The color for the points representing the observations, black by
#'   default.
#' @param fill The color to fill bars, gray by default
#' @param scale The scale to apply for annotations, 1 by default
#' @param aspect.ratio height/width of the plot, 1 by default (for plots where
#'   the ratio height / width does matter)
#' @param repel Logical. Should repel be used to rearrange points labels?
#' `FALSE`by default
#' @param labels The label of the points (optional)
#' @param title The title of the plot (optional, a reasonable default is used)
#' @param xlab The label for the X axis. Automatically defined if not provided
#' @param ylab Idem for the Y axis
#' @param env The environment where to evaluate code, `parent.frame()` by
#'   default, which should not be changed unless you really know what you are
#'   doing!
#' @param data The original data frame used for the PCA
#' @param newdata A data frame with similar structure to `data` and new
#'   observations
#' @param matrix Indicate which component should be be tidied. See
#'   [broom::tidy.prcomp()]
#'
#' @return [pca()] produces a **pcomp** object.
#' @export
#'
#' @examples
#' library(chart)
#' library(ggplot2)
#' data(iris, package = "datasets")
#' iris_num <- iris[, -5] # Only numeric columns
#' iris_pca <- pca(data = iris_num, ~ .)
#' summary(iris_pca)
#' chart$scree(iris_pca) # OK to keep 2 components
#' chart$altscree(iris_pca) # Different presentation
#'
#' chart$loadings(iris_pca, choices = c(1L, 2L))
#' chart$scores(iris_pca, choices = c(1L, 2L), aspect.ratio = 3/5)
#' # or better:
#' chart$scores(iris_pca, choices = c(1L, 2L), labels = iris$Species,
#'   aspect.ratio = 3/5) +
#'   stat_ellipse()
#'
#' # biplot
#' chart$biplot(iris_pca)
pca <- function(x, ...) {
  # There is a problem with pcomp() that returns a data.frame in scores,
  # while it is a matrix in the original princomp object. pca() corrects this
  res <- pcomp(x, ...) # SciViews::pcomp, should be here instead!
  # Change scores into a matrix
  res$scores <- as.matrix(res$scores)
  res
}

# Not exported utilitary function
scale_axes <- function(data, aspect.ratio = 1) {
  range_x <- range(data[, 1])
  span_x <- abs(max(range_x) - min(range_x))
  range_y <- range(data[, 2])
  span_y <- abs(max(range_y) - min(range_y))
  if ((span_y / aspect.ratio) > span_x) {
    # Adjust range_x
    span_x_2 <- span_y / aspect.ratio / 2
    range_x_mid <- sum(range_x) / 2
    range_x <- c(range_x_mid - span_x_2, range_x_mid + span_x_2)
  } else {
    # Adjust range_y
    span_y_2 <- span_x * aspect.ratio / 2
    range_y_mid <- sum(range_y) / 2
    range_y <- c(range_y_mid - span_y_2, range_y_mid + span_y_2)
  }
  list(x = range_x, y = range_y)
}

# ggfortify:::autoplot.prcomp is not exported => copy it here
ggfortify_autoplot.prcomp <- function(object, data = NULL, scale = 1, x = 1,
y = 2, variance_percentage = TRUE, ...) {
  plot.data <- ggplot2::fortify(object, data = data) # deprecated -> augment()!
  plot.data$rownames <- rownames(plot.data)
  if (inherits(object, "prcomp")) {
    ve <- object$sdev^2/sum(object$sdev^2)
    PC <- paste0("PC", c(x, y))
    x.column <- PC[1]
    y.column <- PC[2]
    loadings.column <- "rotation"
    lam <- object$sdev[c(x, y)]
    lam <- lam * sqrt(nrow(plot.data))
  } else if (inherits(object, "princomp")) {
    ve <- object$sdev^2/sum(object$sdev^2)
    PC <- paste0("Comp.", c(x, y))
    x.column <- PC[1]
    y.column <- PC[2]
    loadings.column <- "loadings"
    lam <- object$sdev[c(x, y)]
    lam <- lam * sqrt(nrow(plot.data))
  } else if (inherits(object, "factanal")) {
    if (is.null(attr(object, "covariance"))) {
      p <- nrow(object$loading)
      ve <- colSums(object$loading^2)/p
    } else ve <- NULL
    PC <- paste0("Factor", c(x, y))
    x.column <- PC[1]
    y.column <- PC[2]
    scale <- 0
    loadings.column <- "loadings"
  } else if (inherits(object, "lfda")) {
    ve <- NULL
    PC <- paste0("PC", c(x, y))
    x.column <- PC[1]
    y.column <- PC[2]
    scale <- 0
    loadings.column <- NULL
  } else {
    stop(paste0("Unsupported class for autoplot.pca_common: ",
      class(object)))
  }
  if (scale != 0) {
    lam <- lam^scale
    plot.data[, c(x.column, y.column)] <- t(t(plot.data[,
      c(x.column, y.column)])/lam)
  }
  plot.columns <- unique(c(x.column, y.column, colnames(plot.data)))
  plot.data <- plot.data[, plot.columns]
  if (!is.null(loadings.column)) {
    loadings.data <- as.data.frame(object[[loadings.column]][,
    ])
    loadings.data$rownames <- rownames(loadings.data)
    loadings.columns <- unique(c(x.column, y.column, colnames(loadings.data)))
    loadings.data <- loadings.data[, loadings.columns]
  } else {
    loadings.data <- NULL
  }
  if (is.null(ve) | !variance_percentage) {
    labs <- PC
  } else {
    ve <- ve[c(x, y)]
    labs <- paste0(PC, " (", round(ve * 100, 2), "%)")
  }
  xlab <- labs[1]
  ylab <- labs[2]
  p <- ggbiplot(plot.data = plot.data, loadings.data = loadings.data,
    xlab = xlab, ylab = ylab, ...)
  return(p)
}

#' @export
#' @rdname pca
autoplot.pcomp <- function(object,
  type = c("screeplot", "altscreeplot", "loadings", "correlations", "scores", "biplot"),
  choices = 1L:2L, name = deparse(substitute(object)), ar.length = 0.1,
  circle.col = "gray", col = "black", fill = "gray", scale = 1, aspect.ratio = 1,
  repel = FALSE, labels, title, xlab, ylab, ...) {
  type = match.arg(type)

  # Needed to avoid spurious note in R CMD check
  . <- NULL

  if (missing(title))
    title <- paste(name, type, sep = " - ")

  contribs <- paste0(names(object$sdev), " (",
    round((object$sdev^2/object$totdev^2) * 100, digits = 1), "%)")[choices]

  scores <- as.data.frame(object$scores[, choices])
  names(scores) <- c("x", "y")
  if (!missing(labels)) {
    if (length(labels) != nrow(scores))
      stop("You must provide a character vector of length ", nrow(scores),
        " for 'labels'")
    scores$labels <- labels
  } else {# Default labels are row numbers
    scores$labels <- 1:nrow(scores)
  }

  lims <- scale_axes(scores, aspect.ratio = aspect.ratio)

  if (!missing(col)) {
    if (length(col) != nrow(scores))
      stop("You must provide a vector of length ", nrow(scores), " for 'col'")
    scores$color <- col
    scores_formula <- y ~ x %col=% color %label=% labels
  } else {
    if (missing(labels)) {
      scores_formula <- y ~ x %label=% labels
    } else {
      scores_formula <- y ~ x %col=% labels %label=% labels
    }
  }

  res <- switch(type,
    screeplot = object %>.% # Classical screeplot
      tidy(., "pcs") %>.%
      chart(data = ., std.dev^2 ~ PC) +
      geom_col(col = col, fill = fill) +
      labs(y = "Variances", title = title),

    altscreeplot = object %>.% # screeplot represented by dots and lines
      tidy(., "pcs") %>.%
      chart(data = ., std.dev^2 ~ PC) +
      geom_line(col = col) +
      geom_point(col = "white", fill = col, size = 2, shape = 21, stroke = 3) +
      labs(y = "Variances", title = title),

    loadings = object %>.% # Plots of the variables
      tidy(., "variables") %>.%
      #spread(., key = PC, value = value) %>.%
      #rename_if(., is.numeric, function(x) paste0("PC", x)) %>.%
      # No dependencies to dplyr or tidyr => stats::reshape() & [,]/set_names
      as.data.frame(.) %>.% # reshape() does not work on tibbles!
      reshape(., idvar = "column", timevar = "PC", direction = "wide") %>.%
      .[, c(1, choices + 1)] %>.%
      set_names(., c("labels", "x", "y")) %>.%
      chart(data = ., y ~ x %xend=% 0 %yend=% 0 %label=% labels) +
      annotate("path", col = circle.col,
        x = cos(seq(0, 2*pi, length.out = 100)),
        y = sin(seq(0, 2*pi, length.out = 100))) +
      geom_hline(yintercept = 0, col = circle.col) +
      geom_vline(xintercept = 0, col = circle.col) +
      geom_segment(arrow = arrow(length = unit(ar.length, "inches"),
        ends = "first")) +
      geom_text_repel(hjust = "outward", vjust = "outward") +
      coord_fixed(ratio = 1) +
      labs(x = contribs[1], y = contribs[2], title = title),

    correlations = object %>.% # Correlations plot
      Correlation(.) %>.%
      as_tibble(., rownames = "labels") %>.%
      .[, c(1, choices + 1)] %>.%
      set_names(., c("labels", "x", "y")) %>.%
      chart(data = ., y ~ x %xend=% 0 %yend=% 0 %label=% labels) +
      annotate("path", col = circle.col,
        x = cos(seq(0, 2*pi, length.out = 100)),
        y = sin(seq(0, 2*pi, length.out = 100))) +
      geom_hline(yintercept = 0, col = circle.col) +
      geom_vline(xintercept = 0, col = circle.col) +
      geom_segment(arrow = arrow(length = unit(ar.length, "inches"),
        ends = "first")) +
      geom_text_repel(hjust = "outward", vjust = "outward") +
      coord_fixed(ratio = 1) +
      labs(x = contribs[1], y = contribs[2], title = title),

    scores = scores %>.% # Plot of the individuals
      chart(data = ., scores_formula) +
      geom_hline(yintercept = 0, col = circle.col) +
      geom_vline(xintercept = 0, col = circle.col) +
      coord_fixed(ratio = 1, xlim = lims$x, ylim = lims$y, expand = TRUE) +
      labs(x = contribs[1], y = contribs[2], title = title) +
      theme(legend.position = "none"),

    biplot = object %>.% # Biplot using ggfortify:::autoplot.prcomp
      as.prcomp(.) %>.%
      ggfortify_autoplot.prcomp(., x = choices[1], y = choices[2],
        scale = scale, size = -1, label = TRUE, loadings = TRUE,
        loadings.label = TRUE) +
      geom_hline(yintercept = 0, col = circle.col) +
      geom_vline(xintercept = 0, col = circle.col) +
      theme_sciviews() +
      labs(x = contribs[1], y = contribs[2], title = title),

    stop("Unrecognized type, must be 'screeplot', 'altscreeplot', loadings', 'correlations', 'scores' or 'biplot'")
  )

  if (type == "scores") {
    if (isTRUE(repel)) {
      res <- res + geom_point() + geom_text_repel() #ggrepel::geom_text_repel()
    } else {# Use text
      res <- res + geom_text()
    }
  }

  if (!missing(xlab))
    res <- res + xlab(xlab)
  if (!missing(ylab))
    res <- res + ylab(ylab)
  res
}

#' @export
#' @rdname pca
chart.pcomp <- function(data, choices = 1L:2L, name = deparse(substitute(data)),
..., type = NULL, env = parent.frame())
  autoplot.pcomp(data, choices = choices, name = name, ..., type = type, env = env)
class(chart.pcomp) <- c("function", "subsettable_type")

#' @export
#' @rdname pca
augment.princomp <- function(x, data = NULL, newdata, ...)
  # broom methods can be defined simply by converting into prcomp objects
  if (missing(newdata)) {
    augment(as.prcomp(x), data = data, ...)
  } else {
    augment(as.prcomp(x), data = data, newdata = newdata, ...)
  }

#' @export
#' @rdname pca
tidy.princomp <- function(x, matrix = "u", ...) {
  # broom methods can be defined simply by converting into prcomp objects
  tidy(as.prcomp(x), matrix = matrix, ...)
}

# There is no glance.prcomp() method

#' @export
#' @rdname pca
as.prcomp <- function(x, ...) {
  # broom implements only methods for prcomp objects, not princomp, while pcomp
  # is compatible with princomp... but prcomp is simpler. So, conversion is easy

  # Comparison of pcomp() -> as.prcomp() with prcomp() directly
  # Almost the same, only no rownames for x (is it important?)
  #iris_prcomp_pcomp <- as.prcomp(pcomp(iris[, -5], scale = TRUE))
  #iris_prcomp <- prcomp(iris[, -5], scale = TRUE)
  UseMethod("as.prcomp")
}

#' @export
#' @rdname pca
as.prcomp.default <- function(x, ...) {
  stop("No method to convert this object into a 'prcomp'")
}

#' @export
#' @rdname pca
as.prcomp.prcomp <- function(x, ...) {
  x
}

#' @export
#' @rdname pca
as.prcomp.princomp <- function(x, ...) {
  structure(list(sdev = as.numeric(x$sdev), rotation = unclass(x$loadings),
    center = x$center, scale = x$scale, x = as.matrix(x$scores)),
    class = "prcomp")
}
