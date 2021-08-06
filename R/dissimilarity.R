# dist is really a dissimilarity matrix => we use dissimilarity() as in the
# {cluster} package, i.e., class is c("dissimilarity", "dist")
# TODO: also make a similarity object and convert between the two
# fun can be stats::dist, vegan::vegdist, vegan::designdist, cluster::daisy
# factoextra::get_dist and probably other dist-compatible functions
# Depending on method =, use either vegan::vegdist or stats::dist as default fun
#
# TODO: `[` by first transforming into a matrix with as.matrix()

#' Calculate a dissimilarity matrix
#'
#' @description Compute a distance matrix from all pairs of columns or rows in
#' a data frame, using a unified SciViews::R formula interface.
#'
#' @param data A data.frame, tibble or matrix.
#' @param formula A right-side only formula (`~ ...`) indicating which columns to keep in the data. The default one (`~ .`) keeps all columns.
#' @param subset An expression indicating which rows to keep from data.
#' @param method The distance (dissimilarity) method to use. By default, it is `"euclidean"`, but it can also be `"maximum"`, `"binary"`, `"minkowski"` from [stats::dist()], or `"bray"`, `"manhattan"`, `"canberra"`, `"clark"`, `"kulczynski"`, `"jaccard"`, `"gower"`, `"altGower"`, `"morisita"`, `"horm"`, `"mountfort"`, `"raup"`, `"binomial"`, `"chao"`, `"cao"`, `"mahalanobis"`, `"chisq"`, or `"chord"` from [vegan::vegdist()], or any other distance from the function you provide in `fun =`.
#' @param scale Do we scale (mean = 0, standard deviation = 1) the data before calculating the distance (`FALSE` by default)?
#' @param rownames.col In case the `data` object does not have row names (a `tibble` for instance), which column should be used for name of the rows?
#' @param transpose Do we transpose `data` first (to calculate distance between columns instead of rows)? By default, not (`FALSE`).
#' @param fun A function that does the calculation and return a `dist`-like object (similar to what [stats::dist()]) provides. If `NULL` (by default), [stats::dist()] or [vegan::vegdist()] is used, depending on `method =`. Note that both functions calculate `"canberra"` differently, and in this case, it is the [vegan::vegdist()] version that is used by default. Other compatible functions: [vegan::designdist()], [cluster::daisy()], [factoextra::get_dist()], and probably more.
#' @param ... Further parameters passed to the `fun =` (see its man page) for [dissimilarity()], or further arguments passed to methods.
#'
#' @return An S3 object of class `c("dissimilarity", "dist")`, thus inheriting from `dist`. A `dissimilarity` object is better displayed (specific `print()` method), and has also dedicated methods `labels()` (get line and column labels), `nobs()` (get number of observations, that is, number of lines or columns), `autoplot()` (generate a ggplot2 from the matrix) and `chart()` (generate a chart version of the ggplot2).
#' @export
#' @seealso [stats::dist()], [vegan::vegdist()], [vegan::designdist()], [cluster::daisy()], [factoextra::get_dist()]
#'
#' @examples
#' SciViews::R
#' read("iris", package = "datasets") %>.%
#'   select(., -species) -> iris_num  # Only numeric columns from iris
#' # Compare the 150 flowers and nicely print the result
#' dissimilarity(iris_num, method = "manhattan")
#' # Compare the measurements by transposing and scaling them first
#' iris_dist <- dissimilarity(iris_num, method = "euclidean",
#'   scale = TRUE, transpose = TRUE)
#' iris_dist
#' class(iris_dist)
#' labels(iris_dist)
#' nobs(iris_dist)
#' # specific plots
#' autoplot(iris_dist)
#' chart(iris_dist, gradient = list(low = "green", mid = "white", high = "red"))
dissimilarity <- function(data, formula = ~ ., subset = NULL,
method = "euclidean", scale = FALSE, rownames.col = "rowname",
transpose = FALSE, fun = NULL, ...) {
  # TODO: get more meaningful warnings and errors by replacing fun by actual
  # name of the function
  if (is.null(fun)) {# Default function depends on the chosen method
    if (method %in% c("maximum", "binary", "minkowski")) {
      fun <- stats::dist
    } else {
      fun <- vegan::vegdist # Note: euclidean/manhattan/canberra in both, but
      # we prioritize vegdist, and canberra is not calculated the same in dist!
    }
  }
  # We accept only formulas with right-hand side => length must be two
  if (length(formula) == 3)
    stop("The formula cannot have a left-hand term")

  # With matrices, we don't use rownames.col: rownames are already correctly set
  if (!is.matrix(data)) {# row names may be in a column (usual for tibbles)
    data <- as.data.frame(data)
    if (rownames.col %in% names(data)) {
      rownames(data) <- data[[rownames.col]]
      data[[rownames.col]] <- NULL
    } else {# rownames.col is NOT used
      rownames.col <- NULL
    }
    if (as.character(formula[2] != ".")) {
      # Subset the columns
      data <- model.frame(formula, data = data, subset = subset)
    } else if (!is.null(subset)) {
      data <- data[subset, ]
    }
  } else {# A matrix
    rownames.col <- NULL
    if (as.character(formula[2] != ".")) {
      # Subset the columns (and possibly the rows)
      if (is.null(subset)) {
        data <- data[, all.vars(formula)]
      } else {
        data <- data[subset, all.vars(formula)]
      }
    }
  }

  if (isTRUE(transpose))
    data <- t(data)

  # Arguments method =/metric = and stand = not always there
  if (!is.null(as.list(args(fun))$metric)) {# metric = instead of method =
    dst <- fun(data, metric = method, stand = scale, ...)
  } else if (isTRUE(scale)) {
    if (is.null(as.list(args(fun))$stand)) {# fun has no stand = argument
      data <- scale(data)
      dst <- fun(data, method = method, ...)
    } else {# We don't standardise ourself because there may be also qualitative
      # or binary data (like for cluster::daisy, for instance)
      dst <- fun(data, method = method, stand = scale, ...)
    }
  } else {# Just method = and scale = FALSE
    dst <- fun(data, method = method, ...)
  }
  attr(dst, "call") <- match.call()
  # Depending if it is a dist or dissimilarity object, the method is stored in
  # method or in Metric, but we use metric in our own version to avoid a clash
  # with the method item in cluster()/hclust() further on (hclust change it
  # into dist.method, but it is better to have the right name right now)
  attr(dst, "metric") <- method
  # dist or dissimilarity object use Labels, but we use labels everywhere else
  # including in cluster()/hclust()
  # So, we make sure labels is present (in hclust, it is labels anyway!)
  attr(dst, "labels") <- rownames(data)
  # Default values for Diag and Upper set to FALSE
  if (is.null(attr(dst, "Diag"))) attr(dst, "Diag") <- FALSE
  if (is.null(attr(dst, "Upper"))) attr(dst, "Upper") <- FALSE
  # Keep info about how raw data were transformed
  attr(dst, "rownames.col") <- rownames.col
  attr(dst, "transpose") <- transpose
  attr(dst, "scale") <- scale
  class(dst) <- unique(c("dissimilarity", class(dst)))
  dst
}

#' Convert a dist or matrix object into a dissimilarity object
#'
#' @description Create a `dissimilarity` matrix from an existing distance matrix as `dist` object (e.g., from [stats::dist()], or [vegan::vegdist()]), or from a similarly shaped `matrix`object.
#'
#' @param x An object to coerce into a `dissimilarity` object.
#' @param ... Further argument passed to the coercion method.
#'
#' @return A `dissimilarity` object.
#' @export
#' @seealso [dissimilarity()]
#'
#' @examples
#' SciViews::R
#' read("iris", package = "datasets") %>.%
#'   select(., -species) -> iris_num  # Only numeric columns from iris
#' # Construct a dist object
#' iris_dist <- dist(iris_num)
#' class(iris_dist)
#' # Convert it into a dissimilarity object
#' iris_dis <- as.dissimilarity(iris_dist)
#' class(iris_dis)
as.dissimilarity <- function(x, ...) {
  UseMethod("as.dissimilarity")
}

#' @export
#' @rdname as.dissimilarity
as_dissimilarity <- as.dissimilarity # Synonym

#' @export
#' @rdname as.dissimilarity
as.dissimilarity.matrix <- function(x, ...) {
  dst <- as.dist(x, ...)
  attr(dst, "call") <- match.call()
  attr(dst, "metric") <- attr(dst, "method") # Make sur metric is used
  class(dst) <- unique(c("dissimilarity", class(dst)))
  dst
}

#' @export
#' @rdname as.dissimilarity
as.dissimilarity.dist <- as.dissimilarity.matrix

#' @export
#' @rdname as.dissimilarity
as.dissimilarity.dissimilarity <- function(x, ...) {
  x # Nothing to do, it is already a dissimilarity object!
}

#' @export
#' @rdname dissimilarity
#' @param x,object A `dissimilarity` object
#' @param  digits.d Number of digits to print, by default, 3.
#' @param rownames.lab The name of the column containing the labels, by default `"labels"`.
print.dissimilarity <- function(x, digits.d = 3L, rownames.lab = "labels",
...) {
  # We want to print only the first few rows and columns
  mat <- as.matrix(x)
  mat <- format(round(mat, digits.d))
  diag(mat) <- ""
  mat[upper.tri(mat)] <- ""
  class(mat) <- c("dst", "matrix")
  tbl <- tibble::as_tibble(mat)
  #tbl <- tibble::add_column(tbl, {{rownames.lab}} = rownames(mat), .before = 1)
  # I prefer this
  tbl <- dplyr::bind_cols(
    as_tibble_col(rownames(mat), column_name = rownames.lab), tbl)
  tbl <- tbl[, -ncol(tbl)]
  more_info <- ""
  if (isTRUE(attr(x, "scale"))) {
    if (isTRUE(attr(x, "transpose"))) {
      more_info <- " (transposed then scaled data)"
    } else {# Only scaled
      more_info <- " (scaled data)"
    }
  } else {
    if (isTRUE(attr(x, "transpose")))
      more_info <- " (transposed data)"
  }
  cat("Dissimilarity matrix with metric: ", attr(x, "metric"),
    more_info, "\n", sep = "")
  print(tbl)
  invisible(x)
}

#' @export
#' @rdname dissimilarity
labels.dissimilarity <- function(object, ...) {
  labs <- attr(object, "labels")
  if (is.null(labs))
    labs <- attr(object, "Labels")
  labs
}

#' @export
#' @rdname dissimilarity
nobs.dissimilarity <- function(object, ...)
  attr(object, "Size")

#' @export
#' @rdname dissimilarity
#' @param order Do we reorder the lines and columns according to their resemblance (`TRUE` by default)?
#' @param show.labels Are the labels displayed on the axes (`TRUE` by default)?
#' @param lab.size Force the size of the labels (`NULL` by default for automatic size).
#' @param gradient The palette of color to use in the plot.
autoplot.dissimilarity <- function(object, order = TRUE, show.labels = TRUE,
lab.size = NULL, gradient = list(low = "blue", mid = "white", high = "red"),
...) {
  factoextra::fviz_dist(object, order = order, show_labels = show.labels,
    lab_size = lab.size, gradient = gradient)
}

#' @export
#' @rdname dissimilarity
#' @param type The type of plot. For the moment, only one plot is possible and the default value (`NULL`) should not be changed.
#' @param env The environment where to evaluate the formula. If you don't understand this, you probably don't have to touch this arguments.
chart.dissimilarity <- function(data, order = TRUE, show.labels = TRUE,
lab.size = NULL, gradient = list(low = "blue", mid = "white", high = "red"),
..., type = NULL, env = parent.frame()) {
  p <- autoplot(data, order = order, show_labels = show.labels,
    lab_size = lab.size, gradient = gradient, type = type, ...) +
    theme_sciviews()
  p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  if (show.labels) {
    p <- p + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x  = element_text(angle = 45, hjust = 1, size = lab.size),
      axis.text.y  = element_text(size = lab.size))
  } else {
    p <- p + theme(
      axis.text    = element_blank(),
      axis.ticks   = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank())
  }
  class(p) <- unique(c("Chart", class(p)))
  p
}
