#' Multiple Factor Analysis (MFA)
#'
#' @description Analyze several groups of variables at once with supplementary
#'   groups of variables or individuals. Each group can be numeric, factor or
#'   contingency tables. Missing values are replaced by the column mean and
#'   missing values for factors are treated as an additional level. This is a
#'   formula interface to the [FactoMineR::MFA()] function.
#'
#' @param data A data frame
#' @param formula A formula that specifies the variables groups to consider
#'   (see details)
#' @param nd Number of dimensions kept in the results (by default, 5)
#' @param suprow A vector indicating the row indices for the supplemental
#'   individuals
#' @param ... Additional arguments to [FactoMineR::MFA()] or to the plot
#' @param graph If `TRUE`a graph is displayed (`FALSE` by default)
#' @param object An **MFA** object
#' @param type The type of plot to produce: `"screeplot"` or `"altscreeplot"`
#'   for two versions of the screeplot, `"loadings"`, `"scores"`, `"groups"`,
#'   `"axes"`, `"contingency"` or `"ellipses"` for the different views of the
#'   MFA.
#' @param choices Vector of two positive integers. The two axes to plot, by
#' default first and second axes.
#' @param name The name of the object (automatically defined by default)
#' @param col The color for the points representing the observations, black by
#'   default.
#' @param fill The color to fill bars, gray by default
#' @param title The title of the plot (optional, a reasonable default is used)
#' @param env The environment where to evaluate code, `parent.frame()` by
#'   default, which should not be changed unless you really know what you are
#'   doing!
#'
#' @details
#' The formula presents how the different columns of the data frame are grouped
#' and indicates the kind of sub-table they are and the name we give to them in
#' the analysis. So, a component of the formula for one group is
#' `n * kind %as% name` where `n` is the number of columns belonging to this
#' group, starting at column 1 for first group, `kind` is `std` for numeric
#' variables to be standardized and used as a PCA, `num` for numerical variables
#' to use as they are also as a PCA, `cnt` for counts in a contingency table to
#' be treated as a CA and `fct` for classical factors (categorical variables).
#' Finally, `name` is a (short) name you use to identify this group. The kind
#' may be omitted and it will be `std` by default. If `%as% name` is omitted, a
#' generic name (group1, group2, group3, ...) is used. The complete formula is
#' the addition of the different groups to include in the analysis and the
#' subtraction of the supplementary groups not included in the analysis, like
#' `~n1*std %as% gr1 - n2*fct %as% gr2 + n3*num %as% gr3`, with groups "gr1" and
#' "gr3" included in the analysis and group "gr2" as supplemental. The total
#' `n1 + n2 + n3` must equal the number of columns in the data frame.
#'
#' @note
#' The symbols for the groups are different in [mfa()] and [FactoMineR::MFA()]).
#' To avoid further confusion, the symbols use three letters here:
#' - `std` is the same as `s` in `MFA()`: "standardized" and is the default
#' - `num` stands here for "numeric", thus continuous variables `c` in `MFA()`
#' - `cnt` stands for "contingency" table and matches `f` in `MFA()`
#' - `fct` stands for "factor", thus qualitative variables `n` in `MFA()`
#'
#' @return An **MFA** object
#' @export
#'
#' @examples
#' # Same example as in {FactoMineR}
#' library(chart)
#' data(wine, package = "FactoMineR")
#' wine_mfa <- mfa(data = wine,
#'   ~ -2*fct %as% orig +5 %as% olf + 3 %as% vis + 10 %as% olfag + 9 %as% gust - 2 %as% ens)
#' wine_mfa
#' summary(wine_mfa)
#'
#' chart$scree(wine_mfa)
#' chart$altscree(wine_mfa)
#'
#' chart$loadings(wine_mfa)
#' chart$scores(wine_mfa)
#' chart$groups(wine_mfa)
#'
#' chart$axes(wine_mfa)
#' # No contingency group! chart$contingency(wine_mfa)
#' chart$ellipses(wine_mfa)
mfa <- function(data, formula, nd = 5, suprow = NA, ..., graph = FALSE) {
  # TODO: rework Call in the final MFA object
  if (is.na(suprow)) suprow <- NULL # MFA uses NULL instead of NA!
  if (!is_formula(formula))
    stop("'formula' must be a formula object")
  if (!is.null(f_lhs(formula)))
    stop("'formula cannot have left-hand side (must be ~ n1 * type...")
  params <- get_groups(list(expr = f_rhs(formula)), translate = TRUE)

  # data must be a data frame
  if (!inherits(data, "data.frame"))
    stop("'data' must be a data.frame")
  data <- as.data.frame(data) # No tibble or data.table!
  # Number of columns in  data must match specifications in the formula
  if (ncol(data) != sum(params$groups))
    stop("You must specify groups in 'formula' for all the columns in 'data'")

  # Call FactoMineR::MFA() with corresponding arguments
  MFA(base = data, group = params$groups, type = params$types,
    ind.sup = suprow, ncp = nd, name.group = params$names,
    num.group.sup = params$suppl, ..., graph = graph)
}

# x is a list with: expr, groups, types, names, suppl
# Note: this is a mess, I need to clean up this code!
# translate = TRUE converts our group kinds into FactoMineR's ones:
# std -> s, num -> c, cnt -> f and fct -> n
get_groups <- function(x, translate = FALSE) {
  items <- as.list(x$expr)
  # We should have here '+', '-' or something else in case of last expression
  if (items[[1]] == "+") {
    x$suppl <- c(FALSE, x$suppl) # Not a supplementary variable
    x$expr <- items[[2]] # Second item is next expression
    # Third item is the information for that group
    # (n, or n * type, or n * type %as% name)
    item <- as.list(items[[3]])
  } else if (items[[1]] == "-") {
    x$suppl <- c(TRUE, x$suppl) # A supplementary variable
    x$expr <- items[[2]] # Second item is next expression
    # Third item is the information for that group
    # (n, or n * type, or n * type %as% name)
    item <- as.list(items[[3]])
  } else {
    # Last expression
    x$expr <- NULL
    item <- items
  }
  if (length(item) < 3) { # either n, or +n or -n
    if (length(item) == 2) {
      x$suppl <- switch(as.character(item[[1]]),
        "-" = c(TRUE, x$suppl),
        "+" = c(FALSE, x$suppl),
        stop("Bad 'formula', see help"))
      n <- item[[2]]
    } else n <- item[[1]]
    if (!is.numeric(n))
      stop("Bad 'formula', see help")
    x$groups <- c(as.integer(n, x$groups))
    x$types <- c("std", x$types)
    x$names <- c(NA, x$names)
  } else if (item[[1]] == "*") { # n * type or n * type %as% name
    if (length(item[[2]]) > 2)
      stop("Bad 'formula', see help")
    if (length(item[[2]]) == 2) {
      x$suppl <- switch(as.character(item[[2]][[1]]),
        "-" = c(TRUE, x$suppl),
        "+" = c(FALSE, x$suppl),
        stop("Bad 'formula', see help"))
      n <- item[[2]][[2]]
    } else n <- item[[2]]
    if (!is.numeric(n))
      stop("Bad 'formula', see help")
    x$groups <- c(as.integer(n), x$groups)
    # Right-hand side after '*': type or type %as% name
    subitem <- as.list(item[[3]])
    if (subitem[[1]] == "%as%") { # type %as name
      x$types <- c(as.character(subitem[[2]]), x$types)
      x$names <- c(as.character(subitem[[3]]), x$names)
    } else if (length(subitem) == 1 && is.name(subitem[[1]])) { # type only
      x$types <- c(as.character(subitem[[1]]), x$types)
      x$names <- c(NA, x$names)
    } else stop("Bad 'formula', see help") # Error
  } else if (item[[1]] == "%as%") { # n %as% name
    if (length(item[[2]]) == 2) {
      x$suppl <- switch(as.character(item[[2]][[1]]),
        "-" = c(TRUE, x$suppl),
        "+" = c(FALSE, x$suppl),
        stop("Bad 'formula', see help"))
      n <- item[[2]][[2]]
    } else n <- item[[2]]
    if (!is.numeric(n))
      stop("Bad 'formula', see help")
    x$groups <- c(as.integer(n), x$groups)
    x$types <- c("std", x$types) # type by default (std)
    x$names <- c(as.character(item[[3]]), x$names)
  } else stop("Bad 'formula', see help") # Error
  # Is there another expression to evaluate?
  if (!is.null(x$expr)) {
    x <- get_groups(x, translate = FALSE)
  } else {
    # Get correct vector for suppl
    if (length(x$suppl) < length(x$groups))
      x$suppl <- c(FALSE, x$suppl)
    # We need the group indices for the supplementary variables instead
    x$suppl <- (1:length(x$groups))[x$suppl]
    if (!length(x$suppl)) x$suppl <- NULL
    # Fix names
    def_names <- paste("group", 1:length(x$groups), sep = ".")
    names <- x$names
    names[is.na(names)] <- def_names[is.na(names)]
    x$names <- names
  }
  # Check types
  if (!all(x$types %in% c("std", "num", "cnt", "fct")))
    stop("Wrong formula: kind must be std, num, cnt or fct only")
  # Do we translate?
  if (isTRUE(translate)) {
    tkind <- c(std = "s", num = "c", cnt = "f", fct = "n")
    x$types <- tkind[x$types]
  }
  x
}
#form <- ~ -4 %as% Environnement -24*cnt %as% Plancton +1*cnt +18*num
#get_groups(list(expr = rlang::f_rhs(form)))
#get_groups(list(expr = rlang::f_rhs(form)), translate = TRUE)

# TODO: methods for broom...

#' @export
#' @rdname mfa
autoplot.MFA <- function(object, type = c("screeplot", "altscreeplot",
"loadings", "scores", "groups", "axes", "contingency", "ellipses"),
choices = 1L:2L, name = deparse(substitute(object)), col = "black",
fill = "gray", title, ..., env) {
  type = match.arg(type)

  # Needed to avoid spurious message in R CMD Check
  . <- NULL

  if (missing(title))
    title <- paste(name, type, sep = " - ")

  res <- switch(type,
    screeplot = object %>.% # Classical screeplot
      tibble(eig = .$eig[, 1], PC = 1:nrow(.$eig)) %>.%
      chart(data = ., eig ~ PC) +
      geom_col(col = col, fill = fill) +
      labs(y = "Eigenvalues", title = title),

    altscreeplot = object %>.% # screeplot represented by dots and lines
      tibble(eig = .$eig[, 1], PC = 1:nrow(.$eig)) %>.%
      chart(data = ., eig ~ PC) +
      geom_line(col = col) +
      geom_point(col = "white", fill = col, size = 2, shape = 21, stroke = 3) +
      labs(y = "Eigenvalues", title = title),

    loadings = object %>.% # Plots of the variables
      plot(., axes = choices, choix = "var", title = title, ...),

    scores = object %>.% # Plot of the individuals
      plot(., axes = choices, choix = "ind", title = title, ...),

    groups = object %>.% # Plot of the groups
      plot(., axes = choices, choix = "group", title = title, ...),

    axes = object %>.% # Plot of the loadings for the various groups
      plot(., axes = choices, choix = "axes", title = title, ...),

    contingency = object %>.% # Plot of the correspondence analysis
      if (any(.$call$nature.group == "contingency")) {
        plot(., axes = choices, choix = "freq", title = title, ...)
      } else {
        stop("No contingency (cnt) group; Nothing to plot.")
      },

    ellipses = object %>.% # Plot of the individuals with ellipses
      {
        # TODO: a ggplot version of this plot
        opar <- trellis.par.get()
        theme_sciviews_lattice()
        on.exit(trellis.par.set(theme = opar, strict = 2))
        p <- plotellipses(., axes = choices, title = title, ...) #FactoMineR::plotellipses
        print(p)
        p
      },

    stop("Unrecognized type, must be 'screeplot', 'altscreeplot',
      'loadings', 'scores', 'groups', 'axes', 'contingency', or 'ellipses'")
  )
  if (inherits(res, "ggplot")) {
    res
  } else {
    invisible(res)
  }
}

#' @export
#' @rdname mfa
chart.MFA <- function(data, choices = 1L:2L, name = deparse(substitute(data)),
..., type = NULL, env = parent.frame())
  autoplot.MFA(data, choices = choices, name = name, ..., type = type, env = env)
class(chart.MFA) <- c("function", "subsettable_type")
