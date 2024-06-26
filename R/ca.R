# In place of MASS::corresp(, nf = 2), we use ca::ca()

#' Correspondence Analysis (CA)
#'
#' @description `ca()` is a reexport of the function from the \{ca\} package, it
#'   offers a `ca(formula, data, ...)` interface. It is supplemented here with
#'   various `chart()` types.
#'
#' @param obj A formula or a data frame with numeric columns, or a matrix, or a
#' table or xtabs two-way contingency table, see [ca::ca()]. The formula version
#' allows to specify two categorical variables from a data frame as `~f1 + f2`.
#' The other versions analyze a two-way contingency table crossing two factors.
#' @param ... Further arguments from [ca::ca()] or for plot.
#' @param object A **pcomp** object
#' @param data Idem
#' @param type The type of plot to produce: `"screeplot"` or `"altscreeplot"`
#'   for two versions of the screeplot, or `"biplot"` for the CA biplot.
#' @param choices Vector of two positive integers. The two axes to plot, by
#' @param col The color for the points representing the observations, black by
#'   default.
#' @param fill The color to fill bars, gray by default
#' @param aspect.ratio height/width of the plot, 1 by default (for plots where
#'   the ratio height / width does matter)
#' @param repel Logical. Should repel be used to rearrange points labels?
#' `FALSE`by default
#' @param env The environment where to evaluate code, `parent.frame()` by
#'   default, which should not be changed unless you really know what you are
#'   doing!
#'
#' @return [pca()] produces a **ca** object.
#' @export
#'
#' @examples
#' library(chart)
#' data(caith, package = "MASS")
#' caith # A two-way contingency table
#' class(caith) # in a data frame
#' caith_ca <- ca(caith)
#' summary(caith_ca)
#'
#' chart$scree(caith_ca)
#' chart$altscree(caith_ca)
#'
#' chart$biplot(caith_ca)
ca <- ca::ca

# Not used yet!
#plot3d <- rgl::plot3d
#plot3d.ca <- ca:::plot3d.ca

# ca:::plot.ca() is not exported, so, this is a copy of it (+ .arrows)
.arrows <- function(..., angle = 15) {
  angles <- seq(1, angle, by = 2)
  for (ang in angles) arrows(..., angle = ang)
}

ca_plot.ca <- function(x, dim = c(1, 2), map = "symmetric",
what = c("all", "all"), mass = c(FALSE, FALSE), contrib = c("none", "none"),
col = c("blue", "red"), pch = c(16, 21, 17, 24), labels = c(2, 2),
arrows = c(FALSE, FALSE), lines = c(FALSE, FALSE),  lwd = 1, xlab = "_auto_",
ylab = "_auto_", col.lab = c("blue", "red"), ...) {
  obj <- x
  if (length(what) != 2) {
    what <- rep(what, length = 2)
  }
  if (length(mass) != 2) {
    mass <- rep(mass, length = 2)
  }
  if (length(contrib) != 2) {
    contrib <- rep(contrib, length = 2)
  }
  if (length(col) != 2) {
    col <- rep(col, length = 2)
  }
  if (length(labels) != 2) {
    labels <- rep(labels, length = 2)
  }
  if (length(pch) != 4) {
    pch <- rep(pch, length = 4)
  }
  if (length(lines) != 2) {
    lines <- rep(lines, length = 2)
  }
  if (length(col.lab) != 2) {
    col.lab <- rep(col.lab, length = 2)
  }
  if (!is.numeric(x$suprow)) {
    if (map == "colgab" | map == "colgreen") {
      if (what[1] != "none")
        what[1] <- "active"
    }
  }
  if (!is.numeric(x$supcol)) {
    if (map == "rowgab" | map == "rowgreen") {
      if (what[2] != "none")
        what[2] <- "active"
    }
  }
  if (min(dim) < 0) {
    swisign <- ifelse(dim < 0, -1, 1)
    dim.c <- dim(obj$rowcoord)[2]
    signmat <- diag(rep(swisign, length = dim.c))
    obj$rowcoord <- obj$rowcoord %*% signmat
    obj$colcoord <- obj$colcoord %*% signmat
    dim <- abs(dim)
  }
  K <- dim(obj$rowcoord)[2]
  I <- dim(obj$rowcoord)[1]
  J <- dim(obj$colcoord)[1]
  svF <- matrix(rep(obj$sv[1:K], I), I, K, byrow = TRUE)
  svG <- matrix(rep(obj$sv[1:K], J), J, K, byrow = TRUE)
  rpc <- obj$rowcoord * svF
  cpc <- obj$colcoord * svG
  symrpc <- obj$rowcoord * sqrt(svF)
  symcpc <- obj$colcoord * sqrt(svG)
  mt <- c("symmetric", "rowprincipal", "colprincipal", "symbiplot",
    "rowgab", "colgab", "rowgreen", "colgreen")
  mti <- 1:length(mt)
  mtlut <- list(symmetric = list(x = rpc, y = cpc), rowprincipal = list(x = rpc,
    y = obj$colcoord), colprincipal = list(x = obj$rowcoord,
      y = cpc), symbiplot = list(x = symrpc, y = symcpc), rowgab = list(x = rpc,
        y = obj$colcoord * obj$colmass), colgab = list(x = obj$rowcoord *
            obj$rowmass, y = cpc), rowgreen = list(x = rpc, y = obj$colcoord *
                sqrt(obj$colmass)), rowgreen = list(x = obj$rowcoord *
                    sqrt(obj$rowmass), y = cpc))
  x <- mtlut[[mti[mt == map]]][[1]]
  y <- mtlut[[mti[mt == map]]][[2]]
  x.names <- obj$rownames
  y.names <- obj$colnames
  indx <- dim(x)[1]
  indy <- dim(y)[1]
  pch.x <- rep(pch[1], dim(x)[1])
  pch.y <- rep(pch[3], dim(y)[1])
  pr <- c("none", "active", "passive", "all")
  pri <- 1:4
  if (is.na(obj$rowsup[1])) {
    sup.x <- NA
    act.x <- x
    xn.sup <- NA
    xn.act <- x.names
  } else {
    sup.x <- x[obj$rowsup, ]
    act.x <- x[-obj$rowsup, ]
    pch.x[obj$rowsup] <- pch[2]
    xn.sup <- x.names[obj$rowsup]
    xn.act <- x.names[-obj$rowsup]
  }
  if (is.na(obj$colsup[1])) {
    sup.y <- NA
    act.y <- y
    yn.sup <- NA
    yn.act <- y.names
  } else {
    sup.y <- y[obj$colsup, ]
    act.y <- y[-obj$colsup, ]
    pch.y[obj$colsup] <- pch[4]
    yn.sup <- y.names[obj$colsup]
    yn.act <- y.names[-obj$colsup]
  }
  prlut <- list(none = list(x = NA, y = NA), active = list(x = act.x,
    y = act.y), supplementary = list(x = sup.x, y = sup.y),
    all = list(x = x, y = y))
  nameslut <- list(none = list(x.names = NA, y.names = NA),
    active = list(x.names = xn.act, y.names = yn.act), supplementary =
      list(x.names = xn.sup,
      y.names = yn.sup), all = list(x.names = x.names,
        y.names = y.names))
  pchlut <- list(none = list(x.pch = NA, y.pch = NA), active =
      list(x.pch = rep(pch[1],
    dim(x)[1]), y.pch = rep(pch[3], dim(y)[1])), supplementary =
      list(x.pch = rep(pch[2],
      dim(x)[1]), y.pch = rep(pch[4], dim(y)[1])), all = list(x.pch = pch.x,
        y.pch = pch.y))
  x <- prlut[[pri[pr == what[1]]]][[1]]
  y <- prlut[[pri[pr == what[2]]]][[2]]
  x.names <- nameslut[[pri[pr == what[1]]]][[1]]
  y.names <- nameslut[[pri[pr == what[2]]]][[2]]
  x.pch <- pchlut[[pri[pr == what[1]]]][[1]]
  y.pch <- pchlut[[pri[pr == what[2]]]][[2]]
  if (is.matrix(x)) {
    x <- x[, dim]
  } else {
    x <- matrix(x[dim], ncol = length(dim), nrow = 1)
  }
  if (is.matrix(y)) {
    y <- y[, dim]
  } else {
    y <- matrix(y[dim], ncol = length(dim), nrow = 1)
  }
  if (mass[1]) {
    cex.x <- 0.5 + obj$rowmass^(1/3)/max(obj$rowmass^(1/3))
  } else {
    cex.x <- 1
  }
  if (mass[2]) {
    cex.y <- 0.5 + obj$colmass^(1/3)/max(obj$colmass^(1/3))
  } else {
    cex.y <- 1
  }
  nc0 <- 50
  cst <- 230
  col.x <- col[1]
  col.y <- col[2]
  if (contrib[1] == "relative") {
    cind <- obj$rowmass * (rpc[, dim[1]]^2 + rpc[, dim[2]]^2)/obj$rowinertia
    cb.x <- col2rgb(col[1])
    collut.x <- rgb(seq(cst, cb.x[1, 1], length = nc0), seq(cst,
      cb.x[2, 1], length = nc0), seq(cst, cb.x[3, 1], length = nc0),
      maxColorValue = 255)
    xtemp <- nc0 * (cind)
    col.x <- collut.x[xtemp]
  } else {
    if (contrib[1] == "absolute") {
      cind <- obj$rowmass * (rpc[, dim[1]]^2 + rpc[,
        dim[2]]^2)/(obj$sv[dim[1]]^2 + obj$sv[dim[2]]^2)
      cb.x <- col2rgb(col[1])
      p.x <- cb.x[, 1] + (cst - cb.x[, 1])/indx
      collut.x1 <- rgb(seq(cst, p.x[1], length = nc0/2),
        seq(cst, p.x[2], length = nc0/2), seq(cst, p.x[3],
          length = nc0/2), maxColorValue = 255)
      collut.x2 <- rgb(seq(p.x[1], cb.x[1, 1], length = nc0/2),
        seq(p.x[2], cb.x[2, 1], length = nc0/2), seq(p.x[3],
          cb.x[3, 1], length = nc0/2), maxColorValue = 255)
      collut.x <- c(collut.x1, collut.x2)
      xtemp <- nc0 * (cind)
      col.x <- collut.x[xtemp]
    }
  }
  if (contrib[2] == "relative") {
    cind <- obj$colmass * (cpc[, dim[1]]^2 + cpc[, dim[2]]^2)/obj$colinertia
    cb.y <- col2rgb(col[2])
    collut.y <- rgb(seq(cst, cb.y[1, 1], length = nc0), seq(cst,
      cb.y[2, 1], length = nc0), seq(cst, cb.y[3, 1], length = nc0),
      maxColorValue = 255)
    ytemp <- nc0 * cind
    col.y <- collut.y[ytemp]
  }
  if (contrib[2] == "absolute") {
    cind <- obj$colmass * (cpc[, dim[1]]^2 + cpc[, dim[2]]^2)/(obj$sv[dim[1]]^2 +
        obj$sv[dim[2]]^2)
    cb.y <- col2rgb(col[2])
    p.y <- cb.y[, 1] + (cst - cb.y[, 1])/indy
    collut.y1 <- rgb(seq(cst, p.y[1], length = nc0/2), seq(cst,
      p.y[2], length = nc0/2), seq(cst, p.y[3], length = nc0/2),
      maxColorValue = 255)
    collut.y2 <- rgb(seq(p.y[1], cb.y[1, 1], length = nc0/2),
      seq(p.y[2], cb.y[2, 1], length = nc0/2), seq(p.y[3],
        cb.y[3, 1], length = nc0/2), maxColorValue = 255)
    collut.y <- c(collut.y1, collut.y2)
    ytemp <- nc0 * cind
    col.y <- collut.y[ytemp]
  }
  q1 <- (1:dim(x)[1])
  q2 <- (1:dim(y)[1])
  l1 <- c(x[q1, 1], y[q2, 1])
  l1 <- l1[!is.na(l1)]
  l2 <- c(x[q1, 2], y[q2, 2])
  l2 <- l2[!is.na(l2)]
  if (length(l1) == 0)
    l1 <- c(-0.1, 0.1)
  if (length(l2) == 0)
    l2 <- c(-0.1, 0.1)
  lim1 <- range(l1) + c(-0.05, 0.05) * diff(range(l1))
  lim2 <- range(l2) + c(-0.05, 0.05) * diff(range(l2))
  pct <- round(100 * (obj$sv^2)/sum(obj$sv^2), 1)
  pct <- paste0(" (", pct[dim], "%)")
  if (xlab == "_auto_") {
    xlab = paste0("Dimension ", dim[1], pct[1])
  }
  if (ylab == "_auto_") {
    ylab = paste0("Dimension ", dim[2], pct[2])
  }
  pty.backup <- par()$pty
  plot(c(x[, 1], y[, 1]), c(x[, 2], y[, 2]), xlab = xlab, ylab = ylab,
    type = "n", axes = FALSE, asp = 1, ...)
  box()
  abline(h = 0, v = 0, lty = 3)
  axis(1)
  axis(2)
  if (!is.na(x[1]) & labels[1] != 1) {
    if (arrows[1]) {
      .arrows(rep(0, length(x[, 1])), rep(0, length(x[,
        1])), x[, 1], x[, 2], col = col.x, lwd = lwd,
        length = 0.1)
    } else {
      points(x[, 1], x[, 2], cex = cex.x, col = col.x,
        pch = x.pch)
    }
  }
  if (labels[1] > 0) {
    xoff1 <- if (labels[1] > 1)
      0.5 * strwidth(x.names, cex = 0.75) + 0.5 * strwidth("o",
        cex = 0.75)
    else 0
    xoff2 <- if (labels[1] > 1)
      0.5 * strheight(x.names, cex = 0.75) + 0.5 * strheight("o",
        cex = 0.75)
    else 0
    text(x[, 1] + xoff1, x[, 2] + xoff2, x.names, cex = 0.75,
      xpd = TRUE, col = col.lab[1])
  }
  if (!is.na(y[1]) & labels[2] != 1) {
    if (arrows[2]) {
      .arrows(rep(0, length(y[, 1])), rep(0, length(y[,
        1])), y[, 1], y[, 2], col = col.y, lwd = lwd,
        length = 0.1)
    } else {
      points(y[, 1], y[, 2], cex = cex.y, col = col.y,
        pch = y.pch)
    }
  }
  if (labels[2] > 0) {
    yoff1 <- if (labels[2] > 1)
      0.5 * strwidth(y.names, cex = 0.75) + 0.5 * strwidth("o",
        cex = 0.75)
    else 0
    yoff2 <- if (labels[2] > 1)
      0.5 * strheight(y.names, cex = 0.75) + 0.5 * strheight("o",
        cex = 0.75)
    else 0
    text(y[, 1] + yoff1, y[, 2] + yoff2, y.names, cex = 0.75,
      xpd = TRUE, col = col.lab[2])
  }
  if (lines[1])
    lines(x[order(x[, 1]), ], col = col.x, lwd = lwd)
  if (lines[2])
    lines(y[order(y[, 1]), ], col = col.y, lwd = lwd)
  par(pty = pty.backup)
  rownames(x) <- x.names
  colnames(x) <- paste0("Dim", dim)
  rownames(y) <- y.names
  colnames(y) <- paste0("Dim", dim)
  result <- list(rows = x, cols = y)
  invisible(result)
}

#' @export
#' @rdname ca
autoplot.ca <- function(object, choices = 1L:2L,
  type = c("screeplot", "altscreeplot", "biplot"), col = "black", fill = "gray",
  aspect.ratio = 1, repel = FALSE, ...) {
  type = match.arg(type)

  # This is to avoid suprious warning in R CMD Ckeck
  . <- NULL

  res <- switch(type,
    screeplot = object %>.% # Classical screeplot
      `[[`(., "sv") %>.%
      tibble(Dimension = 1:length(.), sv = .) %>.%
      chart(data = ., sv^2 ~ Dimension) +
      geom_col(col = col, fill = fill) +
      labs(y = "Inertia"),

    altscreeplot = object %>.% # screeplot represented by dots and lines
      `[[`(., "sv") %>.%
      tibble(Dimension = 1:length(.), sv = .) %>.%
      chart(data = ., sv^2 ~ Dimension) +
      geom_line(col = col) +
      geom_point(col = "white", fill = col, size = 2, shape = 21, stroke = 3) +
      labs(y = "Inertia"),

    biplot = {
      # We want to use the function plot.ca(), but without plotting the base
      # plot. So, we place it in a specific environment where all base plot
      # functions are fake and do nothing (we just want to collect points
      # coordinates at the end).
      env <- new.env()
      env$plot_ca <- ca_plot.ca # Copy of ca:::plot.ca() + .arrows() not exported
      environment(env$plot_ca) <- env
      env$plot <- function(...) NULL
      env$box <- function(...) NULL
      env$abline <- function(...) NULL
      env$axis <- function(...) NULL
      env$par <- function(...) NULL
      env$points <- function(...) NULL
      env$lines <- function(...) NULL
      env$.arrows <- function(...) NULL
      env$text <- function(...) NULL
      env$strwidth <- function(...) NULL
      env$strheight <- function(...) NULL

      contribs <- paste0("Dimension ", 1:length(object$sv), " (",
        round(object$sv^2 / sum(object$sv^2) * 100, 1), "%)")[choices]

      # There is an error in plot_ca(): in case rownames in object is NULL
      # -> calculate default rownames for our object
      if (is.null(object$rownames)) {
        object$rownames <- as.character(1:length(object$rowmass))
      }
      res <- env$plot_ca(object, dim = choices, ...)

      rows <- as.data.frame(res$rows)
      rows$Type <- "rows"
      rows$Labels <- object$rownames
      cols <- as.data.frame(res$cols)
      cols$Type <- "cols"
      cols$Labels <- object$colnames
      res <- rbind(rows, cols) #bind_rows(rows, cols)
      names(res) <- c("x", "y", "type", "labels")

      lims <- scale_axes(res, aspect.ratio = aspect.ratio)
      nudge <- (lims$x[2] - lims$x[1]) / 100

      res <- chart(data = res, y ~ x %col=% type %label=% labels) +
        geom_hline(yintercept = 0, col = "gray") +
        geom_vline(xintercept = 0, col = "gray") +
        coord_fixed(ratio = 1, xlim = lims$x, ylim = lims$y, expand = TRUE) +
        theme(legend.position = "none") +
        labs(x = contribs[1], y = contribs[2])

      if (isTRUE(repel)) {
        res <- res + geom_point() + geom_text_repel()
      } else {# Use text
        res <- res + geom_point() +
          geom_text(hjust = 0, vjust = 0, nudge_x = nudge, nudge_y = nudge)
      }
      res
    }
  )
  res
}

#' @export
#' @rdname ca
chart.ca <- function(data, choices = 1L:2L, ...,
  type = c("screeplot", "altscreeplot", "biplot"), env = parent.frame())
  autoplot.ca(data, choices = choices, ..., type = type, env = env)
class(chart.ca) <- c("function", "subsettable_type")
