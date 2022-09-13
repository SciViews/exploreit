
# scale() is a generic function, but it does not provide a method for data
# frames. As such, data frames and tibbles are converted into matrices by the
# default method, which is not what we want

#' Scale a data frame (data.frame, data.table or tibble's tbl_df)
#'
#' @description Center or scale all variables in a data frame. This takes a
#' data frame and return an object of the same class.
#'
#' @param x A data frame
#' @param center Are the columns centered (mean = 0)?
#' @param scale Are the column scaled (standard deviation = 1)?
#'
#' @return An object of the same class as `x`.
#' @export
#' @name scale
#'
#' @examples
#' data(trees, package = "datasets")
#' colMeans(trees)
#' trees2 <- scale(trees)
#' head(trees2)
#' class(trees2)
#' colMeans(trees2)
scale.data.frame <- function(x, center = TRUE, scale = TRUE) {
  as.data.frame(scale(as.matrix(x)))
}

#' @export
#' @rdname scale
scale.tbl_df <- function(x, center = TRUE, scale = TRUE) {
  tibble::as_tibble(scale(as.matrix(x)))
}

#' @export
#' @rdname scale
scale.data.table <- function(x, center = TRUE, scale = TRUE) {
  data.table::as.data.table(scale(as.matrix(x)))
}
