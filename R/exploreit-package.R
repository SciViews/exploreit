#' Exploratory Data Analysis for 'SciViews::R'
#'
#' Multivariate analysis and data exploration for 'SciViews::R'. PCA, CA, MFA,
#' K-Means clustering, hierarchical clustering, MDS...
#'
#' @section Important functions:
#'
#' - [pca()] for Principal Component Analysis (PCA)
#' - [ca()] for Correspondence Analysis (CA)
#' - [mfa()] for Multiple Factor Analysis (MFA)
#' - [k_means()] for K-Means clustering
#' - [dissimilarity] for computing dissimilarity (distance) matrices
#' - [cluster()] for Hierarchical clustering
#' - [mds()] for metric and Non-metric MultiDimensional Scaling (MDS, NMDS)
#'
#' @docType package
#' @name exploreit-package

## usethis namespace: start
#' @importFrom stats as.dendrogram as.dist as.hclust cmdscale cor cutree kmeans
#'   nobs model.frame predict reshape
#' @importFrom fastcluster hclust
#' @importFrom tibble tibble add_column as_tibble as_tibble_col
#' @importFrom data.table as.data.table
#' @importFrom graphics abline arrows axis box lines par points strheight
#'   strwidth symbols text
#' @importFrom grid arrow unit
#' @importFrom grDevices col2rgb rgb
#' @importFrom utils str
#' @importFrom chart chart f_aes theme_sciviews theme_sciviews_lattice
#' @importFrom lattice trellis.par.get trellis.par.set
#' @importFrom ggplot2 aes annotate autoplot coord_fixed coord_flip coord_polar
#'   element_blank element_text expansion fortify geom_col geom_hline geom_line
#'   geom_point geom_segment geom_step geom_text geom_vline ggplot labs
#'   scale_x_continuous scale_x_reverse scale_y_continuous scale_y_reverse theme
#' @importFrom FactoMineR MFA plotellipses
#' @importFrom factoextra fviz_nbclust
#' @importFrom broom augment glance tidy
#' @importFrom vegan goodness metaMDS monoMDS vegdist wcmdscale
#' @importFrom MASS isoMDS sammon Shepard
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggfortify ggbiplot
#' @importFrom SciViews Correlation pcomp
#' @importFrom rlang is_formula f_lhs f_rhs set_names
#' @importFrom svFlow %>.%
#' @importFrom ca ca
## usethis namespace: end
NULL
