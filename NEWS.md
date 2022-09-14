# exploreit 0.1.0

-   `pca()` and `chart()` for PCA added.

-   `mds()` and associated methods and functions (`shepard()`) added.

-   `k_means()`, `profile_k()` and associated methods added.

-   Methods `scale()` for data.frame, data.table and tibble's tbl_df.

# exploreit 0.1.0

-   Functions and methods for hierarchical clustering are added (`dissimilarity()` and `cluster()`).

# exploreit 0.0.0.9000

-   Added hex sticker (logo) with:

``` r
SciViews::R
p <- chart(data = iris, Petal.Length ~ Petal.Width %col=% Species) +
  geom_point() + geom_ellipse()
p <- p + theme_void() + hexSticker::theme_transparent()
p <- p + theme(legend.position = "none")
dir.create ("inst/figures", showWarnings = FALSE)
hexSticker::sticker(p, package = "exploreit", p_size = 9, s_x = 1, s_y = .75,
  s_width = 1.3, s_height = 0.9, h_fill = "seagreen4", h_color = "darkgreen",
  filename = "inst/figures/exploreit.png")
```

-   Added a `NEWS.md` file to track changes to the package.
