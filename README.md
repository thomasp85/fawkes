
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fawkes <a href='https://fawkes.data-imaginist.com'><img src='man/figures/logo.png' align="right" height="138.5" /></a>

<!-- badges: start -->

[![R build
status](https://github.com/thomasp85/fawkes/workflows/R-CMD-check/badge.svg)](https://github.com/thomasp85/fawkes/actions)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R-CMD-check](https://github.com/thomasp85/fawkes/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thomasp85/fawkes/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/thomasp85/fawkes/graph/badge.svg)](https://app.codecov.io/gh/thomasp85/fawkes)
<!-- badges: end -->

This package provides an R API to control the [AxiDraw pen
plotter](https://axidraw.com). Under the hood it interfaces with the
[AxiDraw Python API](https://axidraw.com/doc/py_api/) and provides more
or less the same possibilities. More interesting, though, is that it
also provides integration with the standard R plotting system through a
graphic device (`axi_dev()`) that translates plotting instructions from
e.g.  base and grid graphics to plotter movement.

## Installation

fawkes is pretty niche by its very nature and it is unlikely that it
will be submitted to CRAN. This should not be conflated with it not
being ready for use. The current version can be installed directly from
github using pak:

``` r
# install.packages('pak')
pak::pak('thomasp85/fawkes')
```

## Examples

It is difficult to showcase the use of a piece of software meant to
communicate with a pen plotter virtually. `axi_svg()` provides access to
the svg plotting capabilities build into the python API, and
`axi_manual()` wraps the interactive mode. More interesting from R is
the `axi_dev()` that opens up a graphic device that sends instructions
to the plotter. The device comes with a lot of batteries included and
will e.g. ensure stroking of the requested width by drawing multiple
offsetted lines, draw fill with hatching, ask for pen change when the
colour changes, and optimise the travel path to improve plotting speed.
All of this is impossible to show directly, but fawkes also provides
`ghost_dev()` that uses the exact same routines as `axi_dev()` but
collects the instructions and lets you preview them.

``` r
library(fawkes)
library(ggplot2)

p <- ggplot(mtcars) + 
  geom_point(aes(disp, mpg)) + 
  facet_wrap(~ gear) + 
  theme_bw(base_size = 6) + 
  theme(
    plot.background = element_blank(),
    panel.background = element_blank()
  )

gd <- ghost_dev('A6', portrait = FALSE, margins = 5, ignore_color = TRUE)
p
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
invisible(dev.off())

gd$preview()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

It is also possible to see the movement when the pen is raised

``` r
gd$preview(plot_air = TRUE)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

This makes it easy to quickly assess the efficacy of the automatic path
optimisation:

``` r
gd <- ghost_dev('A6', portrait = FALSE, margins = 5, ignore_color = TRUE,
                optimize_order = 'none')
p
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
#> Warning in value[[3L]](cond): wrong sign in 'by' argument
invisible(dev.off())

gd$preview(plot_air = TRUE)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

## Code of Conduct

Please note that the ‘fawkes’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
