#' Clean theme for maps
#'
#' A clean theme that is good for displaying maps from
#' \code{\link[ggplot2]{geom_map}()}.
#'
#'
#' @family themes
#' @inheritParams ggplot2::theme_grey
#' @example
#' @importFrom ggplot2 %+replace%
#' @importFrom ggplot2 theme_grey
#' @export
#' @rdname theme_map
theme_map <- function(base_size = 9, base_family = "sans") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = grid::unit(0, "lines"),
          plot.background = element_blank(),
          legend.justification = c(0, 0),
          legend.position = c(0, 0))
}
