#' A [ggplot2] theme with opinionated aesthetics for <https://jota.info>
#'
#'
#' @param base_family,base_size base font family and size
#' @param horizontal \code{logical} horizontal axis lines?
#' @param dark_panel \code{logical} darker background for panel?
#'
#' @examples \dontrun{
#' library(ggplot2)
#' library(dplyr)
#' data(Governismo)
#'
#' ggplot(Governismo, aes(x=D1, y=D2)) +
#'   geom_point() +
#'   labs(x="IDEOLOGIA (D1)", y="POSICIONAMENTO ECONÔMICO (D2)"
#'        title="Um exemplo de scatterplot",
#'        subtitle="Apoio ao texto da Reforma: Threshold 80% a 20%",
#'       caption="Fonte: https://jota.info") +
#'   theme_horizontal()
#'  }
#'
#' @inheritParams ggplot2::theme_grey
#'
#' @family themes
#' @export
#' @importFrom ggplot2 theme_grey
#' @export
#' @rdname theme_horizontal
theme_horizontal <- function(base_size = 10, base_family = "sans",
                            horizontal = TRUE, dark_panel = FALSE) {
  ret <-
    ggplot2::theme_grey(base_size = base_size, base_family = base_family) +
    ggplot2::theme(line = element_line(colour = "black"),
          rect = element_rect(fill = NA, colour = NA, linetype = 1),
          text = element_text(colour = "black"),
          ## Axis
          axis.line = element_line(size = rel(0.8)),
          axis.line.y = element_blank(),
          axis.text = element_text(size = rel(1)),
          axis.text.x = element_text(vjust = 0,
                                     margin = ggplot2::margin(t = base_size,
                                                     unit = "pt")),
          axis.text.y = element_text(hjust = 0,
                                     margin = ggplot2::margin(r = base_size,
                                                     unit = "pt")),
          ## I cannot figure out how to get ggplot to do 2 levels of ticks
          ## axis.ticks.margin = grid::unit(3 / 72, "in"),
          axis.ticks = element_line(),
          axis.ticks.y = element_blank(),
          axis.title = element_text(size = rel(1)),
          axis.title.x = element_text(),
          axis.title.y = element_text(angle = 90),
          # axis.ticks.length = grid::unit(-1/32, "in"),
          axis.ticks.length = grid::unit( -base_size * 0.5, "points"),
          legend.background = element_rect(linetype = 0,colour = NA),
          legend.spacing = grid::unit(base_size * 1.5, "points"),
          legend.key = element_rect(linetype = 0),
          legend.key.size = grid::unit(1.2, "lines"),
          legend.key.height = NULL,
          legend.key.width = NULL,
          legend.text = element_text(size = rel(1.25)),
          legend.text.align = NULL,
          legend.title = element_text(size = rel(1),  hjust = 0),
          legend.title.align = NULL,
          legend.position = "top",
          legend.direction = NULL,
          legend.justification = "center",
          ## colour=NA, linetype=0),
          ## Economist only uses vertical lines
          panel.background = element_rect(linetype = 0),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = "white", size = rel(1.75)),
          panel.grid.minor = element_blank(),
          panel.spacing = grid::unit(0.25, "lines"),
          strip.background = element_rect(fill = "#F0F0F0",
                                          colour = NA, linetype = 0),
          strip.text = element_text(size = rel(1.25)),
          strip.text.x = element_text(),
          strip.text.y = element_text(angle = -90),
          plot.background = element_rect(fill = "#F0F0F0",
                                         colour = NA),
          plot.title = element_text(size = rel(1.5),
                                    hjust = 0, face = "bold"),
          plot.margin = grid::unit(c(6, 5, 6, 5) * 2, "points"),
          complete = TRUE)
  if (horizontal) {
    ret <- ret + theme(panel.grid.major.x = element_blank())
  } else {
    ret <- ret + theme(panel.grid.major.y = element_blank())
  }
  if (dark_panel == TRUE) {
    ret <- ret + theme(panel.background =
                         element_rect(fill =
                                        unname("#D2D2D2")),
                       strip.background =
                         element_rect(fill =
                                        unname("#D2D2D2")))
  }
  ret
}
NULL
