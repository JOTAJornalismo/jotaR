#' A [ggplot2] theme with opinionated aesthetics for diplaying maps at <https://jota.info>
#'
#' This theme uses 'Roboto Regular' as the default typoghraphy and white panel plot.
#'
#' Notice: You should [import_roboto]() first and also install the fonts on your
#' system before trying to use this theme.
#'
#' There is an option `jotaR.loadfonts` which -- if set to `TRUE` -- will
#' call `extrafont::loadfonts()` to register non-core fonts with R PDF & PostScript
#' devices. If you are running under Windows, the package calls the same function
#' to register non-core fonts with the Windows graphics device.
#'
#' @md
#' @section Why Roboto?:
#' It's free and has tolerable kerning pairs and multiple weights. It's also the default font type used at JOTA materials.
#' @md
#' @param ... map theme parameters
#'
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 margin
#' @importFrom ggplot2 theme_minimal
#' @export
#'
theme_jota_map <- function(...) {
  ggplot2::theme_minimal() +
    theme(
      text = element_text(family = "Roboto Regular", color = "#333333"),
      # remove all axes
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(family = "Roboto Bold", size = 20, hjust = 0.5),
      plot.subtitle = element_text(size = 16, hjust = 0.5, margin = margin(b = -0.1,t = -0.1,l = 2, unit = "cm"), debug = F),
      # captions
      plot.caption = element_text(size = 7, hjust = .5,margin = margin(t = 0.2, b = 0, unit = "cm")),
      # panel.grid.minor = element_line(color = NA, size = 0.2),
      # panel.grid.major = element_line(color = NA, size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = NA, color = NA),
      panel.background = element_rect(fill = NA, color = NA),
      legend.background = element_rect(fill = NA, color = NA),
      panel.border = element_blank(),
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # legend
      legend.position = "right",
      legend.box.margin = margin(.2, .2, .2, .2),
      legend.key.width = unit(0.22, units = "cm"),
      ...
    )
}


