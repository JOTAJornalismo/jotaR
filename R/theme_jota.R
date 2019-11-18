#' A [ggplot2] theme with opinionated aesthetics for <https://jota.info>
#'
#' This theme uses 'Roboto Regular' as the default typoghraphy and dashed line for grid plot.
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
#' It's free and has tolerable kerning pairs and multiple weights.
#'
#' @md
#' @param base_family,base_size base font family and size
#' @param plot_title_family,plot_title_face,plot_title_size,plot_title_margin plot tilte family, face, size and margin
#' @param subtitle_family,subtitle_face,subtitle_size plot subtitle family, face and size
#' @param subtitle_margin plot subtitle margin bottom (single numeric value)
#' @param strip_text_family,strip_text_face,strip_text_size facet label font family, face and size
#' @param caption_family,caption_face,caption_size,caption_margin plot caption family, face, size and margin
#' @param axis_title_family,axis_title_face,axis_title_size axis title font family, face and size
#' @param axis_title_just axis title font justificationk one of `[blmcrt]`
#' @param axis_text_size font size of axis text
#' @param plot_margin plot margin (specify with [ggplot2::margin])
#' @param grid_col grid color
#' @param grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param axis_col axis color
#' @param axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param ticks ticks if `TRUE` add ticks
#' @param panel_col plot background color, if `FALSE` background color is "white".
#' @export
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 margin
#' @examples \dontrun{
#' library(ggplot2)
#' library(dplyr)
#' data(Governismo)
#'
#' ggplot(Governismo, aes(x=D1, y=D2)) +
#'   geom_point() +
#'   labs(x="IDEOLOGIA (D1)", y="POSICIONAMENTO ECONÔMICO (D2)",
#'        title="Um exemplo de scatterplot",
#'        subtitle="Apoio ao texto da Reforma: Threshold 80% a 20%",
#'       caption="Fonte: https://jota.info") +
#'   theme_jota(grid=FALSE)
#'
#'
#' # seminal bar chart
#'
#' group_by(Governismo, Reeleito) %>%
#'  summarize(Indice = mean(Indice, na.rm=TRUE)) %>%
#'  mutate(Reeleito = ifelse(Reeleito==1, "Reeleito", "Novato")) %>%
#'  ggplot(aes(x=Reeleito, y=Indice)) +
#'  geom_col() +
#'  geom_text(aes(label=round(100*Indice,1)), nudge_y=.02) +
#'  labs(x="Status do parlamentar (Reeleitos vs Novatos)", y="Governismo (Indice)",
#'       title="Um exemplo de gráfico de barras",
#'       subtitle="Novatos compõem a principal força de apoio ao governo",
#'       caption="Fonte: https://jota.info") +
#'  theme_jota(grid="Y") +
#'  theme(axis.text.y=element_blank())
#' }
#'
#' @family themes
#' @export
#' @rdname theme_jota
`theme_jota` <- function(
  base_family="Roboto Regular", base_size = 11,
  plot_title_family = "Roboto Bold", plot_title_size = 18,
  plot_title_face="bold", plot_title_margin = 10,
  subtitle_family=if (.Platform$OS.type == "windows") "Roboto Regular" else "Arial",
  subtitle_size = 13,
  subtitle_face = "plain", subtitle_margin = 15,
  strip_text_family = base_family, strip_text_size = 12,
  strip_text_face = "plain",
  caption_family=if (.Platform$OS.type == "windows") "Roboto Regular" else "Arial",
  caption_size = 9,
  caption_face = "plain",
  caption_margin = 10,
  axis_text_size = base_size,
  axis_title_family = base_family,
  axis_title_size = 9,
  axis_title_face = "plain",
  axis_title_just = "rt",
  plot_margin = margin(5,5,3,5),
  grid_col ="#9e9c90" ,
  grid = TRUE,
  axis_col = "#232323",
  axis = FALSE,
  ticks = FALSE,
  panel_col=FALSE) {
# "#cbcbcb"
# "#F0F0F0"
  if (inherits(panel_col, "character")) {
  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size) +
  ggplot2::theme(plot.background = element_rect(linetype = 0, colour = NA,fill = panel_col)) +
  ggplot2::theme(legend.background = element_rect(linetype = 0,colour = NA))
  }

  else {
    ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size) +
     ggplot2::theme(legend.background=element_blank()) +
    ggplot2::theme(legend.key=element_blank())
  }



  if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + ggplot2::theme(panel.grid=element_line(color=grid_col, size=0.20, linetype = "dashed", inherit.blank = FALSE))
    ret <- ret + ggplot2::theme(panel.grid.major=element_line(color=grid_col, size=0.20, linetype = "dashed", inherit.blank = FALSE))
    ret <- ret + ggplot2::theme(panel.grid.minor=element_line(color=grid_col, size=0.10, linetype = "dashed", inherit.blank = FALSE))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.y=element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.x=element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.y=element_blank())
    }

  } else {
    ret <- ret + ggplot2::theme(panel.grid=element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + ggplot2::theme(axis.line=element_line(color=axis_col, size=0.20))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.x=element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.x=element_line(color=axis_col, size=0.20))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.y=element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.y=element_line(color=axis_col, size=0.20))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.line.x=element_line(color=axis_col, size=0.20))
      ret <- ret + ggplot2::theme(axis.line.y=element_line(color=axis_col, size=0.20))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.line=element_blank())
  }

  if (!ticks) {
    ret <- ret + ggplot2::theme(axis.ticks = element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.x = element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + ggplot2::theme(axis.ticks = element_line(color=axis_col, size=0.20))
    ret <- ret + ggplot2::theme(axis.ticks.x = element_line(color=axis_col, size=0.20))
    ret <- ret + ggplot2::theme(axis.ticks.y = element_line(color=axis_col, size=0.20))
    ret <- ret + ggplot2::theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0, l = 0, m = 0.5, c= 0.5, r = 1, t = 1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0, l = 0, m = 0.5, c= 0.5, r = 1, t = 1)

  ret <- ret + ggplot2::theme(axis.text.x = element_text(size=axis_text_size, margin=ggplot2::margin(t = 0)))
  ret <- ret + ggplot2::theme(axis.text.y = element_text(size=axis_text_size, margin= ggplot2::margin(r = 0)))
  ret <- ret + ggplot2::theme(axis.title = element_text(size = axis_title_size, family= axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.x = element_text(size = axis_title_size,
                                               family = axis_title_family, face= axis_title_face))
  ret <- ret + ggplot2::theme(axis.title.y = element_text(size = axis_title_size,
                                               family = axis_title_family, face = axis_title_face))
  ret <- ret + ggplot2::theme(axis.title.y.right = element_text(size = axis_title_size, angle = 90,
                                                     family = axis_title_family, face=axis_title_face))
  ret <- ret + ggplot2::theme(strip.text=element_text(hjust = 0, size = strip_text_size,
                                             face=strip_text_face, family = strip_text_family))
  ret <- ret + ggplot2::theme(panel.spacing=grid::unit(2, "lines"))
  ret <- ret + ggplot2::theme(plot.title = element_text(hjust = 0, size = plot_title_size, margin = ggplot2::margin(b=plot_title_margin),
                                             family = plot_title_family, face = plot_title_face))
  ret <- ret + ggplot2::theme(plot.subtitle = element_text(hjust = 0, size= subtitle_size,
                                                margin = ggplot2::margin(b= subtitle_margin),
                                                family = subtitle_family, face= subtitle_face))
  ret <- ret + ggplot2::theme(plot.caption=element_text(hjust = 1, size = caption_size,
                                               margin = ggplot2::margin(t = caption_margin),
                                               family = caption_family, face= caption_face))
  ret <- ret + ggplot2::theme(plot.margin = plot_margin)

  ret

}






#' @title Import roboto fonts
#' Make sure the roboto fonts are in your path
#' @md
#' @family fonts
#' @export
#' @rdname import_roboto
#' @export
#' @examples \dontrun{
#' import_roboto()
#' }
import_roboto <- function() {

  rr_font_dir <- system.file("fonts", "roboto", package = "jotaR")

  suppressWarnings(suppressMessages(extrafont::font_import(rr_font_dir, prompt=FALSE)))

  message(
    sprintf(
      "You will likely need to install these fonts on your system as well.\n\nYou can find them in [%s]",
      rr_font_dir)
  )

}



#' @rdname Roboto
#' @md
#' @title Roboto Regular font name R variable aliases
#' @description `font_rr` == "`Roboto Regular`"
#' @format length 1 character vector
#' @export
font_rr <- "Roboto Regular"



#' @rdname Roboto-Bold
#' @md
#' @title Roboto Regular font name R variable aliases
#' @description `font_rb` == "`Roboto Bold`"
#' @format length 1 character vector
#' @export
font_rr <- "Roboto Bold"




import_sans <- function() {

  sr_font_dir <- system.file("fonts", "sans", package="jotaR")

  suppressWarnings(suppressMessages(extrafont::font_import(sr_font_dir, prompt=FALSE)))

  message(
    sprintf(
      "You will likely need to install these fonts on your system as well.\n\nYou can find them in [%s]",
      sr_font_dir)
  )

}


#' @rdname Sans
#' @md
#' @title Sans Regular font name R variable aliases
#' @description `font_sr` == "`Sans Regular`"
#' @format length 1 character vector
#' @export
font_sr <- "Sans Regular"
