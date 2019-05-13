# laranja - #f05741
# preto - #000000
# branco - #ffffff
# cinza escuro - #232323
# cinza claro - #edede9 (geralmente usado para fundos)
# 'areia' - #9e9c90 (geralmente usado para linhas pontilhadas)
# azul - #2ab2e3
# verde - #3fa674
# amarelo - #fed350


jota_palette <- c("#f05741", "#2ab2e3", "#3fa674", "#fed350")

#' A muted, qualitative color palette
#'
#' @export
#' @examples
#' library(scales)
#' scales::show_col(jota_pal()(4))
jota_pal <- function() { manual_pal(jota_palette) }

#' Discrete color & fill scales based on the ipsum palette
#'
#' See [jota_pal]().
#'
#' @md
#' @inheritDotParams ggplot2::discrete_scale -expand -position
#' @rdname scale_pub
#' @export
scale_colour_jota <- function(...) { discrete_scale("colour", "jota", jota_pal(), ...) }

#' @export
#' @rdname scale_pub
scale_color_jota <- scale_colour_jota

#' @export
#' @rdname scale_jota
scale_fill_jota <- function(...) { discrete_scale("fill", "jota", jota_pal(), ...) }



#' @md
#'
#'
#'
#'
#'
#'
#'
#'
#' @export
#' @rdname theme_jota
`theme_jota` <- function(
  base_family="Roboto", base_size = 11.5,
  plot_title_family=base_family, plot_title_size = 18,
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
  plot_margin = margin(30, 30, 30, 30),
  grid_col = "#9e9c90", grid = TRUE,
  axis_col = "#232323", axis = FALSE, ticks = FALSE) {

  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)

  ret <- ret + theme(legend.background=element_blank())
  ret <- ret + theme(legend.key=element_blank())

  if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + theme(panel.grid=element_line(color=grid_col, size=0.2, linetype = "dashed", inherit.blank = FALSE))
    ret <- ret + theme(panel.grid.major=element_line(color=grid_col, size=0.2, linetype = "dashed", inherit.blank = FALSE))
    ret <- ret + theme(panel.grid.minor=element_line(color=grid_col, size=0.15, linetype = "dashed", inherit.blank = FALSE))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y=element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x=element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y=element_blank())
    }

  } else {
    ret <- ret + theme(panel.grid=element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line=element_line(color=axis_col, size=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x=element_blank())
      } else {
        ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y=element_blank())
      } else {
        ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
      }
    } else {
      ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
      ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
    }
  } else {
    ret <- ret + theme(axis.line=element_blank())
  }

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)

  ret <- ret + theme(axis.text.x=element_text(size=axis_text_size, margin=margin(t=0)))
  ret <- ret + theme(axis.text.y=element_text(size=axis_text_size, margin=margin(r=0)))
  ret <- ret + theme(axis.title=element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + theme(axis.title.x=element_text(hjust=xj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y=element_text(hjust=yj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y.right=element_text(hjust=yj, size=axis_title_size, angle=90,
                                                     family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(strip.text=element_text(hjust=0, size=strip_text_size,
                                             face=strip_text_face, family=strip_text_family))
  ret <- ret + theme(panel.spacing=grid::unit(2, "lines"))
  ret <- ret + theme(plot.title=element_text(hjust=0, size=plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                             family=plot_title_family, face=plot_title_face))
  ret <- ret + theme(plot.subtitle=element_text(hjust=0, size=subtitle_size,
                                                margin=margin(b=subtitle_margin),
                                                family=subtitle_family, face=subtitle_face))
  ret <- ret + theme(plot.caption=element_text(hjust=1, size=caption_size,
                                               margin=margin(t=caption_margin),
                                               family=caption_family, face=caption_face))
  ret <- ret + theme(plot.margin=plot_margin)

  ret

}



import_roboto_condensed <- function() {

  rc_font_dir <- system.file("fonts", "roboto", package="rJOTA")

  suppressWarnings(suppressMessages(extrafont::font_import(rc_font_dir, prompt=FALSE)))

  message(
    sprintf(
      "You will likely need to install these fonts on your system as well.\n\nYou can find them in [%s]",
      rc_font_dir)
  )

}


#' @rdname Roboto
#' @md
#' @title Roboto Regular font name R variable aliases
#' @description `font_rc` == "`Roboto Regular`"
#' @format length 1 character vector
#' @export
font_rc <- "Roboto Regular"
