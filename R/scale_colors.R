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
jota_pal <- function() {scales::manual_pal(jota_palette) }

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
