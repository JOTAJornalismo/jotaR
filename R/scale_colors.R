# laranja - #f05741
# preto - #000000
# branco - #ffffff
# cinza escuro - #232323
# cinza claro - #edede9 (geralmente usado para fundos)
# 'areia' - #9e9c90 (geralmente usado para linhas pontilhadas)
# azul - #2ab2e3
# verde - #3fa674
# amarelo - #fed350

agree_disagree_palette <- c("#2AB2E3", "#F05741","#B0B0B0")

#' A muted, qualitative color palette
#'
#' @export
#' @examples
#' library(scales)
#' scales::show_col(agree_disagree_pal()(4))
agree_disagree_pal <- function() {scales::manual_pal(agree_disagree_palette) }

#' Discrete color & fill scales based on the agree, disagree DK categories
#'
#' See [agree_disagree_pal]().
#'
#' @md
#' @importFrom ggplot2 discrete_scale
#' @inheritDotParams ggplot2::discrete_scale -expand -position
#' @rdname scale_agree_disagree
#' @export
scale_colour_agree_disagree <- function(...) { discrete_scale("colour", "agree_disagree", agree_disagree_pal(), ...) }

#' @export
#' @rdname scale_agree_disagree
scale_color_agree_disagree <- scale_colour_agree_disagree

#' @export
#' @rdname scale_agree_disagree
scale_fill_agree_disagree <- function(...) { discrete_scale("fill", "agree_disagree", agree_disagree_pal(), ...) }






idealpoints_palette <- c("#F05741", "#2AB2E3","#B0B0B0")

jota_palette <- c("#F05741", "#2AB2E3", "#3FA674", "#FED350")

#' A muted, qualitative color palette
#'
#' @export
#' @examples
#' library(scales)
#' scales::show_col(idealpoints_pal()(4))
idealpoints_pal <- function() {scales::manual_pal(idealpoints_palette) }

#' Discrete color & fill scales based on the ideal points palette
#'
#' See [idealpoints_pal]().
#'
#' @md
#' @importFrom ggplot2 discrete_scale
#' @inheritDotParams ggplot2::discrete_scale -expand -position
#' @rdname scale_idealpoints
#' @export
scale_colour_idealpoints <- function(...) { discrete_scale("colour", "idealpoints", idealpoints_pal(), ...) }

#' @export
#' @rdname scale_idealpoints
scale_color_idealpoints <- scale_colour_idealpoints

#' @export
#' @rdname scale_idealpoints
scale_fill_idealpoints <- function(...) { discrete_scale("fill", "idealpoints", idealpoints_pal(), ...) }






#' A muted, qualitative color palette
#'
#' @export
#' @examples
#' library(scales)
#' scales::show_col(jota_pal()(4))
jota_pal <- function() {scales::manual_pal(jota_palette) }

#' Discrete color & fill scales based on the JOTA palette
#'
#' See [jota_pal]().
#'
#' @md
#' @importFrom ggplot2 discrete_scale
#' @inheritDotParams ggplot2::discrete_scale -expand -position
#' @rdname scale_jota
#' @export
scale_colour_jota <- function(...) { discrete_scale("colour", "jota", jota_pal(), ...) }

#' @export
#' @rdname scale_jota
scale_color_jota <- scale_colour_jota

#' @export
#' @rdname scale_jota
scale_fill_jota <- function(...) { discrete_scale("fill", "jota", jota_pal(), ...) }
