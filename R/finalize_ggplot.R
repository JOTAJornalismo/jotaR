utils::globalVariables(c("get_legend", ".collapse"))

#' @title Arrange Multiple ggplots
#'
#' @description Arrange multiple ggplots on the same page. Wrapper around
#' \code{\link[cowplot]. Can arrange multiple ggplots over multiple pages,
#' compared to the standard \code{\link[cowplot]}. Can also create a
#' common unique legend for multiple plots.
#'
#' @param ... list of plots to be arranged into the grid. The plots can be
#'   either ggplot2 plot objects or arbitrary gtables.
#' @param plotlist (optional) list of plots to display.
#' @param ncol (optional) number of columns in the plot grid.
#' @param nrow (optional) number of rows in the plot grid.
#' @param labels (optional) list of labels to be added to the plots. You can
#'   also set labels="AUTO" to auto-generate upper-case labels or labels="auto"
#'   to auto-generate lower-case labels.
#' @param font.label a list of arguments for customizing labels. Allowed values
#'   are the combination of the following elements: size (e.g.: 14), face (e.g.:
#'   "plain", "bold", "italic", "bold.italic"), color (e.g.: "red") and family.
#'   For example font.label = list(size = 14, face = "bold", color ="red").
#' @param label.x (optional) Single value or vector of x positions for plot
#'   labels, relative to each subplot. Defaults to 0 for all labels. (Each label
#'   is placed all the way to the left of each plot.)
#' @param label.y (optional) Single value or vector of y positions for plot
#'   labels, relative to each subplot. Defaults to 1 for all labels. (Each label
#'   is placed all the way to the top of each plot.)
#' @param widths (optional) numerical vector of relative columns widths. For
#'   example, in a two-column grid, widths = c(2, 1) would make the first column
#'   twice as wide as the second column.
#' @param hjust A value that controls horizontal justification.
#' @param vjust A value that controls vertical justification.
#' @param align A value that controls aligment, one of ("none", "h", "v", "hv").
#' @param heights same as \code{widths} but for column heights.
#' @param legend character specifying legend position. Allowed values are one of
#' c("top", "bottom", "left", "right", "none"). To remove the legend use
#' legend = "none".
#' @param common.legend logical value. Default is FALSE. If TRUE, a common
#'   unique legend will be created for arranged plots.
#' @return return an object of class \code{arrange_ggplot}, which is a ggplot or a
#'   list of ggplot.
#'
#' @importFrom ggplot2 theme
#' @export
arrange_ggplot <- function(..., plotlist = NULL, ncol = NULL, nrow = NULL,
                      labels = NULL, label.x = 0, label.y = 1, hjust = -0.5,
                      vjust = 1.5, font.label = list(size = 14, color = "black", face = "bold", family = NULL),
                      align = c("none", "h", "v", "hv"),
                      widths = 1, heights = 1,
                      legend = NULL, common.legend = FALSE )
{

  plots <- c(list(...), plotlist)
  align <- match.arg(align)
  nb.plots <- length(plots)
  page.layout <- .get_layout(ncol, nrow, nb.plots)
  ncol <- page.layout$ncol
  nrow <- page.layout$nrow
  nb.plots.per.page <- .nbplots_per_page(ncol, nrow)

  if(is.null(legend) & common.legend)
    legend <- "top"
  legend <- .check_legend(legend)
  if(!is.null(legend))
    plots <- purrr::map(
      plots,
      function(x) {if(!is.null(x)) x + theme(legend.position = legend) else x}
    )

  leg <- NULL
  if(common.legend){
    leg <- get_legend(plots)
    plots <- purrr::map(
      plots,
      function(x) {if(!is.null(x)) x + theme(legend.position = "none") else x}
    )
  }

  # Split plots over multiple pages
  if(nb.plots > nb.plots.per.page){
    plots <- split(plots, ceiling(seq_along(plots)/nb.plots.per.page))
  }
  # One unique page
  else plots <- list(plots)

  # label arguments
  .lab <- .update_label_pms(font.label, label.x = label.x, label.y = label.y,
                            hjust = hjust, vjust = vjust)

  res <- purrr::map(plots, .plot_grid,
                    ncol = ncol, nrow = nrow, labels = labels,
                    label_size = .lab$size, label_fontfamily = .lab$family,
                    label_fontface = .lab$face, label_colour = .lab$color,
                    label_x = .lab$label.x, label_y = .lab$label.y,
                    hjust = .lab$hjust, vjust = .lab$vjust, align = align,
                    rel_widths = widths, rel_heights = heights,
                    legend = legend, common.legend.grob = leg
  )




  if(length(res) == 1) res <- res[[1]]

  class(res) <- c(class(res), "ggarrange")
  res
}



.get_layout <- function(ncol, nrow, nb.plots){
  if(!is.null(ncol) & !is.null(nrow)){}
  else if(!is.null(ncol)){
    if(ncol == 1) nrow = nb.plots
  }
  else if(!is.null(nrow)){
    if(nrow == 1) ncol = nb.plots
  }
  list(ncol = ncol, nrow = nrow)
}

# Compute number of plots per page
.nbplots_per_page <- function(ncol = NULL, nrow = NULL){

  if(!is.null(ncol) & !is.null(nrow))
    ncol * nrow
  else if(!is.null(ncol))
    ncol
  else if(!is.null(nrow))
    nrow
  else Inf
}


#' @importFrom grid unit
#'
.plot_grid <- function(plotlist, legend = "top", common.legend.grob = NULL,  ... ){


  res <- cowplot::plot_grid(plotlist = plotlist, ...)
  if(is.null(common.legend.grob)) return(res)
  else {
    leg <- common.legend.grob
    lheight <- sum(leg$height)
    lwidth <- sum(leg$width)
  }

  arrangeGrob <- gridExtra::arrangeGrob
  unit.c <- grid::unit.c
  .unit <- grid::unit(1, "npc")

  res <- switch(legend,
                top = arrangeGrob(leg, res, ncol = 1,
                                  heights = unit.c(lheight, .unit - lheight)),
                bottom = arrangeGrob(res, leg, ncol = 1,
                                     heights = unit.c(unit(1, "npc") - lheight, lheight)),
                left = arrangeGrob(leg, res, ncol = 2,
                                   widths = unit.c(lwidth, .unit - lwidth)),
                right = arrangeGrob(res, leg, ncol = 2,
                                    widths = unit.c(.unit - lwidth, lwidth))
  )

  p <- cowplot::ggdraw() + cowplot::draw_grob(grid::grobTree(res))
  p

}


# update label parameters for cowplot::plot_grid()
.update_label_pms <- function(font.label,
                              label.x = 0, label.y = 1, hjust = -0.5, vjust = 1.5)
{

  .font <- list(size = 14, color = "black", face = "bold", family = NULL)
  new.font.names <- names(font.label)
  for(i in new.font.names) .font[[i]] <- font.label[[i]]

  pms <- .font
  list(
    size = pms$size,
    family = pms$family,
    face = pms$face,
    color = pms$color,
    label.x = label.x, label.y = label.y,
    hjust = hjust, vjust = vjust
  )
}




# Check legend argument
.check_legend <- function(legend){

  allowed.values <- c("top", "bottom", "left", "right", "none")

  if(is.null(legend) | is.numeric(legend))
    return(legend)
  else if(is.logical(legend)){
    if(legend) legend <- "top"
    else legend <- "none"
  }
  else if(is.character(legend)){
    legend <- legend[1]
    if(!legend %in% allowed.values)
      stop("Argument legend should be one of ", .collapse(allowed.values, sep = ", "))
  }
  return (legend)
}




#' @title Save ggplot2 objetc
#'
#' @description A wrapper function for the ggplot2 ggsave with custom parameters.
#' @param plot The plot object (ggplot2)
#' @param save_filepath Exact filepath that you want the plot to be saved to
#' @param width Width in pixels that you want to save your chart to
#' @param height Height in pixels that you want to save your chart to
#' @importFrom grid grid.draw
#'
#' @export
save_ggplot <- function(plot, width, height, save_filepath) {
  grid::grid.draw(plot)
  #save it
  ggplot2::ggsave(filename = save_filepath, plot = plot, width = (width/72), height = (height/72))
}


#Left align text
left_align <- function(plot_name, pieces){
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  return(grob)
}

right_align <- function(plot_name, pieces){
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  return(grob)
}



#' @importFrom grid linesGrob
#' @importFrom grid textGrob
#' @importFrom grid rasterGrob
#' @importFrom grid gpar
#' @importFrom png readPNG
# @importFrom rsvg rsvg
#'
create_footer640x450 <- function(source, image_path) {
  #Make the footer
  footer <- grid::grobTree(#grid::linesGrob(x = grid::unit(c(0, .10), "npc"), y = grid::unit(16, "npc"), gp = grid::gpar(col = 'red', lwd = 7, lineend = "square")),
                          # grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(1.05, "npc"), gp = grid::gpar(col = 'lightgrey', lwd = 2.5)),
                           grid::textGrob(source,
                                          x = 0.010, hjust = 0, vjust = 0.450, gp = grid::gpar(fontsize=10)),
                           grid::rasterGrob(png::readPNG(image_path), y = 0.600, x = 0.940, interpolate = TRUE))
  return(footer)
}




#' @importFrom grid linesGrob
#' @importFrom grid textGrob
#' @importFrom grid rasterGrob
#' @importFrom grid gpar
#' @importFrom png readPNG
# @importFrom rsvg rsvg
#'
create_footer593x367 <- function(source, image_path) {
  #Make the footer
  footer <- grid::grobTree(# grid::linesGrob(x = grid::unit(c(0, .10), "npc"), y = grid::unit(12, "npc"), gp = grid::gpar(col = 'red', lwd = 7)),
                           # grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(1.05, "npc"), gp = grid::gpar(col = 'lightgrey', lwd = 2.5)),
                           grid::textGrob(source,
                                          x = 0.010, hjust = 0, vjust = 0.450, gp = grid::gpar(fontsize=10)),
                           grid::rasterGrob(png::readPNG(image_path),y = 0.600, x = 0.938, interpolate = TRUE))
  return(footer)
}





#' @title Arrange alignment and save JOTA ggplot chart
#'
#' Running this function will save your plot with the correct guidelines for publication for a BBC News graphic.
#' It will left align your title, subtitle and source, add the JOTA logo at the bottom right and save it to your specified location.
#' @param plot The variable name of the plot you have created that you want to format and save
#' @param source The text you want to come after the text 'Source:' in the bottom left hand side of your side
#' @param save_filepath Exact filepath that you want the plot to be saved to
#' @param width_pixels Width in pixels that you want to save your chart to - defaults to 640
#' @param height_pixels Height in pixels that you want to save your chart to - defaults to 450
#' @param image_path File path for the logo image you want to use in the right hand side of your chart,
#'  which needs to be a PNG file - defaults to JOTA logo image that sits within the data folder of your package
#' @return (Invisibly) an updated ggplot object.

#' @keywords finalize_ggplot
#' @examples
#' \dontrun{
#' library(ggplot2)
#' data(Governismo)
#'
#' p1 = ggplot(Governismo, aes(x=D1, y=D2)) +
#'  geom_point() +
#'  labs(x="IDEOLOGIA (D1)", y="POSICIONAMENTO ECONÔMICO (D2)",
#'       title="Um exemplo de scatterplot",
#'       subtitle="Apoio ao texto da Reforma: Threshold 80% a 20%") +
#'  theme_jota(grid=FALSE)
#'
#'
#' finalize_ggplot(plot = p1,
#' source = "https://jota.info",
#' # save_filepath = "filename_that_my_plot_should_be_saved_to-nc.png",
#' width_pixels = 640,
#' height_pixels = 450,
#' image_path = "image.png"
#' )}
#'
#' @export
finalize_ggplot <- function(plot,
                          source = "",
                          save_filepath=file.path(Sys.getenv("TMPDIR"), "tmp-nc.png"),
                          width_pixels=640,
                          height_pixels=450,
                          image_path = file.path(system.file( "figs", package = "jotaR"), "jotalogo.png")) {

  footer <- create_footer640x450(source, image_path)

  #Draw your left-aligned grid
  plot_left_aligned <- left_align(plot, c("subtitle", "title", "caption"))
  plot <- arrange_ggplot(plot_left_aligned, footer,
                                 ncol = 1, nrow = 2,
                                 heights = c(1, 0.065/(height_pixels/450)))
  # print(paste("Saving to", save_filepath))
  save_ggplot(plot, width_pixels, height_pixels, save_filepath)
  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
  invisible(plot)
}



# gdtools::match_family("Roboto")
# gdtools::match_family("Verdana")

# library(rsvg)
# library(webp)
# render it into a bitmap array
# bitmap <- rsvg("jotalogo.svg", width = 3840)
# png::writePNG(bitmap, "jotalogo.png", dpi = 2540)
# webp::write_webp(bitmap, "jotalogo.webp", quality = 100)
# browseURL("jotalogo.png")

# url1 <- "https://upload.wikimedia.org/wikipedia/commons/6/68/Solid_black.png"
#url2 <- "https://upload.wikimedia.org/wikipedia/commons/thumb/4/47/PNG_transparency_demonstration_1.png/280px-PNG_transparency_demonstration_1.png"
# img <- png::readPNG(curl::curl_fetch_memory(url2)$content)

#htmlSVG({
#  plot.new()
#  rasterImage(img, 0.25, 0.25, 0.75, 0.75)
#  rect(0.25, 0.25, 0.75, 0.75, border = "red", lwd = 2)
# })
