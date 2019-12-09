#' Add a plot to a plot_list
#'
#' \code{add_to_plot_list} adds a plot and an optional file name
#' to the supplied list. If no file name is supplied an arbitrary
#' one is created of the form 'plot-' \code{index} '.png'.
#'
#' @param plot_list a list to hold the plots
#' @param plot a plot object
#' @param filename character, the filename to use to save the plot
#'
#' @return the new plot list
#'
#' @examples
#' plot_list <- add_to_plot_list(plot_list, plot)
#' plot_list <- add_to_plot_list(plot_list, plot, filename = 'file1.png')
#'
#' @export
add_to_plot_list <- function(plot_list, plot, filename = NULL) {
  # get current length of plot list
  index <- length(plot_list) + 1
  # create filename if none supplied
  if (is.null(filename)) {
    filename <- paste0('plot-', index, '.png')
  }
  new_entry <- list(filename = filename, plot = plot)
  # add new plot to end of list
  plot_list[[index]] <- new_entry
  return(plot_list)
}

#' Open a graphics device based on a file name
#'
#' \code{open_graphics_device} takes a file name and opens a graphics
#' device based on the file suffix. Defaults to pdf if no match is found.
#'
#' @param filename character, the filename to use
#' @param ... Arguments passed on to the graphics device
#'
#' @return TRUE invisibly
#'
#' @examples
#' open_graphics_device(filename = 'plot.eps')
#' open_graphics_device(filename = 'plot.eps', width = 6, height = 5)
#' open_graphics_device(filename = 'plot.png', width = 640, height = 480)
#'
#' @export
open_graphics_device <- function(filename = 'plot.pdf', ...) {
  if (sub("^.*\\.", "", filename) == "eps") {
    postscript(file = filename, ...)
  } else if (sub("^.*\\.", "", filename) == "svg") {
    library('svglite')
    svglite(file = filename, ...)
  } else if (sub("^.*\\.", "", filename) == "png") {
    png(filename = filename, ...)
  } else {
    pdf(file = filename, ...)
  }
  invisible(TRUE)
}

#' Output a plot from a plot list
#'
#' \code{output_plot} take a plot_entry object and saves the plot using
#' the correct graphics device. Currently the only extra parameters that
#' can be passed through to the graphics device are width, height and bg
#' as these are the only ones that are commmon between all 4 devices,
#'
#' @param plot_entry list containing filename and plot
#' @param ... Arguments passed on to the graphics device
#'
#' @return the new plot list
#'
#' @examples
#' output_plot(plot_entry)
#' output_plot(plot_entry, width = 6, height = 7)
#'
#' @export
output_plot <- function(plot_entry, ...) {
  open_graphics_device(filename = plot_entry$filename, ...)
  print(plot_entry$plot)
  invisible(dev.off())
}
