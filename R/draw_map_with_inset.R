
#' Draw a map with a scaled and positioned inset
#'
#' @param main_map ggplot object of the main map
#' @param inset_map ggplot object of the inset map
#' @param pos character; one of "tl", "tr", "bl", "br" for inset placement
#' @param inset_scale numeric; proportion of main plot area the inset should cover (e.g. 0.3)
#' @param x_offset numeric; horizontal nudge (0–1 scale)
#' @param y_offset numeric; vertical nudge (0–1 scale)
#'
#' @return A ggdraw object with inset embedded
draw_map_with_inset <- function(main_map,
                                inset_map,
                                pos = "tl",
                                inset_scale = 0.3,
                                x_offset = 0,
                                y_offset = 0) {
  library(cowplot)
  library(knitr)

  # Ensure inset is flexible
  inset_map <- inset_map + theme(aspect.ratio = NULL)

  # Get fig size from knitr chunk options
  fig_width <- knitr::opts_current$get("fig.width") %||% 8
  fig_height <- knitr::opts_current$get("fig.height") %||% 6

  # Default position logic
  base_pos <- switch(
    pos,
    "tl" = c(0.02, 0.65),
    "tr" = c(1 - inset_scale - 0.02, 0.65),
    "bl" = c(0.02, 0.02),
    "br" = c(1 - inset_scale - 0.02, 0.02),
    c(0.02, 0.65)  # default to top-left
  )

  x <- base_pos[1] + x_offset
  y <- base_pos[2] + y_offset
  w <- inset_scale
  h <- inset_scale * (fig_width / fig_height)  # preserve figure aspect ratio

  cowplot::ggdraw() +
    draw_plot(main_map) +
    draw_plot(inset_map, x = x, y = y, width = w, height = h)
}
