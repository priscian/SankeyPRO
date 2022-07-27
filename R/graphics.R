
#' @export
sabi_colors <- c(white = "#ffffff", `dark blue` = "#2c6ba9", `light blue` = "#93c0db", orange = "#e79876", red = "#b30014", `dark gray` = "#3b3937")

#' @export
sabi_levels <- sabi_colors %>% `names<-`(c("None", "Mild", "Moderate", "Severe", "Very severe", "Missing"))

## Create plot from "sankeypro" object
#' @export
plot.sankeypro <- function(
  x, # "sankeypro" object originally created from 'ggsankey::make_long()'
  scale_fill_manual_values, # E.g. sabi_levels[c("Missing", "Very severe", "Mild")] %>% as.vector, # gray, red, blue
  aes_label = rlang::expr(sprintf("%s, N = %s (%.1f%%)", node, n, pct * 100)),
  ... # Values passed to 'ggplot2::labs()', e.g. title, subtitle, caption, fill
)
{
  n_row <- attr(x, "nrow")

  p <- ggplot2::ggplot(
    x,
    ggplot2::aes(
      x = x,
      next_x = next_x,
      node = node,
      next_node = next_node,
      fill = node,
      #label = node # Must also invoke 'ggsankey::geom_[sankey|alluvial]_label()' below
      label = !!aes_label
    )
  ) +
  ## N.B. 'ggsankey::geom_sankey()' & 'ggsankey::geom_alluvial()' are essentially interchangeable.
  ggsankey::geom_alluvial(flow.alpha = 0.5, node.color = "black", show.legend = TRUE) + # node.color = 0 for no node border
  ggsankey::geom_alluvial_label(size = 3, color = "black", fill= "white", hjust = 0.3) +
  ggplot2::theme_bw() +
  #ggplot2::theme(legend.position = "none") +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    #axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank()
  ) +
  #ggplot2::scale_fill_viridis_d(option = "inferno") +
  ggplot2::scale_fill_manual(
    values = scale_fill_manual_values,
    guide = ggplot2::guide_legend(reverse = TRUE) # Reverse order of legend items
  ) +
  ggplot2::labs(...) +
  #ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5), labels = scales::percent_format(scale = 100/n_row))
  ggplot2::scale_y_continuous(breaks = seq(0, 1, length.out = 5) * n_row, labels = scales::percent_format(scale = 100/n_row))

  ## Return a "ggplot" object
  p
}


## Interface function to work directly w/ wide data from REDCap etc.
#' @export
plot_sankeypro <- function(
  x, # long data set imported from REDCap etc.
  pro_var, # Name of PRO variable
  time_var, # Variable containing names of the PRO time points, e.g. "Assessment"
  id_var, # Name of ID variable distinguishing patients from one another
  node_cutpoints = NULL,
  node_labels = NULL,
  node_colors,
  legend_title = "",
  show_counts = FALSE, # Should the plot display counts & percentages on node labels?
  ## Less-frequently used arguments:
  long_to_wide... = list(),
  ## 'remove_intermittent': If FALSE, rowwise: At 1st NA from left to right, make all further rightwise columns into NAs; else
  ## If TRUE, remove the intermittently valued rows described above
  remove_intermittent = TRUE,
  node_fmt = "%1.2f",
  show_plot = TRUE
)
{
  long_to_wideArgs <- list(
    x = x,
    long_var = time_var,
    id_cols = id_var
  )
  long_to_wideArgs <- utils::modifyList(long_to_wideArgs, long_to_wide..., keep.null = TRUE)

  w <- do.call(long_to_wide, long_to_wideArgs)

  d <- w %>%
    dplyr::select(matches(sprintf("^%s.*?$", pro_var), perl = TRUE)) %>%
    dplyr::rename_with(~ stringr::str_match(.x, "^.*?_(.*?)$")[, 2]) %>%
    handle_missings(remove_intermittent = remove_intermittent)

  ## Create "sankeypro" data frame customized for plotting
  d_s <- d %>% make_long() %>%
    ## Label order: NA/missing category, < 1st cutpoint, >= 1st cut & < 2nd cut, >= 2nd cut & < 3d cut, ...
    categorize_nodes(cutpoints = node_cutpoints, fmt = node_fmt, labels = node_labels)

  if (is_invalid(node_colors)) {
    node_colors <- colorspace::rainbow_hcl(nlevels(d_s$node))
  }

  p <- plot( # Invokes 'plot.sankeypro()'
    d_s,
    scale_fill_manual_values = node_colors,
    fill = legend_title # Legend title
  )

  if (!show_counts)
    p <- p + ggplot2::aes(label = node)

  if (show_plot)
    print(p)

  p
}
