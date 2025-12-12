#' viz_x_by_y
#'
#' Visualize one var (continuous or categorical) by one facet grid column variable (categorical).
#'
#' @param mydata (unquoted) A data frame
#' @param x (quoted) A variable name for the x-axis
#' @param facet_grid_col_var (quoted) A categorical variable for the facet_grid column variable
#'
#' @importFrom rlang .data
#'
#' @returns plot
#' @export
#'
viz_x_by_y <- function(mydata,
                       x,
                       facet_grid_col_var) {

  plot <- mydata %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[x]])) +
    {
      if (is.numeric(mydata[[x]]))
        ggplot2::geom_histogram()
      else
        ggplot2::geom_bar()
    } +
    ggplot2::facet_grid(cols = ggplot2::vars(.data[[facet_grid_col_var]])) +
    ggplot2::labs(y = "Count", title = paste0({
      if (!is.null(labelled::var_label(mydata[[x]])))
        labelled::var_label(mydata[[x]])
      else
        x
    }, " by ", {
      if (!is.null(labelled::var_label(mydata[[facet_grid_col_var]])))
        labelled::var_label(mydata[[facet_grid_col_var]])
      else
        facet_grid_col_var
    })) +
    ggplot2::theme_bw()
  return(plot)
}
