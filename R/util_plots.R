#' Create a histogram + KDE estimate for a numeric variable.
#'
#' @param eval_data An `eval_data` object.
#' @param var_name Numeric variable name to plot.
#' @param cat1_name Optional categorical variable to group by for subplots.
#' @param cat2_name Optional categorical variable to group by for subplots.
#' 
#' @return A `ggplot2` plot
#' 
#' @export
plot_numeric_hist_kde <- function(eval_data,
                                  var_name,
                                  cat1_name = NULL,
                                  cat2_name = NULL) {
  
  stopifnot(is_eval_data(eval_data))
  
  # construct joint_data
  joint_data <- dplyr::bind_rows(
    confidential = eval_data[["conf_data"]],
    synthetic = eval_data[["synth_data"]], 
    .id = "source"
  )
  
  # check data types
  stopifnot(pillar::type_sum(joint_data[[var_name]]) == "dbl")
  
  if (!is.null(cat1_name)) {
    
    stopifnot(pillar::type_sum(joint_data[[cat1_name]]) == "fct")
    
  }
  
  if (!is.null(cat2_name)) {
    
    
    stopifnot(pillar::type_sum(joint_data[[cat2_name]]) == "fct")
    
  }
  
  # check source variable
  stopifnot(all(c("confidential", "synthetic") %in% 
                  (joint_data[["source"]] %>% unique)))
  
  plot <- ggplot2::ggplot(
    data = joint_data,
    mapping = ggplot2::aes(
      x = !!rlang::sym(var_name), 
      y = ggplot2::after_stat(!!rlang::sym('density')),
      fill = source)
  ) +
    ggplot2::geom_histogram(
      position = "identity",
      bins = 30,
      color = "black",
      alpha = 0.3
    ) + 
    ggplot2::geom_density(
      ggplot2::aes(color = source),
      alpha = 0.3
    ) + 
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90)
    )
  
  if (!is.null(cat1_name) & is.null(cat2_name)) {
    
    plot <- plot + 
      ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(cat1_name)))
    
  }
  
  if (!is.null(cat1_name) & !is.null(cat2_name)) {
    
    plot <- plot + 
      ggplot2::facet_grid(
        rows = ggplot2::vars(!!rlang::sym(cat1_name)),
        cols = ggplot2::vars(!!rlang::sym(cat2_name))
      )
    
  }
  
  return(plot)
  
}

#' Create bar charts for a categorical random variable.
#'
#' @param eval_data An `eval_data` object.
#' @param var_name Categorical variable name to plot.
#' @param cat1_name Optional categorical variable to group by for subplots.
#' @param cat2_name Optional categorical variable to group by for subplots.
#' 
#' @return A `ggplot2` plot
#' 
#' @export
plot_categorical_bar <- function(eval_data,
                                 var_name,
                                 cat1_name = NULL,
                                 cat2_name = NULL) {
  
  stopifnot(is_eval_data(eval_data))
  
  # construct joint_data
  joint_data <- dplyr::bind_rows(
    confidential = eval_data[["conf_data"]],
    synthetic = eval_data[["synth_data"]], 
    .id = "source"
  )
  
  # check data types
  stopifnot(pillar::type_sum(joint_data[[var_name]]) == "fct")
  
  if (!is.null(cat1_name)) {
    
    stopifnot(pillar::type_sum(joint_data[[cat1_name]]) == "fct")
    
  }
  
  if (!is.null(cat2_name)) {
    
    
    stopifnot(pillar::type_sum(joint_data[[cat2_name]]) == "fct")
    
  }
  
  # check source variable
  stopifnot(all(c("confidential", "synthetic") %in% 
                  (joint_data[["source"]] %>% unique)))
  
  plot <- ggplot2::ggplot(
    data = joint_data,
    mapping = ggplot2::aes(x = !!rlang::sym(var_name),
                           fill = source,
                           group = source)
  ) + 
    ggplot2::geom_bar(position = ggplot2::position_dodge()) + 
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90)
    )
  
  if (!is.null(cat1_name) & is.null(cat2_name)) {
    
    plot <- plot + 
      ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(cat1_name)))
    
  }
  
  if (!is.null(cat1_name) & !is.null(cat2_name)) {
    
    plot <- plot + 
      ggplot2::facet_grid(
        rows = ggplot2::vars(!!rlang::sym(cat1_name)),
        cols = ggplot2::vars(!!rlang::sym(cat2_name))
      )
    
  }
  
  return(plot)
  
}

#' Create a correlation heatmap for numeric random variables.
#'
#' @param data A data.frame/
#' @param cor_method A correlation method to pass to `stats::cor(., method=<cor_method>)`
#' 
#' @return A `ggplot2` plot
#' 
#' @export
create_cormat_plot <- function(data, cor_method = "pearson", group_by_q = NULL) {

  # get numeric variables -- this also defines the order used for the axes so
  # the heatmap renders as a clean triangle sloping downward left to right
  var_order <- data |>
    dplyr::select(tidyselect::where(is.numeric)) |>
    names()

  num_vars <- var_order
  if (!is.null(group_by_q)) {
    num_vars <- c(num_vars, group_by_q)
  }

  # subset to numeric variables and grouping variable if supplied
  data <- data |>
    dplyr::select(dplyr::all_of(num_vars))

  # get lower triangular correlation matrix
  cmat_raw <- .lower_triangle(
    data,
    use = "pairwise.complete.obs",
    group_by_q = group_by_q
  )

  # .lower_triangle assigns var1/var2 based on alphabetical order, which can
  # place a pair on the wrong side of the diagonal relative to var_order
  # (the original column order). Since correlation is symmetric, swap var1
  # and var2 wherever needed so var1 always comes earlier than var2 in
  # var_order -- this puts the triangle's right angle in the lower-left
  # corner (empty top-right), with the hypotenuse sloping down left to right.
  pos1 <- match(cmat_raw$var1, var_order)
  pos2 <- match(cmat_raw$var2, var_order)
  needs_swap <- pos1 > pos2

  cmat <- cmat_raw |>
    dplyr::mutate(
      var1_tmp = dplyr::if_else(needs_swap, .data$var2, .data$var1),
      var2_tmp = dplyr::if_else(needs_swap, .data$var1, .data$var2),
      correlation = round(.data$correlation, digits = 2),
      var1 = factor(.data$var1_tmp, levels = var_order),
      var2 = factor(.data$var2_tmp, levels = rev(var_order))
    ) |>
    dplyr::select(-"var1_tmp", -"var2_tmp") |>
    as.data.frame()

  plot <-
    ggplot2::ggplot(
      data = cmat,
      mapping = ggplot2::aes(
        x = .data$var1,
        y = .data$var2,
        fill = .data$correlation
      )
    ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(
      low = "firebrick",
      high = "chartreuse4",
      mid = "white",
      midpoint = 0,
      limit = c(-1, 1),
      space = "Lab",
      name = "Correlation"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 1),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::coord_fixed() +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$correlation),
      color = "black",
      size = 4
    )

  if (!is.null(group_by_q)) {
    plot <- plot +
      ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(group_by_q)), ncol = 1)
  }

  return(plot)

}

#' Create side-by-side correlation heatmaps for numeric random variables.
#'
#' @param eval_data An `eval_data` object.
#' @param cor_method A correlation method to pass to `stats::cor(., method=<cor_method>)`
#'
#' @return A `ggplot2` plot
#'
#' @export
plot_cormat <- function(eval_data, cor_method = "pearson", group_by_q = NULL) {

  stopifnot(is_eval_data(eval_data))

  # subset datasets to numeric variables present in both datasets + grouping variable if supplied
  intersect_numeric <- intersect(
    names(eval_data[["conf_data"]])[sapply(eval_data[["conf_data"]], is.numeric)],
    names(eval_data[["synth_data"]])[sapply(eval_data[["synth_data"]], is.numeric)]
  )
  if (!is.null(group_by_q)) {
    intersect_numeric <- c(intersect_numeric, group_by_q)
  }

  p1 <- create_cormat_plot(eval_data[["conf_data"]][intersect_numeric], cor_method = cor_method, group_by_q = group_by_q) +
    ggplot2::ggtitle("Confidential data")
  p2 <- create_cormat_plot(eval_data[["synth_data"]][intersect_numeric], cor_method = cor_method, group_by_q = group_by_q) +
    ggplot2::ggtitle("Synthetic data")

  plot <- gridExtra::grid.arrange(p1, p2, nrow = 1)

  return(plot)

}
