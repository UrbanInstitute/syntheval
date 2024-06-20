#' Create a histogram + KDE estimate for a numeric variable.
#'
#' @param joint_data A data.frame combining rows from confidential and synthetic 
#' data, with the column 'source' identifying the two.
#' @param var_name Numeric variable name to plot.
#' @param cat1_name Optional categorical variable to group by for subplots.
#' @param cat2_name Optional categorical variable to group by for subplots.
#' 
#' @return A `ggplot2` plot
#' 
#' @export
plot_numeric_hist_kde <- function(joint_data,
                                  var_name,
                                  cat1_name = NULL,
                                  cat2_name = NULL) {
  
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
    mapping = ggplot2::aes(x = !!rlang::sym(var_name), 
                           y = ggplot2::after_stat(density),
                           fill = source)
  ) +
    ggplot2::geom_histogram(position = "identity",
                            bins = 30,
                            color = "black",
                            alpha = .3) + 
    ggplot2::geom_density(ggplot2::aes(color = source),
                          alpha = .3) + 
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90)
    )
  
  if (!is.null(cat1_name) & is.null(cat2_name)) {
    
    plot <- plot + ggplot2::facet_wrap(vars(!!rlang::sym(cat1_name)))
    
  }
  
  if (!is.null(cat1_name) & !is.null(cat2_name)) {
    
    plot <- plot + ggplot2::facet_grid(rows = vars(!!rlang::sym(cat1_name)),
                                       cols = vars(!!rlang::sym(cat2_name)))
    
  }
  
  return(plot)
  
}

#' Create bar charts for a categorical random variable.
#'
#' @param joint_data A data.frame combining rows from confidential and synthetic 
#' data, with the column 'source' identifying the two.
#' @param var_name Categorical variable name to plot.
#' @param cat1_name Optional categorical variable to group by for subplots.
#' @param cat2_name Optional categorical variable to group by for subplots.
#' 
#' @return A `ggplot2` plot
#' 
#' @export
plot_categorical_bar <- function(joint_data,
                                 var_name,
                                 cat1_name = NULL,
                                 cat2_name = NULL) {
  
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
    
    plot <- plot + ggplot2::facet_wrap(vars(!!rlang::sym(cat1_name)))
    
  }
  
  if (!is.null(cat1_name) & !is.null(cat2_name)) {
    
    plot <- plot + ggplot2::facet_grid(rows = vars(!!rlang::sym(cat1_name)),
                                       cols = vars(!!rlang::sym(cat2_name)))
    
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
create_cormat_plot <- function(data, cor_method = "pearson") {
  
  # get numeric variable names
  dtypes <- data %>% purrr::map_chr(~ pillar::type_sum(.x))
  num_vars <- names(dtypes[dtypes == "dbl"])
  
  # get lower triangular correlation matrix
  cmat <- stats::cor(data[num_vars], method = cor_method) %>%
    round(2) 
  cmat[upper.tri(cmat)] <- NA 
  
  # generate plot
  cmat_df <- as.data.frame.table(cmat) %>%
    dplyr::filter(stats::complete.cases(.))
  
  plot <- ggplot2::ggplot(data = cmat_df, 
                          mapping = ggplot2::aes(x = Var1,
                                                 y = Var2, 
                                                 fill = Freq)) + 
    ggplot2::geom_tile(color = "white") + 
    ggplot2::scale_fill_gradient2(low = "firebrick", 
                                  high= "chartreuse4", 
                                  mid = "white",
                                  midpoint = 0, 
                                  limit=c(-1, 1), 
                                  space = "Lab",
                                  name="Correlation") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust = 1),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) + 
    ggplot2::coord_fixed() + 
    ggplot2::geom_text(ggplot2::aes(label = Freq), 
                       color = "black", size = 4)
  
  return(plot)
  
}

#' Create side-by-side correlation heatmaps for numeric random variables.
#'
#' @param conf_data Confidential data
#' @param synth_data Synthetic data
#' @param cor_method A correlation method to pass to `stats::cor(., method=<cor_method>)`
#' 
#' @return A `ggplot2` plot
#' 
#' @export
plot_cormat <- function(conf_data, synth_data, cor_method = "pearson") {
  
  p1 <- create_cormat_plot(conf_data, cor_method = cor_method) + 
    ggplot2::ggtitle("Confidential data")
  p2 <- create_cormat_plot(synth_data, cor_method = cor_method) + 
    ggplot2::ggtitle("Synthetic data")
  
  gridExtra::grid.arrange(p1, p2, nrow=1)
  
}
