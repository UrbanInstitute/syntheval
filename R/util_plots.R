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
#' @param data A data.frame
#' @param statistic A character string specifying which bivariate statistic
#' to be returned by the function. One of "correlation", "covariance", or "rmi" or an
#' abbreviation
#' @param cor_method A correlation method to pass to `stats::cor(., method=<cor_method>)`
#' or `stats::cov()`. Ignored when statistic = "rmi"
#' @param group_by_q Optional quoted character string of a variable name to
#' group the data by. If provided, the statistics will be plotted for each group separately
#' @param fill_limits Optional numeric length-2 vector giving the lower and upper
#'   limits for the heatmap fill scale. If `NULL`, defaults are used by statistic
#'   (`c(-1, 1)` for correlation, `c(0, 1)` for RMI, and data-driven limits for covariance).
#' 
#' @return A `ggplot2` plot
#' 
#' @export
create_cormat_plot <- function(data, statistic, cor_method = "pearson", group_by_q = NULL, fill_limits = NULL) {

  # parameter validation
  valid_statistics <- c("correlation", "covariance", "rmi")
  na.statistic <- pmatch(x = tolower(statistic), table = valid_statistics)
  if (is.na(na.statistic)) stop("invalid 'statistic' argument")
  statistic <- match.arg(arg = tolower(statistic), choice = valid_statistics)

  if (!is.null(group_by_q)) {
    if (!is.character(group_by_q)) {
      stop("group_by_q must be a quoted string", call. = FALSE)
    }
    if (length(group_by_q) != 1) {
      stop("only one variable is supported in group_by_q", call. = FALSE)
    }
    if (!(group_by_q %in% names(data))) {
      stop("variable provided for group_by_q was not found in data", call. = FALSE)
    }
  }

  # get numeric variables -- this also defines the order used for the axes so
  # the heatmap renders as a clean triangle sloping downward left to right

  if (statistic != "rmi") {
    var_order <- data |>
      dplyr::select(tidyselect::where(is.numeric)) |>
      names()

    vars_select <- var_order
    if (!is.null(group_by_q)) {
      vars_select <- c(vars_select, group_by_q)
    }
  } else {
    var_order <- data |>
      dplyr::select(tidyselect::where(is.factor)) |>
      names()

    vars_select <- var_order
    if (!is.null(group_by_q)) {
      vars_select <- c(vars_select, group_by_q)
    }
  }

  # subset to numeric variables and grouping variable if supplied
  data <- data |>
    dplyr::select(dplyr::all_of(vars_select))

  if (statistic != "rmi") {
    mat_raw <- .lower_triangle(
      data,
      statistic = statistic,
      use = "pairwise.complete.obs",
      group_by_q = group_by_q,
      method = cor_method
    )
  } else {
    mat_raw <- .calc_rmi_tibble(
      data,
      group_by_q = group_by_q
    )
  }

  mat <- mat_raw |>
    dplyr::mutate(
      statistic = round(.data$statistic, digits = 2),
      var1 = factor(.data$var1, levels = rev(var_order)),
      var2 = factor(.data$var2, levels = var_order)
    ) |>
    as.data.frame()

  plot <-
    ggplot2::ggplot(
      data = mat,
      mapping = ggplot2::aes(
        x = .data$var1,
        y = .data$var2,
        fill = .data$statistic
      )
    ) +
    ggplot2::geom_tile(color = "white") +
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
      ggplot2::aes(label = .data$statistic),
      color = "black",
      size = 4
    )

    if (statistic == "correlation") {
      plot <- plot +
        ggplot2::scale_fill_gradient2(
          low = "firebrick",
          high = "chartreuse4",
          mid = "white",
          midpoint = 0,
          limits = fill_limits %||% c(-1, 1),
          space = "Lab",
          name = paste0(statistic, ": ", cor_method, " method")
        )
    } else if (statistic == "covariance") {
      plot <- plot +
        ggplot2::scale_fill_gradient2(
          low = "firebrick",
          high = "chartreuse4",
          mid = "white",
          midpoint = 0,
          limits = fill_limits,
          space = "Lab",
          name = paste0(statistic, ": ", cor_method, " method")
        )
    } else if (statistic == "rmi") {
      plot <- plot +
        ggplot2::scale_fill_gradient(
          low = "white",
          high = "chartreuse4",
          limits = fill_limits %||% c(0, 1),
          space = "Lab",
          name = "Relative Mutual Information"
        )
    }

  if (!is.null(group_by_q)) {
    plot <- plot +
      ggplot2::facet_wrap(ggplot2::vars(.data[[group_by_q]]), ncol = 1)
  }

return(plot)

}

#' Create side-by-side bivariate heatmaps for numeric random variables.
#'
#' @param eval_data An `eval_data` object.
#' @param statistic A character string specifying which bivariate statistic.
#' One of "correlation", "covariance", or "rmi"
#' @param cor_method A correlation method to pass to `stats::cor(., method=<cor_method>)`
#' or `stats::cov`. Ignored when statistic = "rmi".
#' @param group_by_q Optional quoted character string of a variable name to
#' group the data by. If provided, the correlation fit metric will be calculated
#' for each group separately.
#'
#' @return A `ggplot2` plot
#'
#' @export
plot_cormat <- function(eval_data, statistic, cor_method = "pearson", group_by_q = NULL) {

  stopifnot(is_eval_data(eval_data))

  # subset datasets to numeric or factor variables present in both datasets + grouping variable if supplied
  if (statistic != "rmi") {
    intersect_vars <- intersect(
      names(eval_data[["conf_data"]])[sapply(eval_data[["conf_data"]], tidyselect::where(is.numeric))],
      names(eval_data[["synth_data"]])[sapply(eval_data[["synth_data"]], tidyselect::where(is.numeric))]
    )
  } else {
    intersect_vars <- intersect(
      names(eval_data[["conf_data"]])[sapply(eval_data[["conf_data"]], tidyselect::where(is.factor))],
      names(eval_data[["synth_data"]])[sapply(eval_data[["synth_data"]], tidyselect::where(is.factor))]
  )
  }
  
  if(length(intersect_vars) < 2 & statistic != "rmi") stop("Must supply at least two numeric variables shared by synth_data and conf_data")
  if(length(intersect_vars) < 2 & statistic == "rmi") stop("Must supply at least two factor variables shared by synth_data and conf_data")
  

  if (!is.null(group_by_q)) {
    intersect_vars <- c(intersect_vars, group_by_q)
  }

  # one shared scale for both panels
  if (statistic == "correlation") {
    shared_limits <- c(-1, 1)
  } else if (statistic == "rmi") {
    shared_limits <- c(0, 1)
  } else {
    conf_vals <- .lower_triangle(
      eval_data$conf_data[intersect_vars],
      statistic = "covariance",
      use = "pairwise.complete.obs",
      method = cor_method
    )
    syn_vals <- .lower_triangle(
      eval_data$synth_data[intersect_vars],
      statistic = "covariance",
      use = "pairwise.complete.obs",
      method = cor_method
    )
    shared_limits <- range(c(conf_vals$statistic, syn_vals$statistic), na.rm = TRUE)
  }

  p1 <- create_cormat_plot(
    eval_data$conf_data[intersect_vars],
    statistic = statistic,
    cor_method = cor_method,
    group_by_q = group_by_q,
    fill_limits = shared_limits
  ) + ggplot2::ggtitle("Confidential data")

  p2 <- create_cormat_plot(
    eval_data$synth_data[intersect_vars],
    statistic = statistic,
    cor_method = cor_method,
    group_by_q = group_by_q,
    fill_limits = shared_limits
  ) + ggplot2::ggtitle("Synthetic data")

  plot <- patchwork::wrap_plots(p1, p2, nrow = 1)

  return(plot)

}

#' @rdname plot_cormat
#' @export
plot_bivariate <- function(eval_data, statistic, cor_method = "pearson", group_by_q = NULL) {
  plot_cormat(
    eval_data = eval_data,
    statistic = statistic,
    cor_method = cor_method,
    group_by_q = group_by_q
  )
}

#' Create a black/white missingness matrix plot for a data set.
#'
#' @param data A data.frame
#' @param na_values An optional scalar or vector of values (in addition to `NA`)
#' that should be treated as missing
#'
#' @return A `ggplot2` plot
#'
#' @export
create_na_matrix_plot <- function(data, na_values = NULL) {
  
  data <- .recode_custom_na(data, na_values = na_values)
  
  na_df <- data |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = -"row_id",
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::mutate(is_na = is.na(.data$value))
  
  plot <- ggplot2::ggplot(
    data = na_df,
    mapping = ggplot2::aes(x = .data$variable, y = .data$row_id, fill = .data$is_na)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_manual(
      values = c(`FALSE` = "white", `TRUE` = "black"),
      name = "Missing"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 1),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  
  return(plot)
  
}

#' Create side-by-side black/white missingness matrix plots.
#'
#' @param eval_data An `eval_data` object.
#' @param na_values An optional scalar or vector of values (in addition to `NA`)
#' that should be treated as missing
#'
#' @return A `ggplot2` plot
#'
#' @export
plot_na_matrix <- function(eval_data, na_values = NULL) {
  
  stopifnot(is_eval_data(eval_data))
  
  p1 <- create_na_matrix_plot(eval_data[["conf_data"]], na_values = na_values) +
    ggplot2::labs(title = "Confidential data")
  p2 <- create_na_matrix_plot(eval_data[["synth_data"]], na_values = na_values) +
    ggplot2::labs(title = "Synthetic data")
  
  plots <- list(p1, p2)
  
  if (!is.null(eval_data[["holdout_data"]])) {
    
    p3 <- create_na_matrix_plot(eval_data[["holdout_data"]], na_values = na_values) +
      ggplot2::labs(title = "Holdout data")
    
    plots <- c(plots, list(p3))
    
  }
  
  plot <- patchwork::wrap_plots(plots, nrow = 1)
  
  return(plot)
  
}
