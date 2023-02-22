#' Scatterplots of Device Image Differences (Continuous)
#'
#' Devices take 3 or 4 images, often with different ratings. This function explores the differences between the images for all devices within the data-frame (continuous traits).
#' For discrete traits, refer to \code{device_image_diff_discrete()}. Note that this function currently only supports MSA-marbling and EMA.
#'
#' @examples
#' data(devices, package = "meatrics") # load the data
#' device_image_diff(devices, "ema")
#'
#' @param df Device data-frame in long format.
#' @param variable A valid column name denoting the variable of interest.
#' @return Scatterplots of pairwise image differences, with marks following AMILSC recommended accuracy standards.
#'
#' @import ggplot2
#' @import cowplot
#'
#' @export

device_image_diff <- function(df, variable) {
  device_names <- unique(df$cold_grader)

  ggplot_list <- list()
  for (i in 1:length(device_names)) {
    ggplot_list[[i]] <- image_diff_scatter(df, variable, device_names[i]) +
      labs(title = paste(device_names[i]))
  }

  plot_grid(plotlist = ggplot_list, ncol = 3)
}


image_diff_scatter <- function(df, variable, device_name) {
  diff_df <- df |>
    filter(cold_grader == device_name) |>
    group_by(body_no, kill_date) |>
    select(body_no, kill_date, variable) |> drop_na()

  split_df_list <- split(diff_df, list(diff_df$body_no, diff_df$kill_date))

  result_list <- map(split_df_list, function(split_df) {
    if (nrow(split_df) >= 3) {
      pairwise_differences <- as.data.frame(as.numeric(dist(split_df[[variable]])))
      pairwise_differences$avg <- mean(split_df[[variable]])

      # adding pairwise combination types
      if (nrow(split_df) == 3) {
        pairwise_differences$type <- c("1 vs 2", "1 vs 3", "2 vs 3")
      } else {
        pairwise_differences$type <- c("1 vs 2", "1 vs 3", "1 vs 4",
                                       "2 vs 3", "2 vs 4", "3 vs 4")
      }

      colnames(pairwise_differences) <- c("diff", "avg", "type")
      return(pairwise_differences)
    }
  })

  result_list <- result_list |> bind_rows()

  plot <- ggplot(result_list, aes(x = avg, y = diff, color = type)) + geom_point(alpha = 0.5) +
    labs(x = paste("Average", variable), y = "Difference", color = "Images") +
    geom_smooth(formula = y ~ x, method = 'loess') + theme_bw()

  if (variable == "ema") {
    plot <- plot + scale_y_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), limits = c(-1, 24)) +
      geom_hline(yintercept = 0, alpha = 0.75, colour = "grey") +
      geom_hline(yintercept = 4, alpha = 0.5, colour = "red") +
      geom_hline(yintercept = 8, alpha = 0.5, colour = "orange") +
      geom_hline(yintercept = 12, alpha = 0.5, colour = "blue")
  } else if (variable == "msa_marbling") {
    plot <- plot + scale_y_continuous(breaks = c(0, 50, 100, 150, 200), limits = c(-1, 201)) +
      geom_hline(yintercept = 0, alpha = 0.75, colour = "grey") +
      geom_hline(yintercept = 50, alpha = 0.5, colour = "#F7D5D4") +
      geom_hline(yintercept = 100, alpha = 0.5, colour = "#E8B3B9") +
      geom_hline(yintercept = 150, alpha = 0.5, colour = "#CB6862") +
      geom_hline(yintercept = 200, alpha = 0.5, colour = "#AA3C3B")
  }

  return(plot)
}

#' Heatmap of Device Image Differences (Discrete)
#'
#' Devices take 3 or 4 images, often with different ratings. This function explores the differences between the images for all devices within the data-frame (discrete/categorical traits).
#' For continuous traits, refer to \code{device_image_diff()}.
#'
#' @examples
#' data(devices, package = "meatrics") # load the data
#' device_image_diff_discrete(devices, "meat_colour", device_name = "C3")
#'
#' @param df Device data-frame in long format.
#' @param variable A valid column name denoting the variable of interest.
#' @param device_name Device of interest.
#' @return Up to 6 heatmaps for each different pairwise difference.
#'
#' @import ggplot2
#' @import janitor
#' @import cowplot
#'
#' @export

device_image_diff_discrete <- function(df, variable, device_name) {

  diff_df <- df |>
    filter(cold_grader == device_name) |>
    group_by(body_no, kill_date) |>
    select(body_no, kill_date, variable, image) |> drop_na()

  # converting levels to numeric
  if (variable == "meat_colour") {
    diff_df <- diff_df |> mutate(meat_colour = ifelse(meat_colour %in% c("1A", "1B", "1C"),
                                                      ifelse(meat_colour == "1A", -1,
                                                             ifelse(meat_colour == "1B", 0, 1)),
                                                      as.numeric(meat_colour)))

    minimum <- -1
  } else if (variable == "aus_marbling" || variable == "fat_colour") {
    diff_df[[variable]] <- as.numeric(diff_df[[variable]])
    minimum <- 0
  } else {
    stop("This function is intended for discrete traits: aus_marbling, fat_colour, meat_colour")
  }

  split_df_list <- split(diff_df,list(diff_df$body_no,diff_df$kill_date))

  result_list <- map(split_df_list, function(split_df) {
    # oops forgot to filter the original device dataset to images >= 3
    if (nrow(split_df) >= 3) {
      pairwise_differences <- as.data.frame(as.numeric(dist(split_df[[variable]])))

      if (janitor::round_half_up(median(as.numeric(split_df[[variable]]))) == 10 &&
          variable == "aus_marbling") {
        pairwise_differences$median <- 9
      } else {
        pairwise_differences$median <- janitor::round_half_up(median(as.numeric(split_df[[variable]])))
      }

      # adding pairwise combination types
      if (nrow(split_df) == 3) {
        pairwise_differences$type <- c("1 vs 2", "1 vs 3", "2 vs 3")
      } else {
        pairwise_differences$type <- c("1 vs 2", "1 vs 3", "1 vs 4",
                                       "2 vs 3", "2 vs 4", "3 vs 4")
      }

      colnames(pairwise_differences) <- c("diff", "median", "type")
      return(pairwise_differences)
    }
  })

  result_list <- result_list |> bind_rows()
  image_split_pairwise <- split(result_list, result_list$type)
  ggplot_list <- list()

  for (i in 1:length(image_split_pairwise)) {
    df <- image_split_pairwise[[i]] |> group_by(diff, median) |>
      summarize(count = n())

    ggplot_list[[i]] <- ggplot(df, aes(x = median, y = diff, fill = count)) + geom_tile(show.legend = FALSE) +
      scale_fill_gradient(low = "white", high = "#AA3C3B") +
      # limits doesn't seem to be inclusive for geom_tile() ...?
      scale_x_continuous(breaks=seq(from = minimum, to = max(result_list$median), by = 1), limits = c(minimum - 1, 1 + max(result_list$median))) +
      scale_y_continuous(breaks=seq(from = min(result_list$diff), to = max(result_list$diff), by = 1), limits = c(-1, 1 + max(result_list$diff))) +
      annotate("text", x = df$median, y = df$diff, label = df$count, size = 3, color = "black", fontface = "plain") + theme_bw() + labs(title = image_split_pairwise[[i]]$type, x = "Median", y = "Difference", fill = "Frequency")
  }

  cowplot::plot_grid(plotlist = ggplot_list, ncol = 3)
}

#' Device Images Comparisons
#'
#' Devices take 3 or 4 images, often with different ratings. This function
#' visualises the grades between the images for a certain within the data-frame
#' in either a heatmap or a scatterplot, depending on if the trait measured is
#' discrete or continuous.
#'
#' @examples
#' data(devices, package = "meatrics") # load the data
#' device_image_comparison(devices, "meat_colour", device_name = "C1")
#' device_image_comparison(devices, "ema", device_name = "C2")
#'
#' @param df Device data-frame in long format.
#' @param variable A valid column name denoting the variable of interest.
#' @param device_name Device of interest.
#' @return 6 heatmaps/scatterplots for each pairwise image grading.
#'
#' @import ggplot2
#' @import cowplot
#'
#' @export

device_image_comparison <- function(df,
                                    variable,
                                    device_name) {

  image_combs <- asplit(combn(1:4, 2), 2)
  plot_list <- list()
  for (i in 1:length(image_combs)) {
    if (variable %in% c("meat_colour", "fat_colour", "aus_marbling")) {
      plot_list[[i]] <- suppressMessages(device_image_compare(df, variable, device_name, as.vector(image_combs[[i]])))
    } else if (variable %in% c("ema", "ribfat_cold", "msa_marbling")) {
      plot_list[[i]] <- suppressMessages(device_image_compare_cont(df, variable, device_name, image_combs[[i]]))
    } else {
      stop("Unrecognised trait! Please follow meatrics' outlined naming conventions.")
    }
  }
  cowplot::plot_grid(plotlist = plot_list, ncol = 3)
}

# helper function
device_image_compare <- function(df,
                                 variable,
                                 device_name,
                                 image_combination) {

  new_df <- df |>
    filter(cold_grader == device_name, image %in% image_combination) |>
    select(variable, image) |>
    pivot_wider(names_from = image,
                values_from = variable) |> drop_na()

  colnames(new_df) <- c("x", "y", "A", "B")
  new_df <- new_df |> group_by(A, B) |> summarise(Frequency = n())
  new_df$is_diag <- new_df$A == new_df$B

  return(ggplot(new_df, aes(x = A, y = B, fill = Frequency)) +
           geom_tile(show.legend = FALSE)  +
           scale_fill_gradient(low = "white", high = "#ff7f7f") +
           geom_tile(aes(colour = is_diag), size = 0.2, show.legend = FALSE) +
           geom_text(aes(label = Frequency)) +
           scale_colour_manual(values = c("grey","black"), guide = "none") +
           labs(x = paste("Image", image_combination[1]),
                y = paste("Image", image_combination[2]),
                title = paste(image_combination[1], "v", image_combination[2])) +
           theme_bw())
}

# helper function
device_image_compare_cont <- function(df,
                                       variable,
                                       device_name,
                                       image_combination) {

  new_df <- df |>
    filter(cold_grader == device_name, image %in% image_combination) |>
    select(variable, image) |>
    pivot_wider(names_from = image,
                values_from = variable) |> drop_na() |>
    rename("A" = 3, "B" = 4)

  return(ggplot(new_df, aes(x = A, y = B)) +
           geom_point(size = 0.3) +
           ggtitle(paste(image_combination[1], "v", image_combination[2])) +
           geom_smooth(formula = y ~ x, method = "lm", size = 0.5) +
           labs(x = paste("Image", image_combination[1]),
                y = paste("Image", image_combination[2])) +
           coord_equal() + theme_bw())
}
