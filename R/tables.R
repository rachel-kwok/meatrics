requireNamespace("purrr")
requireNamespace("tidyverse")


#' Pairwise Statistics Table (continuous traits only!)
#'
#' @param df Input data-frame in long format
#' @param device_status A boolean denoting if this is the device data-frame;
#' default assumes grader data-frame.
#' @param variable A valid column name denoting the variable of interest.
#' @param by_date A boolean denoting if the statistics should be split by date.
#' @return A table containing pairwise maximum difference, mean difference,
#' median difference, R-squared, Kendall's Tau and Spearman's Rho.
#'
#'
#' @import tidyverse
#' @import gt
#'
#' @export

pairwise_stats <- function(df,
                           device_status = FALSE,
                           variable,
                           by_date = FALSE) {

  if (!by_date) {
    pairwise_diff(df, device_status, variable)
  } else {
    df <- to_wide(df, variable, device_status)
    df_list <- split(df, df$kill_date)

    # for computing all data-frames split by date
    gt_list <- map(df_list, function(split_df) {
      # generating combinations
      col_combinations <- combn(colnames(split_df[, 3:ncol(split_df)]), 2)

      result_df <- data.frame(date = character(), graders = character(),
                              average_difference = double(),
                              median_difference = double(),
                              max_difference = double(),
                              r_squared = double(),
                              kendall_tau = double(),
                              spearman_rho = double())

      # computing differences across all column combinations
      result_df <- map_dfr(1:ncol(col_combinations), ~{
        col1 <- col_combinations[[1, .x]]
        col2 <- col_combinations[[2, .x]]

        temp_df <- split_df |> rowwise() |>
          mutate(difference = list(abs(split_df[[col1]] - split_df[[col2]])),
                 graders = paste(col1, "&", col2),
                 date = unique(split_df$kill_date)) |>
          group_by(date, graders) |>
          summarize(average_difference = mean(unlist(difference)),
                    median_difference = median(unlist(difference)),
                    max_difference = max(unlist(difference)),
                    r_squared = cor(split_df[[col1]],
                                    split_df[[col2]], method = "pearson")^2,
                    kendall_tau = cor(split_df[[col1]],
                                      split_df[[col2]], method = "kendall"),
                    spearman_rho = cor(split_df[[col1]],
                                       split_df[[col2]], method = "spearman"))

      })

      colnames(result_df) <- c("Date", "Graders", "Average Difference",
                               "Median Difference", "Maximum Difference",
                               "R-squared", "Kendall's Tau", "Spearman's Rho")
      result_df
    })

    gt_list |> bind_rows() |> gt()
  }
}



#' Helper function of pairwise_stats()
#'
#' @param df Input data-frame in long format
#' @param device_status A boolean denoting if this is the device data-frame; default is human grader
#' @param variable A valid column name denoting the variable of interest
#' @return A table containing pairwise maximum difference, mean difference and median difference
#'
#' @import tidyverse
#' @import gt
#'

pairwise_diff <- function(df, device_status = FALSE, variable) {
  # pivot dataframe so graders are column names
  new_df <- to_wide(df, variable, device_status)

  # dropping kill_date and body_no before getting pairwise combinations of graders
  final_df <- new_df[,-c(1:2)] |> as.data.frame()
  col_combinations <- combn(colnames(final_df), 2)

  # initialise results dataframe
  result_df <- data.frame(graders = character(), average_difference = double(),
                          median_difference = double(), max_difference = double(),
                          r_squared = double(), kendall_tau = double(),
                          spearman_rho = double())

  # iterate through all grader combinations
  for (i in 1:ncol(col_combinations)) {
    col1 <- col_combinations[1,i]
    col2 <- col_combinations[2,i]

    temp_df <- final_df |>
      mutate(difference = abs(final_df[[col1]] - final_df[[col2]]), graders = paste(col1, "&", col2)) |>
      group_by(graders) |>
      summarize(average_difference = mean(difference),
                median_difference = median(difference),
                max_difference = max(difference),
                r_squared = cor(final_df[[col1]], final_df[[col2]], method = "pearson")^2,
                kendall_tau = cor(final_df[[col1]], final_df[[col2]], method = "kendall"),
                spearman_rho = cor(final_df[[col1]], final_df[[col2]], method = "spearman"))

    result_df <- bind_rows(result_df, temp_df)
  }

  colnames(result_df) <- c("Graders", "Average Difference", "Median Difference",
                           "Maximum Difference", "R-squared", "Kendall's Tau", "Spearman's Rho")

  result_df |> gt()
}

#' Table containing counts of exact agreement and ranges of input thresholds
#'
#' @param df Input dataframe in long format
#' @param variable A valid column name denoting the variable of interest
#' @param grader_a First grader
#' @param grader_b Second grader
#' @param units Specified units
#' @param visuals Will output either a stacked bar chart or a table. Can be either "table" or "bar", default is "table".
#' @param partitions An integer specifying how many groups to split the variable into
#' @return A GT() table or ggplot2 stacked bar chart, categorizing differences into 4 categories.
#'
#' @import tidyverse
#' @import ggplot2
#' @import gt
#'
#' @export

# note that this function doesn't work with synthetic dataset :(

threshold_diff <- function(df, device_status = FALSE, variable, grader_a, grader_b, units, visuals = "table", partitions = 1) {

  if (is.numeric(partitions)) {
    if (round(partitions) == partitions) {
      cut_points <- seq(min(df[[variable]], na.rm = TRUE), max(df[[variable]], na.rm = TRUE), length.out = partitions+1)

      data_list <- list()
      if (partitions == 1) {
        data_list[[1]] <- to_wide(df, variable, device_status) |> select(-1, -2)

      } else {
        data_list <- split(df, cut(df[[variable]], breaks = cut_points, right = FALSE))
        for (i in 1:length(data_list)) {
          data_list[[i]] <- to_wide(data_list[[i]], variable, device_status) |> select(-1, -2)
        }
      }
    } else {
      stop("Please enter a whole number for partitions.")
    }
  } else {
    stop("Please enter a numeric partition.")
  }

  result_df <- data.frame(ranges = double(), exact_agreement = double(), within_unit = double(),
                          within_2_unit = double(), outside = double(), n = double())

  for (j in 1:length(data_list)) {
    temp_df <- data_list[[j]] |>
      mutate(ranges = paste(round(cut_points[j], 2), "&", round(cut_points[j+1], 2)),
             diff = abs(data_list[[j]][[grader_a]] - data_list[[j]][[grader_b]]),
             exact_agreement = ifelse(diff == 0, 1, 0),
             within_unit = ifelse(diff <= units, 1, 0),
             within_2_unit = ifelse(diff <= 2*units & diff > units, 1, 0),
             outside = ifelse(diff > 2*units, 1, 0),
             n = 1) |>
      select(exact_agreement, within_unit, within_2_unit, outside, n) |>
      summarise_all(sum) |>
      mutate(n = sum(exact_agreement, within_unit, within_2_unit, outside))

    result_df <- bind_rows(result_df, temp_df)
    result_df[j, 1] <- paste0(round(cut_points[j], 2), "-", round(cut_points[j+1], 2))
  }

  colnames(result_df) <- c("Range", "Exact Agreement", paste("Difference <=", units), paste(units, "< Difference", "<=", 2*units),
                           paste("Difference >", 2*units), "n")

  if (visuals == "table") {
    result_df <- result_df |>
      mutate_at(vars(2:5), funs(sprintf("%.2f%%",./n*100)))

    result_df |> gt()
  } else if (visuals == "bar") {
    result_df <- result_df |> pivot_longer(cols = 2:5, names_to = "level", values_to = "freq")

    return(
      ggplot(result_df, aes(x = Range, y = freq, fill = level, group = level)) +
        geom_bar(stat = "identity", position = "fill") +
        labs(x = "Ranges", y = "Percentage", fill = "Differences")
    )
  } else {
    stop("Invalid visual parameter. Choose 'table' or 'bar'.")
  }

}
