#' AMILSC Minimum Requirements Table (MSA Marbling)
#'
#' This function generates a table following the AMILSC recommended accuracy
#' standards for cut surface cameras. It computes a generalised expert grader by
#' taking the mean of the devices.
#'
#' @param df Human grader data-frame in long format.
#' @param df2 Device data-frame in long format.
#' @param device_name Name of the device that is to be compared with the generalised expert grader.
#' @param grader_count An integer specifying the number of expert graders present in df.
#' @return A GT() table with the minimum accuracy standards, split by date.
#'
#' @import tidyverse
#' @import gt
#'
#' @export

human_device_msa_marbling <- function(df,
                                      df2,
                                      device_name,
                                      grader_count = 3) {
  dataset <- merge_device_grader(df, df2, "msa_marbling")

  dataset$general_human <- rowMeans(dataset[, 3:(2+grader_count)])
  # splitting by date
  df_list <- split(dataset, dataset$kill_date)

  result_list <- map(df_list, function(split_df) {
    temp_df <- split_df |>
      mutate(Date = unique(split_df$kill_date),
             diff = abs(split_df[[device_name]] - split_df$general_human),
             `<= 50` = ifelse(diff <= 50, 1, 0),
             `50-100` = ifelse(diff <= 100 & diff > 50, 1, 0),
             `100-200` = ifelse(diff <= 200 & diff > 100, 1, 0),
             `> 200` = ifelse(diff > 200, 1, 0), n = 1) |>
      select(`<= 50`, `50-100`, `100-200`, `> 200`, n) |>
      summarise_all(sum) |>
      mutate(Date = unique(split_df$kill_date),
             n = sum(`<= 50`, `50-100`, `100-200`, `> 200`)) |>
      mutate_at(vars(1:4), funs(sprintf("%.2f%%",./n*100))) |>
      select(Date, `<= 50`, `50-100`, `100-200`, `> 200`, n)

  })
  result_list |> bind_rows() |> gt() |>
    tab_header(paste0("MSA Marbling (Generalised Expert Grader vs ", device_name, ")"))
}

#' AMILSC Minimum Requirements Table (EMA)
#'
#' This function generates a table following the AMILSC recommended accuracy
#' standards for cut surface cameras. It computes a generalised expert grader by
#' taking the mean of the devices.
#'
#' @param df Human grader data-frame in long format
#' @param df2 Device data-frame in long format
#' @param device_name Name of the device that is to be compared with the generalised expert grader.
#' @param grader_count An integer specifying the number of expert graders present in df.
#' @return A GT() table following the recommended accuracy standards
#'
#' @import tidyverse
#' @importFrom stats prcomp
#' @import gt
#'
#' @export

human_device_ema <- function(df,
                             df2,
                             device_name,
                             grader_count = 3) {

  dataset <- merge_device_grader(df, df2, "ema")
  dataset$general_human <- rowMeans(dataset[, 3:(2+grader_count)])

  # splitting by date
  df_list <- split(dataset, dataset$kill_date)

  result_list <- map(df_list, function(split_df) {
    temp_df <- split_df |>
      mutate(diff = abs(split_df[[device_name]] - split_df$general_human),
             `<= 4` = ifelse(diff <= 4, 1, 0),
             `4-8` = ifelse(diff <= 8 & diff > 4, 1, 0),
             `8-12` = ifelse(diff <= 12 & diff > 8, 1, 0), n = 1) |>
      select(`<= 4`, `4-8`, `8-12`, n) |>
      summarise_all(sum) |>
      mutate(Date = unique(split_df$kill_date),
             n = sum(`<= 50`, `50-100`, `100-200`, `> 200`)) |>
      mutate_at(vars(1:4), funs(sprintf("%.2f%%",./n*100))) |>
      select(Date, `<= 50`, `50-100`, `100-200`, `> 200`, n)

  })
  result_list |> bind_rows() |> gt() |>
    tab_header(paste0("EMA (Generalised Expert Grader vs ", device_name, ")"))
}

#' AMILSC Minimum Requirements Table (AUS-Marbling)
#'
#' This function generates a table following the AMILSC recommended accuracy
#' standards for cut surface cameras. It computes a generalised expert grader by
#' taking the median of the devices.
#'
#' @param df Human grader data-frame in long format
#' @param df2 Device data-frame in long format
#' @param device_name Name of the device that is to be compared with the generalised expert grader.
#' @param grader_count An integer specifying the number of expert graders present in df.
#' @return A GT() table following the recommended accuracy standards
#'
#' @import tidyverse
#' @import gt
#'
#' @export
#'

human_device_aus_marbling <- function(df,
                                      df2,
                                      device_name,
                                      grader_count = 3) {

  dataset <- merge_device_grader(df, df2, "aus_marbling")
  dataset[, 3:ncol(dataset)] <- as.numeric(unlist(dataset[, 3:ncol(dataset)]))

  # getting median to create a "general" expert grader
  get_median <- function(rows) {
    unique_values <- unique(rows)
    if (length(unique_values) == 1) {
      return(unique_values)
    } else {
      return(median(rows, na.rm = TRUE))
    }
  }

  dataset$general_human <- apply(dataset[, 3:(2+grader_count)], MARGIN = 1, FUN = get_median)
  # splitting by date
  df_list <- split(dataset, dataset$kill_date)

  result_list <- map(df_list, function(split_df) {
    # splitting into high and low marbling ranges
    split_df$marbling_group <- cut(split_df$general_human, breaks = c(0, 6, 9), labels = c("0-6", "7-9"))
    marbling_ranges <- split(split_df, split_df$marbling_group)

    temp_df <- map(marbling_ranges, function(split_range) {
      if (nrow(split_range) != 0) {
        temp_df2 <- split_range |>
          mutate(diff = abs(split_range[[device_name]] - split_range$general_human),
                 `Exact Agreement` = ifelse(diff == 0, 1, 0),
                 `+= 1` = ifelse(diff == 1, 1, 0),
                 `> 1` = ifelse(diff > 1, 1, 0)) |> # removed n = 1
          select(`Exact Agreement`, `+= 1`, `> 1`) |>
          summarise_all(sum) |>
          mutate(Date = unique(split_range$kill_date),
                 `Marbling Range` = unique(split_range$marbling_group),
                 n = sum(`Exact Agreement`, `+= 1`, `> 1`)) |>
          mutate_at(vars(1:3), funs(sprintf("%.2f%%",./n*100))) |>
          select(Date, `Marbling Range`, `Exact Agreement`, `+= 1`, `> 1`, n)
      }
    })
    temp_df <- temp_df |> bind_rows()
  })
  result_list |> bind_rows() |> gt() |>
    tab_header(paste0("AUS Marbling (Generalised Expert Grader vs ", device_name, ")"))
}

#' AMILSC Minimum Requirements Table (Meat Colour)
#'
#' This function generates a table following the AMILSC recommended accuracy
#' standards for cut surface cameras. It computes a generalised expert grader by
#' taking the median of the devices.
#'
#' @param df Human grader data-frame in long format.
#' @param df2 Device data-frame in long format.
#' @param device_name Name of the device that is to be compared with the generalised expert grader.
#' @param grader_count An integer specifying the number of expert graders present in df.
#' @return A GT() table following the recommended accuracy standards
#'
#' @import tidyverse
#' @import gt
#'
#' @export
#'

human_device_meat_colour <- function(df,
                                     df2,
                                     device_name,
                                     grader_count = 3) {

  dataset <- merge_device_grader(df, df2, "meat_colour")
  dataset[, 3:ncol(dataset)] <- dataset[, 3:ncol(dataset)] |>
    mutate_all(~ ifelse(. == "1A", 1,
                        ifelse(. == "1B", 2,
                               ifelse(. == "1C", 3, as.numeric(as.character(.)) + 3))))

  # getting mode or median to create a "general" expert grader
  get_median <- function(rows) {
    unique_values <- unique(rows)
    if (length(unique_values) == 1) {
      return(unique_values)
    } else {
      return(median(rows, na.rm = TRUE))
    }
  }

  dataset$general_human <- apply(dataset[, 3:5], MARGIN = 1, FUN = get_median)
  df_list <- split(dataset, dataset$kill_date)

  result_list <- map(df_list, function(split_df) {
    temp_df <- split_df |>
      mutate(diff = abs(split_df[[device_name]] - split_df$general_human),
             `Exact Agreement` = ifelse(diff == 0, 1, 0),
             `+= 1` = ifelse(diff == 1, 1, 0),
             `> 1` = ifelse(diff > 1, 1, 0)) |> # removed n = 1
      select(`Exact Agreement`, `+= 1`, `> 1`) |>
      summarise_all(sum) |>
      mutate(Date = unique(split_df$kill_date),
             n = sum(`Exact Agreement`, `+= 1`, `> 1`)) |>
      mutate_at(vars(1:3), funs(sprintf("%.2f%%",./n*100))) |>
      select(Date, `Exact Agreement`, `+= 1`, `> 1`, n)

  })
  result_list |> bind_rows() |> gt() |>
    tab_header(paste0("Meat Colour (Generalised Expert Grader vs ", device_name, ")"))
}

#' AMILSC Minimum Requirements Table (Fat Colour)
#'
#' This function generates a table following the AMILSC recommended accuracy
#' standards for cut surface cameras. It computes a generalised expert grader by
#' taking the median of the devices.
#'
#' @param df Human grader data-frame in long format.
#' @param df2 Device data-frame in long format.
#' @param device_name Name of the device that is to be compared with the generalised expert grader.
#' @param grader_count An integer specifying the number of expert graders present in df.
#' @return A GT() table following the recommended accuracy standards.
#'
#' @import tidyverse
#' @import gt
#'
#' @export
#'

human_device_fat_colour <- function(df,
                                    df2,
                                    device_name,
                                    grader_count = 3) {
  dataset <- merge_device_grader(df, df2, "meat_colour")
  dataset[, 3:ncol(dataset)] <- dataset[, 3:ncol(dataset)] |>
    mutate_all(~ ifelse(. == "1A", 1,
                        ifelse(. == "1B", 2,
                               ifelse(. == "1C", 3, as.numeric(as.character(.)) + 3))))

  # getting mode or median to create a "general" expert grader
  get_median <- function(rows) {
    unique_values <- unique(rows)
    if (length(unique_values) == 1) {
      return(unique_values)
    } else {
      return(median(rows, na.rm = TRUE))
    }
  }

  dataset$general_human <- apply(dataset[, 3:(2+grader_count)], MARGIN = 1, FUN = get_median)
  df_list <- split(dataset, dataset$kill_date)

  result_list <- map(df_list, function(split_df) {
    temp_df <- split_df |>
      mutate(diff = abs(split_df[[device_name]] - split_df$general_human),
             `Exact Agreement` = ifelse(diff == 0, 1, 0),
             `+= 1` = ifelse(diff == 1, 1, 0),
             `> 1` = ifelse(diff > 1, 1, 0)) |>
      select(`Exact Agreement`, `+= 1`, `> 1`) |>
      summarise_all(sum) |>
      mutate(Date = unique(split_df$kill_date),
             n = sum(`Exact Agreement`, `+= 1`, `> 1`)) |>
      mutate_at(vars(1:3), funs(sprintf("%.2f%%",./n*100))) |>
      select(Date, `Exact Agreement`, `+= 1`, `> 1`, n)

  })
  result_list |> bind_rows() |> gt() |>
    tab_header(paste0("Meat Colour (Generalised Expert Grader vs ", device_name, ")"))
}
