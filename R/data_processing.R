#' Wide-formatted Data-frame
#'
#' @param df Input data-frame in long format.
#' @param variable A valid column name denoting the variable of interest.
#' @param device A boolean denoting if this is the device data-frame; default assumes grader data-frame.
#' @param device_proc Summarises how the multiple device images should be dealt with. If it is a continuous variable,
#' you can specify to take either the mean or median as the value. Otherwise, specify an integer denoting the image number to take.
#' @return A wide data-frame of the variable of interest with graders as column names.
#'
#' @import tidyverse
#' @import dplyr
#' @import purrr
#'
#' @export

to_wide <- function(df,
                    variable,
                    device = FALSE,
                    device_proc = "mean") {

  # suppressing warnings as we know pivoting device data will make list-cols
  # (from the different images)
  suppressWarnings(new_df <- df |> pivot_wider(id_cols = c("kill_date", "body_no"),
                                               names_from = cold_grader,
                                               values_from = variable) |> drop_na())

  # attaining mean value of the images taken
  if (device) {
    if (device_proc %in% c("mean", "median")) {
      if (is.numeric(df[[variable]])) {
        new_df <- new_df |> mutate_at(vars(3:ncol(new_df)),
                                      ~ unlist(discard(lapply(., device_proc), is.na)))

        if (ncol(new_df) == 2) {
          stop("No data available: all entries had some image(s) which was NA.")
        }
      } else {
        # takes first image if device_proc not properly specified and trait not continuous
        new_df <- df[df$image == 1,] |> pivot_wider(id_cols = c("kill_date", "body_no"),
                      names_from = cold_grader, values_from = variable) |> drop_na()
      }
    } else if (device_proc %in% c(1, 2, 3)) {
      new_df <- df |> filter(image == device_proc) |>
        pivot_wider(id_cols = c("kill_date", "body_no"), names_from = cold_grader,
                    values_from = variable) |> drop_na()
    } else {
      stop("Invalid device processing parameter. Choose either mean, median or 1 of the images")
    }
  }
  return(new_df)
}

#' Merges 2 dataframes (device and grader) into wide format
#'
#' @param df1 Grader data-frame in long format.
#' @param df2 Device data-frame in long format.
#' @param variable A valid column name denoting the variable of interest.
#'
#' @return A merged data-frame of device and grader data, based on common body_no and kill_date combinations
#' @import tidyverse
#'
#'
#' @export

merge_device_grader <- function(df1, df2, variable) {
  grader <- to_wide(df1, variable, FALSE)
  device <- to_wide(df2, variable, TRUE)
  merged_df <- merge(grader, device, by.x = c("body_no", "kill_date"),
                     by.y = c("body_no", "kill_date"), all = FALSE)
  return(merged_df)
}

#' Merges 2 data-frames (device and grader) into long format
#'
#' @param df1 Grader data-frame in long format.
#' @param df2 Device data-frame in long format.
#' @param variable A valid column name denoting the variable of interest.
#'
#' @return A merged long data-frame of device and grader data, based on common body_no and kill_date combinations.
#' @import tidyverse
#'
#'
#' @export

merge_device_grader_long <- function(df1, df2, variable) {
  # currently just taking the first image
  df2 <- df2[df2$image == 1,]
  merged_df <- bind_rows(df1, df2)

  return(merged_df)
}
