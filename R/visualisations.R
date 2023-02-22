# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Pairwise Scatterplots
#'
#' This function plots pairwise scatter-plots of all graders in the input
#' data-frame on any variable of interest.
#'
#' @param df Data-frame in long format.
#' @param df2 If not null, df2 must be the device data-frame in long format.
#' @param variable A valid column name denoting the variable of interest
#' @param line A boolean for plotting y = x; default is set to TRUE.
#' @param device_status A boolean denoting if df is the device data-frame; default assumes grader data-frame.
#' This parameter is arbitrary if your df2 is not null.
#' @return A pairwise scatterplot of all graders' assessment on the variable of interest
#'
#' @examples
#' data("expert_graders", package = "meatrics")
#' pairwise_scatter(expert_graders, variable = "ema", line = TRUE, device = FALSE)
#'
#' pairwise_scatter(expert_graders, variable = "msa_marbling", line = FALSE, device = FALSE)
#'
#' @import tidyr
#' @import ggplot2
#' @import cowplot
#'
#' @export

pairwise_scatter <- function(df,
                             device_status = FALSE,
                             df2 = NULL,
                             variable,
                             line = TRUE) {

  # has the option to merge dataframes but can get very messy!
  if (!is.null(df2)) {
    new_df <- merge_device_grader(df, df2, variable)
  } else {
    new_df <- to_wide(df, variable, device_status)
  }

  columns <- names(new_df)
  plots <- list()

  for (i in 3:(length(columns)-1)) {
    for (j in (i+1):length(columns)) {
      p <- ggplot(new_df, aes_string(x = as.name(columns[i]), y = as.name(columns[j]))) +
        geom_point(size = 0.3) + ggtitle(paste(columns[i], "vs", columns[j])) + coord_equal() + theme_bw()

      # resizing title as devices seem to have longer names
      if (device_status) {
        p <- p + theme(plot.title = element_text(size = 6))
      }

      if (line) {
        p <- p + geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = 'red') +
          geom_smooth(formula = y ~ x, method = "lm", size = 0.5)
      }

      plots[[paste(columns[i],columns[j])]] <- p
    }
  }

  plot_grid(plotlist = plots, ncol = 3)
}

#' Bar chart of category-wise Fleiss Kappa
#'
#' @param df Input data-frame in long format; must be grader data-frame if df2 is not null.
#' @param device_status A boolean denoting if df is the device data-frame; default assumes grader data-frame.
#' This parameter should be set to FALSE if your df2 is not null.
#' @param variable A valid column name denoting the categorical variable of interest.
#' @param df2 Device data-frame in long format.
#' @return A bar chart of category-wise Fleiss Kappa
#'
#' @import irr
#' @import ggplot2
#'
#' @examples
#' data("expert_graders", package = "meatrics")
#' fleiss(expert_graders, device_status = FALSE, variable = "meat_colour")
#'
#' @export

fleiss <- function(df,
                   device_status = FALSE,
                   df2 = NULL,
                   variable) {

  if (variable == "meat_colour") {
    all_grades <- c("1A", "1B", "1C", "2", "3", "4", "5", "6", "7")
  } else if (variable == "fat_colour" | variable == "aus_marbling") {
    all_grades <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  } else {
    stop("Trait entered is not valid. Discrete traits supported only, following the naming
         convention: meat_colour, fat_colour, aus_marbling")
  }

  new_df <- to_wide(df, variable, device_status)
  kappa_result <- kappam.fleiss(new_df[,-c(1:2)], detail=TRUE)
  kappa_df <- as.data.frame(kappa_result$detail) |> filter(Var2 == "Kappa") |>
    complete(Var1 = all_grades, fill = list(Freq = 0))

  if (!device_status) {
    kappa_df$device <- "Human"
  } else {
    kappa_df$device <- "Device"
  }

  if (!is.null(df2)) {
    device_df <- to_wide(df2, variable, TRUE)
    kappa_result_device <- kappam.fleiss(device_df[,-c(1:2)], detail=TRUE)
    kappa_result_device <- as.data.frame(kappa_result_device$detail) |>
      filter(Var2 == "Kappa") |> complete(Var1 = all_grades, fill = list(Freq = 0))
    kappa_result_device$device <- "Device"

    kappa_df <- kappa_df |>
      bind_rows(kappa_result_device)
  }

  fat_colour <- c("0" = "#fffcf5",
                  "1" = "#F5F4ED",
                  "2" = "#fdf8e5",
                  "3" = "#fcf7da",
                  "4" = "#fef1d2",
                  "5" = "#fdeabf",
                  "6" = "#fcdea8",
                  "7" = "#fcd497",
                  "8" = "#fbca86",
                  "9" = "#EEAB3F")

  meat_colour <- c("1A" = "#f78d85",
                   "1B" = "#e36c5c",
                   "1C" = "#cb5949",
                   "2" = "#b13e36",
                   "3" = "#a53b34",
                   "4" = "#98362d",
                   "5" = "#843726",
                   "6" = "#7f3135",
                   "7" = "#300001")

  if (variable == "meat_colour") {
    if (is.null(df2)) {
      kappa_df |> ggplot(aes(x = Var1, y = Freq, fill = Var1)) +
        geom_bar(stat="identity", color = "black") +
        scale_fill_manual(values = meat_colour) +
        ggtitle("Meat Colour Fleiss Kappa") +
        labs(x = "Meat Colour Grades", y = "Kappa", fill = "Grades") +
        theme_bw()
    } else {
      kappa_df |> ggplot(aes(x = Var1, y = Freq, fill = device)) +
        geom_bar(position = "dodge", stat = "identity", color = "black") +
        ggtitle("Meat Colour Fleiss Kappa") +
        labs(x = "Meat Colour Grades", y = "Kappa", fill = "Device Type") +
        theme_bw()
    }
  } else if (variable == "fat_colour") {
    if (is.null(df2)) {
      kappa_df |> ggplot(aes(x = Var1, y = Freq, fill = Var1)) +
        geom_bar(stat="identity", color = "black") +
        scale_fill_manual(values = fat_colour) +
        ggtitle("Fat Colour Fleiss Kappa") +
        labs(x = "Fat Colour Grades", y = "Kappa", fill = "Grades") +
        theme_bw()
    } else {
      kappa_df |> ggplot(aes(x = Var1, y = Freq, fill = device)) +
        geom_bar(position = "dodge", stat = "identity", color = "black") +
        ggtitle("Fat Colour Fleiss Kappa") +
        labs(x = "Fat Colour Grades", y = "Kappa", fill = "Device Type") + theme_bw()
    }
  } else if (variable == "aus_marbling") {
    if (is.null(df2)) {
      red <- colorRampPalette(c("white", "red"))(10)
      kappa_df |> ggplot(aes(x = Var1, y = Freq, fill = Var1)) +
        geom_bar(stat="identity", color = "black") +
        scale_fill_manual(values = red) +
        ggtitle("AUS Marbling Fleiss Kappa") +
        labs(x = "AUS Marbling Grades", y = "Kappa", fill = "Grades") + theme_bw()
    } else {
      kappa_df |> ggplot(aes(x = Var1, y = Freq, fill = device)) +
        geom_bar(position = "dodge", stat = "identity", color = "black") +
        ggtitle("AUS Marbling Fleiss Kappa") +
        labs(x = "AUS Marbling Grades", y = "Kappa") +
        labs(fill = "Device Type") + theme_bw()
    }
  }
}


#' Pairwise CCC Radar Graph
#'
#' This function computes pairwise CCC for all graders present in the data-frame
#' and plots it on a radar chart. It allows for partitions in the variable of
#' interest to be made so that differences can be more clearly observed along the
#' entire range of the variable. If two data-frames are input, this function will
#' assume that the first data-frame belongs to the expert graders and will compute
#' the mean of the expert graders' ratings, creating a representative expert grader to compare
#' against each device.
#'
#' @param df Input data-frame in long format.
#' @param device_status A boolean denoting if df is the device data-frame; default assumes grader data-frame.
#' This parameter is arbitrary if your df2 is not null.
#' @param df2 Device data-frame in long format.
#' @param variable A valid column name denoting the variable of interest.
#' @param partitions An integer specifying how many groups to split the variable into.
#' @param message A boolean denoting if the CCC values should be printed to terminal.
#' @return A radar chart of pairwise CCC.
#'
#' @import fmsb
#' @import tibble
#' @importFrom DescTools CCC
#'
#' @examples
#' data("expert_graders", package = "meatrics")
#' pairwise_ccc(expert_graders, device_status = FALSE, variable = "ema", partitions = 5)
#'
#' data("devices", package = "meatrics")
#' pairwise_ccc(expert_graders, device_status = FALSE, df2 = devices, variable = "msa_marbling", partitions = 5)
#'
#' @export
#'

pairwise_ccc <- function(df,
                         device_status = FALSE,
                         df2 = NULL,
                         variable,
                         partitions = 1,
                         message = FALSE) {

  # checking that a whole number is given as input
  if (is.numeric(partitions)) {
    if (round(partitions) == partitions) {
      if (partitions > 5) {
        stop("Maximum number of partitions supported is 5.")
      }

      cut_points <- seq(min(df[[variable]], na.rm = TRUE),
                        max(df[[variable]], na.rm = TRUE),
                        length.out = partitions+1)
      data_list <- list()

      if (partitions == 1) {
        if (is.null(df2)) {
          data_list[[1]] <- to_wide(df, variable, device_status)
        } else {
          data_list[[1]] <- merge_device_grader(df, df2, variable)
          data_list[[1]]$`Expert Grader` <- rowMeans(data_list[[1]][, 3:(2+length(unique(df$cold_grader)))])
          data_list[[1]] <- data_list[[1]][, -(3:(2+length(unique(df$cold_grader))))]
        }
      } else {
        if (is.null(df2)) {
          data_list <- split(df, cut(df[[variable]], breaks = cut_points, right = FALSE))
        } else {
          merged_df <- merge_device_grader_long(df, df2, variable)
          cut_points <- seq(min(merged_df[[variable]], na.rm = TRUE),
                            max(merged_df[[variable]], na.rm = TRUE),
                            length.out = partitions+1)
          data_list <- split(merged_df, cut(merged_df[[variable]],
                                            breaks = cut_points,
                                            right = FALSE))
        }
        for (i in 1:length(data_list)) {
          data_list[[i]] <- to_wide(data_list[[i]], variable, device_status)

          if (!is.null(df2)) {
            data_list[[i]]$`Expert Grader` <- rowMeans(data_list[[i]][, 3:(2+length(unique(df$cold_grader)))])
            data_list[[i]] <- data_list[[i]][, -(3:(2+length(unique(df$cold_grader))))]
          }
        }
      }
    } else {
        stop("Please enter a whole number for partitions.")
      }
  } else {
    stop("Please enter a numeric partition.")
  }

  subset_cols <- tail(names(data_list[[1]]), n = ncol(data_list[[1]]) - 2)
  # getting pairwise combinations of graders
  col_combinations <- combn(subset_cols, 2)

  # initialise results dataframe
  result_df <- data.frame(graders = character(), correlation = double(),
                          rownames = character())

  for (j in 1:length(data_list)) {
    for (i in 1:ncol(col_combinations)) {
      col1 <- col_combinations[1,i]
      col2 <- col_combinations[2,i]

      ccc_results <- CCC(data_list[[j]][[col1]], data_list[[j]][[col2]],
                         ci = "z-transform", conf.level = 0.95)

      temp_df <- data_list[[j]] |>
        mutate(graders = paste(col1, "&", col2)) |>
        group_by(graders) |>
        summarize(correlation = ccc_results$rho.c$est,
                  rownames = paste0(round(cut_points[j], 2), "-",
                                    round(cut_points[j+1], 2)))

      result_df <- bind_rows(result_df, temp_df)
    }
  }

  result_df <- result_df |> pivot_wider(names_from = graders, values_from = correlation)
  # adding rows of maximum and minimum for radar chart
  new_row1 <- data.frame(matrix(1, nrow = 1, ncol = ncol(result_df)))
  new_row2 <- data.frame(matrix(-1, nrow = 1, ncol = ncol(result_df)))
  colnames(new_row1) <- colnames(result_df)
  colnames(new_row2) <- colnames(result_df)
  result_df <- rbind(new_row1, new_row2, result_df)

  # assigning rownames
  rownames(result_df) <- result_df$rownames
  rownames(result_df)[1:2] <- c("Max", "Min")

  colours <- c("#00AFBB", "#E7B800", "#FC4E07", "#A020F0", "#00FF00")

  radarchart(result_df |> select(-rownames), cglcol = "grey", cglty = 1, cglwd = 0.8,
             pcol = colours, pfcol = scales::alpha(colours, 0.1), plwd = 2,
             plty = 1, vlcex = 0.5)

  legend(
    x = "bottom", legend = rownames(result_df[-c(1,2),]), horiz = TRUE,
    bty = "n", pch = 20 , col = colours,
    text.col = "black", cex = 1, pt.cex = 1
  )

  if (message) {
    print(result_df[-c(1, 2), -c(1)])
  }
}

#' ICC3 and ICC3K Line Chart over Time
#'
#' @param df Input dataframe in long format.
#' @param df2 Input device dataframe in long format.
#' @param variable A valid column name denoting the variable of interest.
#' @param filtered A character vector of graders you want included in the ICC; can be a mix of both dataframes, If it does not include any grader name from one data-frame, all graders from that data-frame will be used.
#' @return A line chart of ICC3 and ICC3k with error bars.
#'
#' @import purrr
#' @importFrom psych ICC
#'
#' @examples
#' data("expert_graders", package = "meatrics")
#' data("devices", package = "meatrics")
#' icc_line(expert_graders, df2 = devices, variable = "msa_marbling")
#'
#' @export
#'
icc_line <- function(df,
                     df2 = NULL,
                     variable,
                     filtered = NULL) {

  if (!is.null(filtered)) {
    temp_df <- df |> filter(cold_grader %in% filtered)

    if (nrow(temp_df) != 0) {
      df <- temp_df
    }
  }

  df_new <- to_wide(df, variable)
  if (!is.null(df2)) {
    if (!is.null(filtered)) {
      temp_df <- df2 |> filter(cold_grader %in% filtered)

      if (nrow(temp_df) != 0) {
        df2 <- temp_df
      }
    }

    df <- merge_device_grader(df, df2, variable)
    icc_values <- lapply(split(df[,3:ncol(df)], df$kill_date),
                         function(x) ICC(as.data.frame(lapply(x, as.numeric))))
  } else {
    icc_values <- lapply(split(df_new[,3:ncol(df_new)], df_new$kill_date),
                         function(x) ICC(as.data.frame(lapply(x, as.numeric))))
  }

  # extracting icc3 value, upper and lower bound from the table
  icc3_values_df <- map2_df(icc_values, names(icc_values),
                           ~ data.frame(kill_date = .y, ICC3 = .x$results$ICC[3],
                                        lower = .x$results$`lower bound`[3],
                                        upper = .x$results$`upper bound`[3]))

  icc3_values_df$type <- "Single-rater"

  icc3k_values_df <- map2_df(icc_values, names(icc_values),
                            ~ data.frame(kill_date = .y, ICC3 = .x$results$ICC[6],
                                         lower = .x$results$`lower bound`[6],
                                         upper = .x$results$`upper bound`[6]))

  icc3k_values_df$type <- "Mean"

  icc_values_df <- bind_rows(icc3k_values_df, icc3_values_df)

  ggplot(icc_values_df, aes(x = kill_date, y = ICC3, group = type, color = type)) +
    geom_point(color = "red") +
    geom_line(size = 0.5) + theme_bw() +
    scale_y_continuous(limits = c(0, 1)) +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    labs(x = "Date", color = "ICC3 Type") +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5) +
    geom_hline(yintercept = 0.75, color = "blue", linetype = "dotted", size = 1) +
    geom_hline(yintercept = 0.9, color = "purple", linetype = "dotted", size = 1)
}

#' ICC Line Chart over Time (Singular Device v Group of Expert Graders)
#'
#' @param df Input expert grader data-frame in long format.
#' @param df2 Input device data-frame in long format.
#' @param variable A valid column name denoting the variable of interest.
#' @param grader_count An integer denoting the number of expert graders there are.
#' @return A line chart of ICC.
#'
#' @import purrr
#' @importFrom psych ICC
#'
#' @examples
#' data("expert_graders", package = "meatrics")
#' data("devices", package = "meatrics")
#' icc_device_comparison(expert_graders, devices, variable = "msa_marbling", grader_count = 3)
#'
#' @export

icc_device_comparison <- function(df,
                                  df2,
                                  variable,
                                  grader_count) {

  merged_df <- merge_device_grader(df, df2, variable)

  if (nrow(merged_df) == 0) {
    stop("No common dates to merge the dataframes on.")
  }

  df_list <- map((3+grader_count):ncol(merged_df),
                 ~ merged_df[, c(1:(2+grader_count), .x)])

  icc_values_list <- lapply(df_list, function(df) {
    # split the data frame and apply the ICC function

    temp <- lapply(split(df[,3:ncol(df)], df$kill_date),
                   function(x) ICC(as.data.frame(lapply(x, as.numeric))))

    icc_values_df <- map2_df(temp, names(temp),
                             ~ data.frame(kill_date = .y, ICC3 = .x$results$ICC[6],
                                          lower = .x$results$`lower bound`[6],
                                          upper = .x$results$`upper bound`[6],
                                          graders = paste(colnames(df[,3:ncol(df)]),
                                                          collapse = ",")))
    return(icc_values_df)
  })

  all_icc <- bind_rows(icc_values_list)

  ggplot(all_icc, aes(x = kill_date, y = ICC3, color = graders, group = graders)) +
    geom_point(color = "red") +
    geom_line(size = 0.5) + theme_bw() +
    scale_y_continuous(limits = c(0, 1)) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "Date", color = "Grader Combinations") +
    geom_hline(yintercept = 0.75, color = "blue", linetype = "dotted", size = 1) +
    geom_hline(yintercept = 0.9, color = "purple", linetype = "dotted", size = 1)
}


#' Extended BAC Plot
#'
#' This function generates an extended Bland-Altman Plot from Möller et al. LOA
#' is currently not supported in this.
#'
#' @param df Input dataframe in long format.
#' @param device_status A boolean denoting if df is the device data-frame; default assumes grader data-frame.
#' @param variable A valid column name denoting the variable of interest.
#' @param grader_count An integer specifying the number of graders there are.
#' @return BAC plot (without LOA)
#'
#' @import nnet
#' @import boot
#'
#' @examples
#' data("expert_graders", package = "meatrics")
#' extended_bac(expert_graders, device_status = FALSE, variable = "ema", grader_count = 3)
#'
#' @export

extended_bac <- function(df,
                         device_status = FALSE,
                         variable,
                         grader_count) {
  bac_dat <- to_wide(df, variable, device_status)
  bac_dat$m <- apply(bac_dat[,3:ncol(bac_dat)], 1, mean)
  bac_dat$s <- apply(bac_dat[,3:ncol(bac_dat)], 1, sd)
  n <- dim(bac_dat)[1]
  s <- mean(bac_dat$s)
  # LOA
  L = sqrt(qchisq(0.95,grader_count-1)) * 1 / sqrt(grader_count-1)*s
  # bias
  bias = apply(abs(bac_dat[, -which(names(bac_dat) %in% c("kill_date", "body_no", "m", "s"))] - bac_dat$m), 2, mean)
  whichmax = apply(abs(bac_dat[, -which(names(bac_dat) %in% c("kill_date", "body_no", "m", "s"))] - bac_dat$m), 1, which.is.max)


  funL <- function(df,indices,mm){ #mm denotes m, number of graders
    d <- df[indices,] #save all cols of the df for given rows (indices) to a variable d
    d$s = apply(d[,c("B8","C8","B2")],1,sd) #create a col in d which is the sd between graders for each animal
    s = mean(d$s) #calculate the mean of those sds
    return(sqrt(qchisq(0.95,mm-1)) * 1 / sqrt(mm-1) * s) #
  }

  #b = boot(data = bac_dat,statistic=funL,R= nrow(bac_dat),mm=3) #generate nrow bootstrap replicates, with 3 predictions each, of statistic in funL applied to ema_bac_dat.
  #ci = boot.ci(b, type="bca") #calculate CI of the bootstrapped data claculated in the line above, using the adjusted bootstrap percentile method (BCa).
  #Lll = ci$bca[4] #saving the value of the lower limit of the 95% CI of LOA as Lll
  #Lul = ci$bca[5] #saving the value of the upper limit of the 95% CI of LOA as Lul


  plot(bac_dat$m, rep(NA,times=n), ylim = c(0, max(bac_dat$s)), main=paste0("Human Graders"), pch=20,
       xlab="mean", ylab="standard deviation", xaxt="n")

  for (i in 1:grader_count){
    points(subset(bac_dat$m,whichmax==i), subset(bac_dat$s,whichmax==i), pch=20, col=i) #colour the grader which is farthest from the mean
  }
  for (i in 1:grader_count){
    axis(4,at=abs(bias[i]),lab=NA,col.ticks=i) #add an axis to the RHS of the plot, signifying the bias
  }

  legend("top",pch=20,col=1:grader_count,legend=1:grader_count, ncol=grader_count,bty="n",cex=.7)
  #axis(1,at=seq(4.5,7,0.5))


  #add lines at LOA and 95% CI of LOA
  abline(L,0)
  #abline(Lll,0,lty=3)
  #abline(Lul,0,lty=3)
}

#' Histogram of Grader Differences
#'
#' This function computes pairwise differences of all graders available in the
#' data-frame and outputs the result as a histogram. It then summarises these differences
#' by either taking the mean, median or maximum difference. It allows for partitions in
#' the variable of interest to be made so that differences can be more clearly
#' observed along the entire range of the variable.
#'
#' @param df Input dataframe in long format.
#' @param device_status A boolean denoting if df is the device data-frame; default assumes grader data-frame.
#' @param variable A valid column name denoting the variable of interest.
#' @param partitions An integer specifying how many groups to split the variable into.
#' @param grader_count An integer specifying how many graders are in the data-frame.
#' @param bins An integer specifying what bin-size.
#' @param diff_type Either "mean", "median" or "maximum", specifying what kind of difference to take; default is mean.
#' @return A ggplot2 histogram of absolute differences.
#'
#' @examples
#' data("expert_graders", package = "meatrics")
#' diff_histogram(expert_graders, variable = "msa_marbling", partitions = 4, grader_count = 3)
#'
#' @import ggplot2
#'
#' @export

diff_histogram <- function(df,
                           device_status = FALSE,
                           variable,
                           partitions = 1,
                           grader_count = 3,
                           bins = 30,
                           diff_type = "mean") {

  new_df <- to_wide(df, variable, device_status)
  new_df$average <- rowMeans(new_df[3:ncol(new_df)])
  cut_points <- seq(min(new_df$average-1, na.rm = TRUE),
                    max(new_df$average, na.rm = TRUE), length.out = partitions+1)
  new_df$mean_group <- cut(new_df$average, breaks = cut_points,
                           labels = paste0(round(cut_points[-length(cut_points)]),
                                           " - ", round(cut_points[-1])))
  col_combinations <- combn(colnames(new_df[, 3:(2 + grader_count)]), 2)
  new_df$average_difference <- NA
  x_axis <- list()

  for (i in 1:nrow(new_df)) {
    row_differences <- apply(col_combinations, 2, function(x)
      abs(as.numeric(new_df[i,x[2]]) - as.numeric(new_df[i,x[1]])))

    x_axis[[i]] <- max(row_differences)
    if (diff_type == 'mean') {
      new_df$average_difference[i] <- mean(row_differences)
      x <- "Mean Differences"
    } else if (diff_type == 'median') {
      new_df$average_difference[i] <- median(row_differences)
      x <- "Median Differences"
    } else if (diff_type == 'maximum') {
      new_df$average_difference[i] <- max(row_differences)
      x <- "Maximum Differences"
    } else {
      stop("Invalid difference type. Valid inputs: mean, median, maximum")
    }
  }

  ggplot(new_df, aes(x = average_difference, fill = mean_group)) +
    geom_histogram(alpha = 0.5, position = "identity", bins = bins) +
    scale_fill_brewer(palette = "Set1") +
    labs(x = x, y = "Frequency", fill = "Partitions") +
    expand_limits(x = max(unlist(x_axis))) + theme_bw()
}
