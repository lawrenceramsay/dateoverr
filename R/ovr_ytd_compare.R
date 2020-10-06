#' Calculates the year to date number against previous year to date
#'
#' @param dat A data frame or data.table
#' @param date_col_name The column name of the date column as a character string
#' @param value_col_name The column of the values to be evaluated as a character string
#' @param period Periodicity the data. Can be "month" or "year"
#' @param output_col_name The name of the column in the output data as a character string
#' @param group_cols Columns to group by when evaluating. Up to two are allowed
#' @param skip_period Whether to skip periods in the and go against the most recent period or stick rigidly to the previous period. If false value will be NA for previous period. Only applicable when using group columns. Can produce strange results if true.
#' @param order_data Whether the data should come out ordered or not. Can produce strange results if false.
#' @param period_diff Difference of periods to compare. For example, using "month" period, use 1 for month over previous month. 12 for month over previous year
#' @param percentage Whether the period comparison comes out as a percentage or a absolute value. Defaults to absolute value.
#' @param alt_year_col_name If using a different year to the calendar year, then the name of the year column to use instead. As a character string.
#' @param output_cumsum Output the cumulative sum column used for calculation. Used for tractability of calculation
#' @return returns data frame or data table depending on the input data
#' @examples
#'
#'
#' @import lubridate
#' @impott dplyr
#' @export ovr_ytd_compare

ovr_ytd_compare <- function (dat, date_col_name, value_col_name, period,
                             output_col_name = "compare", group_cols = NULL,
                             skip_period = F, order_data = T, period_diff = 12,
                             percentage = F,
                             alt_year_col_name = NULL, output_cumsum = F) {

  #accepts data tables and data frames. Sets return value for conversion later
   if (data.table::is.data.table(dat)) {
    data_type = "dt"
  } else if (tibble::is_tibble(dat)) {
    data_type = "tb"
    dat <- data.table::as.data.table(dat)
  } else if (is.data.frame(dat)) {
    data_type = "df"
    dat <- data.table::as.data.table(dat)
  } else {
    stop("Data needs to be either a data frame or a data table.")
  }

  #check the columns are present
  if (!date_col_name %in% colnames(dat)) {
    stop("Date column name (date_col_name) needs to correspond to column in data")
  }

  if (!value_col_name %in% colnames(dat)) {
    stop("Value column name (value_col_name) needs to correspond to column in data")
  }

  if (!all(group_cols %in% colnames(dat))) {
    stop("Group columns (group_cols) need to correspond to columns in data")
  }

  #check the columns are in the correct format
  if (typeof(dat[,..date_col_name][[1]]) == "character") {
    stop("Date column must be a date type")
  }

  #use the alternate year or the calendar year
  if (is.null(alt_year_col_name)) {
    dat[,year. := lubridate::year(dat[,get(date_col_name)])]
  } else {
    dat[,year. := get(alt_year_col_name)]
  }

  #start of evaluation
  if(is.null(group_cols)) {

    if(order_data) setorderv(dat, c(date_col_name))

    if (skip_period == T) {

      #most basic form. No groups and skips non existent periods
      if (percentage) {
        #percentage change calculation
        # dat_output <- dat[, output :=  dat[,get(value_col_name)] -
        #                                   cumsum(dat[,get(value_col_name)]) /
        #                     shift(dat[,get(value_col_name)], period_diff, type = "lag")]

        #TODO: finish off the code for the groups
        dat_cum <- dat[,eval(parse(text = paste0("output_cumsum.:=cumsum(",value_col_name,")"))),
                       by=year.]

        dat_output <- dat_cum[,eval(parse(text = paste0("output.:=(output_cumsum.-shift(output_cumsum.,",
                                                        period_diff ,",type = \"lag\")) / ",
                                                        "shift(output_cumsum.,", period_diff ,",type = \"lag\")")))]

      } else {
        #difference calculation

        dat_cum <- dat[,eval(parse(text = paste0("output_cumsum.:=cumsum(",value_col_name,")"))),
                       by=lubridate::year(get(date_col_name))]

        dat_output <- dat_cum[,eval(parse(text = paste0("output.:=output_cumsum.-shift(output_cumsum.,",
                                                        period_diff ,",type = \"lag\")")))]

      }

    } else {

      #no groups and NA for non existent periods
      grid <- data.table::as.data.table(expand.grid(seq(min(dat[,..date_col_name][[1]]),
                                                        max(dat[,..date_col_name][[1]]), by = period)))

      #renames grid column for join
      names(grid)[1] <- date_col_name

      #joins to create all possibilities
      dat_grid <- merge(grid, dat, by = c(date_col_name), all.x = T)

      #shifts data
      if (percentage) {

        dat_cum <- dat_grid[,eval(parse(text = paste0("output_cumsum.:=cumsum(",value_col_name,")"))),
                       by=lubridate::year(get(date_col_name))]

        dat_shift <- dat_cum[,eval(parse(text = paste0("output.:=(output_cumsum.-shift(output_cumsum.,",
                                                        period_diff ,",type = \"lag\")) / ",
                                                        "shift(output_cumsum.,", period_diff ,",type = \"lag\")")))]

        #percentage change calculation
        # dat_shift <- dat_grid[, output :=  (dat_grid[,get(value_col_name)] -
        #                                       shift(dat_grid[,get(value_col_name)], period_diff, type = "lag")) /
        #                         shift(dat_grid[,get(value_col_name)], period_diff, type = "lag")]

      } else {

        dat_cum <- dat_grid[,eval(parse(text = paste0("output_cumsum.:=cumsum(",value_col_name,")"))),
                       by=lubridate::year(get(date_col_name))]

        dat_shift <- dat_cum[,eval(parse(text = paste0("output.:=output_cumsum.-shift(output_cumsum.,",
                                                        period_diff ,",type = \"lag\")")))]

        # dat_shift <- dat_grid[, output := dat_grid[,get(value_col_name)] -
        #                         shift(dat_grid[,get(value_col_name)], period_diff, type = "lag")]
      }

      #omits non used values
      dat_output <- na.omit(dat_shift, cols = value_col_name)

    }

  } else { #groups included

    if(order_data) setorderv(dat, c(date_col_name, group_cols))

    if (skip_period == T) {

      #shifts data
      if (percentage) {

        #with groups, skipping periods
        # expr <- parse(text = paste0("output:=(",value_col_name,
        #                             "-shift(",value_col_name,",", period_diff ,",type = \"lag\"))/",
        #                             "shift(",value_col_name,",", period_diff ,",type = \"lag\")"))

        dat_cum <- dat[,eval(parse(text = paste0("output_cumsum.:=cumsum(",value_col_name,")"))),
                            by=c(group_cols, "year.")]

        dat_output <- dat_cum[,eval(parse(text = paste0("output.:=(output_cumsum.-shift(output_cumsum.,",
                             period_diff ,",type = \"lag\")) / ",
                             "shift(output_cumsum.,", period_diff ,",type = \"lag\")"))),
                             by=group_cols]

        #dat_output <- dat_shift[,eval(expr)]

      } else {

        dat_cum <- dat[,eval(parse(text = paste0("output_cumsum.:=cumsum(",value_col_name,")"))),
                            by=lubridate::year(get(date_col_name))]

        dat_output <- dat_cum[,eval(parse(text = paste0("output.:=output_cumsum.-shift(output_cumsum.,",
                                                       period_diff ,",type = \"lag\")"))), by=group_cols]

        #with groups, skipping periods
        # expr <- parse(text = paste0("output:=",value_col_name,"-shift(",
        #                             value_col_name,",", period_diff ,",type = \"lag\")"))
        # dat_output <- dat[,eval(expr), by=group_cols]

      }

    } else {

      #no groups and NA for non existent periods
      #TODO make the group columns dynamic

      if (length(group_cols) == 2) {

        grid <- data.table::as.data.table(
          expand.grid(seq(min(dat[,..date_col_name][[1]]),max(dat[,..date_col_name][[1]]), by = period),
                      unique(dat[,get(group_cols[1])]), unique(dat[,get(group_cols[2])])
          ))

        names(grid)[1] <- date_col_name
        names(grid)[2] <- group_cols[1]
        names(grid)[3] <- group_cols[2]

      } else if (length(group_cols) == 1) {

        grid <- data.table::as.data.table(
          expand.grid(seq(min(dat[,..date_col_name][[1]]),max(dat[,..date_col_name][[1]]), by = period),
                      unique(dat[,get(group_cols[1])])
          ))

        names(grid)[1] <- date_col_name
        names(grid)[2] <- group_cols[1]

      } else {
        stop("Only 2 groups are allowed to be substituted by at the moment")
      }

      #joins to create all possibilities
      dat_grid <- merge(grid, dat, by = c(date_col_name, group_cols), all.x = T)

      dat_grid[,output_zero. := ifelse(is.na(dat_grid[,get(value_col_name)]),0,dat_grid[,get(value_col_name)])]

      #shifts data
      if (percentage) {

        dat_cum <- dat_grid[,eval(parse(text = paste0("output_cumsum.:=cumsum(output_zero.)"))),
                       by=c(group_cols, "year.")]

        dat_shift <- dat_cum[,eval(parse(text = paste0("output.:=(output_cumsum.-shift(output_cumsum.,",
                              period_diff ,",type = \"lag\")) / ",
                              "shift(output_cumsum.,", period_diff ,",type = \"lag\")"))),
                              by=group_cols]

        #with groups, skipping periods
        # expr <- parse(text = paste0("output:=(",value_col_name,
        #                             "-shift(",value_col_name,",", period_diff ,",type = \"lag\"))/",
        #                             "shift(",value_col_name,",", period_diff ,",type = \"lag\")"))
        # dat_shift <- dat_grid[,eval(expr), by=group_cols]

      } else {

        #with groups, skipping periods
        # expr <- parse(text = paste0("output:=",value_col_name,"-shift(",
        #                             value_col_name,",", period_diff ,",type = \"lag\")"))
        # dat_shift <- dat_grid[,eval(expr), by=group_cols]

        #group_cols = c(group_cols, "year.")

        dat_cum <- dat_grid[,eval(parse(text = paste0("output_cumsum.:=cumsum(output_zero.)"))),
                       by=c(group_cols, "year.")]

        dat_shift <- dat_cum[,eval(parse(text = paste0("output.:=output_cumsum.-shift(output_cumsum.,",
                              period_diff ,",type = \"lag\")"))), by=group_cols]


      }

      #omits non used values
      dat_output <- na.omit(dat_shift, cols = value_col_name)

    }

  }

  #sets the output column
  if(!output_cumsum) dat_output[, ("output_cumsum.") := NULL]

  #TODO: commented out for testing
  #dat_output[, c("output_zero.", "year.") := NULL]

  data.table::setnames(dat_output, old = "output.", new = output_col_name)

  #outputs into correct format
  if(data_type == "df") {
    return(as.data.frame(dat_output))
  } else if (data_type == "dt") {
    return(as.data.table(dat_output))
  } else if (data_type == "tb") {
    return(tibble::as_tibble(dat_output))
  }
}
