#' Add together two numbers
#'
#' @param dat A data frame or data.table
#' @param date_col_name The column name of the date column as a character string
#' @param value_col_name The column of the values to be evaluated
#' @param period Periodicity the data. Allowed periods denoted by seq.Date: "day", "week", "month", "quarter" or "year"
#' @param output_col_name The name of the column in the output data
#' @param group_cols Columns to group by when evaulating. Up to two are allowed
#' @param skip_period Whether to skip periods in the and go against the most recent period or stick rigidly to the previous period. If false value will be NA for preivous period. Only applicable when using group columns.
#' @param order_data Whether the data should come out ordered or not
#' @param period_diff difference of periods to compare. use 1 for period over previous period. 12 for year over year
#' @param percentage whether the period comparison comes out as a percentage or a absolute value. Defaults to absolute value
#' @return returns data frame or data table depending on the input dat
#' @examples
#' #test with groups
#' dt_grp_2 <- expand.grid(dte = seq(min(as.Date("2017-01-01")),
#'                  max(as.Date("2020-05-01")), by = "periods"),
#'                  Group1 = c("A","B"), Group2 = c("C", "D"))
#'
#' dt_grp_2$value = round(runif(nrow(dt_grp_2), 0.1, 99.99),2)
#'
#' #with skipped periods
#' dt_grp_2_sub <- dt_grp_2[sample(1:nrow(dt_grp_2),
#' nrow(dt_grp_2)/2, replace=FALSE),]
#'
#' dateoverr::ovr_period_compare(dt_grp_2, date_col_name = "dte",
#' value_col_name = "value", group_cols = c("Group1", "Group2"))
#'
#' #year on year without skipping periods
#' dateoverr::ovr_period_compare(dt_grp_2, date_col_name = "dte",
#' value_col_name = "value", group_cols = c("Group1", "Group2"),
#' period_diff = 12, skip_period = T)
#'
#' #period on period percentage without skipping periods
#' dateoverr::ovr_period_compare(dt_grp_2, date_col_name = "dte",
#' value_col_name = "value", group_cols = c("Group1", "Group2"),
#' period_diff = 1, skip_period = T, percentage = T)
#'
#' @import data.table
#' @importFrom data.table :=
#' @export ovr_period_compare

ovr_period_compare <- function (dat, date_col_name, value_col_name, period,
                                output_col_name = "compare", group_cols = NULL,
                                skip_period = F, order_data = T, period_diff = 1,
                                percentage = F) {

  #data_type = data.table, data.frame or tibble

  #Check if the dates are real dates
  #Check if they are monthly
  #check numbers are numbers

  #browser()

  #want the changes since the previous month or flexibly to the last data point
  #skip months is whether the months are skipped or rigid to the prvious one

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
  if (typeof(dat[,date_col_name,with=F][[1]]) == "character") {
    stop("Date column must be a date type")
  }

  #start of evaluation
  if(is.null(group_cols)) {

    if(order_data) setorderv(dat, c(date_col_name))

    if (skip_period == T) {

      #most basic form. No groups and skips non existent months
      if (percentage) {
        #percentage change calculation
        dat_output <- dat[, output :=  (dat[,get(value_col_name)] -
                                        shift(dat[,get(value_col_name)], period_diff, type = "lag")) /
                                        shift(dat[,get(value_col_name)], period_diff, type = "lag")]

      } else {
        #difference calculation
        dat_output <- dat[, output :=  dat[,get(value_col_name)] -
                            shift(dat[,get(value_col_name)], period_diff, type = "lag")]
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
        #percentage change calculation
        dat_shift <- dat_grid[, output :=  (dat_grid[,get(value_col_name)] -
                                            shift(dat_grid[,get(value_col_name)], period_diff, type = "lag")) /
                                            shift(dat_grid[,get(value_col_name)], period_diff, type = "lag")]

      } else {

        dat_shift <- dat_grid[, output := dat_grid[,get(value_col_name)] -
                                shift(dat_grid[,get(value_col_name)], period_diff, type = "lag")]
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
        expr <- parse(text = paste0("output:=(",value_col_name,
                                    "-shift(",value_col_name,",", period_diff ,",type = \"lag\"))/",
                                    "shift(",value_col_name,",", period_diff ,",type = \"lag\")"))
        dat_output <- dat[,eval(expr), by=group_cols]

      } else {

        #with groups, skipping periods
        expr <- parse(text = paste0("output:=",value_col_name,"-shift(",
                                    value_col_name,",", period_diff ,",type = \"lag\")"))
        dat_output <- dat[,eval(expr), by=group_cols]

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


      #shifts data
      if (percentage) {

        #with groups, skipping periods
        expr <- parse(text = paste0("output:=(",value_col_name,
                                    "-shift(",value_col_name,",", period_diff ,",type = \"lag\"))/",
                                    "shift(",value_col_name,",", period_diff ,",type = \"lag\")"))
        dat_shift <- dat_grid[,eval(expr), by=group_cols]

      } else {

        #with groups, skipping periods
        expr <- parse(text = paste0("output:=",value_col_name,"-shift(",
                                    value_col_name,",", period_diff ,",type = \"lag\")"))
        dat_shift <- dat_grid[,eval(expr), by=group_cols]

      }

      #omits non used values
      dat_output <- na.omit(dat_shift, cols = value_col_name)

    }

  }

  #sets the output column
  data.table::setnames(dat_output, old = "output", new = output_col_name)

  #outputs into correct format
  if(data_type == "df") {
    return(as.data.frame(dat_output))
  } else if (data_type == "dt") {
    return(as.data.table(dat_output))
  } else if (data_type == "tb") {
    return(tibble::as_tibble(dat_output))
  }

}
