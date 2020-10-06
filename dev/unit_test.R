
#TODO: might need to ignore weeks and days cannot get straight period

devtools::load_all()

period_test = "month" # day, week, month, year
skip_period_test = F # True, False
period_diff_test = 1 # number usually 1 or 12
percentage_test = F # T = as percentage,F = absolute

#data.table
set.seed(1)

dt_no_grp <- expand.grid(dte = seq(min(as.Date("2017-01-01")),max(as.Date("2020-05-01")),
                                   by = period_test))

dt_no_grp$value = round(runif(nrow(dt_no_grp), 0.1, 99.99),2)

dt_no_grp$alt_year <- ifelse(lubridate::month(dt_no_grp$dte)>=6,
                             lubridate::year(dt_no_grp$dte)+1,lubridate::year(dt_no_grp$dte))

#one group
dt_grp_1 <- expand.grid(dte = seq(min(as.Date("2017-01-01")),max(as.Date("2020-05-01")), by = period_test),
                        Group1 = c("A","B"))

dt_grp_1$value = round(runif(nrow(dt_grp_1), 0.1, 99.99),2)

dt_grp_1$alt_year <- ifelse(lubridate::month(dt_grp_1$dte)>=6,
                            lubridate::year(dt_grp_1$dte)+1,lubridate::year(dt_grp_1$dte))

#test with groups
dt_grp_2 <- expand.grid(dte = seq(min(as.Date("2017-01-01")),max(as.Date("2020-05-01")), by = period_test),
            Group1 = c("A","B"), Group2 = c("C", "D"))

dt_grp_2$value = round(runif(nrow(dt_grp_2), 0.1, 99.99),2)

dt_grp_2$alt_year <- ifelse(lubridate::month(dt_grp_2$dte)>=6,
                            lubridate::year(dt_grp_2$dte)+1,lubridate::year(dt_grp_2$dte))

#with skipped months
dt_no_grp_sub <- dt_no_grp[sample(1:nrow(dt_no_grp), nrow(dt_no_grp)/2, replace=FALSE),]
dt_grp_1_sub <- dt_grp_1[sample(1:nrow(dt_grp_1), nrow(dt_grp_1)/2, replace=FALSE),]
dt_grp_2_sub <- dt_grp_2[sample(1:nrow(dt_grp_2), nrow(dt_grp_2)/2, replace=FALSE),]

#month on month
#With and without groups
dateoverr::ovr_period_compare(dt_no_grp_sub, date_col_name = "dte",
                              value_col_name = "value", period = period_test,
                              percentage = percentage_test)

dateoverr::ovr_period_compare(dt_grp_1_sub, date_col_name = "dte", value_col_name = "value",
                              group_cols = c("Group1"), period = period_test,
                              period_diff = period_diff_test, skip_period = skip_period_test,
                              percentage = percentage_test)

dateoverr::ovr_period_compare(dt_grp_2_sub, date_col_name = "dte", value_col_name = "value",
                              group_cols = c("Group1", "Group2"), period = period_test,
                              period_diff = period_diff_test, skip_period = skip_period_test,
                              percentage = percentage_test)


#YTD compare
dateoverr::ovr_ytd_compare(dt_grp_1_sub, date_col_name = "dte", group_cols = c("Group1"),
                              value_col_name = "value", period = period_test, period_diff = 12,
                              percentage = percentage_test, skip_period = F, output_cumsum = T)
