

dat <- read.csv("data//monthly_one_group.csv")

dat$dte = as.Date(dat$dte)
dat$value = as.double(dat$value)
