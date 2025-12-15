library(dplyr)
library(readxl)
library(psych)
pnrqMraw <- read_excel(file.choose())

#select down
pnrqM <- pnrqMraw |>
  select(sat.pnrq1_w:dsat.pnrq4_w,
         sat.pnrq1_m:dsat.pnrq4_m)

#check ranges
describe(pnrqM)

usethis::use_data(pnrqM, overwrite = TRUE)
