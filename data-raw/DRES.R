## code to prepare `DRES` dataset goes here
library(foreign)
library(tidyverse)

DRES <- read.spss(file.choose(), to.data.frame = TRUE, use.value.labels = FALSE)

DRES<- filter(DRES, Day==1, Partner==1)

DRES<- select(DRES, B_A_PRQC_1.1:B_A_PRQC_9.1,
              B_P_PRQC_1.2:B_P_PRQC_9.2,
              B_A_sexsat1.1:B_A_sexsat5.1,
              B_P_sexsat1.2:B_P_sexsat5.2)

names(DRES)<-c("PRQC_1.1", "PRQC_2.1", "PRQC_3.1", "PRQC_4.1", "PRQC_5.1", "PRQC_6.1", "PRQC_7.1", "PRQC_8.1", "PRQC_9.1",
               "PRQC_1.2", "PRQC_2.2", "PRQC_3.2", "PRQC_4.2", "PRQC_5.2", "PRQC_6.2", "PRQC_7.2", "PRQC_8.2", "PRQC_9.2",
               "sexsat1.1", "sexsat2.1", "sexsat3.1", "sexsat4.1", "sexsat5.1",
               "sexsat1.2", "sexsat2.2", "sexsat3.2", "sexsat4.2", "sexsat5.2")

usethis::use_data_raw()

usethis::use_data("DRES")
