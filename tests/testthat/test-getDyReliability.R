test_that("getDyReliability produces correct output ", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".",x_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  sat.config.script <-  scriptCFA(dvn, lvname = "Sat", model = "configural")

  sat.config.mod <- lavaan::cfa(sat.config.script, data = commitmentQ, std.lv = FALSE,
                        auto.fix.first= FALSE, meanstructure = TRUE)

  expect_equal(getDyReliability(dvn, sat.config.mod),
               structure(list(omega.1 = 0.946013641437488, omega.2 = 0.949657726446426), class = c("tbl_df",
                                                                                                   "tbl", "data.frame"), row.names = c(NA, -1L))
               )
})
