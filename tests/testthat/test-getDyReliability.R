test_that("getDyReliability produces correct output ", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".",x_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  sat.indist.script <-  scriptCFA(dvn, lvname = "Sat",
                                  constr_dy_meas = c("loadings", "intercepts", "residuals"),
                                  constr_dy_struct = c("variances","means"), scaleset = "FF")

  sat.indist.mod <- lavaan::cfa(sat.indist.script, data = commitmentQ, std.lv = FALSE,
                        auto.fix.first= FALSE, meanstructure = TRUE)

  expect_equal(getDyReliability(dvn, sat.indist.mod),
               structure(list(omega.1 = 0.94770369, omega.2 = 0.94770369), class = c("tbl_df",
                                                                                                   "tbl", "data.frame"), row.names = c(NA, -1L))
               )
})
