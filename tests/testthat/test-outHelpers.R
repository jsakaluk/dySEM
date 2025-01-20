#### CFA measurement ####

test_that("makeTable creates propper tibble for model = cfa and tabletyle = measurement", {

  dvn <- scrapeVarCross(dat = commitmentM, x_order = "sip", x_stem = "sat.g",
                        x_delim2="_", distinguish_1="f", distinguish_2="m")

  script <- scriptCFA(dvn, lvname = "Sat",
                                 constr_dy_meas = c("loadings", "intercepts", "residuals"),
                                 constr_dy_struct = "none")

  fit <- lavaan::cfa(script, data = commitmentM, std.lv = FALSE,
                                auto.fix.first= FALSE, meanstructure = TRUE)

  expect_equal(dySEM:::makeTable(dvn, fit, model = "cfa", tabletype = "measurement", gtTab = FALSE),
               structure(list(`Latent Factor` = c("Satf", "Satf", "Satf", "Satf",
                                                  "Satf", "Satm", "Satm", "Satm", "Satm", "Satm"), Indicator = c("sat.g1_f",
                                                                                                                 "sat.g2_f", "sat.g3_f", "sat.g4_f", "sat.g5_f", "sat.g1_m", "sat.g2_m",
                                                                                                                 "sat.g3_m", "sat.g4_m", "sat.g5_m"), Loading = c(1.929, 1.742,
                                                                                                                                                                  2.087, 1.985, 2.082, 1.929, 1.742, 2.087, 1.985, 2.082), SE = c(0.089,
                                                                                                                                                                                                                                  0.093, 0.096, 0.089, 0.098, 0.089, 0.093, 0.096, 0.089, 0.098
                                                                                                                                                                  ), Z = c(21.725, 18.699, 21.67, 22.281, 21.301, 21.725, 18.699,
                                                                                                                                                                           21.67, 22.281, 21.301), `p-value` = c("< .001", "< .001", "< .001",
                                                                                                                                                                                                                 "< .001", "< .001", "< .001", "< .001", "< .001", "< .001", "< .001"
                                                                                                                                                                           ), `Std. Loading` = c(0.939, 0.829, 0.941, 0.963, 0.925, 0.939,
                                                                                                                                                                                                 0.828, 0.941, 0.963, 0.924), Intercept = c(7.454, 7.19, 7.031,
                                                                                                                                                                                                                                            7.47, 7.201, 7.454, 7.19, 7.031, 7.47, 7.201)), class = c("tbl_df",
                                                                                                                                                                                                                                                                                                      "tbl", "data.frame"), row.names = c(NA, -10L))
  )
})
