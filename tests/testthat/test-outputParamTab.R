#####Error checks####

test_that("outputConstraintTab produces correct error when writeTo is not character and gtTab is TRUE", {

  dvn <- scrapeVarCross(dat = commitmentM, x_order = "sip", x_stem = "sat.g",
                        x_delim2="_", distinguish_1="f", distinguish_2="m")

  sat.resids.script <- scriptCor(dvn, lvname = "Sat",constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none")

  sat.resids.mod <- lavaan::cfa(sat.resids.script, data = commitmentM, std.lv = FALSE,
                                auto.fix.first= FALSE, meanstructure = TRUE)

  expect_error(outputParamTab(dvn, model = "cfa", sat.resids.mod,
                              gtTab = TRUE,
                              writeTo = 6,
                              fileName = "dCFA_Residual"),
               cat("The `writeTo` argument must be a character string. \n Use `writeTo = '.'` to save output file(s) in the current working directory.")
  )
})

test_that("outputParamTab produces correct error when fileName is not character and gtTab is TRUE", {

  dvn <- scrapeVarCross(dat = commitmentM, x_order = "sip", x_stem = "sat.g",
                        x_delim2="_", distinguish_1="f", distinguish_2="m")

  sat.resids.script <- scriptCor(dvn, lvname = "Sat",constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none")

  sat.resids.mod <- lavaan::cfa(sat.resids.script, data = commitmentM, std.lv = FALSE,
                                auto.fix.first= FALSE, meanstructure = TRUE)

  expect_error(outputParamTab(dvn, model = "cfa", sat.resids.mod,
                                   gtTab = TRUE,
                                   writeTo = tempdir(),
                                   fileName = 5),
               "The `fileName` argument must be a character string."
  )
})

#### output from CFA ####

test_that("outputParamTab produces correct output for CFA measurement table without gt::", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")

  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none",
                                 constr_dy_struct = "none")

  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)



  expect_equal(outputParamTab(dvn, fit = mod, model = "cfa", table = "measurement", gtTab = FALSE),
               structure(list(`Latent Factor` = c("Sat1", "Sat1", "Sat1", "Sat1",
                                                  "Sat1", "Sat2", "Sat2", "Sat2", "Sat2", "Sat2"), Indicator = c("sat.g.1_1",
                                                                                                                 "sat.g.1_2", "sat.g.1_3", "sat.g.1_4", "sat.g.1_5", "sat.g.2_1",
                                                                                                                 "sat.g.2_2", "sat.g.2_3", "sat.g.2_4", "sat.g.2_5"), Loading = c(2.112,
                                                                                                                                                                                  1.909, 2.098, 1.955, 1.869, 1.83, 1.843, 1.883, 1.611, 1.926),
                              SE = c(0.158, 0.168, 0.161, 0.162, 0.179, 0.144, 0.146, 0.146,
                                     0.151, 0.159), Z = c(13.345, 11.383, 13.064, 12.049, 10.429,
                                                          12.668, 12.66, 12.862, 10.669, 12.12), `p-value` = c("< .001",
                                                                                                               "< .001", "< .001", "< .001", "< .001", "< .001", "< .001",
                                                                                                               "< .001", "< .001", "< .001"), `Std. Loading` = c(0.937,
                                                                                                                                                                 0.851, 0.926, 0.884, 0.806, 0.91, 0.908, 0.918, 0.819, 0.887
                                                                                                               ), Intercept = c(6.6, 6.548, 6.409, 6.661, 6.426, 6.93, 6.922,
                                                                                                                                6.704, 7.13, 6.817)), class = c("tbl_df", "tbl", "data.frame"
                                                                                                                                ), row.names = c(NA, -10L))
  )
})

test_that("outputParamTab produces correct output for CFA measurement table with gt + writeTo and fileName", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")

  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none",
                      constr_dy_struct = "none")

  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)

  table <- outputParamTab(dvn, fit = mod, model = "cfa", table = "measurement", gtTab = TRUE, writeTo = tempdir(), fileName = "cfa_indist")
  expect_s3_class(table, "gt_tbl")
  expect_true("_data" %in% names(table)) # Check if "data" component exists

})

test_that("outputParamTab produces correct output for CFA measurement table with gt + writeTo but without fileName", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")

  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none",
                      constr_dy_struct = "none")

  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)

  table <- outputParamTab(dvn, fit = mod, model = "cfa", table = "measurement", gtTab = TRUE, writeTo = tempdir(), fileName = NULL)
  expect_s3_class(table, "gt_tbl")
  expect_true("_data" %in% names(table)) # Check if "data" component exists

})

test_that("outputParamTab produces correct output for CFA measurement table with gt + fileName but without writeTo", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                        x_delim2="_", distinguish_1="1", distinguish_2="2")

  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none",
                      constr_dy_struct = "none")

  mod <- lavaan::cfa(script, data = commitmentQ, std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)

  table <- outputParamTab(dvn, fit = mod, model = "cfa", table = "measurement", gtTab = TRUE, writeTo = NULL, fileName = "cfa_indist")
  expect_s3_class(table, "gt_tbl")
  expect_true("_data" %in% names(table)) # Check if "data" component exists

})

#### output from APIM ####
