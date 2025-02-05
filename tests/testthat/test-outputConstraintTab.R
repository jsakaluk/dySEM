#Error checks

test_that("outputConstraintTab produces correct error when writeTo is not character and gtTab is TRUE", {

  dvn <- scrapeVarCross(dat = commitmentM, x_order = "sip", x_stem = "sat.g",
                        x_delim2="_", distinguish_1="f", distinguish_2="m")

  sat.resids.script <- scriptCor(dvn, lvname = "Sat",constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none")

  sat.resids.mod <- lavaan::cfa(sat.resids.script, data = commitmentM, std.lv = FALSE,
                                auto.fix.first= FALSE, meanstructure = TRUE)

  expect_error(outputConstraintTab(sat.resids.mod,
                                   filterSig = FALSE,
                                   gtTab = TRUE,
                                   writeTo = 5,
                                   fileName = "dCFA_Residual"),
               cat("The `writeTo` argument must be a character string. \n Use `writeTo = '.'` to save output file(s) in the current working directory.")
               )
})

test_that("outputConstraintTab produces correct error when fileName is not character and gtTab is TRUE", {

  dvn <- scrapeVarCross(dat = commitmentM, x_order = "sip", x_stem = "sat.g",
                        x_delim2="_", distinguish_1="f", distinguish_2="m")

  sat.resids.script <- scriptCor(dvn, lvname = "Sat",constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none")

  sat.resids.mod <- lavaan::cfa(sat.resids.script, data = commitmentM, std.lv = FALSE,
                                auto.fix.first= FALSE, meanstructure = TRUE)

  expect_error(outputConstraintTab(sat.resids.mod,
                                   filterSig = FALSE,
                                   gtTab = TRUE,
                                   writeTo = tempdir(),
                                   fileName = 5),
               "The `fileName` argument must be a character string."
  )
})
