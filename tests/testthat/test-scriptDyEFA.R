test_that("scriptDyEFA produces correct output for constr_dy_meas = none and nFactor = 1", {

  dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                         x_delim2="_", distinguish_1="1", distinguish_2="2")


  expect_equal(scriptDyEFA(dvn, nFactor = 1),
               "#Measurement Model\n\n#Loadings\nefa('efa')*f1 =~ sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5 + sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5"
  )
})