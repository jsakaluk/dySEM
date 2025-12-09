#### configural ####
test_that("scriptCor produces correct output for constr_dy_meas = none, constr_dy_struct = none, and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF"),
    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSat2=~NA*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ 0*1"
  )
})

test_that("scriptCor produces correct output for constr_dy_meas = none, constr_dy_struct = none and scaleset= MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "MV"),
    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSat2=~1*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1"
  )
})

test_that("scriptCor produces correct number of parameter estimates for constr_dy_meas = none, constr_dy_struct = none and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )

  # knowns for model w/ intercepts/latent means:
  # var_num <- 10
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ model = configural and scaleset = FF
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF")

  # Fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  # manually calculated free parameters (see LINK)
  # my_param <- 36

  lav_param <- as.double(lavaan::fitmeasures(mod, "npar"))

  expect_equal(
    lav_param,
    36
  )
})

test_that("scriptCor produces correct df for constr_dy_meas = none, constr_dy_struct = none and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )

  # knowns for model w/ intercepts/latent means:
  # var_num <- 10
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ model = configural and scaleset = FF
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF")

  # Fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  # manually calculated free parameters (see LINK)
  # my_param <- 36

  # calculated df (should be 29)
  # my_df <- knowns-my_param

  lav_df <- as.double(lavaan::fitmeasures(mod, "df"))

  expect_equal(
    lav_df,
    29
  )
})

test_that("scriptCor produces same df for constr_dy_meas = none, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  # Script model w/ model = configural and scaleset = FF
  script_ff <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF")

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  # Script model w/ model = configural and scaleset = FF
  script_mv <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "MV")

  # Fit model w FF
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  # get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  # scale setting should have no impact on df
  expect_equal(
    df_ff,
    df_mv
  )
})

test_that("scriptCor produces same chisq for constr_dy_meas = none, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  # Script model w/ model = configural and scaleset = FF
  script_ff <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "FF")

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # get its df
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  # Script model w/ model = configural and scaleset = FF
  script_mv <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "none", constr_dy_struct = "none", scaleset = "MV")

  # Fit model w FF
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  # get its df
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  # scale setting should have no impact on df
  expect_equal(
    chisq_ff,
    chisq_mv
  )
})

#### loading ####
test_that("scriptCor produces correct output for constr_dy_meas = loadings, constr_dy_struct = none and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptCor(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF"),
    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ 0*1"
  )
})

test_that("scriptCor produces correct output for constr_dy_meas = loadings, constr_dy_struct = none and scaleset= MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptCor(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "MV"),
    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1"
  )
})

test_that("scriptCor produces correct number of parameter estimates for constr_dy_meas = loadings, constr_dy_struct = none and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )

  # knowns for model w/ intercepts/latent means:
  # var_num <- 10
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ model = configural and scaleset = FF
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF")

  # Fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  # manually calculated free parameters (see LINK)
  # my_param <- 32

  lav_param <- as.double(lavaan::fitmeasures(mod, "npar"))

  expect_equal(
    lav_param,
    32
  )
})

test_that("scriptCor produces correct df for constr_dy_meas = loadings, constr_dy_struct = none and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )

  # knowns for model w/ intercepts/latent means:
  # var_num <- 10
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ model = configural and scaleset = FF
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF")

  # Fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  # manually calculated free parameters (see LINK)
  # my_param <- 32

  # calculated df (should be 33)
  # my_df <- knowns-my_param

  lav_df <- as.double(lavaan::fitmeasures(mod, "df"))

  expect_equal(
    lav_df,
    33
  )
})

test_that("scriptCor produces same df for constr_dy_meas = loadings, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  # Script model w/ model = configural and scaleset = FF
  script_ff <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF")

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  # Script model w/ model = configural and scaleset = FF
  script_mv <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "MV")

  # Fit model w FF
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  # get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  # scale setting should have no impact on df
  expect_equal(
    df_ff,
    df_mv
  )
})

test_that("scriptCor produces same chisq for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  # Script model w/ model = configural and scaleset = FF
  script_ff <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "FF")

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # get its df
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  # Script model w/ model = configural and scaleset = FF
  script_mv <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = "loadings", constr_dy_struct = "none", scaleset = "MV")

  # Fit model w FF
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  # get its df
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  # scale setting should have no impact on df
  expect_equal(
    chisq_ff,
    chisq_mv
  )
})
#### intercept ####
test_that("scriptCor produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptCor(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF"),
    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ NA*1"
  )
})

test_that("scriptCor produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none and scaleset= MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptCor(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "MV"),
    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1"
  )
})

test_that("scriptCor produces correct number of parameter estimates for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )

  # knowns for model w/ intercepts/latent means:
  # var_num <- 10
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ model = configural and scaleset = FF
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF")

  # Fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  # manually calculated free parameters (see LINK)
  # my_param <- 28

  lav_param <- as.double(lavaan::fitmeasures(mod, "npar"))

  expect_equal(
    lav_param,
    28
  )
})

test_that("scriptCor produces correct df for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )

  # knowns for model w/ intercepts/latent means:
  # var_num <- 10
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ model = configural and scaleset = FF
  script <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF")

  # Fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  # manually calculated free parameters (see LINK)
  # my_param <- 28

  # calculated df (should be 37)
  # my_df <- knowns-my_param

  lav_df <- as.double(lavaan::fitmeasures(mod, "df"))

  expect_equal(
    lav_df,
    37
  )
})

test_that("scriptCor produces same df for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  # Script model w/ model = configural and scaleset = FF
  script_ff <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF")

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  # Script model w/ model = configural and scaleset = FF
  script_mv <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "MV")

  # Fit model w FF
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  # get its df
  df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  # scale setting should have no impact on df
  expect_equal(
    df_ff,
    df_mv
  )
})

test_that("scriptCor produces same chisq for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  # Script model w/ model = configural and scaleset = FF
  script_ff <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "FF")

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # get its df
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  # Script model w/ model = configural and scaleset = FF
  script_mv <- scriptCor(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none", scaleset = "MV")

  # Fit model w FF
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  # get its df
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  # scale setting should have no impact on df
  expect_equal(
    chisq_ff,
    chisq_mv
  )
})
#### residual ####
test_that("scriptCor produces correct output for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptCor(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "FF"),
    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ NA*1"
  )
})

test_that("scriptCor produces correct output for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none and scaleset= MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptCor(dvn, lvname = "Sat", constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none", scaleset = "MV"),
    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1"
  )
})


#### lvariance ####
test_that("scriptCor produces correct output for constr_dy_meas = c(loadings), constr_dy_struct = variances and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptCor(dvn,
      lvname = "Sat",
      constr_dy_meas = c("loadings"),
      constr_dy_struct = c("variances"), scaleset = "FF"
    ),
    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1 + psix*Sat1\nSat2 ~~ psix*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ 0*1"
  )
})

test_that("scriptCor produces correct output for constr_dy_meas = c(loadings), constr_dy_struct = variances and scaleset= MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptCor(dvn,
      lvname = "Sat",
      constr_dy_meas = c("loadings"),
      constr_dy_struct = c("variances"), scaleset = "MV"
    ),
    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ psix*Sat1\nSat2 ~~ psix*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1"
  )
})

#### lmean ####
test_that("scriptCor produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = means and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptCor(dvn,
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts"),
      constr_dy_struct = c("means"), scaleset = "FF"
    ),
    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ 0*1 + alphax*1\nSat2 ~ alphax*1"
  )
})

test_that("scriptCor produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = means and scaleset= MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptCor(dvn,
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts"),
      constr_dy_struct = c("means"), scaleset = "MV"
    ),
    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ alphax*1\nSat2 ~ alphax*1"
  )
})

#### indist ####
test_that("scriptCor produces correct output for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = c(variances, means) and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptCor(dvn,
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts", "residuals"),
      constr_dy_struct = c("variances", "means"), scaleset = "FF"
    ),
    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1 + psix*Sat1\nSat2 ~~ psix*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ 0*1 + alphax*1\nSat2 ~ alphax*1"
  )
})

test_that("scriptCor produces correct output for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = c(variances, means) and scaleset= MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
    x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptCor(dvn,
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts", "residuals"),
      constr_dy_struct = c("variances", "means"), scaleset = "MV"
    ),
    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ psix*Sat1\nSat2 ~~ psix*Sat2\nSat1 ~~ Sat2\n\n#Latent Means\nSat1 ~ alphax*1\nSat2 ~ alphax*1"
  )
})



# TODO: tests for writescript = TRUE

#### Error Handling Tests for scriptCor ####
# ===========================================
# These tests verify that scriptCor properly handles invalid inputs,
# missing required arguments, invalid parameter combinations, and malformed dvn objects.

#### Group 1: Invalid Input Types ####
# ------------------------------------
# These tests verify that scriptCor rejects inputs of incorrect types.

test_that("scriptCor rejects non-list dvn argument", {
  # Test that dvn must be a list object
  expect_error(
    scriptCor(dvn = "not_a_list", lvname = "Sat"),
    "The `dvn` argument must be a list object."
  )
})

test_that("scriptCor rejects non-character lvname argument", {
  # Test that lvname must be a character string
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  expect_error(
    scriptCor(dvn = dvn, lvname = 123),
    "The `lvname` argument must be a character string."
  )
})

test_that("scriptCor rejects non-character writeTo argument when provided", {
  # Test that writeTo must be a character string when provided
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  expect_error(
    scriptCor(dvn = dvn, lvname = "Sat", writeTo = 123),
    "The `writeout` argument must be a character string"
  )
})

test_that("scriptCor rejects non-character fileName argument when provided", {
  # Test that fileName must be a character string when provided
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  expect_error(
    scriptCor(dvn = dvn, lvname = "Sat", writeTo = tempdir(), fileName = 123),
    "The `fileName` argument must be a character string"
  )
})

#### Group 2: Missing Required Arguments ####
# --------------------------------------------
# These tests ensure that required arguments are properly validated.

test_that("scriptCor requires dvn argument", {
  # Test that dvn is required
  expect_error(
    scriptCor(lvname = "Sat"),
    "The `dvn` argument is required and cannot be NULL."
  )
})

#### Group 3: Invalid Parameter Combinations ####
# -------------------------------------------------
# These tests check that invalid parameter values are caught.

test_that("scriptCor rejects invalid constr_dy_meas values", {
  # Test that constr_dy_meas must contain valid options
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  expect_error(
    scriptCor(dvn = dvn, lvname = "Sat", constr_dy_meas = "invalid_option"),
    "constr_dy_meas must be a character vector containing any combination of 'loadings', 'intercepts', 'residuals', or 'none'"
  )
})

test_that("scriptCor rejects invalid constr_dy_struct values", {
  # Test that constr_dy_struct must contain valid options
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  expect_error(
    scriptCor(dvn = dvn, lvname = "Sat", constr_dy_struct = "invalid_option"),
    "constr_dy_struct must be a character vector containing any combination of 'variances', 'means', or 'none'"
  )
})

test_that("scriptCor rejects invalid scaleset value", {
  # Test that scaleset must be either "FF" or "MV"
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  expect_error(
    scriptCor(dvn = dvn, lvname = "Sat", scaleset = "invalid"),
    "scaleset must be either 'FF' \\(fixed-factor\\) or 'MV' \\(marker variable\\)"
  )
})

test_that("scriptCor rejects non-existent directory for writeTo", {
  # Test that writeTo must point to an existing directory
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  expect_error(
    scriptCor(dvn = dvn, lvname = "Sat", writeTo = "/nonexistent/directory/path"),
    "The specified directory does not exist"
  )
})

#### Group 4: Malformed dvn Objects ####
# --------------------------------------
# These tests verify handling of incorrectly structured dvn objects.

test_that("scriptCor rejects dvn object with wrong length (9 elements instead of 6)", {
  # Test that scriptCor requires a 6-element dvn (X only), not 9-element (X and Y)
  dvn_xy <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_"
  )
  expect_error(
    scriptCor(dvn = dvn_xy, lvname = "Sat"),
    "You must supply a dvn object containing information for only X"
  )
})

test_that("scriptCor rejects dvn object with missing required elements", {
  # Test that dvn must contain all required elements
  dvn_malformed <- list(
    p1xvarnames = c("var1", "var2"),
    p2xvarnames = c("var3", "var4")
    # Missing xindper, dist1, dist2, indnum
  )
  expect_error(
    scriptCor(dvn = dvn_malformed, lvname = "Sat"),
    "You must supply a dvn object containing information for only X"
  )
})

test_that("scriptCor handles dvn object with empty variable name vectors", {
  # Test that dvn with empty variable vectors causes an error
  # Function tries to access elements that don't exist
  dvn_empty <- list(
    p1xvarnames = character(0),
    p2xvarnames = character(0),
    xindper = 0L,
    dist1 = "1",
    dist2 = "2",
    indnum = 0L
  )
  # Should error when trying to access empty vectors
  expect_error(
    scriptCor(dvn = dvn_empty, lvname = "Sat"),
    regexp = ".*"
  )
})
