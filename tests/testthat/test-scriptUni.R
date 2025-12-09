# scriptUni testing
# Configural --------------------------------------------------------------
# ---- FF ----
test_that("scriptUni produces correct output for constr_dy_meas = none, constr_dy_struct = none, and scaleset = FF", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  expect_equal(
    scriptUni(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = "none",
      constr_dy_struct = "none"
    ),
    "#Measurement Model\n\n#Loadings\nSatDy=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5+sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSatDy ~~ 1*SatDy\n\n#Latent Means\nSatDy ~ 0*1"
  )
})
test_that("scriptUni produces correct number of parameter estimates for consrt_dy_meas = none, constr_dy_struct = none, and scaleset = FF", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  # knowns for model w/ estimated mean structure:
  # var_num <- 10
  # knowns <- (var_num*(var_num+1)/2)+var_num

  # script configural invariance model w/ scaleset = FF
  script <- scriptUni(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "none",
    constr_dy_struct = "none"
  )

  # fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  # manually calculated free parameters (see LINK)
  # my_param <- 35

  lav_param <- as.double(
    lavaan::fitmeasures(
      mod,
      "npar"
    )
  )

  expect_equal(
    lav_param,
    35
  )
})
test_that("scriptUni produces correct df for constr_dy_meas = none, constr_dy_struct = none, and scaleset = FF", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  # knowns for model w/ estimated mean structure:
  # var_num <- 10
  # knowns <- (var_num*(var_num+1)/2)+var_num


  # script configural invariance model w/ scaleset = FF
  script <- scriptUni(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "none",
    constr_dy_struct = "none"
  )

  # fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  # manually calculated free parameters (see LINK)
  # my_param <- 35

  # calculated df (should be 29)
  # my_df <- knowns-my_param

  lav_df <- as.double(
    lavaan::fitmeasures(
      mod,
      "df"
    )
  )

  expect_equal(
    lav_df,
    30
  )
})

# ---- MV ----
test_that("scriptUni produces correct output for constr_dy_meas = none, constr_dy_struct = none, and scaleset = MV", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  expect_equal(
    scriptUni(
      dvn,
      scaleset = "MV",
      lvname = "Sat",
      constr_dy_meas = "none",
      constr_dy_struct = "none"
    ),
    "#Measurement Model\n\n#Loadings\nSatDy=~1*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5+sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSatDy ~~ NA*SatDy\n\n#Latent Means\nSatDy ~ NA*1"
  )
})

# ---- FF vs. MV ----
test_that("scriptUni produces same df for constr_dy_meas = none, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  # Script configural invariance model w/ scaleset = FF
  script_ff <- scriptUni(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "none",
    constr_dy_struct = "none"
  )

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  # Script configural invariance model w/ scaleset = MV
  script_mv <- scriptUni(
    dvn,
    scaleset = "MV",
    lvname = "Sat",
    constr_dy_meas = "none",
    constr_dy_struct = "none"
  )

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

test_that("scriptUni produces same chisq for constr_dy_meas = none, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  # Script configural invariance model w/ scaleset = FF
  script_ff <- scriptUni(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "none",
    constr_dy_struct = "none"
  )

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # get its chisq
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  # Script configural invariance model w/ scaleset = MV
  script_mv <- scriptUni(
    dvn,
    scaleset = "MV",
    lvname = "Sat",
    constr_dy_meas = "none",
    constr_dy_struct = "none"
  )

  # Fit model w FF
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  # get its chisq
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  # scale setting should have no impact on df
  expect_equal(
    chisq_ff,
    chisq_mv
  )
})

# Loading  --------------------------------------------------------
# ---- FF ----
test_that("scriptUni produces correct output for constr_dy_meas = loadings, constr_dy_struct = none, and scaleset = FF", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  expect_equal(
    scriptUni(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = "loadings",
      constr_dy_struct = "none"
    ),
    "#Measurement Model\n\n#Loadings\nSatDy=~NA*sat.g.1_1+lxg1*sat.g.1_1+lxg2*sat.g.1_2+lxg3*sat.g.1_3+lxg4*sat.g.1_4+lxg5*sat.g.1_5+lxg1*sat.g.2_1+lxg2*sat.g.2_2+lxg3*sat.g.2_3+lxg4*sat.g.2_4+lxg5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSatDy ~~ 1*SatDy\n\n#Latent Means\nSatDy ~ 0*1"
  )
})
test_that("scriptUni produces correct number of parameter estimates for consrt_dy_meas = loadings, constr_dy_struct = none, and scaleset = FF", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )


  # knowns for model w/ estimated mean structure:
  # var_num <- 10
  # knowns <- (var_num*(var_num+1)/2)+var_num

  # script loading invariance model w/ scaleset = FF
  script <- scriptUni(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "loadings",
    constr_dy_struct = "none"
  )

  # fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  # manually calculated free parameters (see LINK)
  # my_param <- 30

  lav_param <- as.double(
    lavaan::fitmeasures(
      mod,
      "npar"
    )
  )

  expect_equal(
    lav_param,
    30
  )
})
test_that("scriptUni produces correct df for constr_dy_meas = loadings, constr_dy_struct = none, and scaleset = FF", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  # knowns for model w/ estimated mean structure:
  # var_num <- 10
  # knowns <- (var_num*(var_num+1)/2)+var_num


  # script configural invariance model w/ scaleset = FF
  script <- scriptUni(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "loadings",
    constr_dy_struct = "none"
  )

  # fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  # manually calculated free parameters (see LINK)
  # my_param <- 30

  # calculated df (should be 35)
  # my_df <- knowns-my_param

  lav_df <- as.double(
    lavaan::fitmeasures(
      mod,
      "df"
    )
  )

  expect_equal(
    lav_df,
    35
  )
})

# ---- MV ----
test_that("scriptUni produces correct output for constr_dy_meas = loadings, constr_dy_struct = none, and scaleset = MV", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  expect_equal(
    scriptUni(
      dvn,
      scaleset = "MV",
      lvname = "Sat",
      constr_dy_meas = "loadings",
      constr_dy_struct = "none"
    ),
    "#Measurement Model\n\n#Loadings\nSatDy=~1*sat.g.1_1+lxg1*sat.g.1_1+lxg2*sat.g.1_2+lxg3*sat.g.1_3+lxg4*sat.g.1_4+lxg5*sat.g.1_5+lxg1*sat.g.2_1+lxg2*sat.g.2_2+lxg3*sat.g.2_3+lxg4*sat.g.2_4+lxg5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSatDy ~~ NA*SatDy\n\n#Latent Means\nSatDy ~ NA*1"
  )
})

# ---- FF vs. MV ----
test_that("scriptUni produces same df for constr_dy_meas = loadings, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  # Script loading invariance model w/ scaleset = FF
  script_ff <- scriptUni(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = "loadings",
    constr_dy_struct = "none"
  )

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  # Script loading invariance model w/ scaleset = MV
  script_mv <- scriptUni(
    dvn,
    scaleset = "MV",
    lvname = "Sat",
    constr_dy_meas = "loadings",
    constr_dy_struct = "none"
  )

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
test_that("scriptUni produces same chisq for constr_dy_meas = loadings, constr_dy_struct = none when scaleset= FF, and scaleset = MV", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  # Script loading invariance model w/ scaleset = FF
  script_ff <- scriptUni(
    dvn,
    scaleset = "MV",
    lvname = "Sat",
    constr_dy_meas = "loadings",
    constr_dy_struct = "none"
  )

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # get its chisq
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  # Script loading invariance model w/ scaleset = MV
  script_mv <- scriptUni(
    dvn,
    scaleset = "MV",
    lvname = "Sat",
    constr_dy_meas = "loadings",
    constr_dy_struct = "none"
  )

  # Fit model w FF
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  # get its chisq
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  # scale setting should have no impact on df
  expect_equal(
    chisq_ff,
    chisq_mv
  )
})



# Intercept ----------------------------------------------------------------
# ---- FF ----
test_that("scriptUni produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none, and scaleset = FF", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  expect_equal(
    scriptUni(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts"),
      constr_dy_struct = "none"
    ),
    "#Measurement Model\n\n#Loadings\nSatDy=~NA*sat.g.1_1+lxg1*sat.g.1_1+lxg2*sat.g.1_2+lxg3*sat.g.1_3+lxg4*sat.g.1_4+lxg5*sat.g.1_5+lxg1*sat.g.2_1+lxg2*sat.g.2_2+lxg3*sat.g.2_3+lxg4*sat.g.2_4+lxg5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSatDy ~~ 1*SatDy\n\n#Latent Means\nSatDy ~ 0*1"
  )
})
test_that("scriptUni produces correct number of parameter estimates for consrt_dy_meas = c(loadings, intercepts), constr_dy_struct = none, and scaleset = FF", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )


  # knowns for model w/ estimated mean structure:
  # var_num <- 10
  # knowns <- (var_num*(var_num+1)/2)+var_num

  # script loading invariance model w/ scaleset = FF
  script <- scriptUni(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts"),
    constr_dy_struct = "none"
  )

  # fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  # manually calculated free parameters (see LINK)
  # my_param <- 25

  lav_param <- as.double(
    lavaan::fitmeasures(
      mod,
      "npar"
    )
  )

  expect_equal(
    lav_param,
    25
  )
})
test_that("scriptUni produces correct df for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none, and scaleset = FF", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  # knowns for model w/ estimated mean structure:
  # var_num <- 10
  # knowns <- (var_num*(var_num+1)/2)+var_num


  # script configural invariance model w/ scaleset = FF
  script <- scriptUni(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts"),
    constr_dy_struct = "none"
  )

  # fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  # manually calculated free parameters (see LINK)
  # my_param <- 25

  # calculated df (should be 40)
  # my_df <- knowns-my_param

  lav_df <- as.double(
    lavaan::fitmeasures(
      mod,
      "df"
    )
  )

  expect_equal(
    lav_df,
    40
  )
})

# ---- MV ----
test_that("scriptUni produces correct output for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none, and scaleset = MV", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  expect_equal(
    scriptUni(
      dvn,
      scaleset = "MV",
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts"),
      constr_dy_struct = "none"
    ),
    "#Measurement Model\n\n#Loadings\nSatDy=~1*sat.g.1_1+lxg1*sat.g.1_1+lxg2*sat.g.1_2+lxg3*sat.g.1_3+lxg4*sat.g.1_4+lxg5*sat.g.1_5+lxg1*sat.g.2_1+lxg2*sat.g.2_2+lxg3*sat.g.2_3+lxg4*sat.g.2_4+lxg5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSatDy ~~ NA*SatDy\n\n#Latent Means\nSatDy ~ NA*1"
  )
})

# ---- FF vs. MV ----
test_that("scriptUni produces same df for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  # Script intercept invariance model w/ scaleset = FF
  script_ff <- scriptUni(
    dvn,
    scaleset = "MV",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts"),
    constr_dy_struct = "none"
  )

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  # Script intercept invariance model w/ scaleset = MV
  script_mv <- scriptUni(
    dvn,
    scaleset = "MV",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts"),
    constr_dy_struct = "none"
  )

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
test_that("scriptUni produces same chisq for constr_dy_meas = c(loadings, intercepts), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  # Script intercept invariance model w/ scaleset = FF
  script_ff <- scriptUni(
    dvn,
    scaleset = "MV",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts"),
    constr_dy_struct = "none"
  )

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # get its chisq
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  # Script intercept invariance model w/ scaleset = MV
  script_mv <- scriptUni(
    dvn,
    scaleset = "MV",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts"),
    constr_dy_struct = "none"
  )

  # Fit model w FF
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  # get its chisq
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  # scale setting should have no impact on df
  expect_equal(
    chisq_ff,
    chisq_mv
  )
})

# Residual ----------------------------------------------------------------
# ---- FF ----
test_that("scriptUni produces correct output for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none, and scaleset = FF", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  expect_equal(
    scriptUni(
      dvn,
      scaleset = "FF",
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts", "residuals"),
      constr_dy_struct = "none"
    ),
    "#Measurement Model\n\n#Loadings\nSatDy=~NA*sat.g.1_1+lxg1*sat.g.1_1+lxg2*sat.g.1_2+lxg3*sat.g.1_3+lxg4*sat.g.1_4+lxg5*sat.g.1_5+lxg1*sat.g.2_1+lxg2*sat.g.2_2+lxg3*sat.g.2_3+lxg4*sat.g.2_4+lxg5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSatDy ~~ 1*SatDy\n\n#Latent Means\nSatDy ~ 0*1"
  )
})
test_that("scriptUni produces correct number of parameter estimates for consrt_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none, and scaleset = FF", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )


  # knowns for model w/ estimated mean structure:
  # var_num <- 10
  # knowns <- (var_num*(var_num+1)/2)+var_num

  # script loading invariance model w/ scaleset = FF
  script <- scriptUni(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = "none"
  )

  # fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  # manually calculated free parameters (see LINK)
  # my_param <- 20

  lav_param <- as.double(
    lavaan::fitmeasures(
      mod,
      "npar"
    )
  )

  expect_equal(
    lav_param,
    20
  )
})
test_that("scriptUni produces correct df for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none, and scaleset = FF", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  # knowns for model w/ estimated mean structure:
  # var_num <- 10
  # knowns <- (var_num*(var_num+1)/2)+var_num


  # script configural invariance model w/ scaleset = FF
  script <- scriptUni(
    dvn,
    scaleset = "FF",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = "none"
  )

  # fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  # manually calculated free parameters (see LINK)
  # my_param <- 20

  # calculated df (should be 45)
  # my_df <- knowns-my_param

  lav_df <- as.double(
    lavaan::fitmeasures(
      mod,
      "df"
    )
  )

  expect_equal(
    lav_df,
    45
  )
})

# ---- MV ----
test_that("scriptUni produces correct output for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none, and scaleset = MV", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  expect_equal(
    scriptUni(
      dvn,
      scaleset = "MV",
      lvname = "Sat",
      constr_dy_meas = c("loadings", "intercepts", "residuals"),
      constr_dy_struct = "none"
    ),
    "#Measurement Model\n\n#Loadings\nSatDy=~1*sat.g.1_1+lxg1*sat.g.1_1+lxg2*sat.g.1_2+lxg3*sat.g.1_3+lxg4*sat.g.1_4+lxg5*sat.g.1_5+lxg1*sat.g.2_1+lxg2*sat.g.2_2+lxg3*sat.g.2_3+lxg4*sat.g.2_4+lxg5*sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSatDy ~~ NA*SatDy\n\n#Latent Means\nSatDy ~ NA*1"
  )
})

# ---- FF vs. MV ----
test_that("scriptUni produces same df for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  # Script residual invariance model w/ scaleset = FF
  script_ff <- scriptUni(
    dvn,
    scaleset = "MV",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = "none"
  )

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # get its df
  df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  # Script residual invariance model w/ scaleset = MV
  script_mv <- scriptUni(
    dvn,
    scaleset = "MV",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts", "residuals"),
    constr_dy_struct = "none"
  )

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
test_that("scriptUni produces same chisq for constr_dy_meas = c(loadings, intercepts, residuals), constr_dy_struct = none when scaleset= FF, and scaleset = MV", {
  dvn <- scrapeVarCross(
    commitmentQ,
    x_order = "spi",
    x_stem = "sat.g",
    x_delim1 = ".",
    x_delim2 = "_",
    distinguish_1 = "1",
    distinguish_2 = "2"
  )

  # Script residual invariance model w/ scaleset = FF
  script_ff <- scriptUni(
    dvn,
    scaleset = "MV",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts", "residual"),
    constr_dy_struct = "none"
  )

  # Fit model w FF
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # get its chisq
  chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  # Script residual invariance model w/ scaleset = MV
  script_mv <- scriptUni(
    dvn,
    scaleset = "MV",
    lvname = "Sat",
    constr_dy_meas = c("loadings", "intercepts", "residual"),
    constr_dy_struct = "none"
  )

  # Fit model w FF
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  # get its chisq
  chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  # scale setting should have no impact on df
  expect_equal(
    chisq_ff,
    chisq_mv
  )
})

#### Error Handling Tests for scriptUni ####
# ============================================
# These tests verify that scriptUni properly handles invalid inputs,
# missing required arguments, invalid parameter combinations, and malformed dvn objects.

#### Group 1: Invalid Input Types ####
test_that("scriptUni rejects non-list dvn argument", {
  expect_error(
    scriptUni(dvn = "not_a_list", lvname = "Sat"),
    "The `dvn` argument must be a list object."
  )
})

test_that("scriptUni rejects non-character lvname argument", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  expect_error(
    scriptUni(dvn = dvn, lvname = 123),
    "The `lvname` argument must be a character string."
  )
})

#### Group 2: Missing Required Arguments ####
test_that("scriptUni requires dvn argument", {
  expect_error(
    scriptUni(lvname = "Sat"),
    "The `dvn` argument is required and cannot be NULL."
  )
})

#### Group 3: Invalid Parameter Combinations ####
test_that("scriptUni rejects invalid constr_dy_meas values", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  expect_error(
    scriptUni(dvn = dvn, lvname = "Sat", constr_dy_meas = "invalid_option"),
    "constr_dy_meas must be a character vector containing any combination of 'loadings', 'intercepts', 'residuals', or 'none'"
  )
})

test_that("scriptUni rejects invalid scaleset value", {
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  expect_error(
    scriptUni(dvn = dvn, lvname = "Sat", scaleset = "invalid"),
    "scaleset must be either 'FF' \\(fixed-factor\\) or 'MV' \\(marker variable\\)"
  )
})

#### Group 4: Malformed dvn Objects ####
test_that("scriptUni rejects dvn object with wrong length", {
  dvn_xy <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_"
  )
  expect_error(
    scriptUni(dvn = dvn_xy, lvname = "Sat"),
    "You must supply a dvn object containing information for only X"
  )
})

test_that("scriptUni rejects dvn object with missing required elements", {
  dvn_malformed <- list(
    p1xvarnames = c("var1", "var2"),
    p2xvarnames = c("var3", "var4")
  )
  expect_error(
    scriptUni(dvn = dvn_malformed, lvname = "Sat"),
    "You must supply a dvn object containing information for only X"
  )
})
