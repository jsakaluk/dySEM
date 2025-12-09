#### mutual configural ####
test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptAPIM(dvn,
      lvxname = "Sat", lvyname = "Com", scaleset = "FF",
      constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
      constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
      constr_dy_xy_struct = c("none"), includeMeanStruct = TRUE
    ),
    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSat2=~NA*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\nCom1=~NA*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5\nCom2=~NA*com.2_1+com.2_2+com.2_3+com.2_4+com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\ncom.1_1 ~ 1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\n\ncom.2_1 ~ 1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\n\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ 1*Com1\nCom2 ~~ 1*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ 0*1\n\nCom1 ~ 0*1\nCom2 ~ 0*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
  )
})

test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptAPIM(dvn,
      lvxname = "Sat", lvyname = "Com", scaleset = "MV",
      constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
      constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
      constr_dy_xy_struct = c("none"), includeMeanStruct = TRUE
    ),
    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSat2=~1*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\nCom1=~1*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5\nCom2=~1*com.2_1+com.2_2+com.2_3+com.2_4+com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\ncom.1_1 ~ 0*1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\n\ncom.2_1 ~ 0*1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\n\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ NA*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\n\nCom1 ~ NA*1\nCom2 ~ NA*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
  )
})

test_that("scriptAPIM produces correct number of parameter estimatesfor constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # knowns for model :
  # var_num <- 20
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ scaleset = FF
  script <- scriptAPIM(dvn,
    lvxname = "Sat", lvyname = "Com", scaleset = "FF",
    constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
    constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
    constr_dy_xy_struct = c("none"), includeMeanStruct = TRUE
  )

  # Fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  # manually calculated free parameters (see LINK)
  # my_param <- 76

  lav_param <- as.double(lavaan::fitmeasures(mod, "npar"))

  expect_equal(
    lav_param,
    76
  )
})

test_that("scriptAPIM produces correct df for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # knowns for model :
  # var_num <- 20
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ scaleset = FF
  script_ff <- scriptAPIM(dvn,
    lvxname = "Sat", lvyname = "Com", scaleset = "FF",
    constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
    constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
    constr_dy_xy_struct = c("none"), includeMeanStruct = TRUE
  )

  # Fit model
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # manually calculated free parameters (see LINK)
  # my_param <- 76

  # calculated df (should be 154)
  # my_df <- knowns-my_param

  lav_df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  expect_equal(
    lav_df_ff,
    154
  )
})

test_that("scriptAPIM produces same df for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none when scaleset= FF, and scaleset = MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # knowns for model :
  # var_num <- 20
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ scaleset = FF
  script_ff <- scriptAPIM(dvn,
    lvxname = "Sat", lvyname = "Com", scaleset = "FF",
    constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
    constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
    constr_dy_xy_struct = c("none"), includeMeanStruct = TRUE
  )

  # Fit model
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # df for FF model
  lav_df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  # Script model w/ scaleset = FF
  script_mv <- scriptAPIM(dvn,
    lvxname = "Sat", lvyname = "Com", scaleset = "MV",
    constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
    constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
    constr_dy_xy_struct = c("none"), includeMeanStruct = TRUE
  )

  # Fit model
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  # df for FF model
  lav_df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  expect_equal(
    lav_df_ff,
    lav_df_mv
  )
})

test_that("scriptAPIM produces same chisq for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none when scaleset= FF, and scaleset = MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )

  # knowns for model :
  # var_num <- 20
  # knowns <- var_num*(var_num+1)/2+var_num

  # Script model w/ scaleset = FF
  script_ff <- scriptAPIM(dvn,
    lvxname = "Sat", lvyname = "Com", scaleset = "FF",
    constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
    constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
    constr_dy_xy_struct = c("none")
  )

  # Fit model
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  # df for FF model
  lav_chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  # Script model w/ scaleset = FF
  script_mv <- scriptAPIM(dvn,
    lvxname = "Sat", lvyname = "Com", scaleset = "MV",
    constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
    constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
    constr_dy_xy_struct = c("none")
  )

  # Fit model
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  # df for FF model
  lav_chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  expect_equal(
    lav_chisq_ff,
    lav_chisq_mv
  )
})

#### mutual loading ####
test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptAPIM(dvn,
      lvxname = "Sat", lvyname = "Com", scaleset = "FF",
      constr_dy_x_meas = c("loadings"), constr_dy_y_meas = c("loadings"),
      constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
      constr_dy_xy_struct = c("none"), includeMeanStruct = TRUE
    ),
    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nCom1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\ncom.1_1 ~ 1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\n\ncom.2_1 ~ 1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\n\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ 1*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ 0*1\n\nCom1 ~ 0*1\nCom2 ~ 0*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
  )
})

test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptAPIM(dvn,
      lvxname = "Sat", lvyname = "Com", scaleset = "MV",
      constr_dy_x_meas = c("loadings"), constr_dy_y_meas = c("loadings"),
      constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
      constr_dy_xy_struct = c("none"), includeMeanStruct = TRUE
    ),
    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nCom1=~1*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\ncom.1_1 ~ 0*1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\n\ncom.2_1 ~ 0*1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\n\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ NA*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\n\nCom1 ~ NA*1\nCom2 ~ NA*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
  )
})

#### mutual intercept ####

test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptAPIM(dvn,
      lvxname = "Sat", lvyname = "Com", scaleset = "FF",
      constr_dy_x_meas = c("loadings", "intercepts"), constr_dy_y_meas = c("loadings", "intercepts"),
      constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
      constr_dy_xy_struct = c("none"), includeMeanStruct = TRUE
    ),
    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nCom1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\ncom.1_1 ~ ty1*1\ncom.1_2 ~ ty2*1\ncom.1_3 ~ ty3*1\ncom.1_4 ~ ty4*1\ncom.1_5 ~ ty5*1\n\ncom.2_1 ~ ty1*1\ncom.2_2 ~ ty2*1\ncom.2_3 ~ ty3*1\ncom.2_4 ~ ty4*1\ncom.2_5 ~ ty5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\n\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ 1*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ NA*1\n\nCom1 ~ 0*1\nCom2 ~ NA*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
  )
})

test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptAPIM(dvn,
      lvxname = "Sat", lvyname = "Com", scaleset = "MV",
      constr_dy_x_meas = c("loadings", "intercepts"), constr_dy_y_meas = c("loadings", "intercepts"),
      constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
      constr_dy_xy_struct = c("none"), includeMeanStruct = TRUE
    ),
    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nCom1=~1*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\ncom.1_1 ~ 0*1 + ty1*1\ncom.1_2 ~ ty2*1\ncom.1_3 ~ ty3*1\ncom.1_4 ~ ty4*1\ncom.1_5 ~ ty5*1\n\ncom.2_1 ~ ty1*1\ncom.2_2 ~ ty2*1\ncom.2_3 ~ ty3*1\ncom.2_4 ~ ty4*1\ncom.2_5 ~ ty5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\n\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ NA*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\n\nCom1 ~ NA*1\nCom2 ~ NA*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
  )
})

#### mutual residual ####

test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= FF", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptAPIM(dvn,
      lvxname = "Sat", lvyname = "Com", scaleset = "FF",
      constr_dy_x_meas = c("loadings", "intercepts", "residuals"), constr_dy_y_meas = c("loadings", "intercepts", "residuals"),
      constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
      constr_dy_xy_struct = c("none"), includeMeanStruct = TRUE
    ),
    "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nCom1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\ncom.1_1 ~ ty1*1\ncom.1_2 ~ ty2*1\ncom.1_3 ~ ty3*1\ncom.1_4 ~ ty4*1\ncom.1_5 ~ ty5*1\n\ncom.2_1 ~ ty1*1\ncom.2_2 ~ ty2*1\ncom.2_3 ~ ty3*1\ncom.2_4 ~ ty4*1\ncom.2_5 ~ ty5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\ncom.1_1 ~~ thy1*com.1_1\ncom.1_2 ~~ thy2*com.1_2\ncom.1_3 ~~ thy3*com.1_3\ncom.1_4 ~~ thy4*com.1_4\ncom.1_5 ~~ thy5*com.1_5\n\ncom.2_1 ~~ thy1*com.2_1\ncom.2_2 ~~ thy2*com.2_2\ncom.2_3 ~~ thy3*com.2_3\ncom.2_4 ~~ thy4*com.2_4\ncom.2_5 ~~ thy5*com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ 1*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ NA*1\n\nCom1 ~ 0*1\nCom2 ~ NA*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
  )
})

test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= MV", {
  dvn <- scrapeVarCross(
    dat = commitmentQ,
    x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_",
    distinguish_1 = "1", distinguish_2 = "2"
  )


  expect_equal(
    scriptAPIM(dvn,
      lvxname = "Sat", lvyname = "Com", scaleset = "MV",
      constr_dy_x_meas = c("loadings", "intercepts", "residuals"), constr_dy_y_meas = c("loadings", "intercepts", "residuals"),
      constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
      constr_dy_xy_struct = c("none"), includeMeanStruct = TRUE
    ),
    "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nCom1=~1*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\ncom.1_1 ~ 0*1 + ty1*1\ncom.1_2 ~ ty2*1\ncom.1_3 ~ ty3*1\ncom.1_4 ~ ty4*1\ncom.1_5 ~ ty5*1\n\ncom.2_1 ~ ty1*1\ncom.2_2 ~ ty2*1\ncom.2_3 ~ ty3*1\ncom.2_4 ~ ty4*1\ncom.2_5 ~ ty5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\ncom.1_1 ~~ thy1*com.1_1\ncom.1_2 ~~ thy2*com.1_2\ncom.1_3 ~~ thy3*com.1_3\ncom.1_4 ~~ thy4*com.1_4\ncom.1_5 ~~ thy5*com.1_5\n\ncom.2_1 ~~ thy1*com.2_1\ncom.2_2 ~~ thy2*com.2_2\ncom.2_3 ~~ thy3*com.2_3\ncom.2_4 ~~ thy4*com.2_4\ncom.2_5 ~~ thy5*com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ NA*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\n\nCom1 ~ NA*1\nCom2 ~ NA*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
  )
})

#### mutual indist ####

#### Error Handling Tests for scriptAPIM ####
# ============================================
# These tests verify that scriptAPIM properly handles invalid inputs,
# missing required arguments, invalid parameter combinations, and malformed dvn objects.

#### Group 1: Invalid Input Types ####
# ------------------------------------
# These tests verify that scriptAPIM rejects inputs of incorrect types.

test_that("scriptAPIM rejects non-list dvn argument", {
  # Test that dvn must be a list object
  expect_error(
    scriptAPIM(dvn = "not_a_list", lvxname = "Sat", lvyname = "Com"),
    "The `dvn` argument must be a list object."
  )
})

test_that("scriptAPIM rejects non-character lvxname argument", {
  # Test that lvxname must be a character string
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_"
  )
  expect_error(
    scriptAPIM(dvn = dvn, lvxname = 123, lvyname = "Com"),
    "The `lvxname` argument must be a character string."
  )
})

test_that("scriptAPIM rejects non-character lvyname argument", {
  # Test that lvyname must be a character string
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_"
  )
  expect_error(
    scriptAPIM(dvn = dvn, lvxname = "Sat", lvyname = 123),
    "The `lvyname` argument must be a character string."
  )
})

test_that("scriptAPIM rejects non-logical est_k argument", {
  # Test that est_k must be a logical value
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_"
  )
  expect_error(
    scriptAPIM(dvn = dvn, lvxname = "Sat", lvyname = "Com", est_k = "yes"),
    "The `est_k` argument must be a logical value \\(TRUE or FALSE\\)."
  )
})

#### Group 2: Missing Required Arguments ####
# --------------------------------------------
# These tests ensure that required arguments are properly validated.

test_that("scriptAPIM requires dvn argument", {
  # Test that dvn is required
  expect_error(
    scriptAPIM(lvxname = "Sat", lvyname = "Com"),
    "The `dvn` argument is required and cannot be NULL."
  )
})

test_that("scriptAPIM requires lvxname argument", {
  # Test that lvxname is required
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_"
  )
  expect_error(
    scriptAPIM(dvn = dvn, lvyname = "Com"),
    "The `lvxname` argument is required and cannot be NULL."
  )
})

test_that("scriptAPIM requires lvyname argument", {
  # Test that lvyname is required
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_"
  )
  expect_error(
    scriptAPIM(dvn = dvn, lvxname = "Sat"),
    "The `lvyname` argument is required and cannot be NULL."
  )
})

#### Group 3: Invalid Parameter Combinations ####
# -------------------------------------------------
# These tests check that invalid parameter values are caught.

test_that("scriptAPIM rejects invalid constr_dy_x_meas values", {
  # Test that constr_dy_x_meas must contain valid options
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_"
  )
  expect_error(
    scriptAPIM(dvn = dvn, lvxname = "Sat", lvyname = "Com", constr_dy_x_meas = "invalid_option"),
    "constr_dy_meas must be a character vector containing any combination of 'loadings', 'intercepts', 'residuals', or 'none'"
  )
})

test_that("scriptAPIM rejects invalid scaleset value", {
  # Test that scaleset must be either "FF" or "MV"
  dvn <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
    y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_"
  )
  expect_error(
    scriptAPIM(dvn = dvn, lvxname = "Sat", lvyname = "Com", scaleset = "invalid"),
    "scaleset must be either 'FF' \\(fixed-factor\\) or 'MV' \\(marker variable\\)"
  )
})

#### Group 4: Malformed dvn Objects ####
# --------------------------------------
# These tests verify handling of incorrectly structured dvn objects.

test_that("scriptAPIM rejects dvn object with wrong length (6 elements instead of 9)", {
  # Test that scriptAPIM requires a 9-element dvn (X and Y), not 6-element (X only)
  dvn_x_only <- scrapeVarCross(
    dat = commitmentQ, x_order = "spi", x_stem = "sat.g",
    x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
  )
  expect_error(
    scriptAPIM(dvn = dvn_x_only, lvxname = "Sat", lvyname = "Com"),
    "You must supply a dvn object containing information for both X and Y"
  )
})

test_that("scriptAPIM rejects dvn object with missing required elements", {
  # Test that dvn must contain all required elements
  dvn_malformed <- list(
    p1xvarnames = c("var1", "var2"),
    p2xvarnames = c("var3", "var4")
    # Missing required elements for X and Y
  )
  expect_error(
    scriptAPIM(dvn = dvn_malformed, lvxname = "Sat", lvyname = "Com"),
    "You must supply a dvn object containing information for both X and Y"
  )
})
