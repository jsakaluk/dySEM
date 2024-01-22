#### mutual configural ####
test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF",
                          constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
                          constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                          constr_dy_xy_struct = c("none")),
               "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSat2=~NA*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\nCom1=~NA*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5\nCom2=~NA*com.2_1+com.2_2+com.2_3+com.2_4+com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\ncom.1_1 ~ 1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\n\ncom.2_1 ~ 1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\n\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ 1*Com1\nCom2 ~~ 1*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ 0*1\n\nCom1 ~ 0*1\nCom2 ~ 0*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
               )
})

test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= MV", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "MV",
                          constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
                          constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                          constr_dy_xy_struct = c("none")),
               "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSat2=~1*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\nCom1=~1*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5\nCom2=~1*com.2_1+com.2_2+com.2_3+com.2_4+com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\ncom.1_1 ~ 0*1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\n\ncom.2_1 ~ 0*1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\n\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ NA*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\n\nCom1 ~ NA*1\nCom2 ~ NA*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
               )
})

test_that("scriptAPIM produces correct number of parameter estimatesfor constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  #knowns for model :
  #var_num <- 20
  #knowns <- var_num*(var_num+1)/2+var_num

  #Script model w/ scaleset = FF
  script <- scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF",
                       constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
                       constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                       constr_dy_xy_struct = c("none"))

  #Fit model
  mod <- lavaan::cfa(script, data = commitmentQ)

  #manually calculated free parameters (see LINK)
  #my_param <- 76

  lav_param <- as.double(lavaan::fitmeasures(mod, "npar"))

  expect_equal(lav_param,
               76
  )

})

test_that("scriptAPIM produces correct df for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  #knowns for model :
  #var_num <- 20
  #knowns <- var_num*(var_num+1)/2+var_num

  #Script model w/ scaleset = FF
  script_ff <- scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF",
                       constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
                       constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                       constr_dy_xy_struct = c("none"))

  #Fit model
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #manually calculated free parameters (see LINK)
  #my_param <- 76

  #calculated df (should be 154)
  #my_df <- knowns-my_param

  lav_df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  expect_equal(lav_df_ff,
               154
  )

})

test_that("scriptAPIM produces same df for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  #knowns for model :
  #var_num <- 20
  #knowns <- var_num*(var_num+1)/2+var_num

  #Script model w/ scaleset = FF
  script_ff <- scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF",
                          constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
                          constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                          constr_dy_xy_struct = c("none"))

  #Fit model
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #df for FF model
  lav_df_ff <- as.double(lavaan::fitmeasures(mod_ff, "df"))

  #Script model w/ scaleset = FF
  script_mv <- scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "MV",
                          constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
                          constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                          constr_dy_xy_struct = c("none"))

  #Fit model
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #df for FF model
  lav_df_mv <- as.double(lavaan::fitmeasures(mod_mv, "df"))

  expect_equal(lav_df_ff,
               lav_df_mv
  )

})

test_that("scriptAPIM produces same chisq for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none when scaleset= FF, and scaleset = MV", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  #knowns for model :
  #var_num <- 20
  #knowns <- var_num*(var_num+1)/2+var_num

  #Script model w/ scaleset = FF
  script_ff <- scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF",
                          constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
                          constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                          constr_dy_xy_struct = c("none"))

  #Fit model
  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  #df for FF model
  lav_chisq_ff <- as.double(lavaan::fitmeasures(mod_ff, "chisq"))

  #Script model w/ scaleset = FF
  script_mv <- scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "MV",
                          constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
                          constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                          constr_dy_xy_struct = c("none"))

  #Fit model
  mod_mv <- lavaan::cfa(script_mv, data = commitmentQ)

  #df for FF model
  lav_chisq_mv <- as.double(lavaan::fitmeasures(mod_mv, "chisq"))

  expect_equal(lav_chisq_ff,
               lav_chisq_mv
  )

})

#### mutual loading ####
test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF",
                          constr_dy_x_meas = c("loadings"), constr_dy_y_meas = c("loadings"),
                          constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                          constr_dy_xy_struct = c("none")),
               "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nCom1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\ncom.1_1 ~ 1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\n\ncom.2_1 ~ 1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\n\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ 1*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ 0*1\n\nCom1 ~ 0*1\nCom2 ~ 0*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
               )
})

test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= MV", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "MV",
                          constr_dy_x_meas = c("loadings"), constr_dy_y_meas = c("loadings"),
                          constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                          constr_dy_xy_struct = c("none")),
               "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nCom1=~1*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\n\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\ncom.1_1 ~ 0*1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\n\ncom.2_1 ~ 0*1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\n\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ NA*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\n\nCom1 ~ NA*1\nCom2 ~ NA*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
               )
})

#### mutual intercept ####

test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF",
                          constr_dy_x_meas = c("loadings", "intercepts"), constr_dy_y_meas = c("loadings", "intercepts"),
                          constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                          constr_dy_xy_struct = c("none")),
               "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nCom1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\ncom.1_1 ~ ty1*1\ncom.1_2 ~ ty2*1\ncom.1_3 ~ ty3*1\ncom.1_4 ~ ty4*1\ncom.1_5 ~ ty5*1\n\ncom.2_1 ~ ty1*1\ncom.2_2 ~ ty2*1\ncom.2_3 ~ ty3*1\ncom.2_4 ~ ty4*1\ncom.2_5 ~ ty5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\n\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ 1*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ NA*1\n\nCom1 ~ 0*1\nCom2 ~ NA*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
               )
})

test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= MV", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "MV",
                          constr_dy_x_meas = c("loadings", "intercepts"), constr_dy_y_meas = c("loadings", "intercepts"),
                          constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                          constr_dy_xy_struct = c("none")),
               "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nCom1=~1*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\ncom.1_1 ~ 0*1 + ty1*1\ncom.1_2 ~ ty2*1\ncom.1_3 ~ ty3*1\ncom.1_4 ~ ty4*1\ncom.1_5 ~ ty5*1\n\ncom.2_1 ~ ty1*1\ncom.2_2 ~ ty2*1\ncom.2_3 ~ ty3*1\ncom.2_4 ~ ty4*1\ncom.2_5 ~ ty5*1\n\n#Residual Variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\n\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\n\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ NA*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\n\nCom1 ~ NA*1\nCom2 ~ NA*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
               )
})

#### mutual residual ####

test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF",
                          constr_dy_x_meas = c("loadings", "intercepts", "residuals"), constr_dy_y_meas = c("loadings", "intercepts", "residuals"),
                          constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                          constr_dy_xy_struct = c("none")),
               "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nCom1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\ncom.1_1 ~ ty1*1\ncom.1_2 ~ ty2*1\ncom.1_3 ~ ty3*1\ncom.1_4 ~ ty4*1\ncom.1_5 ~ ty5*1\n\ncom.2_1 ~ ty1*1\ncom.2_2 ~ ty2*1\ncom.2_3 ~ ty3*1\ncom.2_4 ~ ty4*1\ncom.2_5 ~ ty5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\ncom.1_1 ~~ thy1*com.1_1\ncom.1_2 ~~ thy2*com.1_2\ncom.1_3 ~~ thy3*com.1_3\ncom.1_4 ~~ thy4*com.1_4\ncom.1_5 ~~ thy5*com.1_5\n\ncom.2_1 ~~ thy1*com.2_1\ncom.2_2 ~~ thy2*com.2_2\ncom.2_3 ~~ thy3*com.2_3\ncom.2_4 ~~ thy4*com.2_4\ncom.2_5 ~~ thy5*com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ 1*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ NA*1\n\nCom1 ~ 0*1\nCom2 ~ NA*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
               )
})

test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= MV", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "MV",
                          constr_dy_x_meas = c("loadings", "intercepts", "residuals"), constr_dy_y_meas = c("loadings", "intercepts", "residuals"),
                          constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                          constr_dy_xy_struct = c("none")),
               "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nCom1=~1*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\ncom.1_1 ~ 0*1 + ty1*1\ncom.1_2 ~ ty2*1\ncom.1_3 ~ ty3*1\ncom.1_4 ~ ty4*1\ncom.1_5 ~ ty5*1\n\ncom.2_1 ~ ty1*1\ncom.2_2 ~ ty2*1\ncom.2_3 ~ ty3*1\ncom.2_4 ~ ty4*1\ncom.2_5 ~ ty5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\ncom.1_1 ~~ thy1*com.1_1\ncom.1_2 ~~ thy2*com.1_2\ncom.1_3 ~~ thy3*com.1_3\ncom.1_4 ~~ thy4*com.1_4\ncom.1_5 ~~ thy5*com.1_5\n\ncom.2_1 ~~ thy1*com.2_1\ncom.2_2 ~~ thy2*com.2_2\ncom.2_3 ~~ thy3*com.2_3\ncom.2_4 ~~ thy4*com.2_4\ncom.2_5 ~~ thy5*com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ NA*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\n\nCom1 ~ NA*1\nCom2 ~ NA*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
               )
})

#### mutual indist ####

test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF",
                          constr_dy_x_meas = c("loadings", "intercepts", "residuals"), constr_dy_y_meas = c("loadings", "intercepts", "residuals"),
                          constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                          constr_dy_xy_struct = c("none")),
               "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nCom1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\ncom.1_1 ~ ty1*1\ncom.1_2 ~ ty2*1\ncom.1_3 ~ ty3*1\ncom.1_4 ~ ty4*1\ncom.1_5 ~ ty5*1\n\ncom.2_1 ~ ty1*1\ncom.2_2 ~ ty2*1\ncom.2_3 ~ ty3*1\ncom.2_4 ~ ty4*1\ncom.2_5 ~ ty5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\ncom.1_1 ~~ thy1*com.1_1\ncom.1_2 ~~ thy2*com.1_2\ncom.1_3 ~~ thy3*com.1_3\ncom.1_4 ~~ thy4*com.1_4\ncom.1_5 ~~ thy5*com.1_5\n\ncom.2_1 ~~ thy1*com.2_1\ncom.2_2 ~~ thy2*com.2_2\ncom.2_3 ~~ thy3*com.2_3\ncom.2_4 ~~ thy4*com.2_4\ncom.2_5 ~~ thy5*com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ 1*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ NA*1\n\nCom1 ~ 0*1\nCom2 ~ NA*1\n\n#Latent Actor Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\n\n#Latent Partner Effects\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
  )
})

test_that("scriptAPIM produces correct output for constr_dy_x/y_meas = none, constr_dy_x/y_struct = none, constr_dy_xy_struct = none and scaleset= MV", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "MV",
                          constr_dy_x_meas = c("loadings", "intercepts", "residuals"), constr_dy_y_meas = c("loadings", "intercepts", "residuals"),
                          constr_dy_x_struct = c("variances", "means"), constr_dy_y_struct = c("variances", "means"),
                          constr_dy_xy_struct = c("actors", "partners")),
               "#Measurement Model\n\n#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nCom1=~1*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\n\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\n\ncom.1_1 ~ 0*1 + ty1*1\ncom.1_2 ~ ty2*1\ncom.1_3 ~ ty3*1\ncom.1_4 ~ ty4*1\ncom.1_5 ~ ty5*1\n\ncom.2_1 ~ ty1*1\ncom.2_2 ~ ty2*1\ncom.2_3 ~ ty3*1\ncom.2_4 ~ ty4*1\ncom.2_5 ~ ty5*1\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\n\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\ncom.1_1 ~~ thy1*com.1_1\ncom.1_2 ~~ thy2*com.1_2\ncom.1_3 ~~ thy3*com.1_3\ncom.1_4 ~~ thy4*com.1_4\ncom.1_5 ~~ thy5*com.1_5\n\ncom.2_1 ~~ thy1*com.2_1\ncom.2_2 ~~ thy2*com.2_2\ncom.2_3 ~~ thy3*com.2_3\ncom.2_4 ~~ thy4*com.2_4\ncom.2_5 ~~ thy5*com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ psix*Sat1\nSat2 ~~ psix*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ psiy*Com1\nCom2 ~~ psiy*Com2\nCom1 ~~ Com2\n\n#Latent Means\nSat1 ~ alphax*1\nSat2 ~ alphax*1\n\nCom1 ~ alphay*1\nCom2 ~ alphay*1\n\n#Latent Actor Effects\nCom1 ~ a*Sat1\nCom2 ~ a*Sat2\n\n#Latent Partner Effects\nCom1 ~ p*Sat2\nCom2 ~ p*Sat1"
               )
})
