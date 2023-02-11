#### configural ####
test_that("scriptAPIM produces correct output for model = configural and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", model = "configural", scaleset = "FF"),
               "#Loadings\nSat1=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSat2=~NA*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\nCom1=~NA*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5\nCom2=~NA*com.2_1+com.2_2+com.2_3+com.2_4+com.2_5\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\nSat1 ~~ Sat2\nCom1 ~~ 1*Com1\nCom2 ~~ 1*Com2\nCom1 ~~ Com2\n\n#Residual (Co)variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\ncom.1_1 ~ 1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\ncom.2_1 ~ 1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ 0*1\nCom1 ~ 0*1\nCom2 ~ 0*1\n\n#Actor and Partner Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
  )
})

test_that("scriptAPIM produces correct output for model = configural and scaleset= MV", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", model = "configural", scaleset = "MV"),
               "#Loadings\nSat1=~1*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSat2=~1*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\nCom1=~1*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5\nCom2=~1*com.2_1+com.2_2+com.2_3+com.2_4+com.2_5\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\nCom1 ~~ NA*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Residual (Co)variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\ncom.1_1 ~ 0*1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\ncom.2_1 ~ 0*1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\nCom1 ~ NA*1\nCom2 ~ NA*1\n\n#Actor and Partner Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
  )
})

#### loading ####
test_that("scriptAPIM produces correct output for model = loading and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", model = "loading", scaleset = "FF"),
               "#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nCom1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\nCom1 ~~ 1*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Residual (Co)variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\ncom.1_1 ~ 1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\ncom.2_1 ~ 1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ 0*1\nCom1 ~ 0*1\nCom2 ~ 0*1\n\n#Actor and Partner Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
  )
})

test_that("scriptAPIM produces correct output for model = loading and scaleset= MV", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", model = "loading", scaleset = "MV"),
               "#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nCom1=~1*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\nCom1 ~~ NA*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Residual (Co)variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\nsat.g.2_1 ~ 0*1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\ncom.1_1 ~ 0*1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\ncom.2_1 ~ 0*1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\nCom1 ~ NA*1\nCom2 ~ NA*1\n\n#Actor and Partner Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
  )
})

#### intercept ####
test_that("scriptAPIM produces correct output for model = intercept and scaleset= FF", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", model = "intercept", scaleset = "FF"),
               "#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nCom1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\nCom1 ~~ 1*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Residual (Co)variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Intercepts\nsat.g.1_1 ~ tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\ncom.1_1 ~ ty1*1\ncom.1_2 ~ ty2*1\ncom.1_3 ~ ty3*1\ncom.1_4 ~ ty4*1\ncom.1_5 ~ ty5*1\ncom.2_1 ~ ty1*1\ncom.2_2 ~ ty2*1\ncom.2_3 ~ ty3*1\ncom.2_4 ~ ty4*1\ncom.2_5 ~ ty5*1\n\n#Latent Means\nSat1 ~ 0*1\nSat2 ~ NA*1\nCom1 ~ 0*1\nCom2 ~ NA*1\n\n#Actor and Partner Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
  )
})

test_that("scriptAPIM produces correct output for model = intercept and scaleset= MV", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", model = "intercept", scaleset = "MV"),
               "#Loadings\nSat1=~1*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nCom1=~1*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Latent (Co)Variances\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nSat1 ~~ Sat2\nCom1 ~~ NA*Com1\nCom2 ~~ NA*Com2\nCom1 ~~ Com2\n\n#Residual (Co)variances\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 0*1 + tx1*1\nsat.g.1_2 ~ tx2*1\nsat.g.1_3 ~ tx3*1\nsat.g.1_4 ~ tx4*1\nsat.g.1_5 ~ tx5*1\nsat.g.2_1 ~ tx1*1\nsat.g.2_2 ~ tx2*1\nsat.g.2_3 ~ tx3*1\nsat.g.2_4 ~ tx4*1\nsat.g.2_5 ~ tx5*1\ncom.1_1 ~ 0*1 + ty1*1\ncom.1_2 ~ ty2*1\ncom.1_3 ~ ty3*1\ncom.1_4 ~ ty4*1\ncom.1_5 ~ ty5*1\ncom.2_1 ~ ty1*1\ncom.2_2 ~ ty2*1\ncom.2_3 ~ ty3*1\ncom.2_4 ~ ty4*1\ncom.2_5 ~ ty5*1\n\n#Latent Means\nSat1 ~ NA*1\nSat2 ~ NA*1\nCom1 ~ NA*1\nCom2 ~ NA*1\n\n#Actor and Partner Effects\nCom1 ~ a1*Sat1\nCom2 ~ a2*Sat2\nCom1 ~ p1*Sat2\nCom2 ~ p2*Sat1"
  )
})
