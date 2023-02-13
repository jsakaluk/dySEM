#### type = C configural ####
test_that("scriptBiDy produces correct output for type = C, model = configural, equate = none", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptBiDy(dvn, lvxname = "Sat", type = "C", model = "configural"),
               "#Loadings\nSat1=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSatDy=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5+sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\nSat2=~NA*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\n#(Co)Variances\nSatDy ~~ 1*SatDy\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Residuals\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Latent Means\nSatDy ~ 0*1\nSat1 ~ 0*1\nSat2 ~ 0*1"
               )
})


#### type = C loading ####
test_that("scriptBiDy produces correct output for type = C, model = loading, equate = none", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptBiDy(dvn, lvxname = "Sat", type = "C", model = "loading"),
               "#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSatDy=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\n#(Co)Variances\nSatDy ~~ 1*SatDy\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Residuals\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Latent Means\nSatDy ~ 0*1\nSat1 ~ 0*1\nSat2 ~ 0*1"
               )
})

#### type = C loading_source ####
test_that("scriptBiDy produces correct output for type = C, model = loading_source, equate = none", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptBiDy(dvn, lvxname = "Sat", type = "C", model = "loading_source"),
               "#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSatDy=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5+lx6*sat.g.2_1+lx7*sat.g.2_2+lx8*sat.g.2_3+lx9*sat.g.2_4+lx10*sat.g.2_5\nSat2=~NA*sat.g.2_1+lx6*sat.g.2_1+lx7*sat.g.2_2+lx8*sat.g.2_3+lx9*sat.g.2_4+lx10*sat.g.2_5\n\n#(Co)Variances\nSatDy ~~ 1*SatDy\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Residuals\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Latent Means\nSatDy ~ 0*1\nSat1 ~ 0*1\nSat2 ~ 0*1"
               )
})

#### type = C loading_releq ####
test_that("scriptBiDy produces correct output for type = C, model = loading_releq, equate = none", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptBiDy(dvn, lvxname = "Sat", type = "C", model = "loading_releq"),
               "#Loadings\nSat1=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSatDy=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nSat2=~NA*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\n\n#(Co)Variances\nSatDy ~~ 1*SatDy\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\n\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\n\n#Residuals\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\n\n#Latent Means\nSatDy ~ 0*1\nSat1 ~ 0*1\nSat2 ~ 0*1"  )
})

#### type = S configural ####
test_that("scriptBiDy produces correct output for type = S, model = configural", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptBiDy(dvn, lvxname = "Sat", lvyname = "Com", type = "S", model = "configural"),
               "#Loadings\nSat1=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSatDy=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5+sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\nSat2=~NA*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\nCom1=~NA*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5\nComDy=~NA*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5+com.2_1+com.2_2+com.2_3+com.2_4+com.2_5\nCom2=~NA*com.2_1+com.2_2+com.2_3+com.2_4+com.2_5\n\n#(Co)Variances\nSatDy ~~ 1*SatDy\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\nComDy ~~ 1*ComDy\nCom1 ~~ 1*Com1\nCom2 ~~ 1*Com2\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\nComDy ~~ 0*Com1\nComDy ~~ 0*Com2\nCom1 ~~ 0*Com2\nSatDy ~~ 0*Com1\nSatDy ~~ 0*Com2\nComDy ~~ 0*Sat1\nComDy ~~ 0*Sat2\nSat1 ~~ 0*Com2\nCom1 ~~ 0*Sat2\n\n#Actor and Dyadic Effects\nSat1 ~~ a1*Com1\nSat2 ~~ a2*Com2\nSatDy ~~ ComDy\n\n#Residuals\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\ncom.1_1 ~ 1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\ncom.2_1 ~ 1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Latent Means\nSatDy ~ 0*1\nSat1 ~ 0*1\nSat2 ~ 0*1\nComDy ~ 0*1\nCom1 ~ 0*1\nCom2 ~ 0*1"
               )
})

test_that("scriptBiDy produces correct output for type = S, model = configural and equate = actor", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptBiDy(dvn, lvxname = "Sat", lvyname = "Com", type = "S", model = "configural", equate = "actor"),
               "#Loadings\nSat1=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSatDy=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5+sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\nSat2=~NA*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\nCom1=~NA*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5\nComDy=~NA*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5+com.2_1+com.2_2+com.2_3+com.2_4+com.2_5\nCom2=~NA*com.2_1+com.2_2+com.2_3+com.2_4+com.2_5\n\n#(Co)Variances\nSatDy ~~ 1*SatDy\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\nComDy ~~ 1*ComDy\nCom1 ~~ 1*Com1\nCom2 ~~ 1*Com2\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\nComDy ~~ 0*Com1\nComDy ~~ 0*Com2\nCom1 ~~ 0*Com2\nSatDy ~~ 0*Com1\nSatDy ~~ 0*Com2\nComDy ~~ 0*Sat1\nComDy ~~ 0*Sat2\nSat1 ~~ 0*Com2\nCom1 ~~ 0*Sat2\n\n#Actor and Dyadic Effects\nSat1 ~~ a1*Com1\nSat2 ~~ a1*Com2\nSatDy ~~ ComDy\n\n#Residuals\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\ncom.1_1 ~ 1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\ncom.2_1 ~ 1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Latent Means\nSatDy ~ 0*1\nSat1 ~ 0*1\nSat2 ~ 0*1\nComDy ~ 0*1\nCom1 ~ 0*1\nCom2 ~ 0*1"
               )
})
#### type = S loading ####
test_that("scriptBiDy produces correct output for type = S, model = configural", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptBiDy(dvn, lvxname = "Sat", lvyname = "Com", type = "S", model = "loading"),
               "#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSatDy=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nCom1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nComDy=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#(Co)Variances\nSatDy ~~ 1*SatDy\nSat1 ~~ NA*Sat1\nSat2 ~~ NA*Sat2\nComDy ~~ 1*ComDy\nCom1 ~~ NA*Com1\nCom2 ~~ NA*Com2\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\nComDy ~~ 0*Com1\nComDy ~~ 0*Com2\nCom1 ~~ 0*Com2\nSatDy ~~ 0*Com1\nSatDy ~~ 0*Com2\nComDy ~~ 0*Sat1\nComDy ~~ 0*Sat2\nSat1 ~~ 0*Com2\nCom1 ~~ 0*Sat2\n\n#Actor and Dyadic Effects\nSat1 ~~ a1*Com1\nSat2 ~~ a2*Com2\nSatDy ~~ ComDy\n\n#Residuals\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\ncom.1_1 ~ 1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\ncom.2_1 ~ 1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Latent Means\nSatDy ~ 0*1\nSat1 ~ 0*1\nSat2 ~ 0*1\nComDy ~ 0*1\nCom1 ~ 0*1\nCom2 ~ 0*1"
               )
})

test_that("scriptBiDy produces correct output for type = S, model = configural and equate = actor", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptBiDy(dvn, lvxname = "Sat", lvyname = "Com", type = "S", model = "configural", equate = "actor"),
               "#Loadings\nSat1=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSatDy=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5+sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\nSat2=~NA*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\nCom1=~NA*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5\nComDy=~NA*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5+com.2_1+com.2_2+com.2_3+com.2_4+com.2_5\nCom2=~NA*com.2_1+com.2_2+com.2_3+com.2_4+com.2_5\n\n#(Co)Variances\nSatDy ~~ 1*SatDy\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\nComDy ~~ 1*ComDy\nCom1 ~~ 1*Com1\nCom2 ~~ 1*Com2\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\nComDy ~~ 0*Com1\nComDy ~~ 0*Com2\nCom1 ~~ 0*Com2\nSatDy ~~ 0*Com1\nSatDy ~~ 0*Com2\nComDy ~~ 0*Sat1\nComDy ~~ 0*Sat2\nSat1 ~~ 0*Com2\nCom1 ~~ 0*Sat2\n\n#Actor and Dyadic Effects\nSat1 ~~ a1*Com1\nSat2 ~~ a1*Com2\nSatDy ~~ ComDy\n\n#Residuals\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\ncom.1_1 ~ 1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\ncom.2_1 ~ 1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Latent Means\nSatDy ~ 0*1\nSat1 ~ 0*1\nSat2 ~ 0*1\nComDy ~ 0*1\nCom1 ~ 0*1\nCom2 ~ 0*1"
               )
})

#### type = S loading_source ####
test_that("scriptBiDy produces correct output for type = S, model = loading_source", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptBiDy(dvn, lvxname = "Sat", lvyname = "Com", type = "S", model = "loading_source"),
               "#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSatDy=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5+lx6*sat.g.2_1+lx7*sat.g.2_2+lx8*sat.g.2_3+lx9*sat.g.2_4+lx10*sat.g.2_5\nSat2=~NA*sat.g.2_1+lx6*sat.g.2_1+lx7*sat.g.2_2+lx8*sat.g.2_3+lx9*sat.g.2_4+lx10*sat.g.2_5\nCom1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nComDy=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5+ly6*com.2_1+ly7*com.2_2+ly8*com.2_3+ly9*com.2_4+ly10*com.2_5\nCom2=~NA*com.2_1+ly6*com.2_1+ly7*com.2_2+ly8*com.2_3+ly9*com.2_4+ly10*com.2_5\n\n#(Co)Variances\nSatDy ~~ 1*SatDy\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\nComDy ~~ 1*ComDy\nCom1 ~~ 1*Com1\nCom2 ~~ 1*Com2\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\nComDy ~~ 0*Com1\nComDy ~~ 0*Com2\nCom1 ~~ 0*Com2\nSatDy ~~ 0*Com1\nSatDy ~~ 0*Com2\nComDy ~~ 0*Sat1\nComDy ~~ 0*Sat2\nSat1 ~~ 0*Com2\nCom1 ~~ 0*Sat2\n\n#Actor and Dyadic Effects\nSat1 ~~ a1*Com1\nSat2 ~~ a2*Com2\nSatDy ~~ ComDy\n\n#Residuals\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\ncom.1_1 ~ 1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\ncom.2_1 ~ 1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Latent Means\nSatDy ~ 0*1\nSat1 ~ 0*1\nSat2 ~ 0*1\nComDy ~ 0*1\nCom1 ~ 0*1\nCom2 ~ 0*1"
               )
})

test_that("scriptBiDy produces correct output for type = S, model = loading_source and equate = actor", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptBiDy(dvn, lvxname = "Sat", lvyname = "Com", type = "S", model = "loading_source", equate = "actor"),
               "#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSatDy=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5+lx6*sat.g.2_1+lx7*sat.g.2_2+lx8*sat.g.2_3+lx9*sat.g.2_4+lx10*sat.g.2_5\nSat2=~NA*sat.g.2_1+lx6*sat.g.2_1+lx7*sat.g.2_2+lx8*sat.g.2_3+lx9*sat.g.2_4+lx10*sat.g.2_5\nCom1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nComDy=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5+ly6*com.2_1+ly7*com.2_2+ly8*com.2_3+ly9*com.2_4+ly10*com.2_5\nCom2=~NA*com.2_1+ly6*com.2_1+ly7*com.2_2+ly8*com.2_3+ly9*com.2_4+ly10*com.2_5\n\n#(Co)Variances\nSatDy ~~ 1*SatDy\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\nComDy ~~ 1*ComDy\nCom1 ~~ 1*Com1\nCom2 ~~ 1*Com2\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\nComDy ~~ 0*Com1\nComDy ~~ 0*Com2\nCom1 ~~ 0*Com2\nSatDy ~~ 0*Com1\nSatDy ~~ 0*Com2\nComDy ~~ 0*Sat1\nComDy ~~ 0*Sat2\nSat1 ~~ 0*Com2\nCom1 ~~ 0*Sat2\n\n#Actor and Dyadic Effects\nSat1 ~~ a1*Com1\nSat2 ~~ a1*Com2\nSatDy ~~ ComDy\n\n#Residuals\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\ncom.1_1 ~ 1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\ncom.2_1 ~ 1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Latent Means\nSatDy ~ 0*1\nSat1 ~ 0*1\nSat2 ~ 0*1\nComDy ~ 0*1\nCom1 ~ 0*1\nCom2 ~ 0*1"
               )
})

#### type = S loading_source ####
test_that("scriptBiDy produces correct output for type = S, model = loading_releq", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptBiDy(dvn, lvxname = "Sat", lvyname = "Com", type = "S", model = "loading_releq"),
               "#Loadings\nSat1=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSatDy=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nSat2=~NA*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\nCom1=~NA*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5\nComDy=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\nCom2=~NA*com.2_1+com.2_2+com.2_3+com.2_4+com.2_5\n\n#(Co)Variances\nSatDy ~~ 1*SatDy\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\nComDy ~~ 1*ComDy\nCom1 ~~ 1*Com1\nCom2 ~~ 1*Com2\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\nComDy ~~ 0*Com1\nComDy ~~ 0*Com2\nCom1 ~~ 0*Com2\nSatDy ~~ 0*Com1\nSatDy ~~ 0*Com2\nComDy ~~ 0*Sat1\nComDy ~~ 0*Sat2\nSat1 ~~ 0*Com2\nCom1 ~~ 0*Sat2\n\n#Actor and Dyadic Effects\nSat1 ~~ a1*Com1\nSat2 ~~ a2*Com2\nSatDy ~~ ComDy\n\n#Residuals\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\ncom.1_1 ~ 1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\ncom.2_1 ~ 1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Latent Means\nSatDy ~ 0*1\nSat1 ~ 0*1\nSat2 ~ 0*1\nComDy ~ 0*1\nCom1 ~ 0*1\nCom2 ~ 0*1"
               )
})

test_that("scriptBiDy produces correct output for type = S, model = loading_releq and equate = actor", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")


  expect_equal(scriptBiDy(dvn, lvxname = "Sat", lvyname = "Com", type = "S", model = "loading_releq", equate = "actor"),
               "#Loadings\nSat1=~NA*sat.g.1_1+sat.g.1_2+sat.g.1_3+sat.g.1_4+sat.g.1_5\nSatDy=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\nSat2=~NA*sat.g.2_1+sat.g.2_2+sat.g.2_3+sat.g.2_4+sat.g.2_5\nCom1=~NA*com.1_1+com.1_2+com.1_3+com.1_4+com.1_5\nComDy=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\nCom2=~NA*com.2_1+com.2_2+com.2_3+com.2_4+com.2_5\n\n#(Co)Variances\nSatDy ~~ 1*SatDy\nSat1 ~~ 1*Sat1\nSat2 ~~ 1*Sat2\nComDy ~~ 1*ComDy\nCom1 ~~ 1*Com1\nCom2 ~~ 1*Com2\nSatDy ~~ 0*Sat1\nSatDy ~~ 0*Sat2\nSat1 ~~ 0*Sat2\nComDy ~~ 0*Com1\nComDy ~~ 0*Com2\nCom1 ~~ 0*Com2\nSatDy ~~ 0*Com1\nSatDy ~~ 0*Com2\nComDy ~~ 0*Sat1\nComDy ~~ 0*Sat2\nSat1 ~~ 0*Com2\nCom1 ~~ 0*Sat2\n\n#Actor and Dyadic Effects\nSat1 ~~ a1*Com1\nSat2 ~~ a1*Com2\nSatDy ~~ ComDy\n\n#Residuals\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\nsat.g.1_1 ~~ sat.g.1_1\nsat.g.1_2 ~~ sat.g.1_2\nsat.g.1_3 ~~ sat.g.1_3\nsat.g.1_4 ~~ sat.g.1_4\nsat.g.1_5 ~~ sat.g.1_5\nsat.g.2_1 ~~ sat.g.2_1\nsat.g.2_2 ~~ sat.g.2_2\nsat.g.2_3 ~~ sat.g.2_3\nsat.g.2_4 ~~ sat.g.2_4\nsat.g.2_5 ~~ sat.g.2_5\ncom.1_1 ~~ com.1_1\ncom.1_2 ~~ com.1_2\ncom.1_3 ~~ com.1_3\ncom.1_4 ~~ com.1_4\ncom.1_5 ~~ com.1_5\ncom.2_1 ~~ com.2_1\ncom.2_2 ~~ com.2_2\ncom.2_3 ~~ com.2_3\ncom.2_4 ~~ com.2_4\ncom.2_5 ~~ com.2_5\n\n#Intercepts\nsat.g.1_1 ~ 1\nsat.g.1_2 ~ 1\nsat.g.1_3 ~ 1\nsat.g.1_4 ~ 1\nsat.g.1_5 ~ 1\nsat.g.2_1 ~ 1\nsat.g.2_2 ~ 1\nsat.g.2_3 ~ 1\nsat.g.2_4 ~ 1\nsat.g.2_5 ~ 1\ncom.1_1 ~ 1\ncom.1_2 ~ 1\ncom.1_3 ~ 1\ncom.1_4 ~ 1\ncom.1_5 ~ 1\ncom.2_1 ~ 1\ncom.2_2 ~ 1\ncom.2_3 ~ 1\ncom.2_4 ~ 1\ncom.2_5 ~ 1\n\n#Latent Means\nSatDy ~ 0*1\nSat1 ~ 0*1\nSat2 ~ 0*1\nComDy ~ 0*1\nCom1 ~ 0*1\nCom2 ~ 0*1"
               )
})
