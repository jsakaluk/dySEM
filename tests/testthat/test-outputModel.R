#### Error messages ####

test_that("writeTo == NULL returns a) error with b) correct error message", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  script_ff <- scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF",
                          constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
                          constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                          constr_dy_xy_struct = c("none"))

  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  expect_error({
    outputModel(dvn = dvn, model = "apim", fit =mod_ff,
                table = TRUE, tabletype = "measurement",
                figure = TRUE, figtype = "standardized",
                writeTo = NULL, fileName = "apim_fig")
  }, "Must specify a directory to which the file should be saved. \n Use writeTo = '.' to save output file(s) in the current working directory", fixed = TRUE)
})

test_that("writeTo == 3 returns a) error with b) correct error message", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  script_ff <- scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF",
                          constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
                          constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                          constr_dy_xy_struct = c("none"))

  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  expect_error({
    outputModel(dvn = dvn, model = "apim", fit =mod_ff,
                table = TRUE, tabletype = "measurement",
                figure = TRUE, figtype = "standardized",
                writeTo = 3, fileName = "apim_fig")
  }, "The `writeTo` argument must be a character string. \n Use writeTo = '.' to save output file(s) in the current working directory", fixed = TRUE)
})

test_that("fileName == 3 returns a) error with b) correct error message", {

  dvn <- scrapeVarCross(dat = commitmentQ,
                        x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2="_",
                        y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_",
                        distinguish_1="1", distinguish_2="2")

  script_ff <- scriptAPIM(dvn, lvxname = "Sat", lvyname = "Com", scaleset = "FF",
                          constr_dy_x_meas = c("none"), constr_dy_y_meas = c("none"),
                          constr_dy_x_struct = c("none"), constr_dy_y_struct = c("none"),
                          constr_dy_xy_struct = c("none"))

  mod_ff <- lavaan::cfa(script_ff, data = commitmentQ)

  expect_error({
    outputModel(dvn = dvn, model = "apim", fit =mod_ff,
                table = TRUE, tabletype = "measurement",
                figure = TRUE, figtype = "standardized",
                writeTo = tempdir(), fileName = 3)
  }, "The `fileName` argument must be a character string", fixed = TRUE)
})


