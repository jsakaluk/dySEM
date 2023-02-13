test_that("scriptObsAPIM produces correct output for equate = none k = FALSE", {

  expect_equal(scriptObsAPIM(X1 = "Sat_A", Y1 = "Com_A",
                             X2 = "Sat_B", Y2 = "Com_B",
                             equate = "none", k = FALSE),

               "#Actor and Partner Effects\nCom_A ~ a1*Sat_A + p1*Sat_B\nCom_B ~ a2*Sat_B + p2*Sat_A\n\n#ICC and Residual ICC\nSat_A ~~ Sat_B\nCom_A ~~ Com_B"
              )
})

test_that("scriptObsAPIM produces correct output for equate = none k = TRUE", {

  expect_equal(scriptObsAPIM(X1 = "Sat_A", Y1 = "Com_A",
                             X2 = "Sat_B", Y2 = "Com_B",
                             equate = "none", k = TRUE),

               "#Actor and Partner Effects\nCom_A ~ a1*Sat_A + p1*Sat_B\nCom_B ~ a2*Sat_B + p2*Sat_A\n\n#ICC and Residual ICC\nSat_A ~~ Sat_B\nCom_A ~~ Com_B\n\n# k parameter(s)\nk1 := p1/a1\nk2 := p2/a2"
  )
})

test_that("scriptObsAPIM produces correct output for equate = all k = TRUE", {

  expect_equal(scriptObsAPIM(X1 = "Sat_A", Y1 = "Com_A",
                             X2 = "Sat_B", Y2 = "Com_B",
                             equate = "all", k = TRUE),

               "#Actor and Partner Effects\nCom_A ~ a*Sat_A + p*Sat_B\nCom_B ~ a*Sat_B + p*Sat_A\n\n#ICC and Residual ICC\nSat_A ~~ Sat_B\nCom_A ~~ Com_B\n\n# k parameter(s)\nk := p/a"
  )
})

test_that("scriptObsAPIM produces correct output for equate = actor k = TRUE", {

  expect_equal(scriptObsAPIM(X1 = "Sat_A", Y1 = "Com_A",
                             X2 = "Sat_B", Y2 = "Com_B",
                             equate = "actor", k = TRUE),

               "#Actor and Partner Effects\nCom_A ~ a*Sat_A + p1*Sat_B\nCom_B ~ a*Sat_B + p2*Sat_A\n\n#ICC and Residual ICC\nSat_A ~~ Sat_B\nCom_A ~~ Com_B\n\n# k parameter(s)\nk1 := p1/a\nk2 := p2/a"
  )
})

test_that("scriptObsAPIM produces correct output for equate = partner k = TRUE", {

  expect_equal(scriptObsAPIM(X1 = "Sat_A", Y1 = "Com_A",
                             X2 = "Sat_B", Y2 = "Com_B",
                             equate = "partner", k = TRUE),

               "#Actor and Partner Effects\nCom_A ~ a1*Sat_A + p*Sat_B\nCom_B ~ a2*Sat_B + p*Sat_A\n\n#ICC and Residual ICC\nSat_A ~~ Sat_B\nCom_A ~~ Com_B\n\n# k parameter(s)\nk1 := p/a1\nk2 := p/a2"
  )
})
