test_that("press init works", {
  p1 = press_init(22)
  expect_equal(p1$est,22)
  expect_equal(class(p1),"thermoreg_meas")
  expect_equal(p1$meas, "Pressure")
  expect_equal(p1$units, "torr")
  expect_equal(p1$type, "barometric")

  p2 = press_init(22,
                  type = "avp",
                  units = "mbar")
  expect_equal(p2$est,22)
  expect_equal(class(p2),"thermoreg_meas")
  expect_equal(p2$meas, "Pressure")
  expect_equal(p2$units, "mbar")
  expect_equal(p2$type, "avp")

  p3 = press_init(14,
                  type = "avp",
                  units = "psi")
  expect_equal(p3$est,14)
  expect_equal(class(p3),"thermoreg_meas")
  expect_equal(p3$meas, "Pressure")
  expect_equal(p3$units, "psi")
  expect_equal(p3$type, "avp")

  p4 = press_init(0,
                  type = "svp",
                  units = "atm")
  expect_equal(p4$est,0)
  expect_equal(class(p4),"thermoreg_meas")
  expect_equal(p4$meas, "Pressure")
  expect_equal(p4$units, "atm")
  expect_equal(p4$type, "svp")
})

test_that("convert press works #1", {
  p1 = press_init(20)
  p2 = press_init(1,
                 type = "avp",
                 units = "atm")
  p3 = press_init(76,
                 type = "svp",
                 units = "mbar")

  pc1_1 = press_convert(20,
                       to = "torr",
                       from = "psi")

  pc1_2 = press_convert(p1$est,
                       to = "torr",
                       from = "psi")

  expect_equal(pc1_1, pc1_2)

  pc2_1 = press_convert(1,
                       to = "atm",
                       from = "mbar")

  pc2_2 = press_convert(p2$est,
                       to = "atm",
                       from = "mbar")

  expect_equal(pc2_1, pc2_2)

  pc3_1 = press_convert(76,
                       to = "torr",
                       from = "mbar",
                       type = "svp")

  pc3_2 = press_convert(p3,
                       to = "torr")

  expect_equal(pc3_1, pc3_2)


})

test_that("convert press works #2", {
  expect_error(press_convert(12,
                             to = "adsadf"))
  expect_error(press_convert(12,
                             from = "adsadf"))

  test1 = press_convert(12,
                        to = "mmhg")
  test2 = press_convert(12)

  expect_equal(test1,test2)

  test1 = press_convert(12,
                        to = "torr",
                        from = "mmhg")
  test2 = press_convert(12,
                        from = "torr")

  expect_equal(test1,test2)

  expect_equal(test1$est, 12)
  expect_equal(test1$units, "torr")

  test1 = press_convert(1,
                        to = "psi",
                        from = "atm")
  test2 = press_convert(test1,
                        to = "atm")

  expect_equal(test1$est, 14.6959,
               tolerance = .0001)
  expect_equal(test2$est, 1)
})
