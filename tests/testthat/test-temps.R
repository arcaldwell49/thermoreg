test_that("temp init works", {
  t1 = temp_init(20)

  expect_equal(t1$est,20)
  expect_equal(class(t1),"thermoreg_meas")
  expect_equal(t1$meas, "Temperature")
  expect_equal(t1$units, "celsius")
  expect_equal(t1$type, "tdb")

  t1 = temp_init(20,
                 units = "c")

  expect_equal(t1$est,20)
  expect_equal(class(t1),"thermoreg_meas")
  expect_equal(t1$meas, "Temperature")
  expect_equal(t1$units, "celsius")
  expect_equal(t1$type, "tdb")


  t3 = temp_init(76,
                 type = "wbgt",
                 units = "f")

  expect_equal(t3$est,76)
  expect_equal(class(t3),"thermoreg_meas")
  expect_equal(t3$meas, "Temperature")
  expect_equal(t3$units, "fahrenheit")
  expect_equal(t3$type, "wbgt")

  t2 = temp_init(1,
                 type = "twb",
                 units = "k")

  expect_equal(t2$est,1)
  expect_equal(class(t2),"thermoreg_meas")
  expect_equal(t2$meas, "Temperature")
  expect_equal(t2$units, "kelvin")
  expect_equal(t2$type, "twb")

  expect_error(temp_init(1,
                        type = "twb",
                        units = "kk"))

  expect_error(temp_init(1,
                        type = "ww",
                        units = "k"))
  print(t1)
})

test_that("convert temps works #1", {
  t1 = temp_init(20)
  t2 = temp_init(1,
                 type = "twb",
                 units = "k")
  t3 = temp_init(76,
                 type = "wbgt",
                 units = "f")

  tc1_1 = temp_convert(20,
               to = "f",
               from = "c")

  tc1_2 = temp_convert(t1$est,
                     to = "fahrenheit",
                     from = "celsius")

  expect_equal(tc1_1, tc1_2)

  tc2_1 = temp_convert(1,
                       to = "c",
                       from = "k")

  tc2_2 = temp_convert(t2$est,
                       to = "celsius",
                       from = "kelvin")

  expect_equal(tc2_1, tc2_2)

  tc3_1 = temp_convert(76,
                       to = "c",
                       from = "f")

  tc3_2 = temp_convert(t3$est,
                       to = "celsius",
                       from = "f")

  expect_equal(tc3_1, tc3_2)


})

test_that("convert temps works #2", {
  t1 = temp_convert(30,
               to = "c",
               from = "f")

  expect_equal(30,
               temp_convert(t1$est, to = "f",
                            from = "c")$est)

  t2 = temp_convert(t1$est,
                    to = "k",
                    from = "c")

  expect_equal(30,
               temp_convert(t2, to = "f",
                            from = "k")$est)

  expect_error(temp_convert(23,
                            to = "dsfdf"))

  expect_error(temp_convert(23,
                            from = "dsfdf"))
}
)
