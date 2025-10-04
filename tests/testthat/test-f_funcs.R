test_that("sin_cyc & cos_cyc work", {
  res_sin <- sin_cyc(inputs =seq(0,2*pi,length.out=16)[-16])
  res_cos <- cos_cyc(inputs =seq(0,2*pi,length.out=16)[-16])
  expect_equal(length(res_sin), 15)
  expect_equal(length(res_cos), 15)
})

test_that("sin_cyc & cos_cyc work with combine", {

  res_sin <- combine(.f = \(x) sin_cyc(inputs =seq(0,2*pi,length.out=16)[-16]),
                 .g = \(x) unlist(lapply(1:32, \(x)exp(-x))))

  res_cos <- combine(.f = \(x) cos_cyc(inputs =seq(0,2*pi,length.out=16)[-16]),
                     .g = \(x) unlist(lapply(1:32, \(x)exp(x))))
  expect_equal(length(res_sin), 4)
  expect_equal(length(res_cos), 4)
})

test_that("triangle_cyc & sawtooth_cyc work", {
  res_tri <- triangle_cyc(seq(0, 1, length.out = 16)[-16])
  res_saw <- sawtooth_cyc(seq(0, 1, length.out = 16)[-16])
  expect_equal(length(res_tri), 15)
  expect_equal(length(res_saw), 15)
})

test_that("pulse_cyc & square_cyc work", {
  res_pulse <- pulse_cyc(seq(0, 1, length.out = 16)[-16])
  res_square <- square_cyc(seq(0, 2*pi, length.out = 16)[-16])
  expect_equal(length(res_pulse), 15)
  expect_equal(length(res_square), 15)
})

test_that("complex_cyc works", {
  res_complex <- complex_cyc(seq(0, 2*pi, length.out = 16)[-16])
  expect_equal(length(res_complex), 15)
  expect_true(is.complex(res_complex))
})

test_that("geometric waves work with combine", {
  res_tri <- combine(.f = \(x) triangle_cyc(seq(0, 1, length.out = 16)[-16]),
                     .g = \(x) 1:32)
  res_pulse <- combine(.f = \(x) pulse_cyc(seq(0, 1, length.out = 16)[-16]),
                       .g = \(x) 1:32)
  expect_equal(length(res_tri), 4)
  expect_equal(length(res_pulse), 4)
})
