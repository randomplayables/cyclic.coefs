test_that("sin_mod & cos_mod work", {
  res_sin <- sin_cyc(inputs =seq(0,2*pi,length.out=16)[-16])
  res_cos <- cos_cyc(inputs =seq(0,2*pi,length.out=16)[-16])
  expect_equal(length(res_sin), 15)
  expect_equal(length(res_cos), 15)
})

test_that("sin_mod & cos_mod work with combine", {

  res_sin <- combine(.f = \(x) sin_cyc(inputs =seq(0,2*pi,length.out=16)[-16]),
                 .g = \(x) unlist(lapply(1:32, \(x)exp(-x))))

  res_cos <- combine(.f = \(x) cos_cyc(inputs =seq(0,2*pi,length.out=16)[-16]),
                     .g = \(x) unlist(lapply(1:32, \(x)exp(x))))
  expect_equal(length(res_sin), 4)
  expect_equal(length(res_cos), 4)
})
