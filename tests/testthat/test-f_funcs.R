test_that("sin_mod & cos_mod work", {
  res_sin <- sin_mod(inputs =seq(0,2*pi,length.out=16),
                     period = 2*pi)
  res_cos <- cos_mod(inputs =seq(0,2*pi,length.out=16),
                     period = 2*pi)
  expect_equal(length(res_sin), 16)
  expect_equal(length(res_cos), 16)
})
