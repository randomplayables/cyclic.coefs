test_that("harmonic_g & power_g work", {
  res_harm <- harmonic_g(1:10)
  res_pow <- power_g(1:10, p = 2)
  expect_equal(length(res_harm), 10)
  expect_equal(length(res_pow), 10)
  expect_equal(res_harm[1], 1)
  expect_equal(res_pow[5], 25)
})

test_that("log_g & exp_decay_g work", {
  res_log <- log_g(1:10)
  res_exp <- exp_decay_g(1:10, rate = 1)
  expect_equal(length(res_log), 10)
  expect_equal(length(res_exp), 10)
  expect_true(all(res_exp > 0))
})

test_that("geometric sequences work", {
  res_geo <- geometric_g(1:10, r = 0.5)
  res_alt <- alt_geometric_g(1:10, r = 0.5)
  expect_equal(length(res_geo), 10)
  expect_equal(length(res_alt), 10)
  expect_true(all(res_geo > 0))
  expect_equal(sign(res_alt[1]), -1)
})

test_that("binomial_g & fibonacci_g work", {
  res_binom <- binomial_g(1:10, k = 2)
  res_fib <- fibonacci_g(1:10)
  expect_equal(length(res_binom), 10)
  expect_equal(length(res_fib), 10)
  expect_equal(res_fib[1:3], c(1, 1, 2))
})

test_that("g functions work with combine", {
  res_harm <- combine(.f = \(x) sin_cyc(), .g = \(x) harmonic_g())
  res_geo <- combine(.f = \(x) cos_cyc(), .g = \(x) geometric_g())
  expect_equal(length(res_harm), 4)
  expect_equal(length(res_geo), 4)
})

