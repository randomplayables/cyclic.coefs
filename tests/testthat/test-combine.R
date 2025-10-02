test_that("combine works", {
  res <- combine(.f = \(x) (1:4)/4,
                 .g = \(x) unlist(lapply(1:8, \(x)2^x)))
  expect_equal(length(res), 4)
  expect_equal(length(res$fg_terms), length(res$g_seq))
  expect_equal(res$fg_dot, 416.5)
})
