test_that("base_url joins host and endpoint correctly", {
  expect_equal(
    base_url("https://squidle.org", "api/data"),
    "https://squidle.org/api/data"
  )
})
