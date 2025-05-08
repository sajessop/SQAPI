# Test for basic functionality
test_that("query_filter creates correct filter with value", {
  result <- query_filter("id", "eq", "5432")
  expected <- list(name = "id", op = "eq", val = "5432")

  expect_equal(result, expected)
})

# Test for nested query_filter
test_that("query_filter handles nested filters", {
  nested_filter <- query_filter(
    name = "media",
    op = "any",
    val = query_filter(
      name = "deployment",
      op = "has",
      val = query_filter(
        name = "campaign",
        op = "has",
        val = query_filter(name = "key", op = "eq", val = "Batemans201011")
      )
    )
  )

  expected_nested <- list(
    name = "media",
    op = "any",
    val = list(
      name = "deployment",
      op = "has",
      val = list(
        name = "campaign",
        op = "has",
        val = list(
          name = "key",
          op = "eq",
          val = "Batemans201011"
        )
      )
    )
  )

  expect_equal(nested_filter, expected_nested)
})

# Test if the function returns NULL if no value is provided (optional edge case)
test_that("query_filter returns correct structure without val", {
  result <- query_filter("status", "eq")
  expected <- list(name = "status", op = "eq")

  expect_equal(result, expected)
})



