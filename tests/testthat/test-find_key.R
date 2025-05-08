test_that("find_key returns the correct value for nested key", {
  nested_list <- list(
    a = 1,
    b = list(
      c = 2,
      d = list(
        e = 3,
        target = "found me"
      )
    )
  )

  expect_equal(find_key(nested_list, "target"), "found me")
})

test_that("find_key returns NULL if key not found", {
  test_list <- list(a = 1, b = list(c = 2))
  expect_null(find_key(test_list, "missing"))
})

test_that("find_key works with shallow lists", {
  test_list <- list(foo = "bar")
  expect_equal(find_key(test_list, "foo"), "bar")
})

