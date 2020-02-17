test_that("sql_unique_cols works", {
  expect_s3_class(
    sql_unique_cols("a", c("b", "c")),
    class = "dbtools_unique_cols"
  )

  expect_error(
    sql_unique_cols(),
    class = "dbtools_error_invalid_input"
  )

  expect_error(
    sql_unique_cols(1),
    class = "dbtools_error_invalid_input"
  )

  expect_error(
    sql_unique_cols("a", NA),
    class = "dbtools_error_invalid_input"
  )
})


test_that("sql_constraint works", {
  expect_s3_class(
    sql_constraint("a"),
    class = "dbtools_constraint"
  )

  expect_error(
    sql_constraint(1),
    class = "dbtools_error_invalid_input"
  )

  expect_error(
    sql_constraint(c("a", "b")),
    class = "dbtools_error_invalid_input"
  )

  expect_error(
    sql_constraint(NA),
    class = "dbtools_error_invalid_input"
  )
})
