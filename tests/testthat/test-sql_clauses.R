test_that("sql_clause_from errors on invalid input", {
  expect_error(sql_clause_from(c("a", "b"), con, table = "source"))
})

test_that("sql_clause_from errors on invalid input", {
  expect_error(
    sql_clause_update(
      update = list(SQL("now()")),
      table_name = "source",
      con = con
    ),
    class = "dbtools_error_invalid_input"
  )

  expect_error(
    sql_clause_update(
      update = list(list()),
      table_name = "source",
      con = con
    ),
    class = "dbtools_error_invalid_input"
  )

  expect_error(
    sql_clause_select(
      x = list(list()),
      con = con,
      table = "source"
    ),
    class = "dbtools_error_invalid_input"
  )
})

test_that("multiplication works", {
  expect_error(sql_clause_from(c("a", "b"), con, table = "source"))
})

