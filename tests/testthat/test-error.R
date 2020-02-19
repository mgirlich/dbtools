test_that("check_sql_chr_list works", {
  expect_error(check_sql_chr_list(NULL, "where"), class = "dbtools_error_invalid_input")
  expect_error(check_sql_chr_list(1, "where"), class = "dbtools_error_invalid_input")
  expect_error(check_sql_chr_list(list(1), "where"), class = "dbtools_error_invalid_input")
})

test_that("check_sql_chr_list works", {
  x <- SQL("a")
  expect_error(
    check_sql_names(list(x), TRUE, "where"),
    class = "dbtools_error_invalid_input"
  )
  expect_error(
    check_sql_names(list(a = x), FALSE, "where"),
    class = "dbtools_error_invalid_input"
  )
})
