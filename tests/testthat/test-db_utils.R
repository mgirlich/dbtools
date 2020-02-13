prepare_table()

test_that("db_utils_table_size", {
  expect_known_value(
    db_utils_table_size(con),
    ref_file("db_utils_table_size.rds")
  )
})

test_that("db_utils_index_infos", {
  expect_known_value(
    db_utils_index_infos(con),
    ref_file("db_utils_index_infos.rds")
  )

  expect_equal(
    db_utils_index_infos(con, "dbtools_test")$tablename,
    "dbtools_test"
  )
})

test_that("db_utils_running_queries", {
  expect_known_value(
    db_utils_running_queries(con)[0, ],
    ref_file("db_utils_running_queries.rds")
  )
})
