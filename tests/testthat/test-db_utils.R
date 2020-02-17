prep_table <- function(x, value) {
  if (DBI::dbExistsTable(con, x)) {
    DBI::dbRemoveTable(con, x)
  }
  DBI::dbCreateTable(con, x, fields = value)
  DBI::dbAppendTable(con, x, value = value)
}

test_table2 <- paste0(test_table, "2")
prep_table(test_table, mtcars)
iris$Species <- as.character(iris$Species)
prep_table(test_table2, iris)

test_that("db_utils_table_size", {
  skip_if_not(is_postgres(con))

  expect_known_value(
    db_utils_table_size(con),
    ref_file("db_utils_table_size.rds")
  )
})

test_that("db_utils_index_infos", {
  skip_if_not(is_postgres(con))

  index_infos <- db_utils_index_infos(con)
  expect_known_value(
    index_infos,
    ref_file("db_utils_index_infos.rds")
  )

  expect_equal(
    db_utils_index_infos(con, test_table),
    index_infos[index_infos$tablename == test_table, ]
  )
})

test_that("db_utils_running_queries", {
  skip_if_not(is_postgres(con))

  expect_known_value(
    db_utils_running_queries(con)[0, ],
    ref_file("db_utils_running_queries.rds")
  )
})
