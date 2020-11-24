prep_table <- function(x, value) {
  if (DBI::dbExistsTable(con_pg(), x)) {
    DBI::dbRemoveTable(con_pg(), x)
  }
  DBI::dbCreateTable(con_pg(), x, fields = value)
  DBI::dbAppendTable(con_pg(), x, value = value)
}

test_table2 <- paste0(test_table, "2")
prep_table(test_table, mtcars)
iris$Species <- as.character(iris$Species)
prep_table(test_table2, iris)

test_that("db_utils_table_size", {
  skip_if_not(has_pg())
  skip("not yet decided whether to export")

  expect_snapshot_value(
    db_utils_table_size(con_pg()),
    style = "serialize"
  )
})

test_that("db_utils_index_infos", {
  skip_if_not(has_pg())
  skip("not yet decided whether to export")

  index_infos <- db_utils_index_infos(con_pg())
  expect_snapshot_value(index_infos, style = "serialize")

  expect_equal(
    db_utils_index_infos(con_pg(), test_table),
    index_infos[index_infos$tablename == test_table, ]
  )
})

test_that("db_utils_running_queries", {
  skip_if_not(has_pg())
  skip("not yet decided whether to export")

  expect_snapshot_value(
    db_utils_running_queries(con_pg())[0, ],
    style = "serialize"
  )
})
