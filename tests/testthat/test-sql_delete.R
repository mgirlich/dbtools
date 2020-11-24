test_that("sql_delete works", {
  memdb_frame2(
    !!!mtcars_df[1:5, ],
    .name = "sql_delete_1"
  )

  delete_sql <- sql_delete(
    data = mtcars_df[3:5, ],
    table = "sql_delete_1",
    con = con,
    where = list("mpg", sql("`source`.`cyl` > 4")),
    returning = NULL
  )

  expect_snapshot(delete_sql)
  DBI::dbExecute(con_memdb(), delete_sql)

  expect_equal(
    DBI::dbReadTable(con_memdb(), "sql_delete_1"),
    mtcars_df[1:3, ]
  )
})
