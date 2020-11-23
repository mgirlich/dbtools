test_that("sql_delete works", {
  mtcars_df <- tibble::rownames_to_column(mtcars)[, 1:4]

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
  DBI::dbExecute(src_memdb2(), delete_sql)

  expect_equal(
    DBI::dbReadTable(src_memdb2(), "sql_delete_1"),
    mtcars_df[1:3, ]
  )
})
