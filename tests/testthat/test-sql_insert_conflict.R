test_that("`sql_insert_missing()` works in new mode", {
  mtcars_df <- tibble::rownames_to_column(mtcars)[, 1:4]

  memdb_frame2(
    !!!mtcars_df[1:3, ],
    .name = "sql_insert_missing_1"
  )

  DBI::dbExecute(
    src_memdb2(),
    dbplyr::sql_table_index(
      src_memdb2(),
      "sql_insert_missing_1",
      columns = "rowname",
      name = "my_index",
      unique = TRUE
    )
  )

  insert_missing_sql <- sql_insert_missing(
    data = mtcars_df[3:5, ],
    table = "sql_insert_missing_1",
    con = src_memdb2(),
    conflict_target = c("rowname"),
    # returning = SQL("*"),
    return_all = TRUE,
    mode = "new"
  )

  expect_snapshot(insert_missing_sql)

  DBI::dbExecute(src_memdb2(), insert_missing_sql)

  expect_equal(
    DBI::dbReadTable(src_memdb2(), "sql_insert_missing_1"),
    mtcars_df[1:5, ]
  )
})

test_that("`sql_insert_missing()` works in old mode", {
  mtcars_df <- tibble::rownames_to_column(mtcars)[, 1:4]

  memdb_frame2(
    !!!mtcars_df[1:3, ],
    .name = "sql_insert_missing_2"
  )

  insert_missing_sql <- sql_insert_missing(
    data = mtcars_df[3:5, ],
    table = "sql_insert_missing_2",
    con = src_memdb2(),
    conflict_target = c("rowname"),
    # returning = SQL("*"),
    return_all = TRUE,
    mode = "old"
  )

  expect_snapshot(insert_missing_sql)

  DBI::dbExecute(src_memdb2(), insert_missing_sql)

  expect_equal(
    DBI::dbReadTable(src_memdb2(), "sql_insert_missing_2"),
    mtcars_df[1:5, ]
  )
})
