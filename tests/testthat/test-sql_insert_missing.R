test_that("`sql_insert_missing()` works in new mode", {
  memdb_frame2(
    !!!mtcars_df[1:3, ],
    .name = "sql_insert_missing_1"
  )

  DBI::dbExecute(
    con_memdb(),
    dbplyr::sql_table_index(
      con_memdb(),
      "sql_insert_missing_1",
      columns = "rowname",
      name = "my_index",
      unique = TRUE
    )
  )

  insert_missing_sql <- sql_insert_missing(
    data = mtcars_df[3:5, ],
    table = "sql_insert_missing_1",
    con = con_memdb(),
    conflict_target = c("rowname"),
    # returning = sql("*"),
    # return_all = TRUE,
    mode = "new"
  )

  expect_snapshot(insert_missing_sql)

  DBI::dbExecute(con_memdb(), insert_missing_sql)

  expect_equal(
    DBI::dbReadTable(con_memdb(), "sql_insert_missing_1"),
    mtcars_df[1:5, ]
  )
})

test_that("`sql_insert_missing()` works in old mode", {
  memdb_frame2(
    !!!mtcars_df[1:3, ],
    .name = "sql_insert_missing_2"
  )

  insert_missing_sql <- sql_insert_missing(
    data = mtcars_df[3:5, ],
    table = "sql_insert_missing_2",
    con = con_memdb(),
    conflict_target = c("rowname"),
    # returning = sql("*"),
    # return_all = TRUE,
    mode = "old"
  )

  expect_snapshot(insert_missing_sql)

  DBI::dbExecute(con_memdb(), insert_missing_sql)

  expect_equal(
    DBI::dbReadTable(con_memdb(), "sql_insert_missing_2"),
    mtcars_df[1:5, ]
  )
})


test_that("`sql_insert_missing()` works with `return_all`", {
  skip_if_not(has_pg())

  pg_frame2(
    !!!mtcars_df[1:3, ],
    .name = "sql_insert_missing_3"
  )

  DBI::dbExecute(
    con_pg(),
    dbplyr::sql_table_index(
      con_pg(),
      "sql_insert_missing_3",
      columns = "rowname",
      name = "my_index",
      unique = TRUE
    )
  )

  insert_missing_sql <- sql_insert_missing(
    data = mtcars_df[3:5, ],
    table = "sql_insert_missing_3",
    con = con_pg(),
    conflict_target = c("rowname"),
    returning = sql("*"),
    return_all = TRUE,
    mode = "new"
  )

  expect_snapshot(insert_missing_sql)

  result <- DBI::dbGetQuery(con_pg(), insert_missing_sql)
  expect_equal(
    result[order(result$rowname), ],
    mtcars_df[3:5, ],
    ignore_attr = "row.names"
  )
})
