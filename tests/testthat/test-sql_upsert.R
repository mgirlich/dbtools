test_that("`sql_upsert()` works in new mode", {
  memdb_frame2(
    !!!mtcars_df[1:3, ],
    .name = "sql_upsert_1"
  )

  DBI::dbExecute(
    con_memdb(),
    dbplyr::sql_table_index(
      con_memdb(),
      "sql_upsert_1",
      columns = "rowname",
      name = "my_index_upsert",
      unique = TRUE
    )
  )

  data_new <- mtcars_df[3:5, ]
  data_new$cyl <- data_new$cyl + 2

  upsert_sql <- sql_upsert(
    data = data_new,
    table = "sql_upsert_1",
    con = con_memdb(),
    conflict_target = c("rowname"),
    update = list("cyl", mpg = sql("-`EXCLUDED`.`mpg`")),
    insert_cols = c("rowname", "mpg", "cyl"),
    # returning = sql("*"),
    mode = "new"
  )

  expect_snapshot(upsert_sql)

  DBI::dbExecute(con_memdb(), upsert_sql)

  out <- mtcars_df[1:5, ]
  out$mpg[3] <- -out$mpg[3]
  out$cyl[3:5] <- out$cyl[3:5] + 2
  out$disp[4:5] <- NA

  expect_equal(
    DBI::dbReadTable(con_memdb(), "sql_upsert_1"),
    out
  )
})

test_that("`sql_upsert()` works in old mode", {
  skip_if_not(has_pg())
  pg_frame2(
    !!!mtcars_df[1:3, ],
    .name = "sql_upsert_2"
  )

  data_new <- mtcars_df[3:5, ]
  data_new$cyl <- data_new$cyl + 2

  upsert_sql <- sql_upsert(
    data = data_new,
    table = "sql_upsert_2",
    con = con_pg(),
    conflict_target = c("rowname"),
    update = list("cyl", mpg = sql('-"source"."mpg"')),
    insert_cols = c("rowname", "mpg", "cyl"),
    returning = sql("*"),
    mode = "old"
  )

  expect_snapshot(upsert_sql)

  DBI::dbExecute(con_pg(), upsert_sql)

  out <- mtcars_df[1:5, ]
  out$mpg[3] <- -out$mpg[3]
  out$cyl[3:5] <- out$cyl[3:5] + 2
  out$disp[4:5] <- NA

  expect_equal(
    DBI::dbReadTable(con_pg(), "sql_upsert_2"),
    out
  )
})
