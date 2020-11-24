test_that("`sql_with_clauses()` works", {
  sql_clause_cte_table(con_memdb(), "table", sql_values(con, mtcars_df[1, ]))

  sql_stmts <- sql(
    "`table` AS (
  VALUES
    ('Mazda RX4', 21, 6, 160)
)",
    "select * from `table`"
  )

  expect_equal(sql_with_clauses(con_memdb(), sql_stmts[2]), sql_stmts[2])
  expect_equal(
    sql_with_clauses(con_memdb(), NULL, sql_stmts[[2]], NULL),
    sql_stmts[2]
  )

  expect_equal(
    sql_with_clauses(con_memdb(), sql_stmts[[1]], sql_stmts[[2]], NULL),
    paste_sql("WITH ", collapse_sql(sql_stmts, "\n"))
  )
})

test_that("`sql_clause_data()` handles dataframes", {
  expect_snapshot(sql_clause_data(con_memdb(), mtcars_df[1:2, ], "values"))
})

test_that("`sql_clause_data()` handles characters", {
  expect_null(sql_clause_data(con_memdb(), "source_tbl", "values"))
})

test_that("`sql_values()` works for empty dataframes and SQLite", {
  expect_snapshot(sql_values(con_memdb(), mtcars_df[0, ]))
})

test_that("`sql_values()` works for empty dataframes and PostgreSQL", {
  skip_if_not(has_pg())

  expect_snapshot(sql_values(con_pg(), mtcars_df[0, ]))
})

test_that("`sql_clause_cte_table()` can handle different input types", {
  select_clause <- "SELECT * FROM `source`"
  goal <- sql(
    paste0(
      "`table` AS (\n",
      "  SELECT * FROM `source`\n",
      ")"
    )
  )

  chr_cols <- sql_clause_cte_table(
    con_memdb(),
    "table",
    select_clause,
    columns = c("col 1", "col 2")
  )

  ident_cols <- sql_clause_cte_table(
    con,
    ident("table"),
    sql(select_clause),
    columns = ident("col 1", "col 2")
  )

  expect_snapshot(chr_cols)
  expect_equal(chr_cols, ident_cols)
})

test_that("`sql_clause_set()` works", {
  expect_snapshot(sql_clause_set(con_memdb(), sql(x = "a")))
})

test_that("`sql_clause_set()` checks input type", {
  expect_snapshot_error(sql_clause_set(con_memdb(), c(x = "a")))
  expect_snapshot_error(sql_clause_set(con_memdb(), list(x = sql("a"))))
})

test_that("`sql_clause_set()` checks for names", {
  expect_snapshot_error(
    sql_clause_set(con_memdb(), sql("a")),
    class = "dbtools_error_invalid_input"
  )

  expect_snapshot_error(
    sql_clause_set(con_memdb(), sql(x = "a", "b")),
    class = "dbtools_error_invalid_input"
  )
})

test_that("`sql_clause_returning()` works", {
  expect_snapshot(sql_clause_returning(con_memdb(), c(x = "a", "b")))
  expect_snapshot(sql_clause_returning(con_memdb(), ident(x = "a", "b")))
  expect_snapshot(sql_clause_returning(con_memdb(), sql(time = "now()", "b + 1")))
  expect_snapshot(sql_clause_returning(con_memdb(), list(time = sql("now()"), x = "a", "b")))
})
