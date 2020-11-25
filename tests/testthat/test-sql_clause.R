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

test_that("`sql_values()` escapes different datatypes correctly", {
  skip("not yet done")
  data <- tibble::tibble(
    blob = blob::as_blob(as.raw(c(0x01, 0x02, 0x03))),
    character = c("a", "b", "c"),
    date = Sys.Date(),
    double = 1:3 + 0.0,
    factor = factor(c("f1", "f2", "f3")),
    ident = ident("i1", "i2", "i3"),
    integer = 1L:3L,
    integer64 = bit64::as.integer64("123456789123456789"),
    list = list("l1", 2, Sys.time()),
    logical = c(TRUE, FALSE, NA),
    null = list(NULL),
    posixt = c(Sys.time(), Sys.time() + 10, Sys.time() + 20),
    sql = sql("s1", "s2", "s3")
  )

  expect_snapshot(sql_values(con_memdb(), data))

  # TODO is this really correct like this?
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
