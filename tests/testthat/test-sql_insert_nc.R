f_insert <- purrr::partial(
  sql_insert_nc,
  data = df,
  table = "dbtools_test",
  con = con_memdb()
)

test_that("insert works", {
  memdb_frame2(
    !!!mtcars_df[1:2, ],
    .name = "sql_insert_1"
  )

  insert_cols <- c("rowname", "mpg", "cyl")
  insert_sql <- sql_insert_nc(
    data = mtcars_df[3:5, ],
    table = "sql_insert_1",
    con = con_memdb(),
    insert_cols = insert_cols
  )

  DBI::dbExecute(con_memdb(), insert_sql)

  result <- DBI::dbReadTable(con_memdb(), "sql_insert_1")
  mtcars_df_sub <- mtcars_df[3:5, ]
  mtcars_df_sub[, 4] <- NA
  expect_equal(
    result,
    rbind(mtcars_df[1:2, ], mtcars_df_sub)
  )
})

test_that("sql_insert_nc works", {
  expect_snapshot(
    f_insert(
      insert_cols = NULL,
      returning = NULL,
      return_all = FALSE
    )
  )
})

test_that("`insert_cols` works", {
  expect_snapshot(
    f_insert(
      insert_cols = c("id1", "value1"),
      returning = NULL,
      return_all = FALSE
    )
  )
})

test_that("`returning` works", {
  expect_snapshot(
    f_insert(
      insert_cols = NULL,
      returning = sql("*"),
      return_all = FALSE
    )
  )
})

test_that("do nothing on conflict works", {
  expect_snapshot(
    f_insert(
      conflict = sql_do_nothing(sql_unique_cols("id1", "id2")),
      insert_cols = NULL,
      returning = sql("*"),
      return_all = FALSE
    )
  )
})

test_that("do update on conflict works", {
  skip("not supported by SQLite")
  expect_snapshot(
    f_insert(
      conflict = sql_do_update(sql_unique_cols("id1", "id2"), c("value1")),
      insert_cols = NULL,
      returning = sql("*"),
      return_all = FALSE
    )
  )
})
