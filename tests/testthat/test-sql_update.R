f_update <- purrr::partial(
  sql_update,
  data = df,
  table = "dbtools_test",
  con = con
)

test_that("full update works", {
  up_tbl <- memdb_frame2(
    id1 = c(1, 1, 2, 2),
    id2 = c("a", "b", "c", "d"),
    value1 = c(11, NA, NA, NA),
    value2 = NA,
    .name = "sql_update_1"
  )

  update_sql <- sql_update(
    data = tibble::tibble(
      id1 = c(1, 2),
      id_2 = c("a", "c"),
      value1 = c(101, 103),
      value_2 = c(1, 3)
    ),
    table = "sql_update_1",
    con = src_memdb2(),
    update = c("value1", value2 = "value_2"),
    where = list("id1", sql("`target`.`id2` = `source`.`id_2`"))
  )

  DBI::dbExecute(src_memdb2(), update_sql)

  result <- DBI::dbReadTable(src_memdb2(), "sql_update_1")
  expect_equal(result$value1, c(101, NA, 103, NA))
  expect_equal(result$value2, c(1, NA, 3, NA))
})

test_that("sql_update works", {
  expect_snapshot(
    f_update(
      update = c("value1", "value2"),
      where = c("id1", "id_2")
    )
  )
})

test_that("`data` can be a SQL table", {
  expect_snapshot(
    sql_update(
      data = "my_tbl",
      table = "dbtools_test",
      con = con,
      update = c("value1", "value2"),
      where = c("id1", "id2")
    )
  )
})

test_that("`data` can be a zero row df", {
  expect_snapshot(
    sql_update(
      data = mtcars[0, ],
      table = "dbtools_test",
      con = con,
      update = c("value1", "value2"),
      where = c("id1", "id2")
    )
  )
})

test_that("sql_update can handle lists in `update`", {
  expect_snapshot(
    f_update(
      update = list("value",
                    target_id = "source_id",
                    value1 = sql("target.value1 + 1")),
      where = c("id")
    )
  )
})

test_that("sql_update can handle lists in `where`", {
  expect_snapshot(
    f_update(
      update = c("value"),
      where = list("value",
                   target_id = "source_id",
                   sql("target.value1 + 1"))
    )
  )
})

test_that("sql_update can use returning", {
  expect_snapshot(
    f_update(
      update = c("value1"),
      where = c("id1"),
      returning = c("id1", "id2")
    )
  )

  expect_snapshot(
    f_update(
      update = c("value1", "value2"),
      where = c("id1", "target_col" = "data_col"),
      returning = sql(time = "now()", "id2")
    )
  )
})

test_that("sql_update checks `where`", {
  skip("not yet implemented")
  f_update(
    update = c("value1", "value2"),
    where = sql(target_col = "data_col")
  )
})

test_that("sql_update checks `update`", {
  skip("not yet implemented")
  f_update(
    update = sql("value1"),
    where = c("id1")
  )
})
