test_sql_update_from <- function(from = "my_value_table",
                                 update = c("up1", "up2"),
                                 where = c("where1", "where2"),
                                 returning = NULL) {
  sql_update(
    table = "my_tbl",
    con = con,
    from = from,
    update = update,
    where = where,
    returning = returning
  )
}

ref_file <- function(x) {
  test_path(paste0("sql_update_from-", x, ".rds"))
}

test_that("update, where and returning work", {
  expect_known_value(
    test_sql_update_from(update = def_update),
    ref_file("update")
  )

  expect_known_value(
    test_sql_update_from(where = def_where),
    ref_file("where")
  )

  expect_known_value(
    test_sql_update_from(returning = def_returning),
    ref_file("returning")
  )
})


test_that("from with dataframe works", {
  expect_known_value(
    test_sql_update_from(from = df),
    ref_file("from_dataframe")
  )
})
