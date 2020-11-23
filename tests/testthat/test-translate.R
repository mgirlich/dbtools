
# translate_update() ------------------------------------------------------

test_that("`translate_update()` handles unnamed characters", {
  expect_equal(
    translate_update(
      con,
      update = c("col 1", "col 2")
    ),
    sql(
      `col 1` = sql("`source`.`col 1`"),
      `col 2` = sql("`source`.`col 2`")
    )
  )
})

test_that("`translate_update()` handles (partially) named characters", {
  expect_equal(
    translate_update(
      con,
      update = c(`other name` = "col 1", "col 2")
    ),
    sql(
      `other name` = sql("`source`.`col 1`"),
      `col 2` = sql("`source`.`col 2`")
    )
  )
})

test_that("`translate_update()` handles sql", {
  update_var <- sql(`time` = "now()", `value col` = "`source`.`value col` + 1")

  expect_equal(
    translate_update(
      con,
      update = update_var
    ),
    update_var
  )
})

test_that("`translate_update()` handles lists", {
  expect_equal(
    translate_update(
      con,
      update = list(
        "unnamed_chr",
        `name of chr` = "named chr col",
        `name of sql` = sql("now()")
      )
    ),
    sql(
      unnamed_chr = "`source`.`unnamed_chr`",
      `name of chr` = "`source`.`named chr col`",
      `name of sql` = "now()"
    )
  )
})

test_that("`translate_update()` errors on unnamed sql", {
  expect_error(
    translate_update(con, update = sql("now()")),
    class = "dbtools_error_invalid_input"
  )
})


# translate_where() ------------------------------------------------------

test_that("`translate_where()` handles unnamed characters", {
  expect_equal(
    translate_where(
      con,
      where = c("col 1", "col 2")
    ),
    sql(
      "`target`.`col 1` = `source`.`col 1`",
      "`target`.`col 2` = `source`.`col 2`"
    )
  )
})

test_that("`translate_where()` handles (partially) named characters", {
  expect_equal(
    translate_where(
      con,
      where = c(`other name` = "col 1", "col 2")
    ),
    sql(
      "`target`.`other name` = `source`.`col 1`",
      "`target`.`col 2` = `source`.`col 2`"
    )
  )
})

test_that("`translate_where()` handles sql", {
  where_var <- sql("`target`.`id` > 10", "now() > '2020-01-01'")

  expect_equal(
    translate_where(
      con,
      where = where_var
    ),
    where_var
  )
})

test_that("`translate_where()` handles lists", {
  expect_equal(
    translate_where(
      con,
      where = list(
        "unnamed_chr",
        `name of chr` = "named chr col",
        sql("`target`.`id` > 10")
      )
    ),
    sql(
      "`target`.`unnamed_chr` = `source`.`unnamed_chr`",
      "`target`.`name of chr` = `source`.`named chr col`",
      "`target`.`id` > 10"
    )
  )
})

test_that("`translate_where()` errors on named sql", {
  expect_error(
    translate_where(con, where = sql(target_col = "source_col")),
    class = "dbtools_error_invalid_input"
  )
})


# translate_conflict ------------------------------------------------------

test_that("`translate_conflict()` works", {
  unique_cols <- sql_unique_cols("id1", "id2")
  unique_constraint <- sql_constraint("unique_constraint")

  expect_snapshot(translate_conflict(con, sql_do_nothing(unique_cols)))
  expect_snapshot(translate_conflict(con, sql_do_nothing(unique_constraint)))

  update <- c(`target col` = "source col")
  expect_snapshot(
    translate_conflict(
      con,
      sql_do_update(unique_cols, update)
    )
  )

  expect_snapshot(
    translate_conflict(
      con,
      sql_do_update(unique_constraint, update)
    )
  )
})
