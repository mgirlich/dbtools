test_that("multiplication works", {
  sql_upsert(
    table = "my_tbl",
    con = con,
    from = "my_value_table",
    insert_cols = c("col 1", "col 2", "col 3"),
    updates = list(
      "col 1",
      col2 = "col B",
      col3 = SQL("now()")
    ),
    conflict_target = sql_conflict_cols("conf 1", "conf 2"),
    returning = list(
      "ret 1",
      ret2 = "ret B",
      SQL("now()")
    )
  )
})
