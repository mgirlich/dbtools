test_sql_insert_missing <- function(from = "my_value_table",
                                    insert_cols = c("Sepal.Length", "Species"),
                                    conflict_target = NULL,
                                    returning = NULL) {
  sql_insert_missing(
    table = "my_tbl",
    con = con,
    from = from,
    insert_cols = insert_cols,
    conflict_target = conflict_target,
    returning = returning
  )
}

ref_file <- function(x) {
  test_path(paste0("sql_insert_missing-", x, ".rds"))
}

test_that("sql_insert_missing works", {
  expect_known_value(
    test_sql_insert_missing(),
    ref_file("simple")
  )
})

test_that("conflict_target works", {
  expect_known_value(
    test_sql_insert_missing(
      conflict_target = sql_conflict_cols("conflict 1", "conflict 2")
    ),
    ref_file("conflict_cols")
  )

  expect_known_value(
    test_sql_insert_missing(conflict_target = sql_constraint("my_constraint")),
    ref_file("conflict_constraint")
  )
})

test_that("from dataframe works", {
  expect_known_value(
    test_sql_insert_missing(from = df),
    ref_file("from_dataframe")
  )

  expect_known_value(
    test_sql_insert_missing(from = df[0, ]),
    ref_file("from_empty_dataframe")
  )
})

test_that("returning works", {
  expect_known_value(
    test_sql_insert_missing(returning = def_returning),
    ref_file("returning")
  )
})
