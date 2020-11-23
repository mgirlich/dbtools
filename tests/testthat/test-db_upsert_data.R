test_db_upsert_data <- function(data,
                                update,
                                expected_returned,
                                expected_state,
                                insert_cols = NULL,
                                returning = sql("*"),
                                mode = c("new", "old")) {
  for (m in mode) {
    test_db_f(
      db_upsert_data,
      data = data,
      expected_returned = expected_returned,
      expected_state = expected_state,
      update = update,
      conflict_target = sql_unique_cols("id1", "id2"),
      insert_cols = insert_cols,
      returning = returning,
      mode = m
    )

    prepare_table()
  }
}

test_that("errors for invalid conflict target", {
  f_invalid <- function(conflict_target) {
    db_upsert_data(
      data = df,
      table = test_table,
      con = con,
      conflict_target = conflict_target,
      update = "value1"
    )
  }

  expect_error(f_invalid(NULL), class = "dbtools_error_invalid_input")
})

test_that("update works", {
  skip_if(is_sqlite(con))
  prepare_table(df[1:2, ])
  state_new <- df
  state_new$value1[2] <- state_new$value1[2] + 5
  state_new$value2[2] <- paste0(state_new$value2[2], "_NEW")

  test_db_upsert_data(
    data = state_new[-1, ],
    update = c("value1", "value2"),
    expected_returned = state_new[-1, ],
    expected_state = state_new
  )

  prepare_table(df[1:2, ])
  test_db_upsert_data(
    data = state_new[-1, ],
    update = c("value1", "value2"),
    expected_returned = 3,
    expected_state = state_new,
    returning = NULL
  )
})
