test_db_upsert_data <- function(data,
                                updates,
                                expected_returned,
                                expected_state,
                                insert_cols = NULL,
                                returning = sql("*")) {
  expect_equivalent(
    db_upsert_data(
      data = data,
      table = test_table,
      con = con,
      updates = updates,
      conflict_target = sql_conflict_cols("id1", "id2"),
      insert_cols = insert_cols,
      returning = returning
    ),
    expected_returned
  )

  expect_equivalent(
    get_tbl(),
    expected_state
  )
}

test_that("errors for invalid conflict target", {
  f_invalid <- function(conflict_target) {
    db_upsert_data(
      data = df,
      table = test_table,
      con = con,
      conflict_target = conflict_target,
      updates = "value1"
    )
  }

  expect_error(f_invalid(NULL), class = "dbtools_error_invalid_input")
  expect_error(f_invalid("id1"), class = "dbtools_error_invalid_input")
})

test_that("update works", {
  prepare_table(df[1:2, ])
  state_new <- df
  state_new$value1[2] <- state_new$value1[2] + 5
  state_new$value2[2] <- paste0(state_new$value2[2], "_NEW")

  test_db_upsert_data(
    data = state_new[-1, ],
    updates = c("value1", "value2"),
    expected_returned = state_new[-1, ],
    expected_state = state_new
  )
})

