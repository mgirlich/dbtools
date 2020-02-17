test_db_insert_missing_data <- function(data,
                                        expected_returned,
                                        expected_state,
                                        conflict_target = sql_unique_cols("id1", "id2"),
                                        insert_cols = NULL,
                                        returning = SQL("*"),
                                        return_all = FALSE,
                                        ignore_order = FALSE,
                                        mode = c("new", "old")) {
  for (m in mode) {
    test_db_f(
      f = db_insert_missing_data,
      data = data,
      expected_returned = expected_returned,
      expected_state = expected_state,
      conflict_target = conflict_target,
      insert_cols = insert_cols,
      returning = returning,
      return_all = return_all,
      ignore_order = ignore_order,
      mode = m
    )

    prepare_table()
  }
}

prepare_table()

test_that("empty data work", {
  state_before <- get_tbl()

  test_db_insert_missing_data(
    data = df[0, ],
    expected_returned = df[0, ],
    expected_state = state_before
  )
})

test_that("all conflicted work", {
  state_before <- get_tbl()
  state_new <- state_before
  state_new$value1 <- state_new$value1 + 10

  # all conflicted --> should return empty df
  # state should not have changed
  test_db_insert_missing_data(
    data = state_new,
    expected_returned = df[0, ],
    expected_state = state_before
  )
})

test_that("partially new data work", {
  state_before <- get_tbl()
  new_row <- create_new_row(state_before)
  state_new <- rbind(state_before, new_row)

  test_db_insert_missing_data(
    data = state_new,
    expected_returned = new_row,
    expected_state = state_new
  )
})

test_that("insert cols work", {
  state_before <- get_tbl()

  new_row <- create_new_row(state_before, value2 = NA_character_)
  state_new <- rbind(state_before, new_row)

  test_db_insert_missing_data(
    data = state_new,
    expected_returned = new_row,
    expected_state = state_new,
    insert_cols = c("id1", "id2", "value1")
  )
})

test_that("constraint works", {
  skip_if(is_sqlite(con))
  state_before <- get_tbl()

  new_row <- create_new_row(state_before)
  state_new <- rbind(state_before, new_row)

  test_db_insert_missing_data(
    data = state_new,
    expected_returned = new_row,
    expected_state = state_new,
    conflict_target = sql_constraint(index_name),
    mode = "new"
  )
})

test_that("return_all works", {
  skip_if(is_sqlite(con))
  state_before <- get_tbl()

  new_row <- create_new_row(state_before)
  state_new <- rbind(state_before[1:3, ], new_row)

  cols <- c("id1", "id2")
  test_db_insert_missing_data(
    data = state_new,
    expected_returned = state_new[, c("id1", "id2")],
    expected_state = rbind(state_before, new_row),
    returning = cols,
    return_all = TRUE,
    ignore_order = TRUE
  )
})

test_that("return_all errors for invalid input", {
  f <- function(conflict_target = sql_unique_cols("id1", "id2"),
                returning = SQL("*")) {
    db_insert_missing_data(
      data = df,
      table = test_table,
      con = con,
      conflict_target = conflict_target,
      insert_cols = NULL,
      returning = returning,
      return_all = TRUE
    )
  }

  expect_error(
    f(conflict_target = NULL),
    class = "dbtools_error_invalid_input"
  )

  expect_error(
    f(returning = NULL),
    class = "dbtools_error_invalid_input"
  )
})
