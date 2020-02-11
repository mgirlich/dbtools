test_db_insert_missing_data <- function(data,
                                        expected_returned,
                                        expected_state,
                                        conflict_target = sql_conflict_cols("id1", "id2"),
                                        insert_cols = NULL,
                                        returning = sql("*")) {
  expect_equal(
    db_insert_missing_data(
      data,
      table = test_table,
      con = con,
      conflict_target = conflict_target,
      insert_cols = insert_cols,
      returning = returning
    ),
    expected_returned
  )

  expect_equal(
    get_tbl(),
    expected_state
  )
}


create_new_row <- function(state_before, value2 = NULL) {
  new_row <- state_before[1, ]
  new_row$id1 <- max(state_before$id1) + 100
  new_row$value1 <- max(state_before$value1) + 100

  if (!is.null(value2)) {
    new_row$value2 <- value2
  }

  new_row
}


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

test_that("conflict_target works", {
  state_before <- get_tbl()

  new_row <- create_new_row(state_before)
  state_new <- rbind(state_before, new_row)

  test_db_insert_missing_data(
    data = state_new,
    expected_returned = new_row,
    expected_state = state_new,
    conflict_target = sql_constraint(index_name)
  )
})
