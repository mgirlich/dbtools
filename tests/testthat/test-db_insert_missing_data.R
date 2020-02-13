test_db_insert_missing_data <- function(data,
                                        expected_returned,
                                        expected_state,
                                        conflict_target = sql_conflict_cols("id1", "id2"),
                                        insert_cols = NULL,
                                        returning = sql("*"),
                                        return_all = FALSE,
                                        ignore_order = FALSE) {
  ret <- db_insert_missing_data(
    data,
    table = test_table,
    con = con,
    conflict_target = conflict_target,
    insert_cols = insert_cols,
    returning = returning,
    return_all = return_all
  )

  if (ignore_order) {
    ret <- ret[do.call(order, ret), ]
    expected_returned <- expected_returned[do.call(order, expected_returned), ]
  }

  expect_equivalent(ret, expected_returned)

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

test_that("return_all works", {
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
  f <- function(conflict_target = sql_conflict_cols("id1", "id2"),
                returning = sql("*")) {
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
