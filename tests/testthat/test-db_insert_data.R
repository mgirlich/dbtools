test_db_insert_data <- function(data,
                                expected_returned,
                                expected_state,
                                insert_cols = NULL,
                                returning = sql("*")) {
  expect_equal(
    db_insert_data(
      data,
      table = test_table,
      con = con,
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

prepare_table()

test_that("empty data work", {
  state_before <- get_tbl()

  test_db_insert_data(
    data = df[0, ],
    expected_returned = df[0, ],
    expected_state = state_before
  )
})

test_that("new data works", {
  prepare_table(df[0, ])

  test_db_insert_data(
    data = df,
    expected_returned = df,
    expected_state = df
  )
})

test_that("insert cols work", {
  prepare_table()
  state_before <- get_tbl()

  new_row <- create_new_row(state_before, value2 = NA_character_)
  state_new <- rbind(state_before, new_row)

  test_db_insert_data(
    data = new_row,
    expected_returned = new_row,
    expected_state = state_new,
    insert_cols = c("id1", "id2", "value1")
  )
})
