test_db_f <- function(f,
                      data,
                      expected_returned,
                      expected_state,
                      ...,
                      ignore_order = FALSE) {
  dots <- list(...)
  if (is_sqlite(con)) {
    if (is.data.frame(expected_returned)) {
      expected_returned <- nrow(expected_returned)
    }
    if ("returning" %in% names(dots)) {
      dots$returning <- NULL
    }
  }

  args <- list(
    data = data,
    table = test_table,
    con = con
  )

  ret <- do.call(f, c(args, dots))
  if (ignore_order) {
    ret <- ret[do.call(order, ret), ]
    expected_returned <- expected_returned[do.call(order, expected_returned), ]
  }

  expect_equivalent(
    ret,
    expected_returned
  )

  expect_equivalent(
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
