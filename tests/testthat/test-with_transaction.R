test_that("nested transactions work", {
  expect_equal(
    with_transaction(
      con_memdb(),
      {
        with_transaction(
          con_memdb(),
          {
            memdb_frame2(a = 1, .name = "dummy")
          }
        )
        DBI::dbReadTable(con_memdb(), "dummy")
      }
    ),
    data.frame(a = 1)
  )
})

test_that("reset on error works", {
  expect_error(
    with_transaction(
      con_memdb(),
      abort("dummy error")
    )
  )

  with_transaction(
    con_memdb(),
    {
      with_transaction(
        con_memdb(),
        memdb_frame2(a = 1, .name = "dummy")
      )
      DBI::dbReadTable(con_memdb(), "dummy")
    }
  )

  expect_equal(
    with_transaction(con_memdb(), "a"),
    "a"
  )
})
