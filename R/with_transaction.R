#' Transaction helper
#'
#' Unlike [`DBI::dbWithTransaction()`] this helper allows transactions to be
#' nested as long as all transactions are created with `with_transaction()`.
#'
#' @inheritParams default-args
#' @param code An arbitrary block of R code.
#' @param ... Passed to `...` in [`DBI::dbWithTransaction()`].
#'
#' @export
#'
#' @examples
#' con <- con_memdb()
#' with_transaction(
#'   con,
#'   {
#'     with_transaction(
#'       con,
#'       {
#'         memdb_frame2(a = 1, b = 2, .name = "dummy")
#'       }
#'     )
#'     DBI::dbReadTable(con_memdb(), "dummy")
#'   }
#' )
with_transaction <- function(con, code, ...) {
  cache <- cache()
  label <- paste0("trans_", rlang::env_label(con@ref))

  if (env_get(cache, label, FALSE)) {
    on.exit(env_poke(cache, label, FALSE))
    env_poke(cache, label, TRUE)

    DBI::dbWithTransaction(
      conn = con,
      code,
      ...
    )
  } else {
    code
  }
}
