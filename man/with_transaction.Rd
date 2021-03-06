% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/with_transaction.R
\name{with_transaction}
\alias{with_transaction}
\title{Transaction helper}
\usage{
with_transaction(con, code, ...)
}
\arguments{
\item{con}{A DBIConnection object, as returned by \code{\link[DBI:dbConnect]{DBI::dbConnect()}}.}

\item{code}{An arbitrary block of R code.}

\item{...}{Passed to \code{...} in \code{\link[DBI:dbWithTransaction]{DBI::dbWithTransaction()}}.}
}
\description{
Unlike \code{\link[DBI:dbWithTransaction]{DBI::dbWithTransaction()}} this helper allows transactions to be
nested as long as all transactions are created with \code{with_transaction()}.
}
\examples{
con <- con_memdb()
with_transaction(
  con,
  {
    with_transaction(
      con,
      {
        memdb_frame2(a = 1, b = 2, .name = "dummy")
      }
    )
    DBI::dbReadTable(con_memdb(), "dummy")
  }
)
}
