% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_upsert_data.R
\name{db_upsert_data}
\alias{db_upsert_data}
\title{Update a database table}
\usage{
db_upsert_data(
  data,
  table,
  con,
  update,
  conflict_target,
  insert_cols = NULL,
  returning = NULL,
  trans = TRUE,
  batch_size = 50000,
  mode = "new"
)
}
\arguments{
\item{data}{A data frame or the name of a database table.}

\item{table}{The name of the database table to update, insert to, or upsert
to. Can be an unquoted character string or an identifier created with \code{\link[=ident]{ident()}}.}

\item{con}{A DBIConnection object, as returned by \code{\link[DBI:dbConnect]{DBI::dbConnect()}}.}

\item{update}{specifies the columns of \code{table} to update and its new values.
This can be one of the following:
\itemize{
\item a character vector of column names, e.g. \code{c("a", "b")} to update the
columns \code{a} and \code{b} of \code{table} with the values of the columns \code{a} and \code{b} in
\code{data}.
To update with a different column, use a named vector. For example,
\code{update = c(x = "y")} will update column \code{x} of \code{table} with column \code{y} of \code{data}.
\item a named SQL vector (generated with \code{\link[=sql]{sql()}}. In the SQL code the
database table is named \code{target} and the input data is named \code{source}.
The name specifies the column to update. For example
\code{update = sql(update_counter = "target.update_counter + 1")}
will increase the column \code{update_counter} by one.
\item a list of scalar SQL and scalar character.
}}

\item{conflict_target}{Specifies the conflict target. This can be one of
the following:
\itemize{
\item a character vector that is passed on to \code{\link[=sql_unique_cols]{sql_unique_cols()}}.
\item a constraint created with \code{\link[=sql_constraint]{sql_constraint()}} or \code{\link[=sql_unique_cols]{sql_unique_cols()}}.
}}

\item{insert_cols}{Columns from \code{data} to insert.}

\item{returning}{Specifies the columns to return. If \code{NULL} (the default)
you have to use \code{\link[DBI:dbExecute]{DBI::dbExecute()}} to execute the SQL statement.

If not \code{NULL} you have to use \code{\link[DBI:dbGetQuery]{DBI::dbGetQuery()}} to get the updated rows.
\code{returning} can be one of the following:
\itemize{
\item a (named) character vector of column names.
\item a (named) SQL vector (generated with \code{\link[=sql]{sql()}}). Note that only the
columns from \code{table} are visible, not the ones from \code{source}.
\item a list of scalar SQL and scalar character.
}

Names are used as the names of the returned columns. For example
\code{returning = list("id", time = sql("now()"))}
to return the column \code{id} and the current time in the column \code{time}.}

\item{trans}{perform operation in a transaction?}

\item{batch_size}{number of rows to process in a single statement.}

\item{mode}{Specify how to check for a conflict:
\itemize{
\item "new": use the SQL "ON CONFLICT" clause.
\item "old": do not use the "ON CONFLICT" clause but an anti-join.
}}
}
\value{
If \code{returning} is \code{NULL} the number of rows updated; Otherwise a
tibble of the updated rows and the columns as specified in \code{returning}..
}
\description{
Update a database table
}
\section{Unnecessary Updates}{

Be careful when doing updates very frequently on a table. On an update
operation Postgres writes a new version of a row even when nothing has
changed (due to its \href{https://www.postgresql.org/docs/current/mvcc-intro.html}{MVCC model}).
This means that frequently updating a table without changing a value can
still bloat the table. For example this can easily happen together when
updating a timestamp column via a trigger.
}

