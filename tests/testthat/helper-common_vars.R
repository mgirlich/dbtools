df <- iris[1:2, ]
con <- mr.amsutils::db_con_ams()

# setClass("PqConnection",
#   contains = "DBIConnection",
#   slots = list(ptr = "externalptr", bigint = "character", typnames = "data.frame")
# )
#
# sqlite_con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")
#
# new(
#   "PqConnection",
#   # ptr = structure(list(), "externalptr"),
#   ptr = structure(list(), "externalptr"),
#   bigint = "integer64",
#   typnames = data.frame()
# )
#
# new(
#   "PqConnection",
#   ptr = con@ptr,
#   bigint = "integer64",
#   typnames = data.frame()
# )


def_returning <- list(
  "col 1",
  col2 = "col B",
  col3 = SQL("now()")
)
def_update <- def_returning
def_where <- list(
  "col 1",
  col2 = "col B",
  SQL("now()")
)
