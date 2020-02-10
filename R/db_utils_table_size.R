#' Get database table sizes
#'
#' @export
db_utils_table_size <- function(con) {
  tmp <- DBI::dbGetQuery(
    con,
    "SELECT
       t.tablename,
       c.reltuples AS num_rows,
       pg_total_relation_size(quote_ident(t.tablename)::text) / 1024 As size_total,
       pg_relation_size(quote_ident(t.tablename)::text) / 1024 as size_table
FROM pg_tables t
LEFT OUTER JOIN pg_class c ON t.tablename = c.relname
WHERE t.schemaname = 'public'
ORDER BY 3 Desc;"
  ) %>%
    tibble::as_tibble()

  if (is_installed("fs")) {
    tmp$size_total <- 1024 * fs::fs_bytes(tmp$size_total)
    tmp$size_table <- 1024 * fs::fs_bytes(tmp$size_table)
    tmp$size_external <- tmp$size_total - tmp$size_table
  }

  tmp[, c("tablename", "size_total", "size_table", "size_external", "num_rows")]
}

#' Get database index infos
#'
#' @export
db_utils_index_infos <- function(con) {
  tmp <- DBI::dbGetQuery(
    con,
    "SELECT
    t.schemaname,
    t.tablename,
    indexname,
    c.reltuples AS num_rows,
    pg_size_pretty(pg_relation_size(quote_ident(t.schemaname)::text || '.' || quote_ident(t.tablename)::text)) AS table_size,
    pg_size_pretty(pg_relation_size(quote_ident(t.schemaname)::text || '.' || quote_ident(indexrelname)::text)) AS index_size,
    CASE WHEN indisunique THEN 'Y'
        ELSE 'N'
    END AS UNIQUE,
    number_of_scans,
    tuples_read,
    tuples_fetched
FROM pg_tables t
LEFT OUTER JOIN pg_class c ON t.tablename = c.relname
LEFT OUTER JOIN (
    SELECT
        c.relname AS ctablename,
        ipg.relname AS indexname,
        x.indnatts AS number_of_columns,
        idx_scan AS number_of_scans,
        idx_tup_read AS tuples_read,
        idx_tup_fetch AS tuples_fetched,
        indexrelname,
        indisunique,
        schemaname
    FROM pg_index x
    JOIN pg_class c ON c.oid = x.indrelid
    JOIN pg_class ipg ON ipg.oid = x.indexrelid
    JOIN pg_stat_all_indexes psai ON x.indexrelid = psai.indexrelid
) AS foo ON t.tablename = foo.ctablename AND t.schemaname = foo.schemaname
WHERE t.schemaname NOT IN ('pg_catalog', 'information_schema', 'pg_temp_20')
ORDER BY 1,2;"
  ) %>%
    tibble::as_tibble()

  tmp$size <- fs::as_fs_bytes(tmp$size)
  tmp$external_size <- fs::as_fs_bytes(tmp$external_size)
  tmp
}

#' Get running queries
#'
#' @export
db_utils_running_queries <- function(con) {
  DBI::dbGetQuery(
    con,
    "SELECT
      pid,
      age(clock_timestamp(), query_start),
      query,
      usename,
      application_name,
      query_start,
      wait_event_type,
      wait_event,
      state,
      client_addr, client_hostname, client_port
    FROM pg_stat_activity
    WHERE query != '<IDLE>' AND query NOT ILIKE '%pg_stat_activity%'
    ORDER BY query_start;"
  ) %>%
    tibble::as_tibble()
}
