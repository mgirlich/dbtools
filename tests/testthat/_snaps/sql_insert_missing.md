# `sql_insert_missing()` works in new mode

    Code
      insert_missing_sql
    Output
      <SQL> WITH `source` (`rowname`, `mpg`, `cyl`, `disp`) AS (
        VALUES
          ('Datsun 710', 22.8, 4, 108),
          ('Hornet 4 Drive', 21.4, 6, 258),
          ('Hornet Sportabout', 18.7, 8, 360)
      )
      INSERT INTO `sql_insert_missing_1` AS `target` (`rowname`, `mpg`, `cyl`, `disp`)
      SELECT `rowname`, `mpg`, `cyl`, `disp`
      FROM `source`
      WHERE (true)
      ON CONFLICT ("rowname")
      DO NOTHING

# `sql_insert_missing()` works in old mode

    Code
      insert_missing_sql
    Output
      <SQL> WITH `source` (`rowname`, `mpg`, `cyl`, `disp`) AS (
        VALUES
          ('Datsun 710', 22.8, 4, 108),
          ('Hornet 4 Drive', 21.4, 6, 258),
          ('Hornet Sportabout', 18.7, 8, 360)
      )
      INSERT INTO `sql_insert_missing_2` AS `target` (`rowname`, `mpg`, `cyl`, `disp`)
      SELECT `rowname`, `mpg`, `cyl`, `disp`
      FROM `source`
      WHERE NOT EXISTS (
        SELECT 1
          FROM `sql_insert_missing_2` AS `target`
         WHERE (`target`.`rowname` = `source`.`rowname`)
      )

# `sql_insert_missing()` works with `return_all`

    Code
      insert_missing_sql
    Output
      <SQL> WITH "source" ("rowname", "mpg", "cyl", "disp") AS (
        VALUES
          ('Datsun 710', 22.8::float8, 4::float8, 108::float8),
          ('Hornet 4 Drive', 21.4::float8, 6::float8, 258::float8),
          ('Hornet Sportabout', 18.7::float8, 8::float8, 360::float8)
      )
      ,"ins_result" AS (
        INSERT INTO "sql_insert_missing_3" AS "target" ("rowname", "mpg", "cyl", "disp")
        SELECT "rowname", "mpg", "cyl", "disp"
        FROM "source"
        WHERE (true)
        ON CONFLICT ("rowname")
        DO NOTHING
        RETURNING *
      )
      SELECT *
      FROM "ins_result"
      UNION ALL
      SELECT *
      FROM "sql_insert_missing_3" AS "target"
      WHERE EXISTS (
        SELECT 1
          FROM "source"
         WHERE ("target"."rowname" = "source"."rowname")
      )

