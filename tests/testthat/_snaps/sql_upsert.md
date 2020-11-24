# `sql_upsert()` works in new mode

    Code
      upsert_sql
    Output
      <SQL> WITH `source` (`rowname`, `mpg`, `cyl`, `disp`) AS (
        VALUES
          ('Datsun 710', 22.8, 6, 108),
          ('Hornet 4 Drive', 21.4, 8, 258),
          ('Hornet Sportabout', 18.7, 10, 360)
      )
      INSERT INTO `sql_upsert_1` AS `target` (`rowname`, `mpg`, `cyl`)
      SELECT `rowname`, `mpg`, `cyl`
      FROM `source`
      WHERE (true)
      ON CONFLICT ("rowname")
      DO UPDATE SET
        `cyl` = `EXCLUDED`.`cyl`,
        `mpg` = -`EXCLUDED`.`mpg`

# `sql_upsert()` works in old mode

    Code
      upsert_sql
    Output
      <SQL> WITH "source" ("rowname", "mpg", "cyl", "disp") AS (
        VALUES
          ('Datsun 710', 22.8::float8, 6::float8, 108::float8),
          ('Hornet 4 Drive', 21.4::float8, 8::float8, 258::float8),
          ('Hornet Sportabout', 18.7::float8, 10::float8, 360::float8)
      ),
      "insert_action" AS (
        INSERT INTO "sql_upsert_2" AS "target" ("rowname", "mpg", "cyl")
        SELECT "rowname", "mpg", "cyl"
        FROM "source"
        WHERE NOT EXISTS (
          SELECT 1
            FROM "sql_upsert_2" AS "target"
           WHERE ("target"."rowname" = "source"."rowname")
        )
        RETURNING *
      )
      UPDATE "sql_upsert_2" AS "target"
      SET
        "cyl" = "source"."cyl",
        "mpg" = -"source"."mpg"
      FROM "source"
      WHERE ("target"."rowname" = "source"."rowname")

