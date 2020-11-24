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

