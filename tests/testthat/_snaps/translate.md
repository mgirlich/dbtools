# `translate_conflict()` works

    Code
      translate_conflict(con, sql_do_nothing(unique_cols))
    Output
      <SQL> ON CONFLICT ("id1", "id2")
      DO NOTHING

---

    Code
      translate_conflict(con, sql_do_nothing(unique_constraint))
    Output
      <SQL> ON CONSTRAINT "unique_constraint"
      DO NOTHING

---

    Code
      translate_conflict(con, sql_do_update(unique_cols, update))
    Output
      <SQL> ON CONFLICT ("id1", "id2")
      DO UPDATE SET `target col` = EXCLUDED.`source col`

---

    Code
      translate_conflict(con, sql_do_update(unique_constraint, update))
    Output
      <SQL> ON CONSTRAINT "unique_constraint"
      DO UPDATE SET `target col` = EXCLUDED.`source col`

