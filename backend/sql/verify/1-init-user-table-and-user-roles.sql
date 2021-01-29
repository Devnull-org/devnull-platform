-- Verify devnull:1-init-user-table-and-user-roles on pg

BEGIN;


do $$
  declare
  result varchar;
begin
  result := (
    select "user"."username" from "user" where id = 1
  );
  assert result = 'v0d1ch', 'User v0d1ch not found';
end $$;

ROLLBACK;
