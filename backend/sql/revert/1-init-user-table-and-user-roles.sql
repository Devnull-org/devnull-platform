-- Revert devnull:1-init-user-table-and-user-roles from pg

BEGIN;

DROP TYPE userRole;
DROP TABLE "user";

COMMIT;
