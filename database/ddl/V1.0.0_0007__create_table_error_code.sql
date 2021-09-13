create table error_code(
  error_code      int                         not null,
  procedure       varchar(63)                 not null,
  error_text      varchar(255)                not null
);

create unique index error_code_pk  on error_code(error_code); 


COMMENT ON TABLE  error_code                  is 'database error codes'    ;
COMMENT ON COLUMN error_code.error_code   is 'unique error code'           ;
COMMENT ON COLUMN error_code.procedure    is 'procedure name'              ;
COMMENT ON COLUMN error_code.error_text   is 'error text'                  ;