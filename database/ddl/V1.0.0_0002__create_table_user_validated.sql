create table user_valid(
    user_id          bigserial    not null       ,
    user_name        varchar(100) not null unique,
    email_address    varchar(255) not null       ,
    invalid_from     timestamp with time zone    ,
    valid_from       timestamp with time zone    ,
    datetime_create  timestamp with time zone    ,
    record_state     char(1)                     ,
    constraint user_pk1  primary key(user_id)    , 
    constraint user_rkst check(record_state in ('A', 'D')),
    EXCLUDE USING GIST (user_id     WITH = , tstzrange(valid_from,invalid_from) WITH &&) where (record_state = 'A') );

