create table user_create (
    user_create_id   bigserial    not null       ,
    user_name        varchar(100) not null unique,
    email_address    varchar(255) not null       ,
    validated        boolean      default false  ,
    datetime_create  timestamp with time zone    ,
    constraint user_create_pk1 primary key(user_create_id) );

