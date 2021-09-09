create table email_verification_code(
    email_verification_code_id           bigserial   not null,
    user_create_id                       bigint      not null,
    verification_code                    varchar(10) not null,
    datetime_create                      timestamp with time zone,
    datetime_invalidate                  timestamp with time zone,
    datetime_verificate                  timestamp with time zone,
    constraint email_verification_code_fk foreign key (user_create_id) references user_create(user_create_id)                     
);

create table password(
    user_id                  bigint   not null,
    password                 varchar  not null,
    valid_from               timestamp with time zone,
    invalid_from             timestamp with time zone,
    constraint password_pk primary key (user_id, password),
    constraint password_fk foreign key (user_id) references user_valid(user_id)
);
