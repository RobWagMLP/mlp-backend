create table email_templates(
    memo          varchar(10) unique,
    template      text              ,
    datetime_create timestamp with time zone,
    constraint email_templates_pk primary key(memo)       
);
