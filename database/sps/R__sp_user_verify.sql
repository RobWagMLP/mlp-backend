select sp_drop_function('sp_user_verify');

create or replace function sp_user_verify(
           verification_code     varchar,
           password              varchar
)
returns void
as  
$$
--Revision $Id: f08d6c87498272ebf22f2dc65f17bc921041a137 $ 
declare
  v_verification_code    alias for verification_code;
  v_password             alias for password;
  v_user_create_id       bigint            ;
  v_user_id              bigint            ;
begin

  select  e.user_create_id
    into  v_user_create_id
    from  email_verification_code e
    where e.verification_code = v_verification_code
     and  e.datetime_verificate is null;

 if v_user_create_id is null then
    perform sp_raise_exception(1000001);
 end if;

  update email_verification_code as e
    set datetime_verificate = statement_timestamp()
    where e.user_create_id = v_user_create_id;

  update user_create as u
    set  validated = true
    where u.user_create_id = v_user_create_id;

  insert 
   into user_valid(
     user_name     ,
     email_address ,
     valid_from    ,
     invalid_from  ,
     datetime_create,
     record_state
   )
  select p.user_name      ,
         p.email_address  ,
         clock_timestamp(),
         'infinity'       ,
         clock_timestamp(),
         'A'
   from  user_create p
   where p.user_create_id = v_user_create_id
   returning user_id into v_user_id;

   insert 
     into password(
       user_id    ,
       password   ,
       valid_from ,
       invalid_from
     ) values(
       v_user_id,
       md5(v_password),
       clock_timestamp(),
       'infinity'
     );
end;
$$
language plpgsql
SECURITY DEFINER
SET SEARCH_PATH FROM CURRENT;
