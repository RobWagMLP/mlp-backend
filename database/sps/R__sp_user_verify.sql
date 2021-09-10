select sp_drop_function('sp_user_verify');

create or replace function sp_user_verify(
           verification_code     varchar,
           password              varchar
)
as    
$$
--Revision $Id: f08d6c87498272ebf22f2dc65f17bc921041a137 $ 
declare
  v_verification_code    alias for verification_code;
  v_password             alias for password;
  v_user_create_id       bigint            ;
  v_user_id              bigint            ;
begin

  select e.user_create_id
   into  v_user_create_id
    where e.v_verification_code = v_verification_code
     and  e.datetime_verificate = null;

 if e.user_create_id is null then
    raise exception '%' 'invalid validationcode';
 end if;

  insert 
   into user_valid(
     user_name     ,
     email_address ,
     valid_from    ,
     valid_to      ,
     datetime_create,
     record_state
   )
  values(
  select p.user_name      ,
         p.email_address  ,
         clock_timestamp(),
         'infinity'       ,
         clock_timestamp(),
         'A'
   from  user_create p
   where p.user_create_id = v_user_create_id)
   returning user_id into v_user_id;

   insert 
     into password(
       user_id    ,
       password   ,
       valid_from ,
       invalid_from
     ) values(
       v_user_id,
       v_password,
       clock_timestamp(),
       'infinity'
     );
end;
$$
language plpgsql
SECURITY DEFINER
SET SEARCH_PATH FROM CURRENT;
