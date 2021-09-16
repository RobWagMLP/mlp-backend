select sp_drop_function('sp_user_login');

create or replace function sp_user_login
(
           user_name             varchar,
           password              varchar
)
returns void
as  
$$
--Revision $Id: f08d6c87498272ebf22f2dc65f17bc921041a137 $ 
declare
  v_user_name            alias for user_name        ;
  v_password             alias for password         ;
  v_user_id              bigint                     ;
begin

  select uv.user_id
   into  v_user_id
   from  user_valid uv
   join  password p
     on  uv.user_id = p.user_id
     where p.password = md5(v_password)
       and tstzrange(uv.valid_from, uv.invalid_from) @> statement_timestamp();

  if v_user_id is null then
    perform sp_raise_exception(1000002);
  end if;
end;
$$
language plpgsql
SECURITY DEFINER
SET SEARCH_PATH FROM CURRENT;
