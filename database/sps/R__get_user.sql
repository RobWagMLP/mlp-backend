select sp_drop_function('sp_get_user');

create or replace function sp_get_user(
 inout          user_name         varchar,
   out          email_address     varchar,
   out          user_id           bigint 
)
as    
$$
--Revision $Id: f08d6c87498272ebf22f2dc65f17bc921041a137 $ 
declare
  v_user_name         alias for user_name    ;
  v_email_address     alias for email_address;
  v_user_id           alias for user_id      ;
begin
   select u.user_name    ,
          u.email_address,
          u.user_id   
    into  v_user_name    ,
          v_email_address,
          v_user_id 
    from  user_valid u
    where u.user_name = v_user_name; 

end;
$$
language plpgsql
SECURITY DEFINER
SET SEARCH_PATH FROM CURRENT;
