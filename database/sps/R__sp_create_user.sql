select sp_drop_function('sp_create_user');

create or replace function sp_create_user(
           user_name         varchar      ,
           email_address     varchar(255) ,
  out      user_id           bigint        
)
as    
$$
--Revision $Id: f08d6c87498272ebf22f2dc65f17bc921041a137 $ 
declare
  v_user_name         alias for user_name    ;
  v_email_address     alias for email_address;
  v_user_id           alias for user_id;
begin
   insert
     into user_create
     ( user_name      ,
       email_address  ,
       validated      ,
       datetime_create
     ) values
     (
       v_user_name    ,
       v_email_address,
       false          ,
       statement_timestamp()
     )
     returning user_create_id into v_user_id;

end;
$$
language plpgsql
SECURITY DEFINER
SET SEARCH_PATH FROM CURRENT;
