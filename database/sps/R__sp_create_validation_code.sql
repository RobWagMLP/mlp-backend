select sp_drop_function('sp_create_validation_code');

create or replace function sp_create_validation_code(
           user_create_id          bigint      ,
    out    validation_code         varchar(10)
)
as    
$$
--Revision $Id: f08d6c87498272ebf22f2dc65f17bc921041a137 $ 
declare
  v_user_create_id    alias for user_create_id;
  v_validation_code   alias for validation_code;
begin
   select token
     into v_validation_code
     from sp_create_token();

     insert 
      into email_verification_code(
            user_create_id      ,
            verification_code   ,
            datetime_create     ,
            datetime_invalidate 
      ) values(
          v_user_create_id   ,
          v_validation_code  ,
          statement_timestamp(),
          statement_timestamp() + interval '1 month'
      );

end;
$$
language plpgsql
SECURITY DEFINER
SET SEARCH_PATH FROM CURRENT;
