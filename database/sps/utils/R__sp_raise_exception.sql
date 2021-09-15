select sp_drop_function('sp_raise_exception');

create or replace  function sp_raise_exception(
  v_error_code int                           ,
  v_error_text varchar           default null
)
returns void 
as
$$
declare
--Revision $Id: f08d6c87498272ebf22f2dc65f17bc921041a137 $ 
begin
   if v_error_text is null then 
    v_error_text := error_text from error_code where error_code = v_error_code;
  end if;
  v_error_text := coalesce(v_error_text,'error_text to be set');
  
  RAISE EXCEPTION '%', v_error_code using DETAIL = v_error_text;
end;
$$
language plpgsql
SECURITY DEFINER
SET SEARCH_PATH FROM CURRENT;
