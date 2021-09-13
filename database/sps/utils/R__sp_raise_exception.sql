select sp_drop_function('sp_raise_exception');

create or replace  function sp_raise_exception(
  v_error_code int                           ,
  v_error_text varchar           default null,
  release_request_ids  bigint[]  default null
)
returns void 
as
$$
declare
  v_release_request_ids        alias for release_request_ids    ;
  v_release_status             char(1)                          ;
--Revision $Id: f08d6c87498272ebf22f2dc65f17bc921041a137 $ 
begin
   if v_error_text is null then 
    v_error_text := error_text from error_code where error_code = v_error_code;
  end if;
  v_error_text := coalesce(v_error_text,'error_text to be set');
  
    if v_release_request_ids is not null then
        select status 
          from release_requests r
          into v_release_status 
          where r.release_requests_id = any( v_release_request_ids ) 
          and   r.error_code = v_error_code;
        if  v_release_status is not null or v_release_status = 'R' then
             return;
        end if;
  end if; 
  
  if exists (select 1 from release_rules rr where rr.error_code = v_error_code) then
     RAISE EXCEPTION '%', v_error_code using DETAIL = v_error_text, errcode = 'NDREL';
  end if;
  
  RAISE EXCEPTION '%', v_error_code using DETAIL = v_error_text;
end;
$$
language plpgsql
SECURITY DEFINER
SET SEARCH_PATH FROM CURRENT;
