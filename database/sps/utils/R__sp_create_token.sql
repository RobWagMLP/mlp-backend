
select sp_drop_function('sp_create_token');


create or replace  function sp_create_token(
    token_length       int     default 10 ,
    base               int     default 26 ,
    start_char         char(1) default 'A',
out token              varchar
)                             
as
$$
--Revision $Id: a6c74dc65ad324230e356472d271137542823223 $
declare
  v_base          alias for base             ;
  v_start_char    alias for start_char       ;
  v_token_length  alias for token_length     ;
  v_token         alias for token            ;
begin
  select string_agg(chr(ascii(v_start_char)+ floor(random() * v_base)::int),'')
    into v_token
    from generate_series(1,v_token_length,1);
end;
$$
language plpgsql
SECURITY DEFINER
SET SEARCH_PATH FROM CURRENT;

