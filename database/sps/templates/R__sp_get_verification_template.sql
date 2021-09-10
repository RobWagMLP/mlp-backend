
select sp_drop_function('get_verfication_template');


create or replace  function get_verfication_template(
    user_name          varchar         ,
    email_address      varchar         ,
    verification_code  varchar(10)     ,
    memo               varchar  default 'EMVRY',
out template_text      varchar             ,
out type               varchar             ,
out subject            varchar
)                             
as 
$$
--Revision $Id: a6c74dc65ad324230e356472d271137542823223 $
declare
 v_memo          alias for memo;
 v_user_name     alias for user_name;
 v_email_address alias for email_address;
 v_subject       alias for subject;
 v_type          alias for type   ;
 v_verification_code  alias for verification_code;
 v_template_text      alias for template_text;
begin
  select e.template   ,
         e.type       ,
         e.subject    
    into v_template_text,
         v_type         ,
         v_subject
    from email_templates e
    where e.memo = v_memo;

 select replace(v_template_text, '{user_name}', v_user_name)
  into  v_template_text;
 select replace(v_template_text, '{user_email}', v_email_address)
  into  v_template_text;
 select replace(v_template_text, '{verification_code}', v_verification_code)
  into  v_template_text;

end;
$$
language plpgsql
SECURITY DEFINER
SET SEARCH_PATH FROM CURRENT;

