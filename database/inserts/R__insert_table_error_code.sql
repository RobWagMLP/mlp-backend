--------------------------------------------------------
--dank der referentiellen integritaet koennen wir die tabelle nicht einfach leer machen und neu befuellen...
with new_values (error_code,procedure                     ,error_text                          )
      as(values (1000001   ,'sp_user_verify'              ,'invalid validation code'                                       ),
                (1000002   ,'sp_user_login'               ,'invalid login data'                                            ),
                (9999999   ,'to_be_named'                 ,'error_text to be set'                                          )
                 ),
     deleted   as(delete from error_code
                   where not exists(select 1
                                      from new_values
                                     where new_values.error_code = error_code.error_code)),
     updated as (update error_code
                    set procedure  = new_values.procedure ,
                        error_text = new_values.error_text
                   from new_values
                  where new_values.error_code = error_code.error_code)
insert
  into error_code
      (error_code,
       procedure ,
       error_text)
select new_values.error_code,
       new_values.procedure ,
       new_values.error_text
  from new_values
  left join error_code using( error_code)
 where error_code.error_code is null;

