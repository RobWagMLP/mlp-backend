with new_values (memo   , subject,              type,   template    ) as
        (values ('EMVRY', 'Your Registration', 'plain', 'Hello {user_name} \r\n your verfification code is {verification_code}.\r\n Have a nice day' )
                ),
     deleted   as(delete from email_templates
                   where not exists(select 1
                                      from new_values
                                     where new_values.memo = email_templates.memo)),
     updated as (update email_templates
                    set template = new_values.template,
                        subject = new_values.subject  ,
                        type    = new_values.type
                   from new_values
                  where new_values.memo = email_templates.memo)
insert
  into email_templates
      (memo      ,
       template  ,
       subject   ,
       type      ,
       datetime_create)
select new_values.memo          ,
       new_values.template      ,
       new_values.subject       ,
       new_values.type          ,
       clock_timestamp()
  from new_values
  left join email_templates using(memo)
 where email_templates.memo is null;
