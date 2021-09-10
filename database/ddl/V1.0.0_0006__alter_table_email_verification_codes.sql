alter table email_verification_code
    add  EXCLUDE USING GIST (user_create_id     WITH = , verification_code WITH = ) where datetime_verificate = null;