alter table email_verification_code
    add constraint email_verification_code_gex EXCLUDE USING GIST (user_create_id     WITH = , verification_code WITH = ) where (datetime_verificate = null);