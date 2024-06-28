grant select on fudgemart_employees to guestuser

grant select, insert on fudgemart_employees to guestuser

grant select on v_fudgemart_display_active_products
to guestuser

revoke select on fudgemart_employees to guestuser

drop user guestuser