### Tables characteristics
```sql
select c.relname table_, a.attname column_, t.typname type_, c.relhasindex hasindex, c.relpages pages,
	c.reltuples rows_, round(s.stanullfrac::numeric, 2) null_percent,
	case when coalesce(c.reltuples,0) = 0 then 'EMPTY TABLE or STATBUG'
		else
			round((s.stadistinct/(c.reltuples*(1-s.stanullfrac)))::numeric, 2)::text 
	end dist_percent,
	s.stawidth row_width, /*pc.relpersistence*/
	'###' "#", round(((c.reltuples*s.stawidth)/(2^10)^2)::numeric, 2) total_width_mb
from pg_class c
join pg_statistic s on c.oid = s.starelid
join pg_attribute a on s.staattnum = a.attnum and c.oid = a.attrelid
join pg_type t on a.atttypid = t.oid
where c.relkind in ('r', 'm')
	/*r=ordinary table, i=index, S=sequence, v=view, m=materialized view,
	c=composite type, t=TOAST table, f=foreign table*/
and c.relname like 'crm_some_table'
order by s.staattnum
```
### Grant permissions
```pl
DO $$
declare
	u text[] default array['user'];
	o text[] default array['someschema.object_name'];
BEGIN
for u_i in 1.. array_upper(u, 1) loop
	for o_i in 1.. array_upper(o,1) loop
		EXECUTE 'grant select on ' || o[o_i] || ' to ' || u[u_i];
	end loop;
end loop;
END; $$ 
LANGUAGE plpgsql;
```

### Find attribute
```
DO $$
declare
	table_prefix text;
	search_string text;
	temprow RECORD;
	query text;
	res boolean;
begin
	table_prefix = $table_prefix;
	search_string = $search_string;
	FOR temprow IN
	        (select c.relname, a.attname
			from pg_class c
			join pg_attribute a on c.oid = a.attrelid
			where c.relkind in ('r', 'm') and c.relname like '%'||table_prefix||'%')
	    loop
	    	query = 'select exists (select 1 from '|| temprow.relname ||' where ' || temprow.attname ||'::text like ''%'||search_string||'%'')';
	    	execute query into res;
	    	if res
	    	then
	    		raise notice '%', temprow.relname::text ||'.'|| temprow.attname::text;
	    	end if;
	    END LOOP;
END; $$ 
LANGUAGE plpgsql;
```
