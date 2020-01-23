### Tables characteristics
```sql
select c.relname table_, a.attname column_, t.typname type_, c.relhasindex hasindex, c.relpages pages,
	c.reltuples rows_, round(s.stanullfrac::numeric, 2) null_percent,
	case when coalesce(c.reltuples,0) = 0 then 'EMPTY TABLE or STATBUG'
		when s.stanullfrac = 1 then 'FULL'
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
and c.relnamespace = (select oid from pg_namespace where nspname = 'some_schema')
order by s.staattnum
```
### Grant permissions
```pl
DO $f$
declare
	u text[] default array['report'];
	schema_ text = 'someschema';
	o text[]
		default array['someschema.sometable'];
begin
	/* --get all tables
	select array_agg(schema_||'.'||table_name::text)
	into o
	from information_schema.tables t
	where table_schema = schema_;*/
for u_i in 1.. array_upper(u, 1) loop
	for o_i in 1.. array_upper(o,1) loop
		EXECUTE 'grant select on ' || o[o_i] || ' to ' || u[u_i];
	end loop;
end loop;
END; $f$ 
LANGUAGE plpgsql;
```

### Find attribute
```pl
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

### Get schema via json
```sql
select json_build_object(
	'schema', max(ns.nspname),
	'tables', json_agg(
				json_build_object(
					'table_name', pc.relname,
					'table_id', a.table_id::int,
					'attrs', a.vals::jsonb)))
from pg_class pc
join pg_namespace ns on ns.oid = pc.relnamespace
join (select attrs.table_id, json_agg(row_to_json(attrs))::jsonb-'table_id' vals
		from (select pa.attrelid table_id,
					pa.attnum col_num,
					pa.attname col_name,
					t.typname col_type,
					moe.modified_on_attr_exists,
					col_description(pa.attrelid, pa.attnum) col_desc
				from pg_attribute pa
				join (select pa.attrelid,
						case when pa.attname is not null then True else False end modified_on_attr_exists
					from pg_attribute pa where pa.attname = 'ModifiedOn') moe on pa.attrelid = moe.attrelid
			join pg_type t on pa.atttypid = t.oid) attrs
		group by attrs.table_id		
		) a on pc.oid = a.table_id
```

### Get attr names
```sql
select attname, attnum, split_part(str, '|', 2) caption
from
	(
	select attname, attnum, regexp_split_to_table(caption, ',') str
	from (
		select c.oid, a.attname, a.attnum,
		unnest(regexp_split_to_array(col_description(c.oid, a.attnum),';')) caption
		from pg_class c
		join pg_attribute a on a.attrelid = c.oid
		where c.relkind in ('r', 'm')
			/*r=ordinary table, i=index, S=sequence, v=view, m=materialized view,
			c=composite type, t=TOAST table, f=foreign table*/
		and c.relname like 'Lead'
		and c.relnamespace = (select oid from pg_namespace where nspname = 'public')
		) t
	where caption like 'TS.EntitySchemaColumn.Caption%'
	) lang
where lang.str like '%ru-RU%'
order by attnum
```
