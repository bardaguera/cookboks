### Tables characteristics
```sql
select c.relname table_, a.attname column_, t.typname type_, c.relhasindex hasindex, c.relpages pages, c.reltuples rows_,
	round(s.stanullfrac::numeric, 2) null_percent,
	round((s.stadistinct/(c.reltuples*(1-s.stanullfrac)))::numeric, 2) dist_percent,
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
