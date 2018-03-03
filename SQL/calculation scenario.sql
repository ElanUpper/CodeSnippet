
-- get information about view
select view_name, view_type, is_column_view, is_valid from views where view_name = '/1BCAMDP/0BW:DAP:TR_ET090UFZ8T779G8YGWEGXLROU'

-- get details on calculation scenario
select * from  m_ce_calcscenarios where scenario_name = (
  -- get referenced calculation scenario
  select scenario_name from m_ce_calcview_dependencies where view_name = '/1BCAMDP/0BW:DAP:TR_ET090UFZ8T779G8YGWEGXLROU'
)

--
select to_char(current_date, 'YYYYMMDD') as date, to_char(current_time, 'HHMISS') as time from dummy
-- turn server trace ON
ALTER SYSTEM ALTER CONFIGURATION ('indexserver.ini', 'SYSTEM') SET ('pythontrace', 'trace') = 'on' with reconfigure; 
-- turn server trace OFF
ALTER SYSTEM ALTER CONFIGURATION ('indexserver.ini', 'SYSTEM') UNSET ('pythontrace', 'trace') with reconfigure; 
