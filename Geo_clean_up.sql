  drop table #police_shootings_clean;
  
  select
	[id],
	[name],
	[date],
	[manner_of_death],
	[armed],
	[age],
	[gender],
	[race],
	case when [city] not like '%county%' then replace(replace([city], ', ', ''), 'St.', 'Saint ') end as city,
    case when [city]  like '%county%' then replace([city], ' County', '') 
    when city ='Lakewood' and state='CO' then 'Jefferson' 
    when city ='Centenial' and state='CO' then 'Arapahoe' 
    when city ='Jarupa Valley' and state='CA' then 'Riverside' 
    when city ='Northglenn' and state='CO' then 'Adams' 
    end as county,
	[state],
	[signs_of_mental_illness],
	[threat_level] ,
	[flee],
	[body_camera],
	[longitude],
	[latitude],
	[is_geocoding_exact]
    into #police_shootings_clean
from [dbo].[shootings]

SELECT TOP (1000) 
coalesce(c.State, b.[State]) AS STATE_B
      ,coalesce(c.State_Full, b.[State_full]) AS state_full
      ,c.county as county_C
      ,coalesce(c.county, b.[County]) as county_b
      ,b.[City_alias] as city_alias
,[id]
      ,[name]
      ,[date]
      ,[manner_of_death]
      ,[armed]
      ,[age]
      ,[gender]
      ,[race]
      ,a.[city]
      ,a.county as county_a
      ,a.[state]
      ,[signs_of_mental_illness]
      ,[threat_level]
      ,[flee]
      ,[body_camera]
      ,[longitude]
      ,[latitude]
      ,[is_geocoding_exact]
  FROM #police_shootings_clean a 
  left join [dbo].[cty-cnty] b on lower(a.city)=lower(b.[City_alias]) and lower(a.state)=lower(b.state)
  left join [dbo].[cty-cnty] c on lower(a.county)=lower(c.[County]) and lower(a.state)=lower(c.state)
  order by b.[State]

SELECT 
      a.[city]
      ,a.county as county_a
      ,a.[state]
      ,count([id]) as count
  FROM #police_shootings_clean a 
  left join [dbo].[cty-cnty] b on lower(a.city)=lower(b.[City]) and lower(a.state)=lower(b.state)
  left join [dbo].[cty-cnty] c on lower(a.county)=lower(c.[County]) and lower(a.state)=lower(c.state)
    where coalesce(c.county, b.[County])  is null
group by a.[city]
      ,a.county
      ,a.[state]
    
