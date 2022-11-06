  select
	[id],
	[name],
	[date],
	[manner_of_death],
	[armed],
	[age],
	[gender],
	[race],
	case when [city] not like '%county%' then replace([city], ', ', '') end as city,
    case when [city]  like '%county%' then replace([city], ' County', '') end as county,
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
  left join [dbo].[cty-cnty] c on lower(a.county)=lower(b.[County]) and lower(a.state)=lower(b.state)
  order by b.[State]

  SELECT TOP (1000) 
b.[State] AS STATE_B
      ,b.[State_full] AS state_full
      ,b.[County] as county_b
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
      ,a.[state]
      ,[signs_of_mental_illness]
      ,[threat_level]
      ,[flee]
      ,[body_camera]
      ,[longitude]
      ,[latitude]
      ,[is_geocoding_exact]
  FROM [dbo].[shootings] a 
  left join [dbo].[cty-cnty] b on lower(a.city)=lower(b.[City_alias]) and lower(a.state)=lower(b.state)
  order by b.[State]