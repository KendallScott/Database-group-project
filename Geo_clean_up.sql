 select
	[id],
	[name],
	[date],
	[manner_of_death],
	[armed],
	[age],
	[gender],
	[race],
	(case when city ='Lakewood' and state='CO' and  [city] not like '%county%' then 'Littleton'
    when city like '%East Baton Rouge%' then 'Baton Rouge'
    when city = 'Fox Crossing'and state='WI' then 'Menasha'
    when  [city] not like '%county%' then replace(replace([city], ', ', ''), 'St.', 'Saint ') end) as city,
    case when [city]  like '%county%' then replace([city], ' County', '') 
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

/*distinct table for city join*/
select 
distinct 
[City], 
[State],
[State_full],
max([County]) as County
into #city_clean
from [dbo].[cty-cnty] 
group by [City], 
[State],
[State_full]

/*checking for missing locations*/
SELECT 
      a.[city]
      ,a.county as county_a
      ,a.[state]
      ,count([id]) as count
  FROM #police_shootings_clean a 
  left join #city_clean b on lower(a.city)=lower(b.[City]) and lower(a.state)=lower(b.state)
 where b.[City]  is null
group by a.[city]
      ,a.county
      ,a.[state]

/*join for city*/
SELECT 
distinct
a.*, 
b.[State_full] AS State_full,
b.county as County_full
into #police_shootings_geo1
FROM #police_shootings_clean a 
left join #city_clean b on lower(a.city)=lower(b.[City]) and lower(a.state)=lower(b.state)

/*distinct table for county join*/
select 
distinct 
[State],
[State_full],
[County]
into #county_clean
from [dbo].[cty-cnty] 

SELECT 
distinct
	[id],
	[name],
	[date],
	[manner_of_death],
	[armed],
	[age],
	[gender],
	[race],
    a.[city],
    a.[county],
	a.[state],
	[signs_of_mental_illness],
	[threat_level] ,
	[flee],
	[body_camera],
	[longitude],
	[latitude],
	[is_geocoding_exact],
coalesce( a.[State_full], d.State_full) AS State_full,
coalesce(a.County_full, d.county) as County_full
into #police_shootings_geo2
  FROM #police_shootings_geo1 a 
 left join #county_clean d on lower(a.county)=lower(d.[County])  and lower(a.state)=lower(d.state) and a.County_Full is null

drop table #police_shootings_geo2
select * from #police_shootings_geo

/*distinct table for city alias join*/
select 
distinct 
[State],
[State_full],
max([County]) as county,
City_alias
into #city_alias_clean
from [dbo].[cty-cnty] 
group by City_alias, [State], [State_Full]

SELECT 
distinct
	[id],
	[name],
	[date],
	[manner_of_death],
	[armed],
	[age],
	[gender],
	[race],
    a.[city],
    a.[county],
	a.[state],
	[signs_of_mental_illness],
	[threat_level] ,
	[flee],
	[body_camera],
	[longitude],
	[latitude],
	[is_geocoding_exact],
coalesce( a.[State_full], d.State_full) AS State_full,
coalesce(a.County_full, d.county) as County_full
into #police_shootings_geo3
  FROM #police_shootings_geo2 a 
 left join #city_alias_clean d on lower(a.City)=lower(d.[City_alias])  and lower(a.state)=lower(d.state) and a.County_Full is null

/*Create perm table*/
SELECT *
INTO dbo.Police_Shootings_Geo_Location
FROM #police_shootings_geo3
