/* DNA - capturing failure to engage among women linked to Cafcass*/


select
a.person_id,
a.case_start_date,
a.case_end_date,
b.[start_date] as event_date,

b.SLAM_Event_Type_ID,
b.event_type_of_contact_id,
b.event_outcome_id,

-- identifying DNA events:
dna = case
when lower(b.event_outcome_id) like '%dna by client%' or
	 lower(b.event_outcome_id) like 'dna' or
	 lower(b.event_outcome_id) like 'dna but appointment went ahead' or
	 lower(b.event_outcome_id) like 'did not attend' or
	 lower(b.event_outcome_id) like '%refused access%' or
	 lower(b.event_outcome_id) like '%dna%' or
	 lower(b.event_outcome_id) like '%could not be seen' or
	 lower(b.event_outcome_id) like 'not at address'
then 1
else 0
end,
 
-- identifying patient cancelled events:
cancel = case 
when lower(b.event_outcome_id) like '%cancelled by patient' or
	 lower(b.event_outcome_id) like '%cancelled by client'
then 1
else 0
end

from
(
select distinct
person_id,
brcid,
case_start_date,
case_end_date
from
[cafcass_case_epjs_spine] 
where row_num = 1) as a

left join
[event] as b
on a.brcid = b.brcid 

where [start_date] <= '31-mar-2020' and [start_date] >= '01-jan-2005' and b.brcid is not null
