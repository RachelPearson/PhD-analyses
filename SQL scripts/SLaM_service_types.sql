/* SLaM services - grouped based on SLaM directory (and google) */

select
*
from
(
	select
	*,
  
	-- alcohol services
	alcohol = case 
	when service_text like '%alcohol rehab%' or
		 service_text like'%aao%' or
		 service_text like'%alcohol assertive outreach%'
		 then 1
	else 0
	end,

	-- learning disability services
	ldisab = case
	when service_text like '%mhld%' or
		 substring(service_text, len(service_text) - 2, 3) = '_ld' or 
		 service_text like '%gstt ld adult%' or
		 service_text like '%mhild%' or
		 service_text like '%learning dis%'
		 then 1 
	else 0
	end,

	-- personality disorder services:
	personality = case
	when service_text like '%fipts%' or
	     service_text like'%personality%' or
		 service_text like'%cawley centre%' or
		 service_text like '%dialectical behaviour%'
	     then 1 
	else 0
	end,

  
  -- mental health of older adults services:
	mhoa = case  
	when service_text like '%mhoa%' or
		 service_text like '%dementia%' or
		 service_text like '%older adults%' or
		 service_text like '%older persons%' or
		 service_text like '%hayworth ward%' or
		 service_text like '%memory service%' or
		 service_text like '%memory clinic%' or
		 service_text like '%memory assessment clinic%' or
		 service_text like '%parkinsons%' or
		 service_text like'%chelsham house%' or
		 service_text like'%aubrey lewis 1%' or
		 service_text like'%al1%' or 
		 service_text like'%eamonn fottrell%' or -- day centre for older adults with mh problems 
		 service_text like'%care home intervention%' or
		 cag like 'mhoa and dementia'
		 then 1
	else 0
	end,

  -- mother-baby unit:
	motherbaby = case
	when service_text like '%mother and baby unit%' or
	     service_text like '%mother & baby unit%'
		 then 1
	else 0
	end,

  -- perinatal services (including mother-baby unit):
	perinatal = case   
	when (service_text like '%perinatal%' or
		 service_text like '%post natal%' or
		 service_text like '%baby%' or
		 service_text like '%infant%' or
		 service_text like '%mother%' or
		 service_text like '%mappim%') and (service_text not like '%mother and baby unit%' and  service_text not like '%mother & baby unit%')
		 then 1
	else 0
	end,

  -- Child and Adolescent Mental Health services:
	camhs = case  
	when service_text like '%camhs%' or
		 service_text like '%lyps%' or -- lewisham young people's service
		 service_text like '%paediatric liaison%' or
		 (service_text like '%child%' and service_text like '%adolescent%') or
		 service_text like '%child development%' or
		 service_text like '%belgrave%' or
		 service_text like'%bloomfield%' or
		 service_text like'%tyson east 1%' or -- bethlem hosp adolescent unit
		 service_text like'%children%' or
		 service_text like'%adolescent%' or 
		 service_text like'%carelink%' or
		 service_text like'%care link%' or
		 service_text like'%tamhs%' or
		 service_text like'%famil%' or
		 service_text like'%pair%' or
		 service_text like'%parent and child%' or
		 service_text like'%fft%' or
		 service_text like '%child traumatic stress clinic%' or
		 service_text like'%caft%' -- conduct, adoption and fostering team
		 then 1
	else 0
	end,

  -- family/parent focussed services:
	parent = case  
	when service_text like '%famil%' or
		 service_text like '%fft%' or
		 service_text like '%parenting assessment%' or
		 service_text like '%intensive parenting%' or
		 service_text like '%parent and child%' or 
		 service_text like '%pair%' or
		 service_text like '%carelink%' or
		 service_text like'%care link%' or
		 service_text like'%caft%' or -- conduct, adoption and fostering team
		 service_text like '%hfp%' -- helping families project
		 then 1
	else 0
	end,

  -- Psychosis services:
	psychosis = case 
	when (service_text like '%psychosis%' or
		 service_text like '%clozapine%' or
		 service_text like '%alliance rehab team%' or
		 --service_text like '%homeless outreach%' or
		 service_text like '%tony hillis%' or
		 service_text like '%sharp%' or
		 service_text like '%oasis%' or
		 service_text like '%mckenzie rehabilitation%' or 
		 service_text like '%heather close%' or
		 (service_text like '%westways%' and (service_text like '%ward%' and service_text like '%inpatient%')) or
		 service_text like '%early intervention unit%' or
		 service_text like '%early onset service%' or
		 service_text like '%leo%' or
		 service_text like '%early intervention service%' or
		 service_text like '%promoting recovery%' or
		 --service_text like '%speedwell%' or
		 service_text like '%enhanced recovery%' or
		 service_text like '%recovery and rehab%' or
		 service_text like '%ccos%' or
		 service_text like '%community opportunities service%' or
		 service_text like '%st. giles%' or
		 service_text like '%early intervention team%' or
		 cag like 'psychosis') and (service_text not like '%child%' and service_text not like '%adolescent%' and service_text not like '%camhs%') 
		 then 1
	else 0
	end,

  -- Neurodevelopmental/brain injury services:
	neuro = case  
  when service_text like '%brain injury%' or
		 service_text like '%neurodevel%' or
		 service_text like '%neuro-devel%' or
		 service_text like '%learning dis%' or
		 service_text like '%neurolog%' or
		 service_text like '%neuropsych%' or
		 service_text like '%neuro psych%' or
		 service_text like '%transforming care in autism%' or
		 service_text like '%adult adhd%' or
		 service_text like '%3b asd service%' or
		 service_text like '%3b adhd service%' or
		 service_text like '%mhld%' or
		 substring(service_text, len(service_text) - 2, 3) = '_ld' or 
		 service_text like '%gstt ld adult%' or
		 service_text like '%mhild%' or
		 service_text like '%adult attention%' or
		 service_text like '%lambeth adhd%' or
		 service_text like '%croydon adhd%' or
		 service_text like '%miets%' or
		 cag like 'behavioural and developmental%'
		 then 1
	else 0
	end,

  -- SLaM addictions services:
	addict = case
	when (service_text like '%add-%' or
		 service_text like '%substance misuse%' or
	     service_text like '%dual diagnosis%' or
		 service_text like '%dual team%' or -- e.g. catford
		 service_text like '%addict%' or
		 service_text like '%chemclinic%' or
		 (service_text like '%drug%' and service_text like '%alcohol%')or
		 service_text like '%drug rehab%' or
		 service_text like '%alcohol rehab%' or
		 service_text like '%post natal clinic - marina house%' or -- perinatal addictions servic
		 service_text like'%aao%' or
		 service_text like'%alcohol assertive outreach%' or
		 service_text like'%drr%' or -- drug rehabilitation requirement service
		 service_text like'%stockwell project%' or -- drug rehabilitation requirement service
		 service_text like'%drug testing%' or
		 service_text like'%daat%' or -- drug and alcohol team
		 service_text like'%community drug team%' or
		 service_text like'%cdt%' or -- community drug teams
		 service_text like'%dtto%' or
		 service_text like'%wcdas%') --wandsworth consortium drug and alcohol service
		 and service_text not like '%smok%' -- exclude smoking services
		 then 1
	else 0
	end,

  -- SLaM forensic services:
	forensic = case
	when service_text like '%forensic%' or
		 service_text like '%secure unit%' or
		 service_text like '%lsu%' or -- low secure unit
		 service_text like '%msu%' or -- medium secure unit
		 service_text like '%hsu%' or -- high secure unit
		 service_text like '%aafs%' or -- adolescent at-risk and forensic service
		 service_text like '%high support and recovery%' or
		 service_text like '%mental health learning disabilities placement monitoring%' or
		 service_text like '%fipts%' or -- forensic intensive psychological treatment service
		 service_text like '%bracton%' or -- secure wards
		 service_text like '%brook ward%' or
		 service_text like '%norbury%' or
		 service_text like '%chaffinch%' or
		 service_text like '%danson%' or
		 service_text like'%burgess%' or
		 service_text like'%crofton%' or
		 service_text like'%heath ward%' or 
		 service_text like'%joydons%' or
		 service_text like'%birchwood%' or
		 service_text like '%mckenzie house%' -- low secure residential forensic rehabilitation unit
		 then 1
	else 0
	end,

  -- SLaM services that indicate criminal justice involvement:
	criminal = case
	when service_text like '%criminal%' or
		 service_text like '%prison%' or
		 service_text like'%dtto%' or
		 service_text like'%drr%' or
		 service_text like'%stockwell project%' or
		 service_text like '%offender%'  -- e.g. young offenders service
		then 1
	else 0
	end,

  -- eating disorder services:
	eating = case 
	when service_text like '%eating disorder%' or
		 service_text like '%dietetics%' 
		 then 1
	else 0
	end,

  -- acute psychiatric services (e.g., inpatient or crisis care):
	acute = case 
	when service_text like '%triage ward%' or
	     service_text like '%street triage%' or
		 service_text like '%place of safety%' or
		 service_text like '%emergency clinic%' or
		 service_text like '%effra resource centre%' or -- community service for mental health crises
		 service_text like '%acute%' or
		 (service_text like '%unit%' and service_text like '%inpatient%') or
		 service_text like '%ward%' or
		 service_text like '%home treatment%' or
		 service_text like '%htt%' or
		 service_text like '%cdu%' or -- clinical decision unit
		 service_text like '%cru%' or
		 service_text like '%crisis recovery unit%' or
		 service_text like '%crisis assessment%' or
		 service_text like '%crisis outreach%' or
		 service_text like '%crisis response%' or
		 service_text like '%crisis cafe%' or
		 service_text like '%crisis hostel%' or
		 service_text like '%behavioural disorders unit%' or
		 service_text like '%_arc%' or -- acute referral centre
		 (service_text like '%picu%' and service_text not like '%picup%') or -- psychiatric intensive care unit
		 service_text like '%dove house%' or -- decomissioned in 2009 
		 service_text like '%secure unit%' or
		 service_text like '%lsu%' or -- low secure unit
		 service_text like '%msu%' or -- medium secure unit
		 service_text like '%hsu%' or -- high secure unit
		 service_text like '%tony hillis%' or -- wards
		 service_text like '%mckenzie%' or
		 service_text like '%ladywell unit%' or
		 service_text like '%bridge house%' or
		 service_text like '%fitzmary%' or
		 service_text like '%gresham%' or
		 service_text like '%chelsham house%' or
		 service_text like'%eileen skellern%' or
		 service_text like'%es1%' or
		 service_text like'%es2%' or
		 service_text like'%wharton%' or
		 service_text like'%clare%' or
		 service_text like'%jbu%' or
		 service_text like'%nelson%' or
		 service_text like'%johnson unit%' or
		 service_text like '%inpatient%' or
		 service_text like'%ruskin%' or
		 service_text like'%tyson west 1%' or
		 service_text like'%tyson west 2%' or
		 service_text like'%tyson east 1%' or -- bethlem hosp adolescent unit
		 service_text like '%foxley%' or
		 service_text like '%women%service%' or -- foxley lane women's service
		 cag like 'acute care pathway'
		 then 1
	else 0
	end


	from
		(
		select
		count(*) as n,
		cag,
		service_unit, 
		Service_line, 
		Sub_service_line, 
		location_name,
		lower(coalesce(service_unit, '')+'_'+coalesce(Service_line, '')+'_'+coalesce(Sub_service_line,'')+'_'+coalesce(location_name,'')) as service_text  
	
		from
		(
			select distinct
			brcid
			from
			[SQLCRIS_User].[Rachel].[cafcass_master_linkage_table_after_excl_wout_dups]
					cross apply
			(values
			('epjs', epjs),
			('epjs', epjs_duplicate),
			('epjs', epjs_brcid)) x (brcid_type, brcid)
			where brcid is not null 
		) as a

		left join
		[SQLCRIS].[dbo].[event] as b
		on 
		a.brcid = b.brcid

		where b.brcid is not null and [start_date] <= '31-mar-2020'

		group by cag, service_unit, Service_line, Sub_service_line, location_name

		) as c

	) as d


