select * from Houses
select * from Profiles
select * from Addresses
select * from Employment_details
select * from Referrals
select *,DATEDIFF(dd,move_in_date,move_out_date) from Tenancy_histories where move_out_date is not null

select * from profiles where profile_id = 1


-----Problem 1 solve-----------
With Tenant_Det As (select p.profile_ID,p.first_name,p.last_name,p.phone,th.move_in_date,th.move_out_date,th.move_out_reason,
rank() over ( order by DATEDIFF(dd,th.move_in_date,th.move_out_date) desc) Tenant_age
from Profiles p
left join
Tenancy_histories th
on
p.profile_id=th.profile_id
where move_out_date is not NULL
)

select * from Tenant_Det
where Tenant_age=1

-------end-----------

--------Problem 2 solve-------

select p.first_name,p.last_name,p.email,p.phone,p.marital_status,th.rent
from Profiles p
left join
Tenancy_histories th
ON
p.profile_id=th.profile_id
where p.marital_status='Y' and th.rent>9000

-----------End--------------------

-----------Problem 3 solve-------------

select p.profile_id,p.first_name,p.last_name,p.phone,p.email,p.city,th.house_id,th.move_in_date,th.move_out_date,th.rent,
ed.latest_employer,ed.Occupational_category,rf.referrer_id,count(rf.referrer_id) as Total_Count
from Profiles p
left join
Tenancy_histories th
on
p.profile_id=th.profile_id
left join
Employment_details ed
on
p.profile_id=ed.profile_id
left join
Referrals rf
on
p.profile_id=rf.referrer_id
where referrer_id is NOT NULL
group by p.profile_id,p.first_name,p.last_name,p.phone,p.email,p.city,th.house_id,th.move_in_date,th.move_out_date,th.rent,
ed.latest_employer,ed.Occupational_category,rf.referrer_id
having p.city='Bangalore' or p.city='Pune'
--------end-----------------------------------

-----problem 4 solve---------

select p.first_name,p.last_name,p.email,p.phone,p.referral_code ,
count(rf.referrer_id) as Total_count,sum(rf.referrer_bonus_amount) as Total_Bonus
from Profiles p 
left join
Referrals rf
on 
p.profile_id=rf.referrer_id
where referrer_id is not null 
group by p.first_name,p.last_name,p.email,p.phone,p.referral_code 
having count(rf.referrer_id)>1

---------end---------------------

------problem 5 solve--------------

select p.city,sum(th.rent) as Total_Rent
from Tenancy_histories th
left join
Profiles p
on
th.profile_id=p.profile_id
group by p.city
union
select  'Total', sum(rent) from Tenancy_histories

--------------end---------------------

-----------problem 6 solve------------------
create view vw_tenant
as
select th.profile_id,th.rent,th.move_in_date,h.house_type,h.Beds_vacant,ad.description,ad.city
from Tenancy_histories th
left join
Houses h
on 
th.house_id=h.house_id
left join
Addresses ad
on
th.house_id=ad.house_id
where th.move_in_date>='2015-04-30' and h.Beds_vacant>0

select * from vw_tenant
--------end--------------------------
--------problem 7 solve--------------
--select * from referrals


select *,DATEADD(mm,1,valid_till) as New_Valid_Till from Referrals where referrer_id in (select referrer_id from Referrals group by referrer_id having count(*) > 2)

GO


Update Referrals set valid_till = DATEADD(mm,-1,valid_till) where referrer_id in (select referrer_id from Referrals group by referrer_id having count(*) > 2)

GO

select * from Referrals

select * from Referrals_bkp
------------END-----------------

-------problem 8 solve------
select p.profile_id,p.first_name,p.last_name,p.phone,th.rent,case 
 when rent>10000 then 'Grade A'  
 when rent>7500 and rent<10000 then 'Grade B'
 else 'Grade C' End As Customer_Segment
from Profiles p
left join 
Tenancy_histories th
on
p.profile_id=th.profile_id

-------------------end-------------------------

----------problem 9 solve ----------------------
select h.*,p.first_name,p.last_name,p.phone,p.city,rf.referrer_id from Houses h
left join
Tenancy_histories th
on 
h.house_id=th.house_id
left join
Profiles p
on
th.profile_id=p.profile_id
left join
Referrals rf
on
p.profile_id=rf.referrer_id
where rf.referrer_id is NULL
-------------end--------------------
-----Problem 10 solve------

With House_Det As (select *,rank() over(order by h.bhk_details DESC )  Rank_House
from Houses h where Beds_vacant=0 )
select H.house_id,H.house_type,H.bhk_details,H.bed_count,H.furnishing_type,A.name,A.description,A.pincode,A.city from House_Det H
inner join Addresses A on
H.house_id = A.house_id
where H.Rank_House=1


------end-------------------------









