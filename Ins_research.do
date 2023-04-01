use "/Users/HaileyLouw/Desktop/Year 2/MAT 375/Term Paper/ECO375Data1 (1).dta"
 
log using "TermPaper", replace

ssc install outreg2
summarize
tab athlete
tab long_distance athlete
gen dist_ath = long_distance*athlete
gen pell_ath = pellgrant*athlete
gen female_ath = gender*athlete

** interaction term between athletes and distance is sig. at 99% confidence ** 
reg gpa gender hs_gpa pellgrant athlete long_distance act_2529 act_3036, r

** being from a shorter distance is a big difference in GPA for ACT 25 - 29 
// short distance outperforming ** 
ttest gpa if act_3036 == 0 & act_2529 == 0, by(athlete) 
ttest gpa if act_2529 == 1, by(long_distance)

** testing differences in hs and college GPAs ** 
ttest gpa if long_distance == 1, by(athlete)
ttest gpa if long_distance == 0, by(athlete)

ttest hs_gpa if long_distance == 1, by(athlete)
ttest hs_gpa if long_distance == 0, by(athlete)


** Outreg ** 
quietly reg gpa hs_gpa gender long_distance, r 
outreg2 using "TermPaper_outreg.xml", excel replace ctitle(Freshman GPA)
label var gpa "Freshman GPA"
label var hs_gpa "HS GPA"
label var gender "Female"
label var long_distance "250+ Miles"
quietly reg gpa hs_gpa gender long_distance athlete, r 
outreg2 using "TermPaper_outreg.xml", excel append ctitle(Freshman GPA)
label var athlete "Athlete"
quietly reg gpa hs_gpa gender long_distance athlete pellgrant act_2529 act_3036, r
outreg2 using "TermPaper_outreg.xml", excel append ctitle(Freshman GPA)
label var pellgrant "Pell Grant"
label var act_2529 "ACT 25 - 29"
label var act_3036 "ACT 30 - 36"
quietly reg gpa hs_gpa gender long_distance athlete pellgrant act_2529 act_3036 dist_ath, r
outreg2 using "TermPaper_outreg.xml", excel append ctitle(Freshman GPA)
label var dist_ath "Long Distance x Athlete"
quietly reg gpa hs_gpa gender long_distance athlete pellgrant act_2529 act_3036 dist_ath female_ath pell_ath, r
outreg2 using "TermPaper_outreg.xml", excel append ctitle(Freshman GPA)
label var dist_ath "Female x Athlete"
label var dist_ath "Pell Grant x Athlete"

** Regressing between athletes and non-athletes ** 
reg gpa gender hs_gpa pellgrant long_distance act_2529 act_3036 if athlete == 1, r
reg gpa gender hs_gpa pellgrant long_distance act_2529 act_3036 if athlete == 0, r
reg gpa gender hs_gpa pellgrant long_distance act_2529 act_3036 athlete if long_distance == 0, r
reg gpa gender hs_gpa pellgrant long_distance act_2529 act_3036 athlete if long_distance == 1, r

log close 




