/* 
Import and clean raw survey data for Hoag et al., 2023 Willingness to pay for reintroducing 
wolves. The data came in a strange format so we had to do some cleaning 
to get it to the point where it could be analyzed. 

*/
* Set directory
cd "C:\Users\jbb\OneDrive - Colostate\Wolf_CV\final\analysis\Final_Code_Data"

********************************************************************************
* Clean respondent choices and save dataset as panel of choices
********************************************************************************
import delimited "raw_data.csv", clear
* keep only important variables
keep responseid c1-c8  fa19464f_78a8_4146_851e_35d626e8 a86984634_9697_49ee_ab3d_ef32dca a23f855c1_ea05_4b69_9e5a_58e4615 e5b17b45_3ff6_4fe7_9f0a_ecd3015f a64fdd57b_148f_4bc2_93c6_d6fbb73 a0a9f0e42_7887_4964_9964_55549be fcafb975_d9a9_4d59_84e6_89c82d15 v151 v152 v153 v154 v155 v156 v157 v158 v159 v160 v161 v162 v163 v164 v165 v166 v167 v168 v169 v170 v171 v172 v173 v174 v175 v176 v177 v178 v179 v180 v181 v182 v183 v184 v185 v186 v187 v188 v189 v190 v191 v192 v193 v194 v195 v196 v197 v198 v199 v200 v201 v202 v203 v204 v205 v206 v207 v208 v209 v210 v211 v212 v213 v214 v215 v216 v217 v218 v219 v220 v221 v222 v223 v224 v225 v226 v227 v228 v229 v230 v231 v232 v233 v234 v235 v236 v237 v238 v239 v240 v241 v242 v243 v244 v245 v246 v247 v248 v249 v250 v251 v252 v253 v254 v255 v256 v257 v258 v259 v260 v261 v262 v263 v264 v265 v266 v267 v268 v269 v270 v271 v272 v273 v274 v275 v276 v277 v278 v279 v280 v281 v282 v283 v284 v285 v286 v287 v288 v289 v290 v291 v292 v293 v294 v295 v296 v297 v298 v299 v300 v301 v302 v303 v304 v305 v306 v307 v308 v309 v310 v311

rename fa19464f_78a8_4146_851e_35d626e8 v144
rename a86984634_9697_49ee_ab3d_ef32dca v145
rename a23f855c1_ea05_4b69_9e5a_58e4615 v146
rename e5b17b45_3ff6_4fe7_9f0a_ecd3015f v147
rename a64fdd57b_148f_4bc2_93c6_d6fbb73 v148
rename a0a9f0e42_7887_4964_9964_55549be v149
rename fcafb975_d9a9_4d59_84e6_89c82d15 v150

* Clean up attributes and levels. Variable names start at v144. 
local x = 144 

while `x' < 311 { 
	preserve
	local y = `x'+6
	keep responseid c1-c8 v`x'-v`y'

	local x1 = `x'+6
	rename v`x1' cost
	replace cost = "0" if cost == "$0 "
	replace cost = "10" if cost == "$10 "
	replace cost = "150" if cost == "$150 "
	replace cost = "200" if cost == "$200 "
	replace cost = "50" if cost == "$50 "
	replace cost = "100" if cost == "$100 "

	destring cost, replace

	rename v`x' pop 
	replace pop = "10" if pop == "10 wolves"
	replace pop = "600" if pop == "600 wolves"
	replace pop = "400" if pop == "400 wolves"
	replace pop = "200" if pop == "200 wolves"
	destring pop, replace

	local x1 = `x'+1
	rename v`x1' compensation
	replace compensation = "0" if compensation == "No Compensation"
	replace compensation = "1" if compensation == "Fair Market Value"
	replace compensation = "2" if compensation == "Fair Market Value + Indirect Loss"
	destring compensation, replace

	local x1 = `x'+2
	rename v`x1' cost_sharing
	replace cost_sharing = "0" if cost_sharing == "No Cost Sharing"
	replace cost_sharing = "1" if cost_sharing == "100% of the actual cost"
	destring cost_sharing, replace

	* should have been 8 sheep 
	local x1 = `x'+3
	rename v`x1' livestock
	replace livestock = "8" if livestock == "Minimum (5 cows and 3 sheep per year)"
	replace livestock = "23" if livestock == "Low (15 cows and 18 sheep per year)"
	replace livestock = "90" if livestock == "Moderate (60 cows and 30 sheep per year)"
	replace livestock = "180" if livestock == "High (120 cows and 60 sheep per year)"
	destring livestock, replace

	local x1 = `x'+4
	rename v`x1' gove
	replace gove = "0" if gove == "No wolves lethally removed"
	replace gove = "30" if gove == "Approximately 30 wolves per year (only kill wolves that consistently engage in conflict with livestock)"
	replace gove = "50" if gove == "Approximately 50 wolves per year (kill all wolves that have engaged in conflict with livestock)"
	destring gove, replace

	local x1 = `x'+5
	rename v`x1' hunt
	replace hunt = "0" if hun == "Not Allowed"
	replace hunt = "1" if hun == "Allowed after wolf population is sustainable"
	destring hunt, replace
	
	g card = `x'

	save "c`x'.dta", replace
	restore
	local x = `x' + 7
	display `x'
}
clear
forvalues x = 144/305 {
    cap append using "c`x'.dta"
}
order card 
g choice_set = 0
replace choice_set = 1 if card == 144
replace choice_set = 1 if card == 151
replace choice_set = 1 if card == 158
replace choice_set = 2 if card == 165
replace choice_set = 2 if card ==172
replace choice_set = 2 if card ==179
replace choice_set = 3 if card ==186
replace choice_set = 3 if card ==193
replace choice_set = 3 if card ==200
replace choice_set = 4 if card ==207
replace choice_set = 4 if card ==214
replace choice_set = 4 if card ==221
replace choice_set = 5 if card ==228
replace choice_set = 5 if card ==235
replace choice_set = 5 if card ==242
replace choice_set = 6 if card ==249
replace choice_set = 6 if card ==256
replace choice_set = 6 if card ==263
replace choice_set = 7 if card ==270
replace choice_set = 7 if card ==277
replace choice_set = 7 if card ==284
replace choice_set = 8 if card ==291
replace choice_set = 8 if card ==298
replace choice_set = 8 if card ==305

* protest bidders
egen protest = rowmean(c1-c8)
sort respon card choice
order choice
replace c1 = . if choice_set ~= 1
bys responseid choice_set: g n = _n
order n
forvalues x = 1/8 {
    replace c`x'= 0 if n ~= c`x' & choice_set == `x'
}
forvalues x = 2/8 {
	replace c1 = c`x' if c1 == . & choice_set == `x'
}
replace c1 = 1 if c1 > 1

drop c2-c8
sort responseid 
* panel of choices
save "choices.dta", replace

********************************************************************************
* Merge choices with demographics and other responses to non-choice questions, clean 
* demographics and other variables, and create any additional variables. 
********************************************************************************
* Get other variables 
import delimited "raw_data.csv", clear
drop c1-c8 fa19464f_78a8_4146_851e_35d626e8 a86984634_9697_49ee_ab3d_ef32dca a23f855c1_ea05_4b69_9e5a_58e4615 e5b17b45_3ff6_4fe7_9f0a_ecd3015f a64fdd57b_148f_4bc2_93c6_d6fbb73 a0a9f0e42_7887_4964_9964_55549be fcafb975_d9a9_4d59_84e6_89c82d15 v151 v152 v153 v154 v155 v156 v157 v158 v159 v160 v161 v162 v163 v164 v165 v166 v167 v168 v169 v170 v171 v172 v173 v174 v175 v176 v177 v178 v179 v180 v181 v182 v183 v184 v185 v186 v187 v188 v189 v190 v191 v192 v193 v194 v195 v196 v197 v198 v199 v200 v201 v202 v203 v204 v205 v206 v207 v208 v209 v210 v211 v212 v213 v214 v215 v216 v217 v218 v219 v220 v221 v222 v223 v224 v225 v226 v227 v228 v229 v230 v231 v232 v233 v234 v235 v236 v237 v238 v239 v240 v241 v242 v243 v244 v245 v246 v247 v248 v249 v250 v251 v252 v253 v254 v255 v256 v257 v258 v259 v260 v261 v262 v263 v264 v265 v266 v267 v268 v269 v270 v271 v272 v273 v274 v275 v276 v277 v278 v279 v280 v281 v282 v283 v284 v285 v286 v287 v288 v289 v290 v291 v292 v293 v294 v295 v296 v297 v298 v299 v300 v301 v302 v303 v304 v305 v306 v307 v308 v309 v310 v311
sort responseid
* Merge choice panel 
merge 1:m responseid using "choices.dta"
drop _m 

* Create Alternative Specific Constant (ASC)
g asc = 1 
replace asc = 0 if pop == 10
* population squared
g pop2 = pop^2

* Individual ID
egen id = group(responseid)

* Some summary statistics
sutex  out_1 out_11 out_2 out_4 out_12 out_9 out_10 out_5 out_6 out_7 out_8 out_13 out_8_text, minmax

* Destring reasons for voting
foreach var in vot_reas_ag_1 vot_reas_ag_2 vot_reas_ag_3 vot_reas_ag_4 vot_reas_ag_5 vot_reas_ag_6 vot_reas_ag_7 vot_reas_ag_8 vot_reas_for_1 vot_reas_for_2 vot_reas_for_3 vot_reas_for_4 vot_reas_for_5 vot_reas_for_6 { 
	destring `var', replace
	replace `var' = . if `var' == -99
}
* Summary statistics on voting reasons 
sutex vot_reas_for_1 vot_reas_for_2 vot_reas_for_3 vot_reas_for_4 vot_reas_for_5 vot_reas_for_6 vot_reas_ag_1 vot_reas_ag_2 vot_reas_ag_3 vot_reas_ag_4 vot_reas_ag_5 vot_reas_ag_6 vot_reas_ag_7 vot_reas_ag_8, minmax

* Create demographics. 
* male is 1 
g gender = 0 
replace gender = 1 if ppgender == 1

g white = 0 
replace white = 1 if ppethm == 1 
g black = 0 
replace black = 1 if ppethm == 2
g other_race = 0 
replace other_race = 1 if ppethm == 3
g hispanic = 0 
replace hispanic = 1 if ppethm == 4
g asc_white = asc*white

g home_own = 0 
replace home_own = 1 if pprent == 1

g republican = 0 
replace republican = 1 if xpart ==1 
g democrat = 0 
replace democrat = 1 if xpart == 2
g independent  = 0 
replace independent = 1 if xpart > 2 & xpart < 5

g front_range = 0 
replace front_range = 1 if co_reg3 == 1

g college = 0 
replace college = 1 if ppeducat == 4 

drop caseid cli_disp consent responseid vot_reas_for_1 vot_reas_for_2 vot_reas_for_3 vot_reas_for_4 vot_reas_for_5 vot_reas_for_6 vot_reas_for_text vot_reas_ag_1 vot_reas_ag_2 vot_reas_ag_3 vot_reas_ag_4 vot_reas_ag_5 vot_reas_ag_6 vot_reas_ag_7 vot_reas_ag_8 vot_reas_ag_text out_8_text redirect_url q_url timebefore timeafter vers_cbconjoint revision_cbconjoint

* Summary of other variables. 
* Would they want more funding from other sources? 
sum pveh_1 if pveh_1 >=0, d /* state agency*/
sum pveh_2 if pveh_2 >=0, d /* license plates */
sum pveh_3 if pveh_3 >=0, d /* lottery */

* Destring reasons for voting in favor
foreach var in  vot_reas_for_1 vot_reas_for_2 vot_reas_for_3 vot_reas_for_4 vot_reas_for_5 vot_reas_for_6 { 
destring `var', replace
replace `var' = . if `var' == -99
}

* Destring reasons for voting against
foreach var in  vot_reas_ag_1 vot_reas_ag_2 vot_reas_ag_3 vot_reas_ag_4 vot_reas_ag_5 vot_reas_ag_6 vot_reas_ag_7 vot_reas_ag_8 { 
destring `var', replace
replace `var' = . if `var' == -99
}

* Summarize reasons for voting in favor and against
sutex vot_reas_for_1 vot_reas_for_2 vot_reas_for_3 vot_reas_for_4 vot_reas_for_5 vot_reas_for_6, nobs minmax

sutex  vot_reas_ag_1 vot_reas_ag_2 vot_reas_ag_3 vot_reas_ag_4 vot_reas_ag_5 vot_reas_ag_6 vot_reas_ag_7 vot_reas_ag_8, nobs minmax

/* Household income (under $10k, $10K to <$25k, $25K to <$50k, $50K to <$75k, $75K to
<$100k, $100K to <$150k, and $150K+) */ 
rename ppinc7 inc
replace inc = 10 if inc == 1
replace inc = 17.5 if inc == 2
replace inc = 37.5 if inc == 3
replace inc = 62.5 if inc == 4
replace inc = 87.5 if inc == 5
replace inc = 125 if inc == 6
replace inc = 150 if inc == 7

********************************************************************************
* Export to csv to send to R
********************************************************************************
egen id_card = group(id choice_set)
drop  vot_1 vot_reas_for_1 vot_reas_for_2 vot_reas_for_3 vot_reas_for_4 vot_reas_for_5 vot_reas_for_6 vot_reas_for_text vot_reas_ag_1 vot_reas_ag_2 vot_reas_ag_3 vot_reas_ag_4 vot_reas_ag_5 vot_reas_ag_6 vot_reas_ag_7 vot_reas_ag_8 vot_reas_ag_text reintr enc trust_1 trust_2 trust_3 trust_4 mgmnt_1 mgmnt_2 mgmnt_3 mgmnt_4 wvo_1 wvo_2 wvo_3 wvo_4 wvo_5 wvo_6 coex1_1 coex1_2 coex1_3 coex1_4 coex1_5 coex1_6 coex1_7 coex1_8 coex2_1 coex2_2 coex2_3 coex2_4 coex2_5 coex2_6 emo_1 emo_2 emo_3 emo_4 emo_5 emo_6 emo_7 ppo_1 ppo_2 ppo_3 ppo_4 ppo_5 ppo_6 pno_1 pno_2 pno_3 pno_4 pno_5 pno_6 eth_1 eth_2 eth_3 eth_4 eth_5 eth_6 sogr_1 sogr_2 sogr_3 sogr_4 sogr_5 sogr_6 sogr_7 out_1 out_11 out_2 out_4 out_12 out_9 out_10 out_5 out_6 out_7 out_8 out_13 out_8_text res incomingqualtricsid ext_st termpoint8 redirect_url q_url timebefore timeafter vers_cbconjoint revision_cbconjoint
drop state _caseid _panelaltid meanduration sdduration stduration pop200

* Standardize each variable. 
foreach var in cost pop compensation cost_sharing livestock gove hunt {
	egen m`var' = mean(`var')
	egen sd`var' = sd(`var')
	g n`var' = (`var'-m`var')/sd`var'
	drop m`var' sd`var'
}
* Population squared
g npop2 = npop^2
export delimited using "cleaned_data_for_R.csv", replace

