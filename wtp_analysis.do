* This file creates Table 5

cd "C:\Users\jbb\OneDrive - Colostate\Wolf_CV\final\analysis\Final_Code_Data"* Get other variables 
* import WTP vector and save as stata file. 
import delimited "wtp_with_weights.csv"
drop v1
sort id
save "wtp_with_weights.dta", replace

import delimited "raw_data.csv", clear
drop c1-c8 fa19464f_78a8_4146_851e_35d626e8 a86984634_9697_49ee_ab3d_ef32dca a23f855c1_ea05_4b69_9e5a_58e4615 e5b17b45_3ff6_4fe7_9f0a_ecd3015f a64fdd57b_148f_4bc2_93c6_d6fbb73 a0a9f0e42_7887_4964_9964_55549be fcafb975_d9a9_4d59_84e6_89c82d15 v151 v152 v153 v154 v155 v156 v157 v158 v159 v160 v161 v162 v163 v164 v165 v166 v167 v168 v169 v170 v171 v172 v173 v174 v175 v176 v177 v178 v179 v180 v181 v182 v183 v184 v185 v186 v187 v188 v189 v190 v191 v192 v193 v194 v195 v196 v197 v198 v199 v200 v201 v202 v203 v204 v205 v206 v207 v208 v209 v210 v211 v212 v213 v214 v215 v216 v217 v218 v219 v220 v221 v222 v223 v224 v225 v226 v227 v228 v229 v230 v231 v232 v233 v234 v235 v236 v237 v238 v239 v240 v241 v242 v243 v244 v245 v246 v247 v248 v249 v250 v251 v252 v253 v254 v255 v256 v257 v258 v259 v260 v261 v262 v263 v264 v265 v266 v267 v268 v269 v270 v271 v272 v273 v274 v275 v276 v277 v278 v279 v280 v281 v282 v283 v284 v285 v286 v287 v288 v289 v290 v291 v292 v293 v294 v295 v296 v297 v298 v299 v300 v301 v302 v303 v304 v305 v306 v307 v308 v309 v310 v311
sort responseid
merge 1:m responseid using "choices.dta"
drop _m 

g asc = 1 
replace asc = 0 if pop == 10
g pop2 = pop^2

label variable pop "Population"
label variable pop2 "Population Sq"
label variable compensation "Compensation"
label variable cost_sharing "Cost Sharing"
label variable livestock "Livestock"
label variable gove "Gov Lethal"
label variable hunt "Hunt"
label variable cost "Cost"
label variable asc "ASC"
egen id = group(responseid)
order id

merge m:1 id using "wtp_with_weights.dta"
drop _m 
rename wtp wtp_weight

drop npop_coeff npop2_coeff ncost_coeff
save "dataset_with_wtp.dta", replace

g voty = vot
replace voty = 0 if vot > 1
* regions are co_reg3 1 = front range, 2 east plains, 3 western slope
g front_range = 0
replace front_range = 1 if co_reg3 == 1
* ppmsacat is metro area = 1
* sogr_4 = environmentalist 
* sogr_5 = conservationist
* sogr_6 = farmer/rancher
* out_1 == hiking
* out_6 = hunting
* out_9 = ATV riding

label variable voty "1(Voted Yes)"
g democrat = 0 
replace democrat = 1 if xparty4 == 2
g independent = 0 
replace independent = 1 if xparty4 == 3 
label variable democrat "1(Democrat)"
label variable independent "1(Independent)"
label variable front_range "1(Front Range)"
forvalues x = 1/5 {
	g enc`x' = (enc==`x')
	label variable enc`x' "Encounter `x'"
}
label variable reintr "Reintroduction Support"
g ppage2 = ppage^2
label variable ppage "Age"
label variable ppage2 "Age Squared"
g ppinc72 = ppinc7^2
label variable ppinc7 "Income"
label variable ppinc72 "Income Squared"

reg wtp_weight voty [pweight = weight] if enc >0 & reintr >0 , cluster(id)
outreg2 using "wtp_analysis", replace label 
reg wtp_weight democrat independent [pweight = weight] if enc >0 & reintr >0 , cluster(id)
outreg2 using "wtp_analysis",  label 
reg wtp_weight front_range [pweight = weight] if enc >0 & reintr >0 , cluster(id)
outreg2 using "wtp_analysis",  label 
reg wtp_weight enc2 enc3 enc4 enc5 [pweight = weight] if enc >0 & reintr >0 , cluster(id)
outreg2 using "wtp_analysis",  label 
reg wtp_weight reintr [pweight = weight] if enc >0 & reintr >0 , cluster(id)
outreg2 using "wtp_analysis",  label 
reg wtp_weight ppage ppage2 [pweight = weight] if enc >0 & reintr >0 , cluster(id)
outreg2 using "wtp_analysis",  label 
reg wtp_weight ppinc7 ppinc72 [pweight = weight] if enc >0 & reintr >0 , cluster(id)
outreg2 using "wtp_analysis",  label 
reg wtp_weight voty democrat independent front_range enc2 enc3 enc4 enc5 reintr ppage ppage2 ppinc7 ppinc72  [pweight = weight] if enc >0 & reintr >0 , cluster(id)
outreg2 using "wtp_analysis", label 

