global path "D:\2023\202312\数据"
/*
cd $path
import excel "DATA12.xlsx", firstrow clear
gen code = substr(证券代码,1,6)
drop 证券代码
order code

gen year = year(报告日期)
drop 报告日期
drop if 总资产 == 0

encode code, gen(id)

drop if year < 2007

save data.dta, replace
*----------------------
import excel "1号文件信息匹配：原始数据.xlsx", firstrow clear

rename 证券代码 code
drop stkcd

keep code 行业代码
keep if strmatch(行业代码, "*E*")

* E47 	房屋建筑业
* E48 	土木工程建筑业
* E49 	建筑安装业
* E50	建筑装饰、装修和其他建筑业

duplicates drop code, force

save industry.dta, replace
*----------------------
import excel "上市公司绿色全要素生产率数据（2007-2022）.xlsx", firstrow clear

drop 省属地 建筑企业

rename 证券代码 code

save gtfp.dta, replace
*----------------------
import excel "EN_EquityNatureAll.xlsx", firstrow clear
rename 证券代码 code
gen year = substr(截止日期,1,4)
gen SOE = strmatch(股权性质,"*国*")

keep code year 股权性质 SOE
destring year, replace force
save soe.dta, replace
*----------------------
import excel "全行业公司基本信息.xlsx", firstrow clear

labone, nrow(1) 
drop in 1

keep Scode Prvn
rename Scode code
rename Prvn prov

save prov.dta, replace
*----------------------
use data.dta, clear
merge m:m code using industry.dta
drop if _merge == 2
drop _merge

merge m:m code year using gtfp.dta
drop if _merge == 2
drop _merge

merge m:m code year using soe.dta
drop if _merge == 2
drop _merge

merge m:m code using prov.dta
keep if _merge == 3
drop _merge

save final.dta, replace
*----------------------
use final.dta, clear
gen ipoyear = year(上市日期)
gen preipo = (ipoyear > year)
drop if preipo == 1

gen Treat = (prov == "上海市" | prov == "北京市" | prov == "天津市" | prov == "重庆市" | prov == "湖北省" | prov == "广东省")

gen Post = (year >= 2013)
gen DID = Treat * Post

tsset id year
tab year, gen(yr)
drop yr1

save finaldata.dta, replace
*----------------
use finaldata.dta, clear

gen Size = ln(总资产)
gen ROA = 净利润/总资产
gen Lev = 资产负债率
gen Cost = 管理费用/主营业务收入
gen CF = 经营活动现金净流量/总资产
gen Capital = ln(固定资产净额/员工人数)
gen Top1 = 第1大股东持股比例
gen GTFP = 企业绿色全要素生产率
gen GreInv = ln(1 + 绿色发明申请数)
gen GreUm = ln(1 + 绿色实用新型申请数)
*/
*------------------------
*        统计部分
*------------------------
**描述性统计
asdoc sum GTFP DID Size ROA Lev Cost CF Capital Top1 GreInv GreUm, replace stats(N mean sd min median max) dec(3)
shellout using `"Myfile.doc"'

**面板参数
xtdescribe

**相关性分析
corr2docx GTFP DID Size ROA Lev Cost CF Capital Top1 using 1.docx,replace star note(Lower-triangular cells report Pearson's correlation coefficients, *** p<0.01, ** p<0.05, * p<0.1)
shellout using `"1.docx"'

*------------------------
*        模型检验
*------------------------
**注：运行[global]来对相关参数做出定义：
global y  GTFP
global x  DID
global xc Size ROA Lev Cost CF Capital Top1

**多重共线性
reg $y $x $xc
logout,save(table1) word fix(9) dec(3) replace:estat vif
shellout using `"table1.rtf"'


**豪斯曼检验
qui xtreg $y $x $xc , fe
est store fe
qui xtreg $y $x $xc , re
est store re
hausman fe re,sigmamore constant

*------------------------
*        模型部分
*------------------------
**注：运行[global]来对相关参数做出定义：
global y  GTFP
global x  DID
global xc Size ROA Lev Cost CF Capital Top1

local ad1 "addtext(Firm,yes,Year,yes) keep($x $xc) sortvar($x $xc) addstat(F value,e(F))"
local ad2 "addtext(Firm,no,Year,no) keep($x $xc) sortvar($x $xc) addstat(F value,e(F))"
xtreg $y $x yr*, fe
outreg2 using 1,word replace ctitle($y) `ad1' dec(4)
reg $y $x $xc
outreg2 using 1,word append ctitle($y) `ad2' dec(4)
xtreg $y $x $xc yr*, fe
outreg2 using 1,word append ctitle($y) `ad1' dec(4)
shellout using `"1.rtf"'

*------------------------
*        稳健性
*------------------------
**注：运行[global]来对相关参数做出定义：
global y  GTFP
global x  DID
global xc Size ROA Lev Cost CF Capital Top1



*平行趋势
gen policy = year - 2013
tab policy
replace policy = -4 if policy < -4
replace policy = 4 if policy > 4

forvalues i = 4(-1)1{
  gen pre_`i' = (policy == -`i' & Treat == 1) 
}
gen current = (policy == 0 & Treat == 1)
forvalues j = 1(1)4{
  gen  post_`j' = (policy == `j' & Treat == 1)
}

global prepost pre_3 pre_2 pre_1 current post_1 post_2 post_3


local add "addtext(Firm,yes,Year,yes) keep($prepost $xc) sortvar($prepost $xc) addstat(F value,e(F))"
xtreg $y $prepost $xc yr*, fe r
outreg2 using 1,word replace ctitle($y) `add' dec(4)
shellout using `"1.rtf"'


*作图
xtreg $y $prepost $xc yr*, fe r
coefplot, baselevels ///
keep(pre_* current post_*) ///
vertical ///
yline(0,lcolor(balck*0.8)) ///
xline(4, lwidth(vthin) lpattern(dash) lcolor(gray)) ///
ylabel(,labsize(*0.75)) xlabel(,labsize(*0.75)) ///
ytitle("Dynamic Effect of the Policy", size(small)) ///
xtitle("Period", size(small)) ///
addplot(line @b @at) ///
ciopts(lpattern(dash) recast(rcap) msize(medium)) ///
msymbol(circle_hollow) ///
scheme(s1mono)


*安慰剂
gen Post2009 = (year >= 2009)
gen Post2010 = (year >= 2010)
gen DID2009 = Post2009 * Treat
gen DID2010 = Post2010 * Treat

global x1 DID2009
global x2 DID2010

local add "addtext(Firm,yes,Year,yes) keep($x1 $x2 $xc) sortvar($x1 $x2 $xc) addstat(F value,e(F))"
xtreg $y $x1 $xc yr*, fe
outreg2 using 1,word replace ctitle($y(DID2009)) `add' dec(4)
xtreg $y $x2 $xc yr*, fe
outreg2 using 1,word append ctitle($y(DID2010)) `add' dec(4)
shellout using `"1.rtf"'



*倾向得分匹配、滞后一期解释变量
global D Treat
global x Size ROA Lev Cost CF Capital Top1

*核匹配
psmatch2 $D $x, logit ties ate common odds kernel
gen psm1 = (_support == 1)

*近邻匹配
psmatch2 $D $x, logit ate neighbor(1) common caliper(.0005) ties 
gen psm2 = (_support == 1)

*半径匹配
psmatch2 $D $x, logit radius caliper(1)
gen psm3 = (_support == 1)


sort id year
local add "addtext(Firm,yes,Year,yes) keep($x l.$x $xc) sortvar($x l.$x $xc) addstat(F value,e(F))"
xtreg $y $x $xc yr* if psm1 == 1, fe
outreg2 using 1,word replace ctitle($y(kernel)) `add' dec(4)
xtreg $y $x $xc yr* if psm2 == 1, fe
outreg2 using 1,word append ctitle($y(neighbor)) `add' dec(4)
xtreg $y $x $xc yr* if psm3 == 1, fe
outreg2 using 1,word append ctitle($y(radius)) `add' dec(4)
xtreg $y l.$x $xc yr*, fe
outreg2 using 1,word append ctitle($y) `add' dec(4)
shellout using `"1.rtf"'


*------------------------
*        机制检验
*------------------------
gen DID_Inv = DID * GreInv
gen DID_Um  = DID * GreUm

**注：运行[global]来对相关参数做出定义：
global y  GTFP
global x  DID
global m1 DID_Inv GreInv
global m2 DID_Um GreUm
global xc Size ROA Lev Cost CF Capital Top1

local add "addtext(Firm,yes,Year,yes) keep($m1 $m2 $x $xc) sortvar($m1 $m2 $x $xc) addstat(F value,e(F))"
xtreg $y $m1 $x $xc yr*, fe
outreg2 using 1,word replace ctitle($y) `add' dec(4)
xtreg $y $m2 $x $xc yr*, fe
outreg2 using 1,word append ctitle($y) `add' dec(4)
shellout using `"1.rtf"'


*------------------------
*        异质性检验
*------------------------
egen sizegrouppctile = pctile(Size), p(50)
gen sizegroup = (Size > sizegrouppctile)

egen levgrouppctile = pctile(Lev), p(50)
gen levgroup = (Lev > levgrouppctile)


***东部1中部2西部3
gen 省份 = prov
gen group = 1
replace group = 2 if 省份=="吉林省" | 省份=="黑龙江省" | 省份=="山西省" | 省份=="江西省" | 省份=="河南省" | 省份=="安徽省" | 省份=="湖北省" | 省份=="湖南省"
replace group = 3 if 省份=="内蒙" | 省份=="广西" | 省份=="四川省" | 省份=="贵州省" | 省份=="云南" | 省份=="陕西省" | 省份=="甘肃" | 省份=="青海省" | 省份=="宁夏" | 省份=="新疆维吾尔自治区" | 省份=="重庆市"

gen govgroup = (政府补助 > 0)


**注：运行[global]来对相关参数做出定义：
global y  GTFP
global x  DID
global xc Size ROA Lev Cost CF Capital Top1

local add "addtext(Firm,yes,Year,yes) keep($x $xc) sortvar($x $xc) addstat(F value,e(F))"
xtreg $y $x $xc yr* if sizegroup == 1, fe
outreg2 using 1,word replace ctitle($y(BigSize)) `add' dec(4)
xtreg $y $x $xc yr* if sizegroup == 0, fe
outreg2 using 1,word append ctitle($y(SmallSize)) `add' dec(4)
xtreg $y $x $xc yr* if SOE == 1, fe
outreg2 using 1,word append ctitle($y(State)) `add' dec(4)
xtreg $y $x $xc yr* if SOE == 0, fe
outreg2 using 1,word append ctitle($y(Private)) `add' dec(4)
xtreg $y $x $xc yr* if levgroup == 1, fe
outreg2 using 1,word append ctitle($y(HighLev)) `add' dec(4)
xtreg $y $x $xc yr* if levgroup == 0, fe
outreg2 using 1,word append ctitle($y(LowLev)) `add' dec(4)
shellout using `"1.rtf"'


**注：运行[global]来对相关参数做出定义：
global y  GTFP
global x  DID
global xc Size ROA Lev Cost CF Capital Top1

local add "addtext(Firm,yes,Year,yes) keep($x $xc) sortvar($x $xc) addstat(F value,e(F))"
xtreg $y $x $xc yr* if group == 1, fe
outreg2 using 1,word replace ctitle($y(East)) `add' dec(4)
xtreg $y $x $xc yr* if group == 2, fe
outreg2 using 1,word append ctitle($y(Middle)) `add' dec(4)
reg $y $x $xc yr* id* if group == 3
outreg2 using 1,word append ctitle($y(West)) `add' dec(4)
xtreg $y $x $xc yr* if govgroup == 1, fe
outreg2 using 1,word append ctitle($y(YES)) `add' dec(4)
xtreg $y $x $xc yr* if govgroup == 0, fe
outreg2 using 1,word append ctitle($y(NO)) `add' dec(4)
shellout using `"1.rtf"'

