#eHEART.R README

#The eHEART.R function applies the models calculated within CPRD to a new dataset of primary care records to estimate a 10-year CVD risk.

Package dependencies
*dplyr
*tidyr
*nlme

Function dependencies
*mixoutsamp - mixed model out of sample predictions

Files required
*HR_10year.csv - eHeart hazard ratios by age and sex
“mixed_model_output.rdata” - mixed model linear model output

Data and function arguments
The function expects the input data in long format with a unique variable name for: 
	*Patient ID (id.name) - numerical
	*Sex (sex.name) - numerical
	*Date of birth (date.of.birth.name) - date format %Y-%m-%d
	*Date of exposure (exposure.date.name) - date format %Y-%m-%d
	*Risk factor (exposure.name) - string 
	*Risk factor measurement (exposure.value.name) - numerical
	*Date of formal assessment (baseline.date.name) - string or date with format %Y-%m-%d - Date of formal assessment can be either existing variable or a user-entered date e.g. 		“2020-12-23”.

Within exposure.name variable, the function expects user defined strings for the risk factors:
	*sbp.risk.factor - string - e.g. “sbp”
	*tchol.risk.factor - string
	*hdl.risk.factor - string
	*anti_hypt.risk.factor - string
	*statins.risk.factor - string
	*smoking.risk.factor - string
	*diabetes.risk.factor - string

Additional arguments can specify the values (exposure.value.name) expected for binary variables. The default values provided is:
	*sex_men = 1
	*sex_women = 2
	*anti_hypt_neg = 0
	*anti_hypt_pos = 1
	*statins_neg = 0
	*statins_pos = 1
	*smoking_neg = 0
	*smoking_pos = 1
	*diabetes_neg = 0
	*diabetes_pos = 1

Units for continuous risk factors (exposure.value.name) expected are as follows:
	*Systolic blood pressure: mmHg
	*Total cholesterol: mmol/L
	*HDL cholesterol: mmol/L


Example data
id	sex	date.of.birth		exposure.date	exposure.name	exposure.value  
1	1	01-01-1985		2020-01-10		“sbp”			150
1	1	01-01-1985		2020-01-10		“tchol”			4.5
1	1	01-01-1985		2020-03-12		“anti_hypt”		1
2	2	31-03-1965		2005-05-22		“diabetes”		1
2	2	31-03-1965		2005-07-11		“tchol”			5.7
3	1	08-08-1972		2006-11-11		“hdl”			4.8			



Function overview:

*Check that all variables exist in dataset
*Check that dates are in correct format
*Check that provided values for blood pressure and cholesterol are in correct units
*Remove outliers (Systolic blood pressure <60 or >250 mmHg, total cholesterol <1.75 or >20 mmol/L and HDL cholesterol <0.3 or >3.1 mmol/L)
*Indicator variables for blood pressure medication and statins created
*Clean data and import mixed model outputs for mixed model predictions using mixoutsamp function
*Import previously calculated eHeart hazard ratios (from CPRD) and calculate 10-year risks. 
*Output list of id, sex, baseline age, risk and last observed blood pressure medication and statins





