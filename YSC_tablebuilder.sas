libname canberra '/folders/myfolders/Canberra Workshop';

FILENAME REFFILE '/folders/myfolders/Canberra Workshop/sa4datafilled.csv';

*Import the raw tablebuilder csv file;

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=canberra.sa4data2;
	GETNAMES=YES;

data d1;
	counter = 0;
	state = '   ';
	statecode = 0;
	sa4_code = 0;
	l_sa4 = 'No Usual Addre';
	do i = 1 to 457600;
		set canberra.sa4data2;
		counter = counter+1;
*Check if got to last SA4 in State so time to move to next State;
		if sa4 ne l_sa4 and l_sa4 = 'No Usual Addre' then do;
			statecode = statecode+1;
		end;
		if statecode = 1 then state = 'NSW';
		if statecode = 2 then state = 'VIC';
		if statecode = 3 then state = 'QLD';
		if statecode = 4 then state = 'SA';
		if statecode = 5 then state = 'WA';
		if statecode = 6 then state = 'TAS';
		if statecode = 7 then state = 'NT';
		if statecode = 8 then state = 'ACT';
*Create a unique code for each SA4 by counting every 4400 records;
		if counter = 1 then do;
			sa4_code = sa4_code+1;
		end;
*Recode Indigenous into three categories;
		if indig = 'Aboriginal' then indig = 'Yes Indigenous';
		if indig = 'Torres Strait' then indig = 'Yes Indigenous';
		if indig = 'Both Aborigina' then indig = 'Yes Indigenous';
*Recode education into degree yes/no;
		if educ = 'Postgraduate Degree Level' then educ = 'Degree = Yes';
		if educ = 'Graduate Diploma and Graduate Certificate Level' then educ = 'Degree = Yes';
		if educ = 'Bachelor Degree Level' then educ = 'Degree = Yes';
		if educ = 'Advanced Diploma and Diploma Level' then educ = 'Degree = No';
		if educ = 'Certificate III & IV Level' then educ = 'Degree = No';
		if educ = 'Secondary Education - Years 10 and above' then educ = 'Degree = No';
		if educ = 'Certificate I & II Level' then educ = 'Degree = No';
		if educ = 'Secondary Education - Years 9 and below' then educ = 'Degree = No';
		if educ = 'Supplementary Codes' then educ = 'Degree = No';
		if educ = 'Not stated' then educ = 'Degree = No';
		if educ = 'Not applicable' then educ = 'Degree = No';
*Recode LFS to employed and unemployed;
		if lfs_status = 'Employed, worked full-time' then lfs_status = 'employed';
		if lfs_status = 'Employed, worked part-time' then lfs_status = 'employed';		
		if lfs_status = 'Employed, away from work' then lfs_status = 'employed';
		if lfs_status = 'Unemployed, looking for full-time work' then lfs_status = 'unemployed';		
		if lfs_status = 'Unemployed, looking for part-time work' then lfs_status = 'unemployed';				
*Drop zero cells and the 'funny' SA4 for shipping;
		if count > 0 and sa4 ne 'Migratory - Of' then output;
		l_sa4 = sa4;
		if counter = 4400 then counter = 0;
	end;
	keep state sa4 sa4_code age sex indig educ lfs_status count;

*Collapse data for standard model;
proc means data=d1 noprint;
	class state age sex indig educ lfs_status;
	var count;
	output out=d2 sum=;

data model_data;
	set d2;
*Picks the full cross-classification of the class variables;
	if _type_=63;
*Add a 1/0 outcome for the poisson model;
	y = 0;
	if lfs_status = 'unemployed' then y = 1;
	drop _type_ _freq_;

*Standard logistic model with lines for success and failure with count for freq weighting;
proc logistic data = model_data descending;
	freq count;
	class age sex indig educ state / param=reference ref=first;
	model lfs_status = age sex indig educ state indig*educ age*sex / lackfit outroc=temp aggregate=(age sex indig educ state) scale=none;
	effectplot interaction (x=age sliceby=sex);
	effectplot interaction (x=indig sliceby=educ);	
	
*Poisson comparison;	
proc genmod data=model_data;
	freq count;
	class age sex indig educ state / param=reference ref=first;
	model y = age sex indig educ state indig*educ age*sex / dist=poisson link=log;

*Create data at SA4 level;
proc means data=d1 noprint;
	class state sa4_code age sex indig educ lfs_status;
	var count;
	output out=d2 sum=;

data model_data_ml;
	set d2;
*Picks the full cross-classification of the class variables;
	if _type_=127;
	y = 0;
	if lfs_status = 'unemployed' then y = 1;
	drop _type_ _freq_;

*Confirm gives same results for standard logistic;
proc logistic data = model_data_ml descending;
	freq count;
	class age sex indig educ state / param=reference ref=first;
	model lfs_status = age sex indig educ state indig*educ age*sex / lackfit outroc=temp aggregate=(age sex indig educ state) scale=none;
	
*Now do the GLMM logistic...;
proc glimmix data=model_data_ml method=laplace;
	freq count;
    class age sex indig educ state sa4_code / ref=first;
    model y = age sex indig educ state indig*educ age*sex / dist=binomial link=logit solution;
    random intercept / subject=sa4_code;
run;



