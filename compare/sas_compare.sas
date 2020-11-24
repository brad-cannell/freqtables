* ============================================================================;
* Comparing SAS N-way tables to Freqtables for R
* 2020-11-23
* ============================================================================;

* Create library;
libname nhanes "C:\Users\mcannell\Dropbox\R\Packages\freqtables\compare";

* Import data;
* This data was downloaded and cleaned in R and then saved to the compare folder;
proc import
	out = nhanes.demo
	datafile = "C:\Users\mcannell\Dropbox\R\Packages\freqtables\compare\demo_j_2017_2018.csv"
	dbms = csv;
run;


* ============================================================================;
* One, two, and three-way frequency tables with confidence intervals and 
* hypothesis tests;
* ============================================================================;
ods html file = "C:\Users\mcannell\Dropbox\R\Packages\freqtables\compare\sas_compare.html";

title1 "One-way frequency table";
title2 "Gender from 2017-2018 NHANES";
proc freq data = nhanes.demo;
	tables gender_f / chisq;
run;
footnote1 "Proc Freq will not calculate confidence intervals";
footnote2 "Hypothesis tests are not included by default. I requested them.";

proc surveyfreq data = nhanes.demo;
	tables gender_f / cl chisq;
run;
footnote1;

title1 "Two-way frequency table";
title2 "Gender by education from 2017-2018 NHANES";
proc freq data = nhanes.demo;
	tables gender_f * edu_f / expected chisq fisher;
run;

proc surveyfreq data = nhanes.demo;
	tables gender_f * edu_f / row cl expected chisq;
run;

title1 "Three-way frequency table";
title2 "Gender by education by language from 2017-2018 NHANES";
proc freq data = nhanes.demo;
	tables gender_f * edu_f * language_f / expected chisq fisher;
run;

proc surveyfreq data = nhanes.demo;
	tables gender_f * edu_f * language_f / row cl expected chisq;
run;

ods html close;




