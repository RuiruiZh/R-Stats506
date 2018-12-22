/* import data */
proc import datafile = '/afs/umich.edu/user/r/u/ruiruiz/Stats506/ps4/ps4_q3/Medicare_Provider_Utilization_and_Payment_Data__Physician_and_Other_Supplier_PUF_CY2016.csv' 
out = MPUP;


/* print the first 5 rows for a few variables */
proc print data = MPUP(obs = 5);
run;
		
/* Q3b
reduce the dataset to rows with 
"MRI" in the "hcpcs_description" filed and  
"hcpcs_code" starts with a 7.
*/
DATA MPUP_MRI7;
	SET MPUP;
	IF FINDW(HCPCS_DESCRIPTION, 'MRI');
	ID_TEMP = SUBSTR(LEFT(HCPCS_CODE),1,1);
	PUT ID_TEMP;
	IF ID_TEMP = 7;
	DROP ID_TEMP;
	RUN;
	
PROC PRINT DATA = mpup_MRI7;
RUN;

/* Q3c
Determine the MRI procedures with 
 1. highest volume;  
 2. highest total payment;
 3. highest average payment 
among the procedures represented here
*/
	
proc sort data = mpup_mri7;
by descending Number_of_Services;
run;

/* calculate total payment per record */
DATA MPUP_MRI7_NEW;
SET mpup_mri7;
TOTAL_PAYMENT = AVERAGE_MEDICARE_PAYMENT_AMOUNT * NUMBER_OF_SERVICES;

/* SORT BY HOCPCS_CODE */
PROC SORT data= MPUP_MRI7_NEW;
BY HCPCS_CODE;
RUN; 
/* calculte total payment & number of service by hcpcs_code */
PROC SUMMARY DATA = MPUP_MRI7_NEW;
BY HCPCS_CODE;
OUTPUT OUT = MPUP_MRI7_SUMMARY
	SUM(Number_of_Services) = TOTAL_N_SERVICES
	SUM(TOTAL_PAYMENT) = TOTAL_PAYMENT;

DATA MPUP_MRI7_SUMMARY;
	SET mpup_mri7_summary;
	AVG_PAYMENT = TOTAL_PAYMENT / TOTAL_N_SERVICES;
	PUT AVG_PAYMENT ;
	KEEP HCPCS_CODE TOTAL_N_SERVICES TOTAL_PAYMENT AVG_PAYMENT;



PROC PRINT DATA = MPUP_MRI7_SUMMARY;

/* sort by volumn */
proc sort data = MPUP_MRI7_SUMMARY OUT = MPUP_MRI7_SUMMARY_N;
by descending TOTAL_N_SERVICES;

/* sort by total payment */
proc sort data = MPUP_MRI7_SUMMARY OUT = MPUP_MRI7_SUMMARY_TOTPAY;
by descending TOTAL_PAYMENT;

/* sort by average payment */
proc sort data = MPUP_MRI7_SUMMARY OUT = MPUP_MRI7_SUMMARY_AVGPAY;
by descending AVG_PAYMENT;
run;

/* export as csv */
PROC EXPORT DATA = MPUP_MRI7_SUMMARY_N 
OUTFILE= '/afs/umich.edu/user/r/u/ruiruiz/Stats506/ps4/ps4_q3/ps4_q3c.CSV';
RUN;


/* part d: repeat part b-c using PROC SQL */
/* select MRI and code start with 7 */
proc sql;
	create table SQL_MPUP_mri7 as
	SELECT HCPCS_CODE, Number_of_Services, AVERAGE_MEDICARE_PAYMENT_AMOUNT 
	FROM MPUP
	WHERE HCPCS_DESCRIPTION CONTAINS 'MRI' AND 
			HCPCS_CODE >= 70000 AND 
			HCPCS_CODE < 80000;
QUIT;

/* calculate total payment per entry*/
DATA SQL_MPUP_NEW;
	SET SQL_MPUP_mri7;
	TOTAL_PAYMENT = Number_of_Services * AVERAGE_MEDICARE_PAYMENT_AMOUNT;
	PUT TOTAL_PAYMENT;
	RUN;

/* create summary table: N, total Payment, avg payment grouped by HCPCS_CODE */			
proc sql;
	create table SQL_MPUP_SUMMARY as

	SELECT HCPCS_CODE, TOTAL_N_SERVICES, TOTAL_PAYMENT, TOTAL_PAYMENT / TOTAL_N_SERVICES AS AVG_PAYMENT
	FROM (	SELECT HCPCS_CODE, SUM(Number_of_Services) AS TOTAL_N_SERVICES, SUM(TOTAL_PAYMENT) AS TOTAL_PAYMENT
			FROM SQL_MPUP_NEW
			GROUP BY HCPCS_CODE)
	ORDER BY TOTAL_N_SERVICES DESC;
QUIT;

/* EXPORT TO CSV */
PROC EXPORT DATA = SQL_MPUP_SUMMARY 
OUTFILE= '/afs/umich.edu/user/r/u/ruiruiz/Stats506/ps4/ps4_q3/ps4_q3d.CSV';
RUN;
