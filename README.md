# Overview #
The [Commonwealth Human Rights Initiative (CHRI)](http://www.humanrightsinitiative.org/), founded in 1987, is an independent, non-partisan, international non-governmental organisation, mandated to ensure the practical realisation of human rights in the countries of the Commonwealth.

[National Legal Services Authority (NALSA)](http://nalsa.gov.in/) was established in India in 1987 to provide free Legal Services to the weaker sections of the society. NALSA works in close coordination with the various State Legal Services Authorities, District Legal Services Authorities and other agencies.

This project looks at NALSA-specific data collected by CHRI for the period 1st July 2015 till 30th June 2016. Data is available at district level. The data relates to how well three NALSA schemes have been implemented on the ground. These three schemes are as follows:

1. National Legal Services Authority (Free and Competent Legal Services) Regulations, 2010
2. National Legal Services Authority (Legal Aid Clinics) Regulations, 2011
    * SLSAs on Legal Aid Clinics in Jails
    * NALSA's scheme for Para-legal Volunteers
3. National Legal Service Authority's Model Scheme for Legal Aid Counsel In All Courts of Magistrates


# Structure #
Files in this project are organized in the following manner:

* `data`: Data that in its original and processed forms. 
* `docs`: Documents that help us to understand the context and the data.
* `scripts`: Scripts that automate many aspects of data analysis.

**Due to the confidential nature of data, `data` and `docs` are not part of this repository. They are shared privately to concerned folks.**


# Usage #
Enter `scripts` folder, source the R script named `main.R` and call `main()` function.


# Developer Notes #
* Cleaning is done right after data is read from file. For example, for NALSA 2010 data, study the function `read.Nalsa2010.xlsx()` in file `scripts/main.R`.
* Search for tag `TODO` in source code to identify areas of improvement.
