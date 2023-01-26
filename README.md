# The Association Between Colleges’ Median Earnings with Instructional Expenses and Control Type of Colleges
###### Author: Avianna Bui

## Introduction

### Introduction to Topic
The prospect of receiving a high-income job after graduation is one of the most commonly cited reasons for college attendance. However, the financial return of colleges is dependent on a variety of factors, including both college-based factors such as their employability reputation to potential employers and individual-based factors like the majors students decide to pursue. Since colleges’ return on investment can have a cumulative impact on an individual’s lifetime financial prospects, I am inclined to examine the factors that could potentially influence the earnings that college graduates receive

### Research Questions
For my first question, I explore the relationship between instructional expenditures per full-time student and the median earnings of college graduates 6 years after their entry. In creating this model, I adjust for the effect of the degree type the institution primarily awards, which functions as a confounder since degree type places limitations on jobs applicants are qualified for while impacting instructional costs due to the different length of time and facilities needed to finish different degrees. The first model also accounts for average family income as a precision variable. 

My second question involves whether a private college education can be associated with greater odds of gaining above-average income after college. For this question, I divide the type of institution into public and private higher education institutions, then compare the odds of those colleges having above-average median earnings. I also include average family earnings in this model as a confounder because students from higher-income families would have a higher chance of going to private colleges and better access to high-income jobs than those from low-income backgrounds. 

## Data Context
The College Scorecard data records among 3,676 U.S. colleges a total of 93 characteristics of each institution or a department within the institution, ranging from their location, racial and ethnic representation in the institution, to the percentage of degrees given in each department. Collected by the U.S. Department of Education, the Scorecard is designed to assist prospective college students in comparing the values of different higher education institutions in order to select a college suitable for their economic background as well as educational and professional goals. 

This data set uses a convenience sample: it involves a list of colleges that are already recorded by the Department of Education. The project includes Integrated Postsecondary Education Data System (IPEDS) institutions that take part in Title IV programs, as well as institutions that are not Title IV participants but share similar characteristics with Title IV-participating institutions. While selecting institutions to report in the data set, the Scorecard also excludes colleges without a valid OPEID or an appropriate institutional category. In selecting institutions that are not Title IV members, the data set also eliminates those without a valid OPEID, and without reported data to IPEDS on degree completions or enrollment

I use 5 variables in total for my analysis project, with colleges’ median salary 6 years post-entry, instructional expenditures per student, and colleges’ average family income as quantitative variables. All quantitative variables are recorded in US dollars. Among these, the median salary of colleges ranges from $9,200 to $120,400, whereas their instructional costs range from $257 to $161,644 per student. The average family earnings variable has the widest range, between $5583 to $145,228. For categorical variables, I use the type of degree the institution primary awards, (including certificate, associate, and bachelor’s degree) in the first model, and the control type of institution (public or private college) in the second model. 

The data set was last updated in June 2020. To further examine the data context, such as the calculation method of certain variables or the year a variable is recorded, a Data Dictionary is provided at https://drive.google.com/file/d/1pxQjf-n0LZvCEllIJGpl2FBGQUqDxaIs/view.

## Limitations
The data sample excludes many institutions that do not participate in Title IV, so the Scorecard might not be representative of the larger population of interest, which is U.S. higher education institutions. Instead, the interpreted result could only be generalized to apply to Title IV participants. In addition, the current Scorecard is merged from data files collected in various years, so many variables in the data set are not recorded at the same year period. With the variables I use in the first question, for instance, the instructional expenditures per student numbers are from 2018 - 2019, whereas the median income is from 2014 - 2015 though adjusted to 2017 dollars to account for inflation. Since this is the median earnings 6 years after college entry, the data would mainly record the income of students who graduated in 2012 - 2013, causing a 6-year difference in time compared to the instructional expenditures variable. A similar problem occurs in my second analysis, in which I use the income data in 2014-15, but compared it to the average graduation salary in 2017 to determine the above-average earning metric since the median salary variable is adjusted to 2017 dollars. Such time gaps impact the validity and reliability of the analyses

Furthermore, because the Scorecard is collected on a college level rather than individual-based, students and families should be aware that a great college data-wise might not be the most suitable for each student’s individual needs. For instance, a college with impressive career prospects could have a highly rigorous and demanding curriculum, which can potentially lead to stress and anxiety among students who prefer a more relaxing, progress-based mode of instruction. As a result, prospective college students should not solely rely on the Scorecard to decide their college future, but they should also gather information from additional sources such as the college’s current students or the admission offices, etc. 

## Appendix
Technical Documentation: College Scorecard Institution-Level Data. (2021, July). U.S. Department of Education.
    
    https://collegescorecard.ed.gov/assets/FullDataDocumentation.pdf 
    
Great Expectations: Salaries for 2017 College Grads Hit All-Time High, Korn Ferry Analysis Shows. (2017, May 19). Korn Ferry 

    https://www.kornferry.com/about-us/press/great-expectations-salaries-for-2017-college-grads-hit-all-time-high-korn-ferry-analysis-shows 
