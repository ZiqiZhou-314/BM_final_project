Full report
================
Group 20 - James Ng, Jennifer Tang, Qimin Zhang, Ziqi Zhou
12/14/2019

## Abstract

There are many factors that contribute to the salary of physicians in
medical colleges. In this study, a linear model with mean salaries of
1994 and 1995 as response and gender as main factor is fit to a lawsuit
dataset to determine whether Houston College of Medicine has engaged in
a pattern and practice of discrimination against women in giving
promotions and setting salaries. Department, primary emphasis,
certification, publication rate and experience were determined to be
confounders, while rank was found to be an interaction. Gender
demonstrates no association with salary from 1994 and 1995 after
adjusting for department affliation, clinical emphasis, board
certification status, rank, and experience, which means the data does
not support the claim of gender discrimination in setting salaries at
Houston College of Medicine.

## Introduction

Women being paid less than men is nothing new. In fact, this has been
the norm ever since women started entering the workforce. In order to
address such inequality, the Equal Pay Act and the Civil Rights Act were
both signed into law in 1963 and 1964, respectively, with the purpose of
prohibiting wage disparity based on sex and discrimination against
employees based on sex, race, national origin, color and religion
(Crampton et al, 1997). Despite the efforts to eliminate gender based
wage gap, almost half a century later, a woman working full-time still
only earns an average of 81.2 cents for every dollar a man working
full-time earns in the U.S as of 2018 (United States Census Bureau,
2019).

This issue is not only prevalent in the US, but also happens to
transcends to other countries spanning within and between various
occupations as well. One particular field of interest is in academic
medicine. In the age of modern medicine, the diversity of medical
practitioners has progressed substantially with more than one-third of
physicians being women in a historically male dominated field, however
compensation inequality still remains to be an issue that seeks
improvement (Butkus, 2018). It was reported in a study conducted in
2016, that female physicians makes 90 cents for every dollar that a male
physician makes in academic medicine (Freund et al, 2016). In addition
to the wage disparity between male and female physicians, the lack of
advancement in a female physician’s career, despite the growing number
of women entering the medical field, remains significant (Butkus, 2018).
Many factors have been considered to contribute to such disparities such
as speciality choice, hours worked, years of experience, and publication
counts.

These differences in wage and lack of promotions for female physicians
can only be perceived as unfair and will eventually lead to a lawsuit. A
few years ago, all the female physicians at Houston College of Medicine
filed a lawsuit against the college for violating the Civil Rights Act
of 1964 by engaging in consistent patterns and practice of
discrimination against women faculty members in giving promotions and
setting salaries. To address this situation, this paper explores the
data set that the female faculty members presented of the different
faculty positions held and salaries earned by the male and female
physicians at Houston College of Medicine. Additional factors such as
department affiliation, gender, board certification status, clinical or
research emphasis, publication rate, and years of experience as a
practicing physician, were also provided and examined. The goal is to
find the associations that could support the claim of gender
discrimination in the salary determination at this university hospital.

# Methods

The dataset contains 7 factors that are potentially associated with the
salaries of the Houston College of Medicine faculty. These are
department, gender, primary emphasis (clinical or research),
certification (board certified or not certified), publication rate
(publications on CV/ years between CV date and MD date), years since
obtaining MD and rank (supplemental table 1. The mean salary for the
1994 academic year and the salary for the 1995 academic year (salary
after increment to the 1994 income) are the outcomes. Histograms were
were made to visualize the distribution of the outcomes. The histograms
show right skewness so log transformations of the outcomes were carried
out (supplemental figure 1). Since gender was the main variable of
interest, models were analyzed in R studio (RStudio Team, 2015) to test
for associations between gender and salary.

A simple linear regression model was developed using mean salaries for
the 2 years provided and gender. Using this model as the reference
point, other variables in the dataset were analyzed as potential
confounders or interactions. Model selection was made using
criterion-based procedures. Once the final model was determined
(supplemental table 2), residuals vs fitted values plot,
quantile-quantile plot, scale-location plot and residuals vs leverage
plots were used to diagnose the model (supplemental figure 2) and anova
was used to determine significance (Supplemental table 3).

# Results

Several of the other variables (department, primary emphasis,
certification, publication rate and experience) were determined to be
confounders, while rank was found to be an interaction. High
collinearity was found between publication rate and primary emphasis as
well as publication and department, so publication rate was dropped from
the model. The final model shows that gender is not significant in
determining salary (p-value 0.186) when controlling for department,
primary emphasis, certification, experience and rank. Associated p
values for other variables are also provided. It is interesting to note
that rank, overall as a variable is significantly associated with
salary. However, when examining the individual levels, only full-time
professors were significant. The model shows an adjusted \(R^2\) value
of .9322, meaning that 93% of the variability of the data is represented
by the model. Cook’s distance, quantile-quantile plot, and residuals
plots show that the 184th observation is an influential point
(supplemental figure 2). The influencial point was kept in the model
since the goal was to build a model for association.

# Conclusion/Discussion

Based on the data set, gender demonstrates no association with salary
from 1994 and 1995 after adjusting for department affliation, clinical
emphasis, board certification status, rank, and experience. Therefore,
we conclude that the data does not support the claim of gender
discrimination in setting salaries at Houston College of Medicine.
Moreover, department, clinical or research emphasis, board certified or
not, and years of experience confound the association between salaries
and gender. It’s important to note that rank interacts with gender,
meaning that salary among male and female physicians differ depending on
the position held. The difference in salaries between male and female
physicians at the Houston College of Medicine could be due to other
factors, particularly rank since the claim on the failure in giving
promotions to female faculty member was also in the lawsuit.

Under the assumption that Houston College of Medicine, like most
universities, operates under a tenure system, where salary is based on
academic productivity, women in Houston College would get paid less
based on the unproportional distribution of male full time professors
compared to female full time professors, seen in (Supplemental table 1)
(Reed et al, 2011). Other studies have also shown that differences in
salaries between male and female physicians may be due to characteristic
differences that are affliated with gender roles such as specialty
choice, number of hours worked, practice setting, and work-life balance
(Baker, 1996 and Butkus et al, 2018).

Futhermore, it was determined that physician 184 is an influential point
based on the standardized residuals. Physician 184 is a male uncertified
assistant professor in the department of medicine, with a research
emphasis, a 5.1 publication rate, and has 2 years experience since
obtaining MD. With all things considered, physician 184 has an unusually
high salary. This could be due to other factors that were not included
in this data set such as the importance of research and publication this
particular physician was involved in or the number of hours he works.
