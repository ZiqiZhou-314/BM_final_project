Supplemental
================
Group 20 - James Ng, Jennifer Tang, Qimin Zhang, Ziqi Zhou
12/14/2019

## Supplemental Figure 1

![](supplemental_files/figure-gfm/distribution%20of%20salary-1.png)<!-- -->
**Supplemental Figure 1 -** Distribution of salary for 1994(a), 1995(b),
and the average of both academic years(c) is right skewed. Log
Transformation of 1994(d), 1995(e), and averaged salary(f) shows a
normal distribution.

## Supplemental Figure 2

![](supplemental_files/figure-gfm/final%20model-1.png)<!-- -->
**Supplemental figure 2 -** Residuals vs. Fitted (a), quantile-quantile
plot(b), scale location plot(c) and Cook’s distance plot(d) all show the
184th observation is an outlier and potential influential point

## Supplemental Table 1

|                                | Females (N=106)                 | Males (N=155)                    |
| ------------------------------ | :------------------------------ | :------------------------------- |
| **dept**                       |                                 |                                  |
| Biochemistry/Molecular Biology | 20 (18.9%)                      | 30 (19.4%)                       |
| Physiology                     | 20 (18.9%)                      | 20 (12.9%)                       |
| Genetics                       | 11 (10.4%)                      | 10 (6.5%)                        |
| Pediatrics                     | 20 (18.9%)                      | 10 (6.5%)                        |
| Medicine                       | 30 (28.3%)                      | 50 (32.3%)                       |
| Surgery                        | 5 (4.7%)                        | 35 (22.6%)                       |
| **clin**                       |                                 |                                  |
| Primarily research emphasis    | 46 (43.4%)                      | 55 (35.5%)                       |
| Primarily clinical emphasis    | 60 (56.6%)                      | 100 (64.5%)                      |
| **cert**                       |                                 |                                  |
| Not Certified                  | 36 (34.0%)                      | 37 (23.9%)                       |
| Board Certified                | 70 (66.0%)                      | 118 (76.1%)                      |
| **Prate**                      |                                 |                                  |
| Mean (SD)                      | 5.35 (1.89)                     | 4.65 (1.94)                      |
| Median (IQR)                   | 5.25 (3.73, 7.27)               | 4.00 (3.10, 6.70)                |
| **Exper**                      |                                 |                                  |
| Mean (SD)                      | 7.49 (4.17)                     | 12.10 (6.70)                     |
| Median (IQR)                   | 7.00 (5.00, 10.00)              | 10.00 (7.00, 15.00)              |
| **rank**                       |                                 |                                  |
| Assistant                      | 69 (65.1%)                      | 43 (27.7%)                       |
| Associate                      | 21 (19.8%)                      | 43 (27.7%)                       |
| Full professor                 | 16 (15.1%)                      | 69 (44.5%)                       |
| **Sal94**                      |                                 |                                  |
| Mean (SD)                      | 118871.27 (56168.01)            | 177338.76 (85930.54)             |
| Median (IQR)                   | 108457.00 (75774.50, 143096.00) | 155006.00 (109687.00, 231501.50) |
| **Sal95**                      |                                 |                                  |
| Mean (SD)                      | 130876.92 (62034.51)            | 194914.09 (94902.73)             |
| Median (IQR)                   | 119135.00 (82345.25, 154170.50) | 170967.00 (119952.50, 257163.00) |

**Supplement Table 1 -** General descriptive statistics about the data
set.

## Supplemental Table 2

| term                            |    estimate | std.error |  statistic |   p.value |
| :------------------------------ | ----------: | --------: | ---------: | --------: |
| (Intercept)                     |  10.9593348 | 0.0279356 | 392.307104 | 0.0000000 |
| deptPhysiology                  | \-0.1755436 | 0.0288714 | \-6.080183 | 0.0000000 |
| deptGenetics                    |   0.1845717 | 0.0362059 |   5.097829 | 0.0000007 |
| deptPediatrics                  |   0.2084679 | 0.0355285 |   5.867630 | 0.0000000 |
| deptMedicine                    |   0.5432042 | 0.0293638 |  18.499128 | 0.0000000 |
| deptSurgery                     |   0.9313875 | 0.0352673 |  26.409356 | 0.0000000 |
| clinPrimarily clinical emphasis |   0.1970312 | 0.0221749 |   8.885320 | 0.0000000 |
| certBoard Certified             |   0.1912134 | 0.0213626 |   8.950840 | 0.0000000 |
| Exper                           |   0.0181712 | 0.0018056 |  10.063791 | 0.0000000 |
| rankAssociate                   |   0.1731415 | 0.0339042 |   5.106786 | 0.0000007 |
| rankFull professor              |   0.2822813 | 0.0395941 |   7.129384 | 0.0000000 |
| genderMales                     |   0.0744792 | 0.0275681 |   2.701648 | 0.0073777 |
| rankAssociate:genderMales       | \-0.0829432 | 0.0447499 | \-1.853484 | 0.0650054 |
| rankFull professor:genderMales  | \-0.1052708 | 0.0466541 | \-2.256413 | 0.0249196 |

**Supplement Table 2 -** Final model of the data set. Coefficient
estimates, standard errors and p values are included.

## Supplemental Table 3

|             |  Df |     Sum Sq |   Mean Sq |    F value |   Pr(\>F) |
| ----------- | --: | ---------: | --------: | ---------: | --------: |
| dept        |   5 | 48.6084856 | 9.7216971 | 553.431530 | 0.0000000 |
| clin        |   1 |  2.4268234 | 2.4268234 | 138.152892 | 0.0000000 |
| cert        |   1 |  2.8316533 | 2.8316533 | 161.198832 | 0.0000000 |
| Exper       |   1 |  7.4608050 | 7.4608050 | 424.724681 | 0.0000000 |
| rank        |   2 |  1.5070418 | 0.7535209 |  42.896030 | 0.0000000 |
| gender      |   1 |  0.0308057 | 0.0308057 |   1.753690 | 0.1866376 |
| rank:gender |   2 |  0.1117068 | 0.0558534 |   3.179591 | 0.0433110 |
| Residuals   | 247 |  4.3388550 | 0.0175662 |         NA |        NA |

**Supplement Table 3 -** Anova test of the model. The p-value for gender
was 0.186. This value means that gender as the main covariate of
interest is not a significant contributor to the model when adjusting
for department, primary emphasis, certifcation, experience, and rank.
