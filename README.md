<h1 align="center">FROM CASH TO CASHLESS: EVALUATING PAYNOW AND CASH DEMAND IN SINGAPORE</h1>

**NUS-MOE H3 Humanities and Social Sciences Research Programme (Economics: E3)**

**Nevin Shi En Yu, 2026** 

---

### Research Paper 

The full research report is available here:
[Click here to read the full research report](https://github.com/nevinsey08/H3_HSSR_2026/blob/main/Full%20Research%20Paper/NUS-MOE%20H3%20HSSRP%20Research%20Report_Nevin%20Shi%20En%20Yu_Final%20Draft.pdf)

<br>

**ABSTRACT:**
This paper examines whether PayNow reduced aggregate cash demand in Singapore. Using a constructed monthly currency in circulation to gross domestic product (CIC/GDP) ratio series, this paper estimates seasonal autoregressive integrated moving average (SARIMA) interrupted time-series models over the period from April 2014 to December 2019, treating August 2017 as the PayNow intervention month. The candidate models incorporate both step and ramp specifications to compare and distinguish an immediate level shift from a gradual post-intervention decline effect. The selected baseline model estimates a PayNow step coefficient of −0.00515, significant at the 1% level. This implies an immediate and persistent 0.515 percentage-point reduction in CIC/GDP relative to the no-PayNow counterfactual. Actual CIC/GDP lies below the counterfactual in 28 out of 29 post-intervention months. The coefficients of all ramp-intervention models are insignificant. Overall, the findings suggest that PayNow resulted in a one-time persistent reduction in aggregate cash demand, although the single-country experimental design warrants cautious causal interpretation.

---

**ACKNOWLEDGEMENTS:**
I would like to express my sincere gratitude to my research supervisor Dr Huang Ta-Cheng for his invaluable guidance and support throughout this research. His feedback and dedication were essential in shaping the direction of this paper. In addition, he consistently went above and beyond in every consultation session, generously offering me with his time and expertise to ensure the success of this research. I am also grateful to Ms Valerie Cher for her administrative support throughout the programme. Her assistance in monitoring my progress, serving as a reliable point of contact, and providing timely reminders and essential information greatly facilitated the completion of this research. 

---

### Replication Workflow

To replicate the empirical analysis, the R scripts should be run in the following order:

1. [**GDP Temporal Disaggregation.R**](https://github.com/nevinsey08/H3_HSSR_2026/blob/main/General%20Dataset/GDP%20Temporal%20Disaggregation.R)
2. [**CIC-GDP Ratio.R**](https://github.com/nevinsey08/H3_HSSR_2026/blob/main/General%20Dataset/CIC-GDP%20Ratio.R)
3. [**SARIMA.R**](https://github.com/nevinsey08/H3_HSSR_2026/blob/main/SARIMA/SARIMA.R)
4. [**Counterfactual Analysis.R**](https://github.com/nevinsey08/H3_HSSR_2026/blob/main/Counterfactual%20Analysis/Counterfactual%20Analysis.R)
5. [**In-Time Placebo Tests.R**](https://github.com/nevinsey08/H3_HSSR_2026/blob/main/Robustness%20Tests/In-Time%20Placebo%20Tests.R)
6. [**Falsification Test.R**](https://github.com/nevinsey08/H3_HSSR_2026/blob/main/Robustness%20Tests/Falsification%20Test.R)

The scripts should be run sequentially because later scripts depend on datasets and model outputs created in earlier scripts.

---

**DATA SOURCES:**

- Currency in Circulation, end-of-period, monthly

   Source: data.gov.sg

   Link: https://data.gov.sg/datasets/d_10036483fced016b239ce7d2ab175125/view

<br>

- Quarterly nominal GDP, monthly

   Source: CEIC Data

   Series ID: 225396201

 <br> 

- (Total) Industrial Production Index, monthly

   Source: CEIC Data

  Series ID: 712089357 (SBYMAAAAAAABFU)

 <br> 

- Retail Sales Index excluding motor vehicles, monthly  

  Source: CEIC Data

  Series ID: 720060277 (SHOCARAAAAABLO)

<br>

- Total Domestic Exports, monthly  

  Source: CEIC Data

  Series ID: 36411401 (SJAG)

<br>

- Visitor Arrivals, monthly 

  Source: CEIC Data

  Series ID: 359380027

<br>

- Unemployment Rate, monthly 

  Source: CEIC Data

  Series ID: 539593457

<br>

- SGD/USD Exchange Rate, period-average, monthly

  Source: CEIC Data

  Series ID: 211458702 (SMDAAAAAA)

---

Last Updated: 26/7/2026 
  





