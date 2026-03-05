# DV495_EFLR_Agency
# Dissertation: Women's Agency as a Conversion Factor in Economic Development  This repository contains all code and documentation for my MSc dissertation investigating whether increases in women's agency shape the extent to which local economic opportunities reduce multidimensional poverty in Ethiopia.  

# Dissertation Proposal (Updated)

## Title (working)
**Women’s Agency as a Conversion Factor: Legal Reform, Economic Activity, and Material Wellbeing in Ethiopia**

---

## 1. Motivation and Research Question

This dissertation investigates whether women’s agency shapes the extent to which economic opportunities translate into improvements in material wellbeing. The motivation comes from Sen’s capability approach, which emphasises that expanding resources is not sufficient for improving wellbeing unless individuals have the agency needed to convert those resources into valued functionings.

**Core research question:**  
> Does women’s agency act as a “conversion factor” that amplifies the poverty-reducing effects of local economic activity?

Empirically, this mechanism is difficult to identify because women’s agency and poverty are jointly determined and may be driven by common unobservables or reverse causality. To address this, I leverage an exogenous policy shock to women’s agency.

---

## 2. Policy Context and Source of Exogenous Variation

I exploit the rollout of Ethiopia’s **Revised Family Code (RFC)** as an exogenous shock to women’s legal rights and, plausibly, women’s agency. The RFC strengthened women’s authority in marital and economic life, including:

- Women’s authority to administer common marital property  
- Reduced ability of a spouse to deny permission for the other to work outside the home  
- Greater authority given to courts in divorce and inheritance disputes  
- Legal marriage age increased to 18  

### Rollout variation
The identifying variation occurs only in the early period:

- In **2000**, no region had implemented the RFC.  
- By **2005**, **five regions** had adopted the reform.  
- Between **2005 and 2011**, all remaining regions adopted the RFC, leaving **no cross-regional treatment variation after 2005**.

Because regional adoption occurred due to institutional and administrative autonomy (rather than contemporaneous economic performance), early- vs late-adopting regions are plausibly comparable, providing quasi-experimental variation suitable for causal inference.

---

## 3. Data

### DHS microdata (repeated cross-sections)
I use the Ethiopian Demographic and Health Surveys (DHS) at the **woman** level for **2000 and 2005** to estimate the causal effect of the RFC on women’s agency—these are the only waves aligned with differential policy exposure across regions.

I additionally use later DHS waves (e.g., 2011 and 2016) **only in the mechanism stage**, where treatment is fully rolled out but agency and economic conditions still vary across space.

### Night-time lights (NTL)
I merge **PSU-level night-time lights** to DHS clusters by PSU and year as a proxy for local economic activity.

---

## 4. Key Variables and Measurement

### (i) Women’s agency deprivation (individual level)
Outcome for the first-stage policy analysis: an **individual-level agency deprivation score**, constructed from DHS empowerment items (higher values = more deprived; lower values = higher agency). The index is built using an Alkire–Foster–style approach and includes dimensions such as:

- Decision-making autonomy  
- Attitudes towards domestic violence  
- Bodily autonomy  

### (ii) Material standard of living (SoL)
Main outcome for the mechanism test: a **material standard-of-living indicator**, corresponding to the standard-of-living component of the multidimensional poverty framework. I focus on SoL rather than the full MPI because education and health dimensions could respond to the reform through channels other than agency, raising exclusion-restriction concerns.

### (iii) Local economic activity (PSU level)
**Night-time lights (NTL)** at the PSU-year level, merged onto individuals by PSU.

### (iv) Treatment / instrument (region level)
A binary instrument capturing differential RFC exposure between 2000 and 2005:

- `Treated_r`: equals 1 if region *adopted RFC by 2005*, 0 otherwise  
- `Post_t`: equals 1 in 2005 (post), 0 in 2000 (pre)  
- Instrument: `Z_rt = Treated_r × Post_t`

---

## 5. Empirical Strategy

### 5.1 First Stage (Policy Relevance): RFC → Agency Deprivation (2×2 DiD)

Because all cross-regional variation in adoption occurs between **2000 and 2005**, I use a two-period DiD to estimate the causal effect of the reform on women’s agency deprivation:

\[
AgencyDeprivation_{i,r,t}
= \beta_0 + \beta_1 Treated_r + \beta_2 Post_t
+ \beta_3 (Treated_r \times Post_t) + X_{i,r,t}\Gamma + \varepsilon_{i,r,t}
\]

- The coefficient of interest is \(\beta_3\).  
- A **negative** \(\beta_3\) indicates the reform reduced agency deprivation (i.e., increased agency), consistent with the hypothesised mechanism.

**Inference and clustering:**  
Treatment is assigned at the **region** level, so inference is clustered at the region level. Given the small number of treated regions (five), I will supplement conventional clustered inference with **small-cluster robust methods** (e.g., wild cluster bootstrap and/or randomization inference).

---

### 5.2 Mechanism Test (2SLS): Agency as a Conversion Factor

To test whether women’s agency amplifies the effect of local economic activity on material wellbeing, I estimate:

\[
SoL_{i,PSU,r,t}
= \theta_1 NTL_{PSU,t} + \theta_2 Agency_{i,r,t}
+ \theta_3 (Agency_{i,r,t} \times NTL_{PSU,t}) + X_{i,r,t}\Gamma + \nu_{i,PSU,r,t}
\]

The key parameter is \(\theta_3\):  
> whether the marginal effect of local economic activity (NTL) on SoL is larger when women’s agency is higher.

#### Instrumenting the interaction term
Because agency is endogenous and the mechanism term is an interaction, I instrument:

- Endogenous term: \((Agency \times NTL)_{i,PSU,r,t}\)  
- Instrument: \(((Treated_r \times Post_t) \times NTL_{PSU,t})\)

This leverages reform-induced variation in agency interacted with local economic activity to identify the “conversion factor” channel.

**Inference:**  
All 2SLS models will cluster standard errors at the **region level**, since the exogenous shock (RFC exposure) varies at that level.

---

## 6. Contribution

This dissertation contributes to three strands of literature:

1. **Gender, legal institutions, and empowerment:** by estimating the causal impact of a family law reform on women’s agency deprivation.  
2. **Growth and poverty reduction mechanisms:** by testing whether women’s agency enhances the translation of economic activity into material wellbeing.  
3. **Applied causal methods with multi-level data:** by combining policy-based quasi-experimental variation with an IV interaction design using repeated cross-sections and spatially merged economic proxies.

---

## 7. Next Steps

- Finalise construction of the agency deprivation index and SoL outcome measure  
- Merge PSU-year nightlights to DHS microdata  
- Estimate the 2000–2005 DiD first stage (RFC → agency deprivation)  
- Implement the IV interaction model (Agency × NTL → SoL), using RFC exposure as the instrument  
- Conduct robustness checks and small-cluster inference (wild bootstrap / randomization inference)  
- Draft results, interpretation, and limitations sections

---
