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


