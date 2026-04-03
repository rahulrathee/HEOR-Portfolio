# HEOR Portfolio — Rahul

Health economics and outcomes research projects built in R.

---

## Project 1: Cost-Effectiveness Analysis — NSCLC Immunotherapy

**Disease area:** Non-small cell lung cancer (NSCLC)
**Comparator:** New immunotherapy vs standard of care
**Perspective:** Canadian public payer
**Time horizon:** 5 years (60 monthly cycles)
**Framework:** CADTH guidelines

### Model structure
3-state Markov model: Progression-Free to Progressed to Dead

### Methods
- Deterministic cost-effectiveness analysis
- Probabilistic sensitivity analysis (n = 1,000 Monte Carlo simulations)
- Cost-effectiveness plane
- Cost-effectiveness acceptability curve (CEAC)

### Key findings
- Incremental cost: CAD $89.7M (cohort of 1,000 patients)
- Incremental QALYs: 185.6
- ICER: CAD $483,362 per QALY gained
- Probability cost-effective at CADTH $50,000/QALY threshold: 1%
- Conclusion: New immunotherapy is not cost-effective at the current price

### Tools
R | heemod | dampack | ggplot2 | dplyr


## Project 2: Budget Impact Model — Ambulatory TKA Pain Management

**Disease area:** Total knee arthroplasty (TKA)
**Comparator:** Optimized vs standard post-discharge opioid prescribing
**Perspective:** Ontario public payer
**Time horizon:** 3 years
**Framework:** CADTH budget impact guidance

### Background
Ambulatory TKA programs are expanding across Ontario, with same-day
discharge rates projected to grow from 30% to 46% of provincial TKA
volume over 3 years. Clinical assumptions informed by real-world data
from a Canadian academic hospital.

### Methods
- Population-based budget impact model
- Two scenarios: standard (20 tablets hydromorphone) vs optimized (10 tablets)
- Cost components: medications, ER visits, readmissions, clinic returns
- Sensitivity to ambulatory program uptake growth

### Key findings
- 3-year cost under standard prescribing  : CAD $7,308,084
- 3-year cost under optimized prescribing : CAD $6,659,150
- 3-year total savings                    : CAD $648,934
- Savings grow year over year as ambulatory TKA programs expand

### Tools
R | ggplot2 | dplyr | scales


