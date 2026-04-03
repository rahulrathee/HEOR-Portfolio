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
- Conclusion: New immunotherapy is not cost-effective at current price

### Tools
R | heemod | dampack | ggplot2 | dplyr

