# MB_HydroSim_CustomCode

This is the custom code written in VB.Net and used in **MB_HydroSim** (Gozini et al., 2025).  
MB_HydroSim is a **MODSIM-DSS (v8.5.1)** simulation tool developed for daily water surface elevation, outflow, and hydropower simulation in the Province of Manitoba.

---

### ⚙️ Description

This code replicates the operational logic used in **MB_HydroSim**, for major lakes and control points that regulates the flow to and through 16 hydropower stations in Manitoba.
Sensitive data such as proprietary rule curves, regression coefficients, and operational thresholds have been replaced with the placeholder `'Num'` to comply with confidentiality agreements.
This code can serve as a reference for developing and implementing complex operational rules in other case studies using MODSIM-DSS.

---

### 🧩 DLL Dependencies
All required libraries are available within the MODSIM-DSS installation directory:
- A2CM.dll
- libsim.dll
- ModelOptimization.dll
- ModelSolver.dll
- ModelStatistics.dll
- ModsimModel.dll
- NetworkUtils.dll
- XYFile.dll

---

### 🧾 Reference
> Gozini, H., Asadzadeh, M., Stadnyk, T. et al. Integrated water resources and hydropower generation modelling in a complex, cold-regions river-reservoir system. Sustain. Water Resour. Manag. 12, 20 (2026). https://doi.org/10.1007/s40899-026-01335-7.
