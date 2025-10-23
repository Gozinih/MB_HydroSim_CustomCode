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

### 🔢 Conversion Coefficients
| Conversion | Coefficient | Description |
|-------------|--------------|-------------|
| cms → 1000 m³/day | 86.4 | Used for storage and flow conversions |
| cfs → cms | 35.3147 | Discharge conversion |
| cms → cfs | 0.02831 | Reverse discharge conversion |
| ft → m | 0.3048 | Elevation/height conversion |

Two-decimal precision is used throughout the MODSIM-DSS simulation workflow. Therefore, in the custom code, all data **read** from MODSIM-DSS are divided by 100, and all data **written** back are multiplied by 100, to maintain consistency in numerical scaling of MODSIM-DSS.

---

### 🧾 Reference
> Gozini, H., Asadzadeh, M., Stadnyk, T. A., Riddell, P., & Tolossa, H. (2025).  
> *Integrated Water Resources and Hydropower Generation Modelling in a Complex, Cold Regions River-Reservoir System.*  
> Manuscript submitted for publication to *Sustainable Water Resources Management*.
