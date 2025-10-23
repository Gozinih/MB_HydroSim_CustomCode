# MB_HydroSim_CustomRun

Custom VB.Net code used in **MB_HydroSim** (Gozini et al., 2025, submitted).  
This repository contains the executable logic for custom hydropower system simulation and optimization.

---

### ⚙️ Description

This code replicates the operational logic used in **MODSIM-DSS (v8.5.1)**, extended for the Manitoba Hydro System.  
Sensitive data such as proprietary rule curves, regression coefficients, and operational thresholds have been replaced with the placeholder `'Num'` to comply with confidentiality agreements.

---

### 🧩 DLL Dependencies
All required libraries are available within the MODSIM installation directory:
A2CM.dll
libsim.dll
ModelOptimization.dll
ModelSolver.dll
ModelStatistics.dll
ModsimModel.dll
NetworkUtils.dll
XYFile.dll

---

### 🔢 Conversion Coefficients
| Conversion | Coefficient | Description |
|-------------|--------------|-------------|
| cms → 1000 m³/day | 86.4 | Used for storage and flow conversions |
| cfs → cms | 35.3147 | Discharge conversion |
| cms → cfs | 0.02831 | Reverse discharge conversion |
| ft → m | 0.3048 | Elevation/height conversion |

Two-decimal precision is used throughout the MODSIM simulation workflow.  
All data **read** from MODSIM are divided by 100, and all data **written** back are multiplied by 100 for consistency.

---

### 🧾 Reference
> Gozini, H., Asadzadeh, M., Stadnyk, T. A., Riddell, P., & Tolossa, H. (2025).  
> *Integrated Water Resources and Hydropower Generation Modelling in a Complex, Cold Regions River-Reservoir System.*  
> Manuscript submitted for publication to *Sustainable Water Resources Management*.

---

### 🧠 Data and Code Availability
All code in this repository is open and reproducible.  
Proprietary datasets from Manitoba Hydro and the Government of Manitoba are **not shared** due to confidentiality agreements.

### 📜 License

This project is released under the [MIT License](./LICENSE).  
Users may freely modify and reuse the code under the condition that confidential or restricted data are **not reintroduced** into public versions of the software.
