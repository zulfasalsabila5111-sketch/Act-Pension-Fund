# Analysis of Pension Fund Calculation 
### Using Makeham and Gamma-Gompertz Mortality Laws

## 📌 Project Overview
This repository contains the implementation and analysis of **pension fund calculations** by applying **Makeham** and **Gamma-Gompertz mortality laws**.  

The project is based on a final thesis submitted to fulfill the requirements for the Bachelor of Actuarial Science degree at Institut Teknologi Sepuluh Nopember (ITS).

The main objective is to compare the results of pension liability estimation under different mortality assumptions and to evaluate which model provides a better fit for real data.

---

## 🎯 Objectives
- Construct mortality tables using **Makeham** and **Gamma-Gompertz** laws.  
- Calculate pension liabilities using the **Projected Unit Credit (PUC)** method.  
- Compare the outcomes between the two mortality models.  
- Provide insights into how mortality modeling impacts pension fund estimation and long-term financial planning.  

---

## 🛠️ Methods & Tools
- **Mortality Modeling**
  - Makeham Law
  - Gamma-Gompertz Law
  - Parameter estimation using **Loss Function minimization** and **Nelder-Mead optimization** in R.
- **Pension Fund Calculation**
  - Projected Unit Credit (PUC) method  
  - Normal Cost (NC), Actuarial Liability (AL), and Present Value of Future Benefit (PVFB).
- **Tools**
  - R (RStudio) with `MortalityLaw` package
  - Excel (pension fund calculation)

---

## 📊 Key Findings
- **Mortality Model Accuracy**
  - Makeham fits better for **female participants**.
  - Gamma-Gompertz fits better for **male participants**.
  - Overall, Gamma-Gompertz is more accurate (closer to actual data).  

- **Pension Fund Results**
  - PVFB, NC, and AL increase every year under both models.
  - Makeham produces higher values than Gamma-Gompertz.
  - Female participants consistently show higher pension values.  

These results highlight the importance of mortality model selection in actuarial pension calculations and financial risk management.  

---

## 💡 Recommendations
From the conclusions obtained, several suggestions for future research are as follows:

1. In pension fund calculations, it is important to **consider mortality factors**, as they significantly affect the estimation of pension fund requirements and long-term financial planning.  
2. For pension fund institutions or companies, it is recommended to apply the **Gamma-Gompertz mortality law** in pension fund calculations. This is because, in practice, the Gamma-Gompertz law is more accurate than Makeham's law. Theoretically, the Gamma-Gompertz law reflects mortality patterns more realistically, assuming that the risk of death increases exponentially with age while also accounting for individual variations in mortality. Therefore, this model is considered more capable of capturing the complex and diverse mortality patterns found in population data.

---

## 📖 References

Bowers, N.L., Gerber, H.U., Hickman, J.C., Jones, D.A., Nesbitt, C.J. (1997). Actuarial Mathematics.

Dickson, D.C.M., Hardy, M.R., Waters, H.R. (2013). Actuarial Mathematics for Life Contingent Risks.

Indriani, L., & Sari, N. (2020). Penerapan hukum Makeham dalam perhitungan dana tabarru’.

Ruhiyat, A., dkk. (2022). Perhitungan premi dan cadangan manfaat dengan hukum mortalita Gamma-Gompertz.
