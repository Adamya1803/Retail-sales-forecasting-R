# 🛍️ Retail Sales Forecasting in R

**Objective:**  
Forecast future weekly retail sales using time-series models (ARIMA and ETS).

---

## 📊 Project Overview
A synthetic dataset of 220 weekly sales records (2019–2023) was generated to simulate realistic retail behaviour:
- Gradual upward **trend**  
- Annual **seasonality**  
- Random **noise**

Two statistical forecasting models were applied:
- **ARIMA** – captures trend and seasonality
- **ETS** – exponential smoothing model

---

## 🧮 Results

| Model | RMSE | MAE |
|--------|------|------|
| ARIMA | **7.78** | **6.61** |
| ETS | 33.02 | 30.18 |

**Interpretation:**  
ARIMA produced a far lower error rate, meaning it predicted weekly sales much more accurately than ETS.  
This demonstrates how predictive analytics can support demand planning, inventory management, and revenue forecasting.

---

## 🧰 Tools & Skills
R • Time-Series Forecasting • ARIMA/ETS • RMSE & MAE Evaluation • Data Simulation • Business Analytics

---

## 🧠 Author
**Adamya Shukla**  
Created as part of a self-learning analytics project to strengthen forecasting and R programming skills.
