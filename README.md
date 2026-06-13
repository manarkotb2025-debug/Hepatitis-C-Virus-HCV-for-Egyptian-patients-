# Hepatitis C Virus (HCV) Prediction for Egyptian Patients

---

## 1. **Project Overview**

Hepatitis C Virus (HCV) is a major public health challenge, particularly in Egypt, which historically faced one of the highest prevalence rates globally. Early diagnosis and classification of liver fibrosis progression are critical for effective healthcare intervention.

This project analyzes clinical and laboratory test results of Egyptian patients to build a complete **data analytics pipeline** and an **interactive Power BI dashboard** aimed at analyzing risk factors and predicting HCV progression stages.

**Tools Used:**
* **R (tidyverse, caret, ggplot2)** – Data cleaning, handling missing values, statistical analysis, and machine learning pipeline
* **Excel** – Initial exploration and patient attribute validation
* **Power BI** – Data modeling, DAX analytics, and clinical performance dashboarding

---

## 2. **Project Objective**

The main goal is to build a **data-driven healthcare analytics system** that enables medical professionals, analysts, and stakeholders to:
* Monitor **patient demographics and risk distribution** across categories
* Analyze the correlation between **biochemical markers** (ALT, AST, Albumin, etc.) and HCV severity
* Evaluate and predict the progression of **liver fibrosis stages** (F0 to F4)
* Provide medical stakeholders with an **interactive exploratory view** of clinical laboratory results

---

## 3. **Key Stakeholders**

The system is designed for:
* **Medical Specialists & Hepatologists** – Track patient risk markers and treatment urgency
* **Healthcare Operations Teams** – Evaluate clinical asset and medical resource allocation
* **Public Health Analysts** – Monitor epidemiology trends and demographic vulnerability factors

---

## 4. **Key Performance Indicators (KPIs)**

The dashboard calculates and tracks:
* **Total Patients Evaluated**
* **Fibrosis Distribution Rate** (Percentage of Advanced Fibrosis - F3/F4)
* **Average AST/ALT Ratio** (Critical indicator of liver damage)
* **High-Risk Patient Count** (Based on critical biomarker critical thresholds)
* **Average RNA Load** (Viral viral activity tracking)

---

## 5. **System Architecture**

The project follows a **multi-layer analytics pipeline**:

```mermaid
flowchart LR
    A[Raw Patient CSV Data] --> B[R Scripts Analysis]
    B --> C[Biomarker Discretization & Cleaning]
    C --> D[Processed Star-Schema Datasets]
    D --> E[Power BI Data Model]
    E --> F[DAX Clinical Measures]
    F --> G[Interactive Patient Dashboard]
    G --> H[Medical Insights]
