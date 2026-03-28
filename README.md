# 🛍️ retailr

<p align="center">
  <img src="image_upscaled.png" width="600"/>
</p>
---



## 📌 Overview

`retailr` analyzes fashion product catalog data (CSV format) and segments products into **High**, **Medium** and **Low** profit potential clusters.

It goes beyond clustering by telling retailers:
- Which products to **scale**
- Which to **reposition**
- Which to **discontinue**

---

## 🚀 Key Features

- 📊 Data preprocessing tailored for fashion datasets  
- 🧠 PCA + t-SNE pipeline for dimensionality reduction  
- 🔍 K-means clustering into profit tiers  
- 🎯 Actionable recommendations using feature gap analysis  
- 📈 Built-in visualizations  

---

## ⚙️ Installation

```r
# install.packages("devtools")
devtools::install_github("vritika19/retailr")
```

---

## 🧪 Example Usage

```r
library(retailr)

prepped <- prep_data(raw_data)
result  <- analyse(prepped)

plot_clusters(result)

rec <- recommend(result, product_index = 1)
rec$verdict
rec$feature_gap
```

---

## 📊 What the Model Does

1. Preprocesses product data  
2. Applies PCA for dimensionality reduction  
3. Uses t-SNE for visualization  
4. Clusters products into profit tiers  
5. Generates actionable recommendations  

---

## 💡 Business Value

- Identify high-performing products  
- Improve mid-tier products  
- Avoid losses from weak inventory  

---

## 📈 Visualizations Included

- t-SNE cluster plot  
- Cluster size distribution  
- Price distribution by tier  
- Feature heatmap  
- PCA scree plot  

---

## 🔒 License

MIT License

---

## 👩‍💻 Author

**Vritika**  
 

---

## ⭐ Support

If you found this useful:
- Star ⭐ the repo  
- Fork 🍴 it  
- Contribute 🚀  

---
