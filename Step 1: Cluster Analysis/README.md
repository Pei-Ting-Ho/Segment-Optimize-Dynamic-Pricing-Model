# Cluster Analysis

## Text File
Here, I explore the clustering methods, grouping scientific note / personal understanding into 3 sections:

**1. Brief Introduction: Clustering Concept**
- Basic idea behind cluster analysis
- Common clustering methods

**2. Data Type: Categorical Data**
- K-means Clustering (Binary-Based)
- K-means Clustering (Relative-Frequency-Based)
- K-modes Clustering

**3. Evaluation Criteria**
- Elbow method
- Average silhouette method

## Code File
Here, I perform the data manipulation and implement the clustering methods. Specifically, several R packages are heavily used to help achieve the desired result:
- The `dplyr` package: For the convenient piping workflow and the useful verbs usage 
- The `purrr` package: For the simple yet efficient iteration (The `map_*()` functions)
- The `dummies` package: For transforming categorical attributes to binary attributes  
- The `factoextra`, `gridExtra`, and `ggplot2` packages: For visualizing clustering results and combining several subplots
- The `klaR` package: For performing the k-modes clustering 


