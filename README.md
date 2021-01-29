# Charlie Bucket Effect
**Limited resources help explain the delicate balance between cooperation and competition in human alloparental care**

Code and data used in the analysis of grandmaternal presence and gradchild survival in West Bohemia 18-19th century.
You can fit the model from **02_Model_MCMC.R**, if you are interested in how data is handeled, check **01_data_handle_by_month.R**. This script sources additional code from **00_arrange_data_sample.R**. The model is summarized and the results are visualized using **03_visualizations_and_summary.Rmd**.
In any case it is advisable to start the anlysis from the **final.Rproj** project file that sets the essentials like working directory and encoding.

**source_data.txt** contains the crutial information about the studied children.\
**pedigree_data.txt** contains the relatedness informatin about the whole population that can be used to construct the relatedness matrix.\
**key_places.txt** contains information about residences and tollerated forms of settlement names in the dataset.\
**distances.txt** contains a distance matrix of the settlements based on current GPS data.
**source_data.GM.txt** is an extra file that contains additional information on grandmothers. This is used only to calulate descriptive statistics about the grandmothers.\
**complete_families** is an additional, differently structured dataset, that contains data on recosntructed familes, where all children were identified. This was used to calculate descriptive statistics by families (total number of children, onset of reproduction etc.) Each person in the database is assigned with a unique code, so information on a single person can be stored in more than one dataset.

All data files contain tab-separated values.
