# Charlie Bucket Effect
**Limited resources help explain the delicate balance between cooperation and competition in human alloparental care**

Code and data used in the analysis of grandmaternal presence and gradchild survival in West Bohemia 18-19th century.
You can run the whole statistical analysis from **0_start_scripts.R** that sources all the necessary code from other files.
If you want to inspect the analysis in detail, start with **1_arrange_data.R** script and proceed to consecutive scripts 2, 3 etc.
If scripts share the number, they can be run independently on each other.

Scripts 1 - 3 test the core hypothesis.
Scripts 4 and 5 contain supplementary analyses, post hoc tests of grandmother avalability assumptions and descriptives used in the paper.

**source_data.txt** contains the crutial information about the studied children.
**pedigree_data.txt** contains the relatedness informatin about the whole population that can be used to construct the relatedness matrix.
**key_places** contains information about residences and tollerated forms of settlement names in the dataset.
**distances** contains a distance matrix of the settlements based on current GPS data.
