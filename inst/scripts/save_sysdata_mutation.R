# mutazioni


# https://github.com/PoisonAlien/TCGAmutations
# https://www.oncokb.org/cancer-genes
# https://www.nature.com/articles/nature12634
# https://www.nature.com/articles/ng.2786
# https://www.science.org/doi/10.1126/science.1235122


load("~/Documents/imageTCGA/R/sysdata.rda")

library(maftools)
library(maditr)
library(dplyr)
maf <- read.maf(maf = "/home/ilaria/Downloads/mc3.v0.2.8.PUBLIC.maf.gz")


flag<-as.data.frame(maf@data[, c(1, 16)])
tmp<-flag %>%
  mutate(Tumor_Sample_Barcode = substr(Tumor_Sample_Barcode, 1, 12)) %>%
  mutate(mutation = 1)
tmp_transformed<-dcast(tmp, formula = Tumor_Sample_Barcode ~ Hugo_Symbol, value.var = "mutation", fill = 0)
rownames(tmp_transformed)<-tmp_transformed$Tumor_Sample_Barcode
finale<-tmp_transformed %>%
  mutate(across(-1, ~ ifelse(. == 0, 0, 1)))

finale <- finale %>%
  rename(Case.ID = Tumor_Sample_Barcode)

db_CNA_surv_mut <- merge(
  db,
  finale,
  by = "Case.ID",
  all.x = TRUE
)

library(readr)
gene_list <- read_tsv("/home/ilaria/Downloads/cancerGeneList.tsv")

genes_in_db <- names(db_CNA_surv_mut)

# Colonna con i geni nel file esterno
genes_ref <- gene_list$`Hugo Symbol`

# Geni in comune
common_genes <- intersect(genes_in_db, genes_ref)

length(common_genes)


# top 20 per un dato dataset

library(dplyr)
library(tidyr)

top_genes <- db_CNA_surv_mut %>%
  select(starts_with("A")) %>%   # oppure selezione dinamica dei geni
  summarise(across(everything(), ~ sum(.x != 0, na.rm=TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "gene", values_to = "mut_n") %>%
  arrange(desc(mut_n)) %>%
  slice(1:20)

ggplot(top_genes, aes(x = reorder(gene, mut_n), y = mut_n)) +
  geom_col() +
  coord_flip() +
  ylab("Number of mutated samples") +
  xlab("Gene")
