library(factoextra)

# scale = tRUE to scale variables before pca
pca <-
  prcomp( ~ TradeValuation1 + Age + Mileage + PreviousOwners,
          data = offers_made,
          scale = TRUE)
pca <-
  prcomp( ~ TradeValuation1 + Age + Mileage,
          data = offers_made,
          scale = TRUE)
# pca2 <- princomp(~ TradeValuation1 + Age + Mileage + PreviousOwners, data = pca_data)

fviz_eig(pca)

fviz_pca_ind(
  pca,
  axes = c(1, 3),
  col.ind = offers_made$TransmissionType,
  palette = c("slategrey", "plum4"),
  repel = TRUE
)


fviz_pca_ind(pca,
             col.ind = offers_made$Condition,
             repel = TRUE)

fviz_pca_ind(pca,
             col.ind = offers_made$PreviousOwners,
             repel = TRUE)

fviz_pca_ind(pca,
             col.ind = offers_made$Make,
             repel = TRUE,
             addEllipses = TRUE,
             ellipse.type = "confidence")








