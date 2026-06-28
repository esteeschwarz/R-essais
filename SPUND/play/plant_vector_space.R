## =================================================================
##  VECTOR SPACE OF PLANTS
##  Building an "embedding" space for plants and inspecting it the
##  way you would inspect token embeddings in a language model:
##  nearest neighbours + cosine similarity + a 2-D projection.
##
##  Two embeddings are built, side by side, so you can compare them:
##
##  (A) NAIVE EMBEDDING  -- just the standardized feature vector.
##      This is the "no learning at all" baseline. Traceable, but
##      it's just z-scored measurements, not anything resembling a
##      trained latent space.
##
##  (B) MARKOV-WALK EMBEDDING -- this is the one that's structurally
##      analogous to how word embeddings (the ancestors of LLM
##      latents) are actually built:
##        1. turn plants into a graph (edges = "this plant is
##           similar to that plant")
##        2. treat the graph as a Markov chain and take random
##           walks over it  -> this generates a "corpus" of
##           sequences, exactly like DeepWalk/node2vec do for
##           graph nodes, and exactly like a text corpus is a
##           sequence of tokens
##        3. count co-occurrences inside a sliding window over the
##           walks (skip-gram style context counting)
##        4. turn counts into a PPMI matrix, then SVD it down to
##           d dimensions
##      Levy & Goldberg (2014) showed that word2vec's
##      skip-gram-negative-sampling objective implicitly factorizes
##      a shifted PPMI matrix -- so step 4 is the closed-form,
##      no-gradient-descent version of what word2vec/GloVe learn
##      by training, and what gets reused/extended inside modern
##      LLM embedding layers. This script does that factorization
##      directly with svd(), so every step stays inspectable in
##      base R; no autodiff, no black box.
##
##  Dependencies: base R only (stats, graphics). No packages needed.
## =================================================================

set.seed(42)

## -----------------------------------------------------------------
## 0. DATA: iris, with one derived feature so we have 5 numeric
##    features per plant (Sepal.L, Sepal.W, Petal.L, Petal.W, and
##    Petal.Area = Petal.L * Petal.W). Species is kept ONLY as a
##    label for colouring plots -- it is never fed into the model.
## -----------------------------------------------------------------
data(iris)
plants <- iris
plants$Petal.Area <- plants$Petal.Length * plants$Petal.Width

feature_cols <- c("Sepal.Length", "Sepal.Width",
                   "Petal.Length", "Petal.Width", "Petal.Area")

X_raw <- as.matrix(plants[, feature_cols])
rownames(X_raw) <- paste0(substr(plants$Species, 1, 3), "_", seq_len(nrow(plants)))
species <- plants$Species
n <- nrow(X_raw)

## standardize -> this matrix IS the naive embedding (embedding A)
X <- scale(X_raw)

## -----------------------------------------------------------------
## Helpers
## -----------------------------------------------------------------

## row-wise cosine similarity matrix
cosine_sim_matrix <- function(M) {
  norms <- sqrt(rowSums(M^2))
  M_norm <- M / norms
  M_norm %*% t(M_norm)
}

## top-k nearest neighbours (excluding self) from a similarity matrix
knn_from_sim <- function(sim, k = 5) {
  sim2 <- sim
  diag(sim2) <- -Inf
  t(apply(sim2, 1, function(row) order(row, decreasing = TRUE)[1:k]))
}

print_neighbours <- function(idx, names_vec, knn_mat, sim_mat, label) {
  nb <- knn_mat[idx, ]
  cat(sprintf("\n[%s] nearest neighbours of %s:\n", label, names_vec[idx]))
  for (j in nb) {
    cat(sprintf("   %-10s  cos_sim = %.3f\n", names_vec[j], sim_mat[idx, j]))
  }
}

## =================================================================
## (A) NAIVE EMBEDDING: cosine similarity + neighbours
## =================================================================
sim_naive <- cosine_sim_matrix(X)
knn_naive <- knn_from_sim(sim_naive, k = 5)

cat("=== (A) Naive standardized-feature embedding ===\n")
for (i in c(1, 60, 110)) print_neighbours(i, rownames(X), knn_naive, sim_naive, "naive")

## =================================================================
## (B) MARKOV-WALK EMBEDDING
## =================================================================

## ---- B1. similarity graph: connect each plant to its k_graph
##          most similar neighbours, weight = cosine similarity ----
k_graph <- 8
W <- matrix(0, n, n)
sim_for_graph <- sim_naive
diag(sim_for_graph) <- -Inf
for (i in seq_len(n)) {
  nbrs <- order(sim_for_graph[i, ], decreasing = TRUE)[1:k_graph]
  w <- pmax(sim_for_graph[i, nbrs], 0)   # clip any negative similarity to 0
  W[i, nbrs] <- w
}
W <- (W + t(W)) / 2                       # symmetrize (undirected graph)

## ---- B2. Markov transition matrix: row-normalize the weights ----
P <- W / rowSums(W)
P[is.nan(P)] <- 0

## ---- B3. simulate random walks over the chain ("the corpus") ----
walk_length      <- 40
n_walks_per_node <- 20

simulate_walk <- function(start, P, length) {
  walk <- integer(length)
  walk[1] <- start
  cur <- start
  for (t in 2:length) {
    probs <- P[cur, ]
    if (sum(probs) == 0) probs <- rep(1 / n, n)   # isolated-node fallback
    cur <- sample.int(n, 1, prob = probs)
    walk[t] <- cur
  }
  walk
}

walks <- vector("list", n * n_walks_per_node)
idx <- 1L
for (i in seq_len(n)) {
  for (r in seq_len(n_walks_per_node)) {
    walks[[idx]] <- simulate_walk(i, P, walk_length)
    idx <- idx + 1L
  }
}

## ---- B4. co-occurrence counts within a sliding window ----
##      (this is exactly the skip-gram context-counting step)
window <- 4
Co <- matrix(0, n, n)
for (w in walks) {
  L <- length(w)
  for (t in seq_len(L)) {
    lo <- max(1, t - window); hi <- min(L, t + window)
    ctx <- w[setdiff(lo:hi, t)]
    Co[w[t], ctx] <- Co[w[t], ctx] + 1
  }
}

## ---- B5. PPMI transform (positive pointwise mutual information) ----
##      This is the step that turns raw counts into something where
##      distance reflects "how surprising is it that these two
##      co-occur", same idea used to build classical word vectors.
total   <- sum(Co)
rowsum_ <- rowSums(Co)
colsum_ <- colSums(Co)
PMI  <- log((Co * total) / (outer(rowsum_, colsum_) + 1e-9) + 1e-9)
PPMI <- pmax(PMI, 0)

## ---- B6. SVD factorization -> dense latent vectors ----
##      d is the embedding dimensionality (cf. embedding_dim in an
##      LLM's token-embedding table).
d  <- 10
sv <- svd(PPMI, nu = d, nv = 0)
Z  <- sv$u %*% diag(sqrt(sv$d[1:d]))
rownames(Z) <- rownames(X)

## ---- B7. cosine similarity + neighbours in the LEARNED space ----
sim_latent <- cosine_sim_matrix(Z)
knn_latent <- knn_from_sim(sim_latent, k = 5)

cat("\n=== (B) Markov-walk / PPMI-SVD latent embedding ===\n")
for (i in c(1, 60, 110)) print_neighbours(i, rownames(Z), knn_latent, sim_latent, "latent")

## -----------------------------------------------------------------
## Sanity check: how much do the two embeddings agree on neighbours?
## (Useful diagnostic: if this is very high, the learned embedding
##  hasn't added anything beyond the naive one; if it's very low,
##  something is probably mis-specified.)
## -----------------------------------------------------------------
agreement <- mean(sapply(seq_len(n), function(i) {
  length(intersect(knn_naive[i, ], knn_latent[i, ])) / 5
}))
cat(sprintf("\nAverage neighbour-overlap (naive vs latent), k=5: %.2f\n", agreement))

## =================================================================
## VISUALIZATION: 2-D PCA projection of both vector spaces
## =================================================================
pca_plot <- function(M, species, title, legend_pos = "topright") {
  pc <- prcomp(M, scale. = FALSE)$x[, 1:2]
  col_map <- c(setosa = "forestgreen", versicolor = "darkorange", virginica = "steelblue")
  plot(pc[, 1], pc[, 2], col = col_map[as.character(species)], pch = 19,
       xlab = "PC1", ylab = "PC2", main = title)
  legend(legend_pos, legend = names(col_map), col = col_map, pch = 19,
         bty = "n", cex = 0.8)
}

out_file <- "plant_vector_space_plots.pdf"
pdf(out_file, width = 11, height = 5.5)
par(mfrow = c(1, 2))
pca_plot(X, species, "(A) Naive standardized-feature space", legend_pos = "topright")
pca_plot(Z, species, "(B) Markov-walk / PPMI-SVD latent space", legend_pos = "bottomleft")
par(mfrow = c(1, 1))
dev.off()
cat(sprintf("\nPlots written to: %s\n", out_file))

## -----------------------------------------------------------------
## NOTE on going further with torch:
## To get embeddings that are trained by gradient descent (closer
## still to how an LLM's embedding table is actually fit) rather
## than by closed-form SVD, replace steps B4-B6 with a tiny
## skip-gram model in {torch}: sample (target, context) pairs from
## the same `walks`, build an n x d nn_embedding(), and train it
## with a negative-sampling or full-softmax cross-entropy loss via
## optim_adam(). The walk-generation (B1-B3) stays identical --
## only the "how do counts become vectors" step changes from
## linear algebra to gradient descent.
## =================================================================
