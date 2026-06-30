
library(dplyr)
library(tidyr)
library(ggplot2)
library(ModelMetrics)

setwd("")

run1 <- read.csv("1-mined-combined.csv")
#run1 <- read.csv("2-mined-combined.csv")

#remove pybughive salt --> no labels available
run1 <- run1[run1$is_bugfix != "",]

#transform labels into bool
for (i in 5:ncol(run1)){
  run1[,i] <- as.logical(run1[,i])
  
}


# helpers
my_accuracy <- function(x, y){
  res <- x == y
  return(round(mean(res), 3))
}


# 1 - Performance metrics overall
y <- run1$is_bugfix

all_names <- c()
acc <- c()
prec <- c()
rec <- c()
f1 <- c()

for(i in 6:ncol(run1)){
  X <- run1[,i]
  
  name_temp <- colnames(run1)[i]
  all_names <- c(all_names, unlist(strsplit(name_temp, "_"))[2])
  
  acc <- c(acc, my_accuracy(X,y))
  prec <- c(prec, round(precision(y,X), 3))
  rec <- c(rec, round(recall(y,X), 3))
  f1 <- c(f1, round(f1Score(y,X), 3))

}

results <- data.frame(
  Model = all_names,
  Accuracy = acc,
  Precision = prec,
  Recall = rec,
  F1Score = f1
)
results <- results[order(results$F1Score, decreasing = TRUE),]


# 2 - Model per dataset+repo metrics

all_results <- list()
iter <- 1
for(i in 6:ncol(run1)){
  
  name_temp <- colnames(run1)[i]
  name <- unlist(strsplit(name_temp, "_"))[2]
  
  temp_r <- run1 %>%
    group_by(dataset, repo) %>%
    summarise(
      accuracy  = my_accuracy(is_bugfix, .data[[name_temp]]),
      precision = precision(is_bugfix, .data[[name_temp]]),
      recall    = recall(is_bugfix, .data[[name_temp]]),
      f1        = f1Score(is_bugfix, .data[[name_temp]])
      ) %>%
    ungroup()
  
  names(temp_r)[-(1:2)] <- paste0(name, "_", names(temp_r)[-(1:2)])
  
  all_results[[iter]] <- temp_r
  iter <- iter + 1
}

results1 <- Reduce(function(x, y) merge(x, y, by = c("dataset", "repo"), all = TRUE), all_results)

metrics <- results1 %>%
  gather(key = "key", value = "value", -dataset, -repo) %>%
  separate(key, into = c("predictor", "metric"), sep = "_(?=[^_]+$)") %>%
  spread(metric, value)

macrof1 <- metrics %>%
  group_by(predictor) %>%
  summarise(
    mean_f1 = mean(f1, na.rm = TRUE),
    sd_f1   = sd(f1, na.rm = TRUE),
    min_f1  = min(f1, na.rm = TRUE),
    max_f1  = max(f1, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_f1))

macrof1[,sapply(macrof1, is.numeric)] <- sapply(macrof1[,sapply(macrof1, is.numeric)], round, 3)


counts <- count(run1, dataset, repo)

# 2.1 - micro vs macro (overall f1 vs avg. per repo f1)
f1comp <- merge(results[,c(1,5)], macrof1[,c(1,2)], by.x = "Model", by.y = "predictor")
f1comp <- f1comp[order(f1comp$F1Score, decreasing = TRUE),]
#### This is not useful because we have only one repo labeled


# 2.2 - per dataset view, not per repo view
dsres <- metrics %>% group_by(dataset, predictor) %>%
  summarise(mean_f1 = round(mean(f1, na.rm = TRUE), 3)) %>%
  arrange(dataset, desc(mean_f1)) %>%
  ungroup()

ord <- c("mistral.small3.2.24b","qwen3.6.35b","qwen3.5.122b",
         "qwen3.coder.30b","gemma4.12b","codestral.22b",
         "codegemma.7b","stemming")

dsresWide <- dsres %>%
  spread(key = predictor, value = mean_f1) %>%
  select(dataset, one_of(ord))


# 2.3 - per repo view only in smartshark (only with negative labels)
smsh_only <- results1 %>%
  filter(dataset == "smartshark") %>%
  gather(key = "key", value = "value", -dataset, -repo) %>%
  separate(key, into = c("predictor", "metric"), sep = "_(?=[^_]+$)") %>%
  spread(metric, value)

macrof_smsh <- smsh_only %>%
  group_by(predictor) %>%
  summarise(
    mean_f1 = mean(f1, na.rm = TRUE),
    sd_f1   = sd(f1, na.rm = TRUE),
    min_f1  = min(f1, na.rm = TRUE),
    max_f1  = max(f1, na.rm = TRUE),
    mean_prec = mean(precision, na.rm = TRUE),
    sd_prec   = sd(precision, na.rm = TRUE),
    min_prec  = min(precision, na.rm = TRUE),
    max_prec  = max(precision, na.rm = TRUE),
    mean_rec = mean(recall, na.rm = TRUE),
    sd_rec   = sd(recall, na.rm = TRUE),
    min_rec  = min(recall, na.rm = TRUE),
    max_rec  = max(recall, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_f1))

macrof_smsh[,sapply(macrof_smsh, is.numeric)] <- sapply(macrof_smsh[,sapply(macrof_smsh, is.numeric)], round, 3)


# 2.4 - per repo difficutly figure
ss <- metrics %>% filter(dataset == "smartshark") %>% filter(repo != "xerces2-j")

# order predictors by mean F1, repos by mean F1
pred_order <- ss %>% group_by(predictor) %>%
  summarise(m = mean(f1, na.rm = TRUE)) %>% arrange(m) %>% pull(predictor)
repo_order <- ss %>% group_by(repo) %>%
  summarise(m = mean(f1, na.rm = TRUE)) %>% arrange(m) %>% pull(repo)

ss <- ss %>%
  mutate(predictor = factor(predictor, levels = pred_order),
         repo      = factor(repo,      levels = repo_order))

# --- Version A: absolute F1 ---
ggplot(ss, aes(repo, predictor, fill = f1)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", limits = c(0, 1)) +
  labs(x = "Repository (easy \u2192 hard)", y = NULL, fill = "F1",
       title = "smartshark per-repository F1") +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# --- Version B: within-repo centered F1 (disagreement) ---
ss_c <- ss %>% group_by(repo) %>%
  mutate(f1_centered = f1 - mean(f1, na.rm = TRUE)) %>% ungroup()

ggplot(ss_c, aes(repo, predictor, fill = f1_centered)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0) +
  labs(x = "Repository (easy \u2192 hard)", y = NULL, fill = "F1 \u2212 repo mean",
       title = "smartshark: predictor disagreement (repo-centered F1)") +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



# 3 - Friedman test to finally rank the models:
## ---- 1. Build the repo x model F1 matrix for smartshark, one prompt ----
## metrics = your long frame: dataset, repo, predictor, f1  (for prompt P1)

ss <- metrics %>%
  filter(dataset == "smartshark") %>% filter(repo != "xerces2-j") %>%
  select(repo, predictor, f1)

## wide: rows = repositories (blocks), cols = models (treatments)
wide <- ss %>%
  spread(predictor, f1)   # tidyr >= 1.0
## tidyr 0.8 fallback: spread(predictor, f1)

mat <- as.matrix(wide[,-1])      # numeric matrix, rows=repos, cols=models
rownames(mat) <- wide$repo

## ---- 3. Omnibus Friedman test ----
ft <- friedman.test(mat)
print(ft)
## If p >= 0.05: STOP. No model differs; the "leading cluster" ordering
## is not statistically supported and you report that honestly.
## If p <  0.05: at least one model differs -> proceed to post-hoc.

## ---- 4. Nemenyi post-hoc (all pairwise) ----
# install.packages("PMCMRplus")
library(PMCMR)
ny <- posthoc.friedman.nemenyi.test(mat)   # matrix of pairwise p-values
print(ny)

## ---- 5. Average ranks + critical difference (for the CD diagram) ----
## ranks per repo: best F1 = rank 1 (hence the minus sign)
ranks <- t(apply(-mat, 1, rank))
avg_rank <- sort(colMeans(ranks))   # lower = better
print(round(avg_rank, 3))

k <- ncol(mat); N <- nrow(mat)
q_alpha <- qtukey(0.95, k, df = Inf) / sqrt(2)   # Nemenyi critical value
CD <- q_alpha * sqrt(k * (k + 1) / (6 * N))
cat(sprintf("k=%d models, N=%d repos, critical difference (CD) = %.3f\n",
            k, N, CD))

library(scmamp)
plotCD(as.data.frame(mat), alpha = 0.05)
