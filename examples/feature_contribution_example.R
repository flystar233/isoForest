# ========================================
# isoForest 特征贡献度分析示例
# ========================================

# 加载必要的包
library(isoForest)
library(ggplot2)

# 准备示例数据
data(iris)

# 创建一个包含明显异常点的数据集
iris_modified <- iris[1:4]  # 只使用数值特征
# 人工创建异常点：让某些样本的特定特征值变得极端
iris_modified[1, "Sepal.Length"] <- 10  # 异常大的萼片长度
iris_modified[50, "Petal.Width"] <- 5   # 异常大的花瓣宽度
iris_modified[100, c("Sepal.Width", "Petal.Length")] <- c(1, 8)  # 两个特征都异常

# 训练isoForest模型
cat("训练isoForest模型...\n")
model <- isoForest(iris_modified, num_trees = 100, seed = 123)

# ========================================
# 步骤1: 识别异常点
# ========================================

cat("\n步骤1: 识别异常点\n")
cat("==================\n")

# 使用5%的污染率检测异常点
anomalies <- is_anomaly(model, contamination = 0.05)
anomaly_ids <- which(anomalies)

cat("检测到的异常点ID:", anomaly_ids, "\n")
cat("异常点数量:", length(anomaly_ids), "\n")

# 查看异常点的分数
anomaly_scores <- model$scores[anomalies, ]
print(anomaly_scores)

# ========================================
# 步骤2: 分析特定异常点的特征贡献
# ========================================

cat("\n步骤2: 分析特征贡献度\n")
cat("=====================\n")

# 分析前3个异常点的特征贡献
target_samples <- head(anomaly_ids, 3)
cat("分析样本ID:", target_samples, "\n\n")

# 使用路径分析方法
cat("使用路径分析方法...\n")
contributions_path <- feature_contribution(
  model, 
  sample_ids = target_samples, 
  data = iris_modified, 
  method = "path"
)

print(contributions_path)

# ========================================
# 步骤3: 使用置换重要性方法
# ========================================

cat("\n步骤3: 置换重要性分析\n")
cat("===================\n")

# 使用置换方法（计算时间较长但更准确）
cat("使用置换重要性方法...\n")
contributions_perm <- feature_contribution(
  model, 
  sample_ids = target_samples[1:2],  # 只分析前2个样本以节省时间
  data = iris_modified, 
  method = "permutation",
  n_permutations = 30
)

print(contributions_perm)

# ========================================
# 步骤4: 可视化特征贡献
# ========================================

cat("\n步骤4: 可视化分析\n")
cat("================\n")

# 为特定样本创建条形图
if (require(ggplot2, quietly = TRUE)) {
  
  # 样本1的路径贡献条形图
  cat("创建样本", target_samples[1], "的特征贡献条形图...\n")
  p1 <- plot_feature_contribution(contributions_path, sample_id = target_samples[1], method = "path")
  print(p1)
  
  # 样本1的饼图
  cat("创建样本", target_samples[1], "的特征贡献饼图...\n")
  p2 <- plot_feature_contribution(contributions_path, sample_id = target_samples[1], method = "path", plot_type = "pie")
  print(p2)
  
  # 所有样本的汇总图
  cat("创建所有异常样本的特征贡献汇总图...\n")
  p3 <- plot_feature_contribution(contributions_path, method = "path")
  print(p3)
  
} else {
  cat("需要安装ggplot2包才能创建可视化图表\n")
}

# ========================================
# 步骤5: 详细解释结果
# ========================================

cat("\n步骤5: 结果解释\n")
cat("==============\n")

# 获取第一个异常点的详细信息
sample_1_data <- iris_modified[target_samples[1], ]
sample_1_contrib <- contributions_path[[paste0("sample_", target_samples[1])]]

cat("样本", target_samples[1], "的详细分析:\n")
cat("原始特征值:\n")
print(sample_1_data)

cat("\n异常分数:", round(sample_1_contrib$original_score, 4), "\n")

cat("\n特征贡献度 (路径分析):\n")
path_contrib <- sample_1_contrib$path_contributions
path_contrib_sorted <- sort(path_contrib, decreasing = TRUE)
for (i in seq_along(path_contrib_sorted)) {
  cat(sprintf("  %s: %.1f%%\n", names(path_contrib_sorted)[i], path_contrib_sorted[i] * 100))
}

# ========================================
# 步骤6: 实用建议和总结
# ========================================

cat("\n步骤6: 实用建议\n")
cat("==============\n")
cat("特征贡献度分析的两种方法:\n")
cat("1. 路径分析 (path): 基于决策树路径，快速但可能不够精确\n")
cat("2. 置换重要性 (permutation): 更准确但计算量大\n\n")

cat("使用建议:\n")
cat("- 快速分析: 使用路径分析方法\n")
cat("- 精确分析: 使用置换重要性方法\n")
cat("- 批量分析: 先用路径方法筛选，再用置换方法精确分析重点样本\n")
cat("- 可视化: 使用条形图查看具体数值，使用饼图查看相对比例\n\n")

cat("注意事项:\n")
cat("- 特征贡献度是相对概念，表示该特征对异常检测的相对重要性\n")
cat("- 高贡献度不一定意味着该特征值异常，而是该特征在检测过程中起重要作用\n")
cat("- 建议结合原始特征值和贡献度一起分析\n")

# ========================================
# 实际应用示例
# ========================================

cat("\n实际应用示例:\n")
cat("=============\n")

# 创建一个简单的异常诊断报告
for (i in seq_along(target_samples)) {
  sample_id <- target_samples[i]
  sample_data <- iris_modified[sample_id, ]
  contrib_data <- contributions_path[[paste0("sample_", sample_id)]]$path_contributions
  
  cat(sprintf("\n异常样本 #%d 诊断报告:\n", sample_id))
  cat(sprintf("异常分数: %.4f\n", contrib_data))
  
  # 找出贡献最大的特征
  top_feature <- names(which.max(contrib_data))
  top_contribution <- max(contrib_data)
  
  cat(sprintf("主要异常特征: %s (贡献度: %.1f%%)\n", top_feature, top_contribution * 100))
  cat(sprintf("该特征值: %.2f\n", sample_data[[top_feature]]))
  
  # 与正常值比较
  normal_mean <- mean(iris_modified[[top_feature]][-target_samples])
  cat(sprintf("正常值均值: %.2f\n", normal_mean))
  cat(sprintf("偏离程度: %.2f倍标准差\n", 
              abs(sample_data[[top_feature]] - normal_mean) / sd(iris_modified[[top_feature]])))
}

cat("\n分析完成！\n")
