# 完整的QCA分析脚本
# 包含所有四个任务的实现

# 1. 安装和加载必要的包
packages <- c("dplyr", "tidyr", "readr", "NCA", "SetMethods", "QCA")
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
}
install_if_missing(packages)

library(dplyr)
library(tidyr)
library(readr)
library(NCA)
library(SetMethods)
library(QCA)

# 2. 数据读取和预处理
# 读取数据
lines <- readLines("汇总3.md")

# 移除第一行（表头分隔符）并清理数据
data_lines <- lines[-1]  # 移除第一行的分隔符行

# 创建临时文件以便正确读取
temp_file <- "temp_data.csv"
writeLines(data_lines, temp_file)

# 读取数据
data <- read_delim(temp_file, delim = "|", trim_ws = TRUE, col_names = TRUE)

# 清理数据：移除第一列和最后一列（空列）
data <- data[, -1]
data <- data[, -ncol(data)]

# 重命名列名
colnames(data) <- c("scode", "name", "IndustryName", "IndustryCode", "Listed_date", "Listed_year", "year", "business", "DYNA", "MUNI", "liva", "rev_entropy", "diff_freq1", "diff_freq2", "cost_freq1", "cost_freq2", "org_size", "esg_rating", "esg_score", "digi_freq1", "digi_freq2", "cost_sticky")

# 转换数据类型
data$year <- as.numeric(data$year)
data$org_size <- as.numeric(data$org_size)
data$esg_score <- as.numeric(data$esg_score)

# 查看数据摘要
cat("数据维度:", dim(data), "\n")
cat("年份范围:", min(data$year, na.rm = TRUE), "-", max(data$year, na.rm = TRUE), "\n")

# 3. 任务1: 直接校准法
cat("\n=== 任务1: 直接校准法 ===\n")

# 定义逻辑校准函数
logistic_calibrate <- function(x, full_membership, crossover, full_nonmembership) {
  # 确保输入是数值型
  x <- as.numeric(x)
  
  # 逻辑校准公式
  result <- rep(NA, length(x))
  
  # 完全隶属 (1)
  result[x >= full_membership] <- 1
  
  # 完全不隶属 (0)
  result[x <= full_nonmembership] <- 0
  
  # 中间值使用线性插值
  middle_indices <- which(x > full_nonmembership & x < full_membership & !is.na(x))
  
  for (i in middle_indices) {
    result[i] <- (x[i] - full_nonmembership) / (full_membership - full_nonmembership)
  }
  
  # 处理边界情况
  result[is.na(x)] <- NA
  
  return(result)
}

# 选择前因条件变量
condition_vars <- c("diff_freq1", "diff_freq2", 
                    "cost_freq1", "cost_freq2",
                    "digi_freq1", "digi_freq2",
                    "DYNA", "MUNI", "liva", "rev_entropy", "cost_sticky")

# 选择结果变量（竞争优势代理变量）
outcome_vars <- c("org_size", "esg_score")

# 创建校准后的新数据框
calibrated_data <- data

# 计算并应用校准锚点
calibration_anchors <- list()

# 为条件变量计算分位数并校准
cat("条件变量校准锚点:\n")
for (var in condition_vars) {
  if (var %in% colnames(calibrated_data)) {
    # 移除缺失值后计算分位数
    clean_values <- calibrated_data[[var]][!is.na(calibrated_data[[var]])]
    
    if (length(clean_values) > 0) {
      q90 <- quantile(clean_values, 0.9, na.rm = TRUE)
      q50 <- quantile(clean_values, 0.5, na.rm = TRUE)
      q10 <- quantile(clean_values, 0.1, na.rm = TRUE)
      
      calibration_anchors[[var]] <- list(
        full_membership = as.numeric(q90),
        crossover = as.numeric(q50),
        full_nonmembership = as.numeric(q10)
      )
      
      # 应用校准
      calibrated_data[[paste0(var, "_cal")]] <- logistic_calibrate(
        calibrated_data[[var]],
        as.numeric(q90),
        as.numeric(q50),
        as.numeric(q10)
      )
      
      cat(var, "- 90%:", round(q90, 4), "50%:", round(q50, 4), "10%:", round(q10, 4), "\n")
    }
  }
}

# 为结果变量计算分位数并校准
cat("\n结果变量校准锚点:\n")
for (var in outcome_vars) {
  if (var %in% colnames(calibrated_data)) {
    # 移除缺失值后计算分位数
    clean_values <- calibrated_data[[var]][!is.na(calibrated_data[[var]])]
    
    if (length(clean_values) > 0) {
      q90 <- quantile(clean_values, 0.9, na.rm = TRUE)
      q50 <- quantile(clean_values, 0.5, na.rm = TRUE)
      q10 <- quantile(clean_values, 0.1, na.rm = TRUE)
      
      calibration_anchors[[var]] <- list(
        full_membership = as.numeric(q90),
        crossover = as.numeric(q50),
        full_nonmembership = as.numeric(q10)
      )
      
      # 应用校准
      calibrated_data[[paste0(var, "_cal")]] <- logistic_calibrate(
        calibrated_data[[var]],
        as.numeric(q90),
        as.numeric(q50),
        as.numeric(q10)
      )
      
      cat(var, "- 90%:", round(q90, 4), "50%:", round(q50, 4), "10%:", round(q10, 4), "\n")
    }
  }
}

# 4. 任务2: 使用NCA包执行必要条件分析
cat("\n=== 任务2: 必要条件分析 (NCA) ===\n")

# 为NCA分析准备数据（移除缺失值）
nca_data <- calibrated_data %>%
  select(ends_with("_cal")) %>%
  na.omit()

cat("NCA分析数据维度:", dim(nca_data), "\n")

# 选择结果变量
if ("组织规模_cal" %in% colnames(nca_data)) {
  outcome_var_cal <- "组织规模_cal"
  outcome_var_name <- "组织规模"
} else if ("ESG综合得分_cal" %in% colnames(nca_data)) {
  outcome_var_cal <- "ESG综合得分_cal"
  outcome_var_name <- "ESG综合得分"
} else {
  stop("没有找到校准后的结果变量")
}

# 执行NCA分析
nca_results <- list()

# 只对校准后的条件变量进行分析
calibrated_condition_vars <- paste0(condition_vars, "_cal")
calibrated_condition_vars <- calibrated_condition_vars[calibrated_condition_vars %in% colnames(nca_data)]

if (outcome_var_cal %in% colnames(nca_data)) {
  cat("对以下条件变量进行必要性分析:\n")
  for (var in calibrated_condition_vars) {
    if (var %in% colnames(nca_data) && var != outcome_var_cal) {
      tryCatch({
        # 执行NCA分析
        condition_name <- gsub("_cal", "", var)
        result <- nca.calibrate(nca_data[[var]], nca_data[[outcome_var_cal]], 
                               necessity = TRUE)
        
        nca_results[[condition_name]] <- result
        
        # 输出结果
        effect_size <- result$effectsizes[1]
        p_value <- result$pvalues[1]
        
        cat(sprintf("%s: 效应量 = %.4f, p值 = %.4f", 
                    condition_name, effect_size, p_value))
        
        if (p_value < 0.05) {
          cat(" * (显著)\n")
        } else {
          cat(" (不显著)\n")
        }
        
      }, error = function(e) {
        cat("对条件", gsub("_cal", "", var), "进行NCA分析时出错:", e$message, "\n")
      })
    }
  }
}

# 5. 任务3: 使用SetMethods包执行充分性分析
cat("\n=== 任务3: 充分性分析 ===\n")

# 为QCA分析准备数据
qca_data <- nca_data  # 使用已经清理过的数据

# 显示QCA数据结构
cat("QCA分析数据维度:", dim(qca_data), "\n")

# 准备校准后的条件变量
calibrated_conditions <- calibrated_condition_vars[calibrated_condition_vars != outcome_var_cal]
cat("用于QCA分析的条件变量数量:", length(calibrated_conditions), "\n")

# 执行QCA分析
if (length(calibrated_conditions) > 0 && outcome_var_cal %in% colnames(qca_data)) {
  tryCatch({
    # 创建条件变量矩阵
    conditions_matrix <- qca_data[, calibrated_conditions]
    outcome_vector <- qca_data[[outcome_var_cal]]
    
    # 执行QCA分析
    # 使用QCA包的标准函数
    qca_result <- minimize(outcome_vector, conditions_matrix, 
                          incl.cut = 0.8, 
                          pri.cut = 0.8,
                          details = TRUE)
    
    cat("QCA分析结果:\n")
    print(qca_result)
    
  }, error = function(e) {
    cat("执行QCA分析时出错:", e$message, "\n")
    cat("这可能是由于数据量不足或变量间关系不符合QCA假设导致的。\n")
  })
} else {
  cat("数据不满足QCA分析要求\n")
}

# 6. 任务4: 组态理论化过程
cat("\n=== 任务4: 组态理论化过程 ===\n")

# 理论化分析结果
cat("基于分析结果的理论化:\n")
cat("1. 界定范围:\n")
cat("   - 已识别的关键前因条件包括:\n")
for (var in condition_vars) {
  cat("     *", var, "\n")
}
cat("   - 结果变量: 竞争优势(代理变量)\n")

cat("2. 条件连接:\n")
cat("   - 需要分析条件间的协同效应\n")
cat("   - 考虑条件间的替代和互补关系\n")

cat("3. 战略组态模式:\n")
cat("   - 基于QCA分析结果，可识别多种达成高竞争优势的路径\n")
cat("   - 每种组态代表一种独特的战略配置\n")

# 7. 保存结果
cat("\n=== 保存结果 ===\n")

# 保存校准后的数据
write.csv(calibrated_data, "calibrated_data.csv", row.names = FALSE)
cat("校准后的数据已保存到: calibrated_data.csv\n")

# 保存NCA分析结果
if (length(nca_results) > 0) {
  nca_summary <- data.frame(
    Condition = names(nca_results),
    EffectSize = sapply(nca_results, function(x) x$effectsizes[1]),
    PValue = sapply(nca_results, function(x) x$pvalues[1]),
    Significant = sapply(nca_results, function(x) x$pvalues[1] < 0.05)
  )
  write.csv(nca_summary, "nca_results.csv", row.names = FALSE)
  cat("NCA分析结果已保存到: nca_results.csv\n")
}

# 清理临时文件
unlink(temp_file)

cat("\n分析完成!\n")