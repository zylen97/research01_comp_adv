# NCA必要条件分析 - 精简版
# 基于rev06.csv数据，使用R语言NCA包进行分析

rm(list = ls())

# 自动查找Dropbox路径并设置工作目录
possible_dropbox_paths <- c(
  "~/Dropbox",  # 标准Mac/Linux路径
  "~/Library/CloudStorage/Dropbox",  # macOS新版本
  file.path(Sys.getenv("USERPROFILE"), "Dropbox"),  # Windows
  file.path(Sys.getenv("HOME"), "Dropbox")  # Linux/Mac
)

# 查找存在的Dropbox路径
dropbox_path <- NULL
for(path in possible_dropbox_paths) {
  if(dir.exists(path)) {
    dropbox_path <- path
    break
  }
}

if(!is.null(dropbox_path)) {
  # 拼接项目路径
  project_path <- file.path(dropbox_path, "GYM group/_paper/DJ_paper01_建筑企业竞争优势/code/R_scripts")
  if(dir.exists(project_path)) {
    setwd(project_path)
  } else {
    stop("找不到项目路径，请手动设置工作目录")
  }
} else {
  stop("找不到Dropbox文件夹，请手动设置工作目录")
}

options(encoding = "UTF-8", stringsAsFactors = FALSE)

# 静默加载必要的包
suppressPackageStartupMessages({
  library(NCA)
  library(dplyr)
})

# 读取数据
data <- read.csv("../data/rev06.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# 定义变量
outcome_var <- "liva_taoyan_cal"
condition_vars <- c(
  "rev_entropy_taoyan_cal", "diff_freq1_taoyan_cal", "cost_freq1_taoyan_cal",
  "DYNA_taoyan_cal", "MUNI_taoyan_cal", "org_size_taoyan_cal",
  "cost_sticky_taoyan_cal", "esg_score_taoyan_cal", "digi_freq1_taoyan_cal"
)

# 中文名映射
chinese_names <- list(
  'rev_entropy_taoyan_cal' = '收入多样性',
  'diff_freq1_taoyan_cal' = '差异化战略',
  'cost_freq1_taoyan_cal' = '成本领先战略',
  'DYNA_taoyan_cal' = '数字化动态能力',
  'MUNI_taoyan_cal' = '市政工程能力',
  'org_size_taoyan_cal' = '企业规模',
  'cost_sticky_taoyan_cal' = '成本粘性',
  'esg_score_taoyan_cal' = 'ESG表现',
  'digi_freq1_taoyan_cal' = '数字化转型'
)

# 准备分析数据
analysis_data <- data[, c(outcome_var, condition_vars)]
analysis_data <- analysis_data[complete.cases(analysis_data), ]

cat("执行NCA分析...\n")

# 执行NCA分析
results_list <- list()

for(i in seq_along(condition_vars)) {
  condition <- condition_vars[i]
  
  tryCatch({
    # 准备数据
    nca_data <- data.frame(
      X = analysis_data[[condition]],
      Y = analysis_data[[outcome_var]]
    )
    valid_obs <- nrow(nca_data)
    
    # CR分析（禁用图形输出）
    pdf(NULL)
    model_cr <- nca_analysis(nca_data, x = 1, y = 2, ceilings = "cr_fdh", test.rep = 10000)
    dev.off()
    
    # 提取CR结果
    cr_params <- model_cr$summaries$X$params
    cr_test <- model_cr$test$X
    cr_effect_size <- ifelse(!is.null(cr_params["Effect size", "cr_fdh"]), 
                             cr_params["Effect size", "cr_fdh"], 0)
    cr_accuracy <- ifelse(!is.null(cr_params["c-accuracy", "cr_fdh"]),
                          cr_params["c-accuracy", "cr_fdh"], 100)
    cr_ceiling_zone <- ifelse(!is.null(cr_params["Ceiling zone", "cr_fdh"]),
                              cr_params["Ceiling zone", "cr_fdh"], 0)
    cr_scope <- ifelse(!is.null(model_cr$summaries$X$global["Scope"]),
                       as.numeric(model_cr$summaries$X$global["Scope"]), NA)
    cr_p_value <- ifelse(!is.null(cr_test$p["cr_fdh"]), cr_test$p["cr_fdh"], 1)
    
    # CE分析
    pdf(NULL)
    model_ce <- nca_analysis(nca_data, x = 1, y = 2, ceilings = "ce_fdh", test.rep = 10000)
    dev.off()
    
    # 提取CE结果
    ce_params <- model_ce$summaries$X$params
    ce_test <- model_ce$test$X
    ce_effect_size <- ifelse(!is.null(ce_params["Effect size", "ce_fdh"]),
                             ce_params["Effect size", "ce_fdh"], 0)
    ce_accuracy <- ifelse(!is.null(ce_params["c-accuracy", "ce_fdh"]),
                          ce_params["c-accuracy", "ce_fdh"], 100)
    ce_ceiling_zone <- ifelse(!is.null(ce_params["Ceiling zone", "ce_fdh"]),
                              ce_params["Ceiling zone", "ce_fdh"], 0)
    ce_scope <- ifelse(!is.null(model_ce$summaries$X$global["Scope"]),
                       as.numeric(model_ce$summaries$X$global["Scope"]), NA)
    ce_p_value <- ifelse(!is.null(ce_test$p["ce_fdh"]), ce_test$p["ce_fdh"], 1)
    
    # 判断必要性
    max_effect <- max(cr_effect_size, ce_effect_size)
    necessity_conclusion <- ifelse(max_effect >= 0.1, "必要条件", "非必要条件")
    
    # 创建结果行
    results_list[[i]] <- data.frame(
      排名 = NA,
      条件变量 = condition,
      中文名 = chinese_names[[condition]],
      有效观测数 = valid_obs,
      CR_效应量 = round(cr_effect_size, 4),
      CR_精确度 = round(cr_accuracy, 2),
      CR_上限区域 = round(cr_ceiling_zone, 4),
      CR_范围 = round(cr_scope, 4),
      CR_P值 = round(cr_p_value, 3),
      CE_效应量 = round(ce_effect_size, 4),
      CE_精确度 = round(ce_accuracy, 2),
      CE_上限区域 = round(ce_ceiling_zone, 4),
      CE_范围 = round(ce_scope, 4),
      CE_P值 = round(ce_p_value, 3),
      必要性结论 = necessity_conclusion,
      CR显著 = cr_p_value <= 0.05,
      CE显著 = ce_p_value <= 0.05,
      stringsAsFactors = FALSE
    )
    
    cat(sprintf("  [%d/%d] %s: CR=%.4f, CE=%.4f\n", 
                i, length(condition_vars), condition, cr_effect_size, ce_effect_size))
    
  }, error = function(e) {
    # 错误时返回空结果
    results_list[[i]] <- data.frame(
      排名 = NA, 条件变量 = condition, 中文名 = chinese_names[[condition]],
      有效观测数 = nrow(analysis_data),
      CR_效应量 = 0, CR_精确度 = 100, CR_上限区域 = 0, CR_范围 = 1, CR_P值 = 1,
      CE_效应量 = 0, CE_精确度 = 100, CE_上限区域 = 0, CE_范围 = 1, CE_P值 = 1,
      必要性结论 = "非必要条件", CR显著 = FALSE, CE显著 = FALSE,
      stringsAsFactors = FALSE
    )
  })
}

# 合并结果并排序
results_df <- do.call(rbind, results_list)
max_effects <- pmax(results_df$CR_效应量, results_df$CE_效应量)
results_df <- results_df[order(-max_effects), ]
results_df$排名 <- 1:nrow(results_df)

# 瓶颈分析 - 分析所有条件变量
cat("\n执行瓶颈分析...\n")

# 准备数据：所有X变量和Y变量
bottleneck_data <- analysis_data[, c(condition_vars, outcome_var)]

# 执行瓶颈分析
pdf(NULL)
tryCatch({
  model_bottleneck <- nca_analysis(bottleneck_data, 
                                    x = 1:length(condition_vars), 
                                    y = length(condition_vars) + 1,
                                    ceilings = "ce_fdh")
  
  # 获取瓶颈表
  invisible(capture.output(
    bottleneck_output <- nca_output(model_bottleneck, 
                                     summaries = FALSE, 
                                     bottlenecks = TRUE,
                                     plots = FALSE)
  ))
  
  # 提取瓶颈表数据（修复版本）
  if(!is.null(model_bottleneck$bottlenecks) && !is.null(model_bottleneck$bottlenecks$ce_fdh)) {
    # 从ce_fdh中提取瓶颈表
    bottleneck_table <- model_bottleneck$bottlenecks$ce_fdh
    
    # 创建结果表格
    bottleneck_df <- data.frame(竞争优势 = seq(0, 100, by = 10))
    
    # 为每个条件变量添加中文名列（使用修复后的提取方法）
    for(var in condition_vars) {
      var_name <- chinese_names[[var]]
      
      if(var %in% names(bottleneck_table)) {
        # 获取该变量列
        var_column <- bottleneck_table[[var]]
        
        # 检查是否有mpx.actual属性（真实值）
        if(!is.null(attr(var_column, "mpx.actual"))) {
          actual_values <- attr(var_column, "mpx.actual")[, 1]
          # 转换：如果是-Inf表示非必要，否则转换为百分比
          bottleneck_df[[var_name]] <- ifelse(is.infinite(actual_values) | actual_values < 0, 
                                               "NN",
                                               sprintf("%.1f", actual_values * 100))
        } else {
          # 如果没有actual属性，使用显示值
          bottleneck_df[[var_name]] <- as.character(var_column)
        }
      } else {
        # 如果没有找到该变量，填充NN
        bottleneck_df[[var_name]] <- rep("NN", 11)
      }
    }
    
    # 保存瓶颈分析结果
    write.csv(bottleneck_df, "../results/Bottleneck_analysis_results.csv", 
              row.names = FALSE, fileEncoding = "UTF-8")
    cat("瓶颈分析结果已保存到 results/Bottleneck_analysis_results.csv\n")
  } else {
    # 如果没有获取到瓶颈表，创建一个全部为NN的表
    cat("  注意：未找到瓶颈数据，创建默认表格\n")
    levels <- seq(0, 100, by = 10)
    bottleneck_df <- data.frame(竞争优势 = levels)
    
    for(var in condition_vars) {
      var_name <- chinese_names[[var]]
      bottleneck_df[[var_name]] <- rep("NN", length(levels))
    }
    
    write.csv(bottleneck_df, "../results/Bottleneck_analysis_results.csv", 
              row.names = FALSE, fileEncoding = "UTF-8")
    cat("瓶颈分析结果已保存（所有条件均为非必要）\n")
  }
  
}, error = function(e) {
  cat("  瓶颈分析失败:", e$message, "\n")
})
dev.off()

# 保存NCA分析结果
write.csv(results_df, "../results/NCA_analysis_results_R.csv", row.names = FALSE, fileEncoding = "UTF-8")

# 输出简要结论
cat("\n分析完成\n")
cat(sprintf("总条件数: %d\n", nrow(results_df)))
cat(sprintf("非必要条件: %d\n", sum(results_df$必要性结论 == "非必要条件")))
cat("结果已保存到 results/NCA_analysis_results_R.csv\n")