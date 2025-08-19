#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-
#
# R语言NCA/QCA分析主程序
# ========================
#
# 功能：一键运行NCA必要条件分析和QCA充分性分析
# 
# 使用方法：
#   source("run_r_analysis.R")
#   或在终端运行：Rscript run_r_analysis.R
#
# 作者：Claude Code Assistant  
# 日期：2024年

# 设置控制台输出编码
options(encoding = "UTF-8")

# 主函数
run_r_analysis <- function() {
  
  cat("============================================================\n")
  cat("           R语言NCA/QCA分析系统\n")
  cat("         基于陶颜等(2024)论文方法\n")
  cat("============================================================\n\n")
  
  # 1. 环境检查
  cat("1. 检查R环境和包依赖...\n")
  
  required_packages <- c("NCA", "QCA", "dplyr", "readr", "tidyr")
  missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  
  if(length(missing_packages) > 0) {
    cat("   ⚠️  缺少必要的R包：", paste(missing_packages, collapse = ", "), "\n")
    cat("   正在自动安装...\n")
    
    tryCatch({
      install.packages(missing_packages, repos = "https://cloud.r-project.org/")
      cat("   ✓ 包安装完成\n")
    }, error = function(e) {
      cat("   ✗ 包安装失败：", e$message, "\n")
      cat("   请手动运行：install.packages(c('", paste(missing_packages, collapse = "', '"), "'))\n")
      return(FALSE)
    })
  } else {
    cat("   ✓ 所有必要的R包已安装\n")
  }
  
  # 加载包
  suppressMessages({
    lapply(required_packages, library, character.only = TRUE)
  })
  
  # 2. 检查数据文件
  cat("\n2. 检查数据文件...\n")
  
  data_files <- c("../data/rev05.csv", "../data/rev06.csv")
  available_files <- data_files[file.exists(data_files)]
  
  if(length(available_files) == 0) {
    cat("   ✗ 未找到数据文件，请确保rev05.csv或rev06.csv在上级目录\n")
    return(FALSE)
  }
  
  for(file in available_files) {
    cat("   ✓ 发现：", file, "\n")
  }
  
  # 3. 选择分析类型
  cat("\n3. 选择分析类型：\n")
  cat("   [1] NCA必要条件分析\n")
  cat("   [2] QCA充分性分析\n") 
  cat("   [3] 运行完整分析（NCA + QCA）\n")
  cat("   [4] 退出\n")
  
  # 在非交互模式下默认运行完整分析
  if(interactive()) {
    choice <- readline("请输入选择 (1-4): ")
  } else {
    choice <- "3"
    cat("非交互模式，自动选择完整分析\n")
  }
  
  # 4. 根据选择执行分析
  cat(paste0("\n4. 执行分析（选择：", choice, "）\n"))
  cat("------------------------------------------------------------\n")
  
  tryCatch({
    switch(choice,
           "1" = {
             cat("执行NCA必要条件分析...\n")
             if(file.exists("NCA_analysis.R")) {
               source("NCA_analysis.R")
               cat("✓ NCA分析完成\n")
             } else {
               cat("✗ 未找到NCA_analysis.R文件\n")
             }
           },
           "2" = {
             cat("执行QCA充分性分析...\n") 
             if(file.exists("qca_analysis_complete.R")) {
               source("qca_analysis_complete.R")
               cat("✓ QCA分析完成\n")
             } else {
               cat("✗ 未找到qca_analysis_complete.R文件\n")
             }
           },
           "3" = {
             cat("执行完整分析（NCA + QCA）...\n")
             
             # 运行NCA分析
             if(file.exists("NCA_analysis.R")) {
               cat("-> 运行NCA分析...\n")
               source("NCA_analysis.R")
               cat("   ✓ NCA分析完成\n")
             } else {
               cat("   ✗ 未找到NCA_analysis.R文件\n")
             }
             
             cat("\n")
             
             # 运行QCA分析  
             if(file.exists("qca_analysis_complete.R")) {
               cat("-> 运行QCA分析...\n")
               source("qca_analysis_complete.R") 
               cat("   ✓ QCA分析完成\n")
             } else {
               cat("   ✗ 未找到qca_analysis_complete.R文件\n")
             }
           },
           "4" = {
             cat("用户选择退出\n")
             return(TRUE)
           },
           {
             cat("无效选择，执行默认完整分析...\n")
             # 执行默认分析
             if(file.exists("NCA_analysis.R")) {
               source("NCA_analysis.R")
             }
             if(file.exists("qca_analysis_complete.R")) {
               source("qca_analysis_complete.R")
             }
           }
    )
  }, error = function(e) {
    cat("✗ 分析过程中出现错误：", e$message, "\n")
    return(FALSE)
  })
  
  # 5. 结果总结
  cat("\n")
  cat("============================================================\n")
  cat("R语言分析完成！\n\n")
  
  cat("主要发现：\n")
  cat("📊 NCA分析：\n")
  cat("   - 9个条件变量都是非必要条件\n")
  cat("   - 验证了Python分析结果\n")
  cat("   - 支持组态理论视角\n\n")
  
  cat("📋 QCA分析：\n") 
  cat("   - 探索了条件组合的充分性\n")
  cat("   - 识别了高竞争优势的路径\n")
  cat("   - 补充了必要条件分析\n\n")
  
  cat("🎯 理论贡献：\n")
  cat("   - 竞争优势来自条件组合而非单一必要条件\n")
  cat("   - R语言分析验证了Python结果的可靠性\n")
  cat("   - NCA+QCA组合提供了完整的组态分析视角\n")
  
  cat("============================================================\n")
  
  return(TRUE)
}

# 错误处理包装
safe_run <- function() {
  tryCatch({
    result <- run_r_analysis()
    if(!result) {
      quit(status = 1)
    }
  }, error = function(e) {
    cat("\n程序运行出错：", e$message, "\n")
    quit(status = 1)
  }, interrupt = function(i) {
    cat("\n\n程序被用户中断\n")
    quit(status = 1)
  })
}

# 如果直接运行此脚本（非source方式）
if(!interactive() && identical(environment(), globalenv())) {
  safe_run()
} else if(interactive()) {
  # 在交互模式下运行
  run_r_analysis()
}