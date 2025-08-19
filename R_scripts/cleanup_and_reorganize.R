# R脚本清理和重组工具
# ===================
# 
# 功能：清理R_scripts文件夹，整理NCA和QCA分析脚本
# 
# 作者：Claude Code Assistant
# 日期：2024年

# 设置工作目录
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

cat("=== R脚本清理和重组 ===\n")

# 获取当前目录所有R文件
r_files <- list.files(pattern = "\\.R$", full.names = FALSE)
cat("发现", length(r_files), "个R文件\n")

# 定义要保留的核心文件及其功能
keep_files <- list(
  "NCA_analysis.R" = "NCA必要条件分析（核心）",
  "qca_analysis_complete.R" = "QCA充分性分析（完整版）", 
  "execute_qca_analysis.R" = "QCA分析执行器",
  "cleanup_and_reorganize.R" = "清理重组工具（本脚本）"
)

# 检查文件内容，确定保留策略
cat("\n文件保留策略：\n")
for (file in names(keep_files)) {
  if (file.exists(file)) {
    cat("✓ 保留:", file, "-", keep_files[[file]], "\n")
  } else {
    cat("✗ 缺失:", file, "\n")
  }
}

# 删除重复或低质量的文件
delete_files <- c(
  "qca_analysis.R",      # 功能不完整，被complete版本替代
  "simple_qca_analysis.R" # 功能过于简化，不适用于正式分析
)

cat("\n删除文件：\n")
for (file in delete_files) {
  if (file.exists(file)) {
    file.remove(file)
    cat("✗ 删除:", file, "\n")
  }
}

cat("\n=== 清理完成 ===\n")

# 验证最终文件列表
final_files <- list.files(pattern = "\\.R$", full.names = FALSE)
cat("最终保留", length(final_files), "个R文件：\n")
for (file in final_files) {
  cat("-", file, "\n")
}