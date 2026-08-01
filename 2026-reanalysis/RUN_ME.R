# ============================================================
#  これだけ Source すればいい（本命ランナー）
# ============================================================
# RStudio: このファイルを開いて Source / Run
# または Console に次を1行:
#   source("~/Documents/work-folder/Mas R Contents Improve/RUN_ME.R")
#
# やること: 本番 MAS を読んで munge → 掃除版 EFA
# やらないこと: 本番 contents.R の上書き
# 結果: 同じフォルダの out/
# ============================================================

source(file.path(
  path.expand("~/Documents/work-folder/Mas R Contents Improve"),
  "scr", "run_efa_after_mas_munge.R"
))
