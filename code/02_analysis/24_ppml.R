## Paths are defined in code/00_setup.R (run from the repository root).
if (!exists("PROJ")) source("code/00_setup.R")

###############################################################################
#  PPML (Poisson Pseudo-Maximum Likelihood) with two-way fixed effects
#  Robustness check for "Rivalry, Capacity, and GEO Activity"
#
#  为什么要跑 PPML:
#    正文的 DID 用 y = log(1 + n_geo_entries)。Chen & Roth (2024) 指出,
#    log(1+y) 的系数不是尺度不变的 —— 换成 log(1+1000*y) 结果会变,
#    所以它不能被解释成"百分比效应"。PPML (Silva & Tenreyro 2006) 直接
#    对 count 建模 E[y_it | .] = exp(a_i + l_t + b*D_it), 系数天然就是
#    半弹性 (exp(b)-1 = 百分比变化), 而且对 y=0 完全不需要加常数。
#
#  ⚠️ 关键警告 (先看 Section 2 的诊断输出再解读结果):
#    加了国家固定效应以后, Poisson 似然会把"在样本期内从未有过一次
#    GEO entry"的国家整个丢掉 (perfect separation, a_i -> -inf)。
#    你的面板 97.4% 是 0, 172 个国家里只有 31 个曾经 entry 过。
#    所以 PPML 的实际估计样本 ≈ 1,465 obs / 31 国 / 13 个 treated,
#    而不是 OLS 的 8,211 obs / 172 国 / 29 个 treated。
#    → PPML 不是"同一个回归换个 link", 它回答的是一个更窄的问题:
#      在已经进入过 GEO 的国家内部, rivalry shock 有没有提高 entry 强度。
#      这其实正好对应你 H2 的 capacity-conditioning 逻辑。
#
#  Requirements: R >= 4.1, fixest >= 0.11
###############################################################################
## (workspace is not cleared: paths come from code/00_setup.R)
library(readr)
library(dplyr)
library(tidyr)
library(slider)
library(fixest)
library(ggplot2)

setFixest_notes(TRUE)   # 让 fixest 打印 separation 删掉了多少观测

PATH    <- PANEL
OUT_DIR <- OUT_FIG
START   <- 1970
END     <- 2020
THRESH  <- 0.2      # jump 阈值 (percentage points), 与正文一致


# ═══════════════════════════════════════════════════════════════════════════
# 1) 数据 + 处理组构造 (与 sun_abraham_did.R 完全一致)
# ═══════════════════════════════════════════════════════════════════════════

df0 <- read_csv(PATH, show_col_types = FALSE) %>%
  mutate(
    year          = as.integer(year),
    ccode         = as.integer(ccode),
    rivals_pct    = 100 * as.numeric(rivals_share_5yr_owner),
    n_geo_entries = as.numeric(n_geo_entries),
    y_log1p       = log1p(pmax(n_geo_entries, 0))
  ) %>%
  arrange(ccode, year)

df <- df0 %>%
  group_by(ccode) %>% arrange(year) %>%
  mutate(
    base_mean = slide_dbl(lag(rivals_pct, 1), ~mean(.x, na.rm = TRUE),
                          .before = 2, .complete = TRUE),
    jump      = rivals_pct - base_mean
  ) %>% ungroup()

cohort_df <- df %>%
  group_by(ccode) %>%
  summarise(cohort = suppressWarnings(
              min(year[!is.na(jump) & jump >= THRESH], na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(cohort = ifelse(is.infinite(cohort), 10000L, as.integer(cohort)))

df <- df %>% left_join(cohort_df, by = "ccode")

d <- df %>% filter(year >= START, year <= END)

unit_info <- d %>% distinct(ccode, cohort) %>%
  mutate(treated = cohort < 10000,
         ok      = treated & (cohort - START >= 5) & (END - cohort >= 5))

d <- d %>%
  filter(ccode %in% (unit_info %>% filter(ok | !treated) %>% pull(ccode))) %>%
  left_join(unit_info %>% select(ccode, ok, treated), by = "ccode") %>%
  mutate(
    treated_flag = as.integer(ok),
    post_flag    = as.integer(ok & year >= cohort),
    D            = treated_flag * post_flag,                  # 静态 DID 处理项
    rel_time     = ifelse(ok, year - cohort, NA_integer_),    # 事件时间
    lgdp         = log(gdp_kd),
    lpop         = log(pop_total),
    mil_bn       = mil_exp_cd / 1e9,
    # 事前能力 (H2): t-1 时点过去五年是否已有 payload
    cap_lag      = as.integer(coalesce(lag(payloads_5yr_owner), 0) > 0)
  )


# ═══════════════════════════════════════════════════════════════════════════
# 2) SEPARATION 诊断 —— 跑 PPML 之前必看
# ═══════════════════════════════════════════════════════════════════════════

ever <- d %>% group_by(ccode) %>%
  summarise(total = sum(n_geo_entries, na.rm = TRUE), .groups = "drop") %>%
  mutate(ever_entrant = total > 0)

d <- d %>% left_join(ever %>% select(ccode, ever_entrant), by = "ccode")

cat("\n══════ SEPARATION 诊断 ══════\n")
cat(sprintf("  OLS 样本:        N = %5d | 国家 = %3d | treated = %2d\n",
            nrow(d), n_distinct(d$ccode), sum(unit_info$ok)))
cat(sprintf("  DV 为 0 的比例:  %.1f%%\n", 100 * mean(d$n_geo_entries == 0)))
cat(sprintf("  PPML 有效样本:   N = %5d | 国家 = %3d | treated = %2d\n",
            sum(d$ever_entrant), sum(ever$ever_entrant),
            n_distinct(d$ccode[d$ok & d$ever_entrant])))
cat("  被 separation 丢掉的国家 = 窗口内从未 entry 的国家\n")
cat("  留下的 treated 国家: ",
    paste(sort(unique(d$country_name[d$ok & d$ever_entrant])), collapse = ", "),
    "\n══════════════════════════════\n\n")


# ═══════════════════════════════════════════════════════════════════════════
# 3) 描述性 PPML (对应正文 Table 2, 只有年份 FE)
#    —— 这一块不受 country-FE separation 影响, 样本几乎全保留
# ═══════════════════════════════════════════════════════════════════════════

p0 <- fepois(n_geo_entries ~ rivals_share_5yr_owner | year,
             data = d, cluster = ~ccode)
p1 <- fepois(n_geo_entries ~ rivals_share_5yr_owner + lgdp | year,
             data = d, cluster = ~ccode)
p2 <- fepois(n_geo_entries ~ rivals_share_5yr_owner + lgdp + lpop | year,
             data = d, cluster = ~ccode)
p3 <- fepois(n_geo_entries ~ rivals_share_5yr_owner + lgdp + lpop + mil_bn | year,
             data = d, cluster = ~ccode)

cat("\n=== A) 描述性 PPML: 年份 FE (Table 2 的 Poisson 版) ===\n")
etable(p0, p1, p2, p3, se.below = TRUE,
       dict = c(rivals_share_5yr_owner = "Rival share (5yr)",
                lgdp = "Log GDP", lpop = "Log population",
                mil_bn = "Military exp. (bn USD)",
                n_geo_entries = "Annual GEO entries"))


# ═══════════════════════════════════════════════════════════════════════════
# 4) 主结果: PPML 静态 DID (双向固定效应)
# ═══════════════════════════════════════════════════════════════════════════

m_ols  <- feols (y_log1p       ~ D | ccode + year, data = d, cluster = ~ccode)
m_ppml <- fepois(n_geo_entries ~ D | ccode + year, data = d, cluster = ~ccode)

# 让 OLS 跑在 PPML 的同一个子样本上, 才是苹果对苹果的比较
m_ols_sub <- feols(y_log1p ~ D | ccode + year,
                   data = filter(d, ever_entrant), cluster = ~ccode)

# 加控制变量 (注意: GDP/pop 缺失会再砍样本, 结果对此很敏感)
m_ppml_x <- fepois(n_geo_entries ~ D + lgdp + lpop | ccode + year,
                   data = d, cluster = ~ccode)

cat("\n=== B) 静态 DID: OLS log1p vs PPML ===\n")
etable(m_ols, m_ols_sub, m_ppml, m_ppml_x, se.below = TRUE,
       headers = c("OLS log1p (full)", "OLS log1p (ever-entrant)",
                   "PPML", "PPML + controls"))

b <- coef(m_ppml)["D"]
cat(sprintf("\nPPML 解读: beta = %.4f  →  exp(beta)-1 = %+.1f%% 的 GEO entry 数量变化\n",
            b, 100 * (exp(b) - 1)))
cat("  (OLS 的 log1p 系数 0.0611 不能这样直接读成 6.3%, 这正是 PPML 的意义)\n")


# ═══════════════════════════════════════════════════════════════════════════
# 5) PPML 事件研究 + Sun–Abraham (Poisson 版)
# ═══════════════════════════════════════════════════════════════════════════

# 5a) TWFE 事件研究。never-treated 的 rel_time 是 NA, fixest 会把它们
#     放进参照组 (基准期), 这正是我们要的。
d_es <- d %>% mutate(rel_bin = case_when(
  is.na(rel_time)   ~ NA_integer_,
  rel_time < -5     ~ -6L,       # bin 掉远端, 避免弱识别的端点系数
  rel_time >  5     ~  6L,
  TRUE              ~ as.integer(rel_time)))

es_ppml <- fepois(n_geo_entries ~ i(rel_bin, ref = -1) | ccode + year,
                  data = d_es, cluster = ~ccode)

# 5b) Sun–Abraham 交互加权, Poisson 版本 (fixest 的 sunab 支持 fepois)
sa_ppml <- fepois(n_geo_entries ~ sunab(cohort, year) | ccode + year,
                  data = d, cluster = ~ccode)

cat("\n=== C) Sun–Abraham (PPML) 汇总 ATT ===\n")
print(coeftable(summary(sa_ppml, agg = "ATT")))

cat("\n=== D) 事前趋势联合检验 (PPML, t = -5..-2) ===\n")
pre_nm <- grep("::-[2-5]$", names(coef(sa_ppml)), value = TRUE)
if (length(pre_nm)) print(wald(sa_ppml, keep = pre_nm))

# 5c) 画图
es_tab <- coeftable(es_ppml) %>% as.data.frame() %>%
  tibble::rownames_to_column("term") %>%
  filter(grepl("rel_bin::", term)) %>%
  transmute(rel = as.integer(sub(".*::", "", term)),
            est = Estimate, se = `Std. Error`) %>%
  bind_rows(tibble(rel = -1L, est = 0, se = 0)) %>%
  filter(rel >= -5, rel <= 5) %>% arrange(rel)

p <- ggplot(es_tab, aes(rel, est)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", colour = "grey70") +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                width = .2, colour = "firebrick") +
  geom_point(size = 2.4, colour = "firebrick") +
  labs(title    = "Event-Study: PPML (Poisson, two-way FE)",
       subtitle = "Outcome: GEO entry count | ref = t-1 | 95% CI, clustered by country",
       x = expression("Event time (year " - ~T[i] ~ ")"),
       y = "Coefficient (semi-elasticity)") +
  theme_minimal(base_size = 12)

ggsave(file.path(OUT_FIG, "event_study_ppml.pdf"), p, width = 8, height = 5)
ggsave(file.path(OUT_FIG, "event_study_ppml.png"), p, width = 8, height = 5, dpi = 300)
cat("\n  Saved: event_study_ppml.pdf / .png\n")


# ═══════════════════════════════════════════════════════════════════════════
# 6) H2: 按事前能力做异质性 (PPML)
# ═══════════════════════════════════════════════════════════════════════════

h_ppml <- fepois(n_geo_entries ~ D + D:cap_lag | ccode + year,
                 data = d, cluster = ~ccode)
cat("\n=== E) 异质性: 事前是否已有 GEO payload ===\n")
etable(h_ppml, se.below = TRUE)


# ═══════════════════════════════════════════════════════════════════════════
# 7) 稳健性: 阈值 / 窗口 / 无年份 FE
# ═══════════════════════════════════════════════════════════════════════════

cat("\n=== F) 阈值敏感性 (PPML) ===\n")
for (thr in c(0.1, 0.15, 0.2, 0.3, 0.5)) {
  coh_t <- df %>% group_by(ccode) %>%
    summarise(c2 = suppressWarnings(min(year[!is.na(jump) & jump >= thr], na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(c2 = ifelse(is.infinite(c2), 10000L, as.integer(c2)))
  dt <- d %>% select(-cohort) %>% left_join(coh_t, by = "ccode") %>%
    mutate(ok2 = c2 < 10000 & (c2 - START >= 5) & (END - c2 >= 5),
           D2  = as.integer(ok2 & year >= c2))
  mt <- tryCatch(fepois(n_geo_entries ~ D2 | ccode + year, data = dt,
                        cluster = ~ccode, notes = FALSE), error = function(e) NULL)
  if (!is.null(mt)) {
    ct <- coeftable(mt)
    cat(sprintf("  thr = %.2f | treated = %2d | beta = %+.4f (SE %.4f, p = %.3f)\n",
                thr, n_distinct(dt$ccode[dt$ok2]),
                ct["D2", 1], ct["D2", 2], ct["D2", 4]))
  }
}

# 只有国家 FE, 不吸收全球时间趋势 —— 仅供对照, 不可作为主结果
m_nofe <- fepois(n_geo_entries ~ D | ccode, data = d, cluster = ~ccode)
cat("\n=== G) 只有国家 FE (对照, 全球 GEO 扩张趋势未被吸收) ===\n")
print(coeftable(m_nofe))


cat("\n\nDone. 解读前请回看 Section 2 的 separation 诊断。\n")

###############################################################################
#  Stata 等价写法 (如果要放进 .do 流程):
#
#    ssc install ppmlhdfe
#    ppmlhdfe n_geo_entries D, absorb(ccode year) vce(cluster ccode)
#    * ppmlhdfe 会自动做 separation 检测 (Correia, Guimarães & Zylkin 2020),
#    * 比 poisson + i.ccode 稳健得多, 不要用 xtpoisson, fe 后再手动加虚拟变量。
#
#  推荐引用:
#    Silva & Tenreyro (2006), "The Log of Gravity", REStat.
#    Correia, Guimarães & Zylkin (2020), "Fast Poisson estimation with
#      high-dimensional fixed effects", Stata Journal.
#    Chen & Roth (2024), "Logs with Zeros?", QJE.  ← 用来正当化这个稳健性检验
#    Cohn, Liu & Wardlaw (2022), "Count (and count-like) data in finance", JFE.
###############################################################################
