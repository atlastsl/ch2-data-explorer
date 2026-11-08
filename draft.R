
SLB_final_mprc_db |>
  filter(spt_failed_dum == 0) |>
  summarise(
    q5 = quantile(ret, 0.05, na.rm = T),
    med = median(ret, na.rm = T),
    avg = mean(ret, na.rm = T),
    q95 = quantile(ret, 0.95, na.rm = T),
    .by = c("post_obs_date_num")
  ) |>
ggplot(aes(x = post_obs_date_num)) +
  geom_ribbon(aes(ymin = q5, ymax = q95), fill = "deepskyblue", alpha = 0.15) +
  geom_line(aes(y = q5), color = "deepskyblue3", linetype = "dashed", linewidth = 0.5) +
  geom_line(aes(y = q95), color = "deepskyblue3", linetype = "dashed", linewidth = 0.5) +
  geom_line(aes(y = avg), color = "navyblue", linewidth = 1) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  geom_text(
    aes(x = 0, y = max(q95), label = "Observation date"), 
    color = "red", angle = 90, vjust = -0.5, hjust = 1
  ) +
  labs(
    x = "Months after/before KPI observation date",
    y = "Mean SLB returns"
  ) +
  theme_minimal()

SLB_final_mprc_db |>
  filter(spt_failed_dum == 1) |>
  summarise(
    q5 = quantile(ret, 0.05, na.rm = T),
    med = median(ret, na.rm = T),
    avg = mean(ret, na.rm = T),
    q95 = quantile(ret, 0.95, na.rm = T),
    .by = c("post_obs_date_num")
  ) |>
  ggplot(aes(x = post_obs_date_num)) +
  geom_ribbon(aes(ymin = q5, ymax = q95), fill = "deepskyblue", alpha = 0.15) +
  geom_line(aes(y = q5), color = "deepskyblue3", linetype = "dashed", linewidth = 0.5) +
  geom_line(aes(y = q95), color = "deepskyblue3", linetype = "dashed", linewidth = 0.5) +
  geom_line(aes(y = avg), color = "navyblue", linewidth = 1) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  geom_text(
    aes(x = 0, y = max(q95), label = "Observation date"), 
    color = "red", angle = 90, vjust = -0.5, hjust = 1
  ) +
  labs(
    x = "Months after/before KPI observation date",
    y = "Mean SLB returns"
  ) +
  theme_minimal()

SLB_final_mprc_db |>
  summarise(
    q5 = quantile(ret, 0.05, na.rm = T),
    med = median(ret, na.rm = T),
    avg = mean(ret, na.rm = T),
    q95 = quantile(ret, 0.95, na.rm = T),
    .by = c("post_obs_date_num")
  ) |>
  ggplot(aes(x = post_obs_date_num)) +
  geom_ribbon(aes(ymin = q5, ymax = q95), fill = "deepskyblue", alpha = 0.15) +
  geom_line(aes(y = q5), color = "deepskyblue3", linetype = "dashed", linewidth = 0.5) +
  geom_line(aes(y = q95), color = "deepskyblue3", linetype = "dashed", linewidth = 0.5) +
  geom_line(aes(y = avg), color = "navyblue", linewidth = 1) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  geom_text(
    aes(x = 0, y = max(q95), label = "Observation date"), 
    color = "red", angle = 90, vjust = -0.5, hjust = 1
  ) +
  labs(
    x = "Months after/before KPI observation date",
    y = "Mean SLB returns"
  ) +
  theme_minimal()


# fit1 <- felp_model(
#   dataset = SLB_final_wprc_db |> filter(!is.na(spt_pnlt_rwrd_num), !is.na(ret), post_obs_date_num <= 8.1, post_obs_date_num >= -7.1), 
#   Y = c("ret"), 
#   X = c(
#     "spt_pnlt_rwrd_num",
#     "post_obs_date_dum",
#     "spt_pnlt_rwrd_num:post_obs_date_dum"
#   ),
#   fe = c("obs_date"),
#   cl = c("cusip_id")
# )

# summary(fit1)