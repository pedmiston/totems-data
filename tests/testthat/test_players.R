# context("Players")
#
# test_that("Isolated 50 minute participants have Ancestor == 1", {
#   I50 <- read_table("Table_Player") %>%
#     replace_id_group() %>%
#     replace_id_player() %>%
#     label_strategy() %>%
#     label_session_duration() %>%
#     filter(Strategy == "Isolated", SessionDuration == 50)
#
#   expect_true(all(I50$Ancestor == 1))
# })
