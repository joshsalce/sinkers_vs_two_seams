SELECT
player_name,
pitcher,
game_year,
SUM(CASE WHEN bb_type = "ground_ball" THEN 1 ELSE 0 END) / SUM(bip) AS GB_pct
FROM db.statcast
WHERE game_year = 2025
AND bip IS NOT NULL
GROUP BY player_name, pitcher, game_year
HAVING sum(bip) > 50