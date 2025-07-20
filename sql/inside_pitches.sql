WITH pitch_counts AS (
	SELECT pitcher, game_year, COUNT(*) as N
    FROM db.statcast
	WHERE game_year = 2025
    GROUP BY pitcher 
    HAVING COUNT(*) > 300
)

SELECT player_name, mlb.pitcher, p_throws, mlb.game_year, events, pitch_type, woba_value, woba_denom, estimated_woba_using_speedangle, delta_run_exp
FROM db.statcast mlb
JOIN pitch_counts p 
	ON mlb.pitcher = p.pitcher 
	AND mlb.game_year = p.game_year
WHERE mlb.game_year = 2025
AND (
	(zone in (3, 6, 9, 12, 14) and p_throws = "L" and stand = "L")
    OR (zone in (1, 4, 7, 11, 13) and p_throws = "R" and stand = "R")
)