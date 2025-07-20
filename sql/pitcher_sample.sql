SELECT
game_year,
pitcher
FROM db.statcast
WHERE game_year >= 2023
AND pitch_type IS NOT NULL 
AND pitch_type NOT in ('CS', 'FA', 'EP', 'IN', 'PO', 'FO', 'SC', 'KN', '', 'AB')
GROUP BY game_year, pitcher
HAVING (SUM(CASE WHEN pitch_type = 'SI' THEN 1 ELSE 0 END) / COUNT(*)) >= 0.10
AND (SUM(CASE WHEN pitch_type = 'FF' THEN 1 ELSE 0 END) / COUNT(*)) >= 0.10
AND COUNT(*) > 200
ORDER BY pitcher, game_year