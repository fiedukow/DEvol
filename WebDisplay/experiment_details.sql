SELECT ex.`id`, ex.`suite_id`, ep.`value_text`, ep2.`value_numeric`, ex.`start_timestamp`, ex.`end_timestamp`, `avg_best`, `sd_best`, `avg_mid`, `sd_mid` FROM `Experiment` ex
JOIN `ExperimentParameter` ep ON ep.`experiment_id`=ex.`id`
JOIN `ExperimentParameter` ep2 ON ep2.`experiment_id`=ex.`id`
JOIN (
	SELECT
    ex.`id` as eid, AVG(min_best.`min_best`) as avg_best, STDDEV(min_best.`min_best`) as sd_best
    FROM `Experiment` ex
	JOIN `Run` r ON r.`experiment_id`=ex.`id`
	JOIN
    (
		SELECT r.`id` as rid, MIN(s.`value_numeric`) as min_best 
        FROM `Run` r
        JOIN `{EX_ID}_Series` s ON s.`run_id`=r.`id`
        WHERE s.`name`="best values" AND r.`experiment_id`={EX_ID}
        GROUP BY r.`id`
	) min_best ON `min_best`.rid = r.`id`
	WHERE ex.`id`={EX_ID}
	GROUP BY ex.`id`
) bv ON ex.`id` = bv.`eid`
JOIN
(
	SELECT
    ex.`id` as eid, AVG(min_mid.`min_mid`) as avg_mid, STDDEV(min_mid.`min_mid`) as sd_mid
    FROM `Experiment` ex
	JOIN `Run` r ON r.`experiment_id`=ex.`id`
	JOIN
    (
		SELECT r.`id` as rid, MIN(s.`value_numeric`) as min_mid 
        FROM `Run` r
        JOIN `{EX_ID}_Series` s ON s.`run_id`=r.`id`
        WHERE s.`name`="mid values" AND r.`experiment_id`={EX_ID}
        GROUP BY r.`id`
	) min_mid ON `min_mid`.rid = r.`id`
	WHERE ex.`id`={EX_ID}
	GROUP BY ex.`id`
) bm ON ex.`id` = bm.`eid`
WHERE ex.`id`={EX_ID} and ep.`name`="fitness function" and ep2.`name`="dim"
