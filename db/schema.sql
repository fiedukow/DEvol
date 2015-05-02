START TRANSACTION;

CREATE TABLE `Suite` (
	`id` INTEGER NOT NULL PRIMARY KEY AUTO_INCREMENT,
    `name` TEXT NOT NULL,
    `description` TEXT
);

CREATE TABLE `Experiment` (
	`id`	INTEGER NOT NULL PRIMARY KEY AUTO_INCREMENT,
    `suite_id` INTEGER NOT NULL,
	`start_timestamp`	INTEGER NOT NULL,
	`end_timestamp`	INTEGER NOT NULL,
    FOREIGN KEY(suite_id) REFERENCES Suite(id) ON DELETE CASCADE
);

CREATE TABLE `Run` (
	`id`	INTEGER PRIMARY KEY AUTO_INCREMENT,
	`experiment_id`	INTEGER NOT NULL,
	`start_timestamp` 	INTEGER NOT NULL,
	`end_timestamp`	INTEGER NOT NULL,
	FOREIGN KEY(experiment_id) REFERENCES Experiment(id) ON DELETE CASCADE
);

CREATE TABLE `Series` (
	`id`	INTEGER NOT NULL PRIMARY KEY AUTO_INCREMENT,
	`run_id`	INTEGER NOT NULL,
	`name`	TEXT NOT NULL,
	`point`	TEXT,
	`value_numeric`	DOUBLE,
    `value_text` TEXT,
	`order`	INTEGER NOT NULL,
	FOREIGN KEY(run_id) REFERENCES Run(id) ON DELETE CASCADE
);

CREATE TABLE `ExperimentParameter` (
	`experiment_id`	INTEGER NOT NULL,
	`name`	VARCHAR(128) NOT NULL,
	`value_numeric`	DOUBLE,
	`value_text`	TEXT NOT NULL,
	PRIMARY KEY(experiment_id,name),
	FOREIGN KEY(experiment_id) REFERENCES Experiment(id) ON DELETE CASCADE
);

CREATE TABLE `SuiteParameter` (
	`suite_id`	INTEGER NOT NULL,
	`name`	VARCHAR(128) NOT NULL,
	`value_numeric`	DOUBLE,
	`value_text`	TEXT NOT NULL,
	PRIMARY KEY(suite_id,name),
	FOREIGN KEY(suite_id) REFERENCES Suite(id) ON DELETE CASCADE
);

COMMIT;

DROP TABLE `SuiteParameter`;
DROP TABLE `ExperimentParameter`;
DROP TABLE `Series`;
DROP TABLE `Run`;
DROP TABLE `Experiment`;
DROP TABLE `Suite`;