SELECT * FROM org;

SELECT orgRef, COUNT(*) FROM repo GROUP BY orgRef;

SELECT orgRef, lastRun, COUNT(*) FROM repoQuery GROUP BY orgRef, lastRun;
