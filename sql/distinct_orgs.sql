SELECT * FROM org;

SELECT orgRef, COUNT(*) FROM repo GROUP BY orgRef;

SELECT orgRef2, lastRun, COUNT(*) FROM repoQuery GROUP BY orgRef2, lastRun;
