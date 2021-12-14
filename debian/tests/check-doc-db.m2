stdio << "checking if documentation database can be loaded ... "
-- check a key containing a UTF-8 character that is known to cause problems
-- when a gdbm database is loaded on the wrong architecture
if (needsPackage "Macaulay2Doc")#"raw documentation database"#?"Gröbner bases"
then (print "yes"; exit 0) else (print "no"; exit 1)
