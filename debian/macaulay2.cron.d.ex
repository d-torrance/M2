#
# Regular cron jobs for the macaulay2 package
#
0 4	* * *	root	[ -x /usr/bin/macaulay2_maintenance ] && /usr/bin/macaulay2_maintenance
