use evaluate;

header "
#ifdef HAVE_TIME_H
 #include <time.h>
#endif
";

structtm := Type "struct tm";
structtmstar := Pointer "struct tm *";

time_t := integerType "time_t";
toTimeT(x:ZZcell):time_t := Ccode(time_t, "mpz_get_si(", x.v, ")");
toExpr(t:time_t):Expr := Expr(ZZcell(toInteger(long(t))));

tmtocaltime(s:structtmstar):Expr := (
    t := newHashTable(hashTableClass, nothingClass);
    storeInHashTable(t, toExpr("second"),
	toExpr(Ccode(int, s, "->tm_sec")));
    storeInHashTable(t, toExpr("minute"),
	toExpr(Ccode(int, s, "->tm_min")));
    storeInHashTable(t, toExpr("hour"),
	toExpr(Ccode(int, s, "->tm_hour")));
    storeInHashTable(t, toExpr("day of month"),
	toExpr(Ccode(int, s, "->tm_mday")));
    storeInHashTable(t, toExpr("month"),
	toExpr(Ccode(int, s, "->tm_mon")));
    storeInHashTable(t, toExpr("year"),
	toExpr(Ccode(int, s, "->tm_year")));
    storeInHashTable(t, toExpr("day of week"),
	toExpr(Ccode(int, s, "->tm_wday")));
    storeInHashTable(t, toExpr("day of year"),
	toExpr(Ccode(int, s, "->tm_yday")));
    storeInHashTable(t, toExpr("daylight saving time"),
	toExpr(Ccode(int, s, "->tm_isdst")));
    Expr(t)
);

zerostructtm():structtm := (
    declarations "struct tm result = {0};";
    Ccode(structtm, "result")
);

getcaltimevalue(t:HashTable, key:string):int := (
    val := lookup(t, toExpr(key));
    when val is n:ZZcell do toInt(n)
    else 0
);

caltimetotm(t:HashTable):structtm := (
    s := zerostructtm();
    Ccode(void, s, ".tm_sec = ", getcaltimevalue(t, "second"));
    Ccode(void, s, ".tm_min = ", getcaltimevalue(t, "minute"));
    Ccode(void, s, ".tm_hour = ", getcaltimevalue(t, "hour"));
    Ccode(void, s, ".tm_mday = ", getcaltimevalue(t, "day of month"));
    Ccode(void, s, ".tm_mon = ", getcaltimevalue(t, "month"));
    Ccode(void, s, ".tm_year = ", getcaltimevalue(t, "year"));
    Ccode(void, s, ".tm_wday = ", getcaltimevalue(t, "day of week"));
    Ccode(void, s, ".tm_yday = ", getcaltimevalue(t, "day of year"));
    Ccode(void, s, ".tm_isdst = ", getcaltimevalue(t, "dayight saving time"));
    s
);

localtime_(e:Expr):Expr := (
    when e is n:ZZcell do (
	t := toTimeT(n);
	buf := zerostructtm();
	result := Ccode(structtmstar, "localtime_r(&", t, ", &", buf, ")");
	tmtocaltime(result)
    ) else WrongArgZZ()
);
setupfun("localtime", localtime_);

gmtime_(e:Expr):Expr := (
    when e is n:ZZcell do (
	t := toTimeT(n);
	buf := zerostructtm();
	result := Ccode(structtmstar, "gmtime_r(&", t, ", &", buf, ")");
	tmtocaltime(result)
    ) else WrongArgZZ()
);
setupfun("gmtime", gmtime_);

ctime_(e:Expr):Expr := (
    when e is n:ZZcell do (
	t := toTimeT(n);
	toExpr(tostring(Ccode(charstar, "ctime(&", t, ")")))
    ) else WrongArgZZ()
);
setupfun("ctime", ctime_);


asctime_(e:Expr):Expr := (
    when e is t:HashTable do (
	s := caltimetotm(t);
	declarations "char buf;";
	toExpr(tostring(Ccode(charstar, "asctime_r(&", s, ", &buf)")))
    ) else WrongArg("a hash table")
);
setupfun("asctime", asctime_);


strftime_(e:Expr):Expr := (
    when e is args:Sequence do
	if length(args) == 2 then
	    when args.0 is str:stringCell do
		when args.1 is t:HashTable do (
		    s := caltimetotm(t);
		    outstr := tocharstar(newstring(200));
		    Ccode(void, "strftime(", outstr, ", 200, ",
			tocharstar(str.v), ", &", s, ")");
		    toExpr(tostring(outstr))
		) else WrongArg(2, "a hash table")
	    else WrongArgString(1)
	else WrongNumArgs(2)
    else WrongNumArgs(2)
);
setupfun("strftime", strftime_);

mktime_(e:Expr):Expr := (
    when e is t:HashTable do (
	s := caltimetotm(t);
	toExpr(Ccode(time_t, "mktime(&", s, ")"))
    ) else WrongArg("a hash table")
);
setupfun("mktime", mktime_);
