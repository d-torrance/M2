newPackage("CalendarTimes",
    Version => "0.1",
    Date => "2021-02-06",
    Headline => "calendar times",
    Authors => {{Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}}
)

importFrom_Core {
    "CalendarTime",
    "localtime",
    "gmtime",
    "mktime",
    "asctime"
}

export {
    "CalendarTime",
    "localTime",
    "coordinatedUniversalTime",
    "unixTime"
}

CalendarTime.synonym = "calendar time"
net CalendarTime := toString CalendarTime := asctime

inttocaltime = (n, f) -> new CalendarTime from f n
seqtocaltime = (s, f) -> if #s == 0 then f currentTime() else
    error "expected 0 or 1 arguments"

localTime = method(Dispatch => Thing, TypicalValue => CalendarTime)
localTime ZZ := n -> inttocaltime(n, localtime)
localTime Sequence := s -> seqtocaltime(s, localTime)

coordinatedUniversalTime = method(Dispatch => Thing,
    TypicalValue => CalendarTime)
coordinatedUniversalTime ZZ := n -> inttocaltime(n, gmtime)
coordinatedUniversalTime Sequence := s ->
    seqtocaltime(s, coordinatedUniversalTime)

unixTime = method(TypicalValue => ZZ)
unixTime CalendarTime := mktime

-- actually defined in M2/Macaulay2/m2/methods.m2
format(String, CalendarTime) := format(CalendarTime, String) := format
