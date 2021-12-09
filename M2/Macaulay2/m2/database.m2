databaseLibrary = method()
databaseLibrary Database := db -> (
    lib := dblibrary db;
    if lib == 0 then "gdbm"
    else if lib == 1 then "recutils"
    else "unknown library")

Database#{Standard,AfterPrint} = db -> (
    << endl;
    << concatenate(interpreterDepth:"o") << lineNumber << " : ";
    << "Database (" << databaseLibrary db << ")";
    << endl)

openDatabase = method()
openDatabase String := s -> dbmopenin(s, 0)
