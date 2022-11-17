newPackage("Ruby",
    PackageExports => {"ForeignFunctions"})

libruby = openSharedLibrary "ruby-3.0"
rubyInit = foreignFunction(libruby, "ruby_init", void, void)
rubyCleanup = foreignFunction(libruby, "ruby_cleanup", int, int)
rbEvalStringProtect = foreignFunction(libruby, "rb_eval_string_protect",
    voidstar, {charstar, voidstar})

end

debug loadPackage("Ruby", Reload => true)
rubyInit()

state = address int 0
rbEvalStringProtect("puts 'Hello, world!", state)
int state

rubyCleanup 0
