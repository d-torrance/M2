-------------------------------------------------------------------------------
-- This script is responsible for creating a list of all builtin symbols, such
-- as keywords, types, etc. and substituting them in grammar files for various
-- editors and syntax highlighting engines. Grammar file templates are assumed
-- to be located in the same directory as this script.
-- Currently:
--  - Emacs
--  - Atom & Linguist: https://github.com/Macaulay2/language-macaulay2
--  - Rouge
-------------------------------------------------------------------------------

needsPackage "Grammars"

symbolsForVim = symbolsForEditor(///""///, " ", false)
symbolsForEmacs = symbolsForEditor(";;", " ", true)
symbolsForAtom = symbolsForEditor("##", "|", false)
symbolsForPrism = symbolsForEditor("//", "|", false)
symbolsForRouge = symbolsForEditor("##", "|", false)

-- Emacs: Write M2-symbols.el
generateGrammar("emacs/M2-symbols.el", symbolsForEmacs)

-- Atom & Linguist: Write macaulay2.cson
generateGrammar("atom/macaulay2.cson", symbolsForAtom);

-- Prism: Write macaulay2.js
generateGrammar("prism/macaulay2.js", symbolsForPrism);

-- Vim: Write m2.vim.syntax and m2.vim.dict
generateGrammar("vim/m2.vim.syntax", symbolsForVim);
generateGrammar("vim/m2.vim.dict", symbolsForVim); -- TODO: is this necessary?

-- Rouge: Write macaulay2.rb
--generateGrammar("rouge/macaulay2.rb", symbolsForRouge);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/emacs M2-symbols "
-- End:
