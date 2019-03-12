let err = Error("syntax error", ("".startIndex)..<("".startIndex), "here")
err.report(with: SourceFile(name: "blah.meda", src: "ffffdafdasfjs"))