This should probably be considered a sketch of a way to compute edits of Haskell code rather than something you can pick up and use today. Contributions are welcome!

# Basic Programmatic Editing of Haskell Source

- [x] `import` statement addition
- [x] Add to existing `import`
- [x] Remove from existing `import`

# Example

If this borrowed [example](https://github.com/commercialhaskell/intero/issues/516) program is saved as `gelisam1.hs`,

```haskell
module MyLib where

import System.Exit
  ( ExitCode(ExitFailure, ExitSuccess)
  )
import Control.Monad (forever)
import Data.Maybe


foo = ExitSuccess
bar = forever (print "hello")
```

We can run,

```
$ hsedit '(remove-import "test/gelisam1.hs" "System.Exit" "ExitCode" "ExitFailure")'
(replace-lines 3 5 "import System.Exit (ExitCode(ExitSuccess))")
```

Our command is `remove-import` on the file name `"test/gelisam1.hs"`, the module name `System.Exit`, the imported thing `ExitCode`, and the constructor name `ExitFailure` (this last argument can be a constructor name or a type class method name). The last two arguments are both optional, we could also remove the entire import of `System.Exit`, or, if we left off just the last argument, the import of the `ExitCode` type and all of its constructors.

The response from `hs-edit` is what we should do to the source file: remove lines 3-5, and insert in their place the given text.
