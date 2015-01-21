
Compares strings to produce change lists.  `diffChars` and `diffLines` are provided.

```elm
import Diff (..)

diffChars "abc" "aBcd"
  -- [ NoChange "a", Changed "b B", NoChange "c", Added "d" ]
```
