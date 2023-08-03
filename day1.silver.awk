#!/usr/bin/awk -f

        { elfs[idx] += $1 }
NF == 0 { idx++ }
END     { for (i = 1; i <= idx; i++)
            if (elfs[i] > max)
              max = elfs[i]
          print max }
