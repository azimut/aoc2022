#!/usr/bin/awk -f

        { calories += $1 }
NF == 0 { if(calories > max) max = calories
          calories = 0 }
END     { print max }
