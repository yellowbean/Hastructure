#!/bin/bash



sed -i "s/\-{2}\s{0,4}`debug`/`debug`/g"  app/Main.hs >app/Main.hs2