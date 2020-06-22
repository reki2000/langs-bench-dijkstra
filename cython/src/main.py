import cy_g
import sys

count = int(sys.argv[1])
is_debug = len(sys.argv) > 2 and sys.argv[2] == "debug"

cy_g.main(count, is_debug)
