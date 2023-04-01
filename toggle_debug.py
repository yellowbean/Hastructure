import re,sys

def debug_off(f):
    with open(f,'r') as i_file:
        content = i_file.read()
        content_new = re.


if __name__ == '__main__':
    _,a,v = sys.argv

# toggle on debug
# sed -r "s/\s+--\s+`debug`/ `debug` /g"  app/Main.hs > app/Main.hs2
# sed -i "s/\-{2}\s{0,4}`debug`/`debug`/g"  app/Main.hs >app/Main.hs2


