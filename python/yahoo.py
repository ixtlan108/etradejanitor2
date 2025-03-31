
from optparse import OptionParser

import yfinance as y

xTICKERS = [
    #"AKERBP",
    "AKSO",
    "BAKKA",
    "BWLPG",
    "DNB",
    "DNO",
    "EQNR",
    "GJF",
    "GOGL",
    #"MHG",
    "NAS",
    "NHY",
    "OBX",
    "ORK",
    "PGS",
    #"REC",
    "SDRL",
    "STB",
    "SUBC",
    "TEL",
    "TGS",
    "TOM",
    "YAR"
]

TICKERS = [
    "YAR",
]

# use "period" instead of start/end
# valid periods: 1d,5d,1mo,3mo,6mo,1y,2y,5y,10y,ytd,max

PERIOD = "1d"


def save_to_cvs(t,period):
  try:
      print("%s" % t)
      f = open("../feed/%s.csv" % t, "w")
      t = y.Ticker("%s.OL" % t)
      h = t.history(period=period)
      csv = h.to_csv()
      f.write(csv)
      f.close()
  except:
      print("Could not download: %s" % t)

def demo():
  save_to_cvs("YAR","2y")

if __name__ == '__main__':
  parser = OptionParser()
  parser.add_option("--studiop", action="store_true", default=False,
                    help="Studio P. Default: False")
  parser.add_option("--demo", action="store_true", default=False,
                    help="Run demo. Default: False")
  (opts, args) = parser.parse_args()

  if opts.demo == True:
    demo()
