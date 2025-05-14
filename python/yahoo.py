import sys
from optparse import OptionParser, Values

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

# HOME = "/Users/zeus/Projects/lisp/etradejanitor2"
HOME = "/home/rcs/opt/etradejanitor2"

FEED = "%s/feed" % HOME

SPOT_FEED = "%s/spot" % HOME

def download(t,period,feed):
  try:
      print("%s" % t)
      f = open("%s/%s.csv" % (feed,t), "w")
      t = y.Ticker("%s.OL" % t)
      h = t.history(period=period)
      csv = h.to_csv()
      f.write(csv)
      f.close()
  except Exception as e:
      print("Could not download: %s (%s)" % (t,e))

VALID_PERIODS = ["1d","5d","1mo","3mo","6mo","1y","2y","5y","10y"]

def validate_period(value):
  if value in VALID_PERIODS:
    return True
  else:
    return False

def save_to_cvs(t,period):
  download(t,period,FEED)

def spot(t):
  download(t,"1d",SPOT_FEED)

if __name__ == '__main__':
  parser = OptionParser()
  parser.add_option("-t","--ticker", dest="ticker", metavar="TICKER", help="Stock ticker")
  parser.add_option("-p","--period", dest="period", metavar="YAHOO_PERIOD", help="Valid periods: 1d, 5d, 1m, 3m, 6m, 1y, 2y, 5y, 10y")
  parser.add_option("-x","--spot", action="store_true", default=False, help="Download Spot. Default: False")

  (opts, args) = parser.parse_args()

  if opts.spot == True:
    spot(opts.ticker)
  else:
    if validate_period(opts.period) != True:
      print ("Invalid period: ",opts.period )
      sys.exit()
    else:
      print (opts.period)
      save_to_cvs(opts.ticker, opts.period)
