from git import Repo
from datetime import date
from operator import itemgetter, attrgetter
import re,os,sys,json

def pickCommits(r, currentTag, lastTag):
    
    lastCommit = r.tags[lastTag].commit
    flagCommit = r.tags[currentTag].commit
    
    newCommits = []
    
    for i in range(300):
        if flagCommit != lastCommit :
            newCommits.append(flagCommit)
            flagCommit = flagCommit.parents[0]
        else:
            break
            
    return newCommits

def extractMsg(commits):
    r = [ c.message for c in commits]
    for _r in r:
        if re.match(r"bump\sversion\sto",_r):
            continue
        if re.match(r"bump\sversion",_r):
            continue
        if _r.startswith("DEVOPS") or _r.startswith("REFACTOR"):
            continue
        yield _r.rstrip("\n")

def cleanUpfile(_input,_newInput):
    os.remove(_input)
    os.rename(_newInput, _input)      

def getLatestVersion(x):
    return x['records'][0]['version']
        
def updateLogs(_input,st):
    with open(_input,'r') as logfile:
        r = json.load(logfile)
    lastConsoleVersion = getLatestVersion(r)

    repo = Repo(".")
    newCommits = pickCommits(repo,st,lastConsoleVersion)
    
    latestCommit = newCommits[0]
    _latestDate = date.fromtimestamp(latestCommit.committed_date)
    latestDate = _latestDate.strftime("%Y%m%d")
    newMsgs = list(extractMsg(newCommits))
    

    r['update'] = latestDate
    r['records'] = [{"version":st,"date":latestDate,"changes":newMsgs}] + r['records']
    
    with open(f"{_input}_new",'w') as newfile:
        json.dump(r, newfile, indent=2)
        
    cleanUpfile(_input,f"{_input}_new")
	
if __name__=='__main__':
	_,logfileName,startTag = sys.argv
	updateLogs(logfileName,startTag)
