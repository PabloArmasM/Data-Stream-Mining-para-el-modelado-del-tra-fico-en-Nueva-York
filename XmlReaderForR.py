import urllib2
from xml.dom.minidom import parse
import xml.dom.minidom
import time

class XmlReaderForR(object):
    def __init__(self):
        for i in range (1403, 2880):
            aResp = urllib2.urlopen("http://dotsignals.org/nyc-links-cams/TrafficSpeed.php")
            DOMTree = xml.dom.minidom.parse(aResp)
            name = 'xml'+str(i)+'.xml'
            file = open(name, 'w')
            DOMTree.writexml(file)
            file.close()
            print 'iteracion: ' + str(i)
            time.sleep(60)
        """file = open("xml0.xml", 'r')
        lines = file.readlines()
        countLines = len(lines)
        print(countLines)

        file2 = open("xmlll.xml", 'w')

        newFile = file2.writelines(lines[0:countLines-2])
        file.close()

        for i in range (1, 60):
            name = 'xml'+str(i)+'.xml'
            file = open(name,  'r')
            lines = file.readlines()
            countLines = len(lines)
            newFile = file2.writelines(lines[1:countLines-2])

        newFile = file2.writelines(lines[countLines-1])
        file2.close()"""

        """collection = DOMTree.documentElement
        if collection.hasAttribute("shelf"):
            print "Root element : %s" % collection.getAttribute("shelf")

        data = collection.getElementsByTagName("TrafficSpeedData")

        self.i = 0
        self.resul = []
        for dat in data:
            #print (dat.getAttribute("linkId"))
            row = [dat.getAttribute("linkId"), dat.getAttribute("linkSpeed"), dat.getAttribute("linkTimeStamp")]
            self.resul.append(row)
            self.i += 1
        self.i = 0
        for dat in data:
            print (self.resul[self.i][0] + " " + self.resul[self.i][1] + " " + self.resul[self.i][2])
            self.i += 1
        exit( self.resul)"""


if __name__ == '__main__':
    XmlReaderForR()