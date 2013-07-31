define: defs.def

node: CP*

coding_query:

/* The first three columns: the date of the text
 * These queries acutally cover the EME and MBE corpora as well
 */

1: {
    \7: (*-17* inID)
    \8: (*-18* inID)
    \9: (*-19* inID)
    \1: (CMKENTHO*|CMPETERB* inID)
    \2: (CMLAMB*|CMTRINIT*|CMORM*|CMVICES1*|CMSAWLES*|CMHALI*|CMKATHE*|CMJULIA*|CMMARGA*|CMANCRIW*|CMKENTSE* inID)
    \3: (CMAYENBI*|CMEARLPS*|CMPOLYCH*|CMNTEST*|CMPURVEY*|CMEDVERN*|CMCTPARS*|CMCTMELI*|CMEQUATO*|CMBOETH*|CMOTEST*|CMCLOUD*|CMASTRO*|CMHORSE*|CMHILTON*|CMEDTHOR* inID)
    \4: (CMAELR3*|CMROLLTR*|CMROLLEP*|CMBRUT3*|CMWYCSER*|CMMANDEV*|CMBENRUL*|CMEDTHORN*|CMGAYTRY*|CMVICES4*|CMJULNOR*|CMROYAL*|CMMIRK*|CMTHORN*|CMEDMUND*|CMKEMPE*|CMCAPSER*|CMCAPCHR*|CMMALORY*|CMREYNES*|CMGREGOR*|CMREYNAR*|CMFITZJA*|CMINNOCE*|CMAELR4* inID)
    \5: (CMSIEGE* inID)
    \4: (LETTER column 3 14*) OR (PASTON,II,127.312.8588 inID)
    \5: (LETTER column 3 15*) OR (ABOTT-E1*|AMBASS-E1*|APLUMPT-E1*|APOOLE-E1*|ASCH-E1*|BEDYLL-E1*|BLUNDEV-E2*|BOETHCO-E1*|BOETHEL-E2*|CHAPLAIN-E1*|CLOWESOBS-E2*|CROMWELL-E1*|DACRE-E1*|DELAPOLE-E1*|DPLUMPT-E1*|EBEAUM-E1*|ECUMBERL-E1*|EDMONDES-E2*|EDWARD-E1*|ELIZ*|ELYOT-E1*|EPOOLE-E1*|FABYAN-E1*|FISHER-E1*|FITZH-E1*|FRIAR-E1*|GASCOIGNE*|GAWDY-E2*|GCROMW-E1*|GIFFORD-E2*|GPOOLE*|GREY-E1*|HARMAN-E1*|HATCHER-E2*|HENRY*|INTERVIEW-E1*|IPLUMPT-E1*|JUDALL-E2*|KSCROPE*|LATIMER-E1*|LELAND-E1*|LORDS-E1*|MACHYN-E1*|MADOX-E2*|MANNERS-E1*|MARCHES-E1*|MERRYTAL-E1*|MHOWARD-E1*|MORELET*|MORERIC*|MOREWOL*|MOWNTAYNE-E1*|MROPER-E1*|MTUDOR*|NEVILL-E1*|PERROTT-E2*|RCECIL-E2*|RECORD-E1*|ROPER-E1*|RPLUMPT*|RUSSELL-E1*|SAVILL-E1*|SHAKESP-E2*|SMITH-E2*|STAT-15*|STEVENSO-E1*|STOW-E2*|SURETY-E1*|TALBOT-E2*|THOWARD*|THROCKM-E1*|TORKINGT-E1*|TRINCOLL-E2*|TUNSTALL-E1*|TURNER*|TYND*|UDALL-E1*|UNDERHILL-E1*|VICARY-E1*|WCECIL*|WOLSEY-E1*|WPLUMPT* inID)
    \6: (LETTER column 3 16*) OR (ALHATTON2-E3*|ANHATTON-E3*|ARMIN-E2*|AUNGIER-E3*|AUTH*|BACON-E2*|BEHN-E3*|BOETHPR-E3*|BOYLECOL-E3*|BOYLE-E3*|BRINSLEY-E2*|BURNET*|CAPEL-E3*|CHARLES*|CHATTON-E3*|CLOWES-E2*|COMMISS-E3*|CONWAY*|COUNC-E3*|COVERTE-E2*|DELL-E3*|DELONEY-E2*|DERING-E2*|DRUMMOND-E3*|EHATTON*|EOXINDEN*|ESSEX*|EVELYN-E3*|EVERARD-E2*|FHATTON-E3*|FIENNES-E3*|FORMAN*|FOX-E3*|FRYER-E3*|HARLEY*|HAYWARD-E2*|HOBY-E2*|HOOKE-E3*|HOOKER*-E2*|HOOLE-E3*|HOXINDEN*|JACKSON-E3*|JBARRING-E2*|JETAYLOR*|JOPINNEY-E3*|JOTAYLOR-E2*|JOXINDEN-E2*|JPINNEY-E3*|JUBARRING-E2*|KNYVETT*|KOXINDEN-E2*|KPASTON-E2*|LANGF-E3*|LISLE-E3*|LOCKE-E3*|MARKHAM-E2*|MASHAM-E2*|MEMO-E3*|MHATTON-E3*|MIDDLET-E2*|MILTON-E3*|MONTAGUE-E3*|MOXINDEN-E2*|NFERRAR-E2*|OATES-E3*|OSBORNE-E3*|PENNY-E3*|PEPYS-E3*|PETTIT*|PEYTON-E2*|PHENRY-E3*|PROPOSALS-E3*|PROUD*|RALEIGH-E2*|RFERRAR-E2*|RHADDJR-E3*|RHADDSR-16*|RICH-E2*|ROXINDEN*|SOMERS-E3*|SOUTHARD-E3*|SPENCER-16*|STAT-16*|STRYPE-E3*|TBARRING-E2*|TILLOTS*|VANBR-E3*|WALTON-E3*|WPASTON2-E2*|ZOUCH-E3* inID)
    \7: (LETTER column 3 17*) OR (ALHATTON-E3*|FARQUHAR-E3*|NHADD*|RHADDSR-17*|SPENCER-17* inID)
    -: ELSE
}
2: {
    \0: (*-170*|*-180*|*-190* inID)
    \1: (*-171*|*-181*|*-191* inID)
    \2: (*-172*|*-182*|*-192* inID)
    \3: (*-173*|*-183*|*-193* inID)
    \4: (*-174*|*-184*|*-194* inID)
    \5: (*-175*|*-185*|*-195* inID)
    \6: (*-176*|*-186*|*-196* inID)
    \7: (*-177*|*-187*|*-197* inID)
    \8: (*-178*|*-188*|*-198* inID)
    \9: (*-179*|*-189*|*-199* inID)
    \0: (CMORM*|CMVICES1*|CMANCRIW*|CMAYENBI*|CMAELR3*|CMROLLTR*|CMROLLEP*|CMEDVERN*|CMCTPARS*|CMCTMELI*|CMBRUT3*|CMWYCSER*|CMBOETH*|CMMANDEV*|CMHORSES*|GMGAYTRY*|CMVICES4*|CMJULNOR*|CMTHORN*|CMMALORY*|CMSIEGE* inID)
    \1: (CMMIRK* inID)
    \2: (CMLAMBET*|CMTRINIT*|CMKENTHO*|CMLAMB*|CMSAWLES*|CMHALI*|CMKATHE*|CMJULIA*|CMMARGA*|CMBENRUL*|CMROYAL* inID)
    \3: (CMANCRIW*|CMEDMUND* inID)
    \4: (CMAYENBI*|CMROLLTR*|CMGAYTRY*|CMTHORN* inID)
    \5: (CMPETERB*|CMEALRPS*|CMROLLEP*|CMHORSES*|CMEDTHORN*|CMAELR4*|CMKEMPE*|CMCAPSER*|CMEDTHOR*|CMEARLPS* inID)
    \6: (CMCAPCHR*|CMAELR4* inID)
    \7: (CMKENTSE*|CMCLOUD*|CMMALORY*|CMGREGOR* inID)
    \8: (CMPOLYCH*|CMNTEST*|CMPURVEY*|CMBOETH*|CMREYNES*|CMREYNAR* inID)
    \9: (CMEDVERN*|CMCTPARS*|CMCTMELI*|CMEQUATO*|CMOTEST*|CMASTRO*|CMHILTON*|CMFITZJA*|CMINNOCE* inID)
    \0: (LETTER column 3 140*|150*|160*|170*) OR (ALHATTON-E3*|APLUMPT-E1*|ARMIN-E2*|BACON-E2*|CHAPLAIN-E1*|CLOWES-E2*|DELONEY-E2*|DPLUMPT-E1*|EBEAUM-E1*|EPOOLE-E1*|ESSEX*|FARQUHAR-E3*|FORMAN*|GASCOIGNE-1500-E1*|GPOOLE-1500-E1*|HOBY-E2*|IPLUMPT-E1*|NEVILL-E1*|NHADD-1700-E3*|RALEIGH-E2*|RHADDSR-1700-E3*|ROXINDEN-1600-E2*|RPLUMPT2-E1*|SPENCER-1700-E3*|STAT-1600-E2*|WPLUMPT-1500-E1* inID)
    \1: (LETTER column 3 141*|151*|161*|171*) OR (AUTH*|COVERTE-E2*|DACRE-E1*|FABYAN-E1*|GASCOIGNE-1510-E1*|GPOOLE-1510-E1*|HAYWARD-E2*|HENRY-1510-E1*|HOOKER*-E2*|MARKHAM-E2*|MORERIC-E1*|MTUDOR-1510-E1*|NHADD-1710-E3*|RHADDSR-1710-E3*|STAT-1500-E1*|STAT-1510-E1*|THOWARD-E1*|TORKINGT-E1*|TUNSTALL-E1*|WPLUMPT-1510-E1* inID)
    \2: (LETTER column 3 142*|152*|162*|172*) OR (AMBASS-E1*|BRINSLEY-E2*|CONWAY-E2*|EVERARD-E2*|FISHER-E1*|GREY-E1*|HARLEY-E2*|HENRY-1520-E1*|INTERVIEW-E1*|JBARRING-E2*|KNYVETT-1620-E2*|KPASTON-E2*|MERRYTAL-E1*|MORELET1-E1*|MOREWOL-E1*|MTUDOR-1520-E1*|NFERRAR-E2*|PETTIT*|PROUD-1620-E2*|RICH-E2*|ROXINDEN-1620-E2*|STAT-1620-E2*|WOLSEY-E1*|WPASTON2-E2* inID)
    \3: (LETTER column 3 143*|153*|163*|173*) OR (ABOTT-E1*|BEDYLL-E1*|CROMWELL-E1*|DELAPOLE-E1*|ELYOT-E1*|FITZH-E1*|FRIAR-E1*|GCROMW-E1*|HARLEYEDW-E2*|HENRY-1530-E1*|JOTAYLOR-E2*|JOXINDEN-E2*|JUBARRING-E2*|KNYVETT-1630-E2*|KOXINDEN-E2*|KSCROPE-1530-E1*|LELAND-E1*|LORDS-E1*|MANNERS-E1*|MARCHES-E1*|MASHAM-E2*|MHOWARD-E1*|MIDDLET-E2*|MORELET2-E1*|MROPER-E1*|PEYTON-E2*|PROUD-1630-E2*|RFERRAR-E2*|ROXINDEN2-E2*|RPLUMPT-E1*|RUSSELL-E1*|STAT-1530-E1*|TBARRING-E2*|TYND*|WPLUMPT-1530-E1* inID)
    \4: (LETTER column 3 144*|154*|164*|174*) OR (DERING-E2*|ECUMBERL-E1*|HOXINDEN-1640-E3*|LATIMER-E1*|MOXINDEN-E2*|SAVILL-E1*|STAT-1540-E1*|STAT-1640-E2*|SURETY-E1*|UDALL-E1*|VICARY-E1*|ZOUCH-E3* inID)
    \5: (LETTER column 3 145*|155*|165*|175*) OR (APOOLE-E1*|BOETHCO-E1*|CHARLES-1650-E3*|EDWARD-E1*|EOXINDEN-1650-E3*|HOXINDEN-1650-E3*|JACKSON-E3*|MACHYN-E1*|MOWNTAYNE-E1*|RECORD-E1*|RHADDSR-1650-E3*|ROPER-E1*|STAT-1550-E1*|STEVENSO-E1*|THROCKM-E1* inID) OR (PASTON,II,127.312.8588 inID)
    \6: (LETTER column 3 146*|156*|166*|176*) OR (ASCH-E1*|BEHN-E3*|BOYLECOL-E3*|EHATTON-E3*|ELIZ-1560-E1*|EOXINDEN-1660-E3*|HARMAN-E1*|HOOKE-E3*|HOOLE-E3*|HOXINDEN-1660-E3*|JETAYLORMEAS-E3*|PEPYS-E3*|STAT-1560-E1*|STAT-1660-E3*|STRYPE-E3*|TURNER*|UNDERHILL-E1*|WCECIL-1560-E1* inID)
    \7: (LETTER column 3 147*|157*|167*|177*) OR (AUNGIER-E3*|BOYLE-E3*|CAPEL-E3*|CHARLES-1670-E3*|COMMISS-E3*|CONWAY2-E3*|ELIZ-1570-E2*|FHATTON-E3*|FOX-E3*|FRYER-E3*|JETAYLOR-E3*|MEMO-E3*|MHATTON-E3*|MILTON-E3*|OSBORNE-E3*|PROPOSALS-E3*|RHADDSR-1670-E3*|STAT-1570-E2*|STAT-1670-E3*|THOWARD2-E2*|TILLOTS-A-E3*|TILLOTS-B-E3*|WALTON-E3* inID)
    \8: (LETTER column 3 148*|158*|168*|178*) OR (ALHATTON2-E3*|BURNETROC-E3*|COUNC-E3*|DELL-E3*|ELIZ-1580-E2*|EOXINDEN-1680-E3*|EVELYN-E3*|GAWDY-E2*|HATCHER-E2*|JOPINNEY-E3*|JPINNEY-E3*|KSCROPE-1580-E2*|LISLE-E3*|LOCKE-E3*|MADOX-E2*|MONTAGUE-E3*|OATES-E3*|PENNY-E3*|PHENRY-E3*|RHADDSR-1680-E3*|SOUTHARD-E3*|SPENCER-1680-E3*|STAT-1580-E2*|STOW-E2*|TILLOTS-C-E3*|WCECIL-1580-E2* inID)
    \9: (LETTER column 3 149*|159*|169*|179*) OR (ANHATTON-E3*|BLUNDEV-E2*|BOETHEL-E2*|BOETHPR-E3*|BURNETCHA-E3*|CHATTON-E3*|CLOWESOBS-E2*|DRUMMOND-E3*|EDMONDES-E2*|EHATTON2-E3*|ELIZ-1590-E2*|FIENNES-E3*|GIFFORD-E2*|JUDALL-E2*|LANGF-E3*|PERROTT-E2*|RCECIL-E2*|RHADDJR-E3*|SHAKESP-E2*|SMITH-E2*|SOMERS-E3*|STAT-1590-E2*|STAT-1690-E3*|TALBOT-E2*|TRINCOLL-E2*|VANBR-E3* inID)
    -: ELSE
}
3: {
   \0: (CMPETERB*|CMORM*|CMVICES1*|CMANCRIW*|CMAYENBI*|CMEARLPS*|CMAELR3*|CMROLLTR*|CMROLLEP*|CMEDVERN*|CMCTPARS*|CMCTMELI*|CMBRUT3*|CMWYCSER*|CMBOETH*|CMMANDEV*|CMHORSES*|CMEDTHORN*|CMGAYTRY*|CMVICES*|CMJULIAN*|CMTHORN*|CMAELRE4*|CMKEMPE*|CMMALORY*|CMSIEGE*|CMEDTHOR*|CMJULNOR* inID)
    \1: (CMASTRO*|CMREYNAR* inID)
    \2: (CMCLOUD*|CMEQUATO*|CMCAPSER* inID)
    \4: (CMCAPCHR* inID)
    \5: (CMLAMBET*|CMTRINIT*|CMKENTHO*|CMLAMB*|CMSAWLES*|CMHALI*|CMKATHE*|CMJULIA*|CMMARGA*|CMKENTSE*|CMOTEST*|CMBENRUL*|CMROYAL*|CMMIRK*|CMREYNES*|CMGREGOR*|CMFITZJA* inID)
    \6: (CMHILTON* inID)
    \7: (CMPOLYCH*|CMINNOCE* inID)
    \8: (CMNTEST*|CMPURVEY*|CMEDMUND* inID)
    \0: (CMAELR4* inID)
    \0: (LETTER column 3 1400*|1410*|1420*|1430*|1440*|1450*|1460*|1470*|1480*|1490*|1500*|1510*|1520*|1530*|1540*|1550*|1560*|1570*|1580*|1590*|1600*|1610*|1620*|1630*|1640*|1650*|1660*|1670*|1680*|1690*) OR (ABOTT-E1*|ALHATTON-E3*|BURNETROC-E3*|DELONEY-E2*|DRUMMOND-E3*|EHATTON2-E3*|EOXINDEN-1680-E3*|ESSEX*|HATCHER-E2*|HOBY-E2*|HOOLE-E3*|INTERVIEW-E1*|JOTAYLOR-E2*|JUBARRING-E2*|JUDALL-E2*|MASHAM-E2*|MIDDLET-E2*|MILTON-E3*|MONTAGUE-E3*|MOXINDEN-E2*|RALEIGH-E2*|ROXINDEN2-E2*|STAT-1640-E2*|STOW-E2*|TBARRING-E2*|TILLOTS-C-E3*|TYNDOLD-E1* inID)
    \1: (LETTER column 3 1401*|1411*|1421*|1431*|1441*|1451*|1461*|1471*|1481*|1491*|1501*|1511*|1521*|1531*|1541*|1551*|1561*|1571*|1581*|1591*|1601*|1611*|1621*|1631*|1641*|1651*|1661*|1671*|1681*|1691*) OR (AUTH*|CHATTON-E3*|EDWARD-E1*|ELYOT-E1*|FISHER-E1*|HENRY-1530-E1*|RECORD-E1*|SMITH-E2*|STAT-1500-E1*|STAT-1670-E3*|THOWARD2-E2*|TILLOTS-A-E3*|UNDERHILL-E1* inID)
    \2: (LETTER column 3 1402*|1412*|1422*|1432*|1442*|1452*|1462*|1472*|1482*|1492*|1502*|1512*|1522*|1532*|1542*|1552*|1562*|1572*|1582*|1592*|1602*|1612*|1622*|1632*|1642*|1652*|1662*|1672*|1682*|1692*) OR (APOOLE-E1*|CLOWES-E2*|COVERTE-E2*|FORMAN*|HAYWARD-E2*|HOXINDEN-1650-E3*|JETAYLORMEAS-E3*|MADOX-E2*|PEYTON-E2*|RHADDJR-E3*|RHADDSR-1710-E3*|RPLUMPT2-E1*|STAT-1570-E2*|TURNERHERB-E1* inID)
    \3: (LETTER column 3 1403*|1413*|1423*|1433*|1443*|1453*|1463*|1473*|1483*|1493*|1503*|1513*|1523*|1533*|1543*|1553*|1563*|1573*|1583*|1593*|1603*|1613*|1623*|1633*|1643*|1653*|1663*|1673*|1683*|1693*) OR (ANHATTON-E3*|APLUMPT-E1*|BOETHEL-E2*|BURNETCHA-E3*|COMMISS-E3*|CONWAY-E2*|DACRE-E1*|ELIZ-1570-E2*|EPOOLE-E1*|GASCOIGNE-1500-E1*|GIFFORD-E2*|GREY-E1*|HOXINDEN-1660-E3*|JETAYLOR-E3*|JOXINDEN-E2*|MORERIC-E1*|RFERRAR-E2*|RHADDSR-1670-E3*|RHADDSR-1700-E3*|STAT-1530-E1*|STAT-1540-E1*|STAT-1550-E1*|STAT-1560-E1*|STAT-1590-E2*|STAT-1660-E3*|TALBOT-E2*|WPLUMPT-1500-E1* inID)
    \4: (LETTER column 3 1404*|1414*|1424*|1434*|1444*|1454*|1464*|1474*|1484*|1494*|1504*|1514*|1524*|1534*|1544*|1554*|1564*|1574*|1584*|1594*|1604*|1614*|1624*|1634*|1644*|1654*|1664*|1674*|1684*|1694*) OR (AUNGIER-E3*|BOYLECOL-E3*|CROMWELL-E1*|FITZH-E1*|FOX-E3*|GCROMW-E1*|GPOOLE-1500-E1*|HENRY-1520-E1*|HOOKER*-E2*|KNYVETT-1620-E2*|KSCROPE-1580-E2*|MORELET2-E1*|MOREWOL-E1*|MROPER-E1*|RICH-E2*|STRYPE-E3*|THROCKM-E1*|TRINCOLL-E2*|TYNDNEW-E1*|UDALL-E1* inID)
    \5: (LETTER column 3 1405*|1415*|1425*|1435*|1445*|1455*|1465*|1475*|1485*|1495*|1505*|1515*|1525*|1535*|1545*|1555*|1565*|1575*|1585*|1595*|1605*|1615*|1625*|1635*|1645*|1655*|1665*|1675*|1685*|1695*) OR (AMBASS-E1*|BACON-E2*|BOETHPR-E3*|CONWAY2-E3*|ECUMBERL-E1*|ELIZ-1580-E2*|EOXINDEN-1660-E3*|GASCOIGNE-1510-E1*|GPOOLE-1510-E1*|HENRY-1510-E1*|HOOKE-E3*|JACKSON-E3*|KPASTON-E2*|LISLE-E3*|LOCKE-E3*|MANNERS-E1*|MARKHAM-E2*|MEMO-E3*|MOWNTAYNE-E1*|MTUDOR-1510-E1*|NEVILL-E1*|OATES-E3*|PETTIT-E2*|PHENRY-E3*|ROPER-E1*|SAVILL-E1*|STAT-1510-E1*|STAT-1580-E2*|WCECIL-1560-E1*|WPASTON2-E2* inID) OR (PASTON,II,127.312.8588 inID)
    \6: (LETTER column 3 1406*|1416*|1426*|1436*|1446*|1456*|1466*|1476*|1486*|1496*|1506*|1516*|1526*|1536*|1546*|1556*|1566*|1576*|1586*|1596*|1606*|1616*|1626*|1636*|1646*|1656*|1666*|1676*|1686*|1696*) OR (ASCH-E1*|BOETHCO-E1*|BOYLE-E3*|CAPEL-E3*|CHAPLAIN-E1*|CHARLES-1670-E3*|CLOWESOBS-E2*|EHATTON-E3*|ELIZ-1560-E1*|FABYAN-E1*|JPINNEY-E3*|KOXINDEN-E2*|KSCROPE-1530-E1*|LANGF-E3*|MACHYN-E1*|MERRYTAL-E1*|MHOWARD-E1*|NHADD-1700-E3*|OSBORNE-E3*|PENNY-E3*|PROUD-1620-E2*|PROUD-1630-E2*|RCECIL-E2*|ROXINDEN-1620-E2*|RPLUMPT-E1*|SOUTHARD-E3*|STAT-1620-E2*|VANBR-E3*|WALTON-E3*|WPLUMPT-1510-E1* inID)
    \7: (LETTER column 3 1407*|1417*|1427*|1437*|1447*|1457*|1467*|1477*|1487*|1497*|1507*|1517*|1527*|1537*|1547*|1557*|1567*|1577*|1587*|1597*|1607*|1617*|1627*|1637*|1647*|1657*|1667*|1677*|1687*|1697*) OR (ALHATTON2-E3*|BEDYLL-E1*|BLUNDEV-E2*|BRINSLEY-E2*|CHARLES-1650-E3*|DERING-E2*|DPLUMPT-E1*|FARQUHAR-E3*|FHATTON-E3*|FRYER-E3*|IPLUMPT-E1*|KNYVETT-1630-E2*|MARCHES-E1*|PEPYS-E3*|PETTIT2-E2*|PROPOSALS-E3*|ROXINDEN-1600-E2*|SOMERS-E3*|STAT-1600-E2*|STAT-1690-E3*|THOWARD-E1*|TORKINGT-E1*|WCECIL-1580-E2* inID)
    \8: (LETTER column 3 1408*|1418*|1428*|1438*|1448*|1458*|1468*|1478*|1488*|1498*|1508*|1518*|1528*|1538*|1548*|1558*|1568*|1578*|1588*|1598*|1608*|1618*|1628*|1638*|1648*|1658*|1668*|1678*|1688*|1698*) OR (ARMIN-E2*|BEHN-E3*|COUNC-E3*|DELAPOLE-E1*|DELL-E3*|EBEAUM-E1*|EOXINDEN-1650-E3*|FIENNES-E3*|FRIAR-E1*|GAWDY-E2*|HARLEYEDW-E2*|HARMAN-E1*|HOXINDEN-1640-E3*|JOPINNEY-E3*|MHATTON-E3*|MTUDOR-1520-E1*|PERROTT-E2*|RHADDSR-1650-E3*|RHADDSR-1680-E3*|RUSSELL-E1*|SPENCER-1680-E3*|SPENCER-1700-E3*|STEVENSO-E1*|TURNER-E1*|VICARY-E1*|WPLUMPT-1530-E1* inID)
    \9: (LETTER column 3 1409*|1419*|1429*|1439*|1449*|1459*|1469*|1479*|1489*|1499*|1509*|1519*|1529*|1539*|1549*|1559*|1569*|1579*|1589*|1599*|1609*|1619*|1629*|1639*|1649*|1659*|1669*|1679*|1689*|1699*) OR (EDMONDES-E2*|ELIZ-1590-E2*|EVELYN-E3*|EVERARD-E2*|HARLEY-E2*|JBARRING-E2*|LATIMER-E1*|LELAND-E1*|LORDS-E1*|MORELET1-E1*|NFERRAR-E2*|NHADD-1710-E3*|SHAKESP-E2*|SURETY-E1*|TILLOTS-B-E3*|TUNSTALL-E1*|WOLSEY-E1*|ZOUCH-E3* inID)
    \0: (*0,* inID)
	\1: (*1,* inID)
	\2: (*2,* inID)
	\3: (*3,* inID)
	\4: (*4,* inID)
	\5: (*5,* inID)
	\6: (*6,* inID)
	\7: (*7,* inID)
	\8: (*8,* inID)
	\9: (*9,* inID)
	\5: (*X,* inID)
    -: ELSE
}

/* Various reasons to exclude a token of negation */

4: {
   /* contracted ne */
   X: IP* idoms NEG+*
   /* ne ... none or ne ... never type constructions */
   X: IP* doms Q AND Q idoms other_neg
   X: IP* idoms ADVP* AND ADVP* idoms ADV AND ADV idoms never
   /* not only type constructions */
   X: not iprecedes FP*
   /* the following cases look like constituent negation */
   /* John might eat but not drink */
   X: (IP* idoms NP-SBJ) AND (NP-SBJ idoms \*con\*) AND (IP* idoms NEG) AND (NEG idoms not) AND (NEG iprecedes tensed_verb)
   /* John might not frequently eat */
   X: (IP* idoms ADVP*) AND (IP* idoms NEG) AND (NEG idoms not) AND (NEG iprecedes ADVP*)
   /* conjoined IPs don't count */
   X: (IP* idoms CONJP*) AND (IP* idoms NEG) AND (NEG precedes tensed_verb) AND (CONJP* doms tensed_verb)
   /* corpus bugs */
   X: CMCTPARS,301.C1.513 inID
   X: CMEDMUND,170.216 inID
   -: ELSE
}

/* is ne present */

5: {
   ne: (IP* idoms NEG) AND (IP* idoms tensed_verb) AND (NEG idoms ne) AND (NEG precedes tensed_verb)

   /* don't count contracted instances (should already be filtered out) */
   contracted: (IP* idoms NEG+V*|NEG+B*|NEG+M*|NEG+H*|NEG+AX*)

   /* should not happen */
   error: (IP* idoms NEG) AND (IP* idoms tensed_verb) AND (NEG idoms ne) AND (tensed_verb precedes NEG)

   -: ELSE
}

/* is not present */

6: {
   preverbal: (IP* idoms NEG) AND (IP* idoms tensed_verb) AND (NEG idoms not) AND (NEG precedes tensed_verb)
   postverbal: (IP* idoms NEG) AND (IP* idoms tensed_verb) AND (NEG idoms not) AND (tensed_verb precedes NEG)
   -: ELSE
}

/* clause type */

7: {
    matrix: CP-MAT* isroot
    relative: CP-REL* isroot
    that: CP-THT* isroot
    other: ELSE
}

/* never position */
8: {
   preverbal: IP* idoms ADVP* AND ADVP* idomsonly ADV* AND ADV* idoms never AND IP* idoms tensed_verb AND ADV* precedes tensed_verb
   postverbal: IP* idoms ADVP* AND ADVP* idomsonly ADV* AND ADV* idoms never AND IP* idoms tensed_verb AND tensed_verb precedes ADV*
   -: ELSE
}

/* not position */
9: {
   preverbal: IP* idoms NEG* AND NEG* idomsonly not AND IP* idoms tensed_verb AND NEG* precedes tensed_verb
   postverbal: IP* idoms NEG* AND NEG* idomsonly not AND IP* idoms tensed_verb AND tensed_verb precedes NEG*
   -: ELSE
}

10: {
    finite: IP* idoms tensed_verb
    -: ELSE
}
