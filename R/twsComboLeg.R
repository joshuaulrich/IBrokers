twsComboLeg <- function(conId=0,
                        ratio=0,
                        action=NULL,
                        exchange=NULL,
                        openClose=0,
                        shortSaleSlot=0,
                        designatedLocation=NULL)
{
  structure(list(conId=conId,
                 ratio=ratio,
                 action=action,                 # BUY/SELL/SSHORT
                 exchange=exchange,
                 openClose=openClose,
                 shortSaleSlot=shortSaleSlot,
                 designatedLocation=designatedLocation,
                 SAME=0,OPEN=1,CLOSE=2,UNKNOWN=3),
            class="twsComboLeg") 
}
