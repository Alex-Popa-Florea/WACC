package frontend

object color {
    def makeRed(msg:String):String = {
        Console.RED+msg+Console.RESET
    }

    def makeGreen(msg:String):String = {
        Console.GREEN+msg+Console.RESET
    }

}
