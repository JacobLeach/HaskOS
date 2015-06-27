module TSOS {
  
  export class DeviceController {
    private memory: Memory;
    private terminal: Terminal;
    private programReader: ProgramReader;

    constructor() {
      this.memory = new Memory();
      _Memory = this.memory;
      
      this.printMemory();

      this.terminal = new Terminal(_Canvas);
      _Console = this.terminal;
      this.programReader = new ProgramReader();
    }

    public printMemory(): void
    {
      var memoryAsString = "";
      for(var i = 0; i < 1024; i++)
      {
        if(i == 0)
        {
          var num = i.toString(16) + "-" + (i + 8).toString(16) + ":";
          
          while(num.length < 11)
          {
            num += " ";
          }
         
          memoryAsString += num;  
        }
        else if((i % 8) == 0)
        {
          memoryAsString += "\n";

          var num = i.toString(16) + "-" + (i + 8).toString(16) + ":";
          while(num.length < 11)
          {
            num += " ";
          }
          
          memoryAsString += num;  
        }
        var num = this.memory.getByte(new Short(i)).asNumber().toString(16);
        if(num.length == 1)
        {
          num = "0" + num;
        }

        memoryAsString += num + " ";
      }

      (<HTMLInputElement>document.getElementById("memoryBox")).value = memoryAsString;
    }
    
    public getByte(address: Short): Byte {
      if(address == undefined) {
        return new Byte(0);
      }
      else if(address.asNumber() < this.memory.getSize()) {
        return this.memory.getByte(address);
      }
      else if(address.asNumber() >= 0xFF00 && address.asNumber() <= 0xFFFF) {
        switch(address.asNumber()) {
          //Get terminal width
          case 0xFF01:
            break;
          //Get terminal height
          case 0xFF02:
            break;
          case 0xFF11:
            return this.programReader.getByte();
            break;
          case 0xFF12:
            return this.programReader.atEnd();
            break;
          //Input from OS
          case 0xFFF0:
            return new Byte(Stdio.getChar());
            break;
        }
      }
      return undefined;
    }

    public setByte(address: Short, data: Byte): void {
      if(address == undefined)
      {
        
      }
      else if(address.asNumber() < this.memory.getSize()) 
      {
        this.memory.setByte(address, data);
      }
      else if(address.asNumber() >= 0xFF00 && address.asNumber() <= 0xFFFF) 
      {
        switch(address.asNumber()) 
        {
          case 0xFF00:
            this.terminal.write(data); 
            break;
          case 0xFF10:
            this.programReader.setAddress(data);
            break;
          case 0xFFF1:
            break;
        }
      }
      else
      {
        //Segfault
      }
      this.printMemory();
    }
  }
}
