/* ------------
  Memory.ts

  Memory simulation for our CPU.
  ------------ */

module TSOS {

  export class Memory {
    private static DEFAULT_SIZE = 1024;

    private memory: Byte[];  
    private size: number;

    constructor(size: number = Memory.DEFAULT_SIZE) {
      this.size = size;
      this.memory = [];

      for(var i: number = 0; i < size; i++) {
        this.memory[i] = new Byte(0);
      }
      //Print single char
      this.memory[0x0300] = new Byte(0x8C);
      this.memory[0x0301] = new Byte(0x00);
      this.memory[0x0302] = new Byte(0xFF);
      this.memory[0x0303] = new Byte(0x40);

      //Load char from buffer
      this.memory[0x0304] = new Byte(0xAD); //Load accumulator from memory
      this.memory[0x0305] = new Byte(0xF0); //Device address for buffer
      this.memory[0x0306] = new Byte(0xFF);
      this.memory[0x0307] = new Byte(0x40);

      //STDIO putString
      //Load char from liblos buffer
      this.memory[0x0308] = new Byte(0xAD); 
      this.memory[0x0309] = new Byte(0xF0); 
      this.memory[0x030A] = new Byte(0xFF);
      this.memory[0x030B] = new Byte(0xA8);

      //Compare Y to null
      this.memory[0x030C] = new Byte(0xCC);
      this.memory[0x030D] = new Byte(0x18);
      this.memory[0x030E] = new Byte(0x03);
      this.memory[0x030F] = new Byte(0xF0);
      this.memory[0x0310] = new Byte(6);

      //Print char
      this.memory[0x0311] = new Byte(0x8C);
      this.memory[0x0312] = new Byte(0x00);
      this.memory[0x0313] = new Byte(0xFF);
      this.memory[0x0314] = new Byte(0x4C);
      this.memory[0x0315] = new Byte(0x08);
      this.memory[0x0316] = new Byte(0x03);
      this.memory[0x0317] = new Byte(0x40);
      this.memory[0x0318] = new Byte(0x00);
      
      //Load program
      this.memory[0x0319] = new Byte(0xA9);
      this.memory[0x031A] = new Byte(0x00);//Load accc with 0 (increment this)
      this.memory[0x031B] = new Byte(0x8D);
      this.memory[0x031C] = new Byte(0x10);
      this.memory[0x031D] = new Byte(0xFF);//Set address for program reader
      this.memory[0x031E] = new Byte(0xAE); 
      this.memory[0x031F] = new Byte(0x11);
      this.memory[0x0320] = new Byte(0xFF);//Load from program reader into x
      this.memory[0x0321] = new Byte(0x8E);
      this.memory[0x0322] = new Byte(0x00);//Store x reg in zero (increment this)
      this.memory[0x0323] = new Byte(0x00);
      this.memory[0x0324] = new Byte(0xEE); 
      this.memory[0x0325] = new Byte(0x1A); 
      this.memory[0x0326] = new Byte(0x03); 
      this.memory[0x0327] = new Byte(0xEE); 
      this.memory[0x0328] = new Byte(0x22); 
      this.memory[0x0329] = new Byte(0x03); 
      this.memory[0x032A] = new Byte(0xAE);
      this.memory[0x032B] = new Byte(0x12); 
      this.memory[0x032C] = new Byte(0xFF);//Check whether or not we are done
      this.memory[0x032D] = new Byte(0xEC);
      this.memory[0x032E] = new Byte(0x41);
      this.memory[0x032F] = new Byte(0x03);//Compare to zero
      this.memory[0x0330] = new Byte(0xF0);//Branch past jump if done (equal to zero)
      this.memory[0x0331] = new Byte(0x02);
      this.memory[0x0332] = new Byte(0x4C);
      this.memory[0x0333] = new Byte(0x19);
      this.memory[0x0334] = new Byte(0x03);//Jump back to beginning
      this.memory[0x0335] = new Byte(0xA9);
      this.memory[0x0336] = new Byte(0x00);
      this.memory[0x0337] = new Byte(0x8D);
      this.memory[0x0338] = new Byte(0x1A);
      this.memory[0x0339] = new Byte(0x03);
      this.memory[0x033A] = new Byte(0x8D);
      this.memory[0x033B] = new Byte(0x22);
      this.memory[0x033C] = new Byte(0x03);
      this.memory[0x033D] = new Byte(0x8D);
      this.memory[0x033E] = new Byte(0x10);
      this.memory[0x033F] = new Byte(0xFF);
      this.memory[0x0340] = new Byte(0x40);      
      this.memory[0x0341] = new Byte(0x00);
      
      this.memory[0x0342] = new Byte(0x8D);//Print string
      this.memory[0x0343] = new Byte(0x4A);
      this.memory[0x0344] = new Byte(0x03);
      this.memory[0x0345] = new Byte(0x8C);
      this.memory[0x0346] = new Byte(0x49);
      this.memory[0x0347] = new Byte(0x03);
      this.memory[0x0348] = new Byte(0xAC);//Load from memory
      this.memory[0x0349] = new Byte(0x00);
      this.memory[0x034A] = new Byte(0x00);
      this.memory[0x034B] = new Byte(0xA2);
      this.memory[0x034C] = new Byte(0x06);
      this.memory[0x034D] = new Byte(0xCC);//Compare y to zero
      this.memory[0x034E] = new Byte(0x5C);
      this.memory[0x034F] = new Byte(0x03);
      this.memory[0x0350] = new Byte(0xF0);//If equal, branch
      this.memory[0x0351] = new Byte(0x09);
      this.memory[0x0352] = new Byte(0x8C);//Write char to screen
      this.memory[0x0353] = new Byte(0x00);
      this.memory[0x0354] = new Byte(0xFF);
      this.memory[0x0355] = new Byte(0xEE);//Increment address
      this.memory[0x0356] = new Byte(0x49);
      this.memory[0x0357] = new Byte(0x03);
      this.memory[0x0358] = new Byte(0x4C);//Jump back
      this.memory[0x0359] = new Byte(0x48);
      this.memory[0x035A] = new Byte(0x03);
      this.memory[0x035B] = new Byte(0x40);
      this.memory[0x035C] = new Byte(0x00);
      
      this.memory[0x035D] = new Byte(0x8D);//Deallocate (zero) memory
      this.memory[0x035E] = new Byte(0x64);
      this.memory[0x035F] = new Byte(0x03);
      this.memory[0x0360] = new Byte(0xA2);
      this.memory[0x0361] = new Byte(0x00);
      this.memory[0x0362] = new Byte(0x8E);
      this.memory[0x0363] = new Byte(0x00);
      this.memory[0x0364] = new Byte(0x00);
      this.memory[0x0365] = new Byte(0xEE);
      this.memory[0x0366] = new Byte(0x63);
      this.memory[0x0367] = new Byte(0x03);
      this.memory[0x0368] = new Byte(0xEC);
      this.memory[0x0369] = new Byte(0x63);
      this.memory[0x036A] = new Byte(0x03);
      this.memory[0x036B] = new Byte(0xF0);
      this.memory[0x036C] = new Byte(0x03);
      this.memory[0x036D] = new Byte(0x4C);//Jump back
      this.memory[0x036E] = new Byte(0x62);
      this.memory[0x036F] = new Byte(0x03);
      this.memory[0x0370] = new Byte(0x40);

      
    }

    public getSize(): number {
      return this.size;
    }

    public setByte(address: Short, value: Byte): void {
      if(address.asNumber() < this.size) {
        this.memory[address.asNumber()] = value;
      }
      else {
        //Should throw an interrupt
      }
    }

    public getByte(address: Short): Byte {
      var toReturn: Byte;

      if(address.asNumber() < this.size) {
        toReturn = this.memory[address.asNumber()];
      }
      else {
        //Should throw an interrupt
        toReturn = undefined;
      }

      return toReturn;
    }
  }

  export class Byte {
    private value: number;

    constructor(value: number) {
      this.setValue(value);  
    } 

    private setValue(value: number): void {
      this.value = value & 0xFF;
    }

    public increment(): Byte {
      return new Byte(++this.value);
    }

    public asNumber(): number {
      return this.value;
    }
  }

  export class Short {
    private lowByte: Byte;
    private highByte: Byte;

    constructor(value: number) {
      this.lowByte = new Byte(value & 0xFF);
      this.highByte = new Byte((value & 0xFF00) >> 8);
    }

    public increment(): Short {
      if(this.lowByte.asNumber() == 255) {
        this.highByte.increment();
      }
      
      this.lowByte.increment();

      return bytesToShort(this.lowByte, this.highByte);
    }

    public getHighByte(): Byte
    {
      return new Byte(this.highByte.asNumber());
    }

    public asNumber(): number {
      var lowAsString: string = this.lowByte.asNumber().toString(2); 
      
      while(lowAsString.length < 8) {
        lowAsString = "0" + lowAsString;
      }
      
      var highAsString: string = this.highByte.asNumber().toString(2);
      
      while(highAsString.length < 8) {
        highAsString = "0" + highAsString;
      }

      var shortAsString: string = highAsString + lowAsString;
      var shortAsNumber = parseInt(shortAsString, 2);
      
      return shortAsNumber;
    }
  }

  export function bytesToShort(lowByte: Byte, highByte: Byte): Short {
    return new Short(lowByte.asNumber() + (highByte.asNumber() << 8));
  }
}
