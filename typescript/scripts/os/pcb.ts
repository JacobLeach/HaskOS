/* ------------
  PCB.ts
  
  A Proccess Control Block  
------------ */

module TSOS {

  export class PCB {
    private static next_pid = 0;

    private programCounter: Short;
    private accumulator: Byte;
    private xRegister: Byte;
    private yRegister: Byte;
    private zFlag: boolean;
    private kernelMode: boolean; 
    
    private memoryBounds: MemoryBounds;
    private pid: number;

    constructor(memoryBounds: MemoryBounds) {
      this.programCounter = new Short(0);
      this.accumulator = new Byte(0);
      this.xRegister = new Byte(0);
      this.yRegister = new Byte(0);
      this.zFlag = false;
      this.kernelMode = false; 

      this.memoryBounds = memoryBounds;
      this.pid = PCB.next_pid++;
    }

    public getSegment(): number
    {
      return this.memoryBounds.getSegment();
    }

    public getPid(): number
    {
      return this.pid;
    }

    public getProgramCounter(): Short
    {
      return this.programCounter;
    }

    public getAccumulator(): Byte
    { 
      return this.accumulator;
    }

    public getXRegister(): Byte
    { 
      return this.xRegister;
    }
    
    public getYRegister(): Byte
    { 
      return this.yRegister;
    }

    public getZFlag(): boolean
    {
      return this.zFlag;
    }

    public getKernelMode(): boolean
    {
      return this.kernelMode;
    }

    public getLowAddress(): Short
    {
      return this.memoryBounds.lower();
    } 
    
    public getHighAddress(): Short
    {
      return this.memoryBounds.upper();
    } 

    public setProgramCounter(data: Short): void
    {
      this.programCounter = data;
    }

    public setAccumulator(data: Byte): void
    {
      this.accumulator = data;
    }

    public setXRegister(data: Byte): void
    {
      this.xRegister = data;
    }
    
    public setYRegister(data: Byte): void
    {
      this.yRegister = data;
    }

    public setZFlag(data: boolean): void
    {
      this.zFlag = data;
    }

    public setKernelMode(data: boolean): void
    {
      this.kernelMode = data;
    }
  }
}
