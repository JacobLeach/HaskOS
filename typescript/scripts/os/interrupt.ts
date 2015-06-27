/* ------------
   Interrupt.ts
   ------------ */

module TSOS {
  
  export class Interrupt 
  {
    private static nextId: number = 0;
    
    private id: number;
    private interruptType: InterruptType;
    private interruptParameters: string[];

    constructor(type: InterruptType, parameters: any) 
    {
      this.interruptType = type;
      this.interruptParameters = parameters;
      this.id = Interrupt.nextId++;
    }
  
    public type(): InterruptType
    {
      return this.interruptType;
    }

    public parameters(): any
    {
      return this.interruptParameters;
    }
  }
  
  export enum InterruptType
  {
    TIMER,
    KEYBOARD,
    SYSTEM_CALL,
    BREAK,
    RETURN,
    SEG_FAULT,
    INVALID_OP,
    SWITCH
  }
}
