/*
 * MemoryManager.ts
 *
 * A memory manager object. Handles allocating and deallocating memory.
 */

module TSOS 
{

  export class MemoryManager 
  {
    private static SEGMENT_SIZE: number = 256;

    private memorySegments: boolean[];

    constructor() 
    {
      this.memorySegments = [];
    }

    public isReserved(segment: number): boolean
    {
      return this.memorySegments[segment];
    }

    public allocate(): MemoryBounds 
    {
      for(var i = 0; i < this.memorySegments.length; i++) 
      {
        if(!this.memorySegments[i] || this.memorySegments[i] === undefined) 
        {
          return this.reserve(i);
        }
      }
      
      return undefined;
    }

    public reserve(segment: number): MemoryBounds
    {
      this.memorySegments[segment] = true;
       
      return this.getBounds(segment);
    }

    public getBounds(segment: number): MemoryBounds
    {
      var toReturn: MemoryBounds;
      
      var lowerBound = segment * MemoryManager.SEGMENT_SIZE;
      var upperBound = lowerBound + MemoryManager.SEGMENT_SIZE - 1;
      
      toReturn = new MemoryBounds(segment, new Short(lowerBound), new Short(upperBound)); 
       
      return toReturn;
    }

    public deallocate(segment: number): void 
    {
      if(segment < 0 || 
         segment > this.memorySegments.length || 
         !this.memorySegments[segment])
      {
        //SEGFAULT
      }
      else 
      {
        this.memorySegments[segment] = false;
      }
    }
  }

  export class MemoryBounds 
  {
    private segment: number;
    private lowerBound: Short;
    private upperBound: Short;

    constructor(segment: number, lowerBound: Short, upperBound: Short) 
    {
      this.segment = segment;
      this.lowerBound = lowerBound;
      this.upperBound = upperBound;
    }
    
    public getSegment(): number
    {
      return this.segment;
    }

    public lower(): Short 
    {
      return this.lowerBound;
    }

    public upper(): Short 
    {
      return this.upperBound;
    }
  }
}
