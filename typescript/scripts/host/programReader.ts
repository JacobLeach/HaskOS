module TSOS
{
  export class ProgramReader
  {
    private address: Byte;
    
    constructor()
    {
      this.address = new Byte(0);
    }

    public setAddress(address: Byte): void
    {
      this.address = address;
    }

    public getByte(): Byte
    {
      var code: string = (<HTMLInputElement>document.getElementById("taProgramInput")).value;
      code = code.replace(/ /g,'');
      code = code.replace(/\n/g,'');
      
      var index: number  = this.address.asNumber() * 2;
      var first: string  = code[index];
      var second: string = code[index + 1]

      var asNumber: number = parseInt((first + second), 16);

      return new Byte(asNumber);
    }

    public atEnd(): Byte
    {
      var code: string = (<HTMLInputElement>document.getElementById("taProgramInput")).value;
      code = code.replace(/ /g,'');
      code = code.replace(/\n/g,'');
      
      if(this.address.asNumber() * 2 >= code.length)
      {
        return new Byte(0);
      }
      else
      {
        return new Byte(1);
      }
    }
  } 
}
