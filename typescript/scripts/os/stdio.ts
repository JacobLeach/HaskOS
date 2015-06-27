/* ------------
  Stdio.ts
  
  Standard IO for the Terminal.
  ------------ */

module TSOS {
  
  export class Stdio {
    
    private static buffer: string = "";

    public static getChar(): number {
      var toReturn = Stdio.buffer[0];
      Stdio.buffer = Stdio.buffer.substring(1, Stdio.buffer.length);

      return toReturn.charCodeAt(0);
    }
    
    //Wrapper to write text to output
    public static putString(text: String) {
      for(var i = 0; i < text.length; i++) {
        Stdio.buffer += (text.charAt(i));
      }
      Stdio.buffer += String.fromCharCode(0);
      liblos.putString();
    }
    
    public static putStringLn(text: String) {
      for(var i = 0; i < text.length; i++) {
        Stdio.buffer += (text.charAt(i));
      }
      Stdio.buffer += String.fromCharCode(13);
      Stdio.buffer += String.fromCharCode(0);
      liblos.putString();
    }
  }
}
