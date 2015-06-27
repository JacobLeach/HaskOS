///<reference path="../globals.ts" />

/* ------------
  Terminal.ts
  
  Requires globals.ts
  
  A terminal emulator.
------------ */

module TSOS {

  export class Terminal {

    private canvas: HTMLCanvasElement;
    private context;
    private font = '12pt Courier'

    private lineSpacing: number = 4;
    private rows: number;
    private columns: number;
    private charWidth: number;
    private lineHeight: number;

    private cursor = {x: 0, y: 0}
    private inputBuffer = [];

    private echo: boolean = true;
    private lastCharEscape: boolean = false;
    private ansi: boolean = false;
    private ansiNumber = "";

    private chars = [[]];
    private blink = false;
    private intervalID;

    /*
     * This controls whether or not the terminal is canonical or not.
     *
     * In other words, whether or not it buffers.
     * If it is canonical, it only passes the input back after a newline.
     * If it is non-ccanonical it passes back each character as they come.
     *
     * TODO: Make the shell do ALL the buffering
     *       AKA, put this in non-canonical mode
     *       AKA, make this SUPER dumb
    */
    private canonical = false;

    constructor(canvas: HTMLCanvasElement) {
      this.canvas = canvas;

      this.context = canvas.getContext('2d'); 
      this.context.font = this.font;

      this.charWidth = this.context.measureText(String.fromCharCode(9608)).width + 1;
      this.lineHeight = 12 * 1.5;

      //This is a bit of a hack
      //I chose -2 because it works
      this.columns = Math.round(this.canvas.height / this.charWidth) - 2
      this.rows = Math.round(this.canvas.width / this.lineHeight) - 2;

      for(var i = 0; i < this.columns; i++)
      {
        this.chars[i] = [];
        for(var j = 0; j < this.rows; j++)
        {
          this.chars[i][j] = ' ';
        }
      }
     
      /*
       * This is why Javascript is the worst language EVER invented
       * AND why it needs to burn in hell for all eternity.
       * AND why web development is literally complete shit.
       * AND why I fucking hate it with a fucking passion that
       * FUCKING burns hotter than NINETY FUCKING THOUSANND
       * MASSIVE BURNING FUCKING SUNS
      /
      this.intervalID = setInterval(
          (function(self) {
            return function() {
              self.printCursor(); 
            }
          })(this), 500);
          */
    }

  //HACKS HACKS HACKS!
    public bluescreen() {
      this.cursor.x = 0;
      console.log("FUCK");
      this.cursor.y = 0;
      this.context.fillStyle='#0000FF';
      this.context.fillRect(0, 0, this.canvas.width, this.canvas.height);
      this.context.fillStyle='#000000';
    }

    //HACKS HACKS HACKS!
    public writeWhiteText(text) {
      this.context.fillStyle='#FFFFFF';
      for(var i = 0; i < text.length; i++) {
        this.printChar(text.charAt(i), false);
      }
    }

    public printCursor(): void
    {
      if(this.blink)
      {
        this.clear();
        this.drawChar(this.chars[this.cursor.x][this.cursor.y]);
        this.blink = false;
      }
      else
      {
        this.clear();
        this.drawChar(String.fromCharCode(9608));
        this.blink = true;
      }
    }
   
    public write(data: Byte): void {
      var asChar: string = String.fromCharCode(data.asNumber()); 
      this.handleChar(asChar, true);
    }

    private handleInputChar(): void {
      if(this.inputBuffer.length > 0) {
        var character: String = this.inputBuffer[this.inputBuffer.length - 1]; 
        this.handleChar(character, true);
      }
    }

    private handleChar(character: String, isInput: boolean): void {
      //Add character to the input buffer
      //Wow. This is totally a bug somehow. I bet in canonical mode this is broken.
      this.inputBuffer.push(character);

      var printable: boolean = true;
      var input: String = "";
     
      //Checking to see if this is going to be an ANSI control code
      //If it is, then handle it
      if(character === ESCAPE) {
        printable = false;
        this.lastCharEscape = true;
      }
      else if(character === '[') {
        if(this.lastCharEscape) {
          this.ansi = true;
          printable = false;
        }
        this.lastCharEscape = false;
      }
      else if(character === String.fromCharCode(0)) {
        printable = false; 
      }
      else if(this.ansi) {
        if(character >= '0' && character <= '9') {
          //+ "" is to make the type system happy
          this.ansiNumber += character + "";
          printable = false;
        }
        else {
          var amount: any;
          if(this.ansiNumber === "") {
            amount = 1; 
          }
          else {
            amount= parseInt(this.ansiNumber, 10);
          }
          switch(character) {
            case 'A':
              this.moveCursorUp(amount);
              break;
            case 'B':
              this.moveCursorDown(amount);
              break;
            case 'C':
              this.moveCursorRight(amount);
              break;
            case 'D':
              this.moveCursorLeft(amount);
              break;
            case 'E':
              this.makeNewLine();
              break;
            case 'F':
              this.cursor.x = 0;
              this.moveCursorUp(1);
              break;
            case 'G':
              this.cursor.x = 0;
              break;
            case 'J':
              this.clearAll();
              break;
            case 'J':
              this.clearAll();
              break;
            case 'K':
              this.clearLine();
              break;
          }
          this.ansiNumber = "";
          printable = false;
          this.ansi = false;
        }
      }

      if(character === BACKSPACE) {
        //Pop the backspace
        this.inputBuffer.pop();
        printable = false;
         
        if(!this.canonical || this.inputBuffer.length > 0) { 
          //Do not print the backspace
          this.clear();
          this.drawChar(this.chars[this.cursor.x][this.cursor.y]);
          this.moveCursorLeft(1);
          this.clear();
          this.chars[this.cursor.x][this.cursor.y] = ' ';

          //Pop the erased character
          this.inputBuffer.pop();
        }
      }
      else if(character === ENTER) {
        printable = false;
        
        while(this.inputBuffer[0] != ENTER) {
          input += this.inputBuffer.shift();
        }
        
        //Remove the newline
        input += this.inputBuffer.shift();
        this.clear(); 
        this.makeNewLine();
      }
      //Not a special character and non-buffering
      else if(!this.canonical) {
        input += this.inputBuffer.shift();
      }

      //If it is a printable character, print it
      if(((this.echo && isInput) || !isInput)  && printable) {
        this.chars[this.cursor.x][this.cursor.y] = character;
        this.printChar(character, true); 
      }
    }

    private putChar(character: String): void {
      this.handleChar(character, false);
    }

    private moveCursorUp(amount: number): void {
      if(this.cursor.y - amount < 0) {
        this.cursor.y = 0;
      }
      else {
        this.cursor.y -= amount;   
      }
    }
    
    private moveCursorDown(amount: number): void {
      if(this.cursor.y + amount > this.rows) {
        this.cursor.y = this.rows;   
      }
      else {
        this.cursor.y += amount;   
      }
    }

    private moveCursorLeft(amount: number): void {
      if(this.cursor.x - amount < 0) {
        this.cursor.x = 0;
      }
      else {
        this.cursor.x -= amount;
      }
    }
    
    private moveCursorRight(amount: number): void {
      if(this.cursor.x + amount > this.columns) {
        this.cursor.x = this.columns;
      }
      else {
        this.cursor.x += amount;
      }
    }

    private makeNewLine() {
      this.cursor.x = 0;
      if(this.cursor.y === this.rows) { 
        
        //Get an image of the canvas of all but the top line
        var image = this.context.getImageData(0, this.lineHeight, this.context.canvas.width, this.rows * this.lineHeight);
        
        //Draw it at the top (aka scroll up)
        this.context.putImageData(image, 0, 0);

        //Clear the line that was just created (aka the bottom)
        this.clearLine();
      }
      else {
        this.moveCursorDown(1);
      }
    }

    private printChar(character: String, clearLine: boolean): void {
      //Get coordinates on the screen
      var coords = this.cursorToCoords();
      
      //Clear the spot incase a letter is already here
      if(clearLine) {
        this.clear();
      }

      //Write the letter to the screen
      this.context.fillText(character, coords.x, coords.y);
      
      //Advance the cursor
      this.cursor.x++;
      
      if(this.cursor.x == this.columns) {
        this.cursor.x = 0;
        this.makeNewLine(); 
      }
    }

    private drawChar(character: string): void
    {
      var coords = this.cursorToCoords();
      this.context.fillText(character, coords.x, coords.y);
    }

    private clear(): void {
      //Get topleft corner of the cursor location
      var topLeft = this.cursorTopLeft();

      //Clear the place of any previous character
      //Add the line spacing to make sure the lowest characters are removed
      this.context.clearRect(topLeft.x, topLeft.y, this.charWidth + 1, this.lineHeight + this.lineSpacing);
    }

    private clearLine(): void {
      //Get topleft corner of the cursor location
      var topLeft = this.cursorTopLeft();
      this.context.clearRect(0, topLeft.y, this.canvas.width, this.lineHeight + this.lineSpacing);
    }

    private clearAll(): void {
      this.cursor.x = 0;
      this.cursor.y = 0;
      this.context.clearRect(0, 0, this.canvas.width, this.canvas.height);
    }
    
    private cursorToCoords() {
      var newX = (this.cursor.x * this.charWidth) + 1;
      var newY = ((this.cursor.y + 1) * this.lineHeight) - this.lineSpacing;

      return {x: newX, y: newY};
    }
    
    private cursorTopLeft() {
      //Add one to move font off the edge one pixel
      var newX = (this.cursor.x * this.charWidth) + 1;
      var newY = this.cursor.y * this.lineHeight;

      return {x: newX, y: newY};
    }
  }
}
