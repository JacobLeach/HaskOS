///<reference path="deviceDriver.ts" />

/* ----------------------------------
   DeviceDriverKeyboard.ts

   Requires deviceDriver.ts

   The Kernel Keyboard Device Driver.
   ---------------------------------- */

module TSOS {

    // Extends DeviceDriver
    export class DeviceDriverKeyboard extends DeviceDriver {

        constructor() {
            super(this.init, this.handleKeyPress);
        }

        public init() {
            this.status = "loaded";
        }

        // Turn scancode into ascii.
        // Ugly way of doing it but it works.
        public handleKeyPress(params) {
            //TODO: Check that they are valid and osTrapError if not.
            var scanCode = params[0];
            var isShifted = params[1];
            
            _Kernel.krnTrace("Scancode:" + scanCode + " shifted:" + isShifted);
            
            var character = "";
            
            //Is a character
            if ((scanCode >= 65) && (scanCode <= 90)) {
              //If it is not shifted, add 32 to get the ASCII lowercase character
              if (!isShifted) {
                character = String.fromCharCode(scanCode + 32);
              }
              //Otherwise it is already correct
              else {
                character = String.fromCharCode(scanCode);
              }
            }
            //Is a top row number key
            else if ((scanCode >= 48) && (scanCode <= 57)) {
              //If it is not shifted, the ASCII code is already correct
              if (!isShifted) {
                character = String.fromCharCode(scanCode);
              }
              //Otherwise it is not
              else {
                //Based on a standard keyboard
                switch(scanCode) {
                  case 48:
                    character = ')';
                    break;
                  case 49:
                    character = '!';
                    break;
                  case 50:
                    character = '@';
                    break;
                  case 51:
                    character = '#';
                    break;
                  case 52:
                    character = '$';
                    break;
                  case 53:
                    character = '%';
                    break;
                  case 54:
                    character = '^';
                    break;
                  case 55:
                    character = '&';
                    break;
                  case 56:
                    character = '*';
                    break;
                  case 57:
                    character = '(';
                    break;
                }//end case
              }//end else
            }//end else if
            //If it is a key with two symbols on it
            else if ((scanCode >= 186) && (scanCode <= 192) || 
                    ((scanCode >= 219) && scanCode <= 222)) {
              if(!isShifted) {
                switch(scanCode) {
                  case 186:
                    character = ';';
                    break;
                  case 187:
                    character = '=';
                    break;
                  case 188:
                    character = ',';
                    break;
                  case 189:
                    character = '-';
                    break;
                  case 190:
                    character = '.';
                    break;
                  case 191:
                    character = '/';
                    break;
                  case 192:
                    character = '`';
                    break;
                  case 219:
                    character = '[';
                    break;
                  case 220:
                    character = '\\';
                    break;
                  case 221:
                    character = ']';
                    break;
                  case 222:
                    character = '\'';
                    break;
                }//end switch
              }//end if
              else {
                switch(scanCode) {
                  case 186:
                    character = ':';
                    break;
                  case 187:
                    character = '+';
                    break;
                  case 188:
                    character = '<';
                    break;
                  case 189:
                    character = '_';
                    break;
                  case 190:
                    character = '>';
                    break;
                  case 191:
                    character = '?';
                    break;
                  case 192:
                    character = '~';
                    break;
                  case 219:
                    character = '{';
                    break;
                  case 220:
                    character = '|';
                    break;
                  case 221:
                    character = '}';
                    break;
                  case 222:
                    character = '"';
                    break;
                }//end switch
              }//end else
            }//end else if
            //Check for cases where scancode == ASCII code
            //Space = 32
            //Enter = 13
            //Escape = 27
            //Backspace = 8
            //Tab = 9
            else if ((scanCode == 32) ||
                     (scanCode == 13) ||
                     (scanCode == 27) ||
                     (scanCode == 8)  || 
                     (scanCode == 9)) {
              character = String.fromCharCode(scanCode);
            }
            //Arrow keys
            else if(scanCode >= 37 && scanCode <= 40) {
              //This implements ANSI control codes
              switch(scanCode) {
                case 37:
                  _KernelInputQueue.enqueue(String.fromCharCode(27));
                  _KernelInputQueue.enqueue("[");
                  character = "D";
                  break;
                case 38:
                  _KernelInputQueue.enqueue(String.fromCharCode(27));
                  _KernelInputQueue.enqueue("[");
                  character = "A";
                  break;
                case 39:
                  _KernelInputQueue.enqueue(String.fromCharCode(27));
                  _KernelInputQueue.enqueue("[");
                  character = "C";
                  break;
                case 40:
                  _KernelInputQueue.enqueue(String.fromCharCode(27));
                  _KernelInputQueue.enqueue("[");
                  character = "B";
                  break;
              }
            }
            //Non-handled characters are just made null
            else {
              character = String.fromCharCode(0); 
            }
            _KernelInputQueue.enqueue(character);
        }//end handleKeyPress
    }
}
