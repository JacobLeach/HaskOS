///<reference path="shellCommand.ts" />
///<reference path="userCommand.ts" />
///<reference path="stdio.ts" />
///<reference path="../utils.ts" />

/* ------------
   Shell.ts

   The OS Shell - The "command line interface" (CLI) for the console.
   ------------ */

// TODO: Write a base class / prototype for system services and let Shell inherit from it.

module TSOS {
    export class Shell {
        // Properties
        public promptStr = ">";
        public commandList = {};
        public curses = "[fuvg],[cvff],[shpx],[phag],[pbpxfhpxre],[zbgureshpxre],[gvgf]";
        public apologies = "[sorry]";
        public historyList = [];
        private current = -2;
        private ansi: boolean = false;
        private lastCharEscape: boolean = false;
        private ansiNumber  = "";

        private inputBuffer: String = "";

        constructor() {

        }

        public isr(character) {
          this.handleCharacter(character);
        }

        public init() {
            var sc = null;
            //
            // Load the command list.

            // ver
            sc = new ShellCommand(this.shellVer,
                                  "ver",
                                  "- Displays the current version data.");
            this.commandList[sc.command] = sc;

            // help
            sc = new ShellCommand(this.shellHelp,
                                  "help",
                                  "- This is the help command. Seek help.");
            this.commandList[sc.command] = sc;

            // shutdown
            sc = new ShellCommand(this.shellShutdown,
                                  "shutdown",
                                  "- Shuts down the virtual OS but leaves the underlying hardware simulation running.");
            this.commandList[sc.command] = sc;

            // cls
            sc = new ShellCommand(this.shellCls,
                                  "cls",
                                  "- Clears the screen and resets the cursor position.");
            this.commandList[sc.command] = sc;

            // man <topic>
            sc = new ShellCommand(this.shellMan,
                                  "man",
                                  "<topic> - Displays the MANual page for <topic>.");
            this.commandList[sc.command] = sc;

            // trace <on | off>
            sc = new ShellCommand(this.shellTrace,
                                  "trace",
                                  "<on | off> - Turns the OS trace on or off.");
            this.commandList[sc.command] = sc;

            // rot13 <string>
            sc = new ShellCommand(this.shellRot13,
                                  "rot13",
                                  "<string> - Does rot13 obfuscation on <string>.");
            this.commandList[sc.command] = sc;

            // prompt <string>
            sc = new ShellCommand(this.shellPrompt,
                                  "prompt",
                                  "<string> - Sets the prompt.");
            this.commandList[sc.command] = sc;
            
            // prompt <string>
            sc = new ShellCommand(this.shellKirby,
                                  "kirby",
                                  "- Displays Kirby");
            this.commandList[sc.command] = sc;
            
            // alias <alias> <command>
            sc = new ShellCommand(this.shellAlias,
                                  "alias",
                                  "<alias> <command> - Aliases a command");
            this.commandList[sc.command] = sc;
            
            // alias <alias> <command>
            sc = new ShellCommand(this.shellDate,
                                  "date",
                                  "- Displays current date and time");
            this.commandList[sc.command] = sc;
            
            // whereami
            sc = new ShellCommand(this.shellLocate,
                                  "whereami",
                                  "- Displays current location");
            this.commandList[sc.command] = sc;
            
            sc = new ShellCommand(this.shellCrash,
                                  "crash",
                                  "- Crashes the OS");
            this.commandList[sc.command] = sc;
            
            sc = new ShellCommand(this.shellStatus,
                                  "status",
                                  "- Changes the status bar status");
            this.commandList[sc.command] = sc;

            sc = new ShellCommand(this.shellLoad,
                                  "load",
                                  "- Loads a program");
            this.commandList[sc.command] = sc;

            // processes - list the running processes and their IDs
            // kill <id> - kills the specified process id.

            //
            // Display the initial prompt.
            this.putPrompt();
        }

        public putPrompt() {
            Stdio.putString(this.promptStr, _StdOut);
        }

        private handleTabCompletion() {
          var command = "";

          for(var current in this.commandList) {
            var currentCommand = this.commandList[current].command;
            if(currentCommand.indexOf(this.inputBuffer) == 0) {
              command = currentCommand;
            }
          }
          //Firgure out what part of the command we need to print
          var toPrint = command.substr(this.inputBuffer.length);

          //Correct the input buffer
          this.inputBuffer = command;

          //Make the screen look correct
          Stdio.putString(toPrint, _StdOut);

        }

        private handleCharacter(character: String): void {
          if(character === ENTER) {
            //Put line in history
            this.historyList[this.historyList.length] = this.inputBuffer;
            this.current = -2;

            //Send the enter to the terminal before processing
            Stdio.putString(character, _StdOut);
            
            //Remove leading and trailing spaces.
            this.inputBuffer = Utils.trim(this.inputBuffer);
            
            //Handle the command
            this.handleCommand(); 

            //Flush the buffer after we handle the command
            this.inputBuffer = "";
          }
          else if(character === TAB) {
            //Erase the tab that got printed to the screen
            this.handleTabCompletion();  
          }
          //This is copy paste and thus is evil.
          //Todo: Make not evil.
          else if(character === ESCAPE) {
            this.lastCharEscape = true;
          }
          else if(character === BACKSPACE) {
            if(this.inputBuffer.length > 0) {
              this.inputBuffer = this.inputBuffer.substr(0, this.inputBuffer.length - 1);
              Stdio.putString(character, _StdOut);
            }
          }
          else if(character === '[') {
            if(this.lastCharEscape) {
              this.ansi = true;
            }
            this.lastCharEscape = false;
          }
          else if(character === String.fromCharCode(0)) {
            //Do nothing
          }
          else if(this.ansi) {
            if(character >= '0' && character <= '9') {
              //+ "" is to make the type system happy
              this.ansiNumber += character + "";
            }
            else {
              var amount: any;
              if(this.ansiNumber === "") {
                amount = 1; 
              }
              else {
                amount= parseInt(this.ansiNumber, 10);
              }
              //Handle ANSI control codes
              switch(character) {
                //Handle an up arrow, aka command history
                case 'A':
                  if(this.current == -2) {
                    this.current = this.historyList.length;
                  }
                  if(this.current != 0) {
                    this.current--;
                  }
                  //These are ANSI control codes to control the cursor
                  //And to erase characters and stuff
                  //http://en.wikipedia.org/wiki/ANSI_escape_code
                  Stdio.putString(ESCAPE + "[K", _StdOut);
                  Stdio.putString(ESCAPE + "[0G", _StdOut);
                  this.putPrompt();
                  Stdio.putString(this.historyList[this.current], _StdOut);
                  this.inputBuffer = this.historyList[this.current]; 
                  break;
                //Handle a down arrow, aka command history
                case 'B':
                  if(this.current >= 0) {
                    if(this.current != this.historyList.length -1) {
                      this.current++;
                    }
                    Stdio.putString(ESCAPE + "[K", _StdOut);
                    Stdio.putString(ESCAPE + "[0G", _StdOut);
                    this.putPrompt();
                    Stdio.putString(this.historyList[this.current], _StdOut);
                    this.inputBuffer = this.historyList[this.current];
                  }
                  break;
                case 'C':
                  break;
                case 'D':
                  break;
                case 'E':
                  break;
                case 'F':
                  break;
              }
              this.ansiNumber = "";
              this.ansi = false;
            }
          }
          else {
            //Add input to the input buffer
            this.inputBuffer += character + "";

            //Send it to the terminal to display
            Stdio.putString(character, _StdOut);
          }
        }

        private handleCommand(): void {
          //Split by spaces for command and arguments
          var temp = this.inputBuffer.split(" ");

          //First element is the command
          var command = temp.shift();
          
          //Rest are parameters
          var parameters = temp;

          //If we haven't typed anything, don't check for a command
          if(this.inputBuffer.length > 0) {
            this.executeCommand(command, parameters);
          }
          
         /*
          * If the cursor is not at the beginning of the line,
          * we need to advance it to the next line before we 
          * print the prompt
          */
          if(_Console.getCursorPosition().x > 0) {
            Stdio.putString(ESCAPE + '[E', _StdOut);
          }
          
          if(command != "crash") {
            this.putPrompt();
          }
        }

       /*
        * Command is 'any' so typescript does not bitch about
        * indexing by String which is valid javascript
        */
        private executeCommand(command: any, parameters: String[]): void {
          if(this.commandList[command] != undefined) {
            this.commandList[command].func(parameters);
          }
          else {
            this.shellInvalidCommand();
          }
        }

        //
        // Shell Command Functions.  Again, not part of Shell() class per se', just called from there.
        //
        public shellInvalidCommand() {
            Stdio.putString("Invalid Command. ", _StdOut);
            if (_SarcasticMode) {
                Stdio.putString("Duh. Go back to your Speak & Spell.", _StdOut);
            } else {
                Stdio.putString("Type 'help' for, well... help.", _StdOut);
            }
        }

        public shellCurse() {
            Stdio.putString("Oh, so that's how it's going to be, eh? Fine.", _StdOut);
            _StdOut.advanceLine();
            Stdio.putString("Bitch.", _StdOut);
            _SarcasticMode = true;
        }

        public shellApology() {
           if (_SarcasticMode) {
              Stdio.putString("Okay. I forgive you. This time.", _StdOut);
              _SarcasticMode = false;
           } else {
              Stdio.putString("For what?", _StdOut);
           }
        }

        public shellVer(args) {
            Stdio.putString(APP_NAME + " version " + APP_VERSION, _StdOut);
        }

        public shellHelp(args) {
            Stdio.putString("Commands:", _StdOut);
            for (var i in _OsShell.commandList) {
                Stdio.putString(ESCAPE + '[E', _StdOut);
                Stdio.putString("  " + _OsShell.commandList[i].command + " " + _OsShell.commandList[i].description, _StdOut);
            }
        }

        public shellShutdown(args) {
             Stdio.putString("Shutting down...", _StdOut);
             // Call Kernel shutdown routine.
            _Kernel.krnShutdown();
            // TODO: Stop the final prompt from being displayed.  If possible.  Not a high priority.  (Damn OCD!)
        }

        public shellCls(args) {
          Stdio.putString(ESCAPE + '[J', _StdOut);
        }

        public shellMan(args) {
            if (args.length > 0) {
                var topic = args[0];
                switch (topic) {
                    case "help":
                        Stdio.putString("Help displays a list of (hopefully) valid commands.", _StdOut);
                        break;
                    default:
                        Stdio.putString("No manual entry for " + args[0] + ".", _StdOut);
                }
            } else {
                Stdio.putString("Usage: man <topic>  Please supply a topic.", _StdOut);
            }
        }

        public shellTrace(args) {
            if (args.length > 0) {
                var setting = args[0];
                switch (setting) {
                    case "on":
                        if (_Trace && _SarcasticMode) {
                            Stdio.putString("Trace is already on, dumbass.", _StdOut);
                        } else {
                            _Trace = true;
                            Stdio.putString("Trace ON", _StdOut);
                        }

                        break;
                    case "off":
                        _Trace = false;
                        Stdio.putString("Trace OFF", _StdOut);
                        break;
                    default:
                        Stdio.putString("Invalid arguement.  Usage: trace <on | off>.", _StdOut);
                }
            } else {
                Stdio.putString("Usage: trace <on | off>", _StdOut);
            }
        }

        public shellRot13(args) {
            if (args.length > 0) {
                // Requires Utils.ts for rot13() function.
                Stdio.putString(args.join(' ') + " = '" + Utils.rot13(args.join(' ')) +"'", _StdOut);
            } else {
                Stdio.putString("Usage: rot13 <string>  Please supply a string.", _StdOut);
            }
        }

        public shellPrompt(args) {
            if (args.length > 0) {
                _OsShell.promptStr = args[0];
            } else {
                Stdio.putString("Usage: prompt <string>  Please supply a string.", _StdOut);
            }
        }
        
        public shellKirby(args) {
          Stdio.putString("<(^.^)>", _StdOut);
        }
        
        public shellAlias(args) {
            if (args.length > 1) {
              //Only work if the command exists and the alias is not already a command
              if (_OsShell.commandList[args[1]] != undefined && _OsShell.commandList[args[0]] === undefined) {
                var sc = new ShellCommand(_OsShell.commandList[args[1]].func,
                                          args[0],
                                          _OsShell.commandList[args[1]].description);
                _OsShell.commandList[sc.command] = sc;
              }
            } 
            else {
              Stdio.putString("Usage: alias <alias> <command>  Please supply a alias and a command.", _StdOut);
            }
        }
        
        public shellDate(args) {
          var date = new Date();
          var formatted =  (date.getMonth() + 1) + "/" +
                            date.getDate() + "/" +
                            date.getFullYear() + " " + 
                            date.getHours() + ":" +
                            date.getMinutes() + ":" +
                           ((date.getSeconds() < 10) ? ("0" + date.getSeconds()) : ("" + date.getSeconds()));
          Stdio.putString(formatted, _StdOut);
        }
        
        public shellLocate(args) {
          if(navigator.geolocation) {
            navigator.geolocation.getCurrentPosition(function(position) {
              Stdio.putString("Latitude: " + position.coords.latitude + " Longitude: " + position.coords.longitude, _StdOut);
            });
          }
          else {
            Stdio.putString("I've alerted the NSA of your location.", _StdOut);
          }
        }
       
        //HACKS HACKS HACKS
        public shellCrash(args) {
          _Console.bluescreen();
          _Console.writeWhiteText("Gotta crash... Mmmhh kay.");
          _Kernel.krnShutdown();
        }
        
        public shellStatus(args) {
          document.getElementById("status").innerHTML = args[0];
        }
        
        public shellLoad(args) {
          var valid = true;
          var code  = (<HTMLInputElement>document.getElementById("taProgramInput")).value;
          console.log(code); 
          for(var i = 0; i < code.length; i++) {
            if(!((code[i] >= '0' && code[i] <= '9') || (code[i] >= 'A' && code[i] <= 'F') 
                  || (code[i] === ' ') || (code[i] === ENTER) || (code[i] === String.fromCharCode(10)))) {
                    console.log(code[i].charCodeAt(0));
              valid = false;
            }
          }

          if(valid) {
            Stdio.putString("It's valid!", _StdOut);
          }
          else {
            Stdio.putString("You done goofed.", _StdOut);
          }
        }
    }
}
