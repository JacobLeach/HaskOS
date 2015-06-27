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
            
            sc = new ShellCommand(this.shellRun,
                                  "run",
                                  "- runs a program");
            this.commandList[sc.command] = sc;
            
            sc = new ShellCommand(this.ps,
                                  "ps",
                                  "- list loaded programs");
            this.commandList[sc.command] = sc;
            
            sc = new ShellCommand(this.kill,
                                  "kill",
                                  "<pid> - kills a program");
            this.commandList[sc.command] = sc;
            
            sc = new ShellCommand(this.clearmem,
                                  "clearmem",
                                  "- clears memory");
            this.commandList[sc.command] = sc;
            
            sc = new ShellCommand(this.runall,
                                  "runall",
                                  "- runs all programs");
            this.commandList[sc.command] = sc;
            
            sc = new ShellCommand(this.quantum,
                                  "quantum",
                                  "<ticks> - Changes round robin quantum");
            this.commandList[sc.command] = sc;

            // processes - list the running processes and their IDs
            // kill <id> - kills the specified process id.

            //
            // Display the initial prompt.
            this.putPrompt();
        }

        public ps(args): void 
        {
          liblos.ps(); 
        } 
        
        public kill(args): void 
        {
          liblos.kill(args[0]); 
        } 
        
        public clearmem(args): void 
        {
          liblos.clearmem(); 
        } 

        public runall(args): void
        {
          liblos.runall();
        }
        
        public quantum(args): void
        {
          _Quant = args[0];
        }

        public putPrompt() {
            Stdio.putString(this.promptStr);
        }

        private handleTabCompletion() {
          var command: any = [];

          for(var current in this.commandList) 
          {
            var currentCommand = this.commandList[current].command;
            
            if(currentCommand.indexOf(this.inputBuffer) == 0) 
            {
              command.push(currentCommand);
            }
          }

          if(command.length > 1)
          {
            Stdio.putString(ESCAPE + "[K");
            Stdio.putString(ESCAPE + "[G");
            this.putPrompt();

            for(var i = 0; i < command.length; i++)
            {
              Stdio.putString(command[i]);
              Stdio.putString(" ");
            }

            Stdio.putStringLn("");
            this.putPrompt();
            Stdio.putString(this.inputBuffer);
          }
          else
          {
            //Figure out what part of the command we need to print
            var toPrint = command[0].substr(this.inputBuffer.length);

            //Correct the input buffer
            this.inputBuffer = command[0];

            //Make the screen look correct
            Stdio.putString(toPrint);
          }
        }

        private handleCharacter(character: String): void {
          if(character === ENTER) {
            //Put line in history
            this.historyList[this.historyList.length] = this.inputBuffer;
            this.current = -2;

            //Send the enter to the terminal before processing
            Stdio.putString(character);
            
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
              Stdio.putString(character);
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
                    if(this.current === 0)
                    {
                      this.current = -2;
                    }
                  }
                  if(this.current != 0 && this.current != -2) {
                    this.current--;
                  }
                  
                  //These are ANSI control codes to control the cursor
                  //And to erase characters and stuff
                  //http://en.wikipedia.org/wiki/ANSI_escape_code
                  if(this.current != -2)
                  {
                    Stdio.putString(ESCAPE + "[K");
                    Stdio.putString(ESCAPE + "[0G");
                    this.putPrompt();
                    Stdio.putString(this.historyList[this.current]);
                    this.inputBuffer = this.historyList[this.current]; 
                  }
                  break;
                //Handle a down arrow, aka command history
                case 'B':
                  if(this.current >= 0) {
                    if(this.current != this.historyList.length -1) {
                      this.current++;
                    }
                    Stdio.putString(ESCAPE + "[K");
                    Stdio.putString(ESCAPE + "[0G");
                    this.putPrompt();
                    Stdio.putString(this.historyList[this.current]);
                    this.inputBuffer = this.historyList[this.current];
                  }
                  break;
                case 'C':
                  Stdio.putString(ESCAPE + "[C");
                  break;
                case 'D':
                  Stdio.putString(ESCAPE + "[D");
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
            Stdio.putString(character);
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
          //if(_Console.getCursorPosition().x > 0) {
          //  Stdio.putString(ESCAPE + '[E');
          //}
          
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
            Stdio.putStringLn("Invalid Command. ");
            if (_SarcasticMode) {
                Stdio.putStringLn("Duh. Go back to your Speak & Spell.");
            } else {
                Stdio.putStringLn("Type 'help' for, well... help.");
            }
        }

        public shellCurse() {
            Stdio.putStringLn("Oh, so that's how it's going to be, eh? Fine.");
            _StdOut.advanceLine();
            Stdio.putStringLn("Bitch.");
            _SarcasticMode = true;
        }

        public shellApology() {
           if (_SarcasticMode) {
              Stdio.putStringLn("Okay. I forgive you. This time.");
              _SarcasticMode = false;
           } else {
              Stdio.putStringLn("For what?");
           }
        }

        public shellVer(args) {
            Stdio.putStringLn(APP_NAME + " version " + APP_VERSION);
        }

        public shellHelp(args) {
            Stdio.putStringLn("Commands:");
            for (var i in _OsShell.commandList) {
                Stdio.putStringLn("  " + _OsShell.commandList[i].command + " " + _OsShell.commandList[i].description);
            }
        }

        public shellShutdown(args) {
            Stdio.putStringLn("Shutting down...");
            liblos.shutdown();
        }

        public shellCls(args) {
          Stdio.putString(ESCAPE + '[J');
        }

        public shellMan(args) {
            if (args.length > 0) {
                var topic = args[0];
                switch (topic) {
                    case "help":
                        Stdio.putStringLn("Help displays a list of (hopefully) valid commands.");
                        break;
                    default:
                        Stdio.putStringLn("No manual entry for " + args[0] + ".");
                }
            } else {
                Stdio.putStringLn("Usage: man <topic>  Please supply a topic.");
            }
        }

        public shellTrace(args) {
            if (args.length > 0) {
                var setting = args[0];
                switch (setting) {
                    case "on":
                        if (_Trace && _SarcasticMode) {
                            Stdio.putStringLn("Trace is already on, dumbass.");
                        } else {
                            _Trace = true;
                            Stdio.putStringLn("Trace ON");
                        }

                        break;
                    case "off":
                        _Trace = false;
                        Stdio.putStringLn("Trace OFF");
                        break;
                    default:
                        Stdio.putStringLn("Invalid arguement.  Usage: trace <on | off>.");
                }
            } else {
                Stdio.putStringLn("Usage: trace <on | off>");
            }
        }

        public shellRot13(args) {
            if (args.length > 0) {
                // Requires Utils.ts for rot13() function.
                Stdio.putStringLn(args.join(' ') + " = '" + Utils.rot13(args.join(' ')) +"'");
            } else {
                Stdio.putStringLn("Usage: rot13 <string>  Please supply a string.");
            }
        }

        public shellPrompt(args) {
            if (args.length > 0) {
                _OsShell.promptStr = args[0];
            } else {
                Stdio.putStringLn("Usage: prompt <string>  Please supply a string.");
            }
        }
        
        public shellKirby(args) {
          Stdio.putStringLn("<(^.^)>");
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
              Stdio.putStringLn("Usage: alias <alias> <command>  Please supply a alias and a command.");
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
          Stdio.putStringLn(formatted);
        }
        
        public shellLocate(args) {
          if(navigator.geolocation) {
            navigator.geolocation.getCurrentPosition(function(position) {
              Stdio.putStringLn("Latitude: " + position.coords.latitude + " Longitude: " + position.coords.longitude);
            });
          }
          else {
            Stdio.putStringLn("I've alerted the NSA of your location.");
          }
        }
       
        //HACKS HACKS HACKS
        public shellCrash(args) {
          _Console.bluescreen();
          _Console.writeWhiteText("Gotta crash... Mmmhh kay.");
          _Kernel.shutdown();
        }
        
        public shellStatus(args) {
          document.getElementById("status").innerHTML = args[0];
        }

        public shellRun(args): void 
        {
          liblos.runProgram(args[0]); 
        } 
        
        public shellLoad(args) {
          var code  = (<HTMLInputElement>document.getElementById("taProgramInput")).value;
          code = code.replace(/ /g,'');
          code = code.replace(/\n/g,'');
          var valid: boolean = true;
          
          for(var i = 0; i < code.length; i++) 
          {
            if(!((code[i] >= '0' && code[i] <= '9') || (code[i] >= 'A' && code[i] <= 'F')))
            {
              valid = false;
            }
          }

          if(valid && code != "" && ((code.length % 2) == 0)) 
          {
            Stdio.putString("Loading...");
            var pid: number = liblos.loadProgram();
            if(pid === undefined)
            {
              Stdio.putStringLn("No space.");
            }
            else 
            {
              Stdio.putStringLn(" Done.");
              Stdio.putStringLn("Pid: " + pid);
            }
          }
          else 
          {
            Stdio.putStringLn("You done goofed.");
          }
        }
    }
}
