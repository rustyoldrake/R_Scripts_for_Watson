/// COGNITIVE BARTENDER
/// Can drive this from RStudio Code using PythonR, or use Serial Monitor here and send characters

#include <Keyboard.h>

int controlCommand;           // raw user input  

const int ledPin3 =  3;      // the number of the LED pin
const int ledPin4 =  4;      // the number of the LED pin
const int ledPin5 =  5;      // the number of the LED pin
const int ledPin6 =  6;      // the number of the LED pin
const int ledPin7 =  7;      // the number of the LED pin
 
void setup() {  
 Serial.begin(9600);  
 Serial.println("      Arduino Serial Pump Control");  
 Serial.println("Press < or > to move, spacebar to center");  
 Serial.println();  

  pinMode(ledPin3, OUTPUT);
  pinMode(ledPin4, OUTPUT);
  pinMode(ledPin5, OUTPUT);
  pinMode(ledPin6, OUTPUT);
  pinMode(ledPin7, OUTPUT);

  digitalWrite(ledPin3, HIGH);
  digitalWrite(ledPin4, HIGH);
  digitalWrite(ledPin5, HIGH);
  digitalWrite(ledPin6, HIGH);
  digitalWrite(ledPin7, HIGH);

}  

void loop() {  
 // wait for serial input  
 if (Serial.available() > 0) {  
   controlCommand = Serial.read();     // read the incoming byte: 


////// PUMP ACTIONS  (later change characters to 1,2,3,4,5 for more intuitive - rather than letters

   if (controlCommand == 97) // 'a' is GIN 1
   { 
    digitalWrite(ledPin3, LOW);   /// Engage Pump 1
    delay(1000);
    digitalWrite(ledPin3, HIGH); // RESTORE
   } 


   if (controlCommand == 115) // 's' is BLue Pump 2
   { 
     digitalWrite(ledPin4, LOW);   /// Engage Pump 2
    delay(1000);
    digitalWrite(ledPin4, HIGH); // RESTORE
   } 


   if (controlCommand == 100) // 'd' is Pink Pump 3 
   { 
    digitalWrite(ledPin5, LOW);   /// Engage Pump
    delay(1000);
    digitalWrite(ledPin5, HIGH); // RESTORE
   } 

   if (controlCommand == 102) // 'f' is Orange Pump 4
   { 
    digitalWrite(ledPin6, LOW);   /// Engage Pump
    delay(1000);
    digitalWrite(ledPin6, HIGH); // RESTORE
   } 

   if (controlCommand == 103) // 'g' is Purple Pump 5
   { 
    digitalWrite(ledPin7, LOW);   /// Engage Pump
    delay(1000);
    digitalWrite(ledPin7, HIGH); // RESTORE
   } 



   if (controlCommand == 32) // ' ' is space for reset  // 
   { 
    // OK - crude way to cycle through the relays for 2,3,4,5,6, 
    digitalWrite(ledPin3, HIGH);  // pump1
    digitalWrite(ledPin4, HIGH);  // pump2
    digitalWrite(ledPin5, HIGH);   // pump3
    digitalWrite(ledPin6, HIGH);   // pump4
    digitalWrite(ledPin7, HIGH);   // pump5

   } 
 
}  
 
}  

