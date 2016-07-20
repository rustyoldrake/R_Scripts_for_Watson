/// COGNITIVE LEGO - ROBOT ARM - This Arduino code is meant to be LISTENING for serial commands from R
/// 

// #include <Servo.h> 
#include <Keyboard.h>

int moveServo;           // raw user input  

const int ledPin2 =  2;      // the number of the LED pin
const int ledPin3 =  3;      // the number of the LED pin
const int ledPin4 =  4;      // the number of the LED pin
const int ledPin5 =  5;      // the number of the LED pin
const int ledPin6 =  6;      // the number of the LED pin
const int ledPin13 =  13;      // the number of the LED pin
 
void setup() {  
 Serial.begin(9600);  
 Serial.println("      Arduino Serial Servo Control");  
 Serial.println("Press < or > to move, spacebar to center");  
 Serial.println();  


  pinMode(ledPin2, OUTPUT);
  pinMode(ledPin3, OUTPUT);
  pinMode(ledPin4, OUTPUT);
  pinMode(ledPin5, OUTPUT);
  pinMode(ledPin6, OUTPUT);
  pinMode(ledPin13, OUTPUT);


  digitalWrite(ledPin2, HIGH);
  digitalWrite(ledPin3, HIGH);
  digitalWrite(ledPin4, HIGH);
  digitalWrite(ledPin5, HIGH);
  digitalWrite(ledPin6, HIGH);
  digitalWrite(ledPin13, HIGH);

}  

void loop() {  
 // wait for serial input  
 if (Serial.available() > 0) {  
   moveServo = Serial.read();     // read the incoming byte: 




////// LEDS AND MOTORS

   if (moveServo == 97) // 'a' is Claw Close
   { 
    digitalWrite(ledPin13, LOW); // this is for the polarity of volatage 
    digitalWrite(ledPin2, LOW);   /// CLAW
    delay(1000);
    digitalWrite(ledPin2, HIGH); // RESTORE
   } 


   if (moveServo == 122) // 'z' is Claw Open
   { 
    digitalWrite(ledPin13, HIGH); // this is for the polarity of volatage 
    digitalWrite(ledPin2, LOW);   /// CLAW
    delay(1000);
    digitalWrite(ledPin2, HIGH); // RESTORE
   } 


   if (moveServo == 115) // 's' is Wrist CLose
   { 
    digitalWrite(ledPin13, LOW); // this is for the polarity of volatage 
    digitalWrite(ledPin3, LOW);   /// CLAW
    delay(1000);
    digitalWrite(ledPin3, HIGH); // RESTORE
   } 


   if (moveServo == 120) // 'x' is Wrist Open
   { 
    digitalWrite(ledPin13, HIGH); // this is for the polarity of volatage 
    digitalWrite(ledPin3, LOW);   /// CLAW
    delay(1000);
    digitalWrite(ledPin3, HIGH); // RESTORE
   } 


   if (moveServo == 100) // 'd' is Elbow Close
   { 
    digitalWrite(ledPin13, LOW); // this is for the polarity of volatage 
    digitalWrite(ledPin4, LOW);   /// CLAW
    delay(1000);
    digitalWrite(ledPin4, HIGH); // RESTORE
   } 


   if (moveServo == 99) // 'c' is Elbow Open
   { 
    digitalWrite(ledPin13, HIGH); // this is for the polarity of volatage 
    digitalWrite(ledPin4, LOW);   /// CLAW
    delay(1000);
    digitalWrite(ledPin4, HIGH); // RESTORE
   } 


   if (moveServo == 102) // 'f' is SHoulder Close
   { 
    digitalWrite(ledPin13, LOW); // this is for the polarity of volatage 
    digitalWrite(ledPin5, LOW);   /// 
    delay(1000);
    digitalWrite(ledPin5, HIGH); // RESTORE
   } 


   if (moveServo == 118) // 'v' is Shoulder Open
   { 
    digitalWrite(ledPin13, HIGH); // this is for the polarity of volatage 
    digitalWrite(ledPin5, LOW);   /// 
    delay(1000);
    digitalWrite(ledPin5, HIGH); // RESTORE
   } 


   if (moveServo == 103) // 'g' is ROtate Left
   { 
    digitalWrite(ledPin13, LOW); // this is for the polarity of volatage 
    digitalWrite(ledPin6, LOW);   /// CLAW
    delay(1000);
    digitalWrite(ledPin6, HIGH); // RESTORE
   } 


   if (moveServo == 98) // 'b' is ROtate RIght
   { 
    digitalWrite(ledPin13, HIGH); // this is for the polarity of volatage 
    digitalWrite(ledPin6, LOW);   /// 
    delay(1000);
    digitalWrite(ledPin6, HIGH); // RESTORE
   } 


   if (moveServo == 32) // ' ' is space for reset  // 
   { 
    digitalWrite(ledPin13, HIGH); // this is for the polarity of volatage - each cycle

    // OK - crude way to cycle through the relays for 2,3,4,5,6, 
    digitalWrite(ledPin2, LOW);   /// CLAW
    delay(1000);
    digitalWrite(ledPin2, HIGH); // WRIST restore this before moving on 
    digitalWrite(ledPin3, LOW);  // pull low to activate for 1 second
    delay(1000);
    digitalWrite(ledPin3, HIGH);  // ELBOW
    digitalWrite(ledPin4, LOW);   
    delay(1000);
    digitalWrite(ledPin4, HIGH);  // SHOULDER
    digitalWrite(ledPin5, LOW);   
    delay(1000);
    digitalWrite(ledPin5, HIGH);   // WAIST
    digitalWrite(ledPin6, LOW);   //
    delay(1000);
    digitalWrite(ledPin6, HIGH);   // restore before moving on
   } 
 
}  
 
}  
