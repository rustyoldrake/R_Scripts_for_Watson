//######################################################
//### ARDUINO CODE - Driver For Eyes and Lights 
//### Experimental Code.  If you get 'resource is busy' just unplug and 'detach' from R python code  
//######################################################

#include <Keyboard.h>

int listenSerial;           // raw user input  
 
void setup() {  
 Serial.begin(9600);  
 Serial.println("      Arduino Serial Servo Control");  
 Serial.println("Press < or > to move, spacebar to center");  
 Serial.println();  


// LEDs
pinMode(2, OUTPUT);  // on/off for LED - 1 on
pinMode(3, OUTPUT);  // on/off for LED - 1 on
pinMode(4, OUTPUT);  // on/off for LED - 1 on
pinMode(5, OUTPUT);  // on/off for LED - 1 on
pinMode(6, OUTPUT);  // on/off for LED - 1 on
pinMode(10, OUTPUT);  // on/off for RELAY X

digitalWrite(2, LOW); // active high is not closed
digitalWrite(3, LOW); // active high is not closed
digitalWrite(4, LOW); // active high is not closed
digitalWrite(5, LOW); // active high is not closed
digitalWrite(6, LOW); // active high is not closed
digitalWrite(10, LOW); // active high is not closed

}  

void loop() {  
 // wait for serial input  
 if (Serial.available() > 0) {  
   // read the incoming byte:  
   listenSerial = Serial.read();  

     // we really shoudl use SWITCH here or CASE
  
   if (listenSerial == 114)  {  digitalWrite(2, HIGH); }  // r RED
   if (listenSerial == 103)  {  digitalWrite(3, HIGH); }  // g GREEN
   if (listenSerial == 98)  {  digitalWrite(4, HIGH); }  // b BLUE
   if (listenSerial == 119)  {  digitalWrite(5, HIGH); }  // w WHITE (not on headset)
   if (listenSerial == 121)  {  digitalWrite(6, HIGH); }  // y YELLOW 

   if (listenSerial == 32) // SPACE CLEARS EVERYTHING
   { 
    digitalWrite(2, LOW); // all leds off
    digitalWrite(3, LOW);
    digitalWrite(4, LOW);
    digitalWrite(5, LOW);
    digitalWrite(6, LOW);
   }  

   if (listenSerial == 120)  {  
    digitalWrite(10, HIGH); 
    delay(3000); // wait 3 seconds - this is the 'super move - that closes the RELAY to turn on the high current LED strips
    digitalWrite(10, LOW); 
    }  // x ASCII x is 120 


 }  
 
}  

