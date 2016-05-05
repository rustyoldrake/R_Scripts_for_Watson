//######################################################
//### Boaty McBoatFace -  Chatting with Cognitive Toys
//### Experimental Code.  Experimental R Interface for IBM Watson Services - Arduino Controller 
//######################################################

#include <Servo.h> 
#include <Keyboard.h>

Servo myservo_a;  // create servo object to control a servo 
Servo myservo_b;  // create servo object to control a servo
Servo myservo_c;  // create servo object to control a servo 
Servo myservo_d;  // create servo object to control a servo 
Servo left_eye1;  // create servo object to control a servo 
Servo right_eye1;  // create servo object to control a servo 
Servo left_eye2;  // create servo object to control a servo 
Servo right_eye2;  // create servo object to control a servo 
Servo myservo_i;  // create servo object to control a servo 
Servo myservo_j;  // create servo object to control a servo 
Servo myservo_k;  // create servo object to control a servo 
 
/** Adjust these values for your servo and setup, if necessary **/  
//int LeftEye      =  8;    // control pin for servo motor  
//int RightEye     =  9;    // control pin for servo motor  

int minPulse     =  600;  // minimum servo position  
int maxPulse     =  2400; // maximum servo position  
int turnRate     =  100;  // servo turn rate increment (larger value, faster rate)  
int refreshTime  =  20;   // time (ms) between pulses (50Hz)  
 
/** The Arduino will calculate these values for you **/  
int centerServo;         // center servo position  
int pulseWidth;          // servo pulse width  
int moveServo;           // raw user input  
long lastPulse   = 0;    // recorded time (ms) of the last pulse  
 
 
void setup() {  
 //pinMode(LeftEye, OUTPUT);  // Set servo pin as an output pin  
 //pinMode(RightEye, OUTPUT);  // Set servo pin as an output pin  
 centerServo = maxPulse - ((maxPulse - minPulse)/2);  
 pulseWidth = centerServo;   // Give the servo a starting point (or it floats)  
 Serial.begin(9600);  
 Serial.println("      Arduino Serial Servo Control");  
 Serial.println("Press < or > to move, spacebar to center");  
 Serial.println();  

left_eye1.attach(6);  // attaches the servo on pin 8 to the servo object 
right_eye1.attach(7);  // attaches the servo on pin 8 to the servo object 
left_eye2.attach(8);  // attaches the servo on pin 8 to the servo object 
right_eye2.attach(9);  // attaches the servo on pin 8 to the servo object 

pinMode(10, OUTPUT);  // on/off for motor control - RELAY
pinMode(2, OUTPUT);  // on/off for LED - 1 on

digitalWrite(10, HIGH); // active high is not closed
digitalWrite(2, LOW); // active high is not closed

}  




 
void loop() {  
 // wait for serial input  
 if (Serial.available() > 0) {  
   // read the incoming byte:  
   moveServo = Serial.read();  
 
   // ASCII '<' is 44, ASCII '>' is 46 (comma and period, really)  
   //if (moveServo == 44) { pulseWidth = pulseWidth - turnRate; }  
   //if (moveServo == 46) { pulseWidth = pulseWidth + turnRate; }  
   // if (moveServo == 32) { pulseWidth = centerServo; }  

   if (moveServo == 32) // ' ' is space for reset
   { 
     left_eye1.write(100); 
     right_eye1.write(100); 
     left_eye2.write(100); 
     right_eye2.write(100); 
  
   } 


   if (moveServo == 108) // 'l' is for left look
   { 
     left_eye1.write(160); 
     right_eye1.write(160); 
     left_eye2.write(160); 
     right_eye2.write(160); 
   
   } 
   if (moveServo == 114) // 'r' is for right look
   { 
     left_eye1.write(20); 
     right_eye1.write(20); 
     left_eye2.write(20); 
     right_eye2.write(20);  
   } 

   if (moveServo == 99) // 'c' is for crosseyed ! :)
   { 
     left_eye1.write(60); 
     right_eye1.write(130); 
     left_eye2.write(60); 
     right_eye2.write(130); 
   } 

   if (moveServo == 97) // 'a' is for up relay! :)
   { digitalWrite(10, LOW); // I think active low to gate
   } 

      if (moveServo == 122) // 'z' is for down! relay :)
   { digitalWrite(10, HIGH); // active high is not closed
   } 

   if (moveServo == 50) // '2' is  'red' color on resistor 
   { 
    digitalWrite(2, HIGH); // active high is not closed
   } 

   if (moveServo == 48) // '0' is  'black' - so turn off
   { 
    digitalWrite(2, LOW); // active high is not closed
   } 
 
 
   // stop servo pulse at min and max  
   //if (pulseWidth > maxPulse) { pulseWidth = maxPulse; }  
   //if (pulseWidth < minPulse) { pulseWidth = minPulse; }  


 
   // print pulseWidth back to the Serial Monitor (uncomment to debug)  
   // Serial.print("Pulse Width: ");  
   // Serial.print(pulseWidth);  
   // Serial.println("us");   // microseconds  
 }  
 
 // pulse the servo every 20 ms (refreshTime) with current pulseWidth  
 // this will hold the servo's position if unchanged, or move it if changed  
// if (millis() - lastPulse >= refreshTime) {  
//   digitalWrite(RightEye, HIGH);   // start the pulse  
//   //digitalWrite(LeftEye, HIGH);   // start the pulse  
//   delayMicroseconds(pulseWidth);  // pulse width  
//   digitalWrite(RightEye, LOW);    // stop the pulse  
//   //digitalWrite(LeftEye, LOW);    // stop the pulse  
//   lastPulse = millis();           // save the time of the last pulse  
// }  
}  
