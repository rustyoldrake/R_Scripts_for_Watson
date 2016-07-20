


const int ledPin1 =  1;      // the number of the LED pin
const int ledPin2 =  2;      // the number of the LED pin
const int ledPin3 =  3;      // the number of the LED pin
const int ledPin4 =  4;      // the number of the LED pin
const int ledPin5 =  5;      // the number of the LED pin
const int ledPin6 =  6;      // the number of the LED pin
const int ledPin7 =  7;      // the number of the LED pin
const int ledPin8 =  8;      // the number of the LED pin
const int ledPin13 =  13;      // the number of the LED pin

// Variables will change :
int ledState = LOW;             // ledState used to set the LED

// Generally, you should use "unsigned long" for variables that hold time
// The value will quickly become too large for an int to store
unsigned long previousMillis = 0;        // will store last time LED was updated

// constants won't change :
const long interval = 1000;           // interval at which to blink (milliseconds)

void setup() {
  // set the digital pin as output:
  pinMode(ledPin1, OUTPUT);
  pinMode(ledPin2, OUTPUT);
  pinMode(ledPin3, OUTPUT);
  pinMode(ledPin4, OUTPUT);
  pinMode(ledPin5, OUTPUT);
  pinMode(ledPin6, OUTPUT);
  pinMode(ledPin13, OUTPUT);

}

void loop() {
  // here is where you'd put code that needs to be running all the time.

  // check to see if it's time to blink the LED; that is, if the
  // difference between the current time and last time you blinked
  // the LED is bigger than the interval at which you want to
  // blink the LED.
  unsigned long currentMillis = millis();

  if (currentMillis - previousMillis >= interval) {
    // save the last time you blinked the LED
    previousMillis = currentMillis;

    // if the LED is off turn it on and vice-versa:
    if (ledState == LOW) {
      ledState = HIGH;
    } else {
      ledState = LOW;
    }
    
    digitalWrite(ledPin1, ledState); // this is for the polarity of volatage - each cycle

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

