int stepPin = 3; // Stepper pin
int dirPin = 4;   // Direction pin
int dir_0 = 0;     // Direction 0
int dir_1 = 1;     // Direction 1

int speedStep = 50;

void setup()
{
  pinMode(dirPin, OUTPUT); // sets the digital pin 4 as output
  pinMode(stepPin, OUTPUT);   // sets the digital pin 3 as output
  
  digitalWrite(dirPin, 0);    // set direction

}

void loop()
{


  
}

void crushCore()
{

  if (digitalRead(6) == 0) {
    stepUp();  
  }
  
  if (digitalRead(7)) {
    digitalWrite(dirPin, 0);
    speedStep = 300;  
  }
  else {
    digitalWrite(dirPin, 1);
    speedStep = 200;  
  }
  
}

void stepUp() {
  
    digitalWrite(stepPin, 1);    // pulse
    delayMicroseconds(speedStep);                    // waits 1ms for the servo to reach the position 
    digitalWrite(stepPin, 0);    // pulse
    delayMicroseconds(speedStep);                    // waits 1ms for the servo to reach the position 
 
}



