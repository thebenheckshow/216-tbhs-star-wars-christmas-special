int stepPin = 3; // Stepper pin
int dirPin = 4;   // Direction pin

void setup()
{
  pinMode(dirPin, OUTPUT); // sets the digital pin 4 as output
  pinMode(stepPin, OUTPUT);   // sets the digital pin 7 as input

  pinMode(13, 1);

}

void loop() {

  
  stepDrive(0, 800, 2000);
  delay(1000);
    
  stepDrive(1, 800, 2000);  
  delay(1000);
  
}



void stepDrive(int directionR, int numSteps, int theSpeed) {

  digitalWrite(dirPin, directionR);
  
  digitalWrite(13, 1);  
  
  for (int x = 0 ; x < numSteps ; x++ ) {
  
    digitalWrite(stepPin, 1);    // pulse
    delayMicroseconds(3);                    // waits 1ms for the servo to reach the position 
    digitalWrite(stepPin, 0);    // pulse
    delayMicroseconds(theSpeed);                    // waits 1ms for the servo to reach the position 
 
  } 

  digitalWrite(13, 0);  
 
}


