int stepPin = 4; // Stepper pin
int dirPin = 3;   // Direction pin
int dir_1 = 0;     // Direction 0
int dir_0 = 1;     // Direction 1

#define right    1
#define left     0
#define low      0
#define high     1

int speedStep = 800;

void setup()
{
  pinMode(dirPin, OUTPUT); 
  pinMode(stepPin, OUTPUT);
    
  digitalWrite(dirPin, left);
  digitalWrite(stepPin, low);

}

void loop()
{

  digitalWrite(stepPin, HIGH);
  delayMicroseconds(100);          
  digitalWrite(stepPin, LOW); 
  delayMicroseconds(100);
  
}


void stepper(unsigned char whichWay, unsigned short howLong) {

  digitalWrite(dirPin, whichWay);
  
  for (int x = 0 ; x < howLong ; x++) {
    digitalWrite(stepPin, 1);
    delayMicroseconds(speedStep);
    digitalWrite(stepPin, 0);
    delayMicroseconds(speedStep);        
  }
 
}

void stepperTest() {
  
  for (int x = 0 ; x < 1000 ; x++) {    
    stepper(right, 1);  
  }
  
  delay(1000);
 
  for (int x = 0 ; x < 1000 ; x++) {    
    stepper(left, 1);  
  }

  delay(1000);

}

void crusher() {
  
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
