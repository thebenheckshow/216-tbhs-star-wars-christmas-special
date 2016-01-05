
/*
 Stepper Motor Control - one revolution

 This program drives a unipolar or bipolar stepper motor.
 The motor is attached to digital pins 8 - 11 of the Arduino.

 The motor should revolve one revolution in one direction, then
 one revolution in the other direction.


 Created 11 Mar. 2007
 Modified 30 Nov. 2009
 by Tom Igoe

 */

#include <Stepper.h>

const int stepsPerRevolution = 200;  // change this to fit the number of steps per revolution
// for your motor

// initialize the stepper library on pins 8 through 11:
Stepper myStepper(stepsPerRevolution, 2, 3, 4, 5);

int enablePIN_1 = 0;
int enablePIN_2 = 7;

void setup() {
  // set the speed at 60 rpm:
  myStepper.setSpeed(30);
  
  // initialize the serial port:
  Serial.begin(9600);

  pinMode(enablePIN_1, OUTPUT); 
  pinMode(enablePIN_2, OUTPUT); 

  digitalWrite(enablePIN_1, HIGH);
  digitalWrite(enablePIN_2, HIGH);

}

void loop() {
  // step one revolution  in one direction:
  Serial.println("clockwise");
  myStepper.step(20);
  delay(500);

  // step one revolution in the other direction:
  Serial.println("counterclockwise");
  myStepper.step(-20);
  delay(500);
}

