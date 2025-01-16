-- This is an alternative formulation of hsm.m I wrote as an experiment. hsm.m
-- used recursion which was not supported by Rumur, and I was curious what a
-- non-recursive version of hsm.m would look like. There's not a lot to be
-- gained by reading the model, but it might serve as an interesting test case
-- as a pseudo realistic model.
--
-- Matthew Fernandez, 2019-09

type
  state_t: enum { vehiclesGreen, vehiclesGreenInt, vehiclesYellow,
                  pedestriansWalk, pedestriansFlash, off };
var
  state:               state_t;
  signalPedestrians:   enum { DONT_WALK, WALK, BLANK };
  signalVehicles:      enum { GREEN, YELLOW, RED };
  isPedestrianWaiting: boolean;
  timerOn:             boolean;
  pedestrianFlashCtr:  0..7;

function in_vehiclesEnabled(st: state_t): boolean; begin
  return st = vehiclesGreen | st = vehiclesGreenInt | st = vehiclesYellow;
end;

function in_pedestriansEnabled(st: state_t): boolean; begin
  return st = pedestriansWalk | st = pedestriansFlash;
end;

function in_operational(st: state_t): boolean; begin
  return in_vehiclesEnabled(st) | in_pedestriansEnabled(st);
end;

procedure entry_vehiclesGreen(); begin
  timerOn := true;
  signalVehicles := GREEN;
  isPedestrianWaiting := false;
  state := vehiclesGreen;
end;

procedure entry_vehiclesGreenInt(); begin
  state := vehiclesGreenInt;
end;

procedure entry_vehiclesYellow(); begin
  timerOn := true;
  signalVehicles := YELLOW;
  state := vehiclesYellow;
end;

procedure entry_vehiclesEnabled(); begin
  signalPedestrians := DONT_WALK;
  entry_vehiclesGreen();
end;

procedure entry_pedestriansWalk(); begin
  timerOn := true;
  signalPedestrians := WALK;
  state := pedestriansWalk;
end;

procedure entry_pedestriansFlash(); begin
  timerOn := true;
  pedestrianFlashCtr := 7;
  state := pedestriansFlash;
end;

procedure entry_pedestriansEnabled(); begin
  signalVehicles := RED;
  entry_pedestriansWalk();
end;

procedure entry_operational(); begin
  entry_vehiclesEnabled();
end;

procedure exit(st: state_t); begin
  if st = vehiclesGreen then
    timerOn := false;
  elsif st = vehiclesYellow then
    timerOn := false;
  elsif st = pedestriansWalk then
    timerOn := false;
  elsif st = pedestriansFlash then
    timerOn := false;
  end;
end;

startstate "initial transition" begin
  entry_operational();
end;

rule "PEDESTRIAN_WAITING 1" state = vehiclesGreen ==> begin
  isPedestrianWaiting := true;
end;

rule "TIMEOUT 1" state = vehiclesGreen ==> begin
  exit(state);
  if isPedestrianWaiting then
    entry_vehiclesYellow();
  else
    entry_vehiclesGreenInt();
  end;
end;

rule "PEDESTRIAN_WAITING 2" state = vehiclesGreenInt ==> begin
  exit(state);
  entry_vehiclesYellow();
end;

rule "TIMEOUT 2" state = vehiclesYellow ==> begin
  exit(state);
  entry_pedestriansEnabled();
end;

rule "TIMEOUT 3" state = pedestriansWalk ==> begin
  exit(state);
  entry_pedestriansFlash();
end;

rule "TIMEOUT 4" state = pedestriansFlash ==> begin
  pedestrianFlashCtr := pedestrianFlashCtr - 1;
  if pedestrianFlashCtr = 0 then
    exit(state);
    entry_vehiclesEnabled();
  elsif pedestrianFlashCtr % 2 = 0 then
    signalPedestrians := DONT_WALK;
  else
    signalPedestrians := BLANK;
  end;
end;

rule "OFF" in_operational(state) ==> begin
  exit(state);
  state := off;
end;

invariant "safety"
    signalPedestrians = DONT_WALK
  | signalPedestrians = BLANK
  | signalVehicles = RED;
