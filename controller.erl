%% Assignment 2: Traffic lights
%% Due on: To be decided.

%% Please fill the following details
%% Full Name: Srijan R Shetty
%% Roll No: 11727


%% In this assignment you have to design a controller that will
%% control the traffic at a pedestrian crossing on a busy highway
%% using two traffic lights. These traffic lights are simple 2-state
%% traffic light, and an implementation is given in the module signal:

%% http://github.com/piyush-kurur/sample-code/blob/assignment/assignments/traffic/signal.erl

%% One of the traffic signal controls the flow of vehicles and the
%% other allows/disallows the pedestrians from crossing.
%%
%% Implement the controller using the erlang function

%% controller(TraffiPid,PedestrianPid,State)

%%
%% The rules of pedestrian crossing is the following:
%%
%% 1. At no point of time should both the signals be green. Both can
%% however be red (see rules 5 and 6 below).
%%
%% 2. The traffic light for vehicles will be green by default and
%% should only turn red on request from a pedestrian subject to the
%% condition that there is a minimum gap of at least 10minutes
%% between two consecutive red lights.
%%
%% 3. The pedestrian can request for a stop of traffic by pressing a
%% stop button. This will send the message 'stop' to the controller.
%%
%% 4. The pedestrian signal should remain green for 5min at a stretch.
%% during which all requests for stop should be ignored (the
%% crossing is already green).
%%
%% 5. To allow the last pedestrian to cross the road safely make sure
%% that the highway signal becomes green only after 2min of the
%% pedestrian signal becoming red.
%%
%% 6. Similarly to allow the last vehicle to cross the junction, make
%% sure that the pedestrian signal becomes green only after 1min of
%% the traffic signal becoming red.

%
% A simple traffic signal light that consists of two states, red and
% green. The controller is supposed to use two such lights.
%

% Note that the atom 'error' in the given defination has been replaced with the
% catchall which according to me was the intended idea.

-module(controller).
-export([send_mesg/2, start/0, stop/1, ask/1]).

send_mesg(Pid, Message) ->
    Pid ! {self(), Message}.

switch_state(Pid, State, NewState) ->
    case NewState of
        red ->
            send_mesg(Pid, 'now red'),
            signal_light(Pid, red);
        green ->
            send_mesg(Pid, 'now green'),
            signal_light(Pid, green);
        _ ->
            send_mesg(Pid,{'unknown new state', NewState}),
            signal_light(Pid, State)
    end.

signal_light(State) ->
    receive
        {Pid, ask} ->
            send_mesg(Pid,State),
            signal_light(State);
        {Pid, NewState} ->
            switch_state(Pid, State, NewState);
        _ ->  
            signal_light(State)
    end.

%% This is the button the pedestrian presse to enquire the status of the lights
ask(ContollerPid) ->
    ContollerPid ! {self(), ask}.

%% This is the stop button that the pedestrian presses when he wants to cross
%% the road
stop(ContollerPid) ->
    ContollerPid ! {self(), stop}.

%% This function spawns a new pedestrian process whose Pid is returned to the
%% calling function, the controller used this Pid henceforth
signal_light(_, State) ->
    spawn(fun () -> signal_light(State) end).

%% The controller, the muscle of this program; given the Pid of the traffic
%% light and the Pedestrian traffic light, and their states, it recieves various
%% messages and acts accordingly
controller(TrafficPid, PedestrianPid, {Tstate, Pstate, Status}) ->
    receive
        %% Here the asking Pedestrian is given a reply as to the state of the
        %% lights
        {Pid, ask} ->
            TrafficPid ! {Pid, ask},
            PedestrianPid ! {Pid, ask},
            controller(TrafficPid, PedestrianPid, {Tstate, Pstate, Status});

        %% The pedestrain requests for a change in the light
        {Pid, stop} ->
            case Status of 
                traffic_lock ->
                    send_mesg(Pid, 'cannot change state, traffic is locked'),
                    controller(TrafficPid, PedestrianPid, {Tstate, Pstate, Status});
                request_lock ->
                    send_mesg(Pid, 'cannot change state, already open'),
                    controller(TrafficPid, PedestrianPid, {Tstate, Pstate, Status});
                request_sent ->
                    send_mesg(Pid, 'cannot change state, clearing traffic'),
                    controller(TrafficPid, PedestrianPid, {Tstate, Pstate, Status});
                _ ->
                    send_mesg(Pid, 'traffic light about to close in 1'),
                    timer:send_after(60000, self(), {Pid, traffic_stop}),
                    controller(TrafficPid, PedestrianPid, {Tstate, Pstate, request_sent})
            end;

        %% On recieving this signal, the controller changes the vehicular
        %% traffic is stopped and only pedestrain traffic is allowed for the
        %% next 5 minutes
        {Pid, traffic_stop} ->
            send_mesg(Pid, 'traffic light closed, pedestrian light open for 5'),
            T = switch_state(Pid, Tstate, red),
            P = switch_state(Pid, Pstate, green),
            timer:send_after(300000, self(), {Pid, pedestrian_stop}),
            controller(T, P, {red, green, request_lock});

        %% On recieving this signal, the controller allows pedestrian
        %% traffic for the last 2 minutes before reverting to normal state
        {Pid, pedestrian_stop} ->
            send_mesg(Pid, 'pedestrian about to close in 2'),
            timer:send_after(120000, self(), {Pid, normal}),
            controller(TrafficPid, PedestrianPid, {Tstate, Pstate, Status});

        %% This reverts the traffic light to normal state and blocks it for the
        %% next 10 minutes so that only vehicular traffic is allowed in the next
        %% 10 minutes
        {Pid, normal} ->
            send_mesg(Pid, 'normal, pedestrian not allowed for 10'),
            T = switch_state(Pid, Tstate, green),
            P = switch_state(Pid, Pstate, red),
            timer:send_after(600000, self(), {Pid, traffic_open}),
            controller(T, P, {green, red, traffic_lock});

        %% The pedestrian can now request for a change of state of the
        %% pedestrian light in order to cross
        {Pid, traffic_open} ->
            send_mesg(Pid, 'normal'),
            controller(TrafficPid, PedestrianPid, {Tstate, Pstate, open});

        %% A catchall condition for errors
        _ ->
            controller(TrafficPid, PedestrianPid, {Tstate, Pstate, Status})
    end.

%% we spawn two processes,
%% 1. TrafficPid
%% 2. PedestrianPid
%%
%% The we call the controller with the Pid of these processes which listens for
%% the various messages that will be sent to the process
start() ->
    TraffiPid = spawn(fun () -> signal_light(green) end),
    PedestrianPid = spawn(fun () -> signal_light(red) end),
    spawn(fun () -> controller(TraffiPid, PedestrianPid, {green, red, open}) end).
