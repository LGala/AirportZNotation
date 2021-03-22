%%%%%%%%%%%%%%%%%%%
% SOME TYPES
%%%%%%%%%%%%%%%%%%%%

planeSeatId_t(PLANESEATID) :-
    PLANESEATSNUMBER = 2 &

    un(int(1,PLANESEATSNUMBER),{-1},PLANESEATID).


destination_t(DESTINATION) :-
    DESTINATION = {milano,roma,null}.

% ------------------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%
% INITIALIZATION
%%%%%%%%%%%%%%%%%%%%


airportInit(Airport_) :-
    RegisteredPassengers_ = {} &

    planeSeatId_t(PLANESEATID) &
    destination_t(DESTINATION) &
    diff(PLANESEATID,{-1},AvalaiblePlaneSeatId) & 
    diff(DESTINATION,{null},AvalaibleDestination) &
    AvalaibleFlightTicket_ = {P:exists([X,Y],
        P=[X,Y] & X in AvalaiblePlaneSeatId & Y in AvalaibleDestination)} &

    size(AvalaibleFlightTicket_,AvalaibleFlightTicketSize) &
    MaxSuitcasesNumber_ is AvalaibleFlightTicketSize * 3 &
    SuitcasesDeposit_ = [[100,150],MaxSuitcasesNumber_,0,{}] &
    
    Airport_ = [RegisteredPassengers_,AvalaibleFlightTicket_,SuitcasesDeposit_] &


    nl & write(start-->airportInit) & nl & nl &
    write(registro) & nl & nl & write(RegisteredPassengers_) & nl & nl & nl &
    write(biglietti) & nl & nl & write(AvalaibleFlightTicket_) & nl & nl & nl &
    write(deposito) & nl & nl & write(SuitcasesDeposit_) & nl & nl & nl &
    write(finish-->airportInit) & nl & nl & nl & nl & nl.
    
    % nl & write(initialized) & nl & nl.



% ------------------------------------------------------------------------------------


%%%%%%%%%%%%%%%%%%%
% BUY FLIGHT TICKET
%%%%%%%%%%%%%%%%%%%%


buyAFlightTicketOk(Airport,PassengerId_i,PassengerDestination_i,PassengerSeatId_i,
                   OperationOutput_o,Airport_) :-

    Airport = [RegisteredPassengers,AvalaibleFlightTicket,SuitcasesDeposit] &

    AvalaibleFlightTicket neq {} &

    [PassengerSeatId_i,PassengerDestination_i] in AvalaibleFlightTicket &

    diff(AvalaibleFlightTicket,{[PassengerSeatId_i,PassengerDestination_i]},
         AvalaibleFlightTicket_) &

    dom(RegisteredPassengers,RegisteredPassengersDom) &

    ( 
        PassengerId_i nin RegisteredPassengersDom & 
        un(RegisteredPassengers,
            {[PassengerId_i,[[PassengerSeatId_i,PassengerDestination_i],{[1,[-1,-1]]}]]},
                RegisteredPassengers_) 
            
        or

        PassengerId_i in RegisteredPassengersDom & 
        oplus(RegisteredPassengers,
            {[PassengerId_i,[[PassengerSeatId_i,PassengerDestination_i],{[1,[-1,-1]]}]]},
                RegisteredPassengers_) 
    ) &

    SuitcasesDeposit_ = SuitcasesDeposit &

    Airport_ = [RegisteredPassengers_,AvalaibleFlightTicket_,SuitcasesDeposit_] &
    
    OperationOutput_o = buyTicketOk.



flightTicketSoldOut(Airport,PassengerDestination_i,PassengerSeatId_i,OperationOutput_o, 
                    Airport_) :-
    

    Airport = [_,AvalaibleFlightTicket,_] &

    AvalaibleFlightTicket = {} &
    
    Airport_ = Airport &

    OperationOutput_o = noMoreFlightTicket.




flightTicketNotAvalaible(Airport,PassengerDestination_i,PassengerSeatId_i,OperationOutput_o, 
                         Airport_) :-

    Airport = [_,AvalaibleFlightTicket,_] &

    [PassengerSeatId_i,PassengerDestination_i] nin AvalaibleFlightTicket &
    
    Airport_ = Airport & 

    OperationOutput_o = wantedTicketNotAvalaible.




% COMPLETE OPERATION
buyAFlightTicket(Airport,PassengerId_i,PassengerDestination_i,PassengerSeatId_i,
                 OperationOutput_o,Airport_) :-

    (
        buyAFlightTicketOk(Airport,PassengerId_i,PassengerDestination_i,PassengerSeatId_i,
                            OperationOutput_o,Airport_) 
            
        or

        flightTicketSoldOut(Airport,PassengerDestination_i,PassengerSeatId_i,OperationOutput_o, 
                                Airport_)

        or

        flightTicketNotAvalaible(Airport,PassengerDestination_i,PassengerSeatId_i,OperationOutput_o, 
                                    Airport_)
    )
        
    &
    
    Airport_ = [RegisteredPassengers_,AvalaibleFlightTicket_,SuitcasesDeposit_] &

    write(start-->buyAFlightTicket) & nl & nl &
    write(registro) & nl & nl & write(RegisteredPassengers_) & nl & nl & nl &
    write(biglietti) & nl & nl & write(AvalaibleFlightTicket_) & nl & nl & nl &
    write(deposito) & nl & nl & write(SuitcasesDeposit_) & nl & nl & nl &
    write(finish-->buyAFlightTicket) & nl & nl & nl & nl & nl.

    % INFORMAZIONI PER IL LOG
    % write(PassengerId_i) & write(-->) & write(OperationOutput_o) & nl & nl.




% ------------------------------------------------------------------------------------



%%%%%%%%%%%%%%%%%%%
% DO CHECKIN 
%%%%%%%%%%%%%%%%%%%%


giveSuitcasesAtCheckinOK(Airport,PassengerId_i,PassengerSuitcases_i,OperationOutput_o, 
                         Airport_) :-
    
    Airport = [RegisteredPassengers,AvalaibleFlightTicket,SuitcasesDeposit] &
    SuitcasesDeposit = [MaxSuitcaseSize,MaxSuitcasesNumber,CurrentSuitcasesNumber,
                        DepositSuitcasesRegister] &

    dom(RegisteredPassengers,RegisteredPassengersDom) &
    PassengerId_i in RegisteredPassengersDom &

    dom(DepositSuitcasesRegister,DepositSuitcasesRegisterDom) &
    PassengerId_i nin DepositSuitcasesRegisterDom &
    
    [FirstMaxSize,SecondMaxSize] = MaxSuitcaseSize &
    foreach([_,[FirstSize,SecondSize]] in PassengerSuitcases_i,
            FirstSize =< FirstMaxSize & SecondSize =< SecondMaxSize) & 
    
    size(PassengerSuitcases_i,PassengerSuitcases_i_Size) &
    PassengerSuitcases_i_Size + CurrentSuitcasesNumber =< MaxSuitcasesNumber &


    apply(RegisteredPassengers,PassengerId_i,[FlightTicket,_]) &
    oplus(RegisteredPassengers,{[PassengerId_i,[FlightTicket,PassengerSuitcases_i]]},
        RegisteredPassengers_) &

    (
        (
            PassengerSuitcases_i neq {} &
            un(DepositSuitcasesRegister,{[PassengerId_i,PassengerSuitcases_i]},
                    DepositSuitcasesRegister_)
        )

        or

        (
            PassengerSuitcases_i = {} &
            DepositSuitcasesRegister_ = DepositSuitcasesRegister
        )
    )

    &

    CurrentSuitcasesNumber_ is CurrentSuitcasesNumber + PassengerSuitcases_i_Size &

    AvalaibleFlightTicket_ = AvalaibleFlightTicket &

    SuitcasesDeposit_ = [MaxSuitcaseSize, MaxSuitcasesNumber, CurrentSuitcasesNumber_, 
                         DepositSuitcasesRegister_] &

    Airport_ = [RegisteredPassengers_,AvalaibleFlightTicket_,SuitcasesDeposit_] &

    OperationOutput_o = giveSuitcasesOk.



passengerDidNotBuyTicket(Airport,PassengerId_i,OperationOutput_o,Airport_) :-

    Airport = [RegisteredPassengers,_,_] &

    dom(RegisteredPassengers,RegisteredPassengersDom) &
    PassengerId_i nin RegisteredPassengersDom &

    Airport_ = Airport & 

    OperationOutput_o = mustBuyTicket.




checkinAlreadyDone(Airport,PassengerId_i,OperationOutput_o,Airport_) :-

    Airport = [_,_,SuitcasesDeposit] &
    SuitcasesDeposit = [_,_,_,DepositSuitcasesRegister] &

    dom(DepositSuitcasesRegister,DepositSuitcasesRegisterDom) &
    PassengerId_i in DepositSuitcasesRegisterDom &

    Airport_ = Airport & 

    OperationOutput_o = tooManyCheckin.




suitcasesAreTooBig(Airport,PassengerSuitcases_i,OperationOutput_o,Airport_) :-

    Airport = [_,_,SuitcasesDeposit] &
    SuitcasesDeposit = [MaxSuitcaseSize,_,_,_] &

    [FirstMaxSize,SecondMaxSize] = MaxSuitcaseSize &
    nforeach([_,[FirstSize,SecondSize]] in PassengerSuitcases_i,
            FirstSize =< FirstMaxSize & SecondSize =< SecondMaxSize) &
    
    Airport_ = Airport & 

    OperationOutput_o = tooBig.




tooManySuitcases(Airport,PassengerSuitcases_i,OperationOutput_o,Airport_) :-

    Airport = [_,_,SuitcasesDeposit] &
    SuitcasesDeposit = [_,MaxSuitcasesNumber,CurrentSuitcasesNumber,_] &

    size(PassengerSuitcases_i,PassengerSuitcases_i_Size) &
    PassengerSuitcases_i_Size + CurrentSuitcasesNumber > MaxSuitcasesNumber &

    Airport_ = Airport & 

    OperationOutput_o = noMoreSpace.  




% COMPLETE OPERATION
doCheckin(Airport,PassengerId_i,PassengerSuitcases_i,OperationOutput_o,Airport_) :-

    (
        % questo assioma (e vale anche per altri) bisognava tradurlo in modo letterale come 
        % al punto 2.11 del file: 
        % https://elly2020.smfi.unipr.it/pluginfile.php/9765/mod_resource/content/8/setlog.pdf
        % ma non ne valutavo il bisogno
        SUITCASESMALLERSIZE = [50,80] &
        [FirstMinSize,SecondMinSize] = SUITCASESMALLERSIZE &
        foreach([_,[FirstSize,SecondSize]] in PassengerSuitcases_i,
                FirstSize >= FirstMinSize & SecondSize >= SecondMinSize) 
        
        &

        (
            giveSuitcasesAtCheckinOK(Airport,PassengerId_i, PassengerSuitcases_i,OperationOutput_o, 
                                     Airport_)

            or

            passengerDidNotBuyTicket(Airport,PassengerId_i,OperationOutput_o,Airport_)

            or

            checkinAlreadyDone(Airport,PassengerId_i,OperationOutput_o,Airport_)

            or

            suitcasesAreTooBig(Airport,PassengerSuitcases_i,OperationOutput_o,Airport_)

            or

            tooManySuitcases(Airport,PassengerSuitcases_i,OperationOutput_o,Airport_)
        )
        
        % &

        % INFORMAZIONI PER IL LOG
        % write(PassengerId_i) & write(-->) & write(OperationOutput_o) & nl & nl

        &

        Airport_ = [RegisteredPassengers_,AvalaibleFlightTicket_,SuitcasesDeposit_] &

        write(start-->doCheckin) & nl & nl &
        write(registro) & nl & nl & write(RegisteredPassengers_) & nl & nl & nl &
        write(biglietti) & nl & nl & write(AvalaibleFlightTicket_) & nl & nl & nl &
        write(deposito) & nl & nl & write(SuitcasesDeposit_) & nl & nl & nl &
        write(finish-->doCheckin) & nl & nl & nl & nl & nl
    )
        
    or 
    
    (
        Airport_ = Airport &
        % write(PassengerId_i) & write(-->) & write(tooLittleSuitcases) & nl & nl &

        Airport_ = [RegisteredPassengers_,AvalaibleFlightTicket_,SuitcasesDeposit_] &

        write(start-->doCheckin) & nl & nl &
        write(registro) & nl & nl & write(RegisteredPassengers_) & nl & nl & nl &
        write(biglietti) & nl & nl & write(AvalaibleFlightTicket_) & nl & nl & nl &
        write(deposito) & nl & nl & write(SuitcasesDeposit_) & nl & nl & nl &
        write(finish-->doCheckin) & nl & nl & nl & nl & nl
    ).


% ------------------------------------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%
% POLICE WANT PASSENGER 
%%%%%%%%%%%%%%%%%%%%%%%




% per rimanere fedeli alla notazione Z: 
% se A chiama lo schema B come predicato allora A dichiara nei suoi predicati 
% i predicati di B
airportPoliceLooksForPassengers(Airport,WantedPassengerDescription_i,WantedPassengers_o, 
                                OperationOutput_o,Airport_) :-
        
    Airport = [RegisteredPassengers,_,_] &

    write(start-->airportPoliceLooksForPassengers) & nl & nl &

    % airportPoliceGaveTooLittleInfo
    WantedPassengerDescription_i = [[PlaneSeatId,Destination],DepositSuitcasesRegister] &

    (
        (
            PlaneSeatId neq -1 &
            Destination neq null &
            DepositSuitcasesRegister neq {} &
            DepositSuitcasesRegister neq {[1,[-1,-1]]} 
        )

        or

        write(airportPoliceGaveTooLittleInfo-->do_not_consider_further_output) & nl & nl
    )
    % -------- 
    
    &

    % airportPoliceFoundNoOne
    (
        ran(RegisteredPassengers,RegisteredPassengersRan) &
        (
            WantedPassengerDescription_i in RegisteredPassengersRan
            or
            write(airportPoliceFoundNoOne-->do_not_consider_further_output) & nl & nl
        )
    ) 
    % ---------
    
    &

    (
        rres(RegisteredPassengers,{WantedPassengerDescription_i},WantedPassenger) &
        dom(WantedPassenger,WantedPassengers_o) &
        OperationOutput_o = describedPassengersStopped &
        
        % % INFORMAZIONI PER IL LOG
        write(wantedPassenger) & write(-->) & write(WantedPassengers_o) & nl & nl
    )

    &

    % % INFORMAZIONI PER IL LOG
    % write(finish_executing-->airportPoliceLooksForPassengers) & nl & nl &

    Airport_ = Airport &

    Airport_ = [RegisteredPassengers_,AvalaibleFlightTicket_,SuitcasesDeposit_] &

    write(registro) & nl & nl & write(RegisteredPassengers_) & nl & nl & nl &
    write(biglietti) & nl & nl & write(AvalaibleFlightTicket_) & nl & nl & nl &
    write(deposito) & nl & nl & write(SuitcasesDeposit_) & nl & nl & nl &
    write(finish-->airportPoliceLooksForPassengers) & nl & nl & nl & nl & nl.






% ------------------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%
% CANCEL FLIGHT 
%%%%%%%%%%%%%%%%%%%%%%%

passengerCanceledHisFlightOK(Airport,PassengerId_i,FlightDepartureTime_i,CurrentTime_i,
    OperationOutput_o,Airport_) :-
    
    Airport = [RegisteredPassengers,AvalaibleFlightTicket,SuitcasesDeposit] &
    SuitcasesDeposit = [MaxSuitcaseSize,MaxSuitcasesNumber,CurrentSuitcasesNumber,
                        DepositSuitcasesRegister] &

    % INFORMAZIONI PER IL LOG
    % write(before_operation) & nl & nl &
    % write(registeredPassengers) & write(-->) & write(RegisteredPassengers) & nl & nl &
    % write(deposit) & write(-->) & write(SuitcasesDeposit) & nl & nl &
    % write(avalaibleFlightTicket) & write(-->) & write(AvalaibleFlightTicket) & nl & nl &


    dom(RegisteredPassengers,RegisteredPassengersDom) &
    PassengerId_i in RegisteredPassengersDom &
        
    apply(RegisteredPassengers,PassengerId_i,[FlightTicket,RegisteredPassengerSuitcases]) &

    % TicketIsAvalaibleAgain - thereIsEnoughTimeBeforeTheFlight
    (
        (
            FlightDepartureTime_i - CurrentTime_i >= 1 &
            un(AvalaibleFlightTicket,{FlightTicket},AvalaibleFlightTicket_)
        )

        or
        
        (
            FlightDepartureTime_i - CurrentTime_i < 1 &
            AvalaibleFlightTicket_ = AvalaibleFlightTicket
        )
    )
    % ------

    &

    % DeletePassengerFlightTicket
    (
        oplus(RegisteredPassengers,
            {[PassengerId_i,[[-1,null],RegisteredPassengerSuitcases]]},
                RegisteredPassengers_)
    )
    % ------

    &

    % PassengerHasSuitcasesInDeposit - GiveBackSuictases
    (          
        (   
            dom(DepositSuitcasesRegister,DepositSuitcasesRegisterDom) &
            PassengerId_i in DepositSuitcasesRegisterDom &

            apply(DepositSuitcasesRegister,PassengerId_i,DepositedSuitcases) &            

            diff(DepositSuitcasesRegister,
                {[PassengerId_i,DepositedSuitcases]},
                    DepositSuitcasesRegister_)
        )
 
        or
 
        (
            dom(DepositSuitcasesRegister,DepositSuitcasesRegisterDom) &
            PassengerId_i nin DepositSuitcasesRegisterDom &
            DepositSuitcasesRegister_ = DepositSuitcasesRegister
        )
    )
    % % ------

    &

    SuitcasesDeposit_ = [MaxSuitcaseSize,MaxSuitcasesNumber,CurrentSuitcasesNumber,
                        DepositSuitcasesRegister_] &

    % INFORMAZIONI PER IL LOG
    % write(later_operation) & nl & nl &
    % write(registeredPassengers) & write(-->) & write(RegisteredPassengers_) & nl & nl &
    % write(deposit) & write(-->) & write(SuitcasesDeposit_) & nl & nl &
    % write(avalaibleFlightTicket) & write(-->) & write(AvalaibleFlightTicket_) & nl & nl &

    Airport_ = [RegisteredPassengers_,AvalaibleFlightTicket_,SuitcasesDeposit_] &

    OperationOutput_o = flightCanceled.




% COMPLETE OPERATION
passengerCanceledHisFlight(Airport,PassengerId_i,FlightDepartureTime_i,CurrentTime_i,
    OperationOutput_o,Airport_) :-

    (
        passengerCanceledHisFlightOK(Airport,PassengerId_i,FlightDepartureTime_i,CurrentTime_i,
                                     OperationOutput_o,Airport_)

        or
        
        passengerDidNotBuyTicket(Airport,PassengerId_i,OperationOutput_o,Airport_)
    )

    &

    % INFORMAZIONI PER IL LOG
    % write(PassengerId_i) & write(-->) & write(OperationOutput_o) & nl & nl.

    Airport_ = [RegisteredPassengers_,AvalaibleFlightTicket_,SuitcasesDeposit_] &

    write(start-->passengerCanceledHisFlight) & nl & nl &
    write(registro) & nl & nl & write(RegisteredPassengers_) & nl & nl & nl &
    write(biglietti) & nl & nl & write(AvalaibleFlightTicket_) & nl & nl & nl &
    write(deposito) & nl & nl & write(SuitcasesDeposit_) & nl & nl & nl &
    write(finish-->passengerCanceledHisFlight) & nl & nl & nl & nl & nl.


% ------------------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%
% INVARIANT
%%%%%%%%%%%%%%%%%%%%%%%


avalaibleFlightTicketsInv(Airport) :-
    
    Airport = [_,AvalaibleFlightTicket,_] &

    foreach([PlaneSeatId,Destination] in AvalaibleFlightTicket,
        PlaneSeatId neq -1 & Destination neq null
    ).


suitcasesInv(Airport) :-
    
    Airport = [RegisteredPassengers,_,SuitcasesDeposit] &
    SuitcasesDeposit = [_,_,_,DepositSuitcasesRegister] &
    
    GoodRegisteredPassengerSuitcasesDom = ris([Id,[FlightTicket,Suitcases]] 
        in RegisteredPassengers,
        Suitcases neq {} & Suitcases neq {[1,[-1,-1]]} & FlightTicket neq [-1,null],
        Id
    )

    &

    dom(DepositSuitcasesRegister,GoodRegisteredPassengerSuitcasesDom).



airportInv(Airport) :-
    (avalaibleFlightTicketsInv(Airport) & suitcasesInv(Airport))

    or

    (write(airportInv-->false) & nl & nl & false).


% ------------------------------------------------------------------------------------



main(_) :-
    airportInit(A) & airportInv(A)
    &
    buyAFlightTicket(A,lorenzo,roma,1,OutAB,B) & airportInv(B)
    &
    buyAFlightTicket(B,luca,milano,1,OutBC,C) & airportInv(C)
    &
    doCheckin(C,luca,{[1,[49,100]],[2,[57,91]]},OutCD,D) & airportInv(D)
    &
    doCheckin(D,lorenzo,{[1,[70,100]],[2,[57,91]]},OutDE,E) & airportInv(E)
    &
    airportPoliceLooksForPassengers(E,[[1,chicago],{[1,[55,90]]}],
                                    WantedPassengers_o,OutEF,F) & airportInv(F)
    &
    passengerCanceledHisFlight(F,lorenzo,23,21,OutFG,G) & airportInv(G).
    
    % airportInit(A) & airportInv(A)
    % &
    % buyAFlightTicket(A,A1,A2,A3,OutAB,B) & airportInv(B)
    % &
    % buyAFlightTicket(B,B1,B2,B3,OutBC,C) & airportInv(C)
    % &
    % doCheckin(C,C1,C2,OutCD,D) & airportInv(D)
    % &
    % doCheckin(D,D1,D2,OutDE,E) & airportInv(E)
    % &
    % airportPoliceLooksForPassengers(E,E1,WantedPassengers_o,OutEF,F) & airportInv(F)
    % &
    % passengerCanceledHisFlight(F,F1,F2,F3,OutFG,G) & airportInv(G).
