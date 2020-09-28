% consult('/home/fran/Repos/schedule/agenda.pl').

agendaDeTurnosInit(AgendaDeTurnos) :-
    AgendaDeTurnos = {[clientes,C],[turnos,T]} &
    C = {} &
    T = {}.

agendaDeTurnosInv(AgendaDeTurnos) :-
    AgendaDeTurnos = {[clientes,C],[turnos,T]} & dom(C,T).

asignarTurno(AgendaDeTurnos,Name_i,Dni_i,Fecha_i,Ahora_i,AgendaDeTurnos_) :-
    asignarTurnoOk(AgendaDeTurnos,Name_i,Dni_i,Fecha_i,Ahora_i,AgendaDeTurnos_)
    or
    asignarTurnoError(AgendaDeTurnos,Fecha_i,Ahora_i,AgendaDeTurnos_).

asignarTurnoOk(AgendaDeTurnos,Name_i,Dni_i,Fecha_i,Ahora_i,AgendaDeTurnos_) :-
    asignarTurnoPrimeraVez(AgendaDeTurnos,Name_i,Dni_i,Fecha_i,Ahora_i,AgendaDeTurnos_)
    or
    asignarTurnoCliente(AgendaDeTurnos,Dni_i,Fecha_i,Ahora_i,AgendaDeTurnos_).

asignarTurnoPrimeraVez(AgendaDeTurnos,Name_i,Dni_i,Fecha_i,Ahora_i,AgendaDeTurnos_) :-
    AgendaDeTurnos = {[clientes,C],[turnos,T]} &
    dom(C,Dc) &
    Dni_i nin Dc &
    dom(T,Dt) &
    Fecha_i nin Dt &
    Fecha_i > Ahora_i &
    un(C,{[Dni_i,Name_i]},C_) &
    un(T,{[Fecha_i,Dni_i]},T_) &
    AgendaDeTurnos_ = {[clientes,C_],[turnos,T_]}.

asignarTurnoCliente(AgendaDeTurnos,Dni_i,Fecha_i,Ahora_i,AgendaDeTurnos_) :-
    AgendaDeTurnos = {[clientes,C],[turnos,T]} &
    dom(C,Dc) &
    Dni_i in Dc &
    dom(T,Dt) &
    Fecha_i nin Dt &
    Fecha_i > Ahora_i &
    un(T,{[Fecha_i,Dni_i]},T_) &
    AgendaDeTurnos_ = {[clientes,C],[turnos,T_]}.

asignarTurnoError(AgendaDeTurnos,Fecha_i,Ahora_i,AgendaDeTurnos_) :-
    errorFechaPasada(AgendaDeTurnos,Fecha_i,Ahora_i,AgendaDeTurnos_)
    or
    turnoYaAsignado(AgendaDeTurnos,Fecha_i,AgendaDeTurnos_).

errorFechaPasada(AgendaDeTurnos,Fecha_i,Ahora_i,AgendaDeTurnos_) :-
    AgendaDeTurnos = {_} &
    Fecha_i =< Ahora_i &
    AgendaDeTurnos_ = AgendaDeTurnos.

turnoYaAsignado(AgendaDeTurnos,Fecha_i,AgendaDeTurnos_) :-
    AgendaDeTurnos = {[turnos,T] / _} &
    dom(T,Dt) &
    Fecha_i in Dt &
    AgendaDeTurnos_ = AgendaDeTurnos.

busquedaPorDniOk(AgendaDeTurnos,Dni_i,Resp_o,AgendaDeTurnos_) :-
    AgendaDeTurnos = {[clientes,C],[turnos,T]} &
    dom(C,Dc) &
    Dni_i in Dc &
    rres(T,{Dni_i},M) &
    dom(M,Resp_o) &
    AgendaDeTurnos = AgendaDeTurnos_.

clienteNoExisteDNI(AgendaDeTurnos,Dni_i,AgendaDeTurnos_) :-
    AgendaDeTurnos = {[clientes,C] / _} &
    dom(C,Dc) &
    Dni_i nin Dc &
    AgendaDeTurnos_ = AgendaDeTurnos.

busquedaPorDni(AgendaDeTurnos,Dni_i,Resp_o,AgendaDeTurnos_) :-
    busquedaPorDniOk(AgendaDeTurnos,Dni_i,Resp_o,AgendaDeTurnos_)
    or
    clienteNoExisteDNI(AgendaDeTurnos,Dni_i,AgendaDeTurnos_).

busquedaPorNombreOk(AgendaDeTurnos,Nombre_i,Resp_o,AgendaDeTurnos_) :-
    AgendaDeTurnos = {[clientes,C],[turnos,T]} &
    ran(C,Ic) &
    Nombre_i in Ic &
    rres(C,{Nombre_i},D) &
    dom(D,Dd) &
    rres(T,Dd,Resp_o) &
    AgendaDeTurnos_ = AgendaDeTurnos.

clienteNoExisteNombre(AgendaDeTurnos,Nombre_i,AgendaDeTurnos_) :-
    AgendaDeTurnos = {[clientes,C] / _} &
    ran(C,Ic) &
    Nombre_i nin Ic &
    AgendaDeTurnos_ = AgendaDeTurnos.

busquedaPorNombre(AgendaDeTurnos,Nombre_i,Resp_o,AgendaDeTurnos_) :-
    busquedaPorNombreOk(AgendaDeTurnos,Nombre_i,Resp_o,AgendaDeTurnos_)
    or
    clienteNoExisteNombre(AgendaDeTurnos,Nombre_i,AgendaDeTurnos_).

busquedaPorFecha(AgendaDeTurnos,Fecha_i,Resp_o,AgendaDeTurnos_) :-
    AgendaDeTurnos = {[turnos,T] / _} &
    dres(Fecha_i,T,Resp_o) &
    AgendaDeTurnos_ = AgendaDeTurnos.
