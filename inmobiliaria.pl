% consult('/home/log/inmobiliaria.pl').

inmobiliariaInicial(Inmobiliaria) :-
  Inmobiliaria = {
    [direcciones, D],
    [titulos, T],
    [precios, P],
    [estados, E],
    [gestores, G],
    [nombres, N],
    [telefonos, Tel],
    [emails, Em]} &
  D = {} &  
  T = {} &  
  P = {} &  
  E = {} &  
  G = {} &  
  N = {} &  
  Tel = {} &  
  Em = {}.  

inmobiliariaInvariante(Inmobiliaria) :-
  Inmobiliaria = {
    [direcciones, D],
    [titulos, T],
    [precios, P],
    [estados, E],
    [gestores, G],
    [nombres, N],
    [telefonos, Tel],
    [emails, Em]} &

  dom(D, DomD) &  
  dom(T, DomT) &  
  dom(P, DomP) &  
  dom(E, DomE) &  
  dom(G, DomG) &  
  dom(N, DomN) &  
  dom(Tel, DomTel) &  
  dom(Em, DomEm) &

  DomD = DomT &
  DomT = DomP &
  DomP = DomE &
  DomE = DomG &

  DomN = DomTel &
  DomTel = DomEm.


propiedadExiste(Inmobiliaria,Prop_id,Res_o,Inmobiliaria_) :-
  Inmobiliaria = {[direcciones,D] / _} &
  dom(D,DomD) & 
  Prop_id in DomD &
  Inmobiliaria_ = Inmobiliaria &
  Res_o = error.

vendedorNoExiste(Inmobiliaria,Prod_i,Res_o,Inmobiliaria_) :-
  Inmobiliaria = {[nombres,N] / _} & 
  dom(N, DomN) &
  Prod_i nin DomN &
  Inmobiliaria_ = Inmobiliaria &
  Res_o = error.

insertarPropiedadOk(Inmobiliaria,PropId_i,Dir_i,Tit_i,P_i,Prod_i,Res_o,Inmobiliaria_) :-
   Inmobiliaria = {
    [direcciones, D],
    [titulos, T],
    [precios, P],
    [estados, E],
    [gestores, G],
    [nombres, N],
    [telefonos, Tel],
    [emails, Em]} &
 
  dom(N, DomN) &
  dom(D, DomD) &
  Prod_i in DomN &
  PropId_i nin DomD &

  Inmobiliaria_ = {
    [direcciones, {[PropId_i, Dir_i] / D} ],
    [titulos, {[PropId_i, Tit_i] / T} ],
    [precios, {[PropId_i, P_i] / P} ],
    [estados, {[PropId_i, venta] / E} ],
    [gestores, {[PropId_i, Prod_i] / G} ],
    [nombres, N],
    [telefonos, Tel],
    [emails, Em]} &
  Res_o = ok.

insertarPropiedad(Inmobiliaria,Prop_id,Dir_i,Tit_i,P_i,Prod_i,Inmobiliaria_) :-
  insertarPropiedadOk(Inmobiliaria,Prop_id,Dir_i,Tit_i,P_i,Prod_i,ok,Inmobiliaria_)
  or
  vendedorNoExiste(Inmobiliaria,Prod_i,error,Inmobiliaria_)
  or
  propiedadExiste(Inmobiliaria,Prop_id,error,Inmobiliaria_).

vendedorExiste(Inmobiliaria,Prod_i,Res_o,Inmobiliaria_) :-
  Inmobiliaria = {[nombres,N] / _} & 
  dom(N, DomN) &
  Prod_i in DomN &
  Inmobiliaria_ = Inmobiliaria &
  Res_o = error.

insertarVendedorOk(Inmobiliaria,Nom_i,Tel_i,Em_i,Prod_i,Res_o,Inmobiliaria_) :-
   Inmobiliaria = {
    [direcciones, D],
    [titulos, T],
    [precios, P],
    [estados, E],
    [gestores, G],
    [nombres, N],
    [telefonos, Tel],
    [emails, Em]} &

  dom(N, DomN) &
  Prod_i nin DomN &

  Inmobiliaria_ = {
    [direcciones, D],
    [titulos, T],
    [precios, P],
    [estados, E],
    [gestores, G],
    [nombres, {[Prod_i, Nom_i] / N}],
    [telefonos, {[Prod_i, Tel_i] / Tel}],
    [emails, {[Prod_i, Em_i] / Em}]} &
  Res_o = ok.

insertarVendedor(Inmobiliaria,Nom_i,Tel_i,Em_i,Prod_i,Inmobiliaria_) :-
  insertarVendedorOk(Inmobiliaria,Nom_i,Tel_i,Em_i,Prod_i,ok,Inmobiliaria_)
  or
  vendedorExiste(Inmobiliaria,Prod_i,error,Inmobiliaria_).

propiedadesGestionadasOk(Inmobiliaria,Prod_i,Props_o, Res_o, Inmobiliaria_) :-
  Inmobiliaria = {[gestores,G], [nombres, N] / _} &
  dom(N, DomN) &
  Prod_i in DomN &

  rres(G,{Prod_i},M) &
  dom(M,Props_o) &

  Inmobiliaria = Inmobiliaria_ &
  Res_o = ok.

propiedadesGestionadas(Inmobiliaria,Prod_i,Props_o,Inmobiliaria_) :-
  propiedadesGestionadasOk(Inmobiliaria,Prod_i,Props_o,ok,Inmobiliaria_)
  or
  vendedorNoExiste(Inmobiliaria,Prod_i,error,Inmobiliaria_).

verificarPrecios(CotInf_i,CotSup_i,{},{}).
verificarPrecios(CotInf_i,CotSup_i,{[Prop_id, Price] / Rest},Props_o) :-
  ((Price >= CotInf_i &
  Price =< CotSup_i &
  Props_o = {Prop_id / Res}) or (Props_o = Res)) &
  verificarPrecios(CotInf_i,CotSup_i,Rest,Res).

filtroPrecioOk(Inmobiliaria,CotInf_i,CotSup_i,Props_o,Inmobiliaria_) :-
  Inmobiliaria = {[precios,P] / _} &
  CotInf_i =< CotSup_i &

  verificarPrecios(CotInf_i,CotSup_i,P,Props_o) &

  Inmobiliaria = Inmobiliaria_.

filtroPrecioError(Inmobiliaria,CotInf_i,CotSup_i,Res_o,Inmobiliaria_) :-
  CotInf_i >= CotSup_i &

  Inmobiliaria = Inmobiliaria_ &
  Res_o = error.

filtroPrecio(Inmobiliaria,CotInf_i,CotSup_i,Props_o,Inmobiliaria_) :-
  filtroPrecioOk(Inmobiliaria,CotInf_i,CotSup_i,Props_o,Inmobiliaria_)
  or
  filtroPrecioError(Inmobiliaria,CotInf_i,CotSup_i,error,Inmobiliaria_).

sumarPrecios(P,{},0).
sumarPrecios(P,{Prop_id / Rest},Suma_o) :-
  apply(P,Prop_id,Precio) &
  Suma_o = Precio + Res &
  sumarPrecios(P,Rest,Res).

calcularSumaVentasOk(Inmobiliaria,Prod_i,Ventas_o,Inmobiliaria_) :-
  Inmobiliaria = {[nombres,N], [gestores, G], [estados, S], [precios, P] / _} &
  dom(N, DomN) &
  Prod_i in DomN &

  rres(G,{Prod_i},M) &
  dom(M,Gestionadas) &

  rres(S,{vendida},K) &
  dom(K,Vendidas) &

  inters(Gestionadas,Vendidas,VendidasYgestionadas) &

  sumarPrecios(P,VendidasYgestionadas,Ventas_o).

calcularSumaVentas(Inmobiliaria,Prod_i,Ventas_o,Inmobiliaria_) :-
  calcularSumaVentasOk(Inmobiliaria,Prod_i,Ventas_o,Inmobiliaria_)
  or
  vendedorNoExiste(Inmobiliaria,Prod_i,error,Inmobiliaria_).
