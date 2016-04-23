% Definições iniciais -----------------------------------------------------------------------------------------------------------------------------

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

:- dynamic utente/4.
:- dynamic servico/4.
:- dynamic consulta/4.

:- op(900, xfy, '::').

:- op(900, xfy, 'e'). % é para o demoExtendido funcionar

% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).


solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

comprimento( S,N ) :-
    length( S,N ).

% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao, falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).


% BASE DE CONHECIMENTO SOBRE SERVIÇOS ------------------------------------------------------------------------------------------
% servico: #Serv, Descrição, Instituição, Cidade -> {V, F, D}

-servico( S,D,I,C ) :-
    nao( servico( S,D,I,C ) ),
    nao( excecao( servico( S,D,I,C ) ) ).

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido

+servico( S,D,I,C ) :: (solucoes( (S,D,I,C),(servico( S,D,I,C )),S ),
                  comprimento( S,N ), N == 1
                  ).
% conhecimento perfeito

servico(1,oftalmologia,hospital_braga,braga).
servico(2,dermatologia,hospital_braga,braga).
servico(3,neurologia,hospital_braga,braga).
servico(4,cardiologia,hospital_sao_joao,porto).
servico(5,radiologia,hospital_sao_joao,porto).
servico(6,cirurgia_geral,hospital_sao_joao,porto).
servico(7,pediatria,hospital_santa_maria,lisboa).
servico(8,cardiologia,hospital_santa_maria,lisboa).
servico(9,psiquiatria,hospital_santa_maria,lisboa).

%Conhecimento imperfeito incerto

servico(10,cardiologia,xpto1,lisboa).
excecao(servico(S,D,I,C)) :- 
    servico(S,D,xpto1,C).
nulo(xpto1).

servico(11,xpto2,hospital_da_luz,lisboa).
excecao(servico(S,D,I,C)) :- 
    servico(S,xpto2,I,C).
nulo(xpto2).

servico(12,pediatria,hospital_da_luz,xpto3).
excecao(servico(S,D,I,C)) :- 
    servico(S,D,I,xpto3).
nulo(xpto3).


%Conhecimento imperfeito impreciso
excecao(servico(13,psiquiatria,hospital_santo_antonio,porto)).
excecao(servico(13,psiquiatria,hospital_sao_joao,porto)).


excecao(servico(14,cirurgia_geral,hospital_santo_antonio,porto)).
excecao(servico(14,cirurgia_geral,hospital_santo_antonio,lisboa)).


-servico(15,radiologia,hospital_viana_do_castelo,porto).
excecao(servico(15,radiologia,hospital_viana_do_castelo,viana_do_castelo)).
excecao(servico(15,radiologia,hospital_viana_do_castelo,braga)).



%Conhecimento imperfeito interdito

+servico( S,D,I,C ) :: (solucoes( (S,Ds,I,C),(servico(16,Ds,hospital_viana_do_castelo,viana_do_castelo),nao(nulo(Ds))),S ),
                  comprimento( S,N ), N == 0 
                  ).

+servico( S,D,I,C ) :: (solucoes( (S,D,Is,C),(servico(17,psiquiatria,Is,viana_do_castelo),nao(nulo(Is))),S ),
                  comprimento( S,N ), N == 0 
                  ).

+servico( S,D,I,C ) :: (solucoes( (S,D,I,Cs),(servico(18,oncologia,ipo_coimbra,Cs),nao(nulo(Cs))),S ),
                  comprimento( S,N ), N == 0 
                  ).



% BASE DE CONHECIMENTO SOBRE OS UTENTES -------------------------------------------------------------------------------------------
% utente: #IdUt, Nome, Idade, Morada -> {V, F, D}

-utente( U,N,I,M ) :-
    nao( utente( U,N,I,M ) ),
    nao( excecao( utente( U,N,I,M ) ) ).

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido

+utente( U,N,I,M ) :: (solucoes( (U,N,I,M),(utente( U,N,I,M )),S ),
                  comprimento( S,N ), N == 1
                  ).

% Conhecimento perfeito

utente(1,joao_castro, 23, lisboa).
utente(2,alberto_antonio, 99, porto).
utente(3,rafael_carvalho, 21, viana_do_castelo).
utente(4,natalio_cunha, 56, braga).
utente(5,gilberto_antunes, 44, braga ).
utente(6,goncalo_guedes, 18, porto).
utente(7,andre_almeida, 24, lisboa).
utente(8,bernardo_silva, 26, viana_do_castelo).
utente(9,maria_castro, 32, porto).
utente(10,nelson_semedo, 30, braga).
utente(17,luis_pereira, 72, lisboa).
utente(18,joana_costa, 37, porto).
utente(19,andre_vilaca, 25, braga).
utente(20,patricia_afonso, 23, viana_do_castelo).

% Conhecimento Imperfeito Incerto(Parametros desconhecidos)

utente(11,noemia_ferreira, 18, xptoc).
utente(12,carlota_santos, xptoi, viana_do_castelo).

excecao( utente( U,N,I,M ) ) :- utente(U,N,I,xptoc).
excecao( utente( U,N,I,M ) ) :- utente(U,N,xptoi,M).

nulo(xptoc).
nulo(xptoi).

% Conhecimento Imperfeito Impreciso

-utente(13,ana_rodrigues, 21, porto).
excecao( utente(13, ana_rodrigues, 22, porto)).
excecao( utente(13, ana_rodrigues, 23, porto)).

-utente(14,sofia_martins, 15, braga).
excecao( utente(14, sofia_martins, 15, vila_real)).
excecao( utente(14, sofia_martins, 15, aveiro)).

% Conhecimento Imperfeito Interdito

excecao( utente(15,miguel_belo,I, viana_do_castelo)).

+utente( 15,miguel_belo,I,viana_do_castelo ) :: solucoes( (utente(15,miguel_belo,I,viana_do_castelo)),(utente(15,miguel_belo,I,viana_do_castelo),S),
                  comprimento( S,N ), N == 0 
                  ).


excecao( utente(16,guilherme_cabral,69,M)).

+utente( 16,guilherme_cabral,69,M ) :: solucoes( (utente(16,guilherme_cabral,69,M)),(utente(16,guilherme_cabral,69,M),S),
                  comprimento( S,N ), N == 0 
                  )


% BASE DE CONHECIMENTO SOBRE AS CONSULTAS ------------------------------------------------------------------------------------------------------
% consulta: Data, #IdUt,#Serv,Custo -> {V,F,D}

-consulta( D,U,S,C ) :-
    nao( consulta( U,N,I,M ) ),
    nao( excecao( consulta( U,N,I,M ) ) ).

-data( D,M,A ) :- 
    nao( data(D,M,A) ),
    nao( excecao( data(D,M,A))).

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido

+consulta( D,U,S,C ) :: (solucoes( (D,U,S,C),(consulta( D,U,S,C )),S ),
                  comprimento( S,N ), N == 1
                  ).


% Conhecimento Perfeito

consulta(data(20,03,2016), 4, 1, 25).
consulta(data(16,05,2015), 7, 5, 30).


% Conhecimento Imperfeito Incerto(Parametros desconhecidos)

consulta(data(31,12,2015), 15, 2, xptop).
consulta(data(27,07,2015), 3, xptos, 30).
consulta(xptod, 16, 17, 20).

excecao( utente( D,U,S,xptop ) ) :- utente(D,U,S,xptop).
excecao( utente( D,U,xptos,30 ) ) :- utente(D,U,xptos,30).
excecao( utente( xptod,U,S,20 ) ) :- utente(xptod,U,S,20).

nulo(xptop).
nulo(xptos).
nulo(xptod).


% Conhecimento Imperfeito Impreciso

-consulta(data(02,09,2015), 20, 8, 20).
excecao(consulta(data(02,09,2015), 20, 8, 30)).
excecao(consulta(data(02,09,2015), 20, 8, 40)).

-consulta(data(18,10,2015), 17, 10, 40).
excecao(consulta(data(18,10,2015), 17, 6, 40)).
excecao(consulta(data(18,10,2015), 17, 9, 40)).

-consulta(data(23,11,2015), 8, 14, 35).
excecao(consulta(data(23,12,2015), 8, 14, 35)).
excecao(consulta(data(23,10,2015), 8, 14, 35)).

% Conhecimento Imperfeito Interdito

excecao(consulta(data(30,01,2016), 5, 15, C)).
+consulta( data(30,01,2016),5,15,C ) :: solucoes( (consulta(data(30,01,2016),5,15,C )),(consulta(data(30,01,2016),5,15,C ),S),
                  comprimento( S,N ), N == 0 
excecao(consulta(data(07,02,2016), 1, S, 70)).
+consulta( data(07,02,2016), 1, S, 70 ) :: solucoes( (consulta(data(07,02,2016), 1, S, 70)),(consulta(data(07,02,2016), 1, S, 70),S),
                  comprimento( S,N ), N == 0 

excecao(consulta(D, 13, 18, 50)).
+consulta( D, 13, 18, 50 ) :: solucoes( (consulta(D, 13, 18, 50)),(consulta(D, 13, 18, 50),S),
                  comprimento( S,N ), N == 0 