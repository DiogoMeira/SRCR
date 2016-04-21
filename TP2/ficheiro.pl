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

servico(1,oftalmologia,hospital_braga,braga).
servico(2,dermatologia,hospital_braga,braga).
servico(3,neurologia,hospital_braga,braga).
servico(4,cardiologia,hospital_sao_joao,porto).
servico(5,radiologia,hospital_sao_joao,porto).
servico(6,cirurgia_geral,hospital_sao_joao,porto).
servico(7,pediatria,hospital_santa_maria,lisboa).
servico(8,cardiologia,hospital_santa_maria,lisboa).
servico(9,psiquiatria,hospital_santa_maria,lisboa).
servico(10,cardiologia,hospital_da_luz,lisboa).
servico(11,oncologia,hospital_da_luz,lisboa).
servico(12,pediatria,hospital_da_luz,lisboa).
servico(13,psiquiatria,hospital_santo_antonio,porto).
servico(14,cirurgia_geral,hospital_santo_antonio,porto).
servico(15,radiologia,hospital_viana_do_castelo,porto).
servico(16,dermatologia,hospital_viana_do_castelo,viana_do_castelo).
servico(17,psiquiatria,centro_saude_perre,viana_do_castelo).
servico(18,oncologia,ipo_coimbra,coimbra).

% BASE DE CONHECIMENTO SOBRE OS UTENTES -------------------------------------------------------------------------------------------
% utente: #IdUt, Nome, Idade, Morada -> {V, F, D}

-utente( U,N,I,M ) :-
    nao( utente( U,N,I,M ) ),
    nao( excecao( utente( U,N,I,M ) ) ).

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
utente(11,noemia_ferreira, 18, lisboa).
utente(12,carlota_santos, 20, viana_do_castelo).
utente(13,ana_rodrigues, 21, porto).
utente(14,sofia_martins, 15, braga).
utente(15,miguel_belo, 2, viana_do_castelo).
utente(16,guilherme_cabral, 69, viana_do_castelo).
utente(17,luis_pereira, 72, lisboa).
utente(18,joana_costa, 37, porto).
utente(19,andre_vilaca, 25, braga).
utente(20,patricia_afonso, 23, viana_do_castelo).

% BASE DE CONHECIMENTO SOBRE AS CONSULTAS
% consulta: Data, #IdUt,#Serv,Custo -> {V,F,D}

consulta(data(20,03,2016), 4, 1, 25).
consulta(data(16,05,2015), 7, 5, 30).
consulta(data(15,08,2015), 13, 18, 50).
consulta(data(30,01,2016), 5, 15, 30).
consulta(data(07,02,2016), 1, 12, 70).
consulta(data(02,09,2015), 20, 8, 20).
consulta(data(18,10,2015), 17, 10, 40).
consulta(data(23,11,2015), 8, 14, 35).
consulta(data(31,12,2015), 15, 2, 65).
consulta(data(27,07,2015), 3, 3, 30).
consulta(data(05,06,2015), 16, 17, 20).

