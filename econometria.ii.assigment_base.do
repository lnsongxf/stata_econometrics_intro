// Econometria II - 2º ano
// Licenciatura em Economia - Universidade do Minho


clear all
set more off
set matsize 800

capture cd "D:\miguel\Dropbox\3.aulas\2018\licenciatura\econometria_ii"
capture cd "/Users/miguelportela/Dropbox/3.aulas/2018/licenciatura/econometria_ii/"

    cd "stata_econometrics_intro"

// construção do ficheiro '.log'

capture log close
log using econometria.ii.assigment.txt, text replace

// # 1. ler e fazer uma 1ª descrição dos dados

use WAGE1, clear        // ficheiro de dados 'original'

    // ==>> definição da amostra

            // de forma a garantir que a análise é feita sobre a mesma base de dados
            // vamos pagar as observações que valores omissos ("missings") nalguma
            // das variáveis relevante para a análise
            // colocar à frente do 'rowmiss' as variáveis que fazem parte do modelo
            // mais completo

            egen nmiss = rowmiss(lwage educ exper female nonwhite married numdep)
                tab nmiss
                keep if nmiss == 0
                    drop nmiss

    // # 1.0 1ª descrição dos dados

        describe                // este comando descreve os dados em memória
        codebook, compact        // permite perceber os valores únicos de cada variável, o que nos dá uma ideia se devemos, ou não,
                                // considerar uma dada variável como contínua ou discreta

        summarize                // produção de estatísticas descritivas para todas as variáveis
        tab educ, sort            // aplicar o 'tab' apenas a variáveis categóricas
        sum wage, detail        // aplicar 'sum' apenas a variáveis contínuas

        tabstat wage,by(smsa) stat(mean sd p50 p90 p95)        // tabulação de estatísticas descritivas

    // # 1.1 exportar a estatística descritiva de um sub-conjunto de variáveis para uma tabela

        // o comando 'outreg2' exporta a estatística descritiva para um ficheiro word
        // da 1ª vez que usam o comando devem instalá-lo: ssc install outreg
        // verificar na pasta de trabalho o ficheiro 'estatistica_descritiva.doc'

                preserve
                    keep wage educ exper female nonwhite married numdep
                    order wage educ exper female nonwhite married numdep
                    outreg2 using estatistica_descritiva.doc, sum(detail) word eqkeep(N mean p25 p50 p99 sd min max) replace
                restore

                // NOTA 1: não reportar o desvio-padrão para variáveis categóricas
                // NOTA 2: para a estatística descritiva as variáveis categóricas devem ser transformadas num conjunto de variáveis dummy

    // # 1.2 exportar gráficos: exemplos; adaptar a cada base de dados em função das variáveis disponíveis

        // ==>> não é necessário aplicar todos os pontos abaixo ao vosso trabalho
        // ==>> são exemplos do que podem fazer

        hist wage, bin(15)

        kdensity wage, normal
            graph export wage_density.png, replace

        histogram female, discrete width(0.5) start(0) frequency
            graph export female_histogram.png, replace

        histogram female, discrete width(0.5) start(0) frequency by(married)
            graph export female_histogram_married.png, replace

        egen mean_wage = mean(wage),by(educ)
        scatter mean_wage educ
            graph export wage_educ_scatter.png, replace

        twoway (scatter mean_wage educ) || (fpfit wage educ)            // versão simples do gráfico

        twoway (scatter mean_wage educ) || (fpfit wage educ), graphregion(color(white)) legend(label(1 "Mean wage") label(2 "Predicted wage") ///
            region(color(white))) scheme(sj) xtitle("Years of education") ytitle("Wage")    // o mesmo gráfico com a inclusão de uma série de opções

            graph export wage_educ_fit.png, replace

        // forma alternativa de preparar os dados para o gráfico

            preserve
                collapse (mean) mean_wage = lwage,by(educ)
                twoway (scatter mean_wage educ)
            restore

    // # 1.3 forma alternativa de fazer uma 1ª caracterização da relação entre salário e educação

        tab edu, missing
            gen high_educ = (educ >12)

        tab high_educ
        label define heduc 0 "Low educ" 1 "High educ", replace
        label values high_educ heduc
        label variable high_educ "Level of education; 1 high, 0 otherwise"
            des high_educ
            tab high_educ

        sum wage, detail
            return list
            scalar p99 = r(p99)

            correlate wage educ

        tabstat wage,by(high_educ) stat(N mean sd p10 p50 p90 p99)

        twoway (kdensity wage if high_educ == 0 & wage <= p99) || (kdensity wage if high_educ == 1 & wage <= p99), graphregion(color(white)) ///
            legend(label(1 "Low education") label(2 "High education") ///
            region(color(white))) scheme(sj) xtitle("Wage") ytitle("Density")
			
			graph export wage_density_educ.png, replace

// # 2. análise de regressão; exemplos de modelo

    generate exper2 = exper^2            // gerar uma variável 'exper2' que é o quadrado da experiência

    // # 2.1

        regress lwage educ exper exper2        // discutir a formulação do modelo e a interpretação dos parâmetros
            estimates store ols1            // guardar os resultados da regressão num "caixa" designada 'ols1'

                test exper2                    // teste de significância individual
                                            // teste de hipóteses: H0: beta3 = 0

                test exper exper2            // teste de significância conjunta à variável experiência,
                                            // que entra no modelo como 'exper' e 'exper2'

        // para que este teste de significância conjunta possa ser implementado manualmente é necessário ter informação
        // da estimação de um modelo sem exper e exper2

                regress lwage educ

                // formulação manual do teste

                    di ((0.3003 - 0.1858)/2)/((1 - 0.3003)/(526 - 3 - 1))

        regress lwage educ c.exper##c.exper    // o modelo acima poderia ter sido estimado sem criar a variável 'exper2'
                                            // utilizando os operadores do tipo '#' do Stata
                                            // esta formulação equivale a inserir na regressão 'exper' e 'exper2'

    // # 2.2

        regress lwage educ exper exper2 married nonwhite
            est store ols2

            test married nonwhite    // teste de significância conjunta
                                    // H0: beta4 = beta5 = 0

    // # 2.3 teste de Chow: avaliar se existe uma quebra de estrutura na formação de salários entre homens e mulheres

        // # 2.3.a versão "long"; ajuda a perceber o funcionamento do teste ==> não usar no trabalho de grupo

            gen female_educ = female*educ
            gen female_exper = female*exper
            gen female_exper2 = female*exper2
            gen female_married = female*married
            gen female_nonwhite = female*nonwhite

        regress lwage educ exper exper2 married nonwhite female female_educ female_exper female_exper2 female_married female_nonwhite

            test female female_educ female_exper female_exper2 female_married female_nonwhite

        // # 2.3.b versão alternativa; a que é habitualmente aplicada ==> esta é que a devem usar no trabalho

        regress lwage educ exper exper2 married nonwhite                    // restricted model: a variável 'female' não é incluída na regressão

        regress lwage educ exper exper2 married nonwhite if female == 0        // un-restricted 1; males
            est store ols3

        regress lwage educ exper exper2 married nonwhite if female == 1        // un-restricted 2; females
            est store ols4

            // test F statistic:             display ((102.3923 - (45.5929658 + 37.9369573))/(6))/((45.5929658 + 37.9369573)/(526 - 2*(5 + 1))) = 19.344887
            // F critic: display invFtail(6,526 - 2*(1 + 1),.05) == 2.1159353
            // comparar com a tabela F para um nível de significância de 5%

                // p-value: 2.057e-20 ~= 0

            display ((102.3923 - (45.5929658 + 37.9369573))/(6))/((45.5929658 + 37.9369573)/(526 - 2*(5 + 1)))


    // # 2.4 add other dummies

        // criação de uma variável categórica com 3 valores a partir de numdep

        tab numdep, missing            // a opção missing avalia se a variável tem valores omissos

        gen numdep_dummies = 0 if numdep == 0
            replace numdep_dummies = 1 if (numdep == 1 | numdep == 2)
            replace numdep_dummies = 2 if (numdep > 2)

            tab numdep_dummies

                gen dummy_1 = (numdep == 1 | numdep == 2)
                gen dummy_2 = (numdep > 2)                    // atenção: para o Stata o 'missing' (valor omisso) é + infinito

        regress lwage educ exper exper2 married nonwhite female i.numdep_dummies

            testparm i.numdep_dummies    // teste de significância conjunta para as duas dummies adicionais
                                        // H0: beta6 = beta7 = 0

                regress lwage educ exper exper2 married nonwhite female dummy_1 dummy_2

                    test dummy_1 dummy_2

            matrix list e(V)                // listar a matriz de variâcias e co-variâncias
                                            // é necessário para obter a covariância que é
                                            // utilizada no teste de igualdade de parâmetros

            test nonwhite = female    // teste de igualdade de parâmetros
                                    // H0: beta5 = beta6

            display (-.0084906-(-.32417))/((.0599608^2+.0371179^2 - 2*(.00008415))^.5) // cálculo da estatística do teste

                    // comparar com a aplicação automática do stata: test nonwhite = female

    // # 2.5 add interaction

        // gen female_educ = female*educ    ==>> a variável já existe

        regress lwage educ exper exper2 married nonwhite female dummy_1 dummy_2 female_educ
            est store ols5

                test female_educ        // avaliar se faz sentido incluir a variável 'interacção' no modelo
                                        // interacção = female_educ = female*educ

            // teste de significância global

                test educ exper exper2 married nonwhite female dummy_1 dummy_2 female_educ

                            // H0: beta1=beta2=beta3=beta4=(...)=beta9=0
                            // comparar com o output do canto superior direito do Stata

                            // a seguir à regressão

        regress lwage c.educ##i.female exper exper2 nonwhite i.numdep_dummies
            // o modelo acima poderia ter sido estimado sem criar a variável 'exper2'
            // utilizar operadores Stata

// # 3. HETEROSKEDASTICITY (para efeitos de ilustração vamos utilizar um modelo mais simples)

        regress lwage educ exper female

            predict e, resid    // previsão dos resíduos
            predict y_hat        // variável dependente prevista

            gen e2 = e^2        // gerar uma variável 'e2' que contém o quadrado dos resíduos

    // # 3.1 exploração gráfica

                scatter e educ
                scatter e exper

            // comando automático

        regress lwage educ exper female
                rvpplot educ
                rvpplot exper

            // solução apropriada

                // num modelo que satisfaça os pressupostos clássicos em relação à variância
                // não deve existir um padrão na relação entre os resíduos e a variável
                // dependente prevista, y_hat

                scatter e y_hat

                rvfplot // comando automático para avaliar se a variância dos resíduos é constante

                    twoway (scatter e y_hat) || (lfit e y_hat) || (fpfit e y_hat)

    // # 3.2 TESTE DE BREUSH-PAGAN (manual, step-by-step)

            reg e2 educ exper female

                eret li /* MOSTRAR OS RESULTADOS GUARDADOS NA MEMÓRIA DO STATA AP”S A REGRESS√O */
                    scalar N=e(N)
                    scalar R2=e(r2)
                    scalar nregressores=e(df_m)
                    di "Estatística do teste:    " N*R2
                    di "P-valor:    " chi2tail(nregressores,N*R2)
                    di invchi2(3,.95)

    // # 3.3 TESTE DE WHITE, solução 1, (manual, step-by-step)

            // quadrados

                // já temos exper2 e female = female^2
                gen educ2 = educ^2

                gen educ_exper=educ*exper
                gen educ_female=educ*female
                gen exper_female=exper*female

                reg e2 educ exper female educ2 exper2 educ_exper educ_female exper_female

                        scalar N=e(N)
                        scalar R2=e(r2)
                        scalar k=e(df_m)
                        di "Estatística do teste:    " N*R2
                        di "P-valor:    " chi2tail(k,N*R2)

    // # 3.4 TESTE DE WHITE, solução 2, (manual, step-by-step)

        // PROCEDIMENTO ALTERNATIVO: UTILIZAR O VALOR PREVISTO DA VARIÁVEL DEPENDENTE E O SEU QUADRADO COMO VARIÁVEIS EXPLICATIVAS DE e^2

                gen y_hat2=y_hat^2

                reg e2 y_hat y_hat2

    // # 3.5 procedimento automático do Stata

            regress lwage educ exper female

        // atenção que o Stata aplica os comandos à última regressão
            estat hettest
            estat imtest, white

    // # 3.6 estimação com erros-padrão robustos, solução automática do Stata

            regress lwage educ exper female, robust

                matrix list e(V)


// # 4. utilização do procedimento do Stata para corrigir para a heteroscedasticidade

    // aplicar apenas se existir heteroscedasticidade

        regress lwage educ exper exper2 married nonwhite female dummy_1 dummy_2 female_educ, robust
            est store ols6

            drop e y_hat e2 y_hat2

            predict y_hat

// # 5. exportar o resultado das extimações para uma tabela word

    // usar o comando 'outreg': solução mais eficiente e actual

        // da 1ª vez que usam o comando devem instalá-lo: ssc install outreg

        // verificar na pasta de trabalho o ficheiro 'regression_results.doc'

    estimates dir

        estimates table ols1 ols2 ols3 ols4 ols5 ols6, star


    outreg, clear

        estimates restore ols1        // add 'tex' to produce a tex file

            outreg using regression_results, replace ///
                    rtitles("Education" \ "" \ "Experience" \ "" \ "Exper^2" \ "" \ "Constant" \ "") ///
                    ctitle("","Simple model, M#1") ///
                    varlabels bdec(3) se starlevels(10 5 1) starloc(1) summstat(r2\rmse \ N) summtitle("R2"\"RMSE" \ "N")


        estimates restore ols2

            outreg using regression_results, merge ///
                    rtitles("Education" \ "" \ "Experience" \ "" \ "Exper^2" \ "" \ "Married" \ "" \ "Non white" \ "" \ "Constant" \ "") ///
                    ctitle("","M#2") ///
                    varlabels bdec(4) se starlevels(10 5 1) starloc(1) summstat(r2\rmse \ N) summtitle("R2"\"RMSE" \ "N")


        estimates restore ols3

            outreg using regression_results, merge ///
                    rtitles("Education" \ "" \ "Experience" \ "" \ "Exper^2" \ "" \ "Married" \ "" \ "Non white" \ "" \ "Constant" \ "") ///
                    ctitle("","Chow, M#3a") ///
                    varlabels bdec(4) se starlevels(10 5 1) starloc(1) summstat(r2\rmse \ N) summtitle("R2"\"RMSE" \ "N")


        estimates restore ols4

            outreg using regression_results, merge ///
                    rtitles("Education" \ "" \ "Experience" \ "" \ "Exper^2" \ "" \ "Married" \ "" \ "Non white" \ "" \ "Constant" \ "") ///
                    ctitle("","Chow, M#3b") ///
                    varlabels bdec(4) se starlevels(10 5 1) starloc(1) summstat(r2\rmse \ N) summtitle("R2"\"RMSE" \ "N")


        estimates restore ols5

            outreg using regression_results, merge ///
                    rtitles("Education" \ "" \ "Experience" \ "" \ "Exper^2" \ "" \ "Married" \ "" \ "Non white" \ "" \ "Female" \ "" \ "1 ou 2 depend." \ "" \ "+ de 2 depend." \ "" \ "Female x Educ" \ "" \ "Constant" \ "") ///
                    ctitle("","Interacção, M#4") ///
                    varlabels bdec(4) se starlevels(10 5 1) starloc(1) summstat(r2\rmse \ N) summtitle("R2"\"RMSE" \ "N")


        estimates restore ols6

            outreg using regression_results, merge ///
                    rtitles("Education" \ "" \ "Experience" \ "" \ "Exper^2" \ "" \ "Married" \ "" \ "Non white" \ "" \ "Female" \ "" \ "1 ou 2 depend." \ "" \ "+ de 2 depend." \ "" \ "Female x Educ" \ "" \ "Constant" \ "") ///
                    ctitle("","Robust, M#5") ///
                    varlabels bdec(4) se starlevels(10 5 1) starloc(1) summstat(r2\rmse \ N) summtitle("R2"\"RMSE" \ "N")


// ==>> NOTAS ADICIONAIS: testes de colinearidade e especificação

// # 6. COLINEARIDADE: discussão & simulação de 'colinearidade'

    set seed 234

        gen age = exper + educ + 6

        // 'age_b' = 'age' + resíduo

        gen age_b = exper + educ + 6 + uniform()        // 'age_b' é colinear com 'age', pois é
                                                        // igual a 'age' mais um resíduo definido pelo termo
                                                        // 'uniform()'

            correlate age age_b            // correlação entre variáveis ==>> se elevada temos um problema de colinearidade

    // # 6.1

        regress lwage educ exper

            vif                        // comando que nos permite avaliar se existe, ou não, um problema de colinearidade
                                    // o valor de referência é '10'; valores acima implicam que se faça um estudo
                                    // adicional para a presença de colinearidade no nosso modelo

    // # 6.2

        regress lwage educ exper age_b
            vif                                // os valores de 'VIF' são elevadíssimos, o que indica um claro problema de colinearidade

            correlate educ exper age_b        // a correlação entre as diferentes variáveis é uma forma simples de avaliar se potencialmente temos um problema de colinearidade

    // # 6.3

        regress lwage educ exper age        // nesta regressão temos um erro de especificação 'GRAVE'
                                            // a idade é igual a 'educ + exper + 6'
                                            // o Stata coloca 'omitted', pois há colinearidade perfeita
                                            // um erro do tipo 'omitted' é muito 'GRAVE'
            vif


// # 7. RESET test: teste de especificação

    capture drop y_hat            // eliminar a variável 'y_hat' que vem dos cálculos anteriores

    regress lwage educ exper exper2 married nonwhite female dummy_1 dummy_2 female_educ

        predict y_hat

            gen y_hat2 = y_hat^2
            gen y_hat3 = y_hat^3

    regress lwage educ exper exper2 married nonwhite female dummy_1 dummy_2 female_educ ///
            y_hat2 y_hat3

        test y_hat2 y_hat3        // implementação do teste RESET: estatística e p-valor do teste

    // comando automático

        // ssc install reset        // instalação do comando (não é um comando base do Stata)

            help reset

            reset lwage educ exper exper2 married nonwhite female dummy_1 dummy_2 female_educ

                // comparar o valor acima com a linha 'Ramsey RESETF2 Test'


// # 8. COMANDO Stata que aplica um conjunto de testes de especificação

    // ssc install regcheck        // instalação do comando

    regress lwage educ exper exper2 married nonwhite female dummy_1 dummy_2 female_educ

        regcheck        // 'vermelho' ==>> 'problema'

// # 9. 'outliers'

    regress lwage educ exper exper2 married nonwhite female dummy_1 dummy_2 female_educ

        gen id = _n

        lvr2plot, mlabel(id)

// # 10. qualidade do ajustamento

    estimates stats ols1 ols2 ols3 ols4 ols5 ols6

    estimates table ols1 ols2 ols3 ols4 ols5 ols6, b(%7.3f) ///
        keep(educ exper exper2 married nonwhite female female_educ) ///
        star(.1 .05 .01) stats(r2 r2_a rmse aic bic)

    estimates table ols1 ols2 ols3 ols4 ols5 ols6, b(%7.3f) se(%7.3f) ///
        keep(educ exper exper2 married nonwhite female female_educ) ///
        stats(r2 r2_a rmse aic bic)


log close

////////////////////////////////////////////////////////////////////////////////

// PROGRAMAÇÃO MANUAL DO 'OLS'

// SOURCE:    https://blog.stata.com/2016/01/05/programming-an-estimation-command-
// in-stata-computing-ols-objects-in-mata/

clear all

sysuse auto

    // # 1. Computing OLS point estimates in Mata

            mata

                y    = st_data(., "price")

                X    = st_data(., "mpg trunk")

                n    = rows(X)

                X    = X,J(n,1,1)

                XpX  = quadcross(X, X)

                XpXi = invsym(XpX)

                b    = XpXi*quadcross(X, y)


            end

    // # 2. Results from Mata and regress

            mata: b'

                regress price mpg trunk

    // # 3. Computing the IID VCE

            mata

                e    = y - X*b

                e2   = e:^2

                k    = cols(X)

                V    = (quadsum(e2)/(n-k))*XpXi

                sqrt(diagonal(V))'

            end

    // # 4. Robust standard errors

            mata
                M    = quadcross(X, e2, X)

                V    = (n/(n-k))*XpXi*M*XpXi

                sqrt(diagonal(V))'

            end

                regress price mpg trunk, robust

clear all
