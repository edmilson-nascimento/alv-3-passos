# Três passos para um ALV Report #

![Static Badge](https://img.shields.io/badge/development-abap-blue)
![GitHub commit activity (branch)](https://img.shields.io/github/commit-activity/t/edmilson-nascimento/alv-3-passos)
![Static Badge](https://img.shields.io/badge/learning-abap-green)


Esta implementação tem objetivo de codificar em _três passos_ um relatório utilizando ALV Report. Este tem como objetivo ser simples e direto.

## Necessidade ##
~~Escrever um código que será postado no linkedin ao invés de escrever aqui e postar o link~~ Codificar um relatório de maneira simples e direta focando 3 principais passos: busca, processamento e exibição.

[Link para post](https://www.linkedin.com/pulse/sobre-estrutura-de-relat%C3%B3rio-alv-edmilson-nascimento-de-jesus/)

## Tecnologia adotada ##
ABAP usando classe `cl_salv_table` para exibição de dados. 


## Solução ##
O relatório usa dados fictícios que podem ser gerados através do TCODE `SEPM_DG`. Alguns dos trechos de códigos *não obedecem melhores práticas* pois a intenção é **usar apenas três "responsabilidades"**. 

## Fluxo do projeto ##
~~Para usar um grafico Mermaid~~ Para melhor entendimento do processo, segue abaixo fluxo .
```mermaid
flowchart TD
    Start((start)) --> SAPData[(Buscar dados)]
    SAPData --> SAPProcess(Processar dados)
    SAPProcess --> SAPInfo(Exibir Informações)
    SAPInfo --> FinishNew((( )))
```

**Esse código é aberto, sujeito a alterações ~~a hora que me der na telha~~ assim que houver uma necessidade que trará ganho didático ao conteúdo e/ deixe o algoritmo com melhor leitura e compreensão.**