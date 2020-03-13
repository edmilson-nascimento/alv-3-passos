# 3 passos para um ALV Report #

[![N|Solid](https://wiki.scn.sap.com/wiki/download/attachments/1710/ABAP%20Development.png?version=1&modificationDate=1446673897000&api=v2)](https://www.sap.com/brazil/developer.html)
Esta implementação tem objetivo de codigificacar em _três passos_ um relatorio utilizando ALV Report. Este tem como objetivo ser simples e direto.

## Necessidade ##
~~Escrever um código que será postado no linkedin ao invés de escrever aqui e postar o link~~ Codificar um relatório de maneira simples e direta focando 3 principais passos: busca, processamento e exibição.

## Tecnologia adotada ##
ABAP usando classe `cl_salv_table`. 

## Solução ##
O relatório usa dados fictícios que podem ser gerados através do TCODE `SEPM_DG`. Alguns dos trechos de códigos *não obedecem melhores práticas* pois a intenção é apenas usar 3 "responsabilidades". 

**Esse código é aberto, sujeito a alterações ~~a hora que me der na telha~~ assim que houver uma necessidade que trará ganho didático ao conteúdo e deixe o algoritmo com melhor leitura e compreens
