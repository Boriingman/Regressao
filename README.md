# Aplicativo Shiny: Análise de Regressão Múltipla

Este é um aplicativo Shiny interativo para realizar análises de regressão múltipla. O aplicativo permite que os usuários carreguem seus próprios dados, selecionem variáveis resposta e preditoras, e visualizem os resultados.

## Funcionalidades

1. **Carregar Dados e Selecionar Variáveis**:
   - Faça o upload de um arquivo `.csv`.
   - Selecione a variável resposta e as variáveis preditoras.

2. **Análise Exploratória**:
   - Visualize um resumo estatístico (`summary`) dos dados.
   - Confira a estrutura (`str`) dos dados.

3. **Resultados da Regressão**:
   - Exibição detalhada dos resultados do modelo gerado pela função `lm()`.

4. **Análise de Resíduos**:
   - Visualize um gráfico de resíduos (valores ajustados vs. resíduos).
   - Analise a normalidade dos resíduos usando um gráfico Q-Q.
   - Faça o download dos gráficos gerados.

## Dependências

O aplicativo utiliza os seguintes pacotes no R:

- **shiny**: Para a interface e funcionalidade do aplicativo.
- **ggplot2**: Para a geração de gráficos.

Certifique-se de que esses pacotes estão instalados no seu ambiente R antes de executar o aplicativo.

## Como Executar

1. Copie o código do aplicativo Shiny e salve-o em um arquivo `app.R`.
2. Execute o arquivo em qualquer ambiente que suporte Shiny, como o RStudio.
3. Interaja com o aplicativo por meio do navegador.

## Observações

- Certifique-se de que os dados carregados no aplicativo estejam no formato correto (arquivo `.csv` com colunas bem definidas).
- O modelo de regressão exige que as variáveis selecionadas sejam compatíveis (por exemplo, a variável resposta deve ser numérica para regressão linear).

---

### Exemplo de uso

#### Input
- Um arquivo CSV com colunas chamadas `idade`, `salário` e `experiência`.

#### Passos
1. Faça o upload do arquivo.
2. Selecione `salário` como variável resposta.
3. Escolha `idade` e `experiência` como variáveis preditoras.

#### Output
- Os resultados do modelo de regressão (coeficientes, significância, etc.).
- Gráficos de resíduos e Q-Q disponíveis para visualização e download.

---

Desenvolvido com R e Shiny.