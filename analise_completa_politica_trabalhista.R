# =============================================================================
# SAIC / Diga Gente — Perfil dos remetentes de cartas com a categoria
# "POLITICA TRABALHISTA" e associação com renda, instrução, idade, sexo e zona
#
# Ordem da análise:
#   1) Preparação dos dados (instrução com analfabetos agrupados ao 1º grau incompleto)
#   2) Perfil dos remetentes (descritivo)
#   3) TESTES DE QUI-QUADRADO (associação bivariada)  <- primeiro passo inferencial
#   4) Efeito de cada variável controlando pelas demais (regressão logística)
#   5) Gráficos descritivos
#   6) Gráficos do logit (OR e efeitos marginais em painéis; prob. prevista: 1 imagem por variável)
# Requer R >= 4.1 (usa o pipe nativo |> e \(x))
# Rode o script INTEIRO, na ordem (Ctrl+A, Ctrl+Enter).
# =============================================================================

# ---- 0. Pacotes e pasta de trabalho -----------------------------------------
pacotes <- c("tidyverse", "marginaleffects")
novos <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(novos)) install.packages(novos)
library(tidyverse)
library(marginaleffects)

# Pasta onde está o CSV (barras normais "/"). Os resultados também são salvos aqui.
setwd("C:/Users/Usuário/Desktop/Mestrado Producao Academica/Artigos/Constituicao dos Sonhos/Constitucionalizando os Direitos Socias por Debaixo")

# Salva CSV; se o arquivo estiver aberto (no Excel, por exemplo), salva com data/hora no nome
salvar_csv <- function(x, arq) {
  ok <- tryCatch({ write_csv(x, arq); TRUE }, error = \(e) FALSE)
  if (!ok) {
    novo <- sub("\\.csv$", paste0("_", format(Sys.time(), "%H%M%S"), ".csv"), arq)
    write_csv(x, novo)
    warning("'", arq, "' estava aberto; salvei como '", novo, "'. Feche o arquivo no Excel.")
  }
}

# ---- 1. Dados ----------------------------------------------------------------
arquivo <- "SAIC_universo_TEMAS.csv"
stopifnot(file.exists(arquivo))

dados <- read_delim(
  arquivo,
  delim = ";",
  locale = locale(encoding = "UTF-8"),   # o arquivo tem BOM; o readr trata
  col_types = cols(.default = col_character())
)

dados <- dados |>
  rename(FAIXA_ETARIA = `FAIXA ETÁRIA`,
         FAIXA_RENDA  = `FAIXA RENDA`,
         ESTADO_CIVIL = `ESTADO CIVIL`)

# Variável dependente (Y): a carta tem a categoria POLITICA TRABALHISTA?
# ATENÇÃO: no CATALOGO o rótulo às vezes quebra de linha ("POLITICA\r\n TRABALHISTA"),
# então a busca por "POLITICA TRABALHISTA" com um espaço perde 2.399 cartas (acha 18.939).
# Com \\s+ no lugar do espaço, o CATALOGO dá 21.338 cartas (29,8%). A coluna
# TEMA_TRABALHISTA é mais ampla (23.005). Escolha qual usar:
usar_tema_trabalhista <- FALSE   # FALSE = CATALOGO | TRUE = coluna TEMA_TRABALHISTA

dados <- dados |>
  mutate(
    trabalhista = if (usar_tema_trabalhista) {
      as.integer(!is.na(TEMA_TRABALHISTA))
    } else {
      as.integer(str_detect(replace_na(CATALOGO, ""), "POLITICA\\s+TRABALHISTA"))
    }
  )

# Fatores com ordem natural das categorias (a 1ª categoria é a REFERÊNCIA no logit)
lv_renda <- c("SEM RENDIMENTO", "ATÉ 1 SALÁRIO MÍNIMO", "MAIS DE 01 A 02 S.M.",
              "MAIS DE 02 A 03 S.M.", "MAIS DE 03 A 05 S.M.",
              "MAIS DE 05 A 10 S.M.", "MAIS DE 10 A 20 S.M.", "MAIS DE 20 S.M.")

# Instrução: ANALFABETO(A) (639 cartas) é agrupado ao 1º grau incompleto
# (= ensino fundamental incompleto), porque o grupo é pequeno e deixava o
# modelo instável. Os nomes seguem a nomenclatura atual; entre parênteses, o
# rótulo original da base SAIC.
nivel_baixo <- "Até ensino fundamental incompleto"   # ANALFABETO(A) + PRIMEIRO GRAU INCOMPLETO
lv_instr <- c(nivel_baixo,
              "Ensino fundamental completo",    # PRIMEIRO GRAU COMPLETO
              "Ensino médio incompleto",        # SEGUNDO GRAU INCOMPLETO
              "Ensino médio completo",          # SEGUNDO GRAU COMPLETO
              "Ensino superior incompleto",     # SUPERIOR INCOMPLETO
              "Ensino superior completo",       # SUPERIOR COMPLETO
              "Pós-graduação")                  # PÓS-GRADUAÇÃO

# Tabela de conversão: rótulo original da base -> nome novo
mapa_instr <- c("ANALFABETO(A)"            = nivel_baixo,
                "PRIMEIRO GRAU INCOMPLETO" = nivel_baixo,
                "PRIMEIRO GRAU COMPLETO"   = "Ensino fundamental completo",
                "SEGUNDO GRAU INCOMPLETO"  = "Ensino médio incompleto",
                "SEGUNDO GRAU COMPLETO"    = "Ensino médio completo",
                "SUPERIOR INCOMPLETO"      = "Ensino superior incompleto",
                "SUPERIOR COMPLETO"        = "Ensino superior completo",
                "PÓS-GRADUAÇÃO"            = "Pós-graduação")

lv_idade <- c("10 A 14 ANOS", "15 A 19 ANOS", "20 A 24 ANOS", "25 A 29 ANOS",
              "30 A 39 ANOS", "40 A 49 ANOS", "50 A 59 ANOS", "ACIMA DE 59 ANOS")

dados <- dados |>
  mutate(
    renda     = factor(FAIXA_RENDA, levels = lv_renda),
    instrucao = factor(unname(mapa_instr[INSTRUCAO]), levels = lv_instr),
    idade     = factor(FAIXA_ETARIA, levels = lv_idade),
    sexo      = factor(SEXO,        levels = c("MASCULINO", "FEMININO")),
    zona      = factor(MORADOR,     levels = c("ZONA URBANA", "ZONA RURAL"))
  )

# Checagem: algum rótulo do arquivo ficou fora dos níveis (problema de acento)?
checa <- function(orig, fat, nome) {
  x <- unique(orig[!is.na(orig) & is.na(fat)])
  if (length(x)) warning(nome, ": rótulos não reconhecidos -> ", paste(x, collapse = " | "))
}
checa(dados$FAIXA_RENDA, dados$renda, "renda")
checa(dados$INSTRUCAO, dados$instrucao, "instrucao")
checa(dados$FAIXA_ETARIA, dados$idade, "idade")
checa(dados$SEXO, dados$sexo, "sexo")
checa(dados$MORADOR, dados$zona, "zona")

vars <- c("renda", "instrucao", "idade", "sexo", "zona")

cat("\nCartas no universo:", nrow(dados),
    "\nCartas com a categoria:", sum(dados$trabalhista),
    sprintf("(%.1f%%)\n", 100 * mean(dados$trabalhista)))

# Valores ausentes por variável (serão excluídos de cada teste)
print(dados |> summarise(across(all_of(vars), \(x) sum(is.na(x)))))

# ---- 2. Perfil dos remetentes ------------------------------------------------
# Composição (%) de quem tem x quem não tem a categoria, em cada variável
perfil <- map_dfr(vars, \(v) {
  dados |>
    filter(!is.na(.data[[v]])) |>
    count(grupo = if_else(trabalhista == 1, "com_categoria", "sem_categoria"),
          categoria = as.character(.data[[v]]),
          ordem = as.integer(.data[[v]])) |>
    group_by(grupo) |>
    mutate(pct = round(100 * n / sum(n), 1)) |>
    ungroup() |>
    pivot_wider(id_cols = c(categoria, ordem),
                names_from = grupo, values_from = c(n, pct)) |>
    arrange(ordem) |>
    mutate(variavel = v, .before = 1) |>
    select(-ordem)
})
print(perfil, n = Inf, width = Inf)
salvar_csv(perfil, "perfil_remetentes_trabalhista.csv")

# ---- 3. TESTES DE QUI-QUADRADO (primeiro passo) ------------------------------
# H0: a presença da categoria é independente da variável.
# Cada teste usa todos os casos válidos daquela variável.
cramer_v <- function(teste, tab) {
  n <- sum(tab)
  sqrt(unname(teste$statistic) / (n * (min(dim(tab)) - 1)))
}

qui <- map(vars, \(v) {
  tab <- table(dados[[v]], dados$trabalhista)       # exclui NA automaticamente
  teste <- chisq.test(tab)
  list(variavel = v, tab = tab, teste = teste, V = cramer_v(teste, tab))
}) |> set_names(vars)

resultado_qui <- map_dfr(qui, \(q) tibble(
  variavel = q$variavel,
  n = sum(q$tab),
  qui2 = round(unname(q$teste$statistic), 1),
  gl = unname(q$teste$parameter),
  p_valor = q$teste$p.value,
  cramer_V = round(q$V, 3),
  menor_esperado = round(min(q$teste$expected), 1)   # pressuposto: >= 5
))
print(resultado_qui)
salvar_csv(resultado_qui, "qui_quadrado_trabalhista.csv")

# Resíduos padronizados: quais categorias pesam na associação
# (|resíduo| > 1,96 => acima/abaixo do esperado sob independência)
walk(qui, \(q) {
  cat("\n---", q$variavel, "---\n")
  print(round(q$teste$stdres, 2))
})

# Leitura do V de Cramér: ~0,1 efeito pequeno | ~0,3 médio | ~0,5 grande.
# Com n > 60 mil quase tudo dá p < 0,001: avalie o TAMANHO do efeito, não só o p.

# ---- 4. Cada variável como independente, as outras como controle ------------
# Mesma amostra (casos completos nas 5 variáveis) para todos os modelos.
cc <- dados |> drop_na(trabalhista, all_of(vars))
cat("\nCasos completos usados nos modelos:", nrow(cc), "\n")

modelo_cheio <- glm(trabalhista ~ renda + instrucao + idade + sexo + zona,
                    family = binomial, data = cc)

# Teste da razão de verossimilhança: para cada variável focal, compara o modelo
# SEM ela (só controles) com o modelo completo -> efeito ajustado pelas demais.
lrt <- map_dfr(vars, \(v) {
  controles <- setdiff(vars, v)
  m0 <- glm(reformulate(controles, response = "trabalhista"),
            family = binomial, data = cc)
  a <- anova(m0, modelo_cheio, test = "Chisq")
  tibble(variavel_independente = v,
         controles = paste(controles, collapse = " + "),
         gl = a$Df[2], qui2_LRT = round(a$Deviance[2], 1),
         p_valor = a$`Pr(>Chi)`[2])
})
print(lrt, width = Inf)
salvar_csv(lrt, "lrt_ajustado_trabalhista.csv")

# Razões de chances (OR) ajustadas, IC 95% de Wald
coefs <- summary(modelo_cheio)$coefficients
or <- tibble(termo = rownames(coefs),
             OR = exp(coefs[, "Estimate"]),
             IC_inf = exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"]),
             IC_sup = exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"]),
             p_valor = coefs[, "Pr(>|z|)"]) |>
  filter(termo != "(Intercept)") |>
  mutate(across(c(OR, IC_inf, IC_sup), \(x) round(x, 2)))
print(or, n = Inf)
salvar_csv(or, "or_ajustadas_trabalhista.csv")
# Referência de cada variável = 1º nível: SEM RENDIMENTO, Até ensino fundamental
# incompleto, 10 A 14 ANOS, MASCULINO, ZONA URBANA.

# Efeitos marginais médios (diferença em pontos percentuais na probabilidade)
ame <- avg_comparisons(modelo_cheio)
print(ame)
salvar_csv(as_tibble(ame), "efeitos_marginais_trabalhista.csv")

# ---- 5. Gráficos descritivos (sem controles) ---------------------------------
nomes_var <- c(renda = "Renda", instrucao = "Instrução", idade = "Idade",
               sexo = "Sexo", zona = "Zona")

# Rótulo legível: "MAIS DE 01 A 02 S.M." -> "Mais de 01 a 02 S.M."
rot <- \(x) x |>
  str_to_sentence() |>
  str_replace_all("s\\.m\\.", "S.M.") |>
  str_wrap(26)

media_geral <- mean(dados$trabalhista)
col_acima  <- "#1d4e89"   # acima da média geral
col_abaixo <- "#8ecae6"   # abaixo da média geral

grafico_desc <- function(v) {
  d <- dados |>
    filter(!is.na(.data[[v]])) |>
    group_by(cat = .data[[v]]) |>
    summarise(n = n(), p = mean(trabalhista), .groups = "drop") |>
    mutate(ep = sqrt(p * (1 - p) / n),
           inf = pmax(p - 1.96 * ep, 0), sup = p + 1.96 * ep,
           acima = p > media_geral)

  ggplot(d, aes(x = cat, y = p, fill = acima)) +
    geom_col(width = 0.7) +
    geom_errorbar(aes(ymin = inf, ymax = sup), width = 0.25, colour = "grey20") +
    geom_text(aes(y = sup, label = scales::percent(p, accuracy = 0.1)),
              hjust = -0.15, size = 4) +
    geom_hline(yintercept = media_geral, linetype = "dashed", colour = "#c1121f") +
    scale_fill_manual(values = c(`TRUE` = col_acima, `FALSE` = col_abaixo),
                      guide = "none") +
    scale_x_discrete(limits = rev, labels = rot) +
    scale_y_continuous(labels = scales::percent,
                       expand = expansion(mult = c(0, 0.15))) +
    coord_flip() +
    labs(title = paste("Presença da categoria Política Trabalhista por",
                       tolower(nomes_var[[v]])),
         subtitle = sprintf("Percentual bruto (sem controles). Linha tracejada = média geral (%.1f%%)",
                            100 * media_geral),
         x = NULL, y = "% das cartas com a categoria") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(colour = "black"),
          plot.title = element_text(face = "bold"))
}

for (v in vars) {
  g <- grafico_desc(v)
  print(g)
  ggsave(paste0("desc_", v, ".png"), g, width = 10, height = 6, dpi = 200,
         bg = "white")
}

# ---- 6. Gráficos do logit (ajustados pelas demais variáveis) -----------------
# - Razões de chances e efeitos marginais: UMA imagem com as 5 variáveis (painéis)
# - Probabilidade prevista: UMA imagem POR variável (5 imagens)
ordem_cats <- unlist(map(vars, \(v) levels(cc[[v]])))

# ---- 6A. Razões de chances ajustadas (forest plot, todas as variáveis) ----
or_raw <- tibble(termo = rownames(coefs),
                 OR = exp(coefs[, "Estimate"]),
                 inf = exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"]),
                 sup = exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"]))

or_plot <- map_dfr(vars, \(v) {
  niv <- levels(cc[[v]])
  tibble(variavel = v, categoria = niv, termo = paste0(v, niv))
}) |>
  left_join(or_raw, by = "termo") |>
  mutate(ref = is.na(OR),
         OR = if_else(ref, 1, OR),
         inf = if_else(ref, 1, inf),
         sup = if_else(ref, 1, sup),
         efeito = case_when(ref ~ "Referência",
                            inf > 1 ~ "Maior chance",
                            sup < 1 ~ "Menor chance",
                            TRUE ~ "Sem diferença"),
         variavel = factor(variavel, levels = vars, labels = nomes_var[vars]),
         categoria = factor(categoria, levels = rev(ordem_cats)))

g_or <- ggplot(or_plot, aes(x = OR, y = categoria, colour = efeito)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
  geom_pointrange(aes(xmin = inf, xmax = sup), size = 0.6) +
  scale_colour_manual(values = c("Maior chance" = "#c1121f",
                                 "Menor chance" = "#1d4e89",
                                 "Sem diferença" = "grey55",
                                 "Referência" = "black"),
                      name = NULL) +
  scale_x_log10() +
  scale_y_discrete(labels = rot) +
  facet_wrap(~ variavel, scales = "free_y", ncol = 2) +
  labs(title = "Razões de chances ajustadas de a carta ter Política Trabalhista",
       subtitle = "Modelo logístico com as cinco variáveis juntas (cada uma controlada pelas demais). IC 95%, escala log.",
       x = "Razão de chances (OR)", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 14),
        plot.title = element_text(face = "bold"))
print(g_or)
ggsave("logit_razoes_de_chances.png", g_or, width = 13, height = 12, dpi = 200,
       bg = "white")

# ---- 6B. Probabilidade prevista ajustada: UMA IMAGEM POR VARIÁVEL ----
pred <- map_dfr(vars, \(v) {
  avg_predictions(modelo_cheio, variables = v) |>   # padronizada: todas as cartas recebem a categoria
    as_tibble() |>
    transmute(variavel = v,
              categoria = as.character(.data[[v]]),
              p = estimate, inf = conf.low, sup = conf.high)
}) |>
  mutate(categoria = factor(categoria, levels = rev(ordem_cats)))

# Escala do eixo da probabilidade, IGUAL nas cinco figuras (comparáveis entre si).
# c(0, 0.5) parte do zero; para dar mais zoom use, por exemplo, c(0.10, 0.45).
limite_x <- c(0, 0.5)

# Altura da figura proporcional ao número de categorias
altura <- \(v) max(3.5, 1.8 + 0.6 * nlevels(cc[[v]]))

grafico_pred <- function(v) {
  d <- pred |> filter(variavel == v) |> mutate(categoria = droplevels(categoria))
  ggplot(d, aes(x = p, y = categoria)) +
    geom_vline(xintercept = mean(cc$trabalhista), linetype = "dashed",
               colour = "#c1121f") +
    geom_pointrange(aes(xmin = inf, xmax = sup), colour = col_acima, size = 0.7) +
    geom_text(aes(label = scales::percent(p, accuracy = 0.1)),
              vjust = -1.1, size = 4) +
    scale_x_continuous(labels = scales::percent, limits = limite_x) +
    scale_y_discrete(labels = rot) +
    labs(title = paste("Probabilidade prevista de ter Política Trabalhista:",
                       tolower(nomes_var[[v]])),
         subtitle = "Média ajustada pelas demais variáveis, IC 95%",
         caption = str_wrap(sprintf("Linha tracejada = média da amostra do modelo (%.1f%%). Previsão padronizada: todas as cartas recebem a categoria, com as demais variáveis como observadas.",
                                    100 * mean(cc$trabalhista)), 110),
         x = "Probabilidade prevista", y = NULL) +
    theme_minimal(base_size = 14) +
    theme(axis.text.y = element_text(colour = "black"),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 11),
          plot.caption = element_text(size = 9, hjust = 0, colour = "grey30"),
          plot.caption.position = "plot")
}

for (v in vars) {
  g <- grafico_pred(v)
  print(g)
  ggsave(paste0("logit_prob_", v, ".png"), g, width = 10, height = altura(v),
         dpi = 200, bg = "white")
}

# ---- 6C. Efeitos marginais médios (pontos percentuais, todas as variáveis) ----
ame_plot <- as_tibble(ame) |>
  mutate(categoria = str_extract(contrast, "^.*(?= - )"),
         variavel = factor(term, levels = vars, labels = nomes_var[vars]),
         efeito = case_when(conf.low > 0 ~ "Maior probabilidade",
                            conf.high < 0 ~ "Menor probabilidade",
                            TRUE ~ "Sem diferença"),
         categoria = factor(categoria, levels = rev(ordem_cats)))

g_ame <- ggplot(ame_plot, aes(x = estimate * 100, y = categoria, colour = efeito)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_pointrange(aes(xmin = conf.low * 100, xmax = conf.high * 100), size = 0.6) +
  scale_colour_manual(values = c("Maior probabilidade" = "#c1121f",
                                 "Menor probabilidade" = "#1d4e89",
                                 "Sem diferença" = "grey55"),
                      name = NULL) +
  scale_y_discrete(labels = rot) +
  facet_wrap(~ variavel, scales = "free_y", ncol = 2) +
  labs(title = "Diferença na probabilidade de ter Política Trabalhista (pontos percentuais)",
       subtitle = "Em relação à categoria de referência de cada variável (1ª categoria), com as demais controladas. IC 95%",
       x = "Pontos percentuais", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 14),
        plot.title = element_text(face = "bold"))
print(g_ame)
ggsave("logit_efeitos_marginais.png", g_ame, width = 13, height = 12, dpi = 200,
       bg = "white")

# ---- 7. Citações (para a seção de metodologia) -------------------------------
# citation(); citation("tidyverse"); citation("marginaleffects"); sessionInfo()
