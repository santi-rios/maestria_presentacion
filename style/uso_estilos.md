
### 1. Referencias más pequeñas

Para que las citas bibliográficas como `[@maramis2021]` aparezcan más pequeñas, puedes añadir una regla CSS.

Agrega el siguiente código al final de tu archivo style.css. Esto hará que el texto de la cita sea un 80% del tamaño del texto que lo rodea.

````css
// ...existing code...
.reveal .slides section .fragment.text-emphasis.visible {
  background-size: 100% 100%;
}

.citation {
  font-size: 80%;
  vertical-align: super;
}
````

### 2. Logo solo en la diapositiva de título

Para que el logo aparezca únicamente en la diapositiva del título, primero elimina la opción `logo` del encabezado YAML de tu archivo presentacion.qmd. Luego, puedes añadir el logo directamente en la diapositiva del título usando la opción `background-image`.

Realiza los siguientes cambios en presentacion.qmd:

````quarto
// ...existing code...
    incremental: false # listas aparecen de uno en uno
    theme: "./style/custom.scss" # beige/blood/dark/league/moon/night/serif/simple/sky/solarized
    slide-number: true # mostrar número de diapositiva
    lang: es
    parallax-background-image: "./style/fondo.png"
// ...existing code...
#   include: false
  execute-dir: project
css: ./style/style.css
bibliography: zoterov3.bib
// ...existing code...
    - shortname: trp
      longname: Triptófano
---

# {background-image="./Figures/unamposgrado.png" background-size="150px" background-position="top 10px right 10px"}
````

# Citas

## Opción 1: CSS puro (automático para referencias múltiples)

Agrega este código a tu archivo style.css:

````css
// ...existing code...
.citation {
  font-size: 70%;
  /* vertical-align: super; */
}

/* Estilos para referencias múltiples */
.citations .citation:nth-child(1) {
  color: #1f77b4; /* Azul */
}

.citations .citation:nth-child(2) {
  color: #ff7f0e; /* Naranja */
}

.citations .citation:nth-child(3) {
  color: #2ca02c; /* Verde */
}

.citations .citation:nth-child(4) {
  color: #d62728; /* Rojo */
}

.citations .citation:nth-child(5) {
  color: #9467bd; /* Púrpura */
}

.citations .citation:nth-child(6) {
  color: #8c564b; /* Marrón */
}

/* Alternativa: usar pseudo-selectores para separar por comas */
.citation-group .citation:nth-of-type(1) { color: #1f77b4; }
.citation-group .citation:nth-of-type(2) { color: #ff7f0e; }
.citation-group .citation:nth-of-type(3) { color: #2ca02c; }
.citation-group .citation:nth-of-type(4) { color: #d62728; }
.citation-group .citation:nth-of-type(5) { color: #9467bd; }
````

## Opción 2: Usar spans manuales (control total)

Para casos específicos donde quieras control total, puedes usar spans con clases personalizadas en tu texto:

````quarto
// ...existing code...
[Estos trastornos afectan cómo aprendemos, recordamos, planificamos y regulamos nuestras conductas [<span class="cite-1">@girotti2024</span>; <span class="cite-2">@uddin2021</span>; <span class="cite-3">@maramis2021</span>].]{style="background-color: black;"}
// ...existing code...
````

Y agrega estos estilos CSS:

````css
// ...existing code...
.cite-1 { color: #1f77b4; font-size: 70%; }
.cite-2 { color: #ff7f0e; font-size: 70%; }
.cite-3 { color: #2ca02c; font-size: 70%; }
.cite-4 { color: #d62728; font-size: 70%; }
.cite-5 { color: #9467bd; font-size: 70%; }
.cite-6 { color: #8c564b; font-size: 70%; }
````

## Opción 3: JavaScript automático (más avanzado)

Si quieres automatizar completamente el proceso, puedes agregar un script que detecte y coloree automáticamente las referencias múltiples. Agrega esto al final de tu archivo CSS:

````css
// ...existing code...
<script>
document.addEventListener('DOMContentLoaded', function() {
  const colors = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b'];
  
  // Buscar elementos con múltiples citas
  const citations = document.querySelectorAll('.citation');
  
  citations.forEach((citation, index) => {
    const siblings = citation.parentElement.querySelectorAll('.citation');
    if (siblings.length > 1) {
      const colorIndex = Array.from(siblings).indexOf(citation);
      citation.style.color = colors[colorIndex % colors.length];
    }
  });
});
</script>
````

**Recomendación**: Te sugiero empezar con la **Opción 2** (spans manuales) ya que te da control total y es más confiable. Para los casos donde tengas múltiples referencias como `[@girotti2024; @uddin2021; @maramis2021]`, simplemente las escribes como:

```markdown
[<span class="cite-1">@girotti2024</span>; <span class="cite-2">@uddin2021</span>; <span class="cite-3">@maramis2021</span>]
```

# Fragments

## 1. **Fragment Blur Effect**
```css
.reveal .slides section .fragment.blur
```

**Qué hace**: Aplica un efecto de desenfoque que se quita cuando el fragmento se vuelve visible.

**Cómo usar**:
```markdown
::: {.fragment .blur}
Este texto aparecerá desenfocado y luego se enfocará
:::

![](imagen.png){.fragment .blur}
```

## 2. **Spotlight Effect**
```css
.reveal .slides section .fragment.spotlight
```

**Qué hace**: El elemento aparece con baja opacidad (30%) y al volverse visible obtiene brillo completo con sombra blanca.

**Cómo usar**:
```markdown
::: {.fragment .spotlight}
¡Este contenido se iluminará dramáticamente!
:::

![](grafico-importante.png){.fragment .spotlight}
```

## 3. **Annotation Pulse**
```css
.reveal .slides section .fragment.annotate
```

**Qué hace**: El elemento aparece escalado y con una animación de pulso dorado que se repite 3 veces.

**Cómo usar**:
```markdown
::: {.fragment .annotate}
**Resultado clave de la investigación**
:::

![](resultado-critico.png){.fragment .annotate}
```

## 4. **Data Build-up**
```css
.reveal .slides section .fragment.data-build
```

**Qué hace**: Revela el contenido progresivamente de izquierda a derecha usando clip-path.

**Cómo usar**:
```markdown
::: {.fragment .data-build}
Los datos muestran un incremento del 250% en neuroplasticidad
:::

![](grafico-barras.png){.fragment .data-build}
```

## 5. **Image Comparison Slider**
```css
.reveal .slides section .fragment.compare-slider
```

**Qué hace**: Crea un efecto de cortina oscura que se extiende sobre el elemento.

**Cómo usar**:
```markdown
::: {.fragment .compare-slider}
![Antes del tratamiento](antes.png)
:::

::: {.fragment}
![Después del tratamiento](despues.png)
:::
```

## 6. **Pointer Effect**
```css
.reveal .slides section .fragment.pointer
```

**Qué hace**: Añade una flecha roja (➤) que rebota, útil para señalar elementos importantes.

**Cómo usar**:
```markdown
::: {.fragment .pointer}
**Área crítica del hipocampo**
:::

![](hipocampo.png){.fragment .pointer style="position: relative;"}
```

## 7. **Text Emphasis**
```css
.reveal .slides section .fragment.text-emphasis
```

**Qué hace**: Aplica un subrayado amarillo progresivo al texto, como un marcatextos.

**Cómo usar**:
```markdown
[La neuroplasticidad es fundamental para el aprendizaje]{.fragment .text-emphasis}

::: {.fragment .text-emphasis}
**Conclusión principal**: El estrés reduce la neurogénesis hipocampal
:::
```

## 8. **Citations con Colores**
```css
.cite-1, .cite-2, .cite-3, etc.
```

**Qué hace**: Aplica colores diferentes a las referencias bibliográficas.

**Cómo usar**:
```markdown
Los estudios confirman estos hallazgos [<span class="cite-1">@smith2023</span>; <span class="cite-2">@jones2022</span>; <span class="cite-3">@garcia2024</span>].
```

## **Ejemplo Completo de Diapositiva**:

```markdown
# Resultados Principales

::: {.fragment .spotlight}
## Hallazgo 1: Neurogénesis Reducida
:::

::: {.fragment .data-build}
![](datos-neurogenesis.png)
:::

::: {.fragment .text-emphasis}
**El estrés crónico redujo la neurogénesis en un 45%**
:::

::: {.fragment .annotate}
Este resultado es consistente con estudios previos [<span class="cite-1">@willner1997</span>; <span class="cite-2">@maramis2021</span>]
:::

::: {.fragment .pointer}
→ Implicaciones para el tratamiento de la depresión
:::
```

## **Recomendación de Uso**:

- **Spotlight**: Para resultados clave o conclusiones importantes
- **Annotate**: Para hallazgos críticos que quieres enfatizar
- **Text-emphasis**: Para definiciones o conceptos centrales
- **Data-build**: Para gráficos o tablas con datos progresivos
- **Blur**: Para revelar información gradualmente
- **Pointer**: Para dirigir atención a elementos específicos en imágenes

Estos efectos te ayudarán a crear una presentación más dinámica y visualmente atractiva para tu defensa de tesis.



### Snippets pendientes


[{{< acr GD >}}, {{< acr CA >}}-3, {{< acr CA >}}]{style="font-size:85%;"} 

<!-- <img class="fragment custom spotlight" src="./Figures/hipocampo_minimal.png">

<div class="fragment custom annotate" 
     style="position:absolute; top:100px; left:150px;">
  <div>Important Feature</div>
</div> -->

<!-- <div class="fragment custom spotlit" 
     data-fragment-index="1">
  <img src="/Figures/hipocampo_minimal.png">
  <div class="fragment custom pointer" 
       data-fragment-index="2"
       style="position:absolute; top:30%; left:40%;">
  </div>
</div> -->

[Estos trastornos afectan cómo aprendemos, recordamos, planificamos y regulamos nuestras conductas [<span class="cite-1">@girotti2024</span>; <span class="cite-2">@uddin2021</span>; <span class="cite-3">@maramis2021</span>].]{style="background-color: black;"}

[Estos trastornos afectan cómo aprendemos, recordamos, planificamos y regulamos nuestras conductas [@girotti2024].]{style="background-color: black;"}
