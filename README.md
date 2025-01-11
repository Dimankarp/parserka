# parserka - Функциональное программирование - ЛР 2

## Введение

- Студент: ***Хороших Дмитрий Максимович 367597***
- Вариант: ***Библиотека парсе-комбинаторов и парсер YAML***

## Описание

Parserka - библиотека LL парсер-комбинаторов.

Прообазом и основным референсом послужила библиотека Parsec - *[Parsec paper](chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf)*.

### Parser

Тип парсера представляет из себя обёртку над функцией, принимающей состояние *State i* и возвращающее значение типа *Consumed = Consumed | Empty*. Фактически *Consumed* позволяет сделать LL(1) парсер.

```haskell
newtype Parser i a = Parser
  {runParser :: State i -> Consumed i a}
```

### Sequence Combinator

Комбинатор, связывающий последовательные парсеры в один *(>>=)* реализуется как метод класса монада.

Логика работы следующая: если парсер завершился удачно, то управление передаётся следующему парсерсу, иначе - возвращается ошибка первого парсера. При этом большую роль играет то, потребил ли первый парсер элементы входного потока (*Consumed*) или нет (*Empty*)

```haskell

instance Monad (Parser i) where
  p >>= f =
    Parser
      ( \state -> case (runParser p state) of
          Empty r1 ->
            case (r1) of
              Ok x state1 _ -> (runParser (f x) state1)
              Error msg -> Empty (Error msg)
          Consumed r1 ->
            Consumed
              ( case (r1) of
                  Ok x state1 _ -> case (runParser (f x) state1) of
                    Consumed r2 -> r2
                    Empty r2 -> r2
                  Error msg -> Error msg
              )
      )

```

### Choice Combinator

Комбинатор, описывающий выбор из 2-х парсеров - левоцентричен, но приоритезирует *Consumed* результаты.

Логика работы следующая: если парсер завершился удачно и принял значение из потока, то его результат возвращается, иначе на том же стейте запускается второй парсер, если он успешен, то его результат выводится.

```haskell

instance Alternative (Parser i) where
  p <|> q =
    Parser
      ( \state -> case (runParser p state) of
          Empty (Error msg1) -> case (runParser q state) of
            Empty (Error msg2) -> mergeError msg1 msg2
            Empty (Ok x state' msg2) -> mergeOk x state' msg1 msg2
            consumed -> consumed
          Empty (Ok x state' msg1) -> case (runParser q state) of
            Empty (Error msg2) -> mergeOk x state' msg1 msg2
            Empty (Ok _ _ msg2) -> mergeOk x state' msg1 msg2
            consumed -> consumed
          consumed -> consumed
      )

...

```

### YAMLparserka

*YAMLparserka* - лексер и парсер подмножества языка YAML, написанный при помощи библиотеки parserka.

Поддерживает основные типы YAML, гетерогенные словари и массивы, идентацию.

Использование:

```bash
stack run YAMLparserka -- file.yaml
```

```bash
$ cat test.yaml
---
# all markdown
MD013: false # Line length
MD007: # Unordered list indentation
  indent: 4
MD051: false # Link fragments should be valid

# slides
MD035: false # Horizontal rule style
MD033: 
  - "br"
  - "div"

# need to be removed
MD045: false # Images should have alternate text (alt text)
MD025: false # Multiple top-level headings in the same document
MD011: false # Reversed link syntax conflix with maths.

MD041: false # Required for course note

---
B: 256
...

stack run YAMLparserka -- test.yaml
...
{
 "MD007";{
          "indent";4
         }

 "MD011";False
 "MD013";False
 "MD025";False
 "MD033";[
          "br",
          "div",
         ]

 "MD035";False
 "MD041";False
 "MD045";False
 "MD051";False
}
{
 "B";256
}
```
