``` ini

BenchmarkDotNet=v0.13.4, OS=ubuntu 22.04
AMD Ryzen 5 4500U with Radeon Graphics, 1 CPU, 6 logical and 6 physical cores
.NET SDK=6.0.113
  [Host]     : .NET 6.0.13 (6.0.1322.60201), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 6.0.13 (6.0.1322.60201), X64 RyuJIT AVX2


```
|     Method |    Mean |    Error |   StdDev |  Median | Ratio | RatioSD |
|----------- |--------:|---------:|---------:|--------:|------:|--------:|
|   BaseMult | 5.482 s | 0.0112 s | 0.0105 s | 5.482 s |  1.00 |    0.00 |
| Level2Mult | 1.633 s | 0.0462 s | 0.1334 s | 1.588 s |  0.34 |    0.01 |
| Level4Mult | 3.158 s | 0.1198 s | 0.3495 s | 3.192 s |  0.58 |    0.06 |
