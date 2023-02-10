``` ini

BenchmarkDotNet=v0.13.4, OS=ubuntu 22.04
AMD Ryzen 5 4500U with Radeon Graphics, 1 CPU, 6 logical and 6 physical cores
.NET SDK=6.0.113
  [Host]     : .NET 6.0.13 (6.0.1322.60201), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 6.0.13 (6.0.1322.60201), X64 RyuJIT AVX2


```
|    Method |    Mean |    Error |   StdDev | Ratio | RatioSD |
|---------- |--------:|---------:|---------:|------:|--------:|
|   BaseAdd | 1.394 s | 0.0274 s | 0.0346 s |  1.00 |    0.00 |
| Level2Add | 1.272 s | 0.0249 s | 0.0467 s |  0.91 |    0.02 |
| Level4Add | 1.244 s | 0.0247 s | 0.0458 s |  0.90 |    0.02 |
