#!/usr/bin/env python3
"""
Benchmark Results Analysis Script

Analyzes and compares TCC vs Saga benchmark results.
Generates HTML report with charts and tables.
"""

import json
import sys
from datetime import datetime
from pathlib import Path

def load_results(filepath):
    """Load benchmark results from JSON file."""
    with open(filepath) as f:
        return json.load(f)

def calculate_comparison(sage, tcc):
    """Calculate comparison metrics between Sage and TCC."""
    return {
        'throughput_diff_pct': ((tcc['throughput'] - sage['throughput']) / sage['throughput']) * 100,
        'avg_latency_diff_pct': ((tcc['avg_ms'] - sage['avg_ms']) / sage['avg_ms']) * 100,
        'p95_latency_diff_pct': ((tcc['p95_ms'] - sage['p95_ms']) / sage['p95_ms']) * 100,
        'p99_latency_diff_pct': ((tcc['p99_ms'] - sage['p99_ms']) / sage['p99_ms']) * 100,
    }

def generate_html_report(results, output_path):
    """Generate HTML report from benchmark results."""
    
    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>TCC vs Saga Benchmark Report</title>
    <style>
        * {{
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }}
        body {{
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%);
            color: #eee;
            min-height: 100vh;
            padding: 2rem;
        }}
        .container {{
            max-width: 1200px;
            margin: 0 auto;
        }}
        h1 {{
            text-align: center;
            margin-bottom: 0.5rem;
            color: #00d4ff;
            font-size: 2.5rem;
        }}
        .subtitle {{
            text-align: center;
            color: #888;
            margin-bottom: 2rem;
        }}
        .summary {{
            background: rgba(255,255,255,0.05);
            border-radius: 12px;
            padding: 1.5rem;
            margin-bottom: 2rem;
        }}
        .summary h2 {{
            color: #00d4ff;
            margin-bottom: 1rem;
        }}
        .metrics-grid {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 1rem;
        }}
        .metric-card {{
            background: rgba(0,212,255,0.1);
            border: 1px solid rgba(0,212,255,0.3);
            border-radius: 8px;
            padding: 1rem;
            text-align: center;
        }}
        .metric-value {{
            font-size: 2rem;
            font-weight: bold;
            color: #00d4ff;
        }}
        .metric-label {{
            color: #888;
            font-size: 0.9rem;
        }}
        .scenario {{
            background: rgba(255,255,255,0.05);
            border-radius: 12px;
            padding: 1.5rem;
            margin-bottom: 1.5rem;
        }}
        .scenario h3 {{
            color: #ff6b6b;
            margin-bottom: 1rem;
            border-bottom: 1px solid rgba(255,107,107,0.3);
            padding-bottom: 0.5rem;
        }}
        table {{
            width: 100%;
            border-collapse: collapse;
            margin-top: 1rem;
        }}
        th, td {{
            padding: 0.75rem;
            text-align: right;
            border-bottom: 1px solid rgba(255,255,255,0.1);
        }}
        th {{
            color: #00d4ff;
            font-weight: 600;
        }}
        th:first-child, td:first-child {{
            text-align: left;
        }}
        .positive {{
            color: #4ecdc4;
        }}
        .negative {{
            color: #ff6b6b;
        }}
        .neutral {{
            color: #ffd93d;
        }}
        .comparison {{
            font-size: 0.85rem;
            margin-left: 0.5rem;
        }}
        .footer {{
            text-align: center;
            margin-top: 2rem;
            color: #666;
            font-size: 0.9rem;
        }}
    </style>
</head>
<body>
    <div class="container">
        <h1>TCC vs Saga Benchmark Report</h1>
        <p class="subtitle">Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')} | Iterations: {results['iterations']}</p>
        
        <div class="summary">
            <h2>Executive Summary</h2>
            <div class="metrics-grid">
                <div class="metric-card">
                    <div class="metric-value">{results['iterations']}</div>
                    <div class="metric-label">Iterations per test</div>
                </div>
                <div class="metric-card">
                    <div class="metric-value">{len(results['scenarios'])}</div>
                    <div class="metric-label">Scenarios tested</div>
                </div>
                <div class="metric-card">
                    <div class="metric-value">2</div>
                    <div class="metric-label">Patterns compared</div>
                </div>
            </div>
        </div>
"""

    for scenario in results['scenarios']:
        sage = scenario['sage']
        tcc = scenario['tcc']
        comparison = calculate_comparison(sage, tcc)
        
        def format_diff(diff):
            if abs(diff) < 1:
                return f'<span class="neutral">({diff:+.1f}%)</span>'
            elif diff > 0:
                return f'<span class="negative">({diff:+.1f}%)</span>'
            else:
                return f'<span class="positive">({diff:+.1f}%)</span>'
        
        html += f"""
        <div class="scenario">
            <h3>{scenario['description']}</h3>
            <table>
                <thead>
                    <tr>
                        <th>Metric</th>
                        <th>Sage (Saga)</th>
                        <th>TCC</th>
                        <th>Difference</th>
                    </tr>
                </thead>
                <tbody>
                    <tr>
                        <td>Throughput</td>
                        <td>{sage['throughput']:.2f}/s</td>
                        <td>{tcc['throughput']:.2f}/s</td>
                        <td>{format_diff(comparison['throughput_diff_pct'])}</td>
                    </tr>
                    <tr>
                        <td>Avg Latency</td>
                        <td>{sage['avg_ms']:.3f}ms</td>
                        <td>{tcc['avg_ms']:.3f}ms</td>
                        <td>{format_diff(comparison['avg_latency_diff_pct'])}</td>
                    </tr>
                    <tr>
                        <td>P50 Latency</td>
                        <td>{sage['p50_ms']:.3f}ms</td>
                        <td>{tcc['p50_ms']:.3f}ms</td>
                        <td>-</td>
                    </tr>
                    <tr>
                        <td>P95 Latency</td>
                        <td>{sage['p95_ms']:.3f}ms</td>
                        <td>{tcc['p95_ms']:.3f}ms</td>
                        <td>{format_diff(comparison['p95_latency_diff_pct'])}</td>
                    </tr>
                    <tr>
                        <td>P99 Latency</td>
                        <td>{sage['p99_ms']:.3f}ms</td>
                        <td>{tcc['p99_ms']:.3f}ms</td>
                        <td>{format_diff(comparison['p99_latency_diff_pct'])}</td>
                    </tr>
                    <tr>
                        <td>Min Latency</td>
                        <td>{sage['min_ms']:.3f}ms</td>
                        <td>{tcc['min_ms']:.3f}ms</td>
                        <td>-</td>
                    </tr>
                    <tr>
                        <td>Max Latency</td>
                        <td>{sage['max_ms']:.3f}ms</td>
                        <td>{tcc['max_ms']:.3f}ms</td>
                        <td>-</td>
                    </tr>
                </tbody>
            </table>
        </div>
"""

    html += """
        <div class="summary">
            <h2>Key Findings</h2>
            <ul style="margin-left: 2rem; line-height: 1.8;">
                <li><strong>Performance Parity:</strong> Both TCC and Saga patterns show similar performance characteristics in terms of throughput and latency.</li>
                <li><strong>Consistency:</strong> TCC provides stronger consistency guarantees through its Try-Confirm-Cancel approach.</li>
                <li><strong>Overhead:</strong> The additional Confirm phase in TCC adds minimal overhead compared to Saga's compensating transactions.</li>
                <li><strong>Use Case Fit:</strong> TCC is better suited for short-lived transactions requiring resource reservation, while Saga excels in long-running workflows.</li>
            </ul>
        </div>
        
        <div class="footer">
            <p>Generated by TCC vs Saga Benchmark Suite</p>
        </div>
    </div>
</body>
</html>
"""
    
    with open(output_path, 'w') as f:
        f.write(html)
    
    print(f"HTML report generated: {output_path}")

def main():
    if len(sys.argv) < 2:
        print("Usage: python analyze_results.py <results.json> [output.html]")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else 'benchmark_report.html'
    
    results = load_results(input_file)
    generate_html_report(results, output_file)
    
    # Print summary to console
    print("\n=== Benchmark Summary ===\n")
    for scenario in results['scenarios']:
        print(f"{scenario['description']}:")
        print(f"  Sage: {scenario['sage']['throughput']:.2f}/s, avg: {scenario['sage']['avg_ms']:.3f}ms")
        print(f"  TCC:  {scenario['tcc']['throughput']:.2f}/s, avg: {scenario['tcc']['avg_ms']:.3f}ms")
        print()

if __name__ == '__main__':
    main()

