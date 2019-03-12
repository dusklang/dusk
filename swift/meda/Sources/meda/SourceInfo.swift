public typealias SourceRange = Range<String.Index>

public func + (a: SourceRange, b: SourceRange) -> SourceRange {
    return Swift.min(a.lowerBound, b.lowerBound)..<Swift.max(a.upperBound, b.upperBound)
}

public struct LineRange {
    public let line: Int
    public let startColumn: Int
    public let endColumn: Int
}

public struct SourceFile {
    private var lines: [String.Index] = []
    public let name: String
    public let src: String

    public init(name: String, src: String) {
        self.name = name 
        self.src = src
    }

    public mutating func nextLinePosition(linePos: String.Index) {
        lines.append(linePos)
    }

    public func substring(from range: SourceRange) -> Substring {
        return src[range]
    }

    public func substring(from line: Int) -> Substring {
        let start = lines[line]
        let end = line == lines.count - 1 ? 
            src.endIndex : 
            lines[line + 1]
        return src[start..<end]
    }

    public func lines(in range: SourceRange) -> [LineRange] {
        var result: [LineRange] = []
        for (i, lineStart) in lines.enumerated() {
            let lineEnd = i == lines.count - 1 ? 
                src.endIndex : 
                lines[i + 1]

            var startColumn = 0
            var endColumn = 0
            let startInRange = lineStart <= range.lowerBound && lineEnd > range.lowerBound
            let endInRange = lineStart < range.upperBound && lineEnd >= range.upperBound
            if startInRange || endInRange {
                if startInRange {
                    var index = lineStart
                    while index != range.lowerBound {
                        src.formIndex(after: &index)
                        startColumn += 1
                    }
                }
                if endInRange {
                    var index = lineStart
                    while index != range.upperBound {
                        src.formIndex(after: &index)
                        endColumn += 1
                    }
                }
            } else if !(range.lowerBound < lineStart && range.lowerBound > lineEnd) {
                continue
            }

            result.append(
                LineRange(
                    line: i,
                    startColumn: startColumn,
                    endColumn: endColumn
                )
            )
        }
        return result
    }
}
