private struct ErrorRange {
    public let range: SourceRange
    public let message: String
}

public struct Error {
    private let message: String
    private let primaryRange: ErrorRange
    private var secondaryRanges: [ErrorRange] = []

    public init(_ message: String, _ primaryRange: SourceRange, _ primaryMessage: String) {
        self.message = message
        self.primaryRange = ErrorRange(range: primaryRange, message: primaryMessage)
    }

    public mutating func adding(range: SourceRange, _ message: String) -> Error {
        secondaryRanges.append(
            ErrorRange(range: range, message: message)
        )
        return self
    }

    public func report(with file: SourceFile) {
        print("\u{001B}[31merror: \(message)")
        fatalError()
    }
}
