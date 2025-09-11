package common.extensions

import java.nio.file.Path

extension (self: Path)
    def +(other: Path) = self.resolve(other)
    def +(other: String) = self.resolve(other)
