package common.extensions

import java.nio.file.{Path, Paths}

extension (self: String)
  def asPath: Path = Paths.get(self)
