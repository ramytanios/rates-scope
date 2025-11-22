package frontend

import java.util.UUID

trait View:

  self: ff4s.Dsl[State, Action] =>

  import html.*

  def truncateUUID(uuid: UUID): String = uuid.toString.split("-").head

  val view =
    useState: state =>
      div(
        cls := "bg-zinc-100 font-mono w-screen h-screen flex flex-col gap-4 items-center justify-center overflow-y-auto"
      )
