package fpfinal.fakes

import fpfinal.app.LiveController
import fpfinal.service.{LiveExpenseService, LivePersonService}

trait FakeEnv
    extends LiveExpenseService
    with LivePersonService
    with FakeConsole
    with LiveController
