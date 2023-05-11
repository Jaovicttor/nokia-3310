module DB.Init where

import DB.Models.Alarm
import DB.Models.Call
import DB.Models.Chip
import DB.Models.Contact
import DB.Models.Event
import DB.Models.Message


init :: IO()
init = do
    createChips
    createAlarms
    createContacts
    createEvents
    createMessages
    createCalls 