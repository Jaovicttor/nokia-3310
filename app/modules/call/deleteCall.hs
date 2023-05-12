module App.Modules.Call.DeleteCall where
import DB.Models.Call
import DB.Models.Chip

deleteCall::Int -> [Call] -> ([Call], Call)
deleteCall call_id_index calls = do
    let call = calls !! call_id_index
    let chip = myChip
    let is_sender = sented_by call == idChip chip
    let updatedCall = Call {
        cid = cid call,
        started_at = started_at call,
        finished_at = finished_at call,
        answered = answered call,
        sented_by = sented_by call,
        received_by = received_by call,
        has_sender_deleted = is_sender || has_sender_deleted call,
        has_receiver_deleted = not is_sender || has_receiver_deleted call
    }
    (filter (\x -> cid x /= cid call) calls, updatedCall)