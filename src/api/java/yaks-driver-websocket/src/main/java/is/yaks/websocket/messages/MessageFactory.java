package is.yaks.websocket.messages;

import is.yaks.socketfe.Message;
import is.yaks.socketfe.MessageCode;

public class MessageFactory {

	// use the getMessage method to get corresponding message type object
	public Message getMessage(MessageCode msgCode) 
	{
		if( msgCode == null ) {
			return new MessageNone();
		} else if(msgCode.equals(MessageCode.OPEN)) {
			return new MessageOpen();
		} else if(msgCode.equals(MessageCode.CREATE)) {
			return new MessageCreate();
		} else if(msgCode.equals(MessageCode.DELETE)) {
			return new MessageDelete();
		} else if(msgCode.equals(MessageCode.PUT)) {
			return new MessagePut();
		} else if(msgCode.equals(MessageCode.PATCH)) {
			return new MessagePatch();
		} else if(msgCode.equals(MessageCode.GET)) {
			return new MessageGet();
		} else if(msgCode.equals(MessageCode.SUB)) {
			return new MessageSub();
		} else if(msgCode.equals(MessageCode.UNSUB)) {
			return new MessageUnsub();
		} else if(msgCode.equals(MessageCode.NOTIFY)) {
			return new MessageNotify();
		} else if(msgCode.equals(MessageCode.EVAL)) {
			return new MessageEval();
		} else if(msgCode.equals(MessageCode.OK)) {
			return new MessageOk();
		} else if(msgCode.equals(MessageCode.VALUE)) {
			return new MessageValue();
		} else if(msgCode.equals(MessageCode.VALUES)) {
			return new MessageValues();
		} else if(msgCode.equals(MessageCode.ERROR)) {
			return new MessageError();
		} else if(msgCode.equals(MessageCode.NONE)) {
			return new MessageNone();
		} 
		return new MessageInvalid();
	}

}
