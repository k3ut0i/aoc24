instance creation
initFrom: fileNameOrInput
	data := self class getMul: (fileNameOrInput asFileReference contents).