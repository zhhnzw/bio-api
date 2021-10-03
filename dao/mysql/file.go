package mysql

type File struct {
	Id       int     `json:"id"`
	FileName string  `json:"fileName" form:"fileName"`
	Output   string  `json:"output" form:"output"`
	Scene    string  `json:"scene" form:"scene"`
	Path     string  `json:"path" form:"path"` // 形如：uid213123/file, uid213123/img
	Domain   string  `json:"domain"`
	Md5      string  `json:"md5"`
	Mtime    float32 `json:"mtime"`
	RetCode  int     `json:"retcode"`
	RetMsg   string  `json:"retmsg"`
	Scenes   string  `json:"scenes"`
	Size     int     `json:"size"`
	Src      string  `json:"src"`
	Url      string  `json:"url"`
}
