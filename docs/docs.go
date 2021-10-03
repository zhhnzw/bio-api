// GENERATED BY THE COMMAND ABOVE; DO NOT EDIT
// This file was generated by swaggo/swag

package docs

import (
	"bytes"
	"encoding/json"
	"strings"

	"github.com/alecthomas/template"
	"github.com/swaggo/swag"
)

var doc = `{
    "schemes": {{ marshal .Schemes }},
    "swagger": "2.0",
    "info": {
        "description": "{{.Description}}",
        "title": "{{.Title}}",
        "termsOfService": "http://swagger.io/terms/",
        "contact": {
            "name": "zhhnzw",
            "url": "http://www.swagger.io/support",
            "email": "2804696160@qq.com"
        },
        "license": {
            "name": "Apache 2.0",
            "url": "http://www.apache.org/licenses/LICENSE-2.0.html"
        },
        "version": "{{.Version}}"
    },
    "host": "{{.Host}}",
    "basePath": "{{.BasePath}}",
    "paths": {
        "/group1/{userName}/{fileName}": {
            "get": {
                "description": "每个用户只能下载自己上传的文件",
                "tags": [
                    "file"
                ],
                "summary": "下载文件",
                "parameters": [
                    {
                        "type": "string",
                        "description": "userName",
                        "name": "userName",
                        "in": "path",
                        "required": true
                    },
                    {
                        "type": "string",
                        "description": "fileName",
                        "name": "fileName",
                        "in": "path",
                        "required": true
                    }
                ]
            }
        },
        "/v1/login": {
            "post": {
                "consumes": [
                    "application/json"
                ],
                "produces": [
                    "application/json"
                ],
                "tags": [
                    "用户"
                ],
                "summary": "登录",
                "parameters": [
                    {
                        "description": "查询参数",
                        "name": "object",
                        "in": "body",
                        "schema": {
                            "$ref": "#/definitions/v1.UserForm"
                        }
                    }
                ],
                "responses": {
                    "200": {
                        "description": "OK",
                        "schema": {
                            "$ref": "#/definitions/utils.Resp"
                        }
                    }
                }
            }
        },
        "/v1/logout": {
            "post": {
                "security": [
                    {
                        "ApiKeyAuth": []
                    }
                ],
                "consumes": [
                    "application/json"
                ],
                "produces": [
                    "application/json"
                ],
                "tags": [
                    "用户"
                ],
                "summary": "注销",
                "parameters": [
                    {
                        "type": "string",
                        "description": "Cookie",
                        "name": "Authorization",
                        "in": "header"
                    }
                ],
                "responses": {
                    "200": {
                        "description": "OK",
                        "schema": {
                            "$ref": "#/definitions/utils.Resp"
                        }
                    }
                }
            }
        },
        "/v1/upload": {
            "post": {
                "description": "上传数据源文件以供生成统计图表",
                "consumes": [
                    "multipart/form-data"
                ],
                "produces": [
                    "application/json"
                ],
                "tags": [
                    "file"
                ],
                "summary": "上传文件",
                "parameters": [
                    {
                        "type": "file",
                        "description": "文件",
                        "name": "file",
                        "in": "formData",
                        "required": true
                    }
                ],
                "responses": {
                    "200": {
                        "description": "OK",
                        "schema": {
                            "$ref": "#/definitions/utils.Resp"
                        }
                    }
                }
            }
        },
        "/v1/user": {
            "get": {
                "security": [
                    {
                        "ApiKeyAuth": []
                    }
                ],
                "description": "获取用户信息",
                "consumes": [
                    "application/json"
                ],
                "produces": [
                    "application/json"
                ],
                "tags": [
                    "用户"
                ],
                "summary": "获取用户信息",
                "parameters": [
                    {
                        "type": "string",
                        "name": "avatar",
                        "in": "query"
                    },
                    {
                        "type": "string",
                        "name": "email",
                        "in": "query"
                    },
                    {
                        "type": "integer",
                        "name": "id",
                        "in": "query"
                    },
                    {
                        "type": "boolean",
                        "name": "isSuper",
                        "in": "query"
                    },
                    {
                        "type": "boolean",
                        "name": "isValid",
                        "in": "query"
                    },
                    {
                        "type": "string",
                        "name": "mobile",
                        "in": "query"
                    },
                    {
                        "type": "string",
                        "name": "nickName",
                        "in": "query"
                    },
                    {
                        "type": "string",
                        "name": "userName",
                        "in": "query"
                    }
                ],
                "responses": {
                    "200": {
                        "description": "OK",
                        "schema": {
                            "$ref": "#/definitions/utils.Resp"
                        }
                    }
                }
            }
        }
    },
    "definitions": {
        "utils.JSONTime": {
            "type": "object",
            "properties": {
                "time.Time": {
                    "type": "string"
                }
            }
        },
        "utils.Resp": {
            "type": "object",
            "properties": {
                "code": {
                    "type": "string"
                },
                "data": {
                    "type": "object"
                },
                "msg": {
                    "type": "string"
                }
            }
        },
        "v1.UserForm": {
            "type": "object",
            "properties": {
                "avatar": {
                    "type": "string"
                },
                "email": {
                    "type": "string"
                },
                "id": {
                    "type": "integer"
                },
                "isValid": {
                    "type": "boolean"
                },
                "mobile": {
                    "type": "string"
                },
                "nickName": {
                    "type": "string"
                },
                "pageIndex": {
                    "type": "integer"
                },
                "pageSize": {
                    "type": "integer"
                },
                "password": {
                    "type": "string",
                    "example": "f81015fee0b7ad8d472717286c0c7a55"
                },
                "roles": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "desc": {
                                "type": "string"
                            },
                            "id": {
                                "type": "integer"
                            },
                            "name": {
                                "type": "string"
                            }
                        }
                    }
                },
                "userName": {
                    "type": "string",
                    "example": "guest"
                }
            }
        }
    }
}`

type swaggerInfo struct {
	Version     string
	Host        string
	BasePath    string
	Schemes     []string
	Title       string
	Description string
}

// SwaggerInfo holds exported Swagger Info so clients can modify it
var SwaggerInfo = swaggerInfo{
	Version:     "1.0",
	Host:        "127.0.0.1:8000",
	BasePath:    "",
	Schemes:     []string{},
	Title:       "bio-api",
	Description: "后台api服务",
}

type s struct{}

func (s *s) ReadDoc() string {
	sInfo := SwaggerInfo
	sInfo.Description = strings.Replace(sInfo.Description, "\n", "\\n", -1)

	t, err := template.New("swagger_info").Funcs(template.FuncMap{
		"marshal": func(v interface{}) string {
			a, _ := json.Marshal(v)
			return string(a)
		},
	}).Parse(doc)
	if err != nil {
		return doc
	}

	var tpl bytes.Buffer
	if err := t.Execute(&tpl, sInfo); err != nil {
		return doc
	}

	return tpl.String()
}

func init() {
	swag.Register(swag.Name, &s{})
}
