module Schemas.RootSchema(RootSchema) where

import Data.Aeson.Schema

type RootSchema = [schema|
{
nfeProc: {
  NFe: {
    Signature: {
      KeyInfo: {
          X509Data: {
              X509Certificate: {
                  pcdata: Text
              }
          }
      },
      SignatureValue: {
          pcdata: Text
      },
      SignedInfo: {
          CanonicalizationMethod: {
              _attrs: {
                  Algorithm: Text
              },
              pcdata: Text
          },
          Reference: {
              DigestMethod: {
                  _attrs: {
                    Algorithm: Text
                  },
                  pcdata: Text
              },
              DigestValue: {
                  pcdata: Text
              },
              Transforms: {
                  Transform: {
                        _attrs: {
                            Algorithm: Text
                        },
                      pcdata: Text
                  }
              },
              _attrs: {
                URI: Text
              }
          },
          SignatureMethod: {
              _attrs: {
                Algorithm: Text
              },
              pcdata: Text
          }
      },
      _attrs: {
        xmlns: Text
      }
    },
    _attrs: {
      xmlns: Text
    },
    infNFe: {
      _attrs: {
        Id: Text,
        versao: Text
      },
      cobr: {
        fat: {
          nFat: { pcdata: Text },
          vLiq: { pcdata: Text },
          vOrig: { pcdata: Text }
        }
      },
      dest: {
        CPF: { pcdata: Text },
        enderDest: {
          CEP: { pcdata: Text },
          UF: { pcdata: Text },
          cMun: { pcdata: Text },
          cPais: { pcdata: Text },
          nro: { pcdata: Text },
          xBairro: { pcdata: Text },
          xLgr: { pcdata: Text },
          xMun: { pcdata: Text },
          xPais: { pcdata: Text }
        },
        indIEDest: { pcdata: Text },
        xNome: {
          pcdata: Text
        }
      },
      det: {
        _attrs: { nItem: Text },
        imposto: {
          COFINS: {
            COFINSOutr: {
              CST: { pcdata: Text },
              qBCProd: { pcdata: Text },
              vAliqProd: { pcdata: Text },
              vCOFINS: { pcdata: Text }
            }
          },
          ICMS: {
            ICMS00: {
              CST: { pcdata: Text },
              modBC: { pcdata: Text },
              orig: { pcdata: Text },
              pICMS: { pcdata: Text },
              vBC: { pcdata: Text },
              vICMS: { pcdata: Text }
            }
          },
          PIS: {
            PISOutr: {
              CST: { pcdata: Text },
              pPIS: { pcdata: Text },
              vBC: { pcdata: Text },
              vPIS: { pcdata: Text }
            }
          },
          vTotTrib: { pcdata: Text }
        },
        prod: {
          CFOP: { pcdata: Text },
          NCM: { pcdata: Text },
          cEAN: { pcdata: Text },
          cEANTrib: { pcdata: Text },
          cProd: { pcdata: Text },
          indTot: { pcdata: Text },
          qCom: { pcdata: Text },
          qTrib: { pcdata: Text },
          uCom: { pcdata: Text },
          uTrib: { pcdata: Text },
          vProd: { pcdata: Text },
          vUnCom: { pcdata: Text },
          vUnTrib: { pcdata: Text },
          xProd: { pcdata: Text }
        }
      },
      emit: {
        CNPJ: { pcdata: Text },
        CRT: { pcdata: Text },
        IE: { pcdata: Text },
        enderEmit: {
          CEP: { pcdata: Text },
          UF: { pcdata: Text },
          cMun: { pcdata: Text },
          cPais: { pcdata: Text },
          fone: { pcdata: Text },
          nro: { pcdata: Text },
          xBairro: { pcdata: Text },
          xLgr: { pcdata: Text },
          xMun: { pcdata: Text },
          xPais: { pcdata: Text }
        },
        xNome: { pcdata: Text }
      },
      entrega: {
        CEP: {pcdata: Text},
        CPF: {pcdata: Text},
        UF: {pcdata: Text},
        cMun: {pcdata: Text},
        cPais: {pcdata: Text},
        nro: {pcdata: Text},
        xBairro: {pcdata: Text},
        xLgr: {pcdata: Text},
        xMun: {pcdata: Text},
        xNome: {pcdata: Text},
        xPais: {pcdata: Text}
      },
      ide: {
        cDV: {pcdata: Text},
        cMunFG: {pcdata: Text},
        cNF: {pcdata: Text},
        cUF: {pcdata: Text},
        dhEmi: {pcdata: Text},
        dhSaiEnt: {pcdata: Text},
        finNFe: {pcdata: Text},
        idDest: {pcdata: Text},
        indFinal: {pcdata: Text},
        indIntermed: {pcdata: Text},
        indPres: {pcdata: Text},
        mod: {pcdata: Text},
        nNF: {pcdata: Text},
        natOp: {pcdata: Text},
        procEmi: {pcdata: Text},
        serie: {pcdata: Text},
        tpAmb: {pcdata: Text},
        tpEmis: {pcdata: Text},
        tpImp: {pcdata: Text},
        tpNF: {pcdata: Text},
        verProc: {pcdata: Text}
      },
      infAdic: {
        infCpl: {pcdata: Text},
        obsCont: {
          _attrs: {
            xCampo: Text
          },
          xTexto: {pcdata: Text}
        }
      },
      infRespTec: {
        CNPJ: {pcdata: Text},
        email: {pcdata: Text},
        fone: {pcdata: Text},
        xContato: {pcdata: Text}
      },
      pag: {
        detPag: {
          indPag: {pcdata: Text},
          tPag: {pcdata: Text},
          vPag: {pcdata: Text}
        },
        vTroco: {pcdata: Text}
      },
      total: {
        ICMSTot: {
          vBC: {pcdata: Text},
          vBCST: {pcdata: Text},
          vCOFINS: {pcdata: Text},
          vDesc: {pcdata: Text},
          vFCP: {pcdata: Text},
          vFCPST: {pcdata: Text},
          vFCPSTRet: {pcdata: Text},
          vFrete: {pcdata: Text},
          vICMS: {pcdata: Text},
          vICMSDeson: {pcdata: Text},
          vII: {pcdata: Text},
          vIPI: {pcdata: Text},
          vIPIDevol: {pcdata: Text},
          vNF: {pcdata: Text},
          vOutro: {pcdata: Text},
          vPIS: {pcdata: Text},
          vProd: {pcdata: Text},
          vST: {pcdata: Text},
          vSeg: {pcdata: Text},
          vTotTrib: {pcdata: Text}
        }
      },
      transp: {
        modFrete: {pcdata: Text}
      }
    }
  },
  _attrs:{
      xmlns: Text,
      versao: Text
  },
  protNFe: {
        _attrs: { versao: Text },
        infProt: {
          cStat: { pcdata: Text },
          chNFe: { pcdata: Text },
          dhRecbto: { pcdata: Text },
          digVal: { pcdata: Text },
          nProt: { pcdata: Text },
          tpAmb: { pcdata: Text },
          verAplic: { pcdata: Text },
          xMotivo: { pcdata: Text }
        }
      }
}
}
|]