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
        }
      },
      SignatureMethod:{
        _attrs: {
          Algorithm: Text
        },
        pcdata: Text
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
        _attrs: { nItem: Int },
        imposto: {
          COFINS: {
            COFINSOutr: {
              CST: { pcdata: Text },
              qBCProd: { pcdata: Double },
              vAliqProd: { pcdata: Double },
              vCOFINS: { pcdata: Double }
            }
          },
          ICMS: {
            ICMS00: {
              CST: { pcdata: Text },
              modBC: { pcdata: Text },
              orig: { pcdata: Text },
              pICMS: { pcdata: Double },
              vBC: { pcdata: Double },
              vICMS: { pcdata: Double }
            }
          },
          PIS: {
            PISOutr: {
              CST: { pcdata: Text },
              pPIS: { pcdata: Double },
              vBC: { pcdata: Double },
              vPIS: { pcdata: Double }
            }
          },
          vTotTrib: { pcdata: Double }
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
          vProd: { pcdata: Double },
          vUnCom: { pcdata: Double },
          vUnTrib: { pcdata: Double },
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
        CEP: Text,
        CPF: Text,
        UF: Text,
        cMun: Text,
        cPais: Text,
        nro: Text,
        xBairro: Text,
        xLgr: Text,
        xMun: Text,
        xNome: Text,
        xPais: Text
      },
      ide: {
        cDV: Int,
        cMunFG: Text,
        cNF: Text,
        cUF: Int,
        dhEmi: Text,
        dhSaiEnt: Text,
        finNFe: Int,
        idDest: Int,
        indFinal: Int,
        indIntermed: Int,
        indPres: Int,
        mod: Int,
        nNF: Int,
        natOp: Text,
        procEmi: Int,
        serie: Int,
        tpAmb: Int,
        tpEmis: Int,
        tpImp: Int,
        tpNF: Int,
        verProc: Text
      },
      infAdic: {
        infCpl: Text,
        obsCont: {
          xCampo: Text,
          xTexto: Text
        }
      },
      infRespTec: {
        CNPJ: Text,
        email: Text,
        fone: Text,
        xContato: Text
      },
      pag: {
        detPag: {
          indPag: Int,
          tPag: Int,
          vPag: Double
      },
        vTroco: Double
      },
      total: {
        ICMSTot: {
          vBC: Double,
          vBCST: Double,
          vCOFINS: Double,
          vDesc: Double,
          vFCP: Double,
          vFCPST: Double,
          vFCPSTRet: Double,
          vFrete: Double,
          vICMS: Double,
          vICMSDeson: Double,
          vII: Double,
          vIPI: Double,
          vIPIDevol: Double,
          vNF: Double,
          vOutro: Double,
          vPIS: Double,
          vProd: Double,
          vST: Double,
          vSeg: Double,
          vTotTrib: Double
        }
      },
      transp: {
        modFrete: Int
      }
    }
  },
  _attrs:{
      xmlns: Text,
      versao: Text
  },
  protNFe:{
    a: Int
  }
}
}
|]