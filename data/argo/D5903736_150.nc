CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:50Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121041150  20190604094021  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @ױ�R}4�1   @ױ��Х@2��Q��d7dZ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�{D�fD�<�D�q�D��3D� D�B�D�i�D�ҏD���D�:�D��=D�ƸD��fD���DڎD���D��D�8�D�p�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @}p�@��R@��RAA?\)A_\)A\)A��A��A��A��AϮA߮A�B =pB�
B�
B�
B�
B'�
B/�
B7�
B?�
BG�
BO�
BW�
B_�
Bg�
Bo�
Bx=pB�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CV]CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDy��D�D�;�D�pRD���D��D�AGD�h�D��GD���D�9GD���D��pD��D��RDڌ�D��{D��GD�7\D�o\D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�
=A�JA�
=A�1A�%A�%A��
A֕�A�dZA�1'A� �A��A��A�oA�JA�
=A�A���A���AՉ7A�
=A�M�Aӕ�A�K�A���A�n�A�oAѕ�A�VA�E�A�ĜA͡�Aͧ�A�dZA̰!A˰!A�5?A�jA�x�A��/A���AƸRAƇ+AƑhAť�A�v�A�ĜA��#A�A�9XA�A�G�A���A�hsA��
A�;dA��-A�  A�/A��jA���A�{A�dZA�M�A�^5A�K�A�ffA��A�|�A��!A�S�A�ȴA��jA�A��DA���A���A�|�A���A���A���A���A�jA� �A���A���A�ZA�K�A�Q�A��A��mA�ffA�oA�dZA�M�A��A�33A�bA�K�A���A�-A�XA���A��!A���A�r�A��A}K�A{�#AzjAx(�AsƨAqVAp{AoS�An��Am�AjffAh�RAd1Aa��A`�A`ffA_��A^��A\��AYoAVv�AUK�AS�hAQhsAPI�AOO�AM/AK�wAJz�AG�AE�ADn�AC��ABA�A@�A=�TA<��A<I�A:��A:-A9dZA7A7�A6��A5��A4�`A3&�A1��A1%A/ƨA.{A-�A,bNA*�A*��A)��A)VA(r�A'�
A'\)A&��A&$�A%�-A#�#A"�A!;dA �+A I�At�AE�AbA|�AVA;dA(�A&�A1A1A=qAr�Ap�AA�9A��A`BA5?A��A�yA��A��A�uA  A�A33A	A\)A��An�A�jA��AbAS�A ��A �@��@��+@��@�b@��+@�@��@���@�^5@�b@�R@��@�o@�ff@��@���@�v�@���@��`@�
=@��
@���@��T@���@�7L@�O�@��@�z�@�-@�%@���@�bN@�l�@Ұ!@�`B@�M�@͑h@��@�&�@̃@�ȴ@�hs@ȣ�@�1@Ǖ�@�"�@Ɵ�@�{@�7L@ļj@�Q�@�  @Õ�@�l�@��@�ȴ@��@�{@���@�\)@��;@�(�@��@���@�{@��h@���@�b@�33@�{@��@���@� �@��@�
=@��+@��#@�O�@��`@�j@��@�dZ@�ȴ@�v�@�^5@�=q@��@��^@�/@��`@���@�  @��@���@���@��h@�x�@�X@��@��`@���@��
@�|�@��!@��T@�hs@�bN@��m@���@�;d@���@�E�@��h@�%@��@��u@�z�@�r�@�j@�bN@�Z@� �@�dZ@�ȴ@��@���@��T@���@��7@�G�@�7L@���@���@���@�z�@�I�@�b@�1@���@�  @���@��m@��P@�K�@�C�@�33@�@���@��@�@��7@�`B@�7L@�Ĝ@���@��D@�bN@�1'@��@�  @���@�S�@�
=@��R@���@���@�~�@��@���@�hs@���@�V@�V@�V@��j@��j@��j@���@���@���@��w@��@�S�@�ȴ@��+@�V@��@���@�G�@�%@��@��9@�bN@�1'@��@���@��w@�o@���@��@�ȴ@��\@�E�@��@�J@���@���@���@�V@��j@��/@�r�@�1@��
@��;@��w@��@��@�K�@��\@���@��^@���@�@��@��#@��-@�p�@�G�@�%@���@�1'@�ƨ@�;d@��!@�5?@�V@�=q@�-@�-@�5?@��@�J@��@��^@��@�`B@�O�@�Ĝ@�Z@�b@���@���@�\)@��@��R@�=q@���@�G�@�/@��@�X@���@��9@���@�r�@��@�dZ@�33@���@�~�@�M�@�{@�=q@��^@�O�@�&�@H�@v�H@o�@g�}@]�S@R��@J�@Cl�@<�@5u�@0�@*�@%��@�@�@a|@�:@Ft@��@	�h@ �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A�
=A�JA�
=A�1A�%A�%A��
A֕�A�dZA�1'A� �A��A��A�oA�JA�
=A�A���A���AՉ7A�
=A�M�Aӕ�A�K�A���A�n�A�oAѕ�A�VA�E�A�ĜA͡�Aͧ�A�dZA̰!A˰!A�5?A�jA�x�A��/A���AƸRAƇ+AƑhAť�A�v�A�ĜA��#A�A�9XA�A�G�A���A�hsA��
A�;dA��-A�  A�/A��jA���A�{A�dZA�M�A�^5A�K�A�ffA��A�|�A��!A�S�A�ȴA��jA�A��DA���A���A�|�A���A���A���A���A�jA� �A���A���A�ZA�K�A�Q�A��A��mA�ffA�oA�dZA�M�A��A�33A�bA�K�A���A�-A�XA���A��!A���A�r�A��A}K�A{�#AzjAx(�AsƨAqVAp{AoS�An��Am�AjffAh�RAd1Aa��A`�A`ffA_��A^��A\��AYoAVv�AUK�AS�hAQhsAPI�AOO�AM/AK�wAJz�AG�AE�ADn�AC��ABA�A@�A=�TA<��A<I�A:��A:-A9dZA7A7�A6��A5��A4�`A3&�A1��A1%A/ƨA.{A-�A,bNA*�A*��A)��A)VA(r�A'�
A'\)A&��A&$�A%�-A#�#A"�A!;dA �+A I�At�AE�AbA|�AVA;dA(�A&�A1A1A=qAr�Ap�AA�9A��A`BA5?A��A�yA��A��A�uA  A�A33A	A\)A��An�A�jA��AbAS�A ��A �@��@��+@��@�b@��+@�@��@���@�^5@�b@�R@��@�o@�ff@��@���@�v�@���@��`@�
=@��
@���@��T@���@�7L@�O�@��@�z�@�-@�%@���@�bN@�l�@Ұ!@�`B@�M�@͑h@��@�&�@̃@�ȴ@�hs@ȣ�@�1@Ǖ�@�"�@Ɵ�@�{@�7L@ļj@�Q�@�  @Õ�@�l�@��@�ȴ@��@�{@���@�\)@��;@�(�@��@���@�{@��h@���@�b@�33@�{@��@���@� �@��@�
=@��+@��#@�O�@��`@�j@��@�dZ@�ȴ@�v�@�^5@�=q@��@��^@�/@��`@���@�  @��@���@���@��h@�x�@�X@��@��`@���@��
@�|�@��!@��T@�hs@�bN@��m@���@�;d@���@�E�@��h@�%@��@��u@�z�@�r�@�j@�bN@�Z@� �@�dZ@�ȴ@��@���@��T@���@��7@�G�@�7L@���@���@���@�z�@�I�@�b@�1@���@�  @���@��m@��P@�K�@�C�@�33@�@���@��@�@��7@�`B@�7L@�Ĝ@���@��D@�bN@�1'@��@�  @���@�S�@�
=@��R@���@���@�~�@��@���@�hs@���@�V@�V@�V@��j@��j@��j@���@���@���@��w@��@�S�@�ȴ@��+@�V@��@���@�G�@�%@��@��9@�bN@�1'@��@���@��w@�o@���@��@�ȴ@��\@�E�@��@�J@���@���@���@�V@��j@��/@�r�@�1@��
@��;@��w@��@��@�K�@��\@���@��^@���@�@��@��#@��-@�p�@�G�@�%@���@�1'@�ƨ@�;d@��!@�5?@�V@�=q@�-@�-@�5?@��@�J@��@��^@��@�`B@�O�@�Ĝ@�Z@�b@���@���@�\)@��@��R@�=q@���@�G�@�/@��@�X@���@��9@���@�r�@��@�dZ@�33@���@�~�@�M�@�{@�=q@��^@�O�G�O�@H�@v�H@o�@g�}@]�S@R��@J�@Cl�@<�@5u�@0�@*�@%��@�@�@a|@�:@Ft@��@	�h@ �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B{B�B{B{B{B{B{BuBbBDBB
��B
�B
�B
�B
�B
�B
��B�B"�B"�B �BdZB~�B�B�FB�}B��B��B�B�NB�B  B{B�B7LBXBw�B�hB��B�qBŢB��B�B	7B�B�B�B#�B)�B0!B6FB8RB9XB33B(�B�B��B�sB��B��B�JBo�BP�B&�B$�B%�B(�B,B%�B�B1B�fB��B�?B�B�-B�3B��B�-B�LB�}BɺB�XB�JBbNBO�B<jB,B�B{B
��B
�TB
��B
ŢB
�'B
��B
�1B
{�B
o�B
ZB
9XB
�B
�B
VB
%B	��B	�HB	��B	�!B	��B	��B	��B	��B	��B	�+B	k�B	ZB	T�B	D�B	49B	,B	#�B	�B	oB	
=B	B��B��B��B�B�B�B�B�B�B�yB�yB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�B�B�B�BB�B��B��BɺBƨB��B�wB�jB�XB�LB�3B�!B�'BB��B��B��BȴBŢB�wB�FB�!B�B�B�B�3B�FB�3B��B��B��B�hB�bB�PB�1B�B|�B}�B|�By�Bv�Bs�Bq�Bo�Bo�Bo�Bn�Bm�Bl�Bm�Bn�Bo�Bt�Bt�Bt�Bt�By�Bz�Bz�Bw�B{�B|�B~�B�DB�\B�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�3B�FB�XB�wB��BȴBɺBȴBȴB��B��B�B�B�)B�5B�;B�HB�NB�B�B��B��B��B��B��B	B	1B	
=B	JB	PB	\B	hB	oB	�B	�B	�B	�B	 �B	#�B	$�B	$�B	$�B	%�B	(�B	,B	-B	-B	-B	.B	.B	.B	/B	1'B	5?B	33B	2-B	33B	33B	2-B	2-B	49B	5?B	8RB	:^B	:^B	;dB	;dB	;dB	<jB	<jB	<jB	=qB	?}B	C�B	H�B	I�B	I�B	J�B	L�B	N�B	N�B	P�B	R�B	S�B	VB	W
B	YB	ZB	ZB	[#B	[#B	\)B	aHB	e`B	ffB	gmB	hsB	k�B	m�B	p�B	q�B	s�B	t�B	x�B	z�B	{�B	|�B	~�B	� B	�B	�B	�DB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�9B	�LB	�XB	�jB	��B	��B	B	B	B	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�;B	�HB	�HB	�HB	�NB	�NB	�TB	�`B	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
+B
%B
B
B
%B
+B
DB
DB
JB
PB
PB
JB
DB
DB

=B

=B

=B
DB
JB
DB

=B
DB
�B
�B
~B
)�B
2�B
<�B
IB
MjB
Q�B
[�B
^�B
c:B
g�B
m�B
rB
v�B
x�B
{�B
�B
��B
�?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B
�B^B
� B
��B
��B
��B
��B
��B
�4B�B"B"B 
Bc�B~=B�VB��B��B�B�B�XB�B��B�@B�B�B6�BWSBwB��B�B��B��B��B��ByB�B�B�B#B)=B/cB5�B7�B8�B2nB(5B�B�1B�B�>B�(B��Bn�BP&B&.B$B%"B(8B+IB%'B�BsB�B�B��B�cB�qB�zB�=B�pB��B��B��B��B��Ba�BO#B;�B+OB B�B
�+B
�B
�EB
��B
�pB
��B
�wB
{/B
n�B
YfB
8�B
�B
�B
�B
jB	�B	��B	�?B	�hB	�B	��B	��B	��B	��B	�sB	j�B	YfB	TFB	C�B	3�B	+TB	#"B	�B	�B		�B	 PB�%B�B�B� B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�
B�B�B� B�"B�B�B�B��B��BߍB�`B�#B�	B�B��B��B��B��B��B��B�}B�nB�oB��B�6B�B�	B��B��B��B��B�kB�[B�QB�VB�|B��B�~B�3B�B��B��B��B��B�xB�RB|8B}<B|8By'BvBsBp�Bn�Bn�Bn�Bm�Bl�Bk�Bl�Bm�Bn�Bt	BtBtBtBy&Bz*Bz+BwB{3B|9B~CB��B��B��B��B��B��B�
B�)B�.B�B�B�B�B�B�B�B�B�*B�)B�'B�(B�+B�5B�EB�NB�ZB�mB�|B��B��B��B��B��B�B��B��B�B�,B�OB�_B�uB݀BކB��B�B��B��B�B�B�$B�3B�>B	ZB	zB		�B	�B	�B	�B	�B	�B	�B	�B	�B	B	 B	#B	$$B	$(B	$&B	%,B	(>B	+QB	,WB	,UB	,[B	-^B	-^B	-^B	.dB	0rB	4�B	2~B	1wB	2}B	2B	1vB	1zB	3�B	4�B	7�B	9�B	9�B	:�B	:�B	:�B	;�B	;�B	;�B	<�B	>�B	B�B	G�B	IB	IB	JB	LB	N#B	N#B	P/B	R=B	SBB	UNB	VZB	X^B	YgB	YhB	ZnB	ZjB	[tB	`�B	d�B	e�B	f�B	g�B	j�B	l�B	o�B	p�B	sB	tB	xB	z-B	{3B	|5B	~EB	LB	�WB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�RB	�lB	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�$B	�(B	�.B	�4B	�;B	�@B	�AB	�AB	�BB	�`B	�^B	�^B	�lB	�rB	ބB	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�%B	�,B	�*B	�)B	�/B	�-B	� B	�B	�$B	�3B	�:B	�>B	�;B	�<B	�GB
UB
WB
ZB
YB
cB
dB
oB
mB
lB
mB
mB
mB
qB
vB
pB
jB
iB
wB
vB

�B

�B
�B
�B
�B
�B

�B

�B
	�B
	�B
	�B

�B
�B

�B
	�G�O�B
+B
6B
�B
)EB
2GB
;�B
HMB
L�B
Q6B
[	B
^6B
b�B
g;B
mHB
q]B
vB
xB
{2B
~�B
�>B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0.0001), vertically averaged dS =-0.001(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940212019060409402120190604094021  AO  ARCAADJP                                                                    20181121041150    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041150  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041150  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094021  IP                  G�O�G�O�G�O�                