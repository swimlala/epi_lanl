CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-24T19:15:52Z AOML 3.0 creation; 2016-05-31T19:14:45Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151024191552  20160531121446  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_128                   2C  D   APEX                            5368                            041511                          846 @�yS
�1   @�yS�+F@3� ě���dr�1'1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBo��Bw��B��B���B�  B�33B�33B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$y�D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�DyffD�  D�9�D��3D��3D�3D�I�D��fD��fD�3D�0 D���D��3D�	�D�9�D�|�D�ɚD��D�6fD�3D�Ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
B=pB�
B�
B'�
B/�
B7�
B?�
BG�
BO�
BW�
B_�
Bh=pBop�Bwp�Bp�B��RB��B��B��B��B��B��B��B��RB��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$wD$�D%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDtwDyc�D��D�8RD���D���D��D�HRD��D��D��D�.�D���D���D�RD�8RD�{�D��RD��D�5D��D�E11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A�l�A�v�A�Q�A�1A�t�A�G�A�5?A�+A�+A� �A�{A��/A�(�AڋDA�K�A��/A�^5A���A؏\A�oA��yA���A׬A׍PA�9XA�ĜA�VA��HA�I�A�ZA�VA�`BA�A�A�VA�p�A�{A��A�bNA�A�A�A�/A�VA�jA���A�?}A��TA��A��
A�hsA��A�VA��`A�ZA�K�A� �A�&�A��HA�"�A���A�33A���A�JA�5?A���A�{A��A�9XA��A���A��!A�Q�A���A��!A��\A�G�A��7A�ƨA���A�n�A���A��A�|�A���A��;A�ĜA�jA��RA�-A�7LA�;dA�$�A���A��A���A��A�C�A�ȴA�ĜA�t�A�VA���A~�uAz�jAx�Avn�Asx�AqAp$�Am�FAlQ�AkAj9XAi�mAh�DAgAeAchsAap�A`v�A^��A]O�A\VA[��AYt�AX1'AVE�ATffAO�ALffAJ~�AH�AE��AC�#AB~�AA��AA�A@E�A>�`A<�A;&�A:�A9x�A8jA6��A4{A3�A2r�A1��A0��A.�A.=qA.1A-�FA,v�A+|�A*�RA(�9A'`BA&bNA%t�A#�A#dZA"�RA!dZA JA��A�A��A~�A�wA��AC�A��A�HA1'A�AA�A?}A�9AE�A7LA��AE�A�A33A�AJA��A�-A&�AoA�A5?A�A
=A
�uA	�;A�A��A�/A��A�7AȴAI�A��A|�A+A �A ��@���@���@��D@��@�?}@���@���@�n�@�$�@�@��@�@���@���@���@�r�@��@땁@�\@�&�@��@�@��`@��@���@���@�A�@��;@�X@�~�@���@ו�@�"�@�5?@�/@��;@���@�G�@�9X@�+@�E�@�j@��
@�\)@��@���@�=q@�G�@�Q�@�dZ@Ƨ�@�hs@ă@å�@�@���@�bN@��w@��\@�-@��^@�O�@���@�A�@�1'@�ƨ@�l�@�+@�K�@��w@���@�b@�b@�Q�@�Q�@�(�@��@��@�(�@�1'@��@��H@���@��+@�^5@�-@��@��9@�(�@��
@�C�@�~�@�E�@�{@�X@�z�@��@�l�@�@�~�@�-@��@���@�p�@�?}@��/@���@�bN@�I�@�(�@��
@�\)@��y@�^5@�$�@�V@�M�@���@�`B@�%@�bN@���@��m@��@��;@�C�@�S�@�K�@�K�@�S�@�S�@�33@�n�@��@���@���@��@���@���@���@�|�@���@���@�n�@�J@��@��#@��@��j@�j@�9X@��
@�t�@�K�@�33@�"�@�ȴ@�n�@��@���@�hs@���@��j@�r�@�1@��
@��w@�dZ@���@��\@�M�@�{@��^@�O�@�&�@��@���@�z�@��@��@���@���@�dZ@�;d@�@��@���@���@�^5@�J@�@��h@�?}@���@��`@���@���@��@�t�@�K�@��@��+@�n�@�$�@���@�X@��@�Ĝ@��u@�A�@���@�@�;d@�  @��@��
@��w@�|�@���@�o@��@�ȴ@���@��+@�ff@�5?@��@�@�p�@��@��@���@��9@�bN@�1'@�b@�ƨ@���@�K�@�+@��R@�5?@�J@��#@���@��h@��h@��h@�x�@�7L@��`@��@� �@�ƨ@�t�@�
=@��@���@�M�@�J@���@�hs@�G�@�/@��@���@��u@�I�@��@��@��m@��;@���@�l�@���@���@�5?@��@�S�@{"�@n�R@g�@_+@X�9@PQ�@G�P@B=q@;33@4��@.$�@)�@%��@ �u@�@�u@�/@ �@�@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�p�A�l�A�v�A�Q�A�1A�t�A�G�A�5?A�+A�+A� �A�{A��/A�(�AڋDA�K�A��/A�^5A���A؏\A�oA��yA���A׬A׍PA�9XA�ĜA�VA��HA�I�A�ZA�VA�`BA�A�A�VA�p�A�{A��A�bNA�A�A�A�/A�VA�jA���A�?}A��TA��A��
A�hsA��A�VA��`A�ZA�K�A� �A�&�A��HA�"�A���A�33A���A�JA�5?A���A�{A��A�9XA��A���A��!A�Q�A���A��!A��\A�G�A��7A�ƨA���A�n�A���A��A�|�A���A��;A�ĜA�jA��RA�-A�7LA�;dA�$�A���A��A���A��A�C�A�ȴA�ĜA�t�A�VA���A~�uAz�jAx�Avn�Asx�AqAp$�Am�FAlQ�AkAj9XAi�mAh�DAgAeAchsAap�A`v�A^��A]O�A\VA[��AYt�AX1'AVE�ATffAO�ALffAJ~�AH�AE��AC�#AB~�AA��AA�A@E�A>�`A<�A;&�A:�A9x�A8jA6��A4{A3�A2r�A1��A0��A.�A.=qA.1A-�FA,v�A+|�A*�RA(�9A'`BA&bNA%t�A#�A#dZA"�RA!dZA JA��A�A��A~�A�wA��AC�A��A�HA1'A�AA�A?}A�9AE�A7LA��AE�A�A33A�AJA��A�-A&�AoA�A5?A�A
=A
�uA	�;A�A��A�/A��A�7AȴAI�A��A|�A+A �A ��@���@���@��D@��@�?}@���@���@�n�@�$�@�@��@�@���@���@���@�r�@��@땁@�\@�&�@��@�@��`@��@���@���@�A�@��;@�X@�~�@���@ו�@�"�@�5?@�/@��;@���@�G�@�9X@�+@�E�@�j@��
@�\)@��@���@�=q@�G�@�Q�@�dZ@Ƨ�@�hs@ă@å�@�@���@�bN@��w@��\@�-@��^@�O�@���@�A�@�1'@�ƨ@�l�@�+@�K�@��w@���@�b@�b@�Q�@�Q�@�(�@��@��@�(�@�1'@��@��H@���@��+@�^5@�-@��@��9@�(�@��
@�C�@�~�@�E�@�{@�X@�z�@��@�l�@�@�~�@�-@��@���@�p�@�?}@��/@���@�bN@�I�@�(�@��
@�\)@��y@�^5@�$�@�V@�M�@���@�`B@�%@�bN@���@��m@��@��;@�C�@�S�@�K�@�K�@�S�@�S�@�33@�n�@��@���@���@��@���@���@���@�|�@���@���@�n�@�J@��@��#@��@��j@�j@�9X@��
@�t�@�K�@�33@�"�@�ȴ@�n�@��@���@�hs@���@��j@�r�@�1@��
@��w@�dZ@���@��\@�M�@�{@��^@�O�@�&�@��@���@�z�@��@��@���@���@�dZ@�;d@�@��@���@���@�^5@�J@�@��h@�?}@���@��`@���@���@��@�t�@�K�@��@��+@�n�@�$�@���@�X@��@�Ĝ@��u@�A�@���@�@�;d@�  @��@��
@��w@�|�@���@�o@��@�ȴ@���@��+@�ff@�5?@��@�@�p�@��@��@���@��9@�bN@�1'@�b@�ƨ@���@�K�@�+@��R@�5?@�J@��#@���@��h@��h@��h@�x�@�7L@��`@��@� �@�ƨ@�t�@�
=@��@���@�M�@�J@���@�hs@�G�@�/@��@���@��u@�I�@��@��@��m@��;@���@�l�@���@���@�5?@��@�S�@{"�@n�R@g�@_+@X�9@PQ�@G�P@B=q@;33@4��@.$�@)�@%��@ �u@�@�u@�/@ �@�@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B{B{BuBuB{B�B�B�B&�B6FB2-BO�B�1B�'B��B�B�HB�fB�yB�B��BB�BJ�B�B��B�B�!B�-B�'B��B��B��B��B�hB�Bp�B\)BH�B7LB)�B�BuBPB+B��B��B�B�B��B�B�=Bm�B[#BA�BB�B�B�VB_;B7LB�B�B#�B+B&�B�BB�B�HB�jB�9B��B��B��B��B�DBŢB�B�ZB�/BĜB��B�\B� Bn�BXBE�BO�B=qB$�BB
�B
��B
�LB
�FB
�9B
��B
�\B
q�B
[#B
E�B
-B
!�B
�B
B	��B	�B	�yB	�fB	�B	��B	��B	�'B	��B	��B	�\B	�+B	�B	x�B	jB	aHB	T�B	C�B	.B	�B	oB		7B��B��B�B�B�yB�`B�;B�B��B��B��BǮBB�qB�^B�RB�FB�3B�!B�B�B�B��B��B��B��B��B��B��B��B�uB�hB�VB�JB�=B�DB�VB��B��B��B�hB�VB�PB�JB�JB�JB�PB�JB�7B�B~�B}�B{�B{�B{�B{�Bz�Bz�Bz�By�By�By�By�By�By�Bx�Bx�Bx�Bx�Bx�B{�B|�B{�B|�B|�B{�B{�Bz�Bz�B|�Bz�Bv�Bv�Bw�Bx�Bz�B{�B{�Bz�Bz�Bz�B�B�B�B�%B�+B�+B�%B�1B�1B�+B�B~�B~�B|�Bz�Bv�Bt�Bv�Bw�By�By�By�Bz�Bz�B{�B|�B~�B~�B�B�B�B�B�B�B�B�%B�7B�JB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�LB�dB�}BĜBɺB��B��B��B�B�B�B�B�HB�fB�B�B��B��B��B	  B	B		7B	JB	PB	VB	\B	hB	uB	�B	�B	�B	�B	�B	�B	'�B	)�B	-B	.B	.B	.B	/B	/B	0!B	49B	7LB	;dB	A�B	B�B	D�B	F�B	D�B	E�B	G�B	K�B	N�B	O�B	S�B	VB	VB	W
B	XB	XB	XB	XB	YB	ZB	]/B	^5B	`BB	e`B	iyB	jB	o�B	r�B	t�B	v�B	w�B	y�B	|�B	~�B	� B	�B	�B	�7B	�DB	�DB	�PB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�?B	�LB	�RB	�XB	�dB	�qB	�wB	�}B	��B	B	B	ÖB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�;B	�BB	�BB	�HB	�NB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
1B
1B
	7B

=B
DB
PB
hB
�B
$�B
+B
1'B
0!B
6FB
?}B
C�B
J�B
P�B
XB
]/B
cTB
hsB
k�B
o�B
r�B
v�B
{�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�BB�BxBzB�B�B�B�B&�B6JB2/BO�B�4B�*B��B�B�LB�kB�B�B��B#B�BJ�B�B��B�B�)B�7B�1B��B��B��B��B�uB�(Bp�B\2BH�B7RB*B�B�BUB/B��B��B�B�B�B�B�?Bm�B['BA�BB�B�"B�[B_BB7QB�B�B#�B+B&�B�BB�B�LB�oB�<B��B��B��B��B�IBŦB�B�]B�1BĠB��B�_B�Bn�BXBE�BO�B=vB$�B#B
�B
��B
�UB
�MB
�BB
��B
�cB
q�B
[.B
E�B
-B
!�B
�B
,B	��B	�B	�B	�uB	�B	��B	��B	�7B	��B	��B	�jB	�:B	�B	x�B	j�B	aZB	UB	C�B	.'B	�B	�B		LB��B��B�B�B�B�xB�OB�-B�B��B��B��B¨B��B�xB�kB�^B�KB�9B�,B�.B�!B�B�B��B��B��B��B��B��B��B��B�qB�fB�YB�_B�qB��B��B��B��B�rB�jB�eB�cB�dB�kB�eB�QB�'BB~B|B| B|B| Bz�Bz�Bz�By�By�By�By�By�By�Bx�Bx�Bx�Bx�Bx�B|B}B| B}	B}
B|B|Bz�Bz�B}	Bz�Bv�Bv�Bw�Bx�Bz�B|B|Bz�Bz�Bz�B�!B�4B�6B�?B�DB�FB�>B�JB�KB�EB�3BBB}Bz�Bv�Bt�Bv�Bw�By�By�By�Bz�Bz�B|B}BBB�B�!B� B� B�"B�%B�3B�>B�QB�aB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�,B�PB�dB�{B��BĴB��B��B��B��B�B�3B�-B�,B�]B�yB��B��B��B��B�	B	 B	%B		JB	\B	dB	kB	qB	}B	�B	�B	�B	�B	�B	�B	�B	(B	*B	-!B	.)B	.'B	.&B	/,B	/+B	04B	4KB	7^B	;xB	A�B	B�B	D�B	F�B	D�B	E�B	G�B	K�B	N�B	O�B	TB	VB	VB	WB	X B	X#B	X"B	XB	Y'B	Z0B	]?B	^FB	`RB	epB	i�B	j�B	o�B	r�B	t�B	v�B	w�B	y�B	|�B	B	�B	�B	�.B	�GB	�SB	�UB	�`B	�iB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�4B	�<B	�JB	�ZB	�`B	�eB	�sB	��B	��B	��B	��B	B	B	äB	ŮB	ǾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�
B	�B	�GB	�QB	�QB	�WB	�[B	�rB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	� B	�B	�B	�B
 B
B
B
B
 B
!B
B
B
 B
$B
B
B
&B
-B
1B
/B
8B
?B
=B
;B
=B
=B
=B
	CB

IB
QB
[B
sB
�B
$�B
+B
10B
0.B
6RB
?�B
C�B
J�B
P�B
XB
]9B
c^B
h}B
k�B
o�B
r�B
v�B
{�B
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214462016053112144620160531121446  AO  ARCAADJP                                                                    20151024191552    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151024191552  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151024191552  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121446  IP                  G�O�G�O�G�O�                