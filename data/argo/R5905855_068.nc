CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:22:42Z creation;2022-06-04T19:22:43Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192242  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               DA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�P\�'O1   @�P\��?@-�z�G��c��hr�!1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @&ff@�  @�  A   A   AA��A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx��B~��B�  B�33B�ffB���B���B�  B�  B�  B�ffB���B���B���B�  B�  B�  B�  B�ffB���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"33C$�C%��C(  C*  C,  C.  C0  C2  C4  C5�fC7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX�CZ�C\�C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du�fDv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @#�
@}p�@��R@��RA\)A@��A_\)A}A��A��A��A��AϮA߮A�A��B�
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
Bx��B~��B��B��B�Q�B��B��RB��B��B��B�Q�B��B��RB��RB��B��B��B��B�Q�BǸRB��BϸRB��B��B��B��B��B��B��B��B��B��B��B��RC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C"(�C$]C%C'��C)��C+��C-��C/��C1��C3��C5�)C7�)C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CV]CX]CZ]C\]C]��C_�)Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&��D&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu��Du�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��2A��>A��A��"A���A�MA��A��A��A��A�PA��A��A�#�A�%zA�*eA�.A�/OA�4�A�:*A�>A�>BA�8RA�3�A�0�A�,=A�2aA�=<A�N�AϘ+AЈfAв�AП�AЎ�A�8RA��Aɐ�A��.A�+�A�IA�2�A���A��A��SA��A��A�qAA�x8A��eA��gA�`�A�,A�"�A��CA��A�MA�G�A��hA��+A���A�jA�FA�]�A�xlA���A�)�A��A��EA��]A��aA�<A�FA�9�A�v�A�#nA�!�A��A���A�B[A�-�A��mA��9A�xA�A�A�2aA{0UAq>�Al�Aj��Ah�Ae�tAb��Aa��A`�NA_:�A]h�AY*0AW�AS��AQRTAO��AM�eAKPHAH�zAG��AF��AE�^AC�AA�7A>\)A<$�A9�gA6xlA2]dA3xlA2�bA1��A1�A1�A1C-A0��A/ȴA.SA,?A+a�A+VA+u%A+R�A*�rA*y>A)*0A'�mA&�.A$��A#zxA#�AA$h
A$�_A#(�A!��A �AXyA�A��A/�A{JAs�A�A�zA��A iA*�A�)A��A�Ae�A��Aj�A�AH�A��A��AzAQ�A �A��A�A��A��ACA�SAxA~�A \AH�APHA�DA��A�A&�A�$Ah
A�AE9AJ�A�A($A@OAFtAM�A9XA�5A�PA?}A�A
�]A
-�A	�XA	U�A	�A!-A��A�4Ae�A \ADgA�A�[A�PA/�A��A��A��Al�AAVmA|�A�RAl�A�A�AخA�IAW?A�A Q@��@��$@��K@�G@���@�c@�0�@��@��U@��@���@�H@���@�Z�@��@��@��@�@��6@�a|@�J@�j�@�@��@��}@��@�=�@�Ft@�~�@�(�@�Q@��@�j@�S&@���@�ȴ@��[@��@��@�s�@��@�l"@��@�!-@�A@�V�@�1'@�e@��@�J#@�@��@�;@�_@�[@���@�B[@�s@��v@�>B@�x�@�*0@�ߤ@�S�@�~�@�@�u�@���@��@�x�@�F�@�q@��`@ڔF@�H�@��@���@��#@���@ڶ�@�H�@�.I@ڀ�@ْ:@�+@�(@ع$@�_@�9X@�@֏\@�D�@��a@�x�@��@��@��@���@�0U@�=@�7�@ѡ�@�/�@К�@�l�@���@�%@�E�@ͧ�@���@̚@�J@�q@���@�*�@�c@���@�i�@���@�U�@�7�@œ@�^�@�o@ĸR@��@�L�@¶�@�D�@��@�w2@��@�<�@��@��N@���@�hs@�
=@�K^@���@���@���@���@�x�@�(�@�ff@��
@���@�~�@�g�@�L�@� \@���@���@�E�@�/@��v@�C-@��@�$t@��b@�V�@��Q@� \@�Ĝ@��z@��@���@�2a@���@�6�@��*@�J#@��s@�Xy@��@�ƨ@���@�q@���@�ԕ@�B�@���@�.�@���@��V@�k�@��@�h
@�@�z�@�<�@�+k@� �@���@��@���@�O�@��@��<@�kQ@��@��@��~@��@�Ta@�"h@��n@�\)@��v@���@�3�@���@�c�@�"�@���@�4n@�7@��>@��k@�`B@��@���@�PH@���@��0@���@��@�_p@�J�@�+@��@���@��!@��_@��@�c @�1@�o�@��@�PH@��T@��f@��@��@�_�@�;�@��@�u@��+@��&@���@�RT@��2@��?@��6@���@�GE@�{@��$@�)_@��K@��@��@�5�@���@�Ɇ@�z@�+k@�	@��@�@� �@��D@��g@���@�`B@�E9@��s@�5?@�)�@�	@��^@�rG@�T�@�9�@�@��@��!@�?�@��Z@��@��q@�p�@�C�@�ں@��\@�^5@�3�@��@�|@�8@��8@��h@�p;@�I�@��@���@���@��M@�x@�c�@�X�@�Mj@�F@�@O@�-w@��@�ȴ@���@�W�@� �@��D@���@�@�~(@�u�@�bN@�8�@��@��W@�hs@��@��@��@��=@�@O@�q@�Y@���@���@�҉@�Ɇ@�Ĝ@���@�v�@�V�@�<�@�7@��@��Z@��>@���@���@���@�W?@�:�@�&�@���@��$@��z@�ff@�x@��@@~�!@~1�@}��@}Dg@|�f@|��@|�@|l"@|@{�A@{��@{!-@z�@yL�@xc�@x$@w��@w�@w�@w�@w�@v�8@v��@vp;@v�@uϫ@u��@uO�@t�P@t�)@t�D@sݘ@s i@r��@rL0@q��@p��@pm�@pb@o�0@oqv@oA�@n��@n&�@m�C@m��@mT�@l��@lc�@k˒@j�6@i��@i�T@ip�@h�@h7�@g�q@g�@f��@f �@e��@e��@e5�@d��@c��@cdZ@b�+@b^5@ba|@bc @b;�@a�@a�@a��@`�e@`/�@_�r@_��@_\)@^�x@^-@^�@]o @]�@\�?@\h�@[�@Z�@Z��@Zi�@ZW�@Z@�@Z	@Y�n@Yhs@X��@X<�@W��@WS�@V��@V��@V	@U;@T��@S��@SMj@R�'@RYK@Q�^@Q0�@P�|@P��@P?�@O��@OdZ@O"�@N��@NYK@NC�@Ne@M�>@M�'@MVm@L�@L�O@L�@L,=@K�@K��@K�@J͟@J�x@J-@J �@I��@I�)@I�9@I��@I5�@G��@Fں@FQ@E�3@E5�@D��@D�o@D9X@C�
@C�@CdZ@CS@B��@BOv@B_@A�@A��@A��@A��@AX@A<6@A*0@A@@@��@@�@?�;@?خ@?�}@?�K@?��@?l�@?o@>ȴ@>� @>4@=p�@=q@=	l@<ѷ@<�4@<~(@<tT@<@;��@;��@;RT@;)_@:�,@::*@9��@9�@9��@9}�@9X@9%F@8�@8�@8�v@8֡@8��@8��@8b@7�m@7��@7K�@6�@6��@6kQ@6d�@6^5@6V@6C�@6!�@5��@5A @4�z@3�@3��@2ȴ@2$�@2�@2�@2�@2	@1@1^�@18�@0��@0�[@0��@0��@0c�@0N�@0b@/�;@/� @/�w@/�[@/iD@/(@.�6@.^5@-��@-�'@-X@-Vm@-S&@-V@,�z@,Q�@+�@+�@+��@+��@+��@+o�@+J#@*�M@*�@*YK@*M�@*�@)�j@)�9@)�d@)��@)�7@)\�@)O�@)*0@(�@(��@(r�@(1'@(x@'�@'U�@'4�@'&@'
=@&��@&��@&��@&��@&��@&�y@&��@&�<@&1�@%k�@%J�@%+�@%+@%�@$��@$�e@$�@$U2@$M@$2�@#�r@#�a@#��@#g�@#.I@"�2@"��@"��@"Q@"!�@!�T@!�T@!�@!?}@ Ɇ@ ��@ [�@ 2�@ �@خ@��@~�@A�@S@�!@i�@B[@&�@�N@w2@-w@�@��@��@�D@�@�@�}@�@�@��@��@�@�T@��@j@�@��@u�@$@ݘ@o�@�@��@��@p;@1�@�)@ԕ@@��@�^@�@��@�t@�-@�X@p�@�@�@�@N�@1@��@��@��@>�@}V@O@�@�@�@��@ϫ@�@�C@�M@hs@c�@O�@�@��@��@PH@�A@خ@�g@�F@t�@�@�8@ȴ@�\@YK@	@�3@�n@�S@zx@m]@`B@IR@7L@;@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��2A��>A��A��"A���A�MA��A��A��A��A�PA��A��A�#�A�%zA�*eA�.A�/OA�4�A�:*A�>A�>BA�8RA�3�A�0�A�,=A�2aA�=<A�N�AϘ+AЈfAв�AП�AЎ�A�8RA��Aɐ�A��.A�+�A�IA�2�A���A��A��SA��A��A�qAA�x8A��eA��gA�`�A�,A�"�A��CA��A�MA�G�A��hA��+A���A�jA�FA�]�A�xlA���A�)�A��A��EA��]A��aA�<A�FA�9�A�v�A�#nA�!�A��A���A�B[A�-�A��mA��9A�xA�A�A�2aA{0UAq>�Al�Aj��Ah�Ae�tAb��Aa��A`�NA_:�A]h�AY*0AW�AS��AQRTAO��AM�eAKPHAH�zAG��AF��AE�^AC�AA�7A>\)A<$�A9�gA6xlA2]dA3xlA2�bA1��A1�A1�A1C-A0��A/ȴA.SA,?A+a�A+VA+u%A+R�A*�rA*y>A)*0A'�mA&�.A$��A#zxA#�AA$h
A$�_A#(�A!��A �AXyA�A��A/�A{JAs�A�A�zA��A iA*�A�)A��A�Ae�A��Aj�A�AH�A��A��AzAQ�A �A��A�A��A��ACA�SAxA~�A \AH�APHA�DA��A�A&�A�$Ah
A�AE9AJ�A�A($A@OAFtAM�A9XA�5A�PA?}A�A
�]A
-�A	�XA	U�A	�A!-A��A�4Ae�A \ADgA�A�[A�PA/�A��A��A��Al�AAVmA|�A�RAl�A�A�AخA�IAW?A�A Q@��@��$@��K@�G@���@�c@�0�@��@��U@��@���@�H@���@�Z�@��@��@��@�@��6@�a|@�J@�j�@�@��@��}@��@�=�@�Ft@�~�@�(�@�Q@��@�j@�S&@���@�ȴ@��[@��@��@�s�@��@�l"@��@�!-@�A@�V�@�1'@�e@��@�J#@�@��@�;@�_@�[@���@�B[@�s@��v@�>B@�x�@�*0@�ߤ@�S�@�~�@�@�u�@���@��@�x�@�F�@�q@��`@ڔF@�H�@��@���@��#@���@ڶ�@�H�@�.I@ڀ�@ْ:@�+@�(@ع$@�_@�9X@�@֏\@�D�@��a@�x�@��@��@��@���@�0U@�=@�7�@ѡ�@�/�@К�@�l�@���@�%@�E�@ͧ�@���@̚@�J@�q@���@�*�@�c@���@�i�@���@�U�@�7�@œ@�^�@�o@ĸR@��@�L�@¶�@�D�@��@�w2@��@�<�@��@��N@���@�hs@�
=@�K^@���@���@���@���@�x�@�(�@�ff@��
@���@�~�@�g�@�L�@� \@���@���@�E�@�/@��v@�C-@��@�$t@��b@�V�@��Q@� \@�Ĝ@��z@��@���@�2a@���@�6�@��*@�J#@��s@�Xy@��@�ƨ@���@�q@���@�ԕ@�B�@���@�.�@���@��V@�k�@��@�h
@�@�z�@�<�@�+k@� �@���@��@���@�O�@��@��<@�kQ@��@��@��~@��@�Ta@�"h@��n@�\)@��v@���@�3�@���@�c�@�"�@���@�4n@�7@��>@��k@�`B@��@���@�PH@���@��0@���@��@�_p@�J�@�+@��@���@��!@��_@��@�c @�1@�o�@��@�PH@��T@��f@��@��@�_�@�;�@��@�u@��+@��&@���@�RT@��2@��?@��6@���@�GE@�{@��$@�)_@��K@��@��@�5�@���@�Ɇ@�z@�+k@�	@��@�@� �@��D@��g@���@�`B@�E9@��s@�5?@�)�@�	@��^@�rG@�T�@�9�@�@��@��!@�?�@��Z@��@��q@�p�@�C�@�ں@��\@�^5@�3�@��@�|@�8@��8@��h@�p;@�I�@��@���@���@��M@�x@�c�@�X�@�Mj@�F@�@O@�-w@��@�ȴ@���@�W�@� �@��D@���@�@�~(@�u�@�bN@�8�@��@��W@�hs@��@��@��@��=@�@O@�q@�Y@���@���@�҉@�Ɇ@�Ĝ@���@�v�@�V�@�<�@�7@��@��Z@��>@���@���@���@�W?@�:�@�&�@���@��$@��z@�ff@�x@��@@~�!@~1�@}��@}Dg@|�f@|��@|�@|l"@|@{�A@{��@{!-@z�@yL�@xc�@x$@w��@w�@w�@w�@w�@v�8@v��@vp;@v�@uϫ@u��@uO�@t�P@t�)@t�D@sݘ@s i@r��@rL0@q��@p��@pm�@pb@o�0@oqv@oA�@n��@n&�@m�C@m��@mT�@l��@lc�@k˒@j�6@i��@i�T@ip�@h�@h7�@g�q@g�@f��@f �@e��@e��@e5�@d��@c��@cdZ@b�+@b^5@ba|@bc @b;�@a�@a�@a��@`�e@`/�@_�r@_��@_\)@^�x@^-@^�@]o @]�@\�?@\h�@[�@Z�@Z��@Zi�@ZW�@Z@�@Z	@Y�n@Yhs@X��@X<�@W��@WS�@V��@V��@V	@U;@T��@S��@SMj@R�'@RYK@Q�^@Q0�@P�|@P��@P?�@O��@OdZ@O"�@N��@NYK@NC�@Ne@M�>@M�'@MVm@L�@L�O@L�@L,=@K�@K��@K�@J͟@J�x@J-@J �@I��@I�)@I�9@I��@I5�@G��@Fں@FQ@E�3@E5�@D��@D�o@D9X@C�
@C�@CdZ@CS@B��@BOv@B_@A�@A��@A��@A��@AX@A<6@A*0@A@@@��@@�@?�;@?خ@?�}@?�K@?��@?l�@?o@>ȴ@>� @>4@=p�@=q@=	l@<ѷ@<�4@<~(@<tT@<@;��@;��@;RT@;)_@:�,@::*@9��@9�@9��@9}�@9X@9%F@8�@8�@8�v@8֡@8��@8��@8b@7�m@7��@7K�@6�@6��@6kQ@6d�@6^5@6V@6C�@6!�@5��@5A @4�z@3�@3��@2ȴ@2$�@2�@2�@2�@2	@1@1^�@18�@0��@0�[@0��@0��@0c�@0N�@0b@/�;@/� @/�w@/�[@/iD@/(@.�6@.^5@-��@-�'@-X@-Vm@-S&@-V@,�z@,Q�@+�@+�@+��@+��@+��@+o�@+J#@*�M@*�@*YK@*M�@*�@)�j@)�9@)�d@)��@)�7@)\�@)O�@)*0@(�@(��@(r�@(1'@(x@'�@'U�@'4�@'&@'
=@&��@&��@&��@&��@&��@&�y@&��@&�<@&1�@%k�@%J�@%+�@%+@%�@$��@$�e@$�@$U2@$M@$2�@#�r@#�a@#��@#g�@#.I@"�2@"��@"��@"Q@"!�@!�T@!�T@!�@!?}@ Ɇ@ ��@ [�@ 2�@ �@خ@��@~�@A�@S@�!@i�@B[@&�@�N@w2@-w@�@��@��@�D@�@�@�}@�@�@��@��@�@�T@��@j@�@��@u�@$@ݘ@o�@�@��@��@p;@1�@�)@ԕ@@��@�^@�@��@�t@�-@�X@p�@�@�@�@N�@1@��@��@��@>�@}V@O@�@�@�@��@ϫ@�@�C@�M@hs@c�@O�@�@��@��@PH@�A@خ@�g@�F@t�@�@�8@ȴ@�\@YK@	@�3@�n@�S@zx@m]@`B@IR@7L@;@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�sB�sB�sB�
B�>B�$B��B�$B�KB�B��B�wB��B�nB�LB��B�B	aB	JB	FB	!�B	&�B	)_B	*�B	+B	3MB	>wB	E9B	[�B	�B
./B
vFB
�7B
��B
�.B
S@B
R�B
L�B
S[B
^�B
v+B
�hB
�B
�"B
��B
��B
��B/�BD�BZQB{�B��BϫB��B�B�TB��B�8BEB�BVB~B�BBfB�B��B�B�aB�fB��B��B��BmCB[�BEmB$�BJB
ߊB
�B
��B
��B
gB
@B
�B	��B	�B	s�B	h�B	^�B	Q�B	G�B	CGB	>BB	72B	/�B	%,B	"�B	#B	!B	�B	9B	aB	�B		RB	�B	1B	B	�B	"B	/OB	I�B	C-B	,�B	g�B	��B	��B	��B	�$B	��B	��B	��B	��B	��B	z�B	{JB	��B	�B	� B	z�B	n�B	_�B	V�B	A�B	?�B	MPB	oOB	��B	�?B	~BB	|B	|�B	{B	|�B	��B	�B	��B	��B	�MB	��B	�#B	��B	�&B	�XB	�"B	��B	��B	��B	��B	�>B	��B	��B	�[B	�gB	�3B	�mB	�=B	��B	�]B	�B	�B	�B	�B	��B	�|B	ޞB	�B	��B	��B	ݘB	�~B	��B	��B	��B	�B	��B	��B	�B	�}B
�B
�B
 �B
 4B
 �B
UB	��B	�aB	��B	��B	�B	�9B	�[B	�OB	�B	� B	�B
 iB
aB
B
�B	��B
�B
SB
3B
�B	�cB	�B	�8B	��B	��B	��B	�2B	�2B	�B	�rB	�2B	�FB	�B	��B	�zB	��B	�lB	�lB	�lB	��B	��B	��B	�JB	��B	�(B
 �B
B
;B
�B
'B
[B
�B
'B
B
�B
B
�B
mB
B
�B
�B
B
 OB	��B	�B	�6B	��B	��B	��B
�B
-B
�B
�B
AB
uB
�B
 �B
 4B	��B	��B	��B
 �B
-B
uB
 B	�]B	�"B	��B	��B	�XB	��B	�lB	�8B	�8B	�RB	�B	�B	�8B	�8B	��B	�XB	�XB	�XB	�rB	��B	��B	�JB	��B	��B
;B
_B
PB
B
JB

�B

�B
B
~B
�B

�B
)B
�B
PB
�B
B
dB
�B
�B
B
B
�B
�B
�B
dB
�B
B
xB
	�B
fB
KB
�B
�B
	RB
�B
KB
B
�B
1B
�B
�B
	�B
)B
xB
�B
B

�B

=B

�B
	�B
	�B
	lB
	�B

#B

�B

�B

�B

XB

rB

�B
xB
�B
�B
�B
�B
xB
�B
B
dB
JB
dB
JB
dB
JB
0B
B
JB
6B
B
6B
jB
�B
VB
<B
�B
�B
"B
�B
�B
�B
�B
�B
VB
\B
\B
HB
�B
4B
 B
�B
�B
}B
�B
�B
VB
6B
6B
�B
�B
vB
vB
�B
�B
vB
vB
B
�B
�B
vB
vB
�B
�B
.B
HB
HB
�B
4B
:B
B
TB
�B
TB
TB
B
�B
B
�B
B
&B
@B
uB
�B
B
,B
�B
gB
�B
9B
mB
�B
�B
�B
�B

B
?B
?B
YB
?B
?B
sB
�B
+B
�B
KB
�B
�B
qB
�B
�B
�B
�B
�B
�B
)B
xB
�B
�B
�B
�B
B
/B
~B
�B
~B
/B
IB
�B
~B
IB
�B
�B
�B
�B
�B
~B
~B
IB
/B
/B
�B
�B
�B
�B
�B
�B
B
/B
/B
dB
�B
�B
 �B
 BB
 �B
!B
 �B
!-B
 �B
 �B
 �B
 �B
!B
!�B
!�B
"B
"�B
"�B
"�B
#:B
#�B
#�B
$B
$&B
$&B
$&B
$ZB
$ZB
$ZB
$@B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
'�B
(�B
(�B
(�B
)B
)*B
)DB
*eB
)�B
*�B
+kB
+�B
,WB
,WB
,WB
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,qB
,WB
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-wB
-�B
./B
.�B
.�B
/ B
/5B
/�B
/�B
0B
/�B
0B
0!B
0!B
0UB
0oB
0�B
0oB
2B
2aB
2|B
2�B
2�B
2aB
3B
33B
3MB
3�B
3�B
4B
49B
4TB
4�B
4�B
4�B
4�B
5ZB
6B
6+B
6+B
6�B
6�B
7LB
7�B
7�B
7�B
8B
8�B
9>B
9�B
9rB
9�B
:DB
:�B
;JB
;�B
;�B
;�B
<B
<jB
<�B
=VB
=�B
>B
>�B
?B
?B
?HB
?�B
?�B
@OB
@�B
AB
@�B
@�B
AB
A;B
AUB
A B
BAB
B[B
BuB
B�B
B�B
C�B
C�B
C{B
C�B
C�B
D3B
DgB
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
FB
F?B
GB
G+B
GzB
G�B
G�B
H�B
H�B
IRB
I�B
I�B
J=B
J�B
J�B
K)B
K^B
K�B
K�B
K�B
K�B
LJB
L~B
L�B
L�B
L�B
L�B
MB
M�B
M�B
M�B
M�B
NB
N"B
N�B
N�B
N�B
O(B
OB
O(B
O(B
OB
O(B
OBB
Q B
QhB
Q�B
R:B
RTB
R�B
R�B
R�B
SB
S&B
S&B
SuB
S�B
S�B
TB
TB
T,B
T,B
T,B
TaB
TaB
TFB
TaB
T�B
UB
UgB
UgB
UgB
UMB
U2B
UMB
U�B
U�B
VB
VSB
V�B
W$B
V�B
W$B
W
B
WsB
WsB
W�B
W�B
W�B
X�B
XyB
YB
YB
YB
Y�B
ZB
Z7B
Z7B
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
\]B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
]IB
]~B
^OB
^�B
_B
_�B
`�B
`�B
`vB
`�B
`BB
`�B
`�B
aB
a|B
abB
aHB
a�B
a�B
a�B
a�B
b4B
a�B
bNB
a�B
bNB
b�B
cB
b�B
cnB
c�B
c�B
c�B
c�B
dB
dZB
d�B
e,B
e,B
e,B
eB
eB
e`B
eFB
e�B
fB
e�B
fB
ffB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gRB
g�B
g�B
g�B
g�B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i_B
jB
jB
j0B
jKB
jKB
jeB
j�B
j�B
j�B
j�B
j�B
kB
kB
kQB
kQB
k�B
k�B
k�B
lB
l=B
lqB
l�B
l�B
l�B
m)B
m�B
m�B
m�B
m�B
nB
ncB
ncB
n}B
n�B
n�B
o5B
oiB
o�B
o�B
o�B
p;B
poB
p�B
p�B
p�B
p�B
qAB
qAB
q[B
q�B
r|B
r�B
sB
shB
s�B
tB
tB
t�B
t�B
uB
u%B
u?B
u�B
u�B
v+B
vFB
vFB
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wfB
w�B
w�B
xB
xRB
x�B
xlB
xRB
x�B
y�B
y�B
y�B
y�B
z*B
zB
z*B
zB
z*B
zDB
z^B
zxB
z^B
z�B
z�B
z�B
{�B
|B
|PB
|6B
|PB
|jB
|�B
|�B
|�B
}B
}B
}<B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~B
~]B
~�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�sB�sB�sB�
B�>B�$B��B�$B�KB�B��B�wB��B�nB�LB��B�B	aB	JB	FB	!�B	&�B	)_B	*�B	+B	3MB	>wB	E9B	[�B	�B
./B
vFB
�7B
��B
�.B
S@B
R�B
L�B
S[B
^�B
v+B
�hB
�B
�"B
��B
��B
��B/�BD�BZQB{�B��BϫB��B�B�TB��B�8BEB�BVB~B�BBfB�B��B�B�aB�fB��B��B��BmCB[�BEmB$�BJB
ߊB
�B
��B
��B
gB
@B
�B	��B	�B	s�B	h�B	^�B	Q�B	G�B	CGB	>BB	72B	/�B	%,B	"�B	#B	!B	�B	9B	aB	�B		RB	�B	1B	B	�B	"B	/OB	I�B	C-B	,�B	g�B	��B	��B	��B	�$B	��B	��B	��B	��B	��B	z�B	{JB	��B	�B	� B	z�B	n�B	_�B	V�B	A�B	?�B	MPB	oOB	��B	�?B	~BB	|B	|�B	{B	|�B	��B	�B	��B	��B	�MB	��B	�#B	��B	�&B	�XB	�"B	��B	��B	��B	��B	�>B	��B	��B	�[B	�gB	�3B	�mB	�=B	��B	�]B	�B	�B	�B	�B	��B	�|B	ޞB	�B	��B	��B	ݘB	�~B	��B	��B	��B	�B	��B	��B	�B	�}B
�B
�B
 �B
 4B
 �B
UB	��B	�aB	��B	��B	�B	�9B	�[B	�OB	�B	� B	�B
 iB
aB
B
�B	��B
�B
SB
3B
�B	�cB	�B	�8B	��B	��B	��B	�2B	�2B	�B	�rB	�2B	�FB	�B	��B	�zB	��B	�lB	�lB	�lB	��B	��B	��B	�JB	��B	�(B
 �B
B
;B
�B
'B
[B
�B
'B
B
�B
B
�B
mB
B
�B
�B
B
 OB	��B	�B	�6B	��B	��B	��B
�B
-B
�B
�B
AB
uB
�B
 �B
 4B	��B	��B	��B
 �B
-B
uB
 B	�]B	�"B	��B	��B	�XB	��B	�lB	�8B	�8B	�RB	�B	�B	�8B	�8B	��B	�XB	�XB	�XB	�rB	��B	��B	�JB	��B	��B
;B
_B
PB
B
JB

�B

�B
B
~B
�B

�B
)B
�B
PB
�B
B
dB
�B
�B
B
B
�B
�B
�B
dB
�B
B
xB
	�B
fB
KB
�B
�B
	RB
�B
KB
B
�B
1B
�B
�B
	�B
)B
xB
�B
B

�B

=B

�B
	�B
	�B
	lB
	�B

#B

�B

�B

�B

XB

rB

�B
xB
�B
�B
�B
�B
xB
�B
B
dB
JB
dB
JB
dB
JB
0B
B
JB
6B
B
6B
jB
�B
VB
<B
�B
�B
"B
�B
�B
�B
�B
�B
VB
\B
\B
HB
�B
4B
 B
�B
�B
}B
�B
�B
VB
6B
6B
�B
�B
vB
vB
�B
�B
vB
vB
B
�B
�B
vB
vB
�B
�B
.B
HB
HB
�B
4B
:B
B
TB
�B
TB
TB
B
�B
B
�B
B
&B
@B
uB
�B
B
,B
�B
gB
�B
9B
mB
�B
�B
�B
�B

B
?B
?B
YB
?B
?B
sB
�B
+B
�B
KB
�B
�B
qB
�B
�B
�B
�B
�B
�B
)B
xB
�B
�B
�B
�B
B
/B
~B
�B
~B
/B
IB
�B
~B
IB
�B
�B
�B
�B
�B
~B
~B
IB
/B
/B
�B
�B
�B
�B
�B
�B
B
/B
/B
dB
�B
�B
 �B
 BB
 �B
!B
 �B
!-B
 �B
 �B
 �B
 �B
!B
!�B
!�B
"B
"�B
"�B
"�B
#:B
#�B
#�B
$B
$&B
$&B
$&B
$ZB
$ZB
$ZB
$@B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
'�B
(�B
(�B
(�B
)B
)*B
)DB
*eB
)�B
*�B
+kB
+�B
,WB
,WB
,WB
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,qB
,WB
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-wB
-�B
./B
.�B
.�B
/ B
/5B
/�B
/�B
0B
/�B
0B
0!B
0!B
0UB
0oB
0�B
0oB
2B
2aB
2|B
2�B
2�B
2aB
3B
33B
3MB
3�B
3�B
4B
49B
4TB
4�B
4�B
4�B
4�B
5ZB
6B
6+B
6+B
6�B
6�B
7LB
7�B
7�B
7�B
8B
8�B
9>B
9�B
9rB
9�B
:DB
:�B
;JB
;�B
;�B
;�B
<B
<jB
<�B
=VB
=�B
>B
>�B
?B
?B
?HB
?�B
?�B
@OB
@�B
AB
@�B
@�B
AB
A;B
AUB
A B
BAB
B[B
BuB
B�B
B�B
C�B
C�B
C{B
C�B
C�B
D3B
DgB
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
FB
F?B
GB
G+B
GzB
G�B
G�B
H�B
H�B
IRB
I�B
I�B
J=B
J�B
J�B
K)B
K^B
K�B
K�B
K�B
K�B
LJB
L~B
L�B
L�B
L�B
L�B
MB
M�B
M�B
M�B
M�B
NB
N"B
N�B
N�B
N�B
O(B
OB
O(B
O(B
OB
O(B
OBB
Q B
QhB
Q�B
R:B
RTB
R�B
R�B
R�B
SB
S&B
S&B
SuB
S�B
S�B
TB
TB
T,B
T,B
T,B
TaB
TaB
TFB
TaB
T�B
UB
UgB
UgB
UgB
UMB
U2B
UMB
U�B
U�B
VB
VSB
V�B
W$B
V�B
W$B
W
B
WsB
WsB
W�B
W�B
W�B
X�B
XyB
YB
YB
YB
Y�B
ZB
Z7B
Z7B
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
\]B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
]IB
]~B
^OB
^�B
_B
_�B
`�B
`�B
`vB
`�B
`BB
`�B
`�B
aB
a|B
abB
aHB
a�B
a�B
a�B
a�B
b4B
a�B
bNB
a�B
bNB
b�B
cB
b�B
cnB
c�B
c�B
c�B
c�B
dB
dZB
d�B
e,B
e,B
e,B
eB
eB
e`B
eFB
e�B
fB
e�B
fB
ffB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gRB
g�B
g�B
g�B
g�B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i_B
jB
jB
j0B
jKB
jKB
jeB
j�B
j�B
j�B
j�B
j�B
kB
kB
kQB
kQB
k�B
k�B
k�B
lB
l=B
lqB
l�B
l�B
l�B
m)B
m�B
m�B
m�B
m�B
nB
ncB
ncB
n}B
n�B
n�B
o5B
oiB
o�B
o�B
o�B
p;B
poB
p�B
p�B
p�B
p�B
qAB
qAB
q[B
q�B
r|B
r�B
sB
shB
s�B
tB
tB
t�B
t�B
uB
u%B
u?B
u�B
u�B
v+B
vFB
vFB
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wfB
w�B
w�B
xB
xRB
x�B
xlB
xRB
x�B
y�B
y�B
y�B
y�B
z*B
zB
z*B
zB
z*B
zDB
z^B
zxB
z^B
z�B
z�B
z�B
{�B
|B
|PB
|6B
|PB
|jB
|�B
|�B
|�B
}B
}B
}<B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~B
~]B
~�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105241  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192242  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192243  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192243                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042251  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042251  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                