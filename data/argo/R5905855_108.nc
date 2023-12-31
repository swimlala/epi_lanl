CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:29:59Z creation;2022-06-04T19:29:59Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192959  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               lA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ٴ�@E�1   @ٴ�7_1�@,��E��dC��%1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@y��@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBy��B}33B�  B�33B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�33B�ffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C633C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  D   D � D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF�fDGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}fD}�fD~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ DɃ3D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ Dټ�D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@w
>@��R@��RA\)A?\)A_\)A}A��A��A��A��AϮA߮A�A��B�
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
Bp=pByp�B}
=B��B��B��B��B��B��RB��B��B��B��B��B��B��B�Q�B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C]C��C	�)C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C6(�C7�)C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce�)Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C��C���C���C���C���C���C���C��C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD)�D)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4��D4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF��DG�DG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD}�D}��D}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�DɁ�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dٻ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D��D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�7LA�R A�V9A�W
A�R A�S&A�R�A�U2A�W�A�XA�ZQA�V9A�PHA�OBA�U�A�VmA�XA�YA�[#A�YKA�ZQA�]/A�[�A�Y�A�Z�A�[#A�YA�W�A�X�A�($A��A���A�)�A�ݘA��jA���A��A��LA���A�qA�`A�<�A� �A�~�A�_�A���A��yA�k�A��A���A��A���A���A��LA�y�A��A�xA�.}A��A�A|�jAy%Atm�Aoq�Al�WAjD�Agc�AfbNA`&AZ�AYL0AW~�AV��AT��AN��AL'�AI�)AFrGAEOAD�AC�+AC�A?��A?�A>�jA<��A;r�A8��A7(A5��A1�MA.ĜA,��A+u%A)�PA(A&��A%�=A$��A$rGA#�A#��A"��A!�	A!qvA!ffA!|A!��A!)�A j�A   A��AϫA��A��A��Az�AJ�A�A��Ak�A�An�AffAe�A)�A��Av�AXA�{A��A��A)_A�AYKAYA�A��AE9A;dA9�A:�A:*AL0A"�A��A��A�0A�MA��A�`A%�A��A7A�ZA�"A�,A�kAC�A��A��A�UA�pAS�A�A�MAzxA��A�uAB�A%A�gA��AL�A
�OA
�bA
�YA
l"A
C�A	�A	��A	<6A�A��A��A]dA@A��A�A�nAw2AA�A�A!�A�0Av`AO�A�A�jA�YAB[AA��A��A\�A[�AFtA�A�oA �A m]@��@�T�@�7L@���@�Ft@�?}@���@�8@���@�C-@���@��$@�O�@���@�M@��K@�<6@�Ĝ@��x@�^5@��A@�@��@�Xy@��@�@��@��@�6z@�?�@���@�.I@�T�@�0�@�L@�($@�@꿱@���@���@��@��@�c�@��)@��]@�B�@�?@�T�@��f@�V�@�C@��`@�4@ދD@ݳ�@���@ۼ�@��@ڭ�@�ݘ@ٯ�@���@��5@�bN@��)@�B�@�Ov@�s�@��@�;�@ӈf@� �@Ч@�GE@ϕ�@ζ�@΅�@Ό@�Q�@���@�X�@��y@̏\@�&�@��@���@�o�@ʰ!@���@�X@�@@�~@ǽ�@�Q�@���@�V�@��m@š�@��@�/�@ó�@��@@�2�@���@�P�@��@��u@�	�@���@���@��@�
=@���@�Xy@���@�=@���@���@�N�@��@���@�Q�@��$@��@���@���@���@�Z@�D�@��$@��@��@���@��@��K@��@�C-@���@��@���@���@��P@�@��@��@�?@���@�H�@�tT@��@���@�@@���@���@�\�@���@��F@�\)@��v@�L0@��#@�S�@� \@���@��6@�1�@���@��K@���@�n/@�j@�g�@�
=@��4@�@�ی@���@�xl@�8�@���@�8@�|�@�
�@���@�e,@��@��5@���@�B[@���@��@��M@��A@�Z@�I�@��@���@��@���@��b@�� @���@�z@�a|@�!�@���@��j@�G@�� @��P@�L�@��@��I@�@��@��)@���@���@�e,@�=�@�"�@�|�@�4n@�@���@�s@�a�@�Dg@�+@���@���@�}V@�a|@��@���@�{J@�/@��@��Y@�5?@��Z@��g@��q@���@�o @�H�@��@���@���@�4n@��+@���@��@�]�@��@���@�w�@�L0@�$�@��{@�C�@��@��@���@�M@�ϫ@��@@�33@���@�|�@�8�@�J@��W@�0�@��@���@��@���@�I�@�	�@��m@���@�j@��@��@���@���@�B[@���@���@�|�@�p�@�o @�e,@�O�@�!�@���@���@�[�@�+k@��W@��@���@�Z�@�?}@�4@���@���@���@���@�bN@�@��K@���@�^�@��H@���@�I�@�=q@��@��D@���@�  @��@���@��@���@���@��$@�x@�;@��@�i�@�GE@�9X@�&�@�@��6@�\)@�>�@�Y@���@�~(@�@��@~��@~��@~�x@~a|@~_@}�n@}-w@|�	@|�O@| �@{��@{P�@{&@{(@z�c@z�@z;�@y��@x��@x��@xK^@w��@w��@wH�@wn/@w)_@vn�@v?@v�@u�@u��@uQ�@t�)@t4n@sy�@sS@r�@r��@r�A@r?@q�d@qs�@qL�@q(�@q�@p�@p(�@o��@o��@oS�@o(@n�\@n �@m�X@mG�@m;@l��@l~(@l<�@l-�@l	�@k��@kC@j�+@i�C@i+@h�@hPH@h-�@hM@g�@@gRT@g&@f�c@e�d@e��@eo @e \@d]d@c��@co�@cE9@b��@b��@bff@a�T@aj@`~(@_�@_��@^�M@^ff@\ی@\V�@\	�@[��@[o@Zߤ@Zں@Z�@Z�<@Zff@Y+�@X�$@X>B@W�@W��@W�	@W�{@We�@W�@VC�@U�)@Um]@T�v@T�@TI�@TH@T~@S�@S]�@R��@R�@R��@Q��@P�f@PĜ@Pw�@PC-@O�@OZ�@N�@NL0@N{@MIR@L��@L�/@L�[@LtT@Lb@K��@K$t@K�@J��@I��@H��@H�O@Hl"@HC-@G�+@G��@G�0@G�4@Gl�@GA�@F�@Fd�@F;�@E�.@E��@EQ�@D�f@D֡@D�@D�@D��@D7@C�a@C�*@C��@CdZ@B��@Bff@B�@A�@A��@Ap�@A�@@��@@ѷ@@A�@@x@?�;@?�@@?8@>�@>�F@>p;@>8�@=�o@=�X@=S&@<�`@<��@<6@<�@;ݘ@;��@;6z@;S@:��@:J�@:	@9[W@95�@9q@8�9@8Q�@84n@7��@7�@7n/@7'�@6�H@6��@6v�@6GE@5��@4�	@4�_@4?�@3�@3�@3@O@2��@2��@2�@2R�@1�@1��@1k�@1�@0�$@0�@0��@0r�@0:�@/�@/b�@/4�@.�@.�h@.��@.V@.�@-�D@-ԕ@-�X@-^�@,�`@,�9@,4n@,b@,�@+��@+��@+�@+��@+Z�@+o@*�H@*�@*V@*!�@)�9@)��@)s�@)<6@)�@(�v@(��@(��@(�Y@'�@'��@'�@'a@'O@'�@&��@&Q@&�@%�>@%�'@%#�@$�@$Xy@$(�@$�@$@#��@#�0@#j�@"��@"�h@"��@"V@"@�@"#:@!��@!�^@!�@!o @!^�@!A @ �5@ ��@ ��@ �u@ �o@ e�@ ,=@��@��@�{@;d@Y@�R@��@C�@{@�@��@O�@+�@�@�I@��@q@<�@@��@@O@�@�s@�h@q�@=q@($@��@Dg@-w@֡@�.@�Y@~(@g8@,=@�@��@��@�	@X�@,�@�@��@@�@e@�T@��@��@�"@T�@0�@�@ی@�@M@*�@�@�@�+@�K@��@�@o�@l�@n/@=@��@҉@��@��@J�@e@��@��@��@x�@?}@�@;@�@�|@��@�.@~(@N�@$@�+@��@��@\)@E9@C�@=@'�@��@�b@~�@xl@h
@H�@4@�@�@��@�@�'@�~@?}@@%@ی@��@m�@K^@,=@(�@�@�m@��@j�@X�@'�@
�M@
��@
�<@
��@
�@
�s@
��@
��@
M�@
=q@
-@	�@	ԕ@	�z@	11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�7LA�R A�V9A�W
A�R A�S&A�R�A�U2A�W�A�XA�ZQA�V9A�PHA�OBA�U�A�VmA�XA�YA�[#A�YKA�ZQA�]/A�[�A�Y�A�Z�A�[#A�YA�W�A�X�A�($A��A���A�)�A�ݘA��jA���A��A��LA���A�qA�`A�<�A� �A�~�A�_�A���A��yA�k�A��A���A��A���A���A��LA�y�A��A�xA�.}A��A�A|�jAy%Atm�Aoq�Al�WAjD�Agc�AfbNA`&AZ�AYL0AW~�AV��AT��AN��AL'�AI�)AFrGAEOAD�AC�+AC�A?��A?�A>�jA<��A;r�A8��A7(A5��A1�MA.ĜA,��A+u%A)�PA(A&��A%�=A$��A$rGA#�A#��A"��A!�	A!qvA!ffA!|A!��A!)�A j�A   A��AϫA��A��A��Az�AJ�A�A��Ak�A�An�AffAe�A)�A��Av�AXA�{A��A��A)_A�AYKAYA�A��AE9A;dA9�A:�A:*AL0A"�A��A��A�0A�MA��A�`A%�A��A7A�ZA�"A�,A�kAC�A��A��A�UA�pAS�A�A�MAzxA��A�uAB�A%A�gA��AL�A
�OA
�bA
�YA
l"A
C�A	�A	��A	<6A�A��A��A]dA@A��A�A�nAw2AA�A�A!�A�0Av`AO�A�A�jA�YAB[AA��A��A\�A[�AFtA�A�oA �A m]@��@�T�@�7L@���@�Ft@�?}@���@�8@���@�C-@���@��$@�O�@���@�M@��K@�<6@�Ĝ@��x@�^5@��A@�@��@�Xy@��@�@��@��@�6z@�?�@���@�.I@�T�@�0�@�L@�($@�@꿱@���@���@��@��@�c�@��)@��]@�B�@�?@�T�@��f@�V�@�C@��`@�4@ދD@ݳ�@���@ۼ�@��@ڭ�@�ݘ@ٯ�@���@��5@�bN@��)@�B�@�Ov@�s�@��@�;�@ӈf@� �@Ч@�GE@ϕ�@ζ�@΅�@Ό@�Q�@���@�X�@��y@̏\@�&�@��@���@�o�@ʰ!@���@�X@�@@�~@ǽ�@�Q�@���@�V�@��m@š�@��@�/�@ó�@��@@�2�@���@�P�@��@��u@�	�@���@���@��@�
=@���@�Xy@���@�=@���@���@�N�@��@���@�Q�@��$@��@���@���@���@�Z@�D�@��$@��@��@���@��@��K@��@�C-@���@��@���@���@��P@�@��@��@�?@���@�H�@�tT@��@���@�@@���@���@�\�@���@��F@�\)@��v@�L0@��#@�S�@� \@���@��6@�1�@���@��K@���@�n/@�j@�g�@�
=@��4@�@�ی@���@�xl@�8�@���@�8@�|�@�
�@���@�e,@��@��5@���@�B[@���@��@��M@��A@�Z@�I�@��@���@��@���@��b@�� @���@�z@�a|@�!�@���@��j@�G@�� @��P@�L�@��@��I@�@��@��)@���@���@�e,@�=�@�"�@�|�@�4n@�@���@�s@�a�@�Dg@�+@���@���@�}V@�a|@��@���@�{J@�/@��@��Y@�5?@��Z@��g@��q@���@�o @�H�@��@���@���@�4n@��+@���@��@�]�@��@���@�w�@�L0@�$�@��{@�C�@��@��@���@�M@�ϫ@��@@�33@���@�|�@�8�@�J@��W@�0�@��@���@��@���@�I�@�	�@��m@���@�j@��@��@���@���@�B[@���@���@�|�@�p�@�o @�e,@�O�@�!�@���@���@�[�@�+k@��W@��@���@�Z�@�?}@�4@���@���@���@���@�bN@�@��K@���@�^�@��H@���@�I�@�=q@��@��D@���@�  @��@���@��@���@���@��$@�x@�;@��@�i�@�GE@�9X@�&�@�@��6@�\)@�>�@�Y@���@�~(@�@��@~��@~��@~�x@~a|@~_@}�n@}-w@|�	@|�O@| �@{��@{P�@{&@{(@z�c@z�@z;�@y��@x��@x��@xK^@w��@w��@wH�@wn/@w)_@vn�@v?@v�@u�@u��@uQ�@t�)@t4n@sy�@sS@r�@r��@r�A@r?@q�d@qs�@qL�@q(�@q�@p�@p(�@o��@o��@oS�@o(@n�\@n �@m�X@mG�@m;@l��@l~(@l<�@l-�@l	�@k��@kC@j�+@i�C@i+@h�@hPH@h-�@hM@g�@@gRT@g&@f�c@e�d@e��@eo @e \@d]d@c��@co�@cE9@b��@b��@bff@a�T@aj@`~(@_�@_��@^�M@^ff@\ی@\V�@\	�@[��@[o@Zߤ@Zں@Z�@Z�<@Zff@Y+�@X�$@X>B@W�@W��@W�	@W�{@We�@W�@VC�@U�)@Um]@T�v@T�@TI�@TH@T~@S�@S]�@R��@R�@R��@Q��@P�f@PĜ@Pw�@PC-@O�@OZ�@N�@NL0@N{@MIR@L��@L�/@L�[@LtT@Lb@K��@K$t@K�@J��@I��@H��@H�O@Hl"@HC-@G�+@G��@G�0@G�4@Gl�@GA�@F�@Fd�@F;�@E�.@E��@EQ�@D�f@D֡@D�@D�@D��@D7@C�a@C�*@C��@CdZ@B��@Bff@B�@A�@A��@Ap�@A�@@��@@ѷ@@A�@@x@?�;@?�@@?8@>�@>�F@>p;@>8�@=�o@=�X@=S&@<�`@<��@<6@<�@;ݘ@;��@;6z@;S@:��@:J�@:	@9[W@95�@9q@8�9@8Q�@84n@7��@7�@7n/@7'�@6�H@6��@6v�@6GE@5��@4�	@4�_@4?�@3�@3�@3@O@2��@2��@2�@2R�@1�@1��@1k�@1�@0�$@0�@0��@0r�@0:�@/�@/b�@/4�@.�@.�h@.��@.V@.�@-�D@-ԕ@-�X@-^�@,�`@,�9@,4n@,b@,�@+��@+��@+�@+��@+Z�@+o@*�H@*�@*V@*!�@)�9@)��@)s�@)<6@)�@(�v@(��@(��@(�Y@'�@'��@'�@'a@'O@'�@&��@&Q@&�@%�>@%�'@%#�@$�@$Xy@$(�@$�@$@#��@#�0@#j�@"��@"�h@"��@"V@"@�@"#:@!��@!�^@!�@!o @!^�@!A @ �5@ ��@ ��@ �u@ �o@ e�@ ,=@��@��@�{@;d@Y@�R@��@C�@{@�@��@O�@+�@�@�I@��@q@<�@@��@@O@�@�s@�h@q�@=q@($@��@Dg@-w@֡@�.@�Y@~(@g8@,=@�@��@��@�	@X�@,�@�@��@@�@e@�T@��@��@�"@T�@0�@�@ی@�@M@*�@�@�@�+@�K@��@�@o�@l�@n/@=@��@҉@��@��@J�@e@��@��@��@x�@?}@�@;@�@�|@��@�.@~(@N�@$@�+@��@��@\)@E9@C�@=@'�@��@�b@~�@xl@h
@H�@4@�@�@��@�@�'@�~@?}@@%@ی@��@m�@K^@,=@(�@�@�m@��@j�@X�@'�@
�M@
��@
�<@
��@
�@
�s@
��@
��@
M�@
=q@
-@	�@	ԕ@	�z@	11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BshBs3Br�Br�BraBrGBr-BraBs�Bs�Bs�Bs�BraBr|Br�Br�Br�Bs�Bs�Br�BrBq�Bq�Bq�Bq�Bq�Bq�Bq�Bu�B	�fB
F?B
��By>B��BڠB�_B�Bz�B��BtBc�B]IBL�BL~B9>B,�BB
��B
��B
�OB
�B
�xB
��B
��B
��B
��B
�-B
i�B
A�B
�B
�B	�B	�B	�;B	�B	�B	�0B	��B	cTB	KB	G+B	?�B	:�B	5�B	&B	]B	,B	^B	EB	B	[B	�B	AB��B��B��B�"B��B	 �B��B	�B��B	 4B	�B	YB	�B		7B	\B	�B	+B	�B	,�B	4�B	=VB	O(B	T{B	[#B	gmB	p!B	o�B	v�B	�3B	�\B	�[B	��B	��B	��B	�>B	��B	��B	�B	��B	�aB	�gB	��B	��B	��B	�XB	ɆB	��B	�B	�\B	�vB	�vB	�gB	�5B	�B	��B	ʌB	�=B	�=B	ʦB	�B	�jB	�PB	��B	��B	�EB	�CB	�QB	��B	��B	ևB	�'B	�B	�B	�B	��B	�B	��B
AB
B
	RB
B
KB
0B
�B
# B
$�B
"NB
 BB
#�B
(sB
&2B
"hB
&�B
'�B
)*B
*KB
*0B
)DB
)yB
)*B
+6B
+�B
,qB
-CB
/�B
/�B
0!B
/�B
/�B
.IB
,=B
+�B
+B
*�B
+QB
*0B
(�B
'mB
'8B
(
B
'�B
&�B
)B
)B
*B
+6B
*B
(�B
'�B
'B
'B
'B
'B
%B
$&B
#TB
"hB
!�B
 'B
�B
�B
�B
�B
QB
B
#B
=B
/B
/B
]B
�B
kB
/B
B
�B
�B
kB
B
B
�B
+B
�B
WB
	B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
PB
�B
�B
B

�B
KB
�B
YB
�B
�B
�B
	lB
�B
�B
�B
mB
�B
9B
	B
	�B
KB
�B
�B
'B
AB
�B
{B
{B
GB
�B
aB
GB
-B
-B
B
GB
-B
-B
�B
�B
�B
gB
�B
gB
�B
�B
�B
�B
gB
B
gB
B
�B
�B
3B
�B
�B
9B
9B
B
9B
�B
1B
�B
B
	B

rB

	B

�B
^B
xB
�B
B
�B
�B

rB
	�B
	�B
JB
0B

�B

�B

�B
^B
\B
�B
"B
B
^B
DB
�B
�B
dB
�B
"B
B
6B
JB
0B
xB
^B
dB
6B
B
jB
�B
PB
pB
�B
�B
�B
B
PB
jB
jB
�B
�B
�B
B
B
�B
�B
�B
�B
NB
�B
hB
hB
hB
NB
B
�B
�B
�B
:B
:B
:B
oB
B
{B
�B
�B
�B
2B
�B
mB
�B
�B
�B
�B
�B
�B
�B
�B
�B
mB
�B
?B
YB
�B
�B
�B
+B
�B
�B
�B
�B
KB
�B
7B
=B
�B
�B
qB
jB
�B
OB
jB
B
B
B
�B
�B
/B
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
B
dB
�B
�B
OB
 B
 \B
 �B
!B
!-B
 �B
 vB
 \B
 'B
!|B
!�B
!�B
!�B
"NB
#B
#nB
#TB
#nB
#�B
#�B
#�B
$B
$B
%�B
%zB
%zB
%zB
%zB
&2B
&2B
&2B
&2B
&�B
&�B
'8B
'mB
'�B
'�B
(�B
(�B
(�B
)B
)*B
)*B
)DB
)_B
)�B
+6B
+�B
,B
,=B
,qB
,�B
-)B
-]B
-]B
-�B
/ B
.�B
.�B
/ B
/�B
/iB
.�B
.}B
/B
/iB
/�B
/�B
0!B
0�B
1[B
1�B
2|B
2�B
2�B
2�B
2�B
3MB
3�B
4B
4�B
5%B
5�B
5�B
5�B
5�B
6�B
7LB
7LB
7LB
7�B
7�B
8RB
8�B
9XB
9>B
9>B
9>B
9>B
9XB
9�B
:*B
:DB
:�B
:�B
;B
;B
;B
;B
;0B
;B
:�B
;B
;B
;dB
:�B
:xB
:DB
:�B
;�B
;�B
;�B
;dB
;0B
:�B
:�B
;0B
;B
;�B
<6B
=VB
=�B
=�B
=�B
>(B
>]B
>]B
>wB
>wB
>�B
?B
?.B
?}B
?�B
?�B
@B
@iB
@�B
AB
AUB
A�B
B'B
BuB
BuB
BuB
B�B
B�B
B�B
CGB
C-B
C-B
CaB
C-B
CGB
CGB
CaB
C{B
C{B
DMB
DB
C�B
C�B
C{B
C�B
C�B
DMB
DB
DgB
D�B
D�B
D�B
D�B
DMB
E9B
D�B
DMB
B�B
C-B
C�B
D�B
ESB
E�B
E�B
E�B
E�B
E�B
G_B
GzB
G�B
G�B
HB
HB
G�B
G�B
GzB
GB
GEB
HKB
H�B
H�B
IB
I7B
JrB
J�B
K)B
K^B
K)B
K)B
K�B
L�B
L�B
L�B
L�B
MPB
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
OBB
O\B
O�B
O�B
O�B
PHB
QB
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
R B
R B
R:B
R�B
R�B
SB
S&B
SB
S�B
S�B
S�B
S�B
S�B
S�B
T{B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UMB
UMB
U2B
UMB
UMB
U�B
VmB
V�B
V�B
W$B
W�B
X_B
X�B
X�B
X�B
YB
YKB
Y�B
ZB
ZkB
Z�B
Z�B
Z�B
[	B
[=B
[qB
[�B
\)B
\)B
\�B
\�B
\�B
]IB
]~B
]dB
]~B
]�B
]�B
^B
^B
^5B
^OB
^5B
_B
_VB
_�B
_�B
_�B
`B
`vB
`�B
a-B
`�B
abB
a�B
a�B
a�B
bNB
bhB
b�B
b�B
b�B
b�B
cB
cnB
cnB
c�B
c�B
c�B
d@B
dZB
dZB
d�B
d�B
d�B
eFB
ezB
e�B
fB
f2B
fB
ffB
ffB
f�B
f�B
f�B
gB
g�B
g�B
g�B
h
B
h>B
hXB
h�B
h�B
h�B
h�B
h�B
i*B
i�B
jB
j0B
j�B
k�B
k�B
l"B
l"B
lqB
l�B
l�B
l�B
l=B
lB
lB
lB
k�B
k�B
lB
l"B
l�B
l�B
mB
mCB
m]B
mwB
m�B
m�B
ncB
n}B
n�B
o B
p!B
p!B
p;B
poB
p�B
p�B
p�B
qB
qvB
q�B
q�B
q�B
raB
raB
r�B
r�B
s3B
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
t�B
u?B
u�B
u�B
u�B
vB
vFB
v`B
v`B
v�B
wLB
w2B
w�B
w�B
w�B
w�B
w�B
x8B
x8B
x�B
x�B
x�B
x�B
x�B
x�B
yXB
y�B
zB
z^B
zxB
zxB
z�B
z�B
{0B
{0B
{dB
{�B
{�B
{�B
|6B
|B
|PB
|�B
|jB
|�B
|�B
|�B
|�B
|�B
}B
}B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~]B
~�B
HB
�B
�B
�B
�B
�B
�B
�B
�OB
�iB
�iB
�4B
�4B
�B
�4B
�OB
�iB
��B
�iB
�iB
��B
� B
��B
� B
�;B
�;B
�UB
��B
��B
��B
��B
��B
�AB
�[B
�[B
�uB
��B
��B
�B
��B
��B
�B
�B
�-B
�B
��B
��B
�B
�9B
�B
�9B
�9B
�mB
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BshBs3Br�Br�BraBrGBr-BraBs�Bs�Bs�Bs�BraBr|Br�Br�Br�Bs�Bs�Br�BrBq�Bq�Bq�Bq�Bq�Bq�Bq�Bu�B	�fB
F?B
��By>B��BڠB�_B�Bz�B��BtBc�B]IBL�BL~B9>B,�BB
��B
��B
�OB
�B
�xB
��B
��B
��B
��B
�-B
i�B
A�B
�B
�B	�B	�B	�;B	�B	�B	�0B	��B	cTB	KB	G+B	?�B	:�B	5�B	&B	]B	,B	^B	EB	B	[B	�B	AB��B��B��B�"B��B	 �B��B	�B��B	 4B	�B	YB	�B		7B	\B	�B	+B	�B	,�B	4�B	=VB	O(B	T{B	[#B	gmB	p!B	o�B	v�B	�3B	�\B	�[B	��B	��B	��B	�>B	��B	��B	�B	��B	�aB	�gB	��B	��B	��B	�XB	ɆB	��B	�B	�\B	�vB	�vB	�gB	�5B	�B	��B	ʌB	�=B	�=B	ʦB	�B	�jB	�PB	��B	��B	�EB	�CB	�QB	��B	��B	ևB	�'B	�B	�B	�B	��B	�B	��B
AB
B
	RB
B
KB
0B
�B
# B
$�B
"NB
 BB
#�B
(sB
&2B
"hB
&�B
'�B
)*B
*KB
*0B
)DB
)yB
)*B
+6B
+�B
,qB
-CB
/�B
/�B
0!B
/�B
/�B
.IB
,=B
+�B
+B
*�B
+QB
*0B
(�B
'mB
'8B
(
B
'�B
&�B
)B
)B
*B
+6B
*B
(�B
'�B
'B
'B
'B
'B
%B
$&B
#TB
"hB
!�B
 'B
�B
�B
�B
�B
QB
B
#B
=B
/B
/B
]B
�B
kB
/B
B
�B
�B
kB
B
B
�B
+B
�B
WB
	B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
PB
�B
�B
B

�B
KB
�B
YB
�B
�B
�B
	lB
�B
�B
�B
mB
�B
9B
	B
	�B
KB
�B
�B
'B
AB
�B
{B
{B
GB
�B
aB
GB
-B
-B
B
GB
-B
-B
�B
�B
�B
gB
�B
gB
�B
�B
�B
�B
gB
B
gB
B
�B
�B
3B
�B
�B
9B
9B
B
9B
�B
1B
�B
B
	B

rB

	B

�B
^B
xB
�B
B
�B
�B

rB
	�B
	�B
JB
0B

�B

�B

�B
^B
\B
�B
"B
B
^B
DB
�B
�B
dB
�B
"B
B
6B
JB
0B
xB
^B
dB
6B
B
jB
�B
PB
pB
�B
�B
�B
B
PB
jB
jB
�B
�B
�B
B
B
�B
�B
�B
�B
NB
�B
hB
hB
hB
NB
B
�B
�B
�B
:B
:B
:B
oB
B
{B
�B
�B
�B
2B
�B
mB
�B
�B
�B
�B
�B
�B
�B
�B
�B
mB
�B
?B
YB
�B
�B
�B
+B
�B
�B
�B
�B
KB
�B
7B
=B
�B
�B
qB
jB
�B
OB
jB
B
B
B
�B
�B
/B
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
B
dB
�B
�B
OB
 B
 \B
 �B
!B
!-B
 �B
 vB
 \B
 'B
!|B
!�B
!�B
!�B
"NB
#B
#nB
#TB
#nB
#�B
#�B
#�B
$B
$B
%�B
%zB
%zB
%zB
%zB
&2B
&2B
&2B
&2B
&�B
&�B
'8B
'mB
'�B
'�B
(�B
(�B
(�B
)B
)*B
)*B
)DB
)_B
)�B
+6B
+�B
,B
,=B
,qB
,�B
-)B
-]B
-]B
-�B
/ B
.�B
.�B
/ B
/�B
/iB
.�B
.}B
/B
/iB
/�B
/�B
0!B
0�B
1[B
1�B
2|B
2�B
2�B
2�B
2�B
3MB
3�B
4B
4�B
5%B
5�B
5�B
5�B
5�B
6�B
7LB
7LB
7LB
7�B
7�B
8RB
8�B
9XB
9>B
9>B
9>B
9>B
9XB
9�B
:*B
:DB
:�B
:�B
;B
;B
;B
;B
;0B
;B
:�B
;B
;B
;dB
:�B
:xB
:DB
:�B
;�B
;�B
;�B
;dB
;0B
:�B
:�B
;0B
;B
;�B
<6B
=VB
=�B
=�B
=�B
>(B
>]B
>]B
>wB
>wB
>�B
?B
?.B
?}B
?�B
?�B
@B
@iB
@�B
AB
AUB
A�B
B'B
BuB
BuB
BuB
B�B
B�B
B�B
CGB
C-B
C-B
CaB
C-B
CGB
CGB
CaB
C{B
C{B
DMB
DB
C�B
C�B
C{B
C�B
C�B
DMB
DB
DgB
D�B
D�B
D�B
D�B
DMB
E9B
D�B
DMB
B�B
C-B
C�B
D�B
ESB
E�B
E�B
E�B
E�B
E�B
G_B
GzB
G�B
G�B
HB
HB
G�B
G�B
GzB
GB
GEB
HKB
H�B
H�B
IB
I7B
JrB
J�B
K)B
K^B
K)B
K)B
K�B
L�B
L�B
L�B
L�B
MPB
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
OBB
O\B
O�B
O�B
O�B
PHB
QB
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
R B
R B
R:B
R�B
R�B
SB
S&B
SB
S�B
S�B
S�B
S�B
S�B
S�B
T{B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UMB
UMB
U2B
UMB
UMB
U�B
VmB
V�B
V�B
W$B
W�B
X_B
X�B
X�B
X�B
YB
YKB
Y�B
ZB
ZkB
Z�B
Z�B
Z�B
[	B
[=B
[qB
[�B
\)B
\)B
\�B
\�B
\�B
]IB
]~B
]dB
]~B
]�B
]�B
^B
^B
^5B
^OB
^5B
_B
_VB
_�B
_�B
_�B
`B
`vB
`�B
a-B
`�B
abB
a�B
a�B
a�B
bNB
bhB
b�B
b�B
b�B
b�B
cB
cnB
cnB
c�B
c�B
c�B
d@B
dZB
dZB
d�B
d�B
d�B
eFB
ezB
e�B
fB
f2B
fB
ffB
ffB
f�B
f�B
f�B
gB
g�B
g�B
g�B
h
B
h>B
hXB
h�B
h�B
h�B
h�B
h�B
i*B
i�B
jB
j0B
j�B
k�B
k�B
l"B
l"B
lqB
l�B
l�B
l�B
l=B
lB
lB
lB
k�B
k�B
lB
l"B
l�B
l�B
mB
mCB
m]B
mwB
m�B
m�B
ncB
n}B
n�B
o B
p!B
p!B
p;B
poB
p�B
p�B
p�B
qB
qvB
q�B
q�B
q�B
raB
raB
r�B
r�B
s3B
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
t�B
u?B
u�B
u�B
u�B
vB
vFB
v`B
v`B
v�B
wLB
w2B
w�B
w�B
w�B
w�B
w�B
x8B
x8B
x�B
x�B
x�B
x�B
x�B
x�B
yXB
y�B
zB
z^B
zxB
zxB
z�B
z�B
{0B
{0B
{dB
{�B
{�B
{�B
|6B
|B
|PB
|�B
|jB
|�B
|�B
|�B
|�B
|�B
}B
}B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~]B
~�B
HB
�B
�B
�B
�B
�B
�B
�B
�OB
�iB
�iB
�4B
�4B
�B
�4B
�OB
�iB
��B
�iB
�iB
��B
� B
��B
� B
�;B
�;B
�UB
��B
��B
��B
��B
��B
�AB
�[B
�[B
�uB
��B
��B
�B
��B
��B
�B
�B
�-B
�B
��B
��B
�B
�9B
�B
�9B
�9B
�mB
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105250  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192959  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192959  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192959                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605043008  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605043008  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                