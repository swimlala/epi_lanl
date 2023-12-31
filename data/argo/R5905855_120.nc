CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:32:20Z creation;2022-06-04T19:32:21Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604193220  20220610161506  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               xA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @���/hL1   @�� a#@0Ffffff�c�ě��T1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�33A   A!��A@  A`  A~ffA�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�ffB���B�  B�33B���B���B�  B�  B�  B�33B���B�  B�ffB�B�33B�  B�  C   C  C�C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"L�C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DxfDx�fDy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@u@�{@��HA
>A=p�A]p�A{�
A��RA��RA��A��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��B��B��B��B��B��B��B��B��B��B��GB��GB�zB�G�BîB��GB�z�B�z�BӮB׮BۮB��GB�z�B�B�zB�G�B��GB��B��B��C�
C�C�pC�
C	�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C"#�C#�pC%�
C'�
C)�
C+�
C-�
C/�
C1�
C3�
C5�
C7�
C9�
C;�
C=�
C?�
CA�
CC�
CE�
CG�
CI�
CK�
CM�
CO�CQ�
CS�
CU�
CW�
CY�pC[�
C]�
C_�
Ca�
Cc�
Ce�
Cg�
Ci�
Ck�
Cm�
Co�
Cq�
Cs�
Cu�
Cw�
Cy�
C{�
C}�
C�
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D|)D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw�)Dx|)Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�7�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��VA��lA���A��VA��cA���A��A��DA�YA��A�	A�	�A�
�A��A��A�PA�PA�PA�PA�
	A��(A��A��NA���A�ŢAд9AИ�A�i�A�Y�A�HKA�6zA�*0A��A��+AϤ@A�qA�C-A�eA���AΛ	A�v`A�m]A�^5A�OBA�FtA�0�A��/A��tAͽ<AͭA�S[A�yrA�|�Aƚ�A���A�4nA��BA�C�A�רA��hA�x8A�	A�C-A��+A��A��xA�_A��SA�.}A��A�u%A��\A��tA��A���A�n�A���A��A�49A�9$A~�AzZAw�Avn�As�`Ane�Aj�AAi|Ah �Ae��Ab<�A]_pAW��ATe�AR
=APS�AL�TAL.�AH��AEFtA@��A=QA=1A:�A9
�A7$A60UA2�XA0=�A.1�A-��A-�A,XA*��A)�A'��A'	lA&�A%��A%��A%|�A%CA$<6A#�
A$u%A%�A$	lA"�6A"�A"7A!VmA ��A �^AOA��AC-A��A�A~�AzxA�9A�A�HA�*A��A��AA��AA!�A�"AF�A�vA�A�Ay>AjAdZA�mAl"A,�A^5A$A�A�UA|A��Av�A[�AQ�A/�A�A?A�2A2�A�A��A'�A�MAخAƨAW�A�A%A��A�A�Ag�AxA
v�A
;A	�4A�MAs�AJA�[A�gAkQA�`A�xA�A��A�AB[A�`AaAJ#A�A��A�tA��A?�A�#A��As�A�A�A�zAv`A@OA1�A  A �[A �}A `�A _@�zx@���@�5�@�[W@��@�X�@��u@���@�_@��M@�`�@�"h@�I�@�o@��j@�D@��@�@�S@�@���@�P�@�v`@�Vm@��H@�b@�2�@��@쎊@�Z@��N@��8@�c @�b@�˒@�o @�8@��@��'@�|�@�{@��
@�:�@�|�@�\)@�v�@��@��]@㯸@��@��@��,@�\@��H@�8�@�	@��9@�g�@޷�@�-@�!@��@��@ݐ�@�%@��@�\�@�[�@�)_@��@�GE@�x@�Ov@غ�@���@ؑ�@�$t@և�@�G@խC@�S�@���@�4@�N�@��6@�
=@а�@��m@�=�@ΝI@�q@�R�@˺^@��p@�c @�4n@� �@ɶF@�t�@�$t@ȁo@���@�F�@�c @��T@�o @�@ġb@�}V@�c�@�?@��@�G@�˒@�[W@§@���@��@���@���@���@��z@��@���@��D@��A@�s�@�v�@�y>@�d�@�j@�G@�P�@���@��n@�u%@��@��M@��@���@�y>@�S�@���@���@�A�@���@�Ta@�_@��w@�P�@�+@��@��I@�c @�_@���@�O�@��u@�	@�|�@��@���@��[@��R@�i�@��@��=@��@�%�@��r@��@���@���@��X@���@�Dg@���@���@�W�@��@��q@�m]@�E9@��@��$@�xl@�0U@��T@���@�L�@�33@��v@��+@�_@��m@���@��V@��4@�^�@��v@���@���@�}V@���@���@�B�@�Ĝ@�Ft@��@��@�Z�@��@��@��@��!@��o@�i�@�\�@�J�@��+@���@�Vm@��f@�~(@��@��^@���@�8�@�ȴ@��A@�tT@�ff@�Xy@�C-@��@���@�x�@�#�@��,@�u�@�Ta@�)�@��&@���@��@�Z�@��@���@��B@��@���@�_�@�.�@�O@��@���@�{J@�4@��@��y@���@�)�@���@���@�o @�A @���@��@���@�s�@�4n@�1�@�2�@�(�@���@���@�w2@�Dg@�
=@��H@���@�c @�7�@�e@� \@��b@�L0@�-�@�~@��Z@���@���@�O�@��!@�Q�@��@���@���@��"@�v`@�n/@�l�@�_p@�X@�Q�@�V@���@���@�>B@�_@���@��@��F@���@��S@���@�dZ@��@��@��O@�S�@�b@��)@���@���@�g�@�A�@�q@���@�Ɇ@��z@�z�@�r�@�YK@��9@��t@���@���@���@���@��f@�6z@��"@�ߤ@���@���@�U2@�خ@���@�n/@�N<@�4�@��H@��o@�xl@�?�@��@���@�6z@�&@��p@�ff@�-�@�@��@�qv@�)_@��@���@�D�@���@��h@�F@���@���@���@��@�V�@�S�@�A�@�=q@� �@�[@;d@�@~��@~O@|ѷ@|��@|%�@{��@{��@{t�@{Z�@{;d@z�R@yϫ@y7L@x�?@x�u@xr�@xb@w�	@v�@vV@v)�@u�@u��@uN<@u?}@t�E@sX�@r��@q�3@qq@pĜ@p�@p_@p�@oƨ@oa@o�@n��@m�@m��@mw2@l�@lG@kMj@j�y@j��@j��@js�@j-@j �@i��@is�@i�@h֡@hx@f�L@e��@eu�@d��@d��@dQ�@d�@cA�@b҉@b�r@bff@bR�@bTa@b0U@b4@a�@au�@`�@`-�@_��@^�2@^p;@^-@^@]�z@]Y�@]�@\�@\>B@\'R@\"h@[�@Z��@Z}V@Z3�@Y�@Y[W@Y�@X�4@X[�@X�@W��@W��@W�
@W��@Wo�@W8@W'�@V��@V$�@U�d@Uhs@U+�@T��@TbN@T�@S�@S˒@S��@S��@SE9@ROv@Q�@Q�-@Qzx@QY�@Q@@P�@P��@P�I@O�@O��@O]�@O;d@OY@N��@N1�@N�@M��@Mc@L��@L�e@Lw�@L1@K��@K�a@KMj@K�@J�]@J+k@I�S@H��@H�.@H$@G�0@G��@GO@G�@F��@F��@F&�@F�@E��@E��@E8�@Dj@C�A@C'�@Bu%@B)�@A��@A��@As�@@�O@@@?��@?�F@?�@@?�P@?e�@>�8@>͟@>��@>=q@>_@=�@=�@<l"@<  @;��@;�k@;a@:��@:�1@:W�@9��@9O�@9&�@8��@8Ɇ@8�9@8|�@7��@6�@6n�@6&�@6_@6 �@5�Z@5��@5u�@5Vm@5IR@5 \@4ی@4l"@4x@3�+@3��@3l�@3@2l�@2-@1��@1s�@1?}@0��@0Z@/�*@/�	@/v`@/X�@/$t@.��@.
�@-u�@-\�@-a�@-e,@-\�@-�@,��@,�|@,Ɇ@,�z@,��@,��@,~(@,bN@+� @+@O@*�H@)�T@)zx@(��@(�@(bN@(*�@'�@'� @'�@'��@'qv@'C�@'"�@'�@&��@&ߤ@&�@&�@&s�@%��@$��@$��@$q@$q@$_@$PH@$N�@$`�@$(�@#��@#�@#t�@#(@"� @"YK@"H�@"B[@"�@!��@!F@!+�@!�@ �P@ ��@ �@ ��@ <�@ /�@ @ x@��@O@�@��@��@��@c@}�@=�@V@�@��@�p@��@��@e�@�r@��@�f@Z�@
=@��@Ta@{@�@�@ϫ@�"@5�@�@��@��@u�@j@Ft@-�@(�@(�@�@��@�@خ@�
@�w@]�@�@�@�R@��@�\@��@~�@��@Ov@C�@�@u�@4@@��@�@��@v`@O@4�@�2@��@�x@�@u%@h
@&�@�@��@c@F@��@�@��@$@�A@g�@�!@�@��@��@H�@�@�Z@�3@��@x�@c�@8�@@@�|@�?@��@�O@�z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��VA��lA���A��VA��cA���A��A��DA�YA��A�	A�	�A�
�A��A��A�PA�PA�PA�PA�
	A��(A��A��NA���A�ŢAд9AИ�A�i�A�Y�A�HKA�6zA�*0A��A��+AϤ@A�qA�C-A�eA���AΛ	A�v`A�m]A�^5A�OBA�FtA�0�A��/A��tAͽ<AͭA�S[A�yrA�|�Aƚ�A���A�4nA��BA�C�A�רA��hA�x8A�	A�C-A��+A��A��xA�_A��SA�.}A��A�u%A��\A��tA��A���A�n�A���A��A�49A�9$A~�AzZAw�Avn�As�`Ane�Aj�AAi|Ah �Ae��Ab<�A]_pAW��ATe�AR
=APS�AL�TAL.�AH��AEFtA@��A=QA=1A:�A9
�A7$A60UA2�XA0=�A.1�A-��A-�A,XA*��A)�A'��A'	lA&�A%��A%��A%|�A%CA$<6A#�
A$u%A%�A$	lA"�6A"�A"7A!VmA ��A �^AOA��AC-A��A�A~�AzxA�9A�A�HA�*A��A��AA��AA!�A�"AF�A�vA�A�Ay>AjAdZA�mAl"A,�A^5A$A�A�UA|A��Av�A[�AQ�A/�A�A?A�2A2�A�A��A'�A�MAخAƨAW�A�A%A��A�A�Ag�AxA
v�A
;A	�4A�MAs�AJA�[A�gAkQA�`A�xA�A��A�AB[A�`AaAJ#A�A��A�tA��A?�A�#A��As�A�A�A�zAv`A@OA1�A  A �[A �}A `�A _@�zx@���@�5�@�[W@��@�X�@��u@���@�_@��M@�`�@�"h@�I�@�o@��j@�D@��@�@�S@�@���@�P�@�v`@�Vm@��H@�b@�2�@��@쎊@�Z@��N@��8@�c @�b@�˒@�o @�8@��@��'@�|�@�{@��
@�:�@�|�@�\)@�v�@��@��]@㯸@��@��@��,@�\@��H@�8�@�	@��9@�g�@޷�@�-@�!@��@��@ݐ�@�%@��@�\�@�[�@�)_@��@�GE@�x@�Ov@غ�@���@ؑ�@�$t@և�@�G@խC@�S�@���@�4@�N�@��6@�
=@а�@��m@�=�@ΝI@�q@�R�@˺^@��p@�c @�4n@� �@ɶF@�t�@�$t@ȁo@���@�F�@�c @��T@�o @�@ġb@�}V@�c�@�?@��@�G@�˒@�[W@§@���@��@���@���@���@��z@��@���@��D@��A@�s�@�v�@�y>@�d�@�j@�G@�P�@���@��n@�u%@��@��M@��@���@�y>@�S�@���@���@�A�@���@�Ta@�_@��w@�P�@�+@��@��I@�c @�_@���@�O�@��u@�	@�|�@��@���@��[@��R@�i�@��@��=@��@�%�@��r@��@���@���@��X@���@�Dg@���@���@�W�@��@��q@�m]@�E9@��@��$@�xl@�0U@��T@���@�L�@�33@��v@��+@�_@��m@���@��V@��4@�^�@��v@���@���@�}V@���@���@�B�@�Ĝ@�Ft@��@��@�Z�@��@��@��@��!@��o@�i�@�\�@�J�@��+@���@�Vm@��f@�~(@��@��^@���@�8�@�ȴ@��A@�tT@�ff@�Xy@�C-@��@���@�x�@�#�@��,@�u�@�Ta@�)�@��&@���@��@�Z�@��@���@��B@��@���@�_�@�.�@�O@��@���@�{J@�4@��@��y@���@�)�@���@���@�o @�A @���@��@���@�s�@�4n@�1�@�2�@�(�@���@���@�w2@�Dg@�
=@��H@���@�c @�7�@�e@� \@��b@�L0@�-�@�~@��Z@���@���@�O�@��!@�Q�@��@���@���@��"@�v`@�n/@�l�@�_p@�X@�Q�@�V@���@���@�>B@�_@���@��@��F@���@��S@���@�dZ@��@��@��O@�S�@�b@��)@���@���@�g�@�A�@�q@���@�Ɇ@��z@�z�@�r�@�YK@��9@��t@���@���@���@���@��f@�6z@��"@�ߤ@���@���@�U2@�خ@���@�n/@�N<@�4�@��H@��o@�xl@�?�@��@���@�6z@�&@��p@�ff@�-�@�@��@�qv@�)_@��@���@�D�@���@��h@�F@���@���@���@��@�V�@�S�@�A�@�=q@� �@�[@;d@�@~��@~O@|ѷ@|��@|%�@{��@{��@{t�@{Z�@{;d@z�R@yϫ@y7L@x�?@x�u@xr�@xb@w�	@v�@vV@v)�@u�@u��@uN<@u?}@t�E@sX�@r��@q�3@qq@pĜ@p�@p_@p�@oƨ@oa@o�@n��@m�@m��@mw2@l�@lG@kMj@j�y@j��@j��@js�@j-@j �@i��@is�@i�@h֡@hx@f�L@e��@eu�@d��@d��@dQ�@d�@cA�@b҉@b�r@bff@bR�@bTa@b0U@b4@a�@au�@`�@`-�@_��@^�2@^p;@^-@^@]�z@]Y�@]�@\�@\>B@\'R@\"h@[�@Z��@Z}V@Z3�@Y�@Y[W@Y�@X�4@X[�@X�@W��@W��@W�
@W��@Wo�@W8@W'�@V��@V$�@U�d@Uhs@U+�@T��@TbN@T�@S�@S˒@S��@S��@SE9@ROv@Q�@Q�-@Qzx@QY�@Q@@P�@P��@P�I@O�@O��@O]�@O;d@OY@N��@N1�@N�@M��@Mc@L��@L�e@Lw�@L1@K��@K�a@KMj@K�@J�]@J+k@I�S@H��@H�.@H$@G�0@G��@GO@G�@F��@F��@F&�@F�@E��@E��@E8�@Dj@C�A@C'�@Bu%@B)�@A��@A��@As�@@�O@@@?��@?�F@?�@@?�P@?e�@>�8@>͟@>��@>=q@>_@=�@=�@<l"@<  @;��@;�k@;a@:��@:�1@:W�@9��@9O�@9&�@8��@8Ɇ@8�9@8|�@7��@6�@6n�@6&�@6_@6 �@5�Z@5��@5u�@5Vm@5IR@5 \@4ی@4l"@4x@3�+@3��@3l�@3@2l�@2-@1��@1s�@1?}@0��@0Z@/�*@/�	@/v`@/X�@/$t@.��@.
�@-u�@-\�@-a�@-e,@-\�@-�@,��@,�|@,Ɇ@,�z@,��@,��@,~(@,bN@+� @+@O@*�H@)�T@)zx@(��@(�@(bN@(*�@'�@'� @'�@'��@'qv@'C�@'"�@'�@&��@&ߤ@&�@&�@&s�@%��@$��@$��@$q@$q@$_@$PH@$N�@$`�@$(�@#��@#�@#t�@#(@"� @"YK@"H�@"B[@"�@!��@!F@!+�@!�@ �P@ ��@ �@ ��@ <�@ /�@ @ x@��@O@�@��@��@��@c@}�@=�@V@�@��@�p@��@��@e�@�r@��@�f@Z�@
=@��@Ta@{@�@�@ϫ@�"@5�@�@��@��@u�@j@Ft@-�@(�@(�@�@��@�@خ@�
@�w@]�@�@�@�R@��@�\@��@~�@��@Ov@C�@�@u�@4@@��@�@��@v`@O@4�@�2@��@�x@�@u%@h
@&�@�@��@c@F@��@�@��@$@�A@g�@�!@�@��@��@H�@�@�Z@�3@��@x�@c�@8�@@@�|@�?@��@�O@�z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�EB	�EB	�_B	�EB	�yB	�B	�B	�yB	��B	�B	�1B	��B	��B	��B	��B	��B	�_B	��B	�B	��B	�]B	�B	�pB	��B	�nB	��B	��B	�qB	�wB	��B	��B	�MB	�9B	��B	�!B	��B	�CB	�;B	�8B	�wB	��B	��B	�4B	�iB	�OB	�OB	��B	��B	�KB	ȚB	�1B	ΥB
EB
6�B
kQB
r�B
��B
��B
��B
�cB
�BaB
��B
�\B
��B
��B
��B
��B
āB
��B
�zB
�-B
�`B
�B
�HB
�MB
��B
�/B
xlB
=�B
�B	��B	�)B	��B	��B	��B	�
B	�hB	�4B	��B	v�B	\�B	@OB	-�B	�B	�B	fB	�B	
XB��B�tBөB�?B�*B�RB��B��B�tB��B	B	B	
=B	�B	/B	!HB	�B	eB	�B	~B	VB	!B	$tB	1�B	;�B	LJB	mB	m�B	c:B	o�B	r�B	r�B	r�B	� B	{�B	y	B	�GB	�,B	��B	�tB	�~B	�@B	�EB	��B	��B	�B	�B	��B	�1B	�FB	�fB	�B	�BB	�MB	�B	��B	�2B	�6B	��B	��B	�fB	��B	�;B	�;B	��B	��B	��B	�B	��B	�lB	�^B	�dB	̘B	�HB	�OB	�B	�B	�B	�MB	�?B	��B	�*B	��B	��B	��B	�jB	�(B	��B	�>B	�TB	��B	�B	�B	��B	�&B	�@B	�&B	�B	�B	��B	� B	��B	��B	�B	�KB	�*B	�B	�XB	�kB	�B	��B	�B	��B	�'B	��B	�B	�B	�3B	�B	�B	��B	�B	�LB	��B	�rB	�^B	��B	��B	�`B	�B	�B	��B	��B	�zB	�$B	��B	�B	�*B	��B	�AB	�B	�B	��B	��B	�QB	��B	��B	�FB	�rB	��B	�B	��B	�jB	�^B	�B	�8B	�lB	��B	�B	�B	�aB	�aB	�GB	��B	�	B	��B	�B	�B	�6B	�B	��B	��B	��B	�B	�$B	��B	�DB	�^B	��B	��B	�	B	��B	�2B	�fB	��B	��B	�B	�`B	��B	�2B	��B	��B	��B	�B	��B	�B	��B	�B	�|B	�+B	��B	��B
  B	��B
 B	�HB	��B
B	��B
 4B
 B
 OB
  B	��B	�B	��B	�B	��B
  B
 OB
�B
�B
�B
�B
AB
�B
B
�B
[B
�B
�B
UB
�B
�B
B
%B
�B
B
+B
B
+B
B
+B
�B
tB
YB
tB
�B
zB
zB
zB
zB
�B
�B
�B
�B
�B
EB
�B
_B
�B
�B
�B
oB
;B
UB
�B
B
B
�B
AB
�B
oB
B
�B
B
?B
%B
�B
�B
�B
	7B
�B
�B
�B
�B
	7B
	RB
	�B

	B

=B

rB

�B
)B
�B
B
<B
�B
B
�B
�B
�B
\B
vB
vB
�B
�B
}B
�B
}B
�B
�B
�B
B
4B
hB
�B
 B
TB
�B
oB
oB
 B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
aB
�B
2B
2B
�B
�B
B
B
9B
SB
mB
mB
mB
$B
�B
�B
�B
�B
�B
1B
B
eB
�B
�B
�B
B
B
B
7B
�B
�B
�B
�B
qB
qB
WB
�B
�B
�B
]B
xB
�B
�B
�B
IB
dB
�B
�B
B
B
OB
jB
�B
jB
�B
�B
�B
�B
 'B
 \B
 �B
 �B
 �B
!B
!HB
!HB
!-B
!-B
!B
!HB
!bB
!�B
"B
"�B
# B
#nB
#nB
#B
$�B
$�B
%FB
%`B
%zB
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'B
'RB
'8B
'mB
'�B
'mB
'mB
'mB
'RB
'�B
'�B
(>B
(�B
(�B
(�B
(�B
)*B
)B
)*B
)B
)B
)_B
)yB
)�B
)�B
*KB
*eB
*�B
*�B
+B
+B
+6B
+QB
+kB
+�B
+�B
+�B
+�B
-CB
-]B
-]B
-CB
-CB
-)B
-)B
.B
.cB
.�B
.�B
.}B
/B
/�B
/�B
0B
0;B
0;B
0�B
0�B
0UB
0oB
0;B
0�B
1'B
1[B
2-B
2|B
2�B
2�B
2�B
3�B
3hB
3�B
3�B
49B
4�B
5ZB
5�B
5�B
6+B
6FB
6�B
6�B
6�B
6�B
6�B
6�B
7LB
7�B
7�B
7�B
7�B
8RB
8lB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9>B
9�B
9�B
:B
:B
:DB
:�B
;B
;dB
;JB
;0B
;JB
;0B
:�B
:�B
<PB
<B
<�B
=VB
=�B
=�B
=�B
=�B
=�B
>(B
=�B
>(B
>�B
>�B
>wB
>�B
?�B
@4B
@�B
@�B
AB
AB
@�B
AB
A;B
A�B
BAB
B'B
B�B
B'B
A�B
A�B
B'B
BuB
B[B
BAB
BuB
B�B
B�B
CB
CB
B�B
C-B
CB
CB
C�B
C�B
D�B
D�B
EB
EB
EB
EB
E9B
E�B
E9B
E�B
F%B
F%B
FB
F%B
G_B
G�B
G�B
G�B
H1B
HfB
H�B
I7B
JXB
J�B
J�B
J�B
KxB
K�B
K�B
K�B
LB
LB
LJB
L�B
L�B
M�B
M�B
N<B
N<B
NVB
N<B
N"B
N�B
OB
O(B
OBB
O\B
O\B
OvB
OvB
O\B
OvB
PbB
P�B
Q B
Q B
Q B
Q�B
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
S[B
S�B
S�B
TB
TB
TB
T�B
T�B
UMB
U2B
U�B
U�B
U�B
VB
VB
VB
V9B
V�B
V�B
V�B
V�B
V�B
WsB
W�B
X_B
X�B
X�B
X�B
YKB
Y1B
ZB
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[=B
[#B
[�B
[�B
[�B
\CB
\�B
]/B
]dB
]~B
]�B
^B
^5B
^jB
^�B
_B
_B
_!B
_B
^�B
^�B
_�B
`'B
`BB
`BB
`BB
`BB
`BB
`vB
`�B
`�B
`�B
`�B
a-B
abB
`�B
`�B
`�B
`�B
`�B
aB
a-B
abB
a�B
a�B
b4B
b�B
c B
c B
c B
c:B
cTB
cnB
cnB
c:B
cB
c:B
cTB
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
dtB
d�B
d�B
eB
d�B
ezB
e�B
e�B
e�B
fB
f2B
ffB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
f�B
f�B
f�B
g�B
h$B
hXB
hsB
hXB
hsB
h�B
hsB
h>B
h�B
h�B
iB
h�B
i�B
i�B
jB
jB
i�B
jB
kB
kB
kB
kB
kQB
kkB
k�B
k�B
k�B
lB
l=B
l"B
lWB
l�B
mB
m�B
oB
n�B
o B
n�B
o5B
oiB
o�B
o�B
o�B
o�B
o�B
pB
poB
p�B
p�B
p�B
qB
q�B
q�B
q�B
q�B
q�B
q�B
rB
raB
r�B
r�B
r�B
r�B
sB
sMB
shB
shB
s3B
s3B
sMB
shB
s�B
s�B
s�B
tB
tTB
t9B
tnB
tnB
t�B
t�B
t�B
tTB
t�B
t�B
t�B
u�B
u�B
u�B
v+B
v�B
wLB
wfB
w�B
w�B
xB
xRB
x8B
xRB
xlB
xRB
x�B
x�B
x�B
x�B
y	B
y>B
y>B
yXB
y�B
y�B
z�B
{dB
{dB
{�B
{B
{�B
{�B
|B
|PB
|�B
|�B
|�B
|�B
|�B
}B
}VB
}VB
}VB
}q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�EB	�EB	�_B	�EB	�yB	�B	�B	�yB	��B	�B	�1B	��B	��B	��B	��B	��B	�_B	��B	�B	��B	�]B	�B	�pB	��B	�nB	��B	��B	�qB	�wB	��B	��B	�MB	�9B	��B	�!B	��B	�CB	�;B	�8B	�wB	��B	��B	�4B	�iB	�OB	�OB	��B	��B	�KB	ȚB	�1B	ΥB
EB
6�B
kQB
r�B
��B
��B
��B
�cB
�BaB
��B
�\B
��B
��B
��B
��B
āB
��B
�zB
�-B
�`B
�B
�HB
�MB
��B
�/B
xlB
=�B
�B	��B	�)B	��B	��B	��B	�
B	�hB	�4B	��B	v�B	\�B	@OB	-�B	�B	�B	fB	�B	
XB��B�tBөB�?B�*B�RB��B��B�tB��B	B	B	
=B	�B	/B	!HB	�B	eB	�B	~B	VB	!B	$tB	1�B	;�B	LJB	mB	m�B	c:B	o�B	r�B	r�B	r�B	� B	{�B	y	B	�GB	�,B	��B	�tB	�~B	�@B	�EB	��B	��B	�B	�B	��B	�1B	�FB	�fB	�B	�BB	�MB	�B	��B	�2B	�6B	��B	��B	�fB	��B	�;B	�;B	��B	��B	��B	�B	��B	�lB	�^B	�dB	̘B	�HB	�OB	�B	�B	�B	�MB	�?B	��B	�*B	��B	��B	��B	�jB	�(B	��B	�>B	�TB	��B	�B	�B	��B	�&B	�@B	�&B	�B	�B	��B	� B	��B	��B	�B	�KB	�*B	�B	�XB	�kB	�B	��B	�B	��B	�'B	��B	�B	�B	�3B	�B	�B	��B	�B	�LB	��B	�rB	�^B	��B	��B	�`B	�B	�B	��B	��B	�zB	�$B	��B	�B	�*B	��B	�AB	�B	�B	��B	��B	�QB	��B	��B	�FB	�rB	��B	�B	��B	�jB	�^B	�B	�8B	�lB	��B	�B	�B	�aB	�aB	�GB	��B	�	B	��B	�B	�B	�6B	�B	��B	��B	��B	�B	�$B	��B	�DB	�^B	��B	��B	�	B	��B	�2B	�fB	��B	��B	�B	�`B	��B	�2B	��B	��B	��B	�B	��B	�B	��B	�B	�|B	�+B	��B	��B
  B	��B
 B	�HB	��B
B	��B
 4B
 B
 OB
  B	��B	�B	��B	�B	��B
  B
 OB
�B
�B
�B
�B
AB
�B
B
�B
[B
�B
�B
UB
�B
�B
B
%B
�B
B
+B
B
+B
B
+B
�B
tB
YB
tB
�B
zB
zB
zB
zB
�B
�B
�B
�B
�B
EB
�B
_B
�B
�B
�B
oB
;B
UB
�B
B
B
�B
AB
�B
oB
B
�B
B
?B
%B
�B
�B
�B
	7B
�B
�B
�B
�B
	7B
	RB
	�B

	B

=B

rB

�B
)B
�B
B
<B
�B
B
�B
�B
�B
\B
vB
vB
�B
�B
}B
�B
}B
�B
�B
�B
B
4B
hB
�B
 B
TB
�B
oB
oB
 B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
aB
�B
2B
2B
�B
�B
B
B
9B
SB
mB
mB
mB
$B
�B
�B
�B
�B
�B
1B
B
eB
�B
�B
�B
B
B
B
7B
�B
�B
�B
�B
qB
qB
WB
�B
�B
�B
]B
xB
�B
�B
�B
IB
dB
�B
�B
B
B
OB
jB
�B
jB
�B
�B
�B
�B
 'B
 \B
 �B
 �B
 �B
!B
!HB
!HB
!-B
!-B
!B
!HB
!bB
!�B
"B
"�B
# B
#nB
#nB
#B
$�B
$�B
%FB
%`B
%zB
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'B
'RB
'8B
'mB
'�B
'mB
'mB
'mB
'RB
'�B
'�B
(>B
(�B
(�B
(�B
(�B
)*B
)B
)*B
)B
)B
)_B
)yB
)�B
)�B
*KB
*eB
*�B
*�B
+B
+B
+6B
+QB
+kB
+�B
+�B
+�B
+�B
-CB
-]B
-]B
-CB
-CB
-)B
-)B
.B
.cB
.�B
.�B
.}B
/B
/�B
/�B
0B
0;B
0;B
0�B
0�B
0UB
0oB
0;B
0�B
1'B
1[B
2-B
2|B
2�B
2�B
2�B
3�B
3hB
3�B
3�B
49B
4�B
5ZB
5�B
5�B
6+B
6FB
6�B
6�B
6�B
6�B
6�B
6�B
7LB
7�B
7�B
7�B
7�B
8RB
8lB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9>B
9�B
9�B
:B
:B
:DB
:�B
;B
;dB
;JB
;0B
;JB
;0B
:�B
:�B
<PB
<B
<�B
=VB
=�B
=�B
=�B
=�B
=�B
>(B
=�B
>(B
>�B
>�B
>wB
>�B
?�B
@4B
@�B
@�B
AB
AB
@�B
AB
A;B
A�B
BAB
B'B
B�B
B'B
A�B
A�B
B'B
BuB
B[B
BAB
BuB
B�B
B�B
CB
CB
B�B
C-B
CB
CB
C�B
C�B
D�B
D�B
EB
EB
EB
EB
E9B
E�B
E9B
E�B
F%B
F%B
FB
F%B
G_B
G�B
G�B
G�B
H1B
HfB
H�B
I7B
JXB
J�B
J�B
J�B
KxB
K�B
K�B
K�B
LB
LB
LJB
L�B
L�B
M�B
M�B
N<B
N<B
NVB
N<B
N"B
N�B
OB
O(B
OBB
O\B
O\B
OvB
OvB
O\B
OvB
PbB
P�B
Q B
Q B
Q B
Q�B
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
S[B
S�B
S�B
TB
TB
TB
T�B
T�B
UMB
U2B
U�B
U�B
U�B
VB
VB
VB
V9B
V�B
V�B
V�B
V�B
V�B
WsB
W�B
X_B
X�B
X�B
X�B
YKB
Y1B
ZB
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[=B
[#B
[�B
[�B
[�B
\CB
\�B
]/B
]dB
]~B
]�B
^B
^5B
^jB
^�B
_B
_B
_!B
_B
^�B
^�B
_�B
`'B
`BB
`BB
`BB
`BB
`BB
`vB
`�B
`�B
`�B
`�B
a-B
abB
`�B
`�B
`�B
`�B
`�B
aB
a-B
abB
a�B
a�B
b4B
b�B
c B
c B
c B
c:B
cTB
cnB
cnB
c:B
cB
c:B
cTB
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
dtB
d�B
d�B
eB
d�B
ezB
e�B
e�B
e�B
fB
f2B
ffB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
f�B
f�B
f�B
g�B
h$B
hXB
hsB
hXB
hsB
h�B
hsB
h>B
h�B
h�B
iB
h�B
i�B
i�B
jB
jB
i�B
jB
kB
kB
kB
kB
kQB
kkB
k�B
k�B
k�B
lB
l=B
l"B
lWB
l�B
mB
m�B
oB
n�B
o B
n�B
o5B
oiB
o�B
o�B
o�B
o�B
o�B
pB
poB
p�B
p�B
p�B
qB
q�B
q�B
q�B
q�B
q�B
q�B
rB
raB
r�B
r�B
r�B
r�B
sB
sMB
shB
shB
s3B
s3B
sMB
shB
s�B
s�B
s�B
tB
tTB
t9B
tnB
tnB
t�B
t�B
t�B
tTB
t�B
t�B
t�B
u�B
u�B
u�B
v+B
v�B
wLB
wfB
w�B
w�B
xB
xRB
x8B
xRB
xlB
xRB
x�B
x�B
x�B
x�B
y	B
y>B
y>B
yXB
y�B
y�B
z�B
{dB
{dB
{�B
{B
{�B
{�B
|B
|PB
|�B
|�B
|�B
|�B
|�B
}B
}VB
}VB
}VB
}q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105253  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604193220  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604193221  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604193221                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605043228  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605043228  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161506                      G�O�G�O�G�O�                