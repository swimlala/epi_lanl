CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:30:25Z creation;2022-06-04T19:30:26Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604193025  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               nA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ٹ���j1   @ٹ��X�@-�+I��c��C��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@y��@���A   A   A@  A`  A�  A�33A�  A�  A�  A�  A���A���A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B�33B�ffB�ffB�  B���B�  B�  B�  B�  B�  B�33B�ffB�ffB���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C33C�fC�fC  C�fC�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Du��Dvy�Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @\)@o\)@��@��HAp�A=p�A]p�A}p�A��A��RA��RA��RAθRA߅A�A��B\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��B��B��B��B��B��B��GB��B��GB��GB�zB�zB��B�G�BîBǮBˮBϮBӮB��GB�zB�zB�z�B�B�B�B�B��B��B��C�
C�
C�
C�
C	�
C�C
=C�pC�pC�
C�pC�pC�
C�
C�
C�
C!�
C#�
C%�
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
CO�
CQ�
CS�
CU�
CW�
CY�
C[�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du�]Dvo]Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�>D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�qG1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AЋAВoAФAа�Aв�AЭwAЭA���Aл�A��A��aA��gA��A���A��aA��A��6A�՛A��sA��yA��A���A��A�چA��#A���A���A��]A���A���A���A��/A��A��pA��A�ߤA���A��BA��A���A���A�AƘ_A�b�A�%zA��7A���A���A�7LA���A�D�A��A��8A��A�c�A�S&A���A�]/A�1�A���A��A�8�A�%FA���A�C�A~rGAy�PAv��AsL�Aq	�An�jAlƨAg_�A`�A]��AY�#AV��AVb�AV�AUp�AS>BAQA�APN�AM��ALo AKϫAJ  AI1'AGc AC��AA�LA@{JA>�A<��A;Y�A9�tA7�A5��A4��A3�A3��A2�oA0�$A/�oA.Y�A-�uA,��A,y>A,6A+{�A*GEA)&�A)+A(��A'��A'C-A'OA&�A%h
A#!A"u�A":�A!�jA!�AA �PA�,AeAD�AȴA�RA�9A8A�A�cA�As�A^5A@�A��Al"A��A��A��A�sA�oA�A�AE�A[�A�)AqAxA�_AtTA�.A iA�@A:*A�bA��A�*Ay�A�DA@OA iA�A$A
`�A
bA
FA
PHA
o�A
qA
xlA
=qA
�A	[�Aa�A1A��As�AJ#AA�qAl�A�,A&�A�$A��ACA~(Au�A/A�A��A��Av�A�hA�AYA��A��A��AM�A �#AYKA��Au�AB�A�A�sAq�A��A��A��A8�A 1�A �@��o@�o @�:�@���@���@�,�@���@���@���@��@�� @��@��@���@��
@��@� �@��@�ƨ@�Y@�p�@�b�@�w�@�$�@�*@�@�5�@�~�@�hs@�)�@�E9@�h
@�-@�v`@���@���@�x@�]d@�	@���@鰊@�Z�@�Q�@勬@�/@���@��H@�u@�U2@���@��@��@�@�X@�p;@��@�a�@�kQ@���@�;d@��@��]@ܭ�@܏\@�r�@�V@�b@�:�@�I�@�b�@�~�@ש*@�7L@���@��@�H�@��@���@Ԅ�@�c @ӶF@ҏ\@ѶF@�Mj@�)_@�c�@ϟV@��@�h�@�9X@���@͒:@�F@�=�@�O@��	@ʟ�@�H@���@ɜ�@��@ȧ�@�{�@�?@���@�$t@��,@�6@�:�@Ƈ�@���@���@��@���@�IR@ @�#:@�ƨ@�j@��@��<@�w�@�j@�`�@�	@���@��$@��{@��O@�@��-@�k�@�5�@��X@�!@���@��o@���@���@���@���@�v`@�8@�_@���@�IR@�
=@��@��@�=q@��D@���@�q@���@�C-@�1'@��W@��'@�H�@�#�@��)@��@��~@�\)@�6z@��@���@�z�@��.@�خ@��t@�B�@��@�x@��H@��X@�g�@�V@�Ɇ@�i�@���@�|@�Q�@��@��@�K^@�/�@���@��@�p;@��@��W@���@��@���@���@�=@���@�ߤ@��j@���@�H�@��A@��M@�/@��@��b@�GE@��j@�O@���@��'@���@�j@�!@�~@�7@��@��)@��F@���@��'@�c�@���@��T@��@��@���@���@��h@�j@�;d@��@��m@�oi@��@�u@��&@���@�A�@�(�@� i@���@���@��+@�YK@�4n@��@�  @��Q@��@��@��@��}@���@��@���@���@���@�qv@�$t@���@��@�7�@�ƨ@�S�@��s@���@���@��x@��@��@��@��@�|�@�>B@���@�K�@���@�Ta@���@���@�S&@�@���@�_�@�,=@��t@�f�@��@��@�@@��s@�4n@�|�@�4@��@��H@���@�u%@�	�@��[@���@�W?@�E9@�T�@�,�@���@���@���@�R�@�E�@�:�@�(�@��@��4@��@��9@�g8@�3�@�+k@��@��#@��[@�O�@�C@��@��@��?@��.@�PH@�~@�@��@���@���@�Dg@�(@� i@���@�Z@�M@�6@�+k@��]@���@���@�X�@��?@��@�PH@��@��#@���@�v`@�33@��@��K@���@�3�@S@~��@~B[@}��@}�N@}�S@}e,@|�@{�{@{�@{@z�!@zE�@y�@yG�@x�K@x��@xu�@x>B@w�@v�@v=q@u�@u|@u?}@t��@s��@s&@r�x@q�.@qJ�@p�@p��@pC-@p"h@o��@oy�@o9�@n��@n;�@ne@m��@mY�@l�O@k�g@k�@j!�@i��@iG�@i;@h�@h��@hw�@h,=@g�]@g��@g��@f��@e��@e�@d�@dN�@d:�@c�+@c�P@cZ�@c33@c&@c!-@b�@b��@a�d@a0�@`��@`V�@_��@_9�@_�@^�B@^n�@]�^@]S&@]7L@\��@\2�@[�
@[O@[�@Zߤ@Z��@ZkQ@Z8�@Y�@Yu�@YIR@X�P@X�p@X�j@X�9@XU2@W�@WP�@W'�@V��@V=q@U�@U�-@Uc@U8�@T��@T�@S1�@R��@R:*@Q�@Q=�@Q%@P��@P�.@P@O��@Oj�@OA�@N�"@NM�@M�@M��@M!�@L�@L��@L�@L�@L~(@LM@K�*@K$t@K�@J��@JC�@I�#@I�N@I�@IS&@H��@H��@H~@G�0@G�@F($@F#:@FO@E��@EX@E%@D�/@D��@D�@DFt@C�&@CU�@CC@C�@B�8@B��@B�@Bc @B�@A��@A��@A7L@@�@@�[@@��@@�@@�@@A�@@	�@?�}@?�q@?O@?!-@>�}@>�@=��@>($@=f�@=B�@<A�@;خ@;�:@:��@:�1@:� @9�@8�@84n@7�@7g�@7A�@7/�@7"�@7C@7$t@7/�@7>�@7.I@7(@6��@6!�@5�@5��@5��@5��@5-w@4�P@4��@3�W@2�]@2\�@2�@1�@1m]@1�@0��@0�`@0�@0�`@0�`@0�E@0�@0?�@09X@0~@/��@/S�@.�c@.��@.�@.��@.h
@.	@-rG@-#�@,ی@,��@,]d@,/�@,M@+|�@+�@*�@*��@*z@*B[@)�D@)��@)#�@(�@'�@'��@'��@'�k@'�@'Mj@&�8@&��@&�L@&�h@&{�@&�@&z@&�@%�9@%^�@$�K@$֡@$��@$l"@$Z@$M@$D�@$1'@#�	@#_p@#>�@"��@"��@"�@!��@!m]@!+@ ��@ �@ �/@ ��@ 9X@�@��@�;@�
@�}@�@�0@��@�f@qv@t�@y�@��@x@a@J#@9�@�M@�6@�A@C�@#:@�@��@u�@:�@�@�z@<�@~@�@ƨ@�@y�@Z�@�@��@��@�A@Z�@)�@�@��@�7@�M@|@T�@�@��@��@y>@1@�@s@(@��@�'@��@�A@V@#:@�@�#@�@�t@�S@N<@8�@7L@@�@��@[�@?�@$@b@G@�@��@�F@~�@]�@J#@9�@��@��@�@��@��@�~@�~@N<@�@u�@e�@Xy@1'@@�@�@�[@��@iD@33@@�2@�X@��@��@:*@	@�@��@�d@��@��@J�@�@�K@ی@��@�@��@m�@Xy@Ft@"h@�@��@�4@U�@J#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AЋAВoAФAа�Aв�AЭwAЭA���Aл�A��A��aA��gA��A���A��aA��A��6A�՛A��sA��yA��A���A��A�چA��#A���A���A��]A���A���A���A��/A��A��pA��A�ߤA���A��BA��A���A���A�AƘ_A�b�A�%zA��7A���A���A�7LA���A�D�A��A��8A��A�c�A�S&A���A�]/A�1�A���A��A�8�A�%FA���A�C�A~rGAy�PAv��AsL�Aq	�An�jAlƨAg_�A`�A]��AY�#AV��AVb�AV�AUp�AS>BAQA�APN�AM��ALo AKϫAJ  AI1'AGc AC��AA�LA@{JA>�A<��A;Y�A9�tA7�A5��A4��A3�A3��A2�oA0�$A/�oA.Y�A-�uA,��A,y>A,6A+{�A*GEA)&�A)+A(��A'��A'C-A'OA&�A%h
A#!A"u�A":�A!�jA!�AA �PA�,AeAD�AȴA�RA�9A8A�A�cA�As�A^5A@�A��Al"A��A��A��A�sA�oA�A�AE�A[�A�)AqAxA�_AtTA�.A iA�@A:*A�bA��A�*Ay�A�DA@OA iA�A$A
`�A
bA
FA
PHA
o�A
qA
xlA
=qA
�A	[�Aa�A1A��As�AJ#AA�qAl�A�,A&�A�$A��ACA~(Au�A/A�A��A��Av�A�hA�AYA��A��A��AM�A �#AYKA��Au�AB�A�A�sAq�A��A��A��A8�A 1�A �@��o@�o @�:�@���@���@�,�@���@���@���@��@�� @��@��@���@��
@��@� �@��@�ƨ@�Y@�p�@�b�@�w�@�$�@�*@�@�5�@�~�@�hs@�)�@�E9@�h
@�-@�v`@���@���@�x@�]d@�	@���@鰊@�Z�@�Q�@勬@�/@���@��H@�u@�U2@���@��@��@�@�X@�p;@��@�a�@�kQ@���@�;d@��@��]@ܭ�@܏\@�r�@�V@�b@�:�@�I�@�b�@�~�@ש*@�7L@���@��@�H�@��@���@Ԅ�@�c @ӶF@ҏ\@ѶF@�Mj@�)_@�c�@ϟV@��@�h�@�9X@���@͒:@�F@�=�@�O@��	@ʟ�@�H@���@ɜ�@��@ȧ�@�{�@�?@���@�$t@��,@�6@�:�@Ƈ�@���@���@��@���@�IR@ @�#:@�ƨ@�j@��@��<@�w�@�j@�`�@�	@���@��$@��{@��O@�@��-@�k�@�5�@��X@�!@���@��o@���@���@���@���@�v`@�8@�_@���@�IR@�
=@��@��@�=q@��D@���@�q@���@�C-@�1'@��W@��'@�H�@�#�@��)@��@��~@�\)@�6z@��@���@�z�@��.@�خ@��t@�B�@��@�x@��H@��X@�g�@�V@�Ɇ@�i�@���@�|@�Q�@��@��@�K^@�/�@���@��@�p;@��@��W@���@��@���@���@�=@���@�ߤ@��j@���@�H�@��A@��M@�/@��@��b@�GE@��j@�O@���@��'@���@�j@�!@�~@�7@��@��)@��F@���@��'@�c�@���@��T@��@��@���@���@��h@�j@�;d@��@��m@�oi@��@�u@��&@���@�A�@�(�@� i@���@���@��+@�YK@�4n@��@�  @��Q@��@��@��@��}@���@��@���@���@���@�qv@�$t@���@��@�7�@�ƨ@�S�@��s@���@���@��x@��@��@��@��@�|�@�>B@���@�K�@���@�Ta@���@���@�S&@�@���@�_�@�,=@��t@�f�@��@��@�@@��s@�4n@�|�@�4@��@��H@���@�u%@�	�@��[@���@�W?@�E9@�T�@�,�@���@���@���@�R�@�E�@�:�@�(�@��@��4@��@��9@�g8@�3�@�+k@��@��#@��[@�O�@�C@��@��@��?@��.@�PH@�~@�@��@���@���@�Dg@�(@� i@���@�Z@�M@�6@�+k@��]@���@���@�X�@��?@��@�PH@��@��#@���@�v`@�33@��@��K@���@�3�@S@~��@~B[@}��@}�N@}�S@}e,@|�@{�{@{�@{@z�!@zE�@y�@yG�@x�K@x��@xu�@x>B@w�@v�@v=q@u�@u|@u?}@t��@s��@s&@r�x@q�.@qJ�@p�@p��@pC-@p"h@o��@oy�@o9�@n��@n;�@ne@m��@mY�@l�O@k�g@k�@j!�@i��@iG�@i;@h�@h��@hw�@h,=@g�]@g��@g��@f��@e��@e�@d�@dN�@d:�@c�+@c�P@cZ�@c33@c&@c!-@b�@b��@a�d@a0�@`��@`V�@_��@_9�@_�@^�B@^n�@]�^@]S&@]7L@\��@\2�@[�
@[O@[�@Zߤ@Z��@ZkQ@Z8�@Y�@Yu�@YIR@X�P@X�p@X�j@X�9@XU2@W�@WP�@W'�@V��@V=q@U�@U�-@Uc@U8�@T��@T�@S1�@R��@R:*@Q�@Q=�@Q%@P��@P�.@P@O��@Oj�@OA�@N�"@NM�@M�@M��@M!�@L�@L��@L�@L�@L~(@LM@K�*@K$t@K�@J��@JC�@I�#@I�N@I�@IS&@H��@H��@H~@G�0@G�@F($@F#:@FO@E��@EX@E%@D�/@D��@D�@DFt@C�&@CU�@CC@C�@B�8@B��@B�@Bc @B�@A��@A��@A7L@@�@@�[@@��@@�@@�@@A�@@	�@?�}@?�q@?O@?!-@>�}@>�@=��@>($@=f�@=B�@<A�@;خ@;�:@:��@:�1@:� @9�@8�@84n@7�@7g�@7A�@7/�@7"�@7C@7$t@7/�@7>�@7.I@7(@6��@6!�@5�@5��@5��@5��@5-w@4�P@4��@3�W@2�]@2\�@2�@1�@1m]@1�@0��@0�`@0�@0�`@0�`@0�E@0�@0?�@09X@0~@/��@/S�@.�c@.��@.�@.��@.h
@.	@-rG@-#�@,ی@,��@,]d@,/�@,M@+|�@+�@*�@*��@*z@*B[@)�D@)��@)#�@(�@'�@'��@'��@'�k@'�@'Mj@&�8@&��@&�L@&�h@&{�@&�@&z@&�@%�9@%^�@$�K@$֡@$��@$l"@$Z@$M@$D�@$1'@#�	@#_p@#>�@"��@"��@"�@!��@!m]@!+@ ��@ �@ �/@ ��@ 9X@�@��@�;@�
@�}@�@�0@��@�f@qv@t�@y�@��@x@a@J#@9�@�M@�6@�A@C�@#:@�@��@u�@:�@�@�z@<�@~@�@ƨ@�@y�@Z�@�@��@��@�A@Z�@)�@�@��@�7@�M@|@T�@�@��@��@y>@1@�@s@(@��@�'@��@�A@V@#:@�@�#@�@�t@�S@N<@8�@7L@@�@��@[�@?�@$@b@G@�@��@�F@~�@]�@J#@9�@��@��@�@��@��@�~@�~@N<@�@u�@e�@Xy@1'@@�@�@�[@��@iD@33@@�2@�X@��@��@:*@	@�@��@�d@��@��@J�@�@�K@ی@��@�@��@m�@Xy@Ft@"h@�@��@�4@U�@J#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�+B��B�YB�%B�?B�tB�tB��B�YB��B�%B�%B�?B�B�?B�%B�?B�%B�%B�YB��BƨB��BƨB��B��B��B��B��B�EB��B�1B�KB�B�KB�fBȀB��B�7B�RB	~B	�B
�BB
��B
�]B
�XB
��BBQB�B
�RB
ևB
�DB
��B
��B
�`B
��B
��B
�hB
�(B
�=B
q�B
J�B
&�B
 iB	߾B	�\B	�aB	�B	�iB	�nB	�1B	c:B	T,B	D�B	8�B	5?B	3�B	0oB	+�B	)*B	$�B	!B	�B	�B	�B	�B	B	�B�0B�fB�hB� B�
B��B�sB�B��B�nB�NB�@B�2B�B�B�AB�ZB��B�B	�B	6B	 BB	)�B	2�B	7�B	7�B	8B	8�B	8�B	2�B	5ZB	GB	J#B	R�B	T�B	T,B	Z�B	h$B	q�B	t�B	vB	x�B	h$B	f�B	h�B	raB	uB	z�B	�;B	��B	��B	�B	��B	ңB	ѝB	̳B	��B	��B	�B	�B	��B	��B	�AB	�vB	�[B	�B	��B	��B	�B	�$B	�yB	�kB	��B	��B	�oB	�B	�3B	��B	�B	�AB	�+B	��B	��B	�BB
uB
�B
B
-B
�B
�B
YB
B
aB
mB
SB
  B	�B	��B	�B
�B
�B
"B
(B
KB
KB
�B
YB
qB
eB
 B
 B
}B
�B
zB
MB
�B
�B
eB
+B
+�B
/�B
0�B
0!B
/�B
.�B
,�B
&�B
&2B
*B
*�B
(XB
#�B
"4B
$@B
&LB
%�B
#�B
�B
�B
�B
B
�B
QB
]B
�B
VB
!B
�B
2B
�B
�B
�B
�B
B
dB
B
�B
IB
	B
�B
7B
B
B
�B
�B
eB
1B
�B
+B
�B
,B
FB
,B
�B
�B
[B
B
�B
:B
�B
�B
hB
�B
�B
�B
bB
�B
�B
�B
�B
B
.B
bB
.B
B
�B
�B
 B
TB
B
�B
�B
�B
�B
�B
�B
vB
B
�B
"B
�B
B
JB
^B
�B
�B

	B
DB

�B

#B
	lB

	B

�B
	RB
�B
�B
	�B
	7B
�B
�B
	�B
B
xB

	B
�B
�B

#B
B
PB
�B
�B
6B
�B
B
<B
"B
�B
jB
�B
�B
B
PB
B
�B
�B
�B
�B
PB
PB
B
�B
�B
JB
�B
�B
B
B
6B
PB
�B
PB
<B
jB
B
�B
�B
<B
�B
�B
�B
(B
\B
�B
�B
B
�B
�B
�B
�B
vB
�B
�B
�B
�B
�B
B
bB
.B
B
HB
4B
hB
4B
B
�B
�B
�B
B
�B
TB
:B
oB
�B
�B
TB
�B
�B
�B
B
�B
�B
�B
,B
gB
�B
SB
SB
mB
�B
mB
�B
sB
�B
�B
�B
+B
_B
�B
KB
eB
eB
�B
B
B
B
B
7B
kB
QB
7B
QB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
dB
dB
�B
�B
�B
B
B
5B
5B
5B
jB
�B
;B
;B
pB
 B
 'B
 B
 B
 BB
 �B
 �B
!�B
!|B
!bB
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"4B
#�B
#:B
"hB
!|B
!B
!�B
!�B
"NB
"�B
#:B
# B
#B
#TB
#�B
$�B
%�B
%�B
%�B
%zB
%FB
%,B
%FB
%�B
%�B
&B
'�B
($B
'�B
(
B
(�B
*�B
+6B
+�B
,�B
-CB
.IB
.cB
.cB
.cB
-�B
,�B
,�B
-�B
.IB
.�B
0!B
0�B
0�B
0�B
1[B
1�B
1�B
1�B
1�B
1�B
2B
2B
2-B
2B
2-B
2aB
2�B
2-B
2-B
1�B
1�B
2|B
2�B
3B
3�B
49B
4TB
4�B
5B
5tB
5�B
5�B
5�B
5�B
6B
6zB
6�B
6`B
7fB
7�B
7LB
7B
7fB
7�B
7�B
7�B
7�B
7�B
7�B
7LB
72B
7�B
8B
9XB
9�B
:DB
:xB
:�B
:�B
:�B
;B
:�B
:�B
;JB
;dB
<PB
<�B
=�B
=�B
>(B
>wB
>�B
>�B
?.B
?B
?.B
?cB
?}B
@B
@OB
@OB
@�B
A B
B'B
B�B
B�B
B�B
B�B
CB
CGB
CGB
C�B
C�B
DB
DMB
DMB
DMB
DMB
D�B
EB
EmB
FB
F�B
F?B
FtB
FtB
FtB
F�B
FtB
F�B
F�B
GEB
G�B
G�B
HKB
HfB
H�B
H�B
H�B
IB
H�B
H�B
H�B
IlB
J	B
I�B
J�B
J�B
JrB
JXB
J	B
J	B
J#B
J=B
J	B
J=B
J=B
J	B
J	B
JrB
J�B
J�B
J�B
J�B
J�B
J�B
K)B
KB
KDB
KxB
LdB
MB
MjB
NB
N�B
N�B
OBB
OBB
N�B
N�B
NVB
N"B
N<B
N<B
N�B
N�B
N�B
OB
OBB
O\B
O�B
QhB
Q�B
RB
Q�B
Q�B
R B
R B
R�B
S&B
S@B
S@B
S�B
TFB
T{B
UMB
UMB
U�B
VB
VB
U�B
U�B
VB
U�B
U�B
V�B
W�B
W�B
W�B
WsB
WYB
WYB
W?B
W�B
W�B
WsB
W?B
WYB
W?B
W�B
W�B
XB
XB
X+B
X�B
XyB
X�B
Y�B
Z�B
Z�B
Z�B
[	B
[�B
[�B
\�B
\�B
\xB
]�B
]IB
]IB
]IB
]dB
]B
]B
]dB
]�B
]�B
^5B
^OB
^�B
_!B
_�B
_�B
`B
`\B
a�B
bB
b�B
b�B
b�B
b�B
cB
c B
b�B
bhB
b4B
bB
b4B
a�B
bNB
bNB
b�B
b�B
cB
b�B
cB
b�B
b�B
b�B
cTB
dB
d@B
eB
e�B
e�B
e�B
fB
fLB
fLB
fLB
f�B
g�B
g�B
hXB
hXB
h�B
h�B
h�B
h>B
g�B
h$B
hsB
iDB
iyB
iyB
iyB
i*B
iDB
iDB
iDB
i�B
jB
kB
kQB
kB
j�B
kB
k�B
l"B
lWB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
ncB
ncB
n}B
n�B
oB
oB
n�B
n�B
oiB
o�B
o�B
o�B
p!B
qAB
raB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
sMB
s3B
s3B
s3B
s3B
sMB
sMB
shB
s�B
s�B
s�B
t9B
tTB
tTB
t�B
t�B
u%B
uZB
utB
u�B
u�B
u�B
vB
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
x8B
x8B
xRB
xRB
xRB
x�B
x�B
x�B
y	B
yrB
y�B
y�B
z^B
z^B
zxB
z�B
z�B
z�B
z�B
{JB
{dB
{JB
{dB
{B
{�B
{�B
{�B
|B
|�B
|�B
|�B
|�B
|�B
}B
}B
}B
|�B
|�B
}B
}<B
}VB
}<B
}"B
|�B
|B
|B
|�B
}"B
}B
}B
|�B
}�B
}�B
~B
~BB
~wB
~�B
~�B
~�B
~�B
B
B
HB
}B
�B
�B
�B
�B
�OB
��B
��B
��B
��B
��B
� B
�oB
�oB
��B
��B
��B
��B
�'B
�'B
�AB
�[B
��B
��B
�B
�GB
�-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�+B��B�YB�%B�?B�tB�tB��B�YB��B�%B�%B�?B�B�?B�%B�?B�%B�%B�YB��BƨB��BƨB��B��B��B��B��B�EB��B�1B�KB�B�KB�fBȀB��B�7B�RB	~B	�B
�BB
��B
�]B
�XB
��BBQB�B
�RB
ևB
�DB
��B
��B
�`B
��B
��B
�hB
�(B
�=B
q�B
J�B
&�B
 iB	߾B	�\B	�aB	�B	�iB	�nB	�1B	c:B	T,B	D�B	8�B	5?B	3�B	0oB	+�B	)*B	$�B	!B	�B	�B	�B	�B	B	�B�0B�fB�hB� B�
B��B�sB�B��B�nB�NB�@B�2B�B�B�AB�ZB��B�B	�B	6B	 BB	)�B	2�B	7�B	7�B	8B	8�B	8�B	2�B	5ZB	GB	J#B	R�B	T�B	T,B	Z�B	h$B	q�B	t�B	vB	x�B	h$B	f�B	h�B	raB	uB	z�B	�;B	��B	��B	�B	��B	ңB	ѝB	̳B	��B	��B	�B	�B	��B	��B	�AB	�vB	�[B	�B	��B	��B	�B	�$B	�yB	�kB	��B	��B	�oB	�B	�3B	��B	�B	�AB	�+B	��B	��B	�BB
uB
�B
B
-B
�B
�B
YB
B
aB
mB
SB
  B	�B	��B	�B
�B
�B
"B
(B
KB
KB
�B
YB
qB
eB
 B
 B
}B
�B
zB
MB
�B
�B
eB
+B
+�B
/�B
0�B
0!B
/�B
.�B
,�B
&�B
&2B
*B
*�B
(XB
#�B
"4B
$@B
&LB
%�B
#�B
�B
�B
�B
B
�B
QB
]B
�B
VB
!B
�B
2B
�B
�B
�B
�B
B
dB
B
�B
IB
	B
�B
7B
B
B
�B
�B
eB
1B
�B
+B
�B
,B
FB
,B
�B
�B
[B
B
�B
:B
�B
�B
hB
�B
�B
�B
bB
�B
�B
�B
�B
B
.B
bB
.B
B
�B
�B
 B
TB
B
�B
�B
�B
�B
�B
�B
vB
B
�B
"B
�B
B
JB
^B
�B
�B

	B
DB

�B

#B
	lB

	B

�B
	RB
�B
�B
	�B
	7B
�B
�B
	�B
B
xB

	B
�B
�B

#B
B
PB
�B
�B
6B
�B
B
<B
"B
�B
jB
�B
�B
B
PB
B
�B
�B
�B
�B
PB
PB
B
�B
�B
JB
�B
�B
B
B
6B
PB
�B
PB
<B
jB
B
�B
�B
<B
�B
�B
�B
(B
\B
�B
�B
B
�B
�B
�B
�B
vB
�B
�B
�B
�B
�B
B
bB
.B
B
HB
4B
hB
4B
B
�B
�B
�B
B
�B
TB
:B
oB
�B
�B
TB
�B
�B
�B
B
�B
�B
�B
,B
gB
�B
SB
SB
mB
�B
mB
�B
sB
�B
�B
�B
+B
_B
�B
KB
eB
eB
�B
B
B
B
B
7B
kB
QB
7B
QB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
dB
dB
�B
�B
�B
B
B
5B
5B
5B
jB
�B
;B
;B
pB
 B
 'B
 B
 B
 BB
 �B
 �B
!�B
!|B
!bB
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"4B
#�B
#:B
"hB
!|B
!B
!�B
!�B
"NB
"�B
#:B
# B
#B
#TB
#�B
$�B
%�B
%�B
%�B
%zB
%FB
%,B
%FB
%�B
%�B
&B
'�B
($B
'�B
(
B
(�B
*�B
+6B
+�B
,�B
-CB
.IB
.cB
.cB
.cB
-�B
,�B
,�B
-�B
.IB
.�B
0!B
0�B
0�B
0�B
1[B
1�B
1�B
1�B
1�B
1�B
2B
2B
2-B
2B
2-B
2aB
2�B
2-B
2-B
1�B
1�B
2|B
2�B
3B
3�B
49B
4TB
4�B
5B
5tB
5�B
5�B
5�B
5�B
6B
6zB
6�B
6`B
7fB
7�B
7LB
7B
7fB
7�B
7�B
7�B
7�B
7�B
7�B
7LB
72B
7�B
8B
9XB
9�B
:DB
:xB
:�B
:�B
:�B
;B
:�B
:�B
;JB
;dB
<PB
<�B
=�B
=�B
>(B
>wB
>�B
>�B
?.B
?B
?.B
?cB
?}B
@B
@OB
@OB
@�B
A B
B'B
B�B
B�B
B�B
B�B
CB
CGB
CGB
C�B
C�B
DB
DMB
DMB
DMB
DMB
D�B
EB
EmB
FB
F�B
F?B
FtB
FtB
FtB
F�B
FtB
F�B
F�B
GEB
G�B
G�B
HKB
HfB
H�B
H�B
H�B
IB
H�B
H�B
H�B
IlB
J	B
I�B
J�B
J�B
JrB
JXB
J	B
J	B
J#B
J=B
J	B
J=B
J=B
J	B
J	B
JrB
J�B
J�B
J�B
J�B
J�B
J�B
K)B
KB
KDB
KxB
LdB
MB
MjB
NB
N�B
N�B
OBB
OBB
N�B
N�B
NVB
N"B
N<B
N<B
N�B
N�B
N�B
OB
OBB
O\B
O�B
QhB
Q�B
RB
Q�B
Q�B
R B
R B
R�B
S&B
S@B
S@B
S�B
TFB
T{B
UMB
UMB
U�B
VB
VB
U�B
U�B
VB
U�B
U�B
V�B
W�B
W�B
W�B
WsB
WYB
WYB
W?B
W�B
W�B
WsB
W?B
WYB
W?B
W�B
W�B
XB
XB
X+B
X�B
XyB
X�B
Y�B
Z�B
Z�B
Z�B
[	B
[�B
[�B
\�B
\�B
\xB
]�B
]IB
]IB
]IB
]dB
]B
]B
]dB
]�B
]�B
^5B
^OB
^�B
_!B
_�B
_�B
`B
`\B
a�B
bB
b�B
b�B
b�B
b�B
cB
c B
b�B
bhB
b4B
bB
b4B
a�B
bNB
bNB
b�B
b�B
cB
b�B
cB
b�B
b�B
b�B
cTB
dB
d@B
eB
e�B
e�B
e�B
fB
fLB
fLB
fLB
f�B
g�B
g�B
hXB
hXB
h�B
h�B
h�B
h>B
g�B
h$B
hsB
iDB
iyB
iyB
iyB
i*B
iDB
iDB
iDB
i�B
jB
kB
kQB
kB
j�B
kB
k�B
l"B
lWB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
ncB
ncB
n}B
n�B
oB
oB
n�B
n�B
oiB
o�B
o�B
o�B
p!B
qAB
raB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
sMB
s3B
s3B
s3B
s3B
sMB
sMB
shB
s�B
s�B
s�B
t9B
tTB
tTB
t�B
t�B
u%B
uZB
utB
u�B
u�B
u�B
vB
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
x8B
x8B
xRB
xRB
xRB
x�B
x�B
x�B
y	B
yrB
y�B
y�B
z^B
z^B
zxB
z�B
z�B
z�B
z�B
{JB
{dB
{JB
{dB
{B
{�B
{�B
{�B
|B
|�B
|�B
|�B
|�B
|�B
}B
}B
}B
|�B
|�B
}B
}<B
}VB
}<B
}"B
|�B
|B
|B
|�B
}"B
}B
}B
|�B
}�B
}�B
~B
~BB
~wB
~�B
~�B
~�B
~�B
B
B
HB
}B
�B
�B
�B
�B
�OB
��B
��B
��B
��B
��B
� B
�oB
�oB
��B
��B
��B
��B
�'B
�'B
�AB
�[B
��B
��B
�B
�GB
�-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105251  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604193025  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604193026  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604193026                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605043033  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605043033  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                