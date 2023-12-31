CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:24:05Z creation;2022-06-04T17:24:06Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �,   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �<   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �D   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20220604172405  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               	A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @غ��:g�1   @غ�Z�$�@,�vȴ9X�d�G�{1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���BǙ�B���B�  Bә�B�ffB���B�  B䙚B�  B�33B���B���B�  B�  C   C  C  C�fC  C
  C  C  C33C  C�fC�fC�fC  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4�C633C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|�fD}  D}� D~  D~� D  D� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@u@��H@��HAp�A=p�A]p�A}p�A��RA��RA��RA��RAθRA��A�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��GB��GB��GB�z�B��B��B��B��B��B��B��B�z�B��B��B�z�B�G�B�z�BЮB�G�B�zB�z�B߮B�G�B�B��GB�z�B�z�B��B��B��C�
C�
C�pC�
C	�
C�
C�
C
=C�
C�pC�pC�pC�
C�
C�
C�
C!�
C#�pC%�
C'�
C)�
C+�
C-�
C/�
C1�
C3�C6
=C7�
C9�
C;�
C=�
C?�
CA�
CC�pCE�
CG�
CI�
CK�
CM�
CO�
CQ�
CS�
CU�
CW�CY�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV�)DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D||)D|��D}u�D}��D~u�D~��Du�D�)D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�w�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�$@A�$A�"�A�($A�($A�)_A�)�A�$�A�'�A�*�A�)�A�)_A�,�A�3hA�6�A�7LA�3�A�6zA�7�A�9�A�;dA�=qA�>A�?HA�@OA�E�A�O�A�I�A�?�A�8RA�1[A��A�YA���AҢ4A�s�A�)_A�#A�{�A�uA��}A��AϵtAϏ�A�-wA���A���A�AɃ�A�%zA��)A�@A�'�A�iDA�g�A���A���A���A��A���A��jA�t�A�F?A�M6A��8A�~A�sMA���A�{A�y>A�Q�A�>�A��8A��{A��KA���A�5A��A���A��YA�xlA�B�A�
rA�E�A�u�A�ӏA�E9A}+�Az�HAw��As�Aq(Ap�AoJAlB[Ad�ZA_zA[z�AR�AP}VAM�YAH�AC�fA@�'A<&�A:4nA9ƨA9�A8��A8w2A6�*A5�eA4��A4-A2�1A1�A18�A0��A0��A0m�A/�A/ �A.;dA-�[A-4A,L0A+��A+��A+�A+\�A+ \A*C�A(��A%�9A%��A%��A%~�A%C�A$ԕA$�+A$ZA#��A"u�A!l"A!.�A!�4A!��A!�SA!�eA!w�A ��AkQA�A�mA �AhsA�PA�;A(�A�NA�[A�SA��A|A1'A<�A�A�uA;�A!A��A~�AE�A �AںA��A�A��A��As�A�TA��A[WA�A�A��A�+A�A9�A|�AY�A�A�^A�AZA�A�A��A�A�NA�'A��Ae�A.�A��A�&A�&A��A��A�Ar�ARTA5�A�A
�A
H�A
-�A
(A	�4A��A�}A�{Ap;Ay>AI�A.IA�A{�A:*A�,Ah
Ae�A#�A͟Aa|A1A��A��A�SAu%A_�A;A�A�A	A��A�6Av`A�A ��A �FA ��A �fA l"A Q�A +kA �@��@���@��@�@��F@�ی@��n@��c@���@��@�ȴ@�H@�@�a�@���@���@���@���@�{@�$t@��@�2�@�U�@�1@�tT@�Q�@�ϫ@�ں@�{�@�[W@��'@�~�@�A@��@�k�@�@��@�u@�+k@��@�V@�e,@��@�K^@�x@�&@��]@�tT@ߠ�@ް�@��@ݗ$@�dZ@�i�@�@���@�|�@�@O@�$t@��8@��@ڸR@ڐ.@�>B@���@��N@ٝ�@�_p@�;d@�0�@�33@�-w@�҉@�V@׺^@�rG@ֵ�@�K^@զ�@�+@��E@��E@ԧ@�^5@��z@�h�@�@��@ђ:@��@Ј�@�S�@�{@�O@��'@�_@��@̛�@ˁ@�q@�e@�t�@��@ȦL@�e@Ǣ�@��5@ƅ�@�p;@�J@�T�@ć�@�#:@���@��o@��o@�خ@��K@ú^@ë�@Û=@�u�@�.I@��@�n�@�$�@�	@��@��N@��@�~�@�Z�@��X@�A�@��@�~(@�:�@�~@��A@���@�@��@��U@�`�@��Z@���@�IR@��2@��@�6z@�ی@�|�@�!�@�ݘ@���@�+@��@�r�@�!@���@�a@�IR@�.I@�$t@��@��@���@�Xy@��@���@�8@��]@�V�@�x@�hs@��H@���@�GE@�,=@�#:@�  @�u�@��@��F@��d@�/@���@�D�@�{@�o�@��?@�q@��@��@�X�@�4@��@���@��@�C-@� �@��>@�w2@��@��B@��1@�m�@��W@��^@���@���@�C�@��O@�oi@�h
@�_@�_@��@��o@��a@��P@�Dg@�)_@��@���@�q@�Z@�4n@�_@��@���@�qv@�҉@���@�}V@�bN@��+@�c@��@��m@�bN@���@�n/@�A�@�/�@� \@���@��p@��@�h�@�@�@��.@��0@�x@�4@�ѷ@�c @��&@�RT@���@�V@���@�l�@�A�@��8@��@��A@�c @�E�@� �@���@��@���@�dZ@��@��@��@�+k@��+@���@��@@�l�@�RT@�"�@��@���@��1@��D@��+@�|�@�z�@��@��X@�b�@��,@�M�@�H�@�E�@���@���@��$@�[W@�8@�	l@��e@�h
@�;�@��Z@���@���@�qv@�J�@��@��@�͟@��F@��@�s�@�h�@�YK@�^5@�D�@��@�خ@���@�{J@�j�@�n/@�?}@�S@��@���@�1@���@���@�t�@�P�@�Y@���@�v�@��@��@���@�s�@�ߤ@�6�@�u@��
@�]�@�!-@�ی@��u@���@�M@� �@���@�O@�0�@�Ĝ@�V@��@;d@~Q@~5?@}ԕ@}�-@}F@|Ĝ@{qv@{
=@zȴ@zv�@zR�@z�@y�@x�9@x�@w�;@w_p@v��@v5?@u��@u�X@uf�@u+�@t�|@t�e@t|�@tV�@s�P@rkQ@r�@qc�@p��@q%@p�@p�@pی@pu�@o�q@o��@odZ@o$t@n�x@m�H@m/@m@m�@l�U@l�I@ltT@l!@kx@ks@j�"@jYK@jR�@i��@i&�@h�)@h�Y@hU2@h�@g��@ga@f�y@f��@e��@e��@d�@dPH@c��@c~�@ca@c�@b)�@a��@am]@af�@`�	@`4n@_��@_+@^��@^�X@^p;@]�H@]!�@\�u@\�@[��@[t�@[o@Z�,@Z��@Z� @Zxl@Z$�@YQ�@XS�@W��@WC�@V�8@V��@V{@U�@Uf�@T�v@TM@S�@Sƨ@S��@S/�@R��@R;�@Q��@Q��@Q?}@Q�@P�$@P|�@PD�@O�K@O�:@Ox@OF�@Oo@N�c@N�X@N��@N=q@M�@M5�@L��@LM@K�}@K�k@J͟@J��@J($@I�)@I-w@Hی@HQ�@G�;@G{J@G33@F�6@E�.@E�@E��@EX@E!�@Dی@DĜ@D��@C�@CU�@C@B��@Bxl@A��@Ahs@@��@@%�@?��@?��@?v`@?Z�@>�H@>��@>�@=�'@=S&@=�@<Ɇ@<bN@;��@;�k@;�4@;b�@;S�@;@:�h@:B[@:)�@9�'@9[W@9&�@8��@8�@8�D@8,=@7�r@7��@7E9@71�@6��@6��@5��@5�@5Q�@5-w@4��@4]d@4�@3|�@2�2@2�L@2YK@2�@1��@1m]@1�@0��@0y>@0S�@/�+@/F�@.@-��@-e,@-Q�@-!�@,�@,�4@,V�@,~@+�W@+|�@+"�@*�M@*͟@*M�@)�@)hs@(�U@(�z@(V�@('R@'�m@'��@'O@&�L@&kQ@%�)@%��@%�n@%�'@%T�@%&�@$ی@$�Y@$Ft@#ݘ@#��@#o�@"�H@"�\@"E�@!��@!��@!��@!rG@!�@ Ɇ@ U2@ �@ 1@�w@�{@X�@/�@�y@��@{�@+k@@ԕ@�7@zx@O�@	l@ی@��@y>@S�@7�@,=@�@��@��@\)@&@@�s@��@��@~�@�@��@zx@�@�@��@Z@b@  @�+@�@S�@ߤ@�@kQ@J@��@�T@��@��@L�@�/@��@�.@r�@S�@7�@"h@�@�@�g@��@x@�@�"@�H@�1@Z�@?@@�@�o@��@��@��@�C@��@f�@@��@�@��@Xy@$@ �@@�@�m@�@@��@g�@9�@�@��@�@�<@h
@=q@�)@�#@�N@��@��@�h@m]@[W@A @�@�p@��@Ɇ@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�$@A�$A�"�A�($A�($A�)_A�)�A�$�A�'�A�*�A�)�A�)_A�,�A�3hA�6�A�7LA�3�A�6zA�7�A�9�A�;dA�=qA�>A�?HA�@OA�E�A�O�A�I�A�?�A�8RA�1[A��A�YA���AҢ4A�s�A�)_A�#A�{�A�uA��}A��AϵtAϏ�A�-wA���A���A�AɃ�A�%zA��)A�@A�'�A�iDA�g�A���A���A���A��A���A��jA�t�A�F?A�M6A��8A�~A�sMA���A�{A�y>A�Q�A�>�A��8A��{A��KA���A�5A��A���A��YA�xlA�B�A�
rA�E�A�u�A�ӏA�E9A}+�Az�HAw��As�Aq(Ap�AoJAlB[Ad�ZA_zA[z�AR�AP}VAM�YAH�AC�fA@�'A<&�A:4nA9ƨA9�A8��A8w2A6�*A5�eA4��A4-A2�1A1�A18�A0��A0��A0m�A/�A/ �A.;dA-�[A-4A,L0A+��A+��A+�A+\�A+ \A*C�A(��A%�9A%��A%��A%~�A%C�A$ԕA$�+A$ZA#��A"u�A!l"A!.�A!�4A!��A!�SA!�eA!w�A ��AkQA�A�mA �AhsA�PA�;A(�A�NA�[A�SA��A|A1'A<�A�A�uA;�A!A��A~�AE�A �AںA��A�A��A��As�A�TA��A[WA�A�A��A�+A�A9�A|�AY�A�A�^A�AZA�A�A��A�A�NA�'A��Ae�A.�A��A�&A�&A��A��A�Ar�ARTA5�A�A
�A
H�A
-�A
(A	�4A��A�}A�{Ap;Ay>AI�A.IA�A{�A:*A�,Ah
Ae�A#�A͟Aa|A1A��A��A�SAu%A_�A;A�A�A	A��A�6Av`A�A ��A �FA ��A �fA l"A Q�A +kA �@��@���@��@�@��F@�ی@��n@��c@���@��@�ȴ@�H@�@�a�@���@���@���@���@�{@�$t@��@�2�@�U�@�1@�tT@�Q�@�ϫ@�ں@�{�@�[W@��'@�~�@�A@��@�k�@�@��@�u@�+k@��@�V@�e,@��@�K^@�x@�&@��]@�tT@ߠ�@ް�@��@ݗ$@�dZ@�i�@�@���@�|�@�@O@�$t@��8@��@ڸR@ڐ.@�>B@���@��N@ٝ�@�_p@�;d@�0�@�33@�-w@�҉@�V@׺^@�rG@ֵ�@�K^@զ�@�+@��E@��E@ԧ@�^5@��z@�h�@�@��@ђ:@��@Ј�@�S�@�{@�O@��'@�_@��@̛�@ˁ@�q@�e@�t�@��@ȦL@�e@Ǣ�@��5@ƅ�@�p;@�J@�T�@ć�@�#:@���@��o@��o@�خ@��K@ú^@ë�@Û=@�u�@�.I@��@�n�@�$�@�	@��@��N@��@�~�@�Z�@��X@�A�@��@�~(@�:�@�~@��A@���@�@��@��U@�`�@��Z@���@�IR@��2@��@�6z@�ی@�|�@�!�@�ݘ@���@�+@��@�r�@�!@���@�a@�IR@�.I@�$t@��@��@���@�Xy@��@���@�8@��]@�V�@�x@�hs@��H@���@�GE@�,=@�#:@�  @�u�@��@��F@��d@�/@���@�D�@�{@�o�@��?@�q@��@��@�X�@�4@��@���@��@�C-@� �@��>@�w2@��@��B@��1@�m�@��W@��^@���@���@�C�@��O@�oi@�h
@�_@�_@��@��o@��a@��P@�Dg@�)_@��@���@�q@�Z@�4n@�_@��@���@�qv@�҉@���@�}V@�bN@��+@�c@��@��m@�bN@���@�n/@�A�@�/�@� \@���@��p@��@�h�@�@�@��.@��0@�x@�4@�ѷ@�c @��&@�RT@���@�V@���@�l�@�A�@��8@��@��A@�c @�E�@� �@���@��@���@�dZ@��@��@��@�+k@��+@���@��@@�l�@�RT@�"�@��@���@��1@��D@��+@�|�@�z�@��@��X@�b�@��,@�M�@�H�@�E�@���@���@��$@�[W@�8@�	l@��e@�h
@�;�@��Z@���@���@�qv@�J�@��@��@�͟@��F@��@�s�@�h�@�YK@�^5@�D�@��@�خ@���@�{J@�j�@�n/@�?}@�S@��@���@�1@���@���@�t�@�P�@�Y@���@�v�@��@��@���@�s�@�ߤ@�6�@�u@��
@�]�@�!-@�ی@��u@���@�M@� �@���@�O@�0�@�Ĝ@�V@��@;d@~Q@~5?@}ԕ@}�-@}F@|Ĝ@{qv@{
=@zȴ@zv�@zR�@z�@y�@x�9@x�@w�;@w_p@v��@v5?@u��@u�X@uf�@u+�@t�|@t�e@t|�@tV�@s�P@rkQ@r�@qc�@p��@q%@p�@p�@pی@pu�@o�q@o��@odZ@o$t@n�x@m�H@m/@m@m�@l�U@l�I@ltT@l!@kx@ks@j�"@jYK@jR�@i��@i&�@h�)@h�Y@hU2@h�@g��@ga@f�y@f��@e��@e��@d�@dPH@c��@c~�@ca@c�@b)�@a��@am]@af�@`�	@`4n@_��@_+@^��@^�X@^p;@]�H@]!�@\�u@\�@[��@[t�@[o@Z�,@Z��@Z� @Zxl@Z$�@YQ�@XS�@W��@WC�@V�8@V��@V{@U�@Uf�@T�v@TM@S�@Sƨ@S��@S/�@R��@R;�@Q��@Q��@Q?}@Q�@P�$@P|�@PD�@O�K@O�:@Ox@OF�@Oo@N�c@N�X@N��@N=q@M�@M5�@L��@LM@K�}@K�k@J͟@J��@J($@I�)@I-w@Hی@HQ�@G�;@G{J@G33@F�6@E�.@E�@E��@EX@E!�@Dی@DĜ@D��@C�@CU�@C@B��@Bxl@A��@Ahs@@��@@%�@?��@?��@?v`@?Z�@>�H@>��@>�@=�'@=S&@=�@<Ɇ@<bN@;��@;�k@;�4@;b�@;S�@;@:�h@:B[@:)�@9�'@9[W@9&�@8��@8�@8�D@8,=@7�r@7��@7E9@71�@6��@6��@5��@5�@5Q�@5-w@4��@4]d@4�@3|�@2�2@2�L@2YK@2�@1��@1m]@1�@0��@0y>@0S�@/�+@/F�@.@-��@-e,@-Q�@-!�@,�@,�4@,V�@,~@+�W@+|�@+"�@*�M@*͟@*M�@)�@)hs@(�U@(�z@(V�@('R@'�m@'��@'O@&�L@&kQ@%�)@%��@%�n@%�'@%T�@%&�@$ی@$�Y@$Ft@#ݘ@#��@#o�@"�H@"�\@"E�@!��@!��@!��@!rG@!�@ Ɇ@ U2@ �@ 1@�w@�{@X�@/�@�y@��@{�@+k@@ԕ@�7@zx@O�@	l@ی@��@y>@S�@7�@,=@�@��@��@\)@&@@�s@��@��@~�@�@��@zx@�@�@��@Z@b@  @�+@�@S�@ߤ@�@kQ@J@��@�T@��@��@L�@�/@��@�.@r�@S�@7�@"h@�@�@�g@��@x@�@�"@�H@�1@Z�@?@@�@�o@��@��@��@�C@��@f�@@��@�@��@Xy@$@ �@@�@�m@�@@��@g�@9�@�@��@�@�<@h
@=q@�)@�#@�N@��@��@�h@m]@[W@A @�@�p@��@Ɇ@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B�B�!B�!B��B��B��B��B��B��B��B�!B�!B�!B��B�B�;B�VB�!B�\B�pB�OB�~B�#B��B��B��B��B��B}qBq'Bh�Bf�BncBp�BtTBy>B�AB��B�~B�B	y	B	�-B
4B
~�B
�dB
��B
��BAB��B�TB�CB��B��B�1B�ZB�~B�(B��B�aBcnBJ�B5%B#�B�BaB
�B
��B
�DB
��B
�gB
�(B
��B
v+B
X�B
N"B
@�B
33B
�B	�B	�MB	��B	ǔB	�wB	�VB	�+B	��B	~(B	W�B	8�B	$ZB	FB	5ZB	ZkB	f�B	jB	{B	HB	��B	�NB	�QB	��B	�:B	�!B	��B	��B	żB	�9B	B	�B	˒B	�0B	��B	�&B	�~B	�B	��B	��B
�B
SB
�B
�B
gB
[B	��B	��B	��B
�B
�B
gB
�B
!B
!|B
!�B
 �B
�B
eB
�B
+B
3�B
4�B
:*B
@�B
;0B
2-B
-�B
-wB
'8B
�B
�B
�B
*0B
*�B
+�B
.B
)�B
)DB
,WB
5�B
9rB
;�B
>BB
>]B
?.B
@iB
A�B
B�B
C�B
C�B
DMB
C�B
CGB
D3B
C�B
DMB
DMB
C�B
E�B
E�B
E9B
E�B
E�B
E�B
HB
K�B
K^B
I�B
HfB
GEB
A�B
A�B
AoB
A�B
C{B
CaB
CB
CaB
C{B
D3B
D�B
EmB
D�B
D�B
D�B
C�B
CaB
C-B
B�B
A;B
@OB
>�B
<�B
;B
<6B
<6B
<�B
>�B
@ B
?.B
>wB
<6B
<B
<�B
;dB
<�B
<PB
:�B
9$B
9>B
;0B
<�B
;JB
:�B
:�B
:�B
9�B
9	B
6�B
5ZB
2B
0�B
/B
.B
-)B
,�B
,WB
,=B
,=B
,"B
+kB
*�B
)�B
&�B
%�B
$�B
#TB
!�B
!B
 \B
�B
;B
�B
�B
�B
!B
OB
/B
�B
�B
=B
�B
QB
�B
�B
EB
B
B
?B
mB
�B
�B
�B
 B
�B
BB
B
�B
xB
�B
�B
PB
B
B
�B
)B

rB
	�B
	B
�B
B
�B
�B
�B
?B
�B
�B
B
�B
mB
mB
mB
�B
�B
mB
�B
�B
B
mB
�B
�B
�B
�B
	lB
_B
�B
B
9B
�B
B
SB
�B
�B
B
�B
�B
�B
MB
�B
�B
�B
{B
B
�B
{B
�B
�B
�B
�B
9B
9B
?B
?B
%B
�B
�B
�B
YB
�B
zB
�B
�B
KB
fB
�B
�B
fB
�B
�B
�B
�B
�B
fB
1B
	�B
	�B
	�B
	�B
	lB
	7B

	B
	�B
	7B
	�B
	RB
	lB
	7B
	B
�B
�B
	B
	RB
	B
�B
	�B

�B

�B

�B

�B

�B
)B

�B

�B
DB
^B

�B

�B

�B

=B

�B

rB

rB

XB

rB

XB

=B
	�B

#B

XB
)B
�B
�B
�B
B
�B
JB
�B
B
0B
�B
�B
�B
dB
dB
JB
�B
B
�B
<B
"B
(B
B
}B
�B
�B
�B
�B
�B
�B
B
4B
4B
4B
�B
:B
:B
�B
�B
&B
�B
�B
�B
�B
oB
uB
�B
�B
gB
9B
SB
mB
�B

B
�B
YB
�B
?B
?B
?B
sB
$B
$B
�B
1B
B
KB
�B
�B
�B
�B
�B
=B
B
CB
)B
CB
B
CB
�B
�B
/B
dB
�B
B
xB
CB
xB
CB
�B
qB
qB
CB
CB
/B
�B
B
�B
!B
�B
�B
 B
 'B
 'B
 BB
 �B
!�B
!�B
"�B
#�B
$&B
$@B
$�B
$�B
$�B
%B
%`B
%zB
%�B
%�B
%�B
%�B
%`B
%zB
%`B
$�B
$ZB
#�B
$�B
&fB
(>B
(sB
(�B
(�B
(�B
(�B
)�B
)�B
*0B
*�B
*�B
+B
+6B
+kB
+�B
+�B
+�B
+�B
+�B
+�B
+�B
,B
,B
,WB
,qB
,�B
,WB
,�B
,�B
,�B
,�B
,WB
,�B
,�B
-�B
-�B
-�B
.B
.}B
.�B
.�B
.�B
-�B
-B
,�B
,�B
+�B
*B
)�B
)�B
)�B
*B
*0B
*eB
*�B
*�B
+�B
,�B
,�B
,�B
.B
.�B
.�B
-�B
-wB
.�B
/�B
/�B
/�B
1�B
1'B
0�B
0�B
0�B
0�B
1'B
1B
2GB
2GB
2aB
3B
49B
4nB
4�B
4�B
4�B
4�B
4�B
4�B
4�B
4nB
4�B
5?B
5�B
6+B
72B
88B
8�B
8�B
8�B
9>B
9�B
9�B
9�B
:�B
:^B
:�B
;0B
;JB
;dB
;�B
;�B
;�B
<B
<�B
<�B
<�B
<�B
=<B
=B
<�B
<PB
<jB
<�B
<�B
<�B
="B
=VB
=VB
=<B
="B
=qB
>�B
?B
?�B
?�B
@4B
@�B
AUB
A;B
AB
AB
A;B
A�B
A�B
BB
BB
BB
B[B
B[B
B�B
B�B
B�B
B�B
CGB
CGB
C�B
CaB
C{B
C�B
D3B
D�B
EB
E9B
EmB
E�B
FB
F�B
F�B
G+B
G�B
H1B
H1B
G�B
HfB
H�B
I7B
I�B
I�B
J	B
I�B
J=B
JXB
J�B
K)B
KDB
K^B
KxB
K�B
K�B
K�B
LB
LJB
L�B
L�B
L�B
L�B
MB
MjB
N"B
NB
N"B
N"B
N�B
NVB
NpB
N�B
N�B
O(B
OvB
O�B
O�B
PHB
PB
PHB
PbB
PHB
P�B
QhB
QhB
Q�B
Q�B
Q�B
R B
RTB
R�B
S@B
S[B
S�B
S�B
S�B
TB
T,B
TaB
T�B
UB
U2B
UMB
U�B
VB
VB
VB
V9B
VB
VmB
V�B
W
B
V�B
W�B
W�B
W�B
W�B
XB
X+B
XyB
XyB
X�B
Y1B
YB
YKB
YB
ZB
Z7B
ZkB
ZkB
[	B
\)B
\xB
]/B
^5B
^5B
^�B
^�B
^�B
_pB
_�B
`vB
`vB
`vB
`�B
a|B
bB
a�B
b�B
b�B
b�B
b�B
cTB
c�B
dB
dB
dZB
dZB
dtB
d@B
dtB
dZB
d�B
e�B
e�B
e�B
e�B
f2B
f�B
f�B
g�B
g�B
h>B
hXB
h>B
h$B
h�B
h�B
h�B
i_B
iyB
i�B
i�B
i�B
j�B
j�B
j�B
kQB
kB
k6B
k�B
kkB
k�B
lWB
l"B
l"B
l�B
mB
m)B
mwB
m�B
m�B
m�B
m�B
nB
nIB
ncB
n�B
n�B
n�B
o B
n�B
o B
o5B
oB
o5B
oOB
oiB
o�B
o�B
o�B
p;B
p!B
pUB
pUB
pUB
p�B
q'B
q[B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
sB
sMB
shB
shB
shB
shB
tB
tTB
tnB
tTB
tnB
t�B
t�B
uB
u%B
u%B
uZB
utB
u�B
vB
vB
vFB
v�B
vzB
v�B
v�B
v�B
wLB
w�B
wfB
wfB
wLB
wLB
w�B
xRB
xlB
xlB
x�B
y	B
yXB
y�B
zxB
z�B
z�B
{JB
{B
{�B
{�B
{�B
|B
|B
|B
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}VB
}VB
}�B
}�B
~BB
}�B
~B
~111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B�B�!B�!B��B��B��B��B��B��B��B�!B�!B�!B��B�B�;B�VB�!B�\B�pB�OB�~B�#B��B��B��B��B��B}qBq'Bh�Bf�BncBp�BtTBy>B�AB��B�~B�B	y	B	�-B
4B
~�B
�dB
��B
��BAB��B�TB�CB��B��B�1B�ZB�~B�(B��B�aBcnBJ�B5%B#�B�BaB
�B
��B
�DB
��B
�gB
�(B
��B
v+B
X�B
N"B
@�B
33B
�B	�B	�MB	��B	ǔB	�wB	�VB	�+B	��B	~(B	W�B	8�B	$ZB	FB	5ZB	ZkB	f�B	jB	{B	HB	��B	�NB	�QB	��B	�:B	�!B	��B	��B	żB	�9B	B	�B	˒B	�0B	��B	�&B	�~B	�B	��B	��B
�B
SB
�B
�B
gB
[B	��B	��B	��B
�B
�B
gB
�B
!B
!|B
!�B
 �B
�B
eB
�B
+B
3�B
4�B
:*B
@�B
;0B
2-B
-�B
-wB
'8B
�B
�B
�B
*0B
*�B
+�B
.B
)�B
)DB
,WB
5�B
9rB
;�B
>BB
>]B
?.B
@iB
A�B
B�B
C�B
C�B
DMB
C�B
CGB
D3B
C�B
DMB
DMB
C�B
E�B
E�B
E9B
E�B
E�B
E�B
HB
K�B
K^B
I�B
HfB
GEB
A�B
A�B
AoB
A�B
C{B
CaB
CB
CaB
C{B
D3B
D�B
EmB
D�B
D�B
D�B
C�B
CaB
C-B
B�B
A;B
@OB
>�B
<�B
;B
<6B
<6B
<�B
>�B
@ B
?.B
>wB
<6B
<B
<�B
;dB
<�B
<PB
:�B
9$B
9>B
;0B
<�B
;JB
:�B
:�B
:�B
9�B
9	B
6�B
5ZB
2B
0�B
/B
.B
-)B
,�B
,WB
,=B
,=B
,"B
+kB
*�B
)�B
&�B
%�B
$�B
#TB
!�B
!B
 \B
�B
;B
�B
�B
�B
!B
OB
/B
�B
�B
=B
�B
QB
�B
�B
EB
B
B
?B
mB
�B
�B
�B
 B
�B
BB
B
�B
xB
�B
�B
PB
B
B
�B
)B

rB
	�B
	B
�B
B
�B
�B
�B
?B
�B
�B
B
�B
mB
mB
mB
�B
�B
mB
�B
�B
B
mB
�B
�B
�B
�B
	lB
_B
�B
B
9B
�B
B
SB
�B
�B
B
�B
�B
�B
MB
�B
�B
�B
{B
B
�B
{B
�B
�B
�B
�B
9B
9B
?B
?B
%B
�B
�B
�B
YB
�B
zB
�B
�B
KB
fB
�B
�B
fB
�B
�B
�B
�B
�B
fB
1B
	�B
	�B
	�B
	�B
	lB
	7B

	B
	�B
	7B
	�B
	RB
	lB
	7B
	B
�B
�B
	B
	RB
	B
�B
	�B

�B

�B

�B

�B

�B
)B

�B

�B
DB
^B

�B

�B

�B

=B

�B

rB

rB

XB

rB

XB

=B
	�B

#B

XB
)B
�B
�B
�B
B
�B
JB
�B
B
0B
�B
�B
�B
dB
dB
JB
�B
B
�B
<B
"B
(B
B
}B
�B
�B
�B
�B
�B
�B
B
4B
4B
4B
�B
:B
:B
�B
�B
&B
�B
�B
�B
�B
oB
uB
�B
�B
gB
9B
SB
mB
�B

B
�B
YB
�B
?B
?B
?B
sB
$B
$B
�B
1B
B
KB
�B
�B
�B
�B
�B
=B
B
CB
)B
CB
B
CB
�B
�B
/B
dB
�B
B
xB
CB
xB
CB
�B
qB
qB
CB
CB
/B
�B
B
�B
!B
�B
�B
 B
 'B
 'B
 BB
 �B
!�B
!�B
"�B
#�B
$&B
$@B
$�B
$�B
$�B
%B
%`B
%zB
%�B
%�B
%�B
%�B
%`B
%zB
%`B
$�B
$ZB
#�B
$�B
&fB
(>B
(sB
(�B
(�B
(�B
(�B
)�B
)�B
*0B
*�B
*�B
+B
+6B
+kB
+�B
+�B
+�B
+�B
+�B
+�B
+�B
,B
,B
,WB
,qB
,�B
,WB
,�B
,�B
,�B
,�B
,WB
,�B
,�B
-�B
-�B
-�B
.B
.}B
.�B
.�B
.�B
-�B
-B
,�B
,�B
+�B
*B
)�B
)�B
)�B
*B
*0B
*eB
*�B
*�B
+�B
,�B
,�B
,�B
.B
.�B
.�B
-�B
-wB
.�B
/�B
/�B
/�B
1�B
1'B
0�B
0�B
0�B
0�B
1'B
1B
2GB
2GB
2aB
3B
49B
4nB
4�B
4�B
4�B
4�B
4�B
4�B
4�B
4nB
4�B
5?B
5�B
6+B
72B
88B
8�B
8�B
8�B
9>B
9�B
9�B
9�B
:�B
:^B
:�B
;0B
;JB
;dB
;�B
;�B
;�B
<B
<�B
<�B
<�B
<�B
=<B
=B
<�B
<PB
<jB
<�B
<�B
<�B
="B
=VB
=VB
=<B
="B
=qB
>�B
?B
?�B
?�B
@4B
@�B
AUB
A;B
AB
AB
A;B
A�B
A�B
BB
BB
BB
B[B
B[B
B�B
B�B
B�B
B�B
CGB
CGB
C�B
CaB
C{B
C�B
D3B
D�B
EB
E9B
EmB
E�B
FB
F�B
F�B
G+B
G�B
H1B
H1B
G�B
HfB
H�B
I7B
I�B
I�B
J	B
I�B
J=B
JXB
J�B
K)B
KDB
K^B
KxB
K�B
K�B
K�B
LB
LJB
L�B
L�B
L�B
L�B
MB
MjB
N"B
NB
N"B
N"B
N�B
NVB
NpB
N�B
N�B
O(B
OvB
O�B
O�B
PHB
PB
PHB
PbB
PHB
P�B
QhB
QhB
Q�B
Q�B
Q�B
R B
RTB
R�B
S@B
S[B
S�B
S�B
S�B
TB
T,B
TaB
T�B
UB
U2B
UMB
U�B
VB
VB
VB
V9B
VB
VmB
V�B
W
B
V�B
W�B
W�B
W�B
W�B
XB
X+B
XyB
XyB
X�B
Y1B
YB
YKB
YB
ZB
Z7B
ZkB
ZkB
[	B
\)B
\xB
]/B
^5B
^5B
^�B
^�B
^�B
_pB
_�B
`vB
`vB
`vB
`�B
a|B
bB
a�B
b�B
b�B
b�B
b�B
cTB
c�B
dB
dB
dZB
dZB
dtB
d@B
dtB
dZB
d�B
e�B
e�B
e�B
e�B
f2B
f�B
f�B
g�B
g�B
h>B
hXB
h>B
h$B
h�B
h�B
h�B
i_B
iyB
i�B
i�B
i�B
j�B
j�B
j�B
kQB
kB
k6B
k�B
kkB
k�B
lWB
l"B
l"B
l�B
mB
m)B
mwB
m�B
m�B
m�B
m�B
nB
nIB
ncB
n�B
n�B
n�B
o B
n�B
o B
o5B
oB
o5B
oOB
oiB
o�B
o�B
o�B
p;B
p!B
pUB
pUB
pUB
p�B
q'B
q[B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
sB
sMB
shB
shB
shB
shB
tB
tTB
tnB
tTB
tnB
t�B
t�B
uB
u%B
u%B
uZB
utB
u�B
vB
vB
vFB
v�B
vzB
v�B
v�B
v�B
wLB
w�B
wfB
wfB
wLB
wLB
w�B
xRB
xlB
xlB
x�B
y	B
yXB
y�B
zxB
z�B
z�B
{JB
{B
{�B
{�B
{�B
|B
|B
|B
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}VB
}VB
}�B
}�B
~BB
}�B
~B
~111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104845  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172405  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172406  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172406                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022413  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022413  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                