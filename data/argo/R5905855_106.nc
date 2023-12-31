CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:29:37Z creation;2022-06-04T19:29:37Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192937  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               jA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ٯއ+�1   @ٯ��n]L@+�=p��
�d-hr� �1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A���AᙚA�  A�33B  B  B  B   B(  B0  B7��B@  BH  BP  BY33B`ffBf  Bp��Bw��B��B�  B�  B�  B�  B�33B�33B�33B���B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C33C�fC�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF33CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DwfDw�fDx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @\)@u@��H@��HA
>A=p�A]p�A}p�A��RA��RA��RA��RAυA�Q�A�RA��B\)B\)B\)B\)B'\)B/\)B6��B?\)BG\)BO\)BX�\B_Be\)Bp(�Bv��B~��B��B��B��B��B��GB��GB��GB�z�B��B��B��B�z�B�z�B��B��B��BîBǮBˮBϮBӮB�zBۮB�z�B�z�B�B�B�B�B��B��B��C�
C�
C�
C�
C	�
C�
C�
C�
C�
C
=C�pC�pC�
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
CF
=CG�
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
Ce�pCg�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv�)Dw|)Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�>D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�~D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��EA���A�خA�خA��EA���A��&A��&A�AѯOAѕ�A�P}A�;A��Aб�AЭCAН~AЍ�A�}"A�g8A�_A�_�A�]/A�2�A�YA�!�A�ŢAЧRA�h�A�]�A�'�A��"A�W?A��A�tA�VA���A�CaA�>BA�5�A��A��A���A�W
A���A�P�A���A���A���A�tA�!A��yA��A�E9A�<�A��A���A�PA�I�A�V9A���A�L�A�\A�GA��>A�&�A�{�A�xA���A�c�A�5tA�tA�tTA�bNAy�Aw�hAs��Aq� Apz�Ao��Aoq�AnK^Ak��Ae��A^��AX��ASAPیAL~�AIݘAI��AHe�AC�`A?��A>O�A<OvA:_�A8��A6��A2sA0��A/�yA.�A,�A*��A*3�A)�HA)�}A)N<A(8�A&(�A#OvA"��A"SA!_A v`A�A�]A ZA �A�CA�rA�A��AݘA
�A�A��A@�AMjA�yA�A��A�6Ay>AkQAU�A�A�}A_A��A��A	Am]A&�A��A�AbAx�AL�A��A��AL�A�$A"hA��Al�A�$A��A
�pA
,�A	�A	J#AԕA4�A��A;�A�A��AjA9XAMA�*A�MA��AM�A�A�PA�PA�ZA~A&�A6A-�A�IA�|A��A
�Al�A��A��A�3A��A@�A1�A �aA ֡@��@�G@�x@���@��1@�/�@�Y@��A@��@�p�@�a|@��n@��p@��0@���@��+@��]@��.@�Y�@�%@��@��K@ﰊ@�@�z@�U2@�b@�2a@�ϫ@�&�@��|@��@궮@�y>@�-�@���@��W@�J�@�ݘ@��@��@�c @�@���@�T�@��M@��@�$@Ⴊ@��@��.@���@��@�^5@ݜ@�;d@���@��j@۩*@�c�@��`@�'R@ٲ-@�A�@��/@ب�@�z@�O�@�Z�@�'R@�)�@�'R@� �@�	@ո�@���@�[�@ӨX@�"�@�Z�@�J@��
@�+�@В�@ϡ�@��|@Ξ�@���@͆�@�v`@�\)@�IR@͆�@�`B@��@���@̛�@�,=@˨X@�]�@��@�|�@��@ȁo@�*�@��)@ǝ�@�"�@���@Ţ�@ķ�@�8�@��3@â�@×�@�!-@�D�@�2a@���@���@�bN@���@�/�@�S@��F@�D�@��@���@��@��M@���@��@���@�j@�W�@�@�qv@��v@�  @�"�@�R�@�#:@���@��@���@�x@��$@�q@��m@��@�zx@�<6@��m@�d�@��d@��4@�C@��,@���@�~(@�/�@���@�u�@�G�@� i@�B[@���@���@�8@��@��O@�V@�;�@��T@��f@�O�@���@�ff@���@��7@�]�@��@��B@��4@�p;@�7�@��&@��@�x@�8�@�	l@��p@�^5@��4@���@�~(@�ϫ@��M@�H�@��@��@���@�U2@�1�@�  @���@�^�@��f@���@���@�]d@�)�@�!@���@���@�0�@�&@��@��@���@��6@��o@�C�@� �@�
�@��.@��@�l�@�+@��@�oi@��@��@�IR@�@��K@���@�kQ@�?�@��@��f@�7@��@���@�4@��[@�s@�=@�
=@���@��H@�Ĝ@��e@���@���@�6�@��@�Z�@��5@���@�q�@�_@�M@�?�@�O@��@���@�|@�X�@��@���@��D@�<�@�ݘ@��0@��X@�&�@�ȴ@��6@�� @�|�@�U2@�C�@�_@��A@���@���@���@�l�@�@@��@�Ɇ@��9@��\@�bN@�6�@�	@��'@��@��@�oi@�I�@�($@��@�x@���@�^�@��@��[@���@���@�}V@�7�@� �@���@���@�s�@�;d@��@���@�ߤ@���@��@�w�@�^5@�S�@�H@�(�@��@���@�U�@�@��@�xl@�^5@�<�@��@�ƨ@���@�-w@���@�Q@�*�@�e@��@�@�1@��@��$@�)_@�ȴ@��<@���@�*�@�P@~�]@~��@~i�@~5?@~O@~{@}�>@}��@}-w@{��@{��@z��@zz@y�Z@y�C@yc�@yX@y�@x�/@xe�@w�m@wt�@w$t@v^5@u��@u�-@u�h@u2a@t��@tbN@s�&@s|�@r}V@q��@q�h@q\�@p��@p�@o��@o�:@oy�@o_p@oC�@oS@n�<@nTa@m�'@m0�@lی@lXy@k�@j�,@j�@i��@i#�@hFt@h�@gW?@f�8@f6�@e�N@es�@e\�@eS&@d�$@dPH@c�+@c��@cl�@b�B@b��@b��@b��@bW�@a��@a��@a�-@a��@`��@_o�@_�@^҉@^�h@^��@^��@^��@^�F@^{@]A @\q@\4n@\G@[�F@[.I@Z��@ZE�@Y��@Yw2@X�p@X��@X��@X�4@Xw�@Wl�@V�,@V�@V��@Va|@VM�@VL0@U��@U*0@T�f@T_@S��@Sb�@R��@RGE@R1�@Q�@P�P@P�p@Pq@O�A@O��@O+@N�,@N��@N��@N��@N~�@N��@Nu%@NL0@N4@M|@MV@L�`@L�.@LFt@L �@L�@Kݘ@Ko�@Kg�@J��@I�'@IJ�@I�@H��@H�@H:�@G�&@G\)@G/�@F��@F�@E��@D��@DɆ@D��@Du�@DM@C�@C��@CK�@C�@B�L@Bs�@BYK@B�@A�@A�@AT�@@�/@@6@?�K@?,�@>i�@>:*@>J@=w2@=Dg@=+@=�@<�)@<g8@;ݘ@;�f@;>�@;�@:��@:
�@9��@9x�@9X@9?}@9�@8I�@8�@7��@7>�@6u%@5�D@5�@5�7@5[W@54@4��@4g8@4V�@46@4C-@4'R@3�a@3x@3.I@2��@2��@2Q@1��@1�9@1�S@1hs@1�@0��@/�&@/@O@.�}@.h
@-��@-o @- \@,Ɇ@,��@+��@+�F@+b�@+!-@*ߤ@*�R@*��@*a|@)�>@)�S@)|@)rG@)Vm@)-w@)	l@(�|@(��@(�@(�@(u�@(�@'�[@'H�@'�@' i@&�@&�H@&҉@&��@&ff@&{@%��@%�t@%��@%%F@$�4@$�@#��@#|�@#iD@#6z@"�M@"�@"�b@"H�@!�.@!@!�~@!c�@!J�@!?}@!+�@ ��@ 1'@ ~@ U2@ ��@ ��@ �I@ |�@ Z@ M@�A@��@��@s@e�@E9@(@�@�+@Z�@B[@5?@$�@�@��@��@�'@f�@�@��@�p@tT@  @ƨ@o�@@O@�@�,@�6@�\@c @($@J@ԕ@�X@w2@%F@�U@��@y>@Q�@"h@x@�r@��@��@�m@�*@;d@ i@�@��@�@YK@V@M�@@��@^�@<6@�@��@��@e�@7�@�&@� @�6@�a@��@s@S�@1�@�@��@z@Ov@J�@@��@��@�S@�S@u�@f�@c�@Q�@<6@ \@�P@�E@H@Q�@D�@'R@��@��@�P@x@_p@;d@$t@��@��@�,@�m@�6@}V@5?@	@�@�@��@s�@|@w2@c�@4@�@�@��@`�@V�@S�@S�@9X@�@�@��@��@�@H�@;d@6z@�@
�"@
��@
��@
0U@
+k@
@�@
	@	�@	��@	m]@	c�@	e,@	J�@	A @	B�@	V@�@�@�[@z�@`�@9X@%�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��EA���A�خA�خA��EA���A��&A��&A�AѯOAѕ�A�P}A�;A��Aб�AЭCAН~AЍ�A�}"A�g8A�_A�_�A�]/A�2�A�YA�!�A�ŢAЧRA�h�A�]�A�'�A��"A�W?A��A�tA�VA���A�CaA�>BA�5�A��A��A���A�W
A���A�P�A���A���A���A�tA�!A��yA��A�E9A�<�A��A���A�PA�I�A�V9A���A�L�A�\A�GA��>A�&�A�{�A�xA���A�c�A�5tA�tA�tTA�bNAy�Aw�hAs��Aq� Apz�Ao��Aoq�AnK^Ak��Ae��A^��AX��ASAPیAL~�AIݘAI��AHe�AC�`A?��A>O�A<OvA:_�A8��A6��A2sA0��A/�yA.�A,�A*��A*3�A)�HA)�}A)N<A(8�A&(�A#OvA"��A"SA!_A v`A�A�]A ZA �A�CA�rA�A��AݘA
�A�A��A@�AMjA�yA�A��A�6Ay>AkQAU�A�A�}A_A��A��A	Am]A&�A��A�AbAx�AL�A��A��AL�A�$A"hA��Al�A�$A��A
�pA
,�A	�A	J#AԕA4�A��A;�A�A��AjA9XAMA�*A�MA��AM�A�A�PA�PA�ZA~A&�A6A-�A�IA�|A��A
�Al�A��A��A�3A��A@�A1�A �aA ֡@��@�G@�x@���@��1@�/�@�Y@��A@��@�p�@�a|@��n@��p@��0@���@��+@��]@��.@�Y�@�%@��@��K@ﰊ@�@�z@�U2@�b@�2a@�ϫ@�&�@��|@��@궮@�y>@�-�@���@��W@�J�@�ݘ@��@��@�c @�@���@�T�@��M@��@�$@Ⴊ@��@��.@���@��@�^5@ݜ@�;d@���@��j@۩*@�c�@��`@�'R@ٲ-@�A�@��/@ب�@�z@�O�@�Z�@�'R@�)�@�'R@� �@�	@ո�@���@�[�@ӨX@�"�@�Z�@�J@��
@�+�@В�@ϡ�@��|@Ξ�@���@͆�@�v`@�\)@�IR@͆�@�`B@��@���@̛�@�,=@˨X@�]�@��@�|�@��@ȁo@�*�@��)@ǝ�@�"�@���@Ţ�@ķ�@�8�@��3@â�@×�@�!-@�D�@�2a@���@���@�bN@���@�/�@�S@��F@�D�@��@���@��@��M@���@��@���@�j@�W�@�@�qv@��v@�  @�"�@�R�@�#:@���@��@���@�x@��$@�q@��m@��@�zx@�<6@��m@�d�@��d@��4@�C@��,@���@�~(@�/�@���@�u�@�G�@� i@�B[@���@���@�8@��@��O@�V@�;�@��T@��f@�O�@���@�ff@���@��7@�]�@��@��B@��4@�p;@�7�@��&@��@�x@�8�@�	l@��p@�^5@��4@���@�~(@�ϫ@��M@�H�@��@��@���@�U2@�1�@�  @���@�^�@��f@���@���@�]d@�)�@�!@���@���@�0�@�&@��@��@���@��6@��o@�C�@� �@�
�@��.@��@�l�@�+@��@�oi@��@��@�IR@�@��K@���@�kQ@�?�@��@��f@�7@��@���@�4@��[@�s@�=@�
=@���@��H@�Ĝ@��e@���@���@�6�@��@�Z�@��5@���@�q�@�_@�M@�?�@�O@��@���@�|@�X�@��@���@��D@�<�@�ݘ@��0@��X@�&�@�ȴ@��6@�� @�|�@�U2@�C�@�_@��A@���@���@���@�l�@�@@��@�Ɇ@��9@��\@�bN@�6�@�	@��'@��@��@�oi@�I�@�($@��@�x@���@�^�@��@��[@���@���@�}V@�7�@� �@���@���@�s�@�;d@��@���@�ߤ@���@��@�w�@�^5@�S�@�H@�(�@��@���@�U�@�@��@�xl@�^5@�<�@��@�ƨ@���@�-w@���@�Q@�*�@�e@��@�@�1@��@��$@�)_@�ȴ@��<@���@�*�@�P@~�]@~��@~i�@~5?@~O@~{@}�>@}��@}-w@{��@{��@z��@zz@y�Z@y�C@yc�@yX@y�@x�/@xe�@w�m@wt�@w$t@v^5@u��@u�-@u�h@u2a@t��@tbN@s�&@s|�@r}V@q��@q�h@q\�@p��@p�@o��@o�:@oy�@o_p@oC�@oS@n�<@nTa@m�'@m0�@lی@lXy@k�@j�,@j�@i��@i#�@hFt@h�@gW?@f�8@f6�@e�N@es�@e\�@eS&@d�$@dPH@c�+@c��@cl�@b�B@b��@b��@b��@bW�@a��@a��@a�-@a��@`��@_o�@_�@^҉@^�h@^��@^��@^��@^�F@^{@]A @\q@\4n@\G@[�F@[.I@Z��@ZE�@Y��@Yw2@X�p@X��@X��@X�4@Xw�@Wl�@V�,@V�@V��@Va|@VM�@VL0@U��@U*0@T�f@T_@S��@Sb�@R��@RGE@R1�@Q�@P�P@P�p@Pq@O�A@O��@O+@N�,@N��@N��@N��@N~�@N��@Nu%@NL0@N4@M|@MV@L�`@L�.@LFt@L �@L�@Kݘ@Ko�@Kg�@J��@I�'@IJ�@I�@H��@H�@H:�@G�&@G\)@G/�@F��@F�@E��@D��@DɆ@D��@Du�@DM@C�@C��@CK�@C�@B�L@Bs�@BYK@B�@A�@A�@AT�@@�/@@6@?�K@?,�@>i�@>:*@>J@=w2@=Dg@=+@=�@<�)@<g8@;ݘ@;�f@;>�@;�@:��@:
�@9��@9x�@9X@9?}@9�@8I�@8�@7��@7>�@6u%@5�D@5�@5�7@5[W@54@4��@4g8@4V�@46@4C-@4'R@3�a@3x@3.I@2��@2��@2Q@1��@1�9@1�S@1hs@1�@0��@/�&@/@O@.�}@.h
@-��@-o @- \@,Ɇ@,��@+��@+�F@+b�@+!-@*ߤ@*�R@*��@*a|@)�>@)�S@)|@)rG@)Vm@)-w@)	l@(�|@(��@(�@(�@(u�@(�@'�[@'H�@'�@' i@&�@&�H@&҉@&��@&ff@&{@%��@%�t@%��@%%F@$�4@$�@#��@#|�@#iD@#6z@"�M@"�@"�b@"H�@!�.@!@!�~@!c�@!J�@!?}@!+�@ ��@ 1'@ ~@ U2@ ��@ ��@ �I@ |�@ Z@ M@�A@��@��@s@e�@E9@(@�@�+@Z�@B[@5?@$�@�@��@��@�'@f�@�@��@�p@tT@  @ƨ@o�@@O@�@�,@�6@�\@c @($@J@ԕ@�X@w2@%F@�U@��@y>@Q�@"h@x@�r@��@��@�m@�*@;d@ i@�@��@�@YK@V@M�@@��@^�@<6@�@��@��@e�@7�@�&@� @�6@�a@��@s@S�@1�@�@��@z@Ov@J�@@��@��@�S@�S@u�@f�@c�@Q�@<6@ \@�P@�E@H@Q�@D�@'R@��@��@�P@x@_p@;d@$t@��@��@�,@�m@�6@}V@5?@	@�@�@��@s�@|@w2@c�@4@�@�@��@`�@V�@S�@S�@9X@�@�@��@��@�@H�@;d@6z@�@
�"@
��@
��@
0U@
+k@
@�@
	@	�@	��@	m]@	c�@	e,@	J�@	A @	B�@	V@�@�@�[@z�@`�@9X@%�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�B�B�3B�hB�hB��B�ZB�B��B��B�BB	RB�B"BpBpB}BB%�B6�BF�Bx�B�vB��B	�QB
�B
~B�B9B�B�BkB�B6�BDgBEmBE�BH�B��B��B�B�+B�xBvzB�tB�
B��B��B��B�JB�B�B��B�EB��B��B��B}B^�BC�B,�BB
�AB
�B
�B
�SB
�!B
y$B
RoB
IlB
;�B
�B	�5B	�B	��B	̳B	ǔB	�B	�AB	��B	� B	��B	n}B	S@B	8�B	-]B	!�B	�B	�B	�B	�B�]B�BB��B	�B	�B	 B�cB��B��B��B�+B��B	�B	hB	#�B	,�B	,"B	�B	�B	 B	;B	'B	0�B	="B	J�B	Z�B	gmB	kQB	l�B	o�B	x8B	��B	�\B	�&B	��B	��B	��B	��B	�bB	��B	�yB	�_B	��B	��B	�/B	�"B	��B	�B	�B	�B	�*B	�DB	��B	��B	�SB	ٴB	�B	�DB	�KB	�B	��B	�]B	�B	�yB	�-B	�B	�ZB	�B	�_B	�sB	�B	�B	��B	�B	�>B	�_B	��B	�B	�5B	�/B	��B	�]B	�)B	�WB	�UB	��B	�ZB	�*B	�]B
3B
�B
�B
B

�B
 B
HB
	�B

�B
BB
uB
eB
FB
�B
uB
	RB	��B
_B
B
�B
NB
�B
�B
B
�B
B
B
_B
9B
FB
�B
�B
\B
VB
�B
�B
bB
B
^B

�B

XB

#B

rB
	RB
	�B
)B
�B
�B
�B
pB
�B
�B
�B
B
�B
�B
)B
	7B
�B
^B
B
�B
B
�B
�B
�B
�B
xB
B
	�B
fB
�B
SB
�B
�B
�B
�B
�B
%B
�B
�B
B
B
�B
�B
�B
�B
fB
B
�B
�B
�B
?B
B
%B
%B
�B
�B
?B
?B
�B
�B
tB
�B
�B
�B
�B
�B
�B
�B
pB
�B
B
JB
�B
)B

XB
fB
EB
�B
�B
YB
YB
9B
B
�B
B
�B
B
%B
zB
+B
�B
�B
�B
�B
�B
�B
�B
�B
�B
_B
EB
KB
�B
6B
�B
dB
JB
�B
jB
B
pB
�B
~B
�B
�B
B
xB
xB
DB
�B
B
�B
�B
PB
6B
PB
~B
�B
�B
�B
�B
B
B
B
0B
�B
�B
�B
�B
0B
�B
6B
�B
�B
�B
pB
�B
�B
(B
B
.B
�B
B
 B
B
4B
NB
hB
�B
B
 B
oB
�B
�B
�B
�B
�B
aB
B
�B
2B
�B
gB
�B
�B
�B
�B
9B
�B
YB
�B
�B
$B
�B
_B
�B
�B
�B
�B
�B
B
�B
QB
QB
�B
�B
�B
�B
�B
�B
�B
�B
#B
WB
�B
�B
�B
�B
xB
�B
�B
�B
/B
�B
�B
CB
]B
�B
IB
�B
B
5B
OB
OB
OB
jB
OB
B
OB
jB
�B
;B
�B
�B
�B
 \B
 \B
 �B
 �B
 �B
!HB
!B
!B
!HB
!�B
!�B
"�B
"hB
"NB
#:B
#nB
#nB
#�B
#�B
#�B
#�B
$@B
$@B
$@B
$&B
$&B
$�B
%,B
%FB
%FB
%FB
%zB
%�B
%�B
%�B
&LB
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(
B
(sB
)�B
)�B
*�B
+B
+�B
+�B
,B
,qB
,WB
,WB
,�B
,�B
-]B
.IB
.�B
.�B
/ B
/B
/5B
/�B
/�B
/�B
/�B
/�B
0B
0�B
1'B
1�B
2-B
2-B
2GB
2B
2�B
4�B
5ZB
5tB
5�B
5�B
5�B
5�B
6+B
6�B
72B
7B
7LB
7�B
8RB
8�B
8�B
8�B
9$B
9$B
9	B
9	B
9$B
9rB
:*B
:*B
:^B
:�B
;B
;0B
;JB
;0B
;dB
;�B
;�B
<�B
<�B
<�B
="B
=VB
=qB
=qB
=�B
=�B
>B
>]B
>wB
?cB
?�B
?�B
?�B
?�B
@4B
@�B
@�B
@�B
@�B
@�B
AB
AB
AUB
A�B
A�B
BB
A�B
A;B
AB
@�B
@iB
A B
A�B
AoB
B[B
B'B
B�B
C-B
CGB
CGB
CGB
C�B
DgB
D�B
D�B
EB
G+B
G_B
GzB
G_B
G�B
G�B
G�B
G�B
G�B
G�B
HfB
HKB
H�B
H�B
H�B
HfB
HKB
HfB
H�B
H�B
I�B
I�B
I�B
I�B
IRB
I7B
IB
H�B
IB
I�B
J#B
JXB
JXB
JrB
K^B
KDB
KB
J�B
KDB
K)B
KB
KDB
KxB
KDB
K�B
K�B
K�B
LdB
L�B
L�B
MPB
M�B
M�B
NpB
N�B
N�B
N�B
O�B
O�B
PHB
PbB
P}B
PbB
PbB
PHB
PHB
P�B
P�B
QB
Q�B
Q�B
Q�B
Q�B
Q�B
RB
R�B
SuB
S[B
S�B
S�B
S�B
S�B
T,B
T{B
UB
UMB
U�B
U�B
U�B
VB
VB
U�B
U�B
U�B
V9B
W
B
W�B
W�B
WsB
W�B
XB
XyB
X�B
X�B
YB
X�B
YB
X�B
YB
Z7B
ZQB
ZkB
[#B
[#B
[qB
[qB
[�B
\�B
]dB
]�B
]�B
]~B
]/B
\�B
\�B
]IB
]dB
]�B
^jB
^�B
_B
_;B
_�B
`'B
`B
_�B
_�B
_�B
_�B
`�B
a�B
a�B
a�B
bB
bhB
b�B
b�B
cB
cnB
c:B
c:B
c�B
c�B
c�B
c�B
d&B
dtB
dB
c�B
d@B
d�B
e�B
e�B
e�B
ffB
f�B
fLB
f2B
fLB
f�B
f�B
g8B
g�B
g�B
g�B
g�B
gmB
gmB
g�B
g�B
g�B
g�B
h$B
h�B
i*B
i*B
i_B
iyB
i�B
i�B
i�B
jB
j�B
j�B
kB
j�B
j�B
j�B
k�B
lWB
l�B
l�B
lWB
l=B
l"B
l"B
lqB
l�B
l�B
l�B
l�B
mB
m)B
m�B
m�B
nB
n}B
o B
o B
n�B
n�B
o�B
qB
q�B
q�B
q�B
rB
rB
rGB
r�B
r�B
r�B
r�B
r�B
sMB
s�B
tB
tB
tB
tB
tB
tB
t9B
tB
tB
t9B
t�B
t�B
t�B
uZB
vB
vB
vzB
vzB
v�B
v�B
v�B
v�B
v�B
wB
wB
w�B
w�B
w�B
xB
xlB
x�B
x�B
x�B
x�B
y>B
y>B
y>B
y$B
y$B
y�B
y�B
zB
zDB
z^B
z�B
z�B
z�B
z�B
z�B
{�B
|B
|B
|PB
|PB
|�B
}B
}<B
}�B
}�B
}�B
}�B
}�B
~(B
~wB
~�B
~�B
~�B
}B
�B
� B
�OB
�iB
� B
� B
�B
�;B
�UB
�UB
�oB
�oB
��B
��B
��B
��B
�AB
�[B
�uB
��B
�B
�aB
��B
��B
��B
�B
�MB
�gB
�gB
��B
��B
��B
�SB
�mB
��B
��B
��B
�%B
�tB
�tB
�tB
�YB
�%B
�B
�B
�%B
�YB
�tB
��B
�+B
��B
��B
��B
��B
��B
��B
�B
�B
�1B
�fB
��B
��B
�KB
�fB
��B
�B
��B
��B
��B
��B
�lB
��B
�	B
�#B
�XB
�XB
�rB
��B
�rB
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�B�B�3B�hB�hB��B�ZB�B��B��B�BB	RB�B"BpBpB}BB%�B6�BF�Bx�B�vB��B	�QB
�B
~B�B9B�B�BkB�B6�BDgBEmBE�BH�B��B��B�B�+B�xBvzB�tB�
B��B��B��B�JB�B�B��B�EB��B��B��B}B^�BC�B,�BB
�AB
�B
�B
�SB
�!B
y$B
RoB
IlB
;�B
�B	�5B	�B	��B	̳B	ǔB	�B	�AB	��B	� B	��B	n}B	S@B	8�B	-]B	!�B	�B	�B	�B	�B�]B�BB��B	�B	�B	 B�cB��B��B��B�+B��B	�B	hB	#�B	,�B	,"B	�B	�B	 B	;B	'B	0�B	="B	J�B	Z�B	gmB	kQB	l�B	o�B	x8B	��B	�\B	�&B	��B	��B	��B	��B	�bB	��B	�yB	�_B	��B	��B	�/B	�"B	��B	�B	�B	�B	�*B	�DB	��B	��B	�SB	ٴB	�B	�DB	�KB	�B	��B	�]B	�B	�yB	�-B	�B	�ZB	�B	�_B	�sB	�B	�B	��B	�B	�>B	�_B	��B	�B	�5B	�/B	��B	�]B	�)B	�WB	�UB	��B	�ZB	�*B	�]B
3B
�B
�B
B

�B
 B
HB
	�B

�B
BB
uB
eB
FB
�B
uB
	RB	��B
_B
B
�B
NB
�B
�B
B
�B
B
B
_B
9B
FB
�B
�B
\B
VB
�B
�B
bB
B
^B

�B

XB

#B

rB
	RB
	�B
)B
�B
�B
�B
pB
�B
�B
�B
B
�B
�B
)B
	7B
�B
^B
B
�B
B
�B
�B
�B
�B
xB
B
	�B
fB
�B
SB
�B
�B
�B
�B
�B
%B
�B
�B
B
B
�B
�B
�B
�B
fB
B
�B
�B
�B
?B
B
%B
%B
�B
�B
?B
?B
�B
�B
tB
�B
�B
�B
�B
�B
�B
�B
pB
�B
B
JB
�B
)B

XB
fB
EB
�B
�B
YB
YB
9B
B
�B
B
�B
B
%B
zB
+B
�B
�B
�B
�B
�B
�B
�B
�B
�B
_B
EB
KB
�B
6B
�B
dB
JB
�B
jB
B
pB
�B
~B
�B
�B
B
xB
xB
DB
�B
B
�B
�B
PB
6B
PB
~B
�B
�B
�B
�B
B
B
B
0B
�B
�B
�B
�B
0B
�B
6B
�B
�B
�B
pB
�B
�B
(B
B
.B
�B
B
 B
B
4B
NB
hB
�B
B
 B
oB
�B
�B
�B
�B
�B
aB
B
�B
2B
�B
gB
�B
�B
�B
�B
9B
�B
YB
�B
�B
$B
�B
_B
�B
�B
�B
�B
�B
B
�B
QB
QB
�B
�B
�B
�B
�B
�B
�B
�B
#B
WB
�B
�B
�B
�B
xB
�B
�B
�B
/B
�B
�B
CB
]B
�B
IB
�B
B
5B
OB
OB
OB
jB
OB
B
OB
jB
�B
;B
�B
�B
�B
 \B
 \B
 �B
 �B
 �B
!HB
!B
!B
!HB
!�B
!�B
"�B
"hB
"NB
#:B
#nB
#nB
#�B
#�B
#�B
#�B
$@B
$@B
$@B
$&B
$&B
$�B
%,B
%FB
%FB
%FB
%zB
%�B
%�B
%�B
&LB
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(
B
(sB
)�B
)�B
*�B
+B
+�B
+�B
,B
,qB
,WB
,WB
,�B
,�B
-]B
.IB
.�B
.�B
/ B
/B
/5B
/�B
/�B
/�B
/�B
/�B
0B
0�B
1'B
1�B
2-B
2-B
2GB
2B
2�B
4�B
5ZB
5tB
5�B
5�B
5�B
5�B
6+B
6�B
72B
7B
7LB
7�B
8RB
8�B
8�B
8�B
9$B
9$B
9	B
9	B
9$B
9rB
:*B
:*B
:^B
:�B
;B
;0B
;JB
;0B
;dB
;�B
;�B
<�B
<�B
<�B
="B
=VB
=qB
=qB
=�B
=�B
>B
>]B
>wB
?cB
?�B
?�B
?�B
?�B
@4B
@�B
@�B
@�B
@�B
@�B
AB
AB
AUB
A�B
A�B
BB
A�B
A;B
AB
@�B
@iB
A B
A�B
AoB
B[B
B'B
B�B
C-B
CGB
CGB
CGB
C�B
DgB
D�B
D�B
EB
G+B
G_B
GzB
G_B
G�B
G�B
G�B
G�B
G�B
G�B
HfB
HKB
H�B
H�B
H�B
HfB
HKB
HfB
H�B
H�B
I�B
I�B
I�B
I�B
IRB
I7B
IB
H�B
IB
I�B
J#B
JXB
JXB
JrB
K^B
KDB
KB
J�B
KDB
K)B
KB
KDB
KxB
KDB
K�B
K�B
K�B
LdB
L�B
L�B
MPB
M�B
M�B
NpB
N�B
N�B
N�B
O�B
O�B
PHB
PbB
P}B
PbB
PbB
PHB
PHB
P�B
P�B
QB
Q�B
Q�B
Q�B
Q�B
Q�B
RB
R�B
SuB
S[B
S�B
S�B
S�B
S�B
T,B
T{B
UB
UMB
U�B
U�B
U�B
VB
VB
U�B
U�B
U�B
V9B
W
B
W�B
W�B
WsB
W�B
XB
XyB
X�B
X�B
YB
X�B
YB
X�B
YB
Z7B
ZQB
ZkB
[#B
[#B
[qB
[qB
[�B
\�B
]dB
]�B
]�B
]~B
]/B
\�B
\�B
]IB
]dB
]�B
^jB
^�B
_B
_;B
_�B
`'B
`B
_�B
_�B
_�B
_�B
`�B
a�B
a�B
a�B
bB
bhB
b�B
b�B
cB
cnB
c:B
c:B
c�B
c�B
c�B
c�B
d&B
dtB
dB
c�B
d@B
d�B
e�B
e�B
e�B
ffB
f�B
fLB
f2B
fLB
f�B
f�B
g8B
g�B
g�B
g�B
g�B
gmB
gmB
g�B
g�B
g�B
g�B
h$B
h�B
i*B
i*B
i_B
iyB
i�B
i�B
i�B
jB
j�B
j�B
kB
j�B
j�B
j�B
k�B
lWB
l�B
l�B
lWB
l=B
l"B
l"B
lqB
l�B
l�B
l�B
l�B
mB
m)B
m�B
m�B
nB
n}B
o B
o B
n�B
n�B
o�B
qB
q�B
q�B
q�B
rB
rB
rGB
r�B
r�B
r�B
r�B
r�B
sMB
s�B
tB
tB
tB
tB
tB
tB
t9B
tB
tB
t9B
t�B
t�B
t�B
uZB
vB
vB
vzB
vzB
v�B
v�B
v�B
v�B
v�B
wB
wB
w�B
w�B
w�B
xB
xlB
x�B
x�B
x�B
x�B
y>B
y>B
y>B
y$B
y$B
y�B
y�B
zB
zDB
z^B
z�B
z�B
z�B
z�B
z�B
{�B
|B
|B
|PB
|PB
|�B
}B
}<B
}�B
}�B
}�B
}�B
}�B
~(B
~wB
~�B
~�B
~�B
}B
�B
� B
�OB
�iB
� B
� B
�B
�;B
�UB
�UB
�oB
�oB
��B
��B
��B
��B
�AB
�[B
�uB
��B
�B
�aB
��B
��B
��B
�B
�MB
�gB
�gB
��B
��B
��B
�SB
�mB
��B
��B
��B
�%B
�tB
�tB
�tB
�YB
�%B
�B
�B
�%B
�YB
�tB
��B
�+B
��B
��B
��B
��B
��B
��B
�B
�B
�1B
�fB
��B
��B
�KB
�fB
��B
�B
��B
��B
��B
��B
�lB
��B
�	B
�#B
�XB
�XB
�rB
��B
�rB
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105250  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192937  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192937  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192937                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042945  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042945  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                