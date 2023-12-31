CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:22:41Z creation;2022-06-04T17:22:41Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ``   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604172241  20220610121506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ئЮz�1   @ئ�(d�@,'-�d��"��`1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�33@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   BffBffBffB   B'��B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�33B�33B�  B�33B���B���B�  B�  B�33B�33B���B�  B�  B�  B�33B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C	�fC  C  C  C  C  C  C  C�C�C�C   C!�fC#�fC%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@|(�@��H@��HAp�A=p�A]p�A{�
A��RA��RA��RA��RAθRA޸RA�RA��RBBBB\)B&��B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bf��Bo\)Bw\)BB��GB��B��GB�z�B�z�B��B��B��GB��GB�G�B��B��B��B��GB��GB�G�B�z�BǮBˮBϮBӮB׮BۮB߮B��GB�B�B�B�B��B��B��C�
C�
C�pC�
C	�pC�
C�
C�
C�
C�
C�
C�
C�C�C�C�
C!�pC#�pC%�pC'�
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
CE�CG�CI�
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
Ce�pCg�pCi�
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
C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D�)Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1|)D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�7�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A͟VA͚kAͣ:Aͣ�AͤA͛	A͞OA͢hA͟VA͡-Aͥ�AͥzAͧAͪ�AͬqAͭ�Aͯ�AͰ�Aͱ�AͲ�Aͳ�Aͮ�Aͪ�AͦAͤtA͙eA͂A�z�A͙�A�ݘA��|Aͽ�A͚A͇+A�v�A�N�A�?HA�.A��A��vA�T�A��WAȰ�AǬqA�+A�qA��A��jA�@OA��RA���A�/�A���A��A�>A�ԕA��A�:^A�2-A���A��A���A��xA�<�A���A��?A��<A��A���A��A���A�?}A��	A�!-A��tA�A�A��RA��sA�!bA��wA��A��MA��A�TaA���A�W�A���A��A���A���A|K^Av�Aq�PAnC-Ai�Af�@AdN<AcIRA[��AX�HAVU2AT�UAR��AM��AIYAF��AD($AB1A?�A?�A>�kA=rGA<�9A<D�A;kQA9A8�A7bA6eA5�YA4/�A2v`A1�uA/kQA.<6A,��A+�DA*یA)4A(��A(!�A'o�A&8A$˒A#��A!u�A<6AOvAg�A~(A-wAA�A�QAȴA�CA�BA��A�A�A�
A+AϫA?}As�A�4A��AO�A  A��Ac�A�$AOA�A�KA��A*0Aa�A�AXA%AcAbA[WA��AS�AFA�A��A}�A�'AHA��A��AOvA
<6A	�A	�A
=A��A0UA�AA�A$�A��AƨA,=A#�A�A�A3�A�fAXyA�'AB�A�bA�A��An/A=A �2A ��A S�A 7@���@�*0@��@�u�@���@���@��P@��|@��o@���@���@�K�@��r@���@�Vm@�~(@��@�:�@��'@�A�@�?�@�Q�@�7�@�G@�o�@�@��)@�M@�1@�Ov@�_@��@��@�6@�=q@�}V@��@柾@�.�@�$�@�<�@�`�@�l�@�:*@��m@��|@���@�$t@�Y@��@�\�@� \@�ѷ@��@�"h@�c @݁�@�o @�W?@�7L@���@܉�@��@۫�@�Y�@�c�@ٯ�@�qv@�O�@�d�@�@և+@��@��@�l"@�&�@�ݘ@Ӳ�@ӑh@�S@�:�@��?@�!@ώ�@�a�@��r@�Z�@��@�ں@��)@̩�@�5?@�K�@ʕ�@�G@�֡@ȇ�@�YK@�B[@�@��@���@Ǹ�@ǜ@�F@Ɠu@��Q@ŕ�@�`B@�@O@�*0@��@Ē�@ĂA@�h
@�:*@��@þw@� \@�;�@�*0@���@�(�@��[@�p�@�]�@�=@��E@���@��L@�i�@��@��@@�j@�4@��$@�1'@���@�&�@���@�{�@�*�@��@�x�@�C�@��@��@���@��@��F@��]@�	l@�,=@��@��@��@��L@�J�@���@�iD@�5�@��@��@�s�@��3@���@�S�@�>�@��s@��H@�@���@��D@���@���@�YK@��@��@�%@�M@��@�u�@��@���@�Q�@�@�U�@��@��R@���@���@�oi@�4@��@�/�@�Y@�@�@���@���@�u�@�GE@�C@��@�xl@�a|@�0U@��0@�_p@�S�@�@O@���@�YK@�-�@��@��@�g�@�V@��@�`�@�  @��	@�=�@��@���@�v�@�H@���@���@�|@�'�@���@��z@�z�@�U2@�6�@��D@��w@��k@���@�o @�W?@��@���@��Z@���@���@�o @��@��s@��h@�GE@��@��@���@�\�@��y@���@��1@��D@�d�@�J@��q@��=@�p�@�33@� i@���@�u�@��@���@�RT@�&�@��@��f@���@�3�@���@�y�@�F�@���@���@�h
@�-�@�ƨ@�x�@�]�@��K@��@���@�a@���@���@� �@���@�zx@�O@���@�Ta@��@��@@��h@�RT@��@��@���@��@�"h@���@��a@���@�s@��@�H�@��@���@�.I@��@��8@��m@���@�� @�s�@�U2@��@~��@~E�@~6�@}��@}�@|�O@|?�@{E9@z�h@zC�@yF@xu�@x6@x�@w�+@w�W@w��@w�6@w��@w��@wt�@vs�@u8�@t�I@tg8@tI�@t4n@t!@s�m@s i@q��@q^�@p��@p�4@p7@o��@oP�@n^5@m^�@l�@l��@k�+@k�V@kC@j�}@ju%@i�@irG@i%@h�@h:�@h�@g�*@g�@fZ�@e�@e��@d��@c�*@cx@cO@b�X@bO@`�_@_ƨ@_W?@_4�@_,�@_)_@_C@_S@^�@^($@]��@]�n@]�@]0�@\�$@\I�@\�@[��@[��@[~�@[iD@[j�@[9�@Z�'@Z?@Y��@Y�@YX@Y*0@X�Y@Xe�@XI�@XG@W��@W�V@W��@W�4@WZ�@V�y@V8�@U�)@U�@UV@T�f@T��@T�Y@TFt@S��@Sƨ@SdZ@R��@R�@Ph�@O�@N+k@MT�@L�@L�@L�_@L�D@LV�@K�@J�c@J�2@J�H@J�@J��@J��@JJ�@J�@I�@I��@IG�@I+�@I!�@I+@H�@Hoi@G��@G�f@G8@F�@F�@F�'@F��@F��@Ew2@DtT@C˒@C|�@CE9@C33@C�@B��@B�]@Bȴ@B�@B��@Bu%@BTa@B�@A��@Aϫ@A��@A��@A�@@2�@?�;@?iD@>�@>�+@>YK@>:*@>($@>e@=�@=q@<y>@;>�@9ϫ@9Q�@9%F@8�@8�z@8[�@7��@7�V@7��@7|�@7J#@7/�@7�@6��@6��@6ߤ@6��@6Ov@66�@61�@6)�@6�@5��@5��@4��@4�@3��@3�*@3��@3��@3�f@3t�@3Mj@3 i@2��@2)�@1��@1�@1��@1@1��@1zx@1 \@0��@0�@/qv@/P�@.�y@.��@.ȴ@.��@.~�@._�@.GE@.�@-�@-�@-L�@,��@,ѷ@,��@,g8@+�q@*�X@*��@*+k@* �@)��@)rG@)G�@)2a@(��@(��@(�@(��@(U2@(,=@(7@(1@'��@'s@'C�@'@&�"@&�y@&��@&�@&�1@&p;@&_�@&Z�@&8�@&�@%�@%j@$�E@$�@$q@$S�@$9X@$�@#� @#Mj@#33@#Y@#�@#�@# i@"�<@"s�@"h
@"E�@"{@!x�@!8�@!�@ �`@ �p@ �O@ �@ ]d@ N�@ C-@ 6@ 2�@ !@��@��@Z�@8@�8@�6@�A@R�@-@	@�@�C@zx@S&@/@�	@֡@��@�e@�u@V�@,=@@H�@��@�b@Ta@B[@0U@	@
�@J@��@�3@�^@��@��@�C@��@�h@|@p�@m]@\�@IR@<6@�@�@PH@�@�@�@�+@�@�&@�w@t�@U�@@O@8@9�@6z@4�@)_@
=@ i@ߤ@�@�A@Ta@Q@Q@R�@Q@Ov@.�@_@��@�o@��@��@O�@/@	l@�K@��@��@��@��@�o@`�@K^@?�@9X@7�@2�@ �@�@�
@ƨ@��@��@��@l�@�@�@��@�L@��@��@z@C�@�@�@�3@��@��@A @+�@�	@�$@�@�@y>@-�@�@  @�@ݘ@�@��@��@��@�	@v`@X�@Y@�@�H@�1@��@E�@4@�M@\�@L�@J�@G�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A͟VA͚kAͣ:Aͣ�AͤA͛	A͞OA͢hA͟VA͡-Aͥ�AͥzAͧAͪ�AͬqAͭ�Aͯ�AͰ�Aͱ�AͲ�Aͳ�Aͮ�Aͪ�AͦAͤtA͙eA͂A�z�A͙�A�ݘA��|Aͽ�A͚A͇+A�v�A�N�A�?HA�.A��A��vA�T�A��WAȰ�AǬqA�+A�qA��A��jA�@OA��RA���A�/�A���A��A�>A�ԕA��A�:^A�2-A���A��A���A��xA�<�A���A��?A��<A��A���A��A���A�?}A��	A�!-A��tA�A�A��RA��sA�!bA��wA��A��MA��A�TaA���A�W�A���A��A���A���A|K^Av�Aq�PAnC-Ai�Af�@AdN<AcIRA[��AX�HAVU2AT�UAR��AM��AIYAF��AD($AB1A?�A?�A>�kA=rGA<�9A<D�A;kQA9A8�A7bA6eA5�YA4/�A2v`A1�uA/kQA.<6A,��A+�DA*یA)4A(��A(!�A'o�A&8A$˒A#��A!u�A<6AOvAg�A~(A-wAA�A�QAȴA�CA�BA��A�A�A�
A+AϫA?}As�A�4A��AO�A  A��Ac�A�$AOA�A�KA��A*0Aa�A�AXA%AcAbA[WA��AS�AFA�A��A}�A�'AHA��A��AOvA
<6A	�A	�A
=A��A0UA�AA�A$�A��AƨA,=A#�A�A�A3�A�fAXyA�'AB�A�bA�A��An/A=A �2A ��A S�A 7@���@�*0@��@�u�@���@���@��P@��|@��o@���@���@�K�@��r@���@�Vm@�~(@��@�:�@��'@�A�@�?�@�Q�@�7�@�G@�o�@�@��)@�M@�1@�Ov@�_@��@��@�6@�=q@�}V@��@柾@�.�@�$�@�<�@�`�@�l�@�:*@��m@��|@���@�$t@�Y@��@�\�@� \@�ѷ@��@�"h@�c @݁�@�o @�W?@�7L@���@܉�@��@۫�@�Y�@�c�@ٯ�@�qv@�O�@�d�@�@և+@��@��@�l"@�&�@�ݘ@Ӳ�@ӑh@�S@�:�@��?@�!@ώ�@�a�@��r@�Z�@��@�ں@��)@̩�@�5?@�K�@ʕ�@�G@�֡@ȇ�@�YK@�B[@�@��@���@Ǹ�@ǜ@�F@Ɠu@��Q@ŕ�@�`B@�@O@�*0@��@Ē�@ĂA@�h
@�:*@��@þw@� \@�;�@�*0@���@�(�@��[@�p�@�]�@�=@��E@���@��L@�i�@��@��@@�j@�4@��$@�1'@���@�&�@���@�{�@�*�@��@�x�@�C�@��@��@���@��@��F@��]@�	l@�,=@��@��@��@��L@�J�@���@�iD@�5�@��@��@�s�@��3@���@�S�@�>�@��s@��H@�@���@��D@���@���@�YK@��@��@�%@�M@��@�u�@��@���@�Q�@�@�U�@��@��R@���@���@�oi@�4@��@�/�@�Y@�@�@���@���@�u�@�GE@�C@��@�xl@�a|@�0U@��0@�_p@�S�@�@O@���@�YK@�-�@��@��@�g�@�V@��@�`�@�  @��	@�=�@��@���@�v�@�H@���@���@�|@�'�@���@��z@�z�@�U2@�6�@��D@��w@��k@���@�o @�W?@��@���@��Z@���@���@�o @��@��s@��h@�GE@��@��@���@�\�@��y@���@��1@��D@�d�@�J@��q@��=@�p�@�33@� i@���@�u�@��@���@�RT@�&�@��@��f@���@�3�@���@�y�@�F�@���@���@�h
@�-�@�ƨ@�x�@�]�@��K@��@���@�a@���@���@� �@���@�zx@�O@���@�Ta@��@��@@��h@�RT@��@��@���@��@�"h@���@��a@���@�s@��@�H�@��@���@�.I@��@��8@��m@���@�� @�s�@�U2@��@~��@~E�@~6�@}��@}�@|�O@|?�@{E9@z�h@zC�@yF@xu�@x6@x�@w�+@w�W@w��@w�6@w��@w��@wt�@vs�@u8�@t�I@tg8@tI�@t4n@t!@s�m@s i@q��@q^�@p��@p�4@p7@o��@oP�@n^5@m^�@l�@l��@k�+@k�V@kC@j�}@ju%@i�@irG@i%@h�@h:�@h�@g�*@g�@fZ�@e�@e��@d��@c�*@cx@cO@b�X@bO@`�_@_ƨ@_W?@_4�@_,�@_)_@_C@_S@^�@^($@]��@]�n@]�@]0�@\�$@\I�@\�@[��@[��@[~�@[iD@[j�@[9�@Z�'@Z?@Y��@Y�@YX@Y*0@X�Y@Xe�@XI�@XG@W��@W�V@W��@W�4@WZ�@V�y@V8�@U�)@U�@UV@T�f@T��@T�Y@TFt@S��@Sƨ@SdZ@R��@R�@Ph�@O�@N+k@MT�@L�@L�@L�_@L�D@LV�@K�@J�c@J�2@J�H@J�@J��@J��@JJ�@J�@I�@I��@IG�@I+�@I!�@I+@H�@Hoi@G��@G�f@G8@F�@F�@F�'@F��@F��@Ew2@DtT@C˒@C|�@CE9@C33@C�@B��@B�]@Bȴ@B�@B��@Bu%@BTa@B�@A��@Aϫ@A��@A��@A�@@2�@?�;@?iD@>�@>�+@>YK@>:*@>($@>e@=�@=q@<y>@;>�@9ϫ@9Q�@9%F@8�@8�z@8[�@7��@7�V@7��@7|�@7J#@7/�@7�@6��@6��@6ߤ@6��@6Ov@66�@61�@6)�@6�@5��@5��@4��@4�@3��@3�*@3��@3��@3�f@3t�@3Mj@3 i@2��@2)�@1��@1�@1��@1@1��@1zx@1 \@0��@0�@/qv@/P�@.�y@.��@.ȴ@.��@.~�@._�@.GE@.�@-�@-�@-L�@,��@,ѷ@,��@,g8@+�q@*�X@*��@*+k@* �@)��@)rG@)G�@)2a@(��@(��@(�@(��@(U2@(,=@(7@(1@'��@'s@'C�@'@&�"@&�y@&��@&�@&�1@&p;@&_�@&Z�@&8�@&�@%�@%j@$�E@$�@$q@$S�@$9X@$�@#� @#Mj@#33@#Y@#�@#�@# i@"�<@"s�@"h
@"E�@"{@!x�@!8�@!�@ �`@ �p@ �O@ �@ ]d@ N�@ C-@ 6@ 2�@ !@��@��@Z�@8@�8@�6@�A@R�@-@	@�@�C@zx@S&@/@�	@֡@��@�e@�u@V�@,=@@H�@��@�b@Ta@B[@0U@	@
�@J@��@�3@�^@��@��@�C@��@�h@|@p�@m]@\�@IR@<6@�@�@PH@�@�@�@�+@�@�&@�w@t�@U�@@O@8@9�@6z@4�@)_@
=@ i@ߤ@�@�A@Ta@Q@Q@R�@Q@Ov@.�@_@��@�o@��@��@O�@/@	l@�K@��@��@��@��@�o@`�@K^@?�@9X@7�@2�@ �@�@�
@ƨ@��@��@��@l�@�@�@��@�L@��@��@z@C�@�@�@�3@��@��@A @+�@�	@�$@�@�@y>@-�@�@  @�@ݘ@�@��@��@��@�	@v`@X�@Y@�@�H@�1@��@E�@4@�M@\�@L�@J�@G�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�`B�2B�2B�B��B��B�FB��B��B�FB��B��B�zB��B��B��B�mB��B��B�XB�WB��B�B��B�B�fB�aB	=B
 OB
��B
��B
�}B
ϑB
̈́B
�VB
ބB
��B
�B
�9B
�0B�B{JB�B��B��B��B��B��B��B��B1B�B%,B+B5�B8�B:�B<�B>wB>�B=�B=�B=�B<�B88B4B1[B+B#B{B
�B�B�B��Bw�BL�B<B0�B�B
�B
�"B
�dB
J�B
2�B
-wB
#nB
�B
+B	�?B	�IB	�B	�SB	y�B	a�B	P�B	E�B	=qB	/�B	�B	�B	~B	SB��B�B�LB�`B�B�0B�B��B�iB��B�AB�tB	
�B	�B	B	�B	uB	B	.B	mB	�B	bB		�B	�B	�B	NB	�B	�B	IB	xB	�B	@B	@B	�B	!B	jB	�B	 BB	$@B	.IB	D�B	O�B	[�B	k�B	m)B	lB	w�B	~�B	�uB	��B	��B	��B	�nB	�B	��B	ÖB	˒B	�B	��B	�B	��B	��B	�hB	�hB	�-B	��B	�B	�WB	ؓB	ҽB	�rB	��B	ǔB	�B	�zB	�_B	�?B	��B	āB	��B	�mB	��B	��B	��B	�zB	��B	�}B	�oB	�vB	�B	�hB	̈́B	�B	�]B	��B	ބB	�B	��B	�B	�B	��B	��B	�/B	��B	ޞB	�B	ݘB	�IB	�xB	��B	�)B	��B	ۦB	چB	��B	��B	��B	�#B	��B	��B	�WB	ںB	��B	�B	ܒB	ޞB	�pB	�;B	�!B	޸B	�B	�xB	�dB	�FB	�zB	��B	�,B	�B	�:B	��B	ߤB	�OB	�IB	�/B	ܬB	ܬB	�;B	ߊB	ݲB	��B	�pB	�vB	��B	��B	�,B	�`B	�`B	��B	�B	�0B	�B	��B	�B	�B	�QB	�6B	�cB	�5B	�B	��B	��B	��B	�B	�OB	�}B	�/B	�}B	��B	��B	�GB	�-B	��B	�B	��B	�B	�vB	�AB	�AB	��B	�B	�UB	��B	�cB	��B	��B	�hB	��B	�B	��B	�B	��B	�3B	�3B	��B	�vB	��B	�'B	�[B	��B	��B	�GB	�aB	�B	��B	��B	��B	��B	��B	�?B	��B	��B	�LB	��B	��B	�PB	�]B	�.B	��B	�wB	�B	�B	��B	�B	�^B	��B	��B	�VB	�(B	�(B	��B	�BB	�BB	��B	��B	�qB	�VB	�BB	��B	�]B	�.B	��B	��B	��B	��B	�HB	��B
 iB
UB
UB
 �B
 �B
 �B
 B
;B
oB
�B
�B
�B
3B
B
MB
gB
MB
B
MB
�B
MB
�B
MB
MB
�B
9B
SB
9B
SB
�B
�B
GB
AB
 �B
AB
�B
gB
aB
AB
 �B
UB
B
_B
1B
�B
�B
�B

#B
)B
�B
�B
�B
�B
JB
0B
�B
B
PB
�B
�B
�B
�B
.B
 B
 B
B
hB
B
 B
�B
�B
�B
FB
gB
�B
�B
�B
sB
�B
+B
B
+B
�B
�B
B
eB
B
B
7B
kB
�B
�B
	B
	B
	B
	B
	B
WB
WB
]B
B
�B
)B
�B
IB
�B
5B
�B
�B
;B
�B
 B
 BB
 B
 B
�B
 B
 BB
 BB
 BB
 \B
 �B
!bB
!-B
!�B
"hB
"�B
"�B
"�B
"�B
"�B
#:B
#�B
$B
$@B
$�B
$�B
%FB
%`B
&2B
&fB
&2B
&�B
'mB
'�B
(
B
(�B
(�B
)�B
*KB
*KB
*�B
+�B
,B
,�B
-]B
-wB
./B
.�B
/ B
/iB
/�B
0�B
0�B
1B
1AB
0�B
1�B
1�B
2GB
2-B
2�B
2aB
2�B
2�B
2�B
3B
3B
3MB
4B
4�B
4�B
4�B
5%B
5ZB
5�B
6+B
6+B
6zB
6�B
7�B
8lB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9XB
:B
:*B
:DB
:DB
:DB
:*B
:B
:�B
;0B
;�B
;�B
;�B
<6B
<PB
<jB
="B
=�B
=�B
=�B
>]B
>wB
>�B
?B
?B
?�B
?�B
@ B
@OB
@iB
@iB
@�B
@�B
A�B
A�B
AoB
B[B
B�B
B�B
B�B
B�B
B�B
DB
D3B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EmB
ESB
EmB
EmB
E�B
E�B
FtB
F�B
FtB
F�B
F�B
F�B
F�B
F�B
G+B
G�B
G�B
G�B
HB
HB
H�B
H�B
H�B
H�B
H�B
H�B
IB
H�B
H�B
I7B
I�B
I�B
J	B
JXB
J=B
JXB
JrB
J�B
J�B
J�B
J�B
J�B
KxB
L�B
MB
NVB
N�B
OBB
O\B
OvB
O\B
O(B
P}B
PbB
PbB
PHB
P}B
P}B
P�B
P�B
Q B
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RTB
R�B
R�B
SB
S[B
S@B
S@B
S&B
SB
TFB
T�B
UB
T�B
UB
T�B
U2B
UMB
UgB
UMB
UMB
UgB
UgB
U�B
U�B
VB
U�B
U�B
U�B
VB
U�B
U�B
U�B
VSB
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
YeB
Y�B
ZQB
ZQB
Z�B
Z�B
Z�B
[=B
[�B
[�B
[�B
[�B
[�B
\B
\)B
\CB
\]B
\�B
]/B
]dB
]�B
^B
^5B
^B
^B
^jB
^�B
_B
_!B
_!B
_!B
_!B
_!B
_B
_�B
_�B
`'B
`vB
`vB
`�B
`�B
`�B
abB
a�B
a�B
bNB
b�B
b�B
b�B
b�B
b�B
cTB
cTB
c�B
c�B
c�B
c�B
d@B
dZB
dtB
d�B
d�B
d�B
e�B
f�B
f�B
gB
g8B
g�B
g�B
h
B
h
B
h>B
hXB
hXB
hsB
h�B
h�B
h�B
h�B
h�B
iDB
i_B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jB
i�B
j0B
jB
jB
jB
k6B
kQB
kkB
kQB
kkB
k�B
k�B
lWB
lWB
lqB
lqB
lqB
lqB
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
n/B
n/B
ncB
ncB
n}B
ncB
n}B
n}B
n�B
n�B
oOB
o5B
o�B
o�B
o�B
o�B
o�B
pB
pB
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
qB
p�B
r-B
rGB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
sMB
s3B
s3B
s3B
s3B
shB
sMB
shB
s�B
sMB
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u?B
uZB
utB
u�B
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
vB
v+B
vFB
vFB
v+B
vFB
v+B
vzB
vzB
v�B
vzB
v�B
v�B
wB
w2B
wLB
wfB
wLB
w�B
w�B
w�B
w�B
w�B
x8B
xB
x8B
xB
xB
xRB
x�B
xlB
x�B
xlB
x�B
x�B
x�B
y	B
y	B
yXB
yXB
yrB
yXB
yrB
y�B
y�B
y�B
zB
zB
zxB
zxB
z�B
z�B
{B
{0B
z�B
{JB
{�B
{B
{�B
{dB
{�B
{0B
{JB
|6B
|PB
{�B
{B
{�B
{�B
{�B
|jB
}"B
|PB
}B
|�B
}�B
}�B
}�B
}�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�`B�2B�2B�B��B��B�FB��B��B�FB��B��B�zB��B��B��B�mB��B��B�XB�WB��B�B��B�B�fB�aB	=B
 OB
��B
��B
�}B
ϑB
̈́B
�VB
ބB
��B
�B
�9B
�0B�B{JB�B��B��B��B��B��B��B��B1B�B%,B+B5�B8�B:�B<�B>wB>�B=�B=�B=�B<�B88B4B1[B+B#B{B
�B�B�B��Bw�BL�B<B0�B�B
�B
�"B
�dB
J�B
2�B
-wB
#nB
�B
+B	�?B	�IB	�B	�SB	y�B	a�B	P�B	E�B	=qB	/�B	�B	�B	~B	SB��B�B�LB�`B�B�0B�B��B�iB��B�AB�tB	
�B	�B	B	�B	uB	B	.B	mB	�B	bB		�B	�B	�B	NB	�B	�B	IB	xB	�B	@B	@B	�B	!B	jB	�B	 BB	$@B	.IB	D�B	O�B	[�B	k�B	m)B	lB	w�B	~�B	�uB	��B	��B	��B	�nB	�B	��B	ÖB	˒B	�B	��B	�B	��B	��B	�hB	�hB	�-B	��B	�B	�WB	ؓB	ҽB	�rB	��B	ǔB	�B	�zB	�_B	�?B	��B	āB	��B	�mB	��B	��B	��B	�zB	��B	�}B	�oB	�vB	�B	�hB	̈́B	�B	�]B	��B	ބB	�B	��B	�B	�B	��B	��B	�/B	��B	ޞB	�B	ݘB	�IB	�xB	��B	�)B	��B	ۦB	چB	��B	��B	��B	�#B	��B	��B	�WB	ںB	��B	�B	ܒB	ޞB	�pB	�;B	�!B	޸B	�B	�xB	�dB	�FB	�zB	��B	�,B	�B	�:B	��B	ߤB	�OB	�IB	�/B	ܬB	ܬB	�;B	ߊB	ݲB	��B	�pB	�vB	��B	��B	�,B	�`B	�`B	��B	�B	�0B	�B	��B	�B	�B	�QB	�6B	�cB	�5B	�B	��B	��B	��B	�B	�OB	�}B	�/B	�}B	��B	��B	�GB	�-B	��B	�B	��B	�B	�vB	�AB	�AB	��B	�B	�UB	��B	�cB	��B	��B	�hB	��B	�B	��B	�B	��B	�3B	�3B	��B	�vB	��B	�'B	�[B	��B	��B	�GB	�aB	�B	��B	��B	��B	��B	��B	�?B	��B	��B	�LB	��B	��B	�PB	�]B	�.B	��B	�wB	�B	�B	��B	�B	�^B	��B	��B	�VB	�(B	�(B	��B	�BB	�BB	��B	��B	�qB	�VB	�BB	��B	�]B	�.B	��B	��B	��B	��B	�HB	��B
 iB
UB
UB
 �B
 �B
 �B
 B
;B
oB
�B
�B
�B
3B
B
MB
gB
MB
B
MB
�B
MB
�B
MB
MB
�B
9B
SB
9B
SB
�B
�B
GB
AB
 �B
AB
�B
gB
aB
AB
 �B
UB
B
_B
1B
�B
�B
�B

#B
)B
�B
�B
�B
�B
JB
0B
�B
B
PB
�B
�B
�B
�B
.B
 B
 B
B
hB
B
 B
�B
�B
�B
FB
gB
�B
�B
�B
sB
�B
+B
B
+B
�B
�B
B
eB
B
B
7B
kB
�B
�B
	B
	B
	B
	B
	B
WB
WB
]B
B
�B
)B
�B
IB
�B
5B
�B
�B
;B
�B
 B
 BB
 B
 B
�B
 B
 BB
 BB
 BB
 \B
 �B
!bB
!-B
!�B
"hB
"�B
"�B
"�B
"�B
"�B
#:B
#�B
$B
$@B
$�B
$�B
%FB
%`B
&2B
&fB
&2B
&�B
'mB
'�B
(
B
(�B
(�B
)�B
*KB
*KB
*�B
+�B
,B
,�B
-]B
-wB
./B
.�B
/ B
/iB
/�B
0�B
0�B
1B
1AB
0�B
1�B
1�B
2GB
2-B
2�B
2aB
2�B
2�B
2�B
3B
3B
3MB
4B
4�B
4�B
4�B
5%B
5ZB
5�B
6+B
6+B
6zB
6�B
7�B
8lB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9XB
:B
:*B
:DB
:DB
:DB
:*B
:B
:�B
;0B
;�B
;�B
;�B
<6B
<PB
<jB
="B
=�B
=�B
=�B
>]B
>wB
>�B
?B
?B
?�B
?�B
@ B
@OB
@iB
@iB
@�B
@�B
A�B
A�B
AoB
B[B
B�B
B�B
B�B
B�B
B�B
DB
D3B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EmB
ESB
EmB
EmB
E�B
E�B
FtB
F�B
FtB
F�B
F�B
F�B
F�B
F�B
G+B
G�B
G�B
G�B
HB
HB
H�B
H�B
H�B
H�B
H�B
H�B
IB
H�B
H�B
I7B
I�B
I�B
J	B
JXB
J=B
JXB
JrB
J�B
J�B
J�B
J�B
J�B
KxB
L�B
MB
NVB
N�B
OBB
O\B
OvB
O\B
O(B
P}B
PbB
PbB
PHB
P}B
P}B
P�B
P�B
Q B
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RTB
R�B
R�B
SB
S[B
S@B
S@B
S&B
SB
TFB
T�B
UB
T�B
UB
T�B
U2B
UMB
UgB
UMB
UMB
UgB
UgB
U�B
U�B
VB
U�B
U�B
U�B
VB
U�B
U�B
U�B
VSB
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
YeB
Y�B
ZQB
ZQB
Z�B
Z�B
Z�B
[=B
[�B
[�B
[�B
[�B
[�B
\B
\)B
\CB
\]B
\�B
]/B
]dB
]�B
^B
^5B
^B
^B
^jB
^�B
_B
_!B
_!B
_!B
_!B
_!B
_B
_�B
_�B
`'B
`vB
`vB
`�B
`�B
`�B
abB
a�B
a�B
bNB
b�B
b�B
b�B
b�B
b�B
cTB
cTB
c�B
c�B
c�B
c�B
d@B
dZB
dtB
d�B
d�B
d�B
e�B
f�B
f�B
gB
g8B
g�B
g�B
h
B
h
B
h>B
hXB
hXB
hsB
h�B
h�B
h�B
h�B
h�B
iDB
i_B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jB
i�B
j0B
jB
jB
jB
k6B
kQB
kkB
kQB
kkB
k�B
k�B
lWB
lWB
lqB
lqB
lqB
lqB
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
n/B
n/B
ncB
ncB
n}B
ncB
n}B
n}B
n�B
n�B
oOB
o5B
o�B
o�B
o�B
o�B
o�B
pB
pB
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
qB
p�B
r-B
rGB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
sMB
s3B
s3B
s3B
s3B
shB
sMB
shB
s�B
sMB
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u?B
uZB
utB
u�B
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
vB
v+B
vFB
vFB
v+B
vFB
v+B
vzB
vzB
v�B
vzB
v�B
v�B
wB
w2B
wLB
wfB
wLB
w�B
w�B
w�B
w�B
w�B
x8B
xB
x8B
xB
xB
xRB
x�B
xlB
x�B
xlB
x�B
x�B
x�B
y	B
y	B
yXB
yXB
yrB
yXB
yrB
y�B
y�B
y�B
zB
zB
zxB
zxB
z�B
z�B
{B
{0B
z�B
{JB
{�B
{B
{�B
{dB
{�B
{0B
{JB
|6B
|PB
{�B
{B
{�B
{�B
{�B
|jB
}"B
|PB
}B
|�B
}�B
}�B
}�B
}�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104842  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172241  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172241  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172241                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022248  QCP$                G�O�G�O�G�O�         208F35EJA  ARGQrqcpc3.6                                                                20220605022248  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610121506                      G�O�G�O�G�O�                