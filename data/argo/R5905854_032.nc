CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:50:14Z creation;2022-06-04T17:50:14Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604175014  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL                A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @���   1   @�����Ն@.gl�C���c;dZ�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A!��A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP��BV  B_��Bg��Bo��Bx  B���B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  BЙ�B�  B�  B���B���B�  B�  B�  B�  B�  B�33B�33C   C  C�C��C�fC	�fC  C  C  C  C  C  C  C33C�fC  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"�fD#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ DŃ3D�� D�  D�@ Dƀ D�� D�  D�C3Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@\)@u@��H@��HA
>A;�
A]p�A}p�A��RA��RA��RA��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BGBP(�BU\)B^��Bf��Bn��Bw\)B�G�B��GB��B��B��B��B��B��B�z�B��B��B��B�z�B��B��B��B��BîBǮBˮB�G�BӮB׮B�z�B�z�B�B�B�B�B�B��GB��GB��C�
C�C��C�pC	�pC�
C�
C�
C�
C�
C�
C�
C
=C�pC�
C�
C!�
C#�pC%�
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
CM�CO�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"|)D"�)D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DT|)DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�~Dź�D���D�:�D�z�Dƺ�D���D�>D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D��G111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�K�A�GA�G�A�H�A�H�A�E9A�P�A�PHA�S�A�V�A�V9A�ZQA�iyA�h�A�i�A�l�A�n/A�n�A�p;A�qA�p�A�x8A�{AԁoAԊ	Aԏ(AԔ�Aԃ�A�  Aҍ�A��A�"A���AЕ�AЯAи�A���A�,�A�Q�A�5tA�aA�d�A��A��A�IA�F?A�m�A��A� �A���A�?�A���A�$A�6�A��_A�`A��;A���A��A��_A�A A��A�רA�yrA��MA�@OA� �A���A�.A�~�A���A�33A��XA��=A���A��IAw֡Aq��Ak�Ai��Ai3�AhC�Ae�)Ab�*A_��A^�AZ��AW�AT�ARa�AOXAL  AI�[AHI�AF�ZAE1�AB&�A@�0A>��A<VmA9^�A8�AA8  A7?A6��A6�.A4�FA3��A3b�A2=qA1�A/�|A.4A,B[A+�A*��A*=A)ѷA(�A'��A%kQA$��A#�A#($A"�HA!�PA!($A ,=A�jAZA�A��Ag�A+AW�AA�	A�A�,AoiA�A&�A�HA*�A�A�A=�A-wAw�A��AA�eA�AxlAOvA�Af�Av`A�A�vA�AJ�A�zA~A�HAY�A*�AȴA�.A�rA�'AsA0�A
��A
�zA
�A
��A
�A
	�A	��A	�A	~(A	aA
A
��A4A+kA	H�A�A�+A�)A��A�A'�A�PAAy�A��A�A�2A�A�QA�AE�Az�A�A��AA��AOvA ��A n/@��@�M�@��~@�(�@�q@��@@��u@��f@���@���@�d�@��@�#�@�xl@�@�-w@��@�֡@�Z�@�p�@���@�M�@�*@�H�@���@�}@���@�+�@�i�@��>@�b�@��@�<@��@�ff@�Xy@�F@��c@�`�@�4@��@� i@�r�@�J@�4�@��H@�h�@�m]@��@�Xy@߬q@��@�n�@ݠ�@�v�@�=�@ڶ�@���@�a�@؎�@��@�4@��]@�M@զ�@ԂA@Ӧ�@��@�6�@�S&@���@НI@�7�@ϩ*@��@��U@�1@�Vm@��@̼j@�	@�o�@�&@ʕ�@�;�@Ɏ�@���@�M@ǿH@�"�@��y@Ƈ�@��@�c�@���@ĆY@��z@��@�y>@�	@���@�S&@�C�@�PH@���@�;d@��@��.@���@���@�;@��Y@�7@��@�m]@��M@���@�:*@��W@�&@��f@��'@�l�@�!@��@���@��@���@���@���@�V�@�u@��@���@�c�@�C@���@�bN@��@�K�@��P@��E@���@�m�@�C�@��o@��@�u�@�*0@��@��@�^�@�S@���@�h
@�J�@���@���@�m]@�o@���@�~@���@���@���@�c�@��@���@��@�e�@�/�@���@�E9@��,@��!@�Z�@�e@���@�S@��@�oi@�H@�@���@�b�@�	l@�j@�c@�:�@�;@�ѷ@��x@�c�@�PH@��@��X@�p�@���@��@��A@�;d@��`@���@�Ft@��@���@���@�L�@� i@���@���@�j@���@���@��6@���@�H@��@���@��M@�1�@�(@��@���@��@�($@��j@��$@�o@��.@�6�@��r@��q@�@���@�ߤ@���@��o@�e�@�-@���@���@�E9@�;@�ں@��@��@���@�/�@��@��$@�}V@�_�@��&@���@�x@�+�@�oi@�u%@�L0@��@���@�G�@��@��/@���@�8�@�@���@���@���@�T�@���@���@���@�S�@�4@���@���@��-@�Vm@��@���@�~(@�9X@�J@��m@���@�zx@�zx@�v`@��@��!@��u@�r�@�U2@�*�@�	@���@��@�}�@�iD@�hs@�=@�!-@��@��H@���@���@��b@��@�u�@�H�@�7�@�-�@�
�@��>@��C@�|@�F@��@���@�u%@�I�@�+k@�b@��@��;@���@�B�@��@���@���@�v�@�8�@�{@~��@~GE@~	@}@}?}@|�	@|��@|��@|V�@|7@{�]@{�@{a@{�@z��@zu@ys�@y�@x�$@xtT@w�W@v�2@v1�@v�@v@u�)@u�@t�@ttT@te�@tXy@tPH@tC-@t'R@s�@s�@r� @q��@p��@p:�@o��@o�$@o9�@n�s@nxl@mϫ@mDg@l�@l!@k�@k��@k@j��@j��@jTa@i��@i8�@h~(@g��@g{J@g�@f6�@f	@e@d��@d1@c��@c��@cv`@cE9@b��@bߤ@b�@b�+@bH�@a�>@aN<@`�@`��@`Xy@`>B@_��@_��@_�a@_�0@_�*@_x@^��@^H�@]�@][W@\[�@[��@[o@Z�8@Z��@Z�X@Z�b@Z~�@Z�@Y��@Y-w@X�@X`�@W��@WO@V�A@Vu%@VB[@U��@Uc@Uq@T�@S�@S��@S�f@Sqv@SZ�@SH�@S6z@S.I@S@R��@R��@Rd�@R6�@Q�N@P�@P?�@O��@O��@O�;@O��@O=@N�@N�r@M�Z@M��@MrG@Mq@L��@L]d@L6@LG@K�@K��@Kg�@K6z@J��@J��@Jn�@J($@J�@Iϫ@I&�@H��@H<�@Ga@GS@F�<@F�+@FE�@E�.@E@E|@E�@D��@DQ�@Cخ@C��@Cg�@C/�@C
=@B�@B�R@BC�@A��@A@Ak�@A#�@@�?@@,=@?�A@?��@?ƨ@?t�@>��@>YK@>!�@=�T@=��@=c@=�@<�?@<C-@;8@:��@:v�@:B[@9��@9�3@9��@9 \@8��@8�Y@8�@7&@6�,@6^5@6_@5�)@5o @5�@4��@4��@4h�@4  @3��@3n/@2��@2c @1��@1��@1�@1\�@0�5@0�p@0y>@/��@/W?@/E9@/8@.��@.L0@.�@-��@-o @-�@,�@,V�@,(�@+��@+��@+$t@*��@*n�@*R�@*@)�@)�X@)7L@)@@(��@(�@(U2@(1'@'�r@'��@'�@'�@'dZ@'&@'�@&�,@&��@&��@&Ov@&($@%��@%��@%��@%G�@$��@$��@$Xy@$M@$'R@#��@#�:@#F�@#�@"��@"��@"v�@"Ta@!�.@!�-@!��@!\�@!�@ �E@ �@  �@�Q@��@8@��@�<@��@ff@J�@$�@J@�@��@�@�@�@c�@:�@�@��@˒@��@l�@�@�L@R�@�D@�@zx@e,@<6@-w@@@��@�@e�@N�@��@��@{J@J#@�@�@�,@��@��@v�@a|@8�@�Z@�d@�@��@�h@?}@��@�@��@�4@q@!@�w@��@��@��@~�@K�@.I@!-@�M@�B@s�@@�@�@�z@��@L�@A @#�@ \@�@�@��@��@��@2�@�A@˒@��@O@!-@�H@{�@J�@-@�@��@@�=@m]@B�@	l@��@�Y@g8@7�@@�6@��@X�@F�@�@
��@
��@
��@
�x@
��@
_�@
.�@
�@	�@	�~@	?}@	V@��@�`@��@�.@j@?�@@��@�[@�:@s@E9@�@�L@l�@Z�@Ta@H�@3�@&�@@�@�@�@�S@j@O�@:�@A @�@��@�?@��@j@N�@9X@�@@�@@  @�r@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�K�A�GA�G�A�H�A�H�A�E9A�P�A�PHA�S�A�V�A�V9A�ZQA�iyA�h�A�i�A�l�A�n/A�n�A�p;A�qA�p�A�x8A�{AԁoAԊ	Aԏ(AԔ�Aԃ�A�  Aҍ�A��A�"A���AЕ�AЯAи�A���A�,�A�Q�A�5tA�aA�d�A��A��A�IA�F?A�m�A��A� �A���A�?�A���A�$A�6�A��_A�`A��;A���A��A��_A�A A��A�רA�yrA��MA�@OA� �A���A�.A�~�A���A�33A��XA��=A���A��IAw֡Aq��Ak�Ai��Ai3�AhC�Ae�)Ab�*A_��A^�AZ��AW�AT�ARa�AOXAL  AI�[AHI�AF�ZAE1�AB&�A@�0A>��A<VmA9^�A8�AA8  A7?A6��A6�.A4�FA3��A3b�A2=qA1�A/�|A.4A,B[A+�A*��A*=A)ѷA(�A'��A%kQA$��A#�A#($A"�HA!�PA!($A ,=A�jAZA�A��Ag�A+AW�AA�	A�A�,AoiA�A&�A�HA*�A�A�A=�A-wAw�A��AA�eA�AxlAOvA�Af�Av`A�A�vA�AJ�A�zA~A�HAY�A*�AȴA�.A�rA�'AsA0�A
��A
�zA
�A
��A
�A
	�A	��A	�A	~(A	aA
A
��A4A+kA	H�A�A�+A�)A��A�A'�A�PAAy�A��A�A�2A�A�QA�AE�Az�A�A��AA��AOvA ��A n/@��@�M�@��~@�(�@�q@��@@��u@��f@���@���@�d�@��@�#�@�xl@�@�-w@��@�֡@�Z�@�p�@���@�M�@�*@�H�@���@�}@���@�+�@�i�@��>@�b�@��@�<@��@�ff@�Xy@�F@��c@�`�@�4@��@� i@�r�@�J@�4�@��H@�h�@�m]@��@�Xy@߬q@��@�n�@ݠ�@�v�@�=�@ڶ�@���@�a�@؎�@��@�4@��]@�M@զ�@ԂA@Ӧ�@��@�6�@�S&@���@НI@�7�@ϩ*@��@��U@�1@�Vm@��@̼j@�	@�o�@�&@ʕ�@�;�@Ɏ�@���@�M@ǿH@�"�@��y@Ƈ�@��@�c�@���@ĆY@��z@��@�y>@�	@���@�S&@�C�@�PH@���@�;d@��@��.@���@���@�;@��Y@�7@��@�m]@��M@���@�:*@��W@�&@��f@��'@�l�@�!@��@���@��@���@���@���@�V�@�u@��@���@�c�@�C@���@�bN@��@�K�@��P@��E@���@�m�@�C�@��o@��@�u�@�*0@��@��@�^�@�S@���@�h
@�J�@���@���@�m]@�o@���@�~@���@���@���@�c�@��@���@��@�e�@�/�@���@�E9@��,@��!@�Z�@�e@���@�S@��@�oi@�H@�@���@�b�@�	l@�j@�c@�:�@�;@�ѷ@��x@�c�@�PH@��@��X@�p�@���@��@��A@�;d@��`@���@�Ft@��@���@���@�L�@� i@���@���@�j@���@���@��6@���@�H@��@���@��M@�1�@�(@��@���@��@�($@��j@��$@�o@��.@�6�@��r@��q@�@���@�ߤ@���@��o@�e�@�-@���@���@�E9@�;@�ں@��@��@���@�/�@��@��$@�}V@�_�@��&@���@�x@�+�@�oi@�u%@�L0@��@���@�G�@��@��/@���@�8�@�@���@���@���@�T�@���@���@���@�S�@�4@���@���@��-@�Vm@��@���@�~(@�9X@�J@��m@���@�zx@�zx@�v`@��@��!@��u@�r�@�U2@�*�@�	@���@��@�}�@�iD@�hs@�=@�!-@��@��H@���@���@��b@��@�u�@�H�@�7�@�-�@�
�@��>@��C@�|@�F@��@���@�u%@�I�@�+k@�b@��@��;@���@�B�@��@���@���@�v�@�8�@�{@~��@~GE@~	@}@}?}@|�	@|��@|��@|V�@|7@{�]@{�@{a@{�@z��@zu@ys�@y�@x�$@xtT@w�W@v�2@v1�@v�@v@u�)@u�@t�@ttT@te�@tXy@tPH@tC-@t'R@s�@s�@r� @q��@p��@p:�@o��@o�$@o9�@n�s@nxl@mϫ@mDg@l�@l!@k�@k��@k@j��@j��@jTa@i��@i8�@h~(@g��@g{J@g�@f6�@f	@e@d��@d1@c��@c��@cv`@cE9@b��@bߤ@b�@b�+@bH�@a�>@aN<@`�@`��@`Xy@`>B@_��@_��@_�a@_�0@_�*@_x@^��@^H�@]�@][W@\[�@[��@[o@Z�8@Z��@Z�X@Z�b@Z~�@Z�@Y��@Y-w@X�@X`�@W��@WO@V�A@Vu%@VB[@U��@Uc@Uq@T�@S�@S��@S�f@Sqv@SZ�@SH�@S6z@S.I@S@R��@R��@Rd�@R6�@Q�N@P�@P?�@O��@O��@O�;@O��@O=@N�@N�r@M�Z@M��@MrG@Mq@L��@L]d@L6@LG@K�@K��@Kg�@K6z@J��@J��@Jn�@J($@J�@Iϫ@I&�@H��@H<�@Ga@GS@F�<@F�+@FE�@E�.@E@E|@E�@D��@DQ�@Cخ@C��@Cg�@C/�@C
=@B�@B�R@BC�@A��@A@Ak�@A#�@@�?@@,=@?�A@?��@?ƨ@?t�@>��@>YK@>!�@=�T@=��@=c@=�@<�?@<C-@;8@:��@:v�@:B[@9��@9�3@9��@9 \@8��@8�Y@8�@7&@6�,@6^5@6_@5�)@5o @5�@4��@4��@4h�@4  @3��@3n/@2��@2c @1��@1��@1�@1\�@0�5@0�p@0y>@/��@/W?@/E9@/8@.��@.L0@.�@-��@-o @-�@,�@,V�@,(�@+��@+��@+$t@*��@*n�@*R�@*@)�@)�X@)7L@)@@(��@(�@(U2@(1'@'�r@'��@'�@'�@'dZ@'&@'�@&�,@&��@&��@&Ov@&($@%��@%��@%��@%G�@$��@$��@$Xy@$M@$'R@#��@#�:@#F�@#�@"��@"��@"v�@"Ta@!�.@!�-@!��@!\�@!�@ �E@ �@  �@�Q@��@8@��@�<@��@ff@J�@$�@J@�@��@�@�@�@c�@:�@�@��@˒@��@l�@�@�L@R�@�D@�@zx@e,@<6@-w@@@��@�@e�@N�@��@��@{J@J#@�@�@�,@��@��@v�@a|@8�@�Z@�d@�@��@�h@?}@��@�@��@�4@q@!@�w@��@��@��@~�@K�@.I@!-@�M@�B@s�@@�@�@�z@��@L�@A @#�@ \@�@�@��@��@��@2�@�A@˒@��@O@!-@�H@{�@J�@-@�@��@@�=@m]@B�@	l@��@�Y@g8@7�@@�6@��@X�@F�@�@
��@
��@
��@
�x@
��@
_�@
.�@
�@	�@	�~@	?}@	V@��@�`@��@�.@j@?�@@��@�[@�:@s@E9@�@�L@l�@Z�@Ta@H�@3�@&�@@�@�@�@�S@j@O�@:�@A @�@��@�?@��@j@N�@9X@�@@�@@  @�r@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bd�Bd�Bd�Bd�Bd�Bd�Bc�Bd&Bc�BcBb�BdBl�Bl�Bk�Bl�Bn}Bn/Bn�Bo Bn/Bs�BxB� B�B��B��B�iBیB	7B	��B
7�B
G�B
]B
�OB
�sB
�YB
�aB
��B
��B
چB
�\B
��B
�B
�0B
�B
��B^BB�BGB
�WB
��B
�EB
��B
��B
��B
tB
�B
��B
cTB
4B
!B
ESB
vFB
��B
��B
UMB
[B
�B
3�B
1AB
 B

�B	�B	��B	HB	_�B	C{B	=�B	=<B	:xB	4�B	)�B	%�B	%,B	# B	�B	B	�B	�B	 �B	#�B	 �B	�B	B	�B	B	�B	 �B�B�B��B��BیB��B��B�B��B�B�-B�aB�B��B��B	B	bB	�B	)�B	/5B	2GB	3�B	72B	:�B	A�B	P.B	a�B	ZB	^B	d�B	dtB	cTB	a�B	d&B	v+B	w�B	vFB	w2B	w2B	vB	s�B	o�B	k�B	jB	xB	�pB	��B	�)B	�_B	�MB	�}B	�B	�0B	�{B	��B	�	B	��B	�B	utB	q�B	j�B	m�B	kB	oB	rGB	r�B	tB	t�B	wfB	u�B	tB	t�B	z�B	cB	��B	��B	��B	��B	�.B	�B	�B	�5B	�!B	�yB	�B	ˬB	�(B	�-B	�B	��B	��B	� B	�JB	��B	�}B	�zB	�zB	��B	��B	��B	�tB	�tB	�tB	�&B	�ZB	�:B	��B	�4B	��B	�B	�B	��B	�B	��B	ܒB	��B	��B	�=B	��B	֡B	�B	�&B	��B	�MB	�sB	�SB	ּB	��B	ԕB	ևB	�B	��B	�EB	ܒB	ܬB	�=B	�kB	�EB	�+B	��B	خB	�+B	��B	�yB	��B	�	B	��B	��B	��B	�B	ݘB	��B	�]B	�]B	�CB	�B	�)B	��B	�B	�WB	یB	�qB	��B	�]B	�CB	�CB	�/B	�CB	ۦB	�]B	ܬB	�xB	�B	�IB	�dB	�B	ݘB	�/B	��B	ۦB	�B	ܒB	�]B	�]B	�]B	��B	��B	�IB	��B	�B	�5B	�B	޸B	��B	�OB	ޞB	�!B	��B	�;B	�B	�-B	��B	��B	��B	�-B	�B	�hB	�B	�B	�B	�:B	�B	�LB	�B	�$B	�_B	�B	��B	��B	�XB	�mB	�B	�>B	��B	�B	�yB	�kB	��B	�B	��B	�B	�=B	�B	�]B	��B	��B	��B	�wB	��B	��B	�B	�cB	�B	�OB	�iB	�iB	�B	�B	�B	�B	�AB	�GB	�-B	�GB	��B	��B	��B	�B	�B	�B	�TB	��B	��B	�zB	��B	��B	��B	�B	��B	�	B	�	B	�>B	��B	��B	�B	��B	��B	��B	��B	�B	�jB	�jB	��B	�VB	�qB	�qB	��B	��B	��B	�(B	�.B	��B	��B	��B
 B
 �B
 �B
B
B
-B
GB
�B
B
MB
�B
MB
mB
�B
_B
�B
�B
	�B
	�B
	�B
	B
	7B
	7B
	RB
	�B

XB

�B

�B
�B
�B
B
�B
vB
.B
�B
 B
�B
4B
�B
�B
�B
�B
B
�B
�B
MB
�B
�B
FB
�B
�B
�B
�B
2B
2B
�B
�B
�B
�B
�B
+B
B
1B
KB
�B
B
kB
	B
�B
)B
]B
CB
�B
�B
xB
	B
�B
�B
�B
�B
�B
dB
B
�B
OB
B
OB
�B
VB
�B
 B
!-B
!bB
!bB
!bB
!HB
!HB
!|B
!�B
!�B
!�B
!�B
"�B
#:B
#�B
$ZB
$�B
$�B
%,B
%�B
%�B
&B
&LB
&�B
&�B
'�B
)B
)�B
*B
*eB
+�B
,"B
,�B
-wB
.B
.IB
.cB
.}B
/5B
/�B
0B
0B
0!B
0UB
0oB
0�B
0�B
1'B
1vB
1�B
1�B
1vB
1vB
2|B
2�B
2�B
2�B
2GB
2B
1�B
1vB
1�B
1'B
1�B
1�B
1�B
1�B
1�B
2|B
2�B
2�B
2�B
2�B
2�B
2�B
33B
33B
3�B
4B
4�B
5B
5ZB
5ZB
5ZB
5�B
6�B
7LB
72B
72B
7LB
7�B
8�B
9XB
9XB
9rB
9XB
9XB
9>B
9XB
9�B
9�B
9�B
9�B
:�B
;0B
;0B
;�B
;�B
;�B
<B
<�B
<�B
<�B
=B
<�B
=VB
=VB
=<B
=<B
=�B
=�B
>BB
>�B
>�B
>�B
>�B
>]B
>�B
?.B
?cB
?}B
?�B
?�B
?�B
@B
@iB
@�B
@�B
AUB
A�B
A�B
A�B
A�B
A�B
A�B
A�B
BB
BB
BB
B'B
B'B
BuB
CB
C-B
C�B
D�B
D�B
ESB
EmB
EmB
E�B
E�B
E�B
FB
F%B
FYB
FYB
F�B
GEB
G�B
HfB
HKB
H1B
G�B
HB
H�B
IB
I�B
I�B
J	B
J#B
J#B
J=B
J=B
J=B
J=B
J�B
J�B
J�B
J�B
J�B
LJB
L0B
LJB
LJB
L0B
L0B
LJB
LJB
LdB
L�B
L�B
MB
M�B
M�B
NB
NB
N<B
N<B
NVB
NpB
N�B
N�B
O(B
OBB
OvB
O\B
O�B
P.B
PHB
P�B
QhB
QhB
Q�B
Q�B
Q�B
Q�B
R:B
R:B
R�B
R�B
S&B
S�B
S�B
S�B
T,B
T,B
T,B
TaB
T�B
T�B
T�B
U2B
UMB
U�B
VB
VSB
VSB
V9B
VmB
W$B
WYB
WsB
W�B
W�B
W�B
X_B
Y1B
Y�B
Z�B
[	B
[qB
[�B
[�B
[�B
[�B
\)B
\)B
\B
\�B
]�B
]�B
^B
^5B
^5B
^�B
^�B
_!B
_B
_VB
_�B
_�B
_�B
`vB
`�B
`�B
aB
aB
abB
a�B
a�B
a�B
bNB
b�B
bhB
bhB
b�B
cB
c B
c:B
c�B
c�B
d&B
d@B
d@B
dtB
d�B
eFB
e�B
e�B
e�B
fB
f2B
f2B
ffB
f�B
f�B
ffB
fLB
gB
g�B
g�B
g�B
hXB
h�B
h�B
h�B
h�B
h�B
iB
h�B
h�B
i_B
iyB
iyB
i�B
jKB
j�B
j�B
j�B
j�B
j�B
kB
kkB
k�B
k�B
k�B
k�B
lB
l"B
lWB
lWB
lqB
l�B
l�B
mB
mCB
m]B
m�B
m�B
m�B
nIB
n}B
n�B
n�B
n�B
o B
o B
oOB
o�B
o�B
pB
pUB
pUB
poB
p�B
p�B
p�B
p�B
qAB
qvB
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s3B
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
t�B
t�B
u?B
utB
u�B
u�B
u�B
u�B
vB
v+B
v+B
v+B
v`B
v�B
v�B
w2B
wLB
w2B
w�B
w�B
xB
xB
xB
x8B
xlB
x�B
x�B
x�B
y>B
yrB
y�B
y�B
y�B
zB
zB
zB
z*B
z^B
zxB
z�B
z�B
z�B
{JB
{�B
{�B
{�B
|PB
|jB
|jB
|jB
|�B
|�B
|�B
}"B
}VB
}�B
}�B
~(B
~BB
~]B
~wB
~�B
~�B
B
.B
HB
HB
}B
�B
�B
�B
� B
�4B
�OB
�OB
��B
� B
�UB
�UB
�oB
��B
��B
��B
�B
�AB
�[B
��B
��B
��B
��B
�-B
��B
��B
��B
��B
��B
�B
�B
�3B
�3B
�MB
��B
��B
�B
�B
�9B
�B
�mB
��B
��B
��B
�YB
�YB
�tB
��B
��B
�tB
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bd�Bd�Bd�Bd�Bd�Bd�Bc�Bd&Bc�BcBb�BdBl�Bl�Bk�Bl�Bn}Bn/Bn�Bo Bn/Bs�BxB� B�B��B��B�iBیB	7B	��B
7�B
G�B
]B
�OB
�sB
�YB
�aB
��B
��B
چB
�\B
��B
�B
�0B
�B
��B^BB�BGB
�WB
��B
�EB
��B
��B
��B
tB
�B
��B
cTB
4B
!B
ESB
vFB
��B
��B
UMB
[B
�B
3�B
1AB
 B

�B	�B	��B	HB	_�B	C{B	=�B	=<B	:xB	4�B	)�B	%�B	%,B	# B	�B	B	�B	�B	 �B	#�B	 �B	�B	B	�B	B	�B	 �B�B�B��B��BیB��B��B�B��B�B�-B�aB�B��B��B	B	bB	�B	)�B	/5B	2GB	3�B	72B	:�B	A�B	P.B	a�B	ZB	^B	d�B	dtB	cTB	a�B	d&B	v+B	w�B	vFB	w2B	w2B	vB	s�B	o�B	k�B	jB	xB	�pB	��B	�)B	�_B	�MB	�}B	�B	�0B	�{B	��B	�	B	��B	�B	utB	q�B	j�B	m�B	kB	oB	rGB	r�B	tB	t�B	wfB	u�B	tB	t�B	z�B	cB	��B	��B	��B	��B	�.B	�B	�B	�5B	�!B	�yB	�B	ˬB	�(B	�-B	�B	��B	��B	� B	�JB	��B	�}B	�zB	�zB	��B	��B	��B	�tB	�tB	�tB	�&B	�ZB	�:B	��B	�4B	��B	�B	�B	��B	�B	��B	ܒB	��B	��B	�=B	��B	֡B	�B	�&B	��B	�MB	�sB	�SB	ּB	��B	ԕB	ևB	�B	��B	�EB	ܒB	ܬB	�=B	�kB	�EB	�+B	��B	خB	�+B	��B	�yB	��B	�	B	��B	��B	��B	�B	ݘB	��B	�]B	�]B	�CB	�B	�)B	��B	�B	�WB	یB	�qB	��B	�]B	�CB	�CB	�/B	�CB	ۦB	�]B	ܬB	�xB	�B	�IB	�dB	�B	ݘB	�/B	��B	ۦB	�B	ܒB	�]B	�]B	�]B	��B	��B	�IB	��B	�B	�5B	�B	޸B	��B	�OB	ޞB	�!B	��B	�;B	�B	�-B	��B	��B	��B	�-B	�B	�hB	�B	�B	�B	�:B	�B	�LB	�B	�$B	�_B	�B	��B	��B	�XB	�mB	�B	�>B	��B	�B	�yB	�kB	��B	�B	��B	�B	�=B	�B	�]B	��B	��B	��B	�wB	��B	��B	�B	�cB	�B	�OB	�iB	�iB	�B	�B	�B	�B	�AB	�GB	�-B	�GB	��B	��B	��B	�B	�B	�B	�TB	��B	��B	�zB	��B	��B	��B	�B	��B	�	B	�	B	�>B	��B	��B	�B	��B	��B	��B	��B	�B	�jB	�jB	��B	�VB	�qB	�qB	��B	��B	��B	�(B	�.B	��B	��B	��B
 B
 �B
 �B
B
B
-B
GB
�B
B
MB
�B
MB
mB
�B
_B
�B
�B
	�B
	�B
	�B
	B
	7B
	7B
	RB
	�B

XB

�B

�B
�B
�B
B
�B
vB
.B
�B
 B
�B
4B
�B
�B
�B
�B
B
�B
�B
MB
�B
�B
FB
�B
�B
�B
�B
2B
2B
�B
�B
�B
�B
�B
+B
B
1B
KB
�B
B
kB
	B
�B
)B
]B
CB
�B
�B
xB
	B
�B
�B
�B
�B
�B
dB
B
�B
OB
B
OB
�B
VB
�B
 B
!-B
!bB
!bB
!bB
!HB
!HB
!|B
!�B
!�B
!�B
!�B
"�B
#:B
#�B
$ZB
$�B
$�B
%,B
%�B
%�B
&B
&LB
&�B
&�B
'�B
)B
)�B
*B
*eB
+�B
,"B
,�B
-wB
.B
.IB
.cB
.}B
/5B
/�B
0B
0B
0!B
0UB
0oB
0�B
0�B
1'B
1vB
1�B
1�B
1vB
1vB
2|B
2�B
2�B
2�B
2GB
2B
1�B
1vB
1�B
1'B
1�B
1�B
1�B
1�B
1�B
2|B
2�B
2�B
2�B
2�B
2�B
2�B
33B
33B
3�B
4B
4�B
5B
5ZB
5ZB
5ZB
5�B
6�B
7LB
72B
72B
7LB
7�B
8�B
9XB
9XB
9rB
9XB
9XB
9>B
9XB
9�B
9�B
9�B
9�B
:�B
;0B
;0B
;�B
;�B
;�B
<B
<�B
<�B
<�B
=B
<�B
=VB
=VB
=<B
=<B
=�B
=�B
>BB
>�B
>�B
>�B
>�B
>]B
>�B
?.B
?cB
?}B
?�B
?�B
?�B
@B
@iB
@�B
@�B
AUB
A�B
A�B
A�B
A�B
A�B
A�B
A�B
BB
BB
BB
B'B
B'B
BuB
CB
C-B
C�B
D�B
D�B
ESB
EmB
EmB
E�B
E�B
E�B
FB
F%B
FYB
FYB
F�B
GEB
G�B
HfB
HKB
H1B
G�B
HB
H�B
IB
I�B
I�B
J	B
J#B
J#B
J=B
J=B
J=B
J=B
J�B
J�B
J�B
J�B
J�B
LJB
L0B
LJB
LJB
L0B
L0B
LJB
LJB
LdB
L�B
L�B
MB
M�B
M�B
NB
NB
N<B
N<B
NVB
NpB
N�B
N�B
O(B
OBB
OvB
O\B
O�B
P.B
PHB
P�B
QhB
QhB
Q�B
Q�B
Q�B
Q�B
R:B
R:B
R�B
R�B
S&B
S�B
S�B
S�B
T,B
T,B
T,B
TaB
T�B
T�B
T�B
U2B
UMB
U�B
VB
VSB
VSB
V9B
VmB
W$B
WYB
WsB
W�B
W�B
W�B
X_B
Y1B
Y�B
Z�B
[	B
[qB
[�B
[�B
[�B
[�B
\)B
\)B
\B
\�B
]�B
]�B
^B
^5B
^5B
^�B
^�B
_!B
_B
_VB
_�B
_�B
_�B
`vB
`�B
`�B
aB
aB
abB
a�B
a�B
a�B
bNB
b�B
bhB
bhB
b�B
cB
c B
c:B
c�B
c�B
d&B
d@B
d@B
dtB
d�B
eFB
e�B
e�B
e�B
fB
f2B
f2B
ffB
f�B
f�B
ffB
fLB
gB
g�B
g�B
g�B
hXB
h�B
h�B
h�B
h�B
h�B
iB
h�B
h�B
i_B
iyB
iyB
i�B
jKB
j�B
j�B
j�B
j�B
j�B
kB
kkB
k�B
k�B
k�B
k�B
lB
l"B
lWB
lWB
lqB
l�B
l�B
mB
mCB
m]B
m�B
m�B
m�B
nIB
n}B
n�B
n�B
n�B
o B
o B
oOB
o�B
o�B
pB
pUB
pUB
poB
p�B
p�B
p�B
p�B
qAB
qvB
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s3B
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
t�B
t�B
u?B
utB
u�B
u�B
u�B
u�B
vB
v+B
v+B
v+B
v`B
v�B
v�B
w2B
wLB
w2B
w�B
w�B
xB
xB
xB
x8B
xlB
x�B
x�B
x�B
y>B
yrB
y�B
y�B
y�B
zB
zB
zB
z*B
z^B
zxB
z�B
z�B
z�B
{JB
{�B
{�B
{�B
|PB
|jB
|jB
|jB
|�B
|�B
|�B
}"B
}VB
}�B
}�B
~(B
~BB
~]B
~wB
~�B
~�B
B
.B
HB
HB
}B
�B
�B
�B
� B
�4B
�OB
�OB
��B
� B
�UB
�UB
�oB
��B
��B
��B
�B
�AB
�[B
��B
��B
��B
��B
�-B
��B
��B
��B
��B
��B
�B
�B
�3B
�3B
�MB
��B
��B
�B
�B
�9B
�B
�mB
��B
��B
��B
�YB
�YB
�tB
��B
��B
�tB
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104947  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175014  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175014  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175014                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025022  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025022  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                