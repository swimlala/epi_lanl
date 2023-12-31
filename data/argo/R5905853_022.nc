CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:26:31Z creation;2022-06-04T17:26:31Z conversion to V3.1      
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pD   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t0   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �(   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �0   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �8   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �x   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604172631  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�ی\)1   @�ۍ"R��@0D�t�j�ca�7Kƨ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A^ffA�  A�  A���A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBy��B�  B�  B�  B�  B���B�  B�  B�  B�33B�  B�33B�  B���B�  B�  B�  B�  B�33B�  B���B���B�33Bי�B�  B�  B�  B�  B�  B���B�  B���B�  C �C33C��C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.�C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��3C�  C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @\)@u@��H@��HAp�A=p�A[�
A}p�A��RA��A��A��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)BoBx��B\)B��B��B��B�z�B��B��B��B��GB��B��GB��B�z�B��B��B��B��B��GBǮB�z�B�z�B��GB�G�BۮB߮B�B�B�B�z�B�B�z�B��B��HC
=C��C�
C�
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
C!�
C#�C%�
C'�
C)�
C+�
C-�C/�pC1�
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
Cq�Cs�Cu�
Cw�
Cy�
C{�
C}�
C�
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C��C��RC��RC��C�޸C��C�޸D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4|)D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ�)D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�7�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D�D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�'�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�gmA�qAA�r|A�qAA�q�A�t�A�v�A�yrA�zA�|�A�|�A�w�A�zA�tTA�tTA�lWA�l�A�d�A�d�A�d&A�QNA�H�A�*�A��Aމ7A�~�A�o�A��|A�:A��PAջ0AԍA�0�A�]�A�2aA��A�m�A̺^A�}�A��A�W
A�#AŦA��jA��A�EA��A�� A�`vA�s�A��xA�oiA�iDA��/A�\A���A�A��A��$A���A��A���A�`vA�"4A���A�l�A�s�A��A�"�A���A�CaA�B[A�YKA�%zA��wA~TaA{~(AyȴAy	Aw%�AraAmS�AiN<Ag�uAcFtA_?AZ�_AU�pAT�>AR��AO�fALȴAK��AJ;dAGxlAEe,ABhsA?W?A=��A:�A7�TA6N�A6A3��A2یA1�A0L�A.�A.�A-O�A-�A,��A,�A+($A*K�A*�A)�ZA)��A)N�A(�A(��A&��A$�hA"��A"a�A"�QA#^�A#�MA$=A$p;A$��A%A#VmA �mA�oA��An�Ag�AGA bA��A/�A�A�/Aa|AqA)_A|AJA��A��A��A�XA��A��AQ�A  A�gA{JA1A�A�A1A�.A�A�A��A�rA��A�A��A�nA��AiDAjA�<AkQA� A�A�DA��A��A��A"hA��A
�MA	��A	��A	�QA	�A	��A	b�A�8A+kA�wAe�A�yAzxA	lA�$A�A��AxlA/�A�	A�0A�MA3�A�9A8�A �A y�A 7@�qv@��K@��z@�@��6@�c�@�1'@���@��Y@�-�@��@�u�@���@��#@���@�;d@�~�@��o@�Ĝ@�	@��a@�>�@��@�_�@�;�@��@�ݘ@��@��@�=@�h
@﫟@��@��@�(�@�+@��@�zx@�5�@���@���@�"�@��@�~@�hs@�!-@�,�@涮@��@�u�@���@�Dg@�tT@�N�@�bN@��@�a@��p@߆�@�L�@��M@ދD@���@ݼ�@��@ܣ@�M�@��>@۰�@�Y@ڴ9@�0U@��9@�O�@� i@���@�V@�A�@֚�@ք�@֨�@֋D@��@՗�@��H@�5?@ҹ�@�4@��T@���@с�@��@й$@�l�@�V�@�	@��'@�U2@���@͌~@�=@�e�@���@˟V@�a�@�?}@�7L@�.I@��@��@ɷ@�^�@�6z@�|�@Ǡ�@�w2@�qv@���@��
@�2a@ĩ�@��@Ê�@��@�d�@�Ov@�7@���@���@��K@�{�@��@�j@�X�@�C�@���@���@��@�ԕ@�4�@��@���@���@�J�@��N@�{J@���@� �@��@��n@��k@�b�@�+@��X@�bN@�@��@�X�@���@�:*@��m@���@���@��W@��"@�&�@���@�V@�?@��@�l"@� �@�f�@�@�ѷ@��h@��9@���@�V@�%�@��@���@���@��F@��@��3@��@���@�e,@�A @�!-@���@���@�S�@�1@���@�[W@�=@�"�@�	l@��8@��m@�($@�ԕ@���@��@�J�@��@���@��A@�B[@�ϫ@�e,@�4�@��@��@��B@���@��D@�Z@��&@���@��9@�s@�@�z@��@���@��[@�T�@��@��@�A�@��@�@�zx@�6z@�&@���@��L@���@���@���@�B�@���@��A@�z@�w�@�oi@�Z@�8�@� �@��@���@��-@�s@�&@���@���@��E@��'@��O@��@�M@���@���@��@���@�"�@�n�@�%�@��@�{@�@��g@���@��@��$@��h@�$�@���@�p�@�P�@��@�i�@�<�@��@�@�D�@��d@���@���@���@�Mj@�ߤ@���@�]d@���@��[@���@���@���@�Q�@�-�@�2�@��@���@��@��@�J#@���@���@���@�oi@�e�@�Q�@�/�@��@��z@���@�X�@��@��s@���@���@�M�@�+k@��@���@��6@�@���@�RT@�0�@��@��v@�H�@�'R@�*�@�7@���@�zx@�C�@�/@�/�@��/@���@�Z@�/�@�@���@���@���@���@�o�@�4�@��@��8@��@�2�@�j@�)_@�+@���@��!@�l"@�ݘ@��P@�N<@�(�@��8@�ȴ@��.@�Z@�$�@��@�V@U�@$t@~�2@~�!@~l�@}o @}2a@}*0@}+@}�@}�@|�e@{��@{P�@{�@zB[@y��@yhs@y=�@y+@y�@x��@x�@x֡@x�)@x�9@xr�@xM@x$@x�@x�@w��@w��@v��@u�9@u\�@u;@t�@t�z@t2�@s��@s~�@s;d@r�@r��@rff@r0U@q�@qA @q�@p@oC�@o+@o@n��@m��@m�@m-w@l�u@l�@kRT@j��@j1�@ic@h�K@hی@h�$@h~(@hx@gj�@gC@g�@f��@fW�@f+k@e��@e \@d��@d_@dM@c��@c{J@c!-@b��@b@�@be@a�t@`�I@`:�@`@_�@_��@_�$@_�@_+@^Ov@]�.@]��@]�@\��@[�
@[�@[�@Z�x@Zs�@Zn�@ZJ�@Y�D@Yzx@X��@XD�@X,=@W�w@W~�@Wj�@W@O@W"�@V�s@Vv�@V6�@U/@T�$@Tz�@Tj@Th�@Th�@Th�@Tj@Tc�@TbN@TK^@T'R@S�g@S��@S>�@S'�@R�@Rz@Q�@Q�M@Qa�@QG�@Q%F@P�5@P|�@PXy@P"h@O��@NTa@M�j@M�@M��@MN<@M�@M�@L��@L֡@L�$@L�_@Lw�@K�Q@K��@K{J@K"�@J͟@J��@I��@I/@H�@H��@H��@HM@HM@H�@G��@G��@G�w@G��@F�"@FC�@E-w@D,=@C�@C��@C{J@CK�@C@Bd�@B�@A�@@�9@?�Q@?��@?�{@?a@?9�@?
=@>�M@>ߤ@>��@>�b@>E�@>-@>O@>e@>�@>e@>	@=�@=��@=|@<�@<r�@<Ft@;��@;ݘ@;خ@;y�@:xl@9�z@9��@9m]@9?}@9�@8��@7��@6��@6s�@6:*@64@5��@5�'@5 \@4�E@4�)@4�$@4�@4y>@4S�@3��@3ƨ@3��@3��@3��@3��@3O@2�@2��@2YK@2)�@2($@2�@2
�@2u@1��@1?}@1+@1�@0��@0[�@0Q�@/��@/33@.��@.��@.Q@.{@.4@.
�@-�@-��@-��@-rG@-Q�@-&�@,�f@,��@,j@+�@+g�@+!-@*��@*?@*�@)�9@)��@)c�@)=�@)@(��@(�Y@(tT@(Xy@(Ft@'�@'n/@'C�@&��@&��@&�\@&}V@&u%@&kQ@&E�@&_@%��@%�n@%��@%�@%}�@%}�@%f�@%@$�)@$�@$%�@#�@#��@#�k@#�	@#|�@#)_@"�2@"��@"��@"��@"5?@"@!��@!@ ��@ ѷ@ �o@ M@�;@�@��@�	@�X@�b@��@xl@v�@i�@W�@M�@:*@�-@\�@8�@/@�@��@�9@|�@h�@S�@?�@�@خ@��@�	@j�@P�@+@@͟@�F@1�@J@��@�^@m]@<6@�@�@�@oi@D�@ �@�;@˒@��@��@a@Mj@O@33@�y@�6@� @~�@3�@�@�=@`B@%F@��@�e@��@~(@l"@1'@�@�6@�w@�F@�q@�	@�B@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�gmA�qAA�r|A�qAA�q�A�t�A�v�A�yrA�zA�|�A�|�A�w�A�zA�tTA�tTA�lWA�l�A�d�A�d�A�d&A�QNA�H�A�*�A��Aމ7A�~�A�o�A��|A�:A��PAջ0AԍA�0�A�]�A�2aA��A�m�A̺^A�}�A��A�W
A�#AŦA��jA��A�EA��A�� A�`vA�s�A��xA�oiA�iDA��/A�\A���A�A��A��$A���A��A���A�`vA�"4A���A�l�A�s�A��A�"�A���A�CaA�B[A�YKA�%zA��wA~TaA{~(AyȴAy	Aw%�AraAmS�AiN<Ag�uAcFtA_?AZ�_AU�pAT�>AR��AO�fALȴAK��AJ;dAGxlAEe,ABhsA?W?A=��A:�A7�TA6N�A6A3��A2یA1�A0L�A.�A.�A-O�A-�A,��A,�A+($A*K�A*�A)�ZA)��A)N�A(�A(��A&��A$�hA"��A"a�A"�QA#^�A#�MA$=A$p;A$��A%A#VmA �mA�oA��An�Ag�AGA bA��A/�A�A�/Aa|AqA)_A|AJA��A��A��A�XA��A��AQ�A  A�gA{JA1A�A�A1A�.A�A�A��A�rA��A�A��A�nA��AiDAjA�<AkQA� A�A�DA��A��A��A"hA��A
�MA	��A	��A	�QA	�A	��A	b�A�8A+kA�wAe�A�yAzxA	lA�$A�A��AxlA/�A�	A�0A�MA3�A�9A8�A �A y�A 7@�qv@��K@��z@�@��6@�c�@�1'@���@��Y@�-�@��@�u�@���@��#@���@�;d@�~�@��o@�Ĝ@�	@��a@�>�@��@�_�@�;�@��@�ݘ@��@��@�=@�h
@﫟@��@��@�(�@�+@��@�zx@�5�@���@���@�"�@��@�~@�hs@�!-@�,�@涮@��@�u�@���@�Dg@�tT@�N�@�bN@��@�a@��p@߆�@�L�@��M@ދD@���@ݼ�@��@ܣ@�M�@��>@۰�@�Y@ڴ9@�0U@��9@�O�@� i@���@�V@�A�@֚�@ք�@֨�@֋D@��@՗�@��H@�5?@ҹ�@�4@��T@���@с�@��@й$@�l�@�V�@�	@��'@�U2@���@͌~@�=@�e�@���@˟V@�a�@�?}@�7L@�.I@��@��@ɷ@�^�@�6z@�|�@Ǡ�@�w2@�qv@���@��
@�2a@ĩ�@��@Ê�@��@�d�@�Ov@�7@���@���@��K@�{�@��@�j@�X�@�C�@���@���@��@�ԕ@�4�@��@���@���@�J�@��N@�{J@���@� �@��@��n@��k@�b�@�+@��X@�bN@�@��@�X�@���@�:*@��m@���@���@��W@��"@�&�@���@�V@�?@��@�l"@� �@�f�@�@�ѷ@��h@��9@���@�V@�%�@��@���@���@��F@��@��3@��@���@�e,@�A @�!-@���@���@�S�@�1@���@�[W@�=@�"�@�	l@��8@��m@�($@�ԕ@���@��@�J�@��@���@��A@�B[@�ϫ@�e,@�4�@��@��@��B@���@��D@�Z@��&@���@��9@�s@�@�z@��@���@��[@�T�@��@��@�A�@��@�@�zx@�6z@�&@���@��L@���@���@���@�B�@���@��A@�z@�w�@�oi@�Z@�8�@� �@��@���@��-@�s@�&@���@���@��E@��'@��O@��@�M@���@���@��@���@�"�@�n�@�%�@��@�{@�@��g@���@��@��$@��h@�$�@���@�p�@�P�@��@�i�@�<�@��@�@�D�@��d@���@���@���@�Mj@�ߤ@���@�]d@���@��[@���@���@���@�Q�@�-�@�2�@��@���@��@��@�J#@���@���@���@�oi@�e�@�Q�@�/�@��@��z@���@�X�@��@��s@���@���@�M�@�+k@��@���@��6@�@���@�RT@�0�@��@��v@�H�@�'R@�*�@�7@���@�zx@�C�@�/@�/�@��/@���@�Z@�/�@�@���@���@���@���@�o�@�4�@��@��8@��@�2�@�j@�)_@�+@���@��!@�l"@�ݘ@��P@�N<@�(�@��8@�ȴ@��.@�Z@�$�@��@�V@U�@$t@~�2@~�!@~l�@}o @}2a@}*0@}+@}�@}�@|�e@{��@{P�@{�@zB[@y��@yhs@y=�@y+@y�@x��@x�@x֡@x�)@x�9@xr�@xM@x$@x�@x�@w��@w��@v��@u�9@u\�@u;@t�@t�z@t2�@s��@s~�@s;d@r�@r��@rff@r0U@q�@qA @q�@p@oC�@o+@o@n��@m��@m�@m-w@l�u@l�@kRT@j��@j1�@ic@h�K@hی@h�$@h~(@hx@gj�@gC@g�@f��@fW�@f+k@e��@e \@d��@d_@dM@c��@c{J@c!-@b��@b@�@be@a�t@`�I@`:�@`@_�@_��@_�$@_�@_+@^Ov@]�.@]��@]�@\��@[�
@[�@[�@Z�x@Zs�@Zn�@ZJ�@Y�D@Yzx@X��@XD�@X,=@W�w@W~�@Wj�@W@O@W"�@V�s@Vv�@V6�@U/@T�$@Tz�@Tj@Th�@Th�@Th�@Tj@Tc�@TbN@TK^@T'R@S�g@S��@S>�@S'�@R�@Rz@Q�@Q�M@Qa�@QG�@Q%F@P�5@P|�@PXy@P"h@O��@NTa@M�j@M�@M��@MN<@M�@M�@L��@L֡@L�$@L�_@Lw�@K�Q@K��@K{J@K"�@J͟@J��@I��@I/@H�@H��@H��@HM@HM@H�@G��@G��@G�w@G��@F�"@FC�@E-w@D,=@C�@C��@C{J@CK�@C@Bd�@B�@A�@@�9@?�Q@?��@?�{@?a@?9�@?
=@>�M@>ߤ@>��@>�b@>E�@>-@>O@>e@>�@>e@>	@=�@=��@=|@<�@<r�@<Ft@;��@;ݘ@;خ@;y�@:xl@9�z@9��@9m]@9?}@9�@8��@7��@6��@6s�@6:*@64@5��@5�'@5 \@4�E@4�)@4�$@4�@4y>@4S�@3��@3ƨ@3��@3��@3��@3��@3O@2�@2��@2YK@2)�@2($@2�@2
�@2u@1��@1?}@1+@1�@0��@0[�@0Q�@/��@/33@.��@.��@.Q@.{@.4@.
�@-�@-��@-��@-rG@-Q�@-&�@,�f@,��@,j@+�@+g�@+!-@*��@*?@*�@)�9@)��@)c�@)=�@)@(��@(�Y@(tT@(Xy@(Ft@'�@'n/@'C�@&��@&��@&�\@&}V@&u%@&kQ@&E�@&_@%��@%�n@%��@%�@%}�@%}�@%f�@%@$�)@$�@$%�@#�@#��@#�k@#�	@#|�@#)_@"�2@"��@"��@"��@"5?@"@!��@!@ ��@ ѷ@ �o@ M@�;@�@��@�	@�X@�b@��@xl@v�@i�@W�@M�@:*@�-@\�@8�@/@�@��@�9@|�@h�@S�@?�@�@خ@��@�	@j�@P�@+@@͟@�F@1�@J@��@�^@m]@<6@�@�@�@oi@D�@ �@�;@˒@��@��@a@Mj@O@33@�y@�6@� @~�@3�@�@�=@`B@%F@��@�e@��@~(@l"@1'@�@�6@�w@�F@�q@�	@�B@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�0B	��B	�B	�B	��B	�0B	�B	��B	�B	��B	�KB	��B	��B	�)B	�]B	�}B	�cB	�B	�;B	�UB	��B	��B	��B	��B	�'B	��B	�/B	��B	�xB	r�B	E�B	@iB	QNB	V�B	T�B	T,B	c�B	k�B	b�B	|PB	��B	��B	��B	֡B
�B
1[B
7B
;JB
?.B
A�B
IB
S�B
X�B
iDB
`'B
F�B
�B	��B	�NB	�UB	�HB	�TB
�B
�B
�B
0UB
A�B
DB
/�B
�B	�B	��B	�hB	��B	��B	ǮB	��B	�WB	��B	��B	��B	p�B	W�B	MB	6�B	�B�cB�*B�B��B�HB��B�B�<B�9B��B��B��B�0B��B�QB��B��B�5B�=B��B�vB��B�HB�tB�PBѷB��B�B�zB�}B	tB	�B	!B	2�B	6B	-�B	�B	�B	�B	#�B	A�B	d�B	x�B	�YB	��B	�2B	�9B	�mB	��B	��B	��B	��B	�'B	�>B	�UB	��B	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�xB	�JB	�jB	�"B	��B	�B	��B	�VB	� B	��B	��B	�qB	ۦB	޸B	�B	��B	�B	�_B	��B	��B	οB	�	B	��B	�lB	�vB	��B	��B	֡B	��B	�KB	�_B	�yB	�yB	�EB	ؓB	�_B	��B	�=B	ܬB	�#B	خB	�B	�QB	�QB	چB	��B	��B	�B	�_B	�?B	�B	�,B	�{B	ևB	�QB	�7B	�B	֡B	�YB	��B	�$B	ՁB	�2B	�
B	רB	�sB	چB	ڠB	�WB	��B	�OB	��B	ݲB	�;B	�B	�B	�FB	�B	�B	�B	�LB	�mB	�yB	��B	�*B	�B	�B	��B	��B	�B	�aB	�B	��B	�B	��B	�OB	�B	�B	��B	�B	�"B	�B	�B	�_B	�XB	�*B	�B	�B	�B	��B	�B	�B	��B	�8B	��B	��B	��B	�:B	��B	�B	�TB	�&B	��B	�,B	��B	��B	�B	��B	�B	�XB	�B	� B	�B	�B	�oB	�B	�B	�)B	�)B	�iB	�'B	�B	��B	�5B	��B	��B	�B	�B	��B	�"B	�B	�B	�B	��B	�-B	�B	�iB	��B	�B	�;B	�B	��B	�B	�B	�-B	�-B	��B	��B	�aB	�B	�?B	��B	�lB	�8B	��B	�2B	��B	�`B	�+B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�>B	�>B	��B	�DB	�xB	�^B	��B	��B	��B	��B	�JB	��B	�0B	��B	��B	�B	�xB	�>B	�8B	�XB	��B	��B	��B	�$B	��B	�rB	�XB	��B	��B	�0B	�JB	��B	��B	��B	��B	��B	�B	��B	��B	�<B	��B
 �B
�B
�B
B
 �B
  B
  B	��B	�HB	��B	��B	��B
 �B	�HB	�cB
 4B
 �B
�B
'B
�B
�B
B
�B
�B
�B
�B
�B
�B
mB
tB
	�B
	�B

	B

�B

�B

�B

�B

�B

�B

�B

�B
DB
^B
DB
xB
xB
�B
�B
0B
0B
B
�B

#B
	�B

�B
�B
dB
dB
dB
�B
�B
~B
dB
0B
dB
�B
~B
6B
�B
\B
�B
bB
TB
B
{B
B
SB
�B
�B
?B
�B
�B
YB
sB
�B
B
�B
B
B
KB
�B
�B
B
#B
#B
#B
#B
�B
�B
�B
�B
�B
�B
�B
/B
IB
�B
�B
dB
�B
�B
OB
jB
�B
�B
VB
 'B
 �B
!HB
"4B
"�B
#B
#nB
#B
#nB
$B
$�B
$�B
$ZB
$&B
$&B
$�B
%�B
%�B
%�B
&LB
&�B
&�B
&�B
'B
'�B
'�B
'�B
(>B
(>B
(sB
(�B
)B
)_B
)_B
)�B
*B
*eB
*KB
*KB
+6B
+�B
+�B
,=B
,WB
,�B
,�B
-]B
-wB
-�B
-]B
,�B
-)B
-�B
./B
.�B
-�B
-wB
-wB
.B
.}B
-�B
-wB
-CB
,�B
,�B
,�B
,�B
,�B
,�B
-B
,�B
-�B
-�B
-�B
.B
-�B
/B
.�B
-�B
-)B
-]B
-]B
-�B
-�B
-�B
.B
.IB
.�B
.�B
/5B
/OB
/�B
/�B
/�B
/�B
/�B
1B
1B
1'B
1AB
1B
0�B
1[B
2�B
3MB
3�B
4�B
5B
5%B
5?B
5ZB
5ZB
5ZB
5tB
5ZB
5?B
5?B
5ZB
5�B
5tB
5ZB
5?B
5�B
5ZB
6�B
7LB
7�B
88B
8lB
8�B
9>B
9�B
9�B
:DB
:�B
:�B
;B
;JB
;�B
<jB
<�B
=<B
=VB
=qB
=VB
=�B
>B
>(B
>BB
>�B
>�B
?cB
?�B
@ B
@iB
@�B
@�B
@�B
@�B
A B
AoB
A�B
A�B
A�B
A�B
BB
BAB
B�B
B�B
B�B
B�B
C-B
B�B
C-B
C{B
C{B
CGB
C�B
C�B
C�B
C�B
C�B
DB
DB
C�B
D�B
D�B
D�B
EB
D�B
E9B
E�B
E�B
FB
FtB
FYB
FYB
FYB
F�B
F�B
HB
H1B
HfB
H�B
H�B
H�B
IB
IB
IlB
I�B
I�B
JXB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
KB
KB
K^B
K^B
K�B
K�B
K�B
LB
LB
L0B
LJB
LdB
L�B
L�B
L�B
L�B
N�B
N�B
N�B
N�B
OvB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q B
Q B
P�B
Q�B
R:B
R�B
R�B
R�B
S[B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T,B
U2B
U�B
UgB
U2B
UMB
UMB
UMB
UB
T�B
T�B
UgB
U�B
U�B
U�B
U�B
VSB
V�B
VmB
VmB
V�B
VmB
V�B
V�B
W$B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
Y�B
ZB
Z�B
ZkB
ZB
ZkB
[�B
\�B
\�B
\�B
\�B
\�B
\�B
^B
^�B
^jB
^�B
^�B
_B
_B
_�B
_�B
_�B
_�B
_�B
_�B
`B
`\B
`�B
`\B
`vB
`\B
`�B
`�B
aB
aHB
aHB
abB
abB
a|B
abB
aHB
a�B
a�B
a�B
a�B
b4B
bhB
bNB
b�B
cB
c:B
cTB
c�B
c�B
c�B
c�B
c�B
dB
d&B
d@B
d@B
dZB
d�B
d�B
d�B
e,B
e�B
e�B
e�B
fLB
ffB
f�B
f�B
f�B
gB
gB
gRB
g�B
g�B
g�B
gRB
g�B
hXB
h>B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
iDB
iyB
iyB
i�B
i�B
i�B
i_B
i�B
i�B
i�B
j0B
j�B
j�B
j�B
j�B
kB
j�B
kQB
k�B
k�B
k�B
k�B
l"B
l"B
l�B
l�B
mB
mB
m]B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o B
n�B
o B
o�B
o�B
o�B
o�B
o�B
pB
p!B
poB
poB
p�B
p�B
p�B
p�B
q'B
qB
qAB
q[B
qvB
qvB
q�B
q�B
r-B
r-B
r�B
r|B
r�B
r�B
sB
sMB
s�B
s�B
s�B
s�B
tB
t9B
t9B
tnB
t�B
t�B
t�B
t�B
uB
u%B
u?B
u?B
u�B
u�B
vB
vFB
vzB
v�B
v�B
v�B
v�B
v�B
w2B
wfB
w�B
w�B
w�B
w�B
w�B
x�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�0B	��B	�B	�B	��B	�0B	�B	��B	�B	��B	�KB	��B	��B	�)B	�]B	�}B	�cB	�B	�;B	�UB	��B	��B	��B	��B	�'B	��B	�/B	��B	�xB	r�B	E�B	@iB	QNB	V�B	T�B	T,B	c�B	k�B	b�B	|PB	��B	��B	��B	֡B
�B
1[B
7B
;JB
?.B
A�B
IB
S�B
X�B
iDB
`'B
F�B
�B	��B	�NB	�UB	�HB	�TB
�B
�B
�B
0UB
A�B
DB
/�B
�B	�B	��B	�hB	��B	��B	ǮB	��B	�WB	��B	��B	��B	p�B	W�B	MB	6�B	�B�cB�*B�B��B�HB��B�B�<B�9B��B��B��B�0B��B�QB��B��B�5B�=B��B�vB��B�HB�tB�PBѷB��B�B�zB�}B	tB	�B	!B	2�B	6B	-�B	�B	�B	�B	#�B	A�B	d�B	x�B	�YB	��B	�2B	�9B	�mB	��B	��B	��B	��B	�'B	�>B	�UB	��B	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�xB	�JB	�jB	�"B	��B	�B	��B	�VB	� B	��B	��B	�qB	ۦB	޸B	�B	��B	�B	�_B	��B	��B	οB	�	B	��B	�lB	�vB	��B	��B	֡B	��B	�KB	�_B	�yB	�yB	�EB	ؓB	�_B	��B	�=B	ܬB	�#B	خB	�B	�QB	�QB	چB	��B	��B	�B	�_B	�?B	�B	�,B	�{B	ևB	�QB	�7B	�B	֡B	�YB	��B	�$B	ՁB	�2B	�
B	רB	�sB	چB	ڠB	�WB	��B	�OB	��B	ݲB	�;B	�B	�B	�FB	�B	�B	�B	�LB	�mB	�yB	��B	�*B	�B	�B	��B	��B	�B	�aB	�B	��B	�B	��B	�OB	�B	�B	��B	�B	�"B	�B	�B	�_B	�XB	�*B	�B	�B	�B	��B	�B	�B	��B	�8B	��B	��B	��B	�:B	��B	�B	�TB	�&B	��B	�,B	��B	��B	�B	��B	�B	�XB	�B	� B	�B	�B	�oB	�B	�B	�)B	�)B	�iB	�'B	�B	��B	�5B	��B	��B	�B	�B	��B	�"B	�B	�B	�B	��B	�-B	�B	�iB	��B	�B	�;B	�B	��B	�B	�B	�-B	�-B	��B	��B	�aB	�B	�?B	��B	�lB	�8B	��B	�2B	��B	�`B	�+B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�>B	�>B	��B	�DB	�xB	�^B	��B	��B	��B	��B	�JB	��B	�0B	��B	��B	�B	�xB	�>B	�8B	�XB	��B	��B	��B	�$B	��B	�rB	�XB	��B	��B	�0B	�JB	��B	��B	��B	��B	��B	�B	��B	��B	�<B	��B
 �B
�B
�B
B
 �B
  B
  B	��B	�HB	��B	��B	��B
 �B	�HB	�cB
 4B
 �B
�B
'B
�B
�B
B
�B
�B
�B
�B
�B
�B
mB
tB
	�B
	�B

	B

�B

�B

�B

�B

�B

�B

�B

�B
DB
^B
DB
xB
xB
�B
�B
0B
0B
B
�B

#B
	�B

�B
�B
dB
dB
dB
�B
�B
~B
dB
0B
dB
�B
~B
6B
�B
\B
�B
bB
TB
B
{B
B
SB
�B
�B
?B
�B
�B
YB
sB
�B
B
�B
B
B
KB
�B
�B
B
#B
#B
#B
#B
�B
�B
�B
�B
�B
�B
�B
/B
IB
�B
�B
dB
�B
�B
OB
jB
�B
�B
VB
 'B
 �B
!HB
"4B
"�B
#B
#nB
#B
#nB
$B
$�B
$�B
$ZB
$&B
$&B
$�B
%�B
%�B
%�B
&LB
&�B
&�B
&�B
'B
'�B
'�B
'�B
(>B
(>B
(sB
(�B
)B
)_B
)_B
)�B
*B
*eB
*KB
*KB
+6B
+�B
+�B
,=B
,WB
,�B
,�B
-]B
-wB
-�B
-]B
,�B
-)B
-�B
./B
.�B
-�B
-wB
-wB
.B
.}B
-�B
-wB
-CB
,�B
,�B
,�B
,�B
,�B
,�B
-B
,�B
-�B
-�B
-�B
.B
-�B
/B
.�B
-�B
-)B
-]B
-]B
-�B
-�B
-�B
.B
.IB
.�B
.�B
/5B
/OB
/�B
/�B
/�B
/�B
/�B
1B
1B
1'B
1AB
1B
0�B
1[B
2�B
3MB
3�B
4�B
5B
5%B
5?B
5ZB
5ZB
5ZB
5tB
5ZB
5?B
5?B
5ZB
5�B
5tB
5ZB
5?B
5�B
5ZB
6�B
7LB
7�B
88B
8lB
8�B
9>B
9�B
9�B
:DB
:�B
:�B
;B
;JB
;�B
<jB
<�B
=<B
=VB
=qB
=VB
=�B
>B
>(B
>BB
>�B
>�B
?cB
?�B
@ B
@iB
@�B
@�B
@�B
@�B
A B
AoB
A�B
A�B
A�B
A�B
BB
BAB
B�B
B�B
B�B
B�B
C-B
B�B
C-B
C{B
C{B
CGB
C�B
C�B
C�B
C�B
C�B
DB
DB
C�B
D�B
D�B
D�B
EB
D�B
E9B
E�B
E�B
FB
FtB
FYB
FYB
FYB
F�B
F�B
HB
H1B
HfB
H�B
H�B
H�B
IB
IB
IlB
I�B
I�B
JXB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
KB
KB
K^B
K^B
K�B
K�B
K�B
LB
LB
L0B
LJB
LdB
L�B
L�B
L�B
L�B
N�B
N�B
N�B
N�B
OvB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q B
Q B
P�B
Q�B
R:B
R�B
R�B
R�B
S[B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T,B
U2B
U�B
UgB
U2B
UMB
UMB
UMB
UB
T�B
T�B
UgB
U�B
U�B
U�B
U�B
VSB
V�B
VmB
VmB
V�B
VmB
V�B
V�B
W$B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
Y�B
ZB
Z�B
ZkB
ZB
ZkB
[�B
\�B
\�B
\�B
\�B
\�B
\�B
^B
^�B
^jB
^�B
^�B
_B
_B
_�B
_�B
_�B
_�B
_�B
_�B
`B
`\B
`�B
`\B
`vB
`\B
`�B
`�B
aB
aHB
aHB
abB
abB
a|B
abB
aHB
a�B
a�B
a�B
a�B
b4B
bhB
bNB
b�B
cB
c:B
cTB
c�B
c�B
c�B
c�B
c�B
dB
d&B
d@B
d@B
dZB
d�B
d�B
d�B
e,B
e�B
e�B
e�B
fLB
ffB
f�B
f�B
f�B
gB
gB
gRB
g�B
g�B
g�B
gRB
g�B
hXB
h>B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
iDB
iyB
iyB
i�B
i�B
i�B
i_B
i�B
i�B
i�B
j0B
j�B
j�B
j�B
j�B
kB
j�B
kQB
k�B
k�B
k�B
k�B
l"B
l"B
l�B
l�B
mB
mB
m]B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o B
n�B
o B
o�B
o�B
o�B
o�B
o�B
pB
p!B
poB
poB
p�B
p�B
p�B
p�B
q'B
qB
qAB
q[B
qvB
qvB
q�B
q�B
r-B
r-B
r�B
r|B
r�B
r�B
sB
sMB
s�B
s�B
s�B
s�B
tB
t9B
t9B
tnB
t�B
t�B
t�B
t�B
uB
u%B
u?B
u?B
u�B
u�B
vB
vFB
vzB
v�B
v�B
v�B
v�B
v�B
w2B
wfB
w�B
w�B
w�B
w�B
w�B
x�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104851  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172631  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172631  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172631                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022638  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022638  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                