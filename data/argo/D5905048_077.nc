CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-01-10T00:35:13Z creation;2017-01-10T00:35:17Z conversion to V3.1;2019-12-19T08:17:43Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170110003513  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               MA   JA  I2_0577_077                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��6\�$ 1   @��7F)�@3"GE8�5�d�&�x��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�C3Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D��3D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�FfD�c31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @|(�@��H@��HAp�A=p�A[�
A}p�A��RA��RA��RA��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��B��B��B��B��B��HB��B��B��B��B��HB�z�B�z�B��BîBǮBˮBϮBӮB׮BۮB߮B�B�B�B�B�B��B��B��C�
C�
C�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Do\D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�>D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�D۾D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D��D��D�AHD�^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aδ9Aβ-Aβ-Aδ9Aδ9Aδ9Aβ-Aΰ!AΛ�AΑhA�x�A�ffA�^5A�ZA�S�A�O�A�M�A�E�A�A�A�E�A�9XA�+A�G�A�v�AΏ\AΙ�AΣ�AάAήAήAΰ!AξwA��AΓuA���A�hsA�\)Aʙ�A�"�A��TA��;Ať�A�p�Aá�A�A�I�A��A���A���A���A�l�A�A�  A��9A���A�7LA�5?A�(�A���A���A���A��A��A�  A���A�=qA��!A�1'A�jA���A�C�A�r�A��A���A��RA���A�{A�A�A�ƨA��+A�  A�z�A���A�z�A���A��!A��A���A�r�A��A�t�A�x�A���A��/A�(�A��A�O�A���A�1'A���A��uA�/A���A��A�^5A�ȴA�^5A�?}A�p�A�^5A��A�A�t�A�?}A��A�dZAC�A~bA{�PAyAy�Au\)At�Ar�/Aqx�ApĜAohsAh-Ae�;AeC�Ac��Aa��A_x�A]��AX��AW��AW�AU��AT�uAQG�AO��AOXALI�AJ�/AJ  AHz�AD��AC�A@��A@5?A?C�A>9XA=33A;�#A;33A:ZA7��A6��A6Q�A5�A5C�A4�jA4Q�A3�;A2�A0�uA.�A-�A+x�A&�9A%33A$�!A$1'A#�#A#7LA"�\A!�7A ��AXA^5AA��A(�A��A�hA|�AXA��AM�A��AoA�FAK�A^5A�;A�PAG�AZAl�A�AVA�#A;dA5?A�HAS�A�jA�A	�;Az�A=qAn�A�A`BAC�A=qAA|�A��A��A ��A �@���@�"�@�n�@���@���@�1@�  @�1'@� �@�@�P@�@�^@�1@��@�bN@�E�@�+@��@�@�V@�C�@�{@�h@�A�@��y@�~�@�$�@ݑh@�Ĝ@�b@�$�@١�@�hs@�`B@�`B@�7L@ج@��
@�t�@ָR@�7L@ԋD@��@ӥ�@�
=@�n�@���@мj@��
@ϥ�@���@��#@�hs@��/@��@��@ʸR@�5?@��#@�G�@��`@�Ĝ@�bN@Ǯ@�v�@�`B@ě�@��m@�"�@���@§�@�$�@���@�I�@�1@��m@��w@���@�\)@�@���@�r�@���@��P@��P@�|�@�;d@��@�n�@�{@��@���@���@�=q@��7@��^@��^@�@�/@���@�`B@���@�@���@��@�bN@�x�@�t�@���@�-@�X@�r�@���@�"�@��H@���@�~�@�^5@��^@�7L@��9@�Q�@�|�@�C�@�C�@��@�S�@�(�@�Q�@� �@��@��
@�b@�9X@�$�@�&�@�l�@���@�7L@�I�@��@��y@�~�@�33@�C�@�"�@�1'@���@���@�j@�(�@���@��P@�S�@��@��H@��@���@�~�@�hs@�O�@�&�@�X@�&�@��@�z�@� �@��F@��F@���@��@�+@�v�@��@��`@�bN@��@��w@��R@�o@�o@��@���@���@�ȴ@���@��+@�^5@�@��T@���@�x�@�hs@�O�@��@��9@�9X@�b@��w@���@���@�|�@�dZ@�\)@�\)@�C�@�o@�ȴ@���@�v�@�E�@�=q@�=q@��@�/@���@�r�@�1'@�  @�l�@�
=@���@��\@��\@�~�@�~�@�~�@�~�@��+@��+@��+@�~�@�ff@�ff@��@��@��y@��!@��\@�n�@�M�@��@���@�?}@��`@�r�@�9X@��;@��
@��P@�o@�v�@��T@��`@���@��D@��`@���@�b@�ƨ@���@��P@�S�@��R@�~�@��!@�=q@��@�{@��#@�p�@�V@�z�@�Q�@�1'@�  @���@�|�@�C�@�+@��!@�ff@�$�@���@�p�@�G�@�?}@�/@�%@���@���@�Q�@��F@���@�t�@�C�@�33@�+@�"�@��@��y@���@��\@�~�@�^5@���@�p�@�/@���@��D@�Q�@�9X@� �@�b@�  @�@K�@~��@~��@~�@~�@~�R@~ff@}�-@}`B@}/@}�@|�/@|z�@|I�@|�@{S�@z�!@z=q@zJ@y�#@y��@y��@yx�@yX@y7L@x��@x�u@xb@w�;@w�;@wl�@wK�@v�y@u�@u?}@tI�@s��@s�F@s33@r�\@rM�@rM�@rM�@rM�@r^5@q��@q7L@q%@p�9@pbN@pA�@pb@o��@o\)@n�@nV@m@mV@l�@l��@lz�@lZ@k��@k�@j��@j~�@j-@j�@i��@i�^@i%@hbN@hb@g�@g�P@f��@e��@eO�@d��@d�/@dZ@c�m@c��@c@b�\@b-@a��@a��@a�7@aG�@`��@`A�@_�;@_��@_+@^��@^�+@^�+@^�+@^v�@^{@]O�@\�@\j@[�F@[S�@Z�@ZM�@Y��@Y%@X�u@X  @W�@W\)@V��@VE�@U�-@U�@U?}@U�@T��@T��@T�@Tz�@S��@St�@SC�@R��@Q�#@Q��@Q�7@QX@P��@Pr�@PA�@Pb@O�w@OK�@O
=@N�R@N�+@M�T@M�h@L��@L�/@Lz�@L1@K�F@Kt�@KdZ@KC�@K@J�!@J�\@J�\@J=q@Ix�@I%@H��@HA�@Hb@G�w@G�@G�P@G+@Fv�@F{@E�T@E�h@EV@D�j@DZ@DI�@D(�@D1@C�m@C��@C33@C33@Co@C@B�!@B~�@BM�@B=q@A�@A�7@@�u@?�w@?\)@?;d@?+@?+@>ȴ@>��@>��@>v�@>E�@=��@=�@=`B@=/@=V@<�/@<z�@<9X@<�@;�m@;dZ@;33@;"�@;o@:�H@:��@:��@:�@9x�@8��@8�`@8��@8��@8��@8Ĝ@8�9@8��@8��@8��@8Q�@8A�@8  @7�@7�P@6��@6�R@6{@5�-@5`B@5?}@5�@4��@4�j@4��@3��@3��@3dZ@3C�@2��@2��@2�!@2^5@2�@1�@1�^@1��@1x�@1G�@0�`@0bN@0A�@0 �@/�@/��@/l�@.��@.{@-�T@-�@-/@-O�@-�@,�j@,�@+�m@+�F@+33@*��@*�\@*�\@*~�@*n�@*-@)�@)�#@)��@)��@)��@)�7@)G�@)&�@(��@(Ĝ@(�@(Q�@(  @'��@'\)@&��@&ȴ@&�R@&�R@&��@&�+@&V@%�@%@%@%��@%p�@%/@%V@$�/@$�j@$j@$9X@$�@#��@#��@#t�@#"�@"��@"�!@"��@"^5@"=q@!��@!�@!�^@!��@!hs@!G�@!&�@ ��@ �`@ �`@ Ĝ@ �9@ �9@ �u@ 1'@��@|�@\)@�y@�@�y@�y@ȴ@�+@�T@�h@p�@�@�D@j@9X@(�@(�@1@�
@��@S�@33@33@@��@��@n�@=q@J@�@��@�7@hs@X@7L@�9@bN@1'@b@�;@�@�P@�P@|�@\)@+@�@�y@�y@�y@�R@�R@��@�+@�+@ff@{@�@@��@p�@/@��@��@z�@9X@�@��@�
@��@t�@"�@�@��@��@�\@~�@-@��@�#@��@�^@��@��@x�@G�@7L@7L@�@��@��@��@Ĝ@�u@A�@  @�;@�;@�;@�;@��@\)@K�@;d@;d@+@
=@�y@ȴ@�R@��@�+@v�@E�@{@�@@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aδ9Aβ-Aβ-Aδ9Aδ9Aδ9Aβ-Aΰ!AΛ�AΑhA�x�A�ffA�^5A�ZA�S�A�O�A�M�A�E�A�A�A�E�A�9XA�+A�G�A�v�AΏ\AΙ�AΣ�AάAήAήAΰ!AξwA��AΓuA���A�hsA�\)Aʙ�A�"�A��TA��;Ať�A�p�Aá�A�A�I�A��A���A���A���A�l�A�A�  A��9A���A�7LA�5?A�(�A���A���A���A��A��A�  A���A�=qA��!A�1'A�jA���A�C�A�r�A��A���A��RA���A�{A�A�A�ƨA��+A�  A�z�A���A�z�A���A��!A��A���A�r�A��A�t�A�x�A���A��/A�(�A��A�O�A���A�1'A���A��uA�/A���A��A�^5A�ȴA�^5A�?}A�p�A�^5A��A�A�t�A�?}A��A�dZAC�A~bA{�PAyAy�Au\)At�Ar�/Aqx�ApĜAohsAh-Ae�;AeC�Ac��Aa��A_x�A]��AX��AW��AW�AU��AT�uAQG�AO��AOXALI�AJ�/AJ  AHz�AD��AC�A@��A@5?A?C�A>9XA=33A;�#A;33A:ZA7��A6��A6Q�A5�A5C�A4�jA4Q�A3�;A2�A0�uA.�A-�A+x�A&�9A%33A$�!A$1'A#�#A#7LA"�\A!�7A ��AXA^5AA��A(�A��A�hA|�AXA��AM�A��AoA�FAK�A^5A�;A�PAG�AZAl�A�AVA�#A;dA5?A�HAS�A�jA�A	�;Az�A=qAn�A�A`BAC�A=qAA|�A��A��A ��A �@���@�"�@�n�@���@���@�1@�  @�1'@� �@�@�P@�@�^@�1@��@�bN@�E�@�+@��@�@�V@�C�@�{@�h@�A�@��y@�~�@�$�@ݑh@�Ĝ@�b@�$�@١�@�hs@�`B@�`B@�7L@ج@��
@�t�@ָR@�7L@ԋD@��@ӥ�@�
=@�n�@���@мj@��
@ϥ�@���@��#@�hs@��/@��@��@ʸR@�5?@��#@�G�@��`@�Ĝ@�bN@Ǯ@�v�@�`B@ě�@��m@�"�@���@§�@�$�@���@�I�@�1@��m@��w@���@�\)@�@���@�r�@���@��P@��P@�|�@�;d@��@�n�@�{@��@���@���@�=q@��7@��^@��^@�@�/@���@�`B@���@�@���@��@�bN@�x�@�t�@���@�-@�X@�r�@���@�"�@��H@���@�~�@�^5@��^@�7L@��9@�Q�@�|�@�C�@�C�@��@�S�@�(�@�Q�@� �@��@��
@�b@�9X@�$�@�&�@�l�@���@�7L@�I�@��@��y@�~�@�33@�C�@�"�@�1'@���@���@�j@�(�@���@��P@�S�@��@��H@��@���@�~�@�hs@�O�@�&�@�X@�&�@��@�z�@� �@��F@��F@���@��@�+@�v�@��@��`@�bN@��@��w@��R@�o@�o@��@���@���@�ȴ@���@��+@�^5@�@��T@���@�x�@�hs@�O�@��@��9@�9X@�b@��w@���@���@�|�@�dZ@�\)@�\)@�C�@�o@�ȴ@���@�v�@�E�@�=q@�=q@��@�/@���@�r�@�1'@�  @�l�@�
=@���@��\@��\@�~�@�~�@�~�@�~�@��+@��+@��+@�~�@�ff@�ff@��@��@��y@��!@��\@�n�@�M�@��@���@�?}@��`@�r�@�9X@��;@��
@��P@�o@�v�@��T@��`@���@��D@��`@���@�b@�ƨ@���@��P@�S�@��R@�~�@��!@�=q@��@�{@��#@�p�@�V@�z�@�Q�@�1'@�  @���@�|�@�C�@�+@��!@�ff@�$�@���@�p�@�G�@�?}@�/@�%@���@���@�Q�@��F@���@�t�@�C�@�33@�+@�"�@��@��y@���@��\@�~�@�^5@���@�p�@�/@���@��D@�Q�@�9X@� �@�b@�  @�@K�@~��@~��@~�@~�@~�R@~ff@}�-@}`B@}/@}�@|�/@|z�@|I�@|�@{S�@z�!@z=q@zJ@y�#@y��@y��@yx�@yX@y7L@x��@x�u@xb@w�;@w�;@wl�@wK�@v�y@u�@u?}@tI�@s��@s�F@s33@r�\@rM�@rM�@rM�@rM�@r^5@q��@q7L@q%@p�9@pbN@pA�@pb@o��@o\)@n�@nV@m@mV@l�@l��@lz�@lZ@k��@k�@j��@j~�@j-@j�@i��@i�^@i%@hbN@hb@g�@g�P@f��@e��@eO�@d��@d�/@dZ@c�m@c��@c@b�\@b-@a��@a��@a�7@aG�@`��@`A�@_�;@_��@_+@^��@^�+@^�+@^�+@^v�@^{@]O�@\�@\j@[�F@[S�@Z�@ZM�@Y��@Y%@X�u@X  @W�@W\)@V��@VE�@U�-@U�@U?}@U�@T��@T��@T�@Tz�@S��@St�@SC�@R��@Q�#@Q��@Q�7@QX@P��@Pr�@PA�@Pb@O�w@OK�@O
=@N�R@N�+@M�T@M�h@L��@L�/@Lz�@L1@K�F@Kt�@KdZ@KC�@K@J�!@J�\@J�\@J=q@Ix�@I%@H��@HA�@Hb@G�w@G�@G�P@G+@Fv�@F{@E�T@E�h@EV@D�j@DZ@DI�@D(�@D1@C�m@C��@C33@C33@Co@C@B�!@B~�@BM�@B=q@A�@A�7@@�u@?�w@?\)@?;d@?+@?+@>ȴ@>��@>��@>v�@>E�@=��@=�@=`B@=/@=V@<�/@<z�@<9X@<�@;�m@;dZ@;33@;"�@;o@:�H@:��@:��@:�@9x�@8��@8�`@8��@8��@8��@8Ĝ@8�9@8��@8��@8��@8Q�@8A�@8  @7�@7�P@6��@6�R@6{@5�-@5`B@5?}@5�@4��@4�j@4��@3��@3��@3dZ@3C�@2��@2��@2�!@2^5@2�@1�@1�^@1��@1x�@1G�@0�`@0bN@0A�@0 �@/�@/��@/l�@.��@.{@-�T@-�@-/@-O�@-�@,�j@,�@+�m@+�F@+33@*��@*�\@*�\@*~�@*n�@*-@)�@)�#@)��@)��@)��@)�7@)G�@)&�@(��@(Ĝ@(�@(Q�@(  @'��@'\)@&��@&ȴ@&�R@&�R@&��@&�+@&V@%�@%@%@%��@%p�@%/@%V@$�/@$�j@$j@$9X@$�@#��@#��@#t�@#"�@"��@"�!@"��@"^5@"=q@!��@!�@!�^@!��@!hs@!G�@!&�@ ��@ �`@ �`@ Ĝ@ �9@ �9@ �u@ 1'@��@|�@\)@�y@�@�y@�y@ȴ@�+@�T@�h@p�@�@�D@j@9X@(�@(�@1@�
@��@S�@33@33@@��@��@n�@=q@J@�@��@�7@hs@X@7L@�9@bN@1'@b@�;@�@�P@�P@|�@\)@+@�@�y@�y@�y@�R@�R@��@�+@�+@ff@{@�@@��@p�@/@��@��@z�@9X@�@��@�
@��@t�@"�@�@��@��@�\@~�@-@��@�#@��@�^@��@��@x�@G�@7L@7L@�@��@��@��@Ĝ@�u@A�@  @�;@�;@�;@�;@��@\)@K�@;d@;d@+@
=@�y@ȴ@�R@��@�+@v�@E�@{@�@@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
��B
��B
��B
��B
��B
��B  BBBBBB	7BDBPBPBVBbB�B�B �B2-BN�BaHBiyBr�B}�B�%B�DB�JB��B�'B�;B�yB�BhBVBVBDB1BB��B��BVBC�B@�B"�B�B'�B#�B<jBE�B@�B:^BD�B|�B�B�=B�=B�VB�{B��B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B�{B�bB�=B�BgmBS�BN�B,B\B��B�`BƨB�B�oB�B|�Bq�Be`B_;BK�BD�B?}B33B�BoB	7BB
�B
�HB
��B
�wB
�B
��B
��B
��B
��B
�bB
s�B
ffB
XB
D�B
?}B
(�B
�B
oB
DB
B	��B	ǮB	�B	��B	��B	�DB	{�B	r�B	Q�B	G�B	B�B	8RB	.B	�B	�B	�B	%B��B��B��B�B�B�B�B�yB�yB�yB�B�B�B�B��B��B��B��B��B��B��B��B��B�B�B�B�B�yB�sB�yB�yB�B�B�B�B�B�B�B�B�yB�sB�fB�fB�ZB�TB�BB�5B�5B�B�B�B��B��B��B��BƨBǮBŢBĜBÖBBB��B��BÖBBÖB��BȴB��B�B�;B�HB�BB�BB�;B�5B�/B�#B�#B�B�
B��BǮB��BŢBƨBȴBȴB�}B�}B�dB�9B�!B��B��B��B�oB��B��B��B��B��B��B�B�B�B�B�B�!B�LB�FB�FB�FB�FB�LB�RB�dB�dB�jB�qB�}B��B��BBĜBŢB��B��B��B��B��B��B��B��B��B�B�
B�B�B�5B�`B�B�B�B�B�B�B�B��B��B��B	B	B	+B	+B	1B	1B		7B		7B	JB	bB	oB	uB	uB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	%�B	+B	6FB	8RB	=qB	A�B	>wB	7LB	33B	2-B	2-B	1'B	1'B	33B	9XB	;dB	=qB	=qB	>wB	C�B	H�B	K�B	P�B	VB	W
B	[#B	`BB	cTB	jB	m�B	o�B	p�B	r�B	v�B	|�B	z�B	y�B	�JB	�VB	�PB	�=B	�DB	�7B	�JB	�uB	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�FB	�FB	�LB	�LB	�LB	�LB	�XB	�qB	��B	��B	ÖB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�)B	�/B	�/B	�5B	�;B	�HB	�NB	�TB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
B
%B
1B
1B
	7B

=B

=B

=B
1B
1B
JB
DB
DB
DB

=B
	7B
1B
+B
+B
1B

=B
DB
JB
PB
PB
JB
PB
VB
VB
\B
\B
\B
bB
bB
hB
hB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
,B
-B
-B
-B
-B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
ZB
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
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
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
�B
��B
��B
�B
�B
�B 4B BUBAB-B9B	RBDBPBjBpB}B�B�B �B1�BN�BaHBi_Br�B}�B�?B�^B�dB��B�-B�B�B��B�BNBB�B
�B�B �B�B�BE�BCaB%�B# B(�B%�B?�BF�BBB;�BD�B|�B��B��B�B�HB��B��B� B�!B�iB��B��B��B�}B��B��B�sB�8B��B��B�B��B�SB�B�TB��B�rBkkBW�BT{B0�BB �B�B�DB��B�FB�+BHBs�Bg�Ba�BMBF%BB�B6`BjBB^B�B
��B
�,B
�}B
��B
�B
��B
��B
��B
��B
��B
u�B
i*B
Z7B
F�B
CB
*�B
VB
,B
B
�B
{B	�XB	�}B	�B	�jB	�<B	B	wLB	S�B	H�B	D�B	:^B	1�B	 �B	_B	�B	1B��B��B�B��B��B��B��B�B�B�6B�B�;B�iB�B�tB�tB��B�tB�tB��B��B��B��B�TB�'B�B�OB�KB�DB�0B�B��B�B�!B�nB��B�B�B�}B��B��B��B��B�FB�&B�bB�pB��B�	B�KB��BңBϫB�"B��BǔBȴB�tB��B�9BāB�gBªB�-B�B�MBĜB��B��B�bBخB��B�B�B�|B��B�!B��B�B��B�B�yBԯB��B��BżB�_B�XB�#B�OB��B��B�ZB��B��B��B�sB��B�\B�B��B��B��B��B�wB��B��B��B��B�[B��B��B�zB�zB��B��B�	B��B�6B�qB�B� B��B�'B�-B�SBƎB�jB�NBЗBѝB�\B�VBϑBңB�MBևB�sBٚBچBޞB��B�B�wB�|B�hB�MB�3B�B�+B�LB��B	�B	mB	EB	_B	�B	�B		�B	
XB	6B	 B	�B	�B	�B	�B	�B	
B	B	=B	�B	�B	~B	B	�B	�B	�B	 'B	%B	%�B	*�B	6`B	8�B	>(B	B�B	@OB	8�B	3�B	2�B	2�B	1�B	1�B	3�B	9�B	;�B	=�B	=�B	>�B	C�B	I7B	L0B	Q�B	V9B	W?B	[WB	`'B	c B	j�B	m�B	o�B	p�B	r�B	w2B	~(B	{0B	y>B	��B	�BB	�B	��B	��B	��B	�B	��B	��B	�B	�vB	�*B	��B	�]B	��B	�vB	��B	��B	�zB	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�bB	ЗB	�}B	�\B	�VB	�6B	�6B	�PB	��B	� B	�9B	�CB	�]B	�dB	�IB	�jB	ߊB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�TB	�B	��B	��B	��B	�-B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�"B	�BB
 4B
 4B
UB
oB
uB
uB
{B
gB
mB
mB
SB
�B
�B
�B
�B
�B
 OB
 4B
B
tB
�B
�B
	RB

rB

�B

�B
�B
1B
�B
xB
xB
�B

�B
	�B
�B
zB
_B
�B

�B
�B
~B
�B
�B
�B
�B
�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
 B
 B
 �B
 �B
!B
 �B
!B
"4B
"4B
#:B
$B
$B
$B
$&B
$B
$�B
$�B
$�B
%B
%B
&2B
&B
&B
'B
'B
'B
'B
'8B
'8B
($B
($B
(>B
)*B
)B
)*B
)*B
)DB
)DB
)DB
*0B
*0B
*0B
*0B
*KB
*eB
+6B
,=B
,"B
,WB
,qB
-wB
-]B
-CB
-CB
-]B
.cB
.cB
.cB
/OB
/iB
/OB
0UB
0;B
0UB
0UB
0oB
1[B
1[B
1vB
1vB
2-B
2GB
2-B
2aB
3�B
3�B
4nB
4�B
4�B
5�B
5tB
5�B
5�B
6�B
6zB
7�B
7�B
7�B
7�B
8�B
8�B
9rB
9rB
9�B
9rB
9�B
9�B
9�B
9�B
:�B
:�B
:�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
@�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
IB
I�B
J#B
K)B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
LB
K�B
K�B
MB
MB
MB
MB
MB
MB
L�B
MB
MB
M�B
M�B
NB
NB
L�B
MB
N"B
N"B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
OB
N�B
OB
PB
P.B
O�B
P.B
QB
Q B
R B
R B
R B
R B
RB
R:B
S&B
T,B
T,B
T,B
T,B
U2B
U2B
U2B
V9B
V9B
V9B
V9B
VB
V9B
W?B
W?B
W$B
W?B
X+B
XEB
X_B
XyB
YKB
YKB
ZQB
[#B
\CB
\]B
\xB
\]B
\]B
]~B
]~B
^OB
^5B
^OB
^jB
^jB
^jB
^OB
^OB
^jB
^OB
^jB
_pB
_pB
_pB
`vB
`vB
a|B
a|B
a�B
b�B
b�B
bhB
cnB
cnB
c�B
c�B
c�B
d�B
d�B
dtB
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
h�B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
hsB
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
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
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
xB
x�B
y	B
x�B
y	B
x�B
x�B
y	B
y	B
y	B
y	B
zB
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.16(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201701140036442017011400364420170114003644201806221307382018062213073820180622130738201804050708102018040507081020180405070810  JA  ARFMdecpA19c                                                                20170110093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170110003513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170110003515  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170110003515  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170110003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170110003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170110003516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170110003516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170110003517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170110003517                      G�O�G�O�G�O�                JA  ARUP                                                                        20170110010301                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170110153343  CV  JULD            G�O�G�O�F�A�                JM  ARCAJMQC2.0                                                                 20170113153644  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170113153644  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220810  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040738  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                