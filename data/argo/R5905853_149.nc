CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-03-10T03:44:48Z creation;2023-03-10T03:44:49Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230310034448  20230310040100  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @����1   @��v�I2@/�M����c#���+1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C33C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C'�fC*  C,  C.  C0  C2�C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @@u@��H@��HAp�A=p�A]p�A}p�A��RA��RA��RA��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BWB_\)Bg\)Bo\)Bw\)B\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BîB�zB�z�B��GB�zB�z�BۮB߮B�B�B�B�B�B��B��B��C�
C�
C
=C�
C	�
C�
C�pC�
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
C'�pC)�
C+�
C-�
C/�
C1�C3�
C5�pC7�
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
Ci�Ck�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C�޸C��C��C�޸C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�>D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�~D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�7�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�+kA�&�A���A���A�K�A���A�E�A��jAŷ�Aŀ�A�y�A�p�A�c�A�V9A�E�A�2�A�#�A�hA���A�PA�/A� \A��VA���Aķ�Aĩ*AĠ�Aĝ�AĚ7Ač�AĆ�AąAĂ�A��A�}�A�}�A��A��A��AĀ�AĈ�AĖSAĢhAĨ�AģAĖ�A�;dAÿHA�\�A��A�^�A���A��A���A�-�A��\A��0A�^A��A�4A��/A�1[A��HA��=A��A��fA�XA�*0A���A��<A��A�0!A�o�A��A�O�A�\�A�NA�l�A���A��-A�(�A�A���A�YA��A�A���A~�Ay&Au\�AtMAq6�Ak�KAhuAf�BAe#�Ad'RAb�4A_��A\�6AYVmAUK^AR4nAO�AM�0AK��AJ'�AH��AFY�AC�4A@��A>$�A<��A:\�A8�jA5A3j�A2W?A2��A1C�A0&�A.��A.� A/�A/�A.�A-�+A,��A+sA*��A)qA(~(A&�A%OvA"��A �BA �AL�A͟A!-A33A��AJ�A�A!�A�A2aA�fAZAx�A33A��A�
AzA�MAbNA�AP�A�^A�wA��A4�A��ARTA��AIRA��Ao�A��A�EA	�AA��A@OA��AVmA-A
��A
�A
�<A
l�A	��A	��A��A	tTA	�	A��A	FtA	�VA	�:A	#:A��AA� AW?A�{As�A\)AcA��A��A�AxA��A�yA��A��A�xAqvA7A ��A c�A O�A \)A�A ͟A ]d@��0@���@��t@��o@���@��@�+@��j@�D�@���@�
=@��g@�҉@� \@��@�"@��g@��X@�C�@��	@�u%@���@���@�u%@�q@�@�q@�O@��@�-@�F@���@���@�F@�	@�g�@�B�@�9�@�#�@�?@�^�@�O@��@�͟@��p@���@޼j@ޱ�@ޖ�@ݿH@�+@��@��@ܰ�@ܛ�@ܐ.@܎�@ۗ$@��@��@�H�@��@�}V@�%�@���@ׁ@�!-@֌@��@�w�@��@��>@ӧ�@�@�v�@�F@�v�@�`�@��@�B�@��@��@�ѷ@�a|@�;d@�1@˶F@�Y�@�%@ʪe@��@Ƀ{@���@�w�@�5?@���@Ǔ@��@�@�=@�m�@Þ�@�Mj@�/�@�!�@ªe@�m�@��@��S@���@��@�s�@��"@�Ft@���@��$@�a�@��@��1@���@�a�@�?}@��@��h@�^5@�(�@�F�@��@��=@��w@���@�w2@�i�@��{@�/�@��@���@��!@�d�@�c�@�PH@��@���@�a�@��X@�M@�2�@��#@��@���@�m�@��\@���@��:@�7L@��@��)@�~�@�0U@���@��@���@�
�@��@�xl@��+@��	@���@�C�@��@���@��t@�qv@���@�e�@��@�j�@�P�@�J#@��@��1@�i�@�=q@� �@��@�dZ@��@�u�@�K�@�$t@�:�@���@���@��"@�u�@�A�@�@���@��x@��@�\�@�4�@�"�@��@��@��p@��j@��z@��.@�q@�{@���@�>�@�V@���@��@��r@��@�j@��@�h�@�1@���@���@��	@�h
@��&@�s�@�'�@���@�_�@��@��3@��7@�A @���@��@�ߤ@��j@���@���@��Y@�_@�	@��@���@���@���@�u�@�S&@��@���@���@�H@�~@��r@���@�zx@�4�@�#�@��@�~(@��]@�@��t@��n@��'@���@�y�@�a�@�0�@��/@��R@�q@���@���@��@�ȴ@���@�c�@�O@��&@��@��[@��:@�a@�%F@��X@���@�Z�@�+k@�	@��@��6@���@�c@�J�@�!�@��[@�R�@�1@���@�y�@�Vm@��@��y@��p@�ȴ@���@�v�@�Ov@�6@��@��Z@��>@�خ@��F@��f@�N<@��@��@���@���@��4@��_@�~�@�Ft@�@��}@���@���@�|@�T�@�0�@�
=@��]@��z@�V�@��@��@��m@���@��	@�X@�F@��s@��o@�Ov@���@���@���@��P@���@�u�@��@���@��x@�H�@��@ݘ@~�@~E�@}ϫ@}x�@}%@|��@|`�@|4n@{=@z��@z�F@y�)@yO�@x�_@xm�@x[�@wC�@v�@u�t@t�`@t��@tI�@s{J@sg�@sn/@s�k@s�K@s�&@sA�@q�T@qS&@pѷ@p[�@p�@o�@ox@oW?@o�@nC�@m�@lɆ@kO@j�R@j@�@i�o@i�"@h��@hoi@g�@g|�@f�"@f��@f�@fJ�@ec�@e:�@d�v@d�j@d|�@d�@c�f@c�@b��@bOv@b�@a�#@a�@azx@a&�@`w�@_�{@^��@]��@]Y�@]	l@\N�@[ƨ@[��@[t�@[33@Z�c@Z}V@Y��@Y�@YX@X�p@X��@X�@X�D@X2�@X�@W��@W��@V�@VOv@V�@V �@U��@U%@T��@T��@T'R@S��@S�@S�@R�@R�2@R��@R��@RC�@Q�.@Q�9@Q��@QS&@Q�@P�	@P��@P�@O��@O;d@N�8@N�s@N��@NQ@N)�@M��@Mc�@M=�@L�5@L2�@K�w@K��@KJ#@Jz@I��@H��@H�z@HK^@G�}@GH�@F�y@F�R@F�L@F��@FW�@F�@E�n@Ef�@E@@D��@D�@C��@Cg�@Cqv@CiD@C�@C4�@C]�@C&@C&@C@C�@Bl�@A�@AX@A!�@@��@@��@@��@@`�@?��@?�4@?P�@?X�@?A�@>�@>�]@>��@>_@=��@=�C@=S&@=�@=+@=�@=�@=2a@<�E@<4n@;�;@;��@;l�@;Z�@;C@:��@:_�@:u@9�C@9L�@9@@8�)@8l"@86@8�@7��@7�0@7��@7qv@7�@6��@6&�@5��@5zx@5IR@5%F@5�@4�@4�@4-�@47@4�@3�r@3��@3v`@38@2�c@2v�@2?@2($@1��@1�@1m]@1o @1S&@0��@0�U@0�D@0��@0I�@0	�@/�@/��@/�[@/>�@.ߤ@.�@.�F@.xl@.^5@.J�@-��@-c@-[W@-7L@-q@,��@,�j@,��@,~(@,�@,�@+n/@+�@*��@*ȴ@*�x@*:*@)�D@)�j@)��@)�@(�p@(K^@(  @'��@'@&�@&�r@&n�@&YK@&W�@&Q@&@�@%�@%�@%|@%Q�@%�@$�$@$�@$M@#�@#�$@#RT@#�@"ں@"z@"H�@"6�@"@"4@!�T@!��@!c�@!�@ �E@ �j@ j@ I�@ �@�6@��@@O@.I@+@"�@�@�s@{�@6�@��@x�@<6@�@�U@_@�@�+@��@��@�V@RT@��@�s@{�@e@�@�@`B@�	@��@y>@>B@x@��@��@�	@P�@6z@
=@��@��@R�@;�@�@u@��@��@��@|@hs@0�@;@��@�[@�4@h�@M@%�@�@�w@�q@y�@.I@@�8@�y@��@��@kQ@L0@6�@($@��@�'@j@:�@*0@V@�4@��@tT@e�@*�@x@�K@��@��@x@g�@W?@J#@.I@�@�@�2@��@��@c @	@�@�@��@x�@+�@�@��@�e@Q�@%�@�m@��@�:@~�@b�@=@Y@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�+kA�&�A���A���A�K�A���A�E�A��jAŷ�Aŀ�A�y�A�p�A�c�A�V9A�E�A�2�A�#�A�hA���A�PA�/A� \A��VA���Aķ�Aĩ*AĠ�Aĝ�AĚ7Ač�AĆ�AąAĂ�A��A�}�A�}�A��A��A��AĀ�AĈ�AĖSAĢhAĨ�AģAĖ�A�;dAÿHA�\�A��A�^�A���A��A���A�-�A��\A��0A�^A��A�4A��/A�1[A��HA��=A��A��fA�XA�*0A���A��<A��A�0!A�o�A��A�O�A�\�A�NA�l�A���A��-A�(�A�A���A�YA��A�A���A~�Ay&Au\�AtMAq6�Ak�KAhuAf�BAe#�Ad'RAb�4A_��A\�6AYVmAUK^AR4nAO�AM�0AK��AJ'�AH��AFY�AC�4A@��A>$�A<��A:\�A8�jA5A3j�A2W?A2��A1C�A0&�A.��A.� A/�A/�A.�A-�+A,��A+sA*��A)qA(~(A&�A%OvA"��A �BA �AL�A͟A!-A33A��AJ�A�A!�A�A2aA�fAZAx�A33A��A�
AzA�MAbNA�AP�A�^A�wA��A4�A��ARTA��AIRA��Ao�A��A�EA	�AA��A@OA��AVmA-A
��A
�A
�<A
l�A	��A	��A��A	tTA	�	A��A	FtA	�VA	�:A	#:A��AA� AW?A�{As�A\)AcA��A��A�AxA��A�yA��A��A�xAqvA7A ��A c�A O�A \)A�A ͟A ]d@��0@���@��t@��o@���@��@�+@��j@�D�@���@�
=@��g@�҉@� \@��@�"@��g@��X@�C�@��	@�u%@���@���@�u%@�q@�@�q@�O@��@�-@�F@���@���@�F@�	@�g�@�B�@�9�@�#�@�?@�^�@�O@��@�͟@��p@���@޼j@ޱ�@ޖ�@ݿH@�+@��@��@ܰ�@ܛ�@ܐ.@܎�@ۗ$@��@��@�H�@��@�}V@�%�@���@ׁ@�!-@֌@��@�w�@��@��>@ӧ�@�@�v�@�F@�v�@�`�@��@�B�@��@��@�ѷ@�a|@�;d@�1@˶F@�Y�@�%@ʪe@��@Ƀ{@���@�w�@�5?@���@Ǔ@��@�@�=@�m�@Þ�@�Mj@�/�@�!�@ªe@�m�@��@��S@���@��@�s�@��"@�Ft@���@��$@�a�@��@��1@���@�a�@�?}@��@��h@�^5@�(�@�F�@��@��=@��w@���@�w2@�i�@��{@�/�@��@���@��!@�d�@�c�@�PH@��@���@�a�@��X@�M@�2�@��#@��@���@�m�@��\@���@��:@�7L@��@��)@�~�@�0U@���@��@���@�
�@��@�xl@��+@��	@���@�C�@��@���@��t@�qv@���@�e�@��@�j�@�P�@�J#@��@��1@�i�@�=q@� �@��@�dZ@��@�u�@�K�@�$t@�:�@���@���@��"@�u�@�A�@�@���@��x@��@�\�@�4�@�"�@��@��@��p@��j@��z@��.@�q@�{@���@�>�@�V@���@��@��r@��@�j@��@�h�@�1@���@���@��	@�h
@��&@�s�@�'�@���@�_�@��@��3@��7@�A @���@��@�ߤ@��j@���@���@��Y@�_@�	@��@���@���@���@�u�@�S&@��@���@���@�H@�~@��r@���@�zx@�4�@�#�@��@�~(@��]@�@��t@��n@��'@���@�y�@�a�@�0�@��/@��R@�q@���@���@��@�ȴ@���@�c�@�O@��&@��@��[@��:@�a@�%F@��X@���@�Z�@�+k@�	@��@��6@���@�c@�J�@�!�@��[@�R�@�1@���@�y�@�Vm@��@��y@��p@�ȴ@���@�v�@�Ov@�6@��@��Z@��>@�خ@��F@��f@�N<@��@��@���@���@��4@��_@�~�@�Ft@�@��}@���@���@�|@�T�@�0�@�
=@��]@��z@�V�@��@��@��m@���@��	@�X@�F@��s@��o@�Ov@���@���@���@��P@���@�u�@��@���@��x@�H�@��@ݘ@~�@~E�@}ϫ@}x�@}%@|��@|`�@|4n@{=@z��@z�F@y�)@yO�@x�_@xm�@x[�@wC�@v�@u�t@t�`@t��@tI�@s{J@sg�@sn/@s�k@s�K@s�&@sA�@q�T@qS&@pѷ@p[�@p�@o�@ox@oW?@o�@nC�@m�@lɆ@kO@j�R@j@�@i�o@i�"@h��@hoi@g�@g|�@f�"@f��@f�@fJ�@ec�@e:�@d�v@d�j@d|�@d�@c�f@c�@b��@bOv@b�@a�#@a�@azx@a&�@`w�@_�{@^��@]��@]Y�@]	l@\N�@[ƨ@[��@[t�@[33@Z�c@Z}V@Y��@Y�@YX@X�p@X��@X�@X�D@X2�@X�@W��@W��@V�@VOv@V�@V �@U��@U%@T��@T��@T'R@S��@S�@S�@R�@R�2@R��@R��@RC�@Q�.@Q�9@Q��@QS&@Q�@P�	@P��@P�@O��@O;d@N�8@N�s@N��@NQ@N)�@M��@Mc�@M=�@L�5@L2�@K�w@K��@KJ#@Jz@I��@H��@H�z@HK^@G�}@GH�@F�y@F�R@F�L@F��@FW�@F�@E�n@Ef�@E@@D��@D�@C��@Cg�@Cqv@CiD@C�@C4�@C]�@C&@C&@C@C�@Bl�@A�@AX@A!�@@��@@��@@��@@`�@?��@?�4@?P�@?X�@?A�@>�@>�]@>��@>_@=��@=�C@=S&@=�@=+@=�@=�@=2a@<�E@<4n@;�;@;��@;l�@;Z�@;C@:��@:_�@:u@9�C@9L�@9@@8�)@8l"@86@8�@7��@7�0@7��@7qv@7�@6��@6&�@5��@5zx@5IR@5%F@5�@4�@4�@4-�@47@4�@3�r@3��@3v`@38@2�c@2v�@2?@2($@1��@1�@1m]@1o @1S&@0��@0�U@0�D@0��@0I�@0	�@/�@/��@/�[@/>�@.ߤ@.�@.�F@.xl@.^5@.J�@-��@-c@-[W@-7L@-q@,��@,�j@,��@,~(@,�@,�@+n/@+�@*��@*ȴ@*�x@*:*@)�D@)�j@)��@)�@(�p@(K^@(  @'��@'@&�@&�r@&n�@&YK@&W�@&Q@&@�@%�@%�@%|@%Q�@%�@$�$@$�@$M@#�@#�$@#RT@#�@"ں@"z@"H�@"6�@"@"4@!�T@!��@!c�@!�@ �E@ �j@ j@ I�@ �@�6@��@@O@.I@+@"�@�@�s@{�@6�@��@x�@<6@�@�U@_@�@�+@��@��@�V@RT@��@�s@{�@e@�@�@`B@�	@��@y>@>B@x@��@��@�	@P�@6z@
=@��@��@R�@;�@�@u@��@��@��@|@hs@0�@;@��@�[@�4@h�@M@%�@�@�w@�q@y�@.I@@�8@�y@��@��@kQ@L0@6�@($@��@�'@j@:�@*0@V@�4@��@tT@e�@*�@x@�K@��@��@x@g�@W?@J#@.I@�@�@�2@��@��@c @	@�@�@��@x�@+�@�@��@�e@Q�@%�@�m@��@�:@~�@b�@=@Y@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�DB��B��B�B��B��B�B�rB	�B	�B	!bB	#nB	$�B	&LB	&�B	(�B	-)B	/OB	1�B	9�B	DB	C-B	?�B	<6B	9�B	7�B	7fB	8RB	9�B	9>B	9>B	9�B	:DB	:�B	:�B	;�B	<�B	=�B	>�B	?cB	BB	F�B	LB	PHB	S�B	WsB	l�B	�KB	��B
�B
�}B
�	B�B)B+�B$tB[WBc�B_�BZ�BQ�BSuBMBJXBB'B8B4�B>BBJ�B@�B0;B"NB#ByB}B�B
��B
��B
ݲB
��B
�B
oOB
B�B
)�B	��B	�hB	ңB	�B	��B	wB	s�B	h�B	M�B	2GB	+kB	%�B	(XB	'�B	�B	B	mB�GB��B�'BߊB�EB�"B��B��B��B��B��B��B��B�TB��B�OB�tB��B�fB�KBѷB׍B�QB	B		B	!bB	'8B	+�B	&LB	!�B	"�B	*B	%�B	�B	hB	�B	�B	B	AB	�B	%,B	.}B	+�B	�B	vB	�B	�B	B	&LB	#�B	�B	)�B	'�B	$@B	!�B	pB	�B	�B	$�B	.�B	:^B	^�B	e`B	\]B	P.B	@iB	;�B	;�B	?}B	CB	P.B	Q�B	Q�B	U�B	V�B	PB	U�B	[�B	nIB	r�B	w�B	~�B	x�B	�B	�B	�^B	�B	��B	�0B	��B	��B	�]B	��B	�8B	�\B	�
B	��B	��B	�xB	�B	�<B	��B	��B	��B	��B	�$B	��B	�mB	��B	��B	��B	��B	��B	��B	�aB	ǮB	��B	�1B	�RB	ȴB	�)B	ΊB	�^B	�=B	��B	ϫB	�bB	�B	�^B	�[B	��B	�2B	�B	��B	�B	�sB	��B	�2B	�B	�CB	��B	��B	�|B	�9B	�+B	�fB	��B	��B	��B	�^B	�B	��B	��B	�B	�B	�B	��B	��B	��B	�fB	�KB	ȚB	�B	�7B	�lB	�~B	�dB	�JB	�0B	̘B	�B	͟B	��B	бB	�NB	ҽB	��B	ӏB	�@B	�[B	�@B	��B	��B	ԕB	�
B	�$B	רB	רB	��B	��B	��B	چB	�B	�QB	ܒB	�B	�/B	�/B	�B	��B	�/B	��B	��B	��B	ܒB	�IB	�dB	�jB	ߊB	�VB	�VB	ߊB	ߊB	߾B	�B	�B	�B	�B	�B	�nB	�nB	�@B	�@B	�tB	�B	�B	��B	�B	�LB	�LB	�B	�>B	�B	�6B	�qB	�qB	�qB	�B	�"B	�B	�CB	��B	�B	�5B	�B	�B	��B	�TB	��B	��B	�MB	��B	�B	��B	��B	�$B	��B	�>B	��B	��B	�xB	��B	��B	��B	�rB	��B	�	B	��B	�B	�<B	�qB	��B	�B
 iB
;B
  B	�B	�.B	�cB	�VB	�<B	��B	��B	�B	��B	��B	��B	��B	�]B	��B	��B
�B
oB
oB
 B
 B
AB
uB
�B
B
�B
�B
SB
�B
	lB
	lB

rB
�B
BB
\B
\B
.B
NB
4B
NB
NB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
 B
�B
B
B
�B
4B
�B
�B
�B
�B
�B
\B
�B
�B
�B
�B
�B
B
B
�B
�B
6B
6B
PB
PB
�B
,B
gB
�B
�B
B
B
9B
�B
�B
�B
�B
�B
�B
qB
WB
�B
xB
�B
�B
;B
 B
�B
�B
 �B
 �B
 'B
 B
 B
�B
 B
�B
 \B
 �B
!�B
"�B
"�B
#nB
# B
#nB
#�B
#�B
#�B
#�B
$B
$tB
$�B
$�B
$�B
$�B
%FB
%�B
&B
&LB
&fB
&�B
&�B
'B
'�B
($B
(�B
(�B
(XB
($B
($B
(�B
)B
)*B
)�B
+B
+�B
+�B
+�B
+�B
+�B
+QB
+�B
+�B
,"B
,=B
,�B
,�B
,�B
-CB
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.cB
./B
.IB
.�B
.�B
/B
/5B
/ B
/�B
/�B
0;B
0UB
0oB
0UB
0�B
0�B
0�B
1'B
2B
1�B
2B
2�B
2�B
2�B
2�B
2�B
3MB
2�B
2aB
2�B
3hB
3�B
4nB
5?B
5tB
5B
5%B
5%B
5B
5�B
5�B
5%B
4�B
6+B
5�B
5%B
4�B
3�B
4�B
6+B
5ZB
5ZB
3�B
3B
2�B
2B
2�B
2�B
4B
4�B
5ZB
4TB
2�B
2�B
2-B
2GB
2-B
2aB
2�B
3MB
49B
4�B
3�B
4B
4B
4�B
5tB
5�B
5tB
5?B
5�B
6�B
88B
88B
7�B
9�B
9�B
9$B
9�B
9�B
9�B
:DB
:DB
:xB
;dB
;�B
;�B
;B
;B
="B
<�B
=�B
=�B
<�B
;�B
;�B
;B
;�B
<B
<jB
<jB
<�B
<�B
<�B
=�B
>�B
>�B
?}B
?�B
@�B
BAB
B'B
A�B
A�B
BB
BB
@�B
@4B
AUB
BB
A�B
AUB
@�B
AB
AUB
AUB
A�B
B�B
CB
CB
B�B
DB
EB
F�B
G�B
HfB
H�B
H�B
H�B
I7B
IRB
I�B
J#B
J=B
J#B
JrB
J�B
J�B
KxB
LdB
L�B
L�B
MB
L�B
L�B
MB
M�B
M�B
M�B
MjB
M6B
L�B
L�B
L�B
MB
L�B
MB
MjB
NB
N"B
NpB
N�B
OB
O�B
PbB
P�B
Q4B
R B
RB
S@B
UgB
U�B
VB
V9B
V�B
W$B
W�B
YeB
Y�B
ZkB
[#B
[	B
Z�B
ZkB
Z�B
[WB
[�B
[�B
\�B
\xB
\�B
]IB
]IB
]dB
]�B
^OB
_�B
a-B
a�B
cB
c�B
dB
d@B
d@B
dtB
dZB
d�B
d�B
d�B
eB
eFB
eFB
e,B
eB
d�B
eFB
e`B
e`B
e�B
fB
f�B
f�B
f�B
ffB
ffB
f�B
g8B
gRB
gmB
g8B
gB
f�B
gB
g�B
h>B
h�B
i*B
i*B
iyB
i�B
jB
jB
jeB
jeB
jB
jeB
jB
j�B
j�B
kB
kB
kB
kQB
kkB
k�B
k�B
k�B
k�B
k�B
l=B
l�B
l�B
l�B
lqB
lqB
lqB
l�B
l�B
m)B
m�B
mwB
m�B
n}B
n}B
oB
o5B
oOB
o�B
oiB
o�B
o�B
p!B
pUB
poB
p�B
qB
qAB
qvB
q�B
r-B
raB
r|B
r|B
raB
raB
r|B
r�B
r�B
r�B
r�B
sB
s3B
sB
shB
s�B
t9B
tTB
t�B
t�B
tnB
t�B
uB
u%B
uB
u?B
uZB
u�B
v+B
vB
vB
vFB
vFB
v`B
v�B
v�B
wLB
wfB
wLB
wLB
wLB
w�B
w�B
xB
w�B
x�B
xlB
x�B
x�B
y	B
y>B
yrB
yrB
yrB
yrB
y�B
z*B
zDB
zDB
z^B
z�B
z�B
{B
{0B
{JB
{B
{�B
{�B
|6B
|B
|PB
|jB
|�B
|�B
}B
}<B
}VB
}qB
}�B
}�B
}�B
}�B
~B
~(B
~B
~BB
~�B
~�B
~�B
~�B
B
B
HB
�B
�B
�B
�B
�4B
�OB
�OB
�iB
��B
��B
��B
�B
� B
� B
�;B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�B
�-B
�aB
�{B
��B
��B
��B
��B
�B
�3B
�MB
��B
��B
��B
��B
��B
�B
�9B
�B
�9B
��B
��B
��B
�%B
�?B
��B
��B
�B
�+B
�zB
�zB
��B
��B
��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�DB��B��B�B��B��B�B�rB	�B	�B	!bB	#nB	$�B	&LB	&�B	(�B	-)B	/OB	1�B	9�B	DB	C-B	?�B	<6B	9�B	7�B	7fB	8RB	9�B	9>B	9>B	9�B	:DB	:�B	:�B	;�B	<�B	=�B	>�B	?cB	BB	F�B	LB	PHB	S�B	WsB	l�B	�KB	��B
�B
�}B
�	B�B)B+�B$tB[WBc�B_�BZ�BQ�BSuBMBJXBB'B8B4�B>BBJ�B@�B0;B"NB#ByB}B�B
��B
��B
ݲB
��B
�B
oOB
B�B
)�B	��B	�hB	ңB	�B	��B	wB	s�B	h�B	M�B	2GB	+kB	%�B	(XB	'�B	�B	B	mB�GB��B�'BߊB�EB�"B��B��B��B��B��B��B��B�TB��B�OB�tB��B�fB�KBѷB׍B�QB	B		B	!bB	'8B	+�B	&LB	!�B	"�B	*B	%�B	�B	hB	�B	�B	B	AB	�B	%,B	.}B	+�B	�B	vB	�B	�B	B	&LB	#�B	�B	)�B	'�B	$@B	!�B	pB	�B	�B	$�B	.�B	:^B	^�B	e`B	\]B	P.B	@iB	;�B	;�B	?}B	CB	P.B	Q�B	Q�B	U�B	V�B	PB	U�B	[�B	nIB	r�B	w�B	~�B	x�B	�B	�B	�^B	�B	��B	�0B	��B	��B	�]B	��B	�8B	�\B	�
B	��B	��B	�xB	�B	�<B	��B	��B	��B	��B	�$B	��B	�mB	��B	��B	��B	��B	��B	��B	�aB	ǮB	��B	�1B	�RB	ȴB	�)B	ΊB	�^B	�=B	��B	ϫB	�bB	�B	�^B	�[B	��B	�2B	�B	��B	�B	�sB	��B	�2B	�B	�CB	��B	��B	�|B	�9B	�+B	�fB	��B	��B	��B	�^B	�B	��B	��B	�B	�B	�B	��B	��B	��B	�fB	�KB	ȚB	�B	�7B	�lB	�~B	�dB	�JB	�0B	̘B	�B	͟B	��B	бB	�NB	ҽB	��B	ӏB	�@B	�[B	�@B	��B	��B	ԕB	�
B	�$B	רB	רB	��B	��B	��B	چB	�B	�QB	ܒB	�B	�/B	�/B	�B	��B	�/B	��B	��B	��B	ܒB	�IB	�dB	�jB	ߊB	�VB	�VB	ߊB	ߊB	߾B	�B	�B	�B	�B	�B	�nB	�nB	�@B	�@B	�tB	�B	�B	��B	�B	�LB	�LB	�B	�>B	�B	�6B	�qB	�qB	�qB	�B	�"B	�B	�CB	��B	�B	�5B	�B	�B	��B	�TB	��B	��B	�MB	��B	�B	��B	��B	�$B	��B	�>B	��B	��B	�xB	��B	��B	��B	�rB	��B	�	B	��B	�B	�<B	�qB	��B	�B
 iB
;B
  B	�B	�.B	�cB	�VB	�<B	��B	��B	�B	��B	��B	��B	��B	�]B	��B	��B
�B
oB
oB
 B
 B
AB
uB
�B
B
�B
�B
SB
�B
	lB
	lB

rB
�B
BB
\B
\B
.B
NB
4B
NB
NB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
 B
�B
B
B
�B
4B
�B
�B
�B
�B
�B
\B
�B
�B
�B
�B
�B
B
B
�B
�B
6B
6B
PB
PB
�B
,B
gB
�B
�B
B
B
9B
�B
�B
�B
�B
�B
�B
qB
WB
�B
xB
�B
�B
;B
 B
�B
�B
 �B
 �B
 'B
 B
 B
�B
 B
�B
 \B
 �B
!�B
"�B
"�B
#nB
# B
#nB
#�B
#�B
#�B
#�B
$B
$tB
$�B
$�B
$�B
$�B
%FB
%�B
&B
&LB
&fB
&�B
&�B
'B
'�B
($B
(�B
(�B
(XB
($B
($B
(�B
)B
)*B
)�B
+B
+�B
+�B
+�B
+�B
+�B
+QB
+�B
+�B
,"B
,=B
,�B
,�B
,�B
-CB
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.cB
./B
.IB
.�B
.�B
/B
/5B
/ B
/�B
/�B
0;B
0UB
0oB
0UB
0�B
0�B
0�B
1'B
2B
1�B
2B
2�B
2�B
2�B
2�B
2�B
3MB
2�B
2aB
2�B
3hB
3�B
4nB
5?B
5tB
5B
5%B
5%B
5B
5�B
5�B
5%B
4�B
6+B
5�B
5%B
4�B
3�B
4�B
6+B
5ZB
5ZB
3�B
3B
2�B
2B
2�B
2�B
4B
4�B
5ZB
4TB
2�B
2�B
2-B
2GB
2-B
2aB
2�B
3MB
49B
4�B
3�B
4B
4B
4�B
5tB
5�B
5tB
5?B
5�B
6�B
88B
88B
7�B
9�B
9�B
9$B
9�B
9�B
9�B
:DB
:DB
:xB
;dB
;�B
;�B
;B
;B
="B
<�B
=�B
=�B
<�B
;�B
;�B
;B
;�B
<B
<jB
<jB
<�B
<�B
<�B
=�B
>�B
>�B
?}B
?�B
@�B
BAB
B'B
A�B
A�B
BB
BB
@�B
@4B
AUB
BB
A�B
AUB
@�B
AB
AUB
AUB
A�B
B�B
CB
CB
B�B
DB
EB
F�B
G�B
HfB
H�B
H�B
H�B
I7B
IRB
I�B
J#B
J=B
J#B
JrB
J�B
J�B
KxB
LdB
L�B
L�B
MB
L�B
L�B
MB
M�B
M�B
M�B
MjB
M6B
L�B
L�B
L�B
MB
L�B
MB
MjB
NB
N"B
NpB
N�B
OB
O�B
PbB
P�B
Q4B
R B
RB
S@B
UgB
U�B
VB
V9B
V�B
W$B
W�B
YeB
Y�B
ZkB
[#B
[	B
Z�B
ZkB
Z�B
[WB
[�B
[�B
\�B
\xB
\�B
]IB
]IB
]dB
]�B
^OB
_�B
a-B
a�B
cB
c�B
dB
d@B
d@B
dtB
dZB
d�B
d�B
d�B
eB
eFB
eFB
e,B
eB
d�B
eFB
e`B
e`B
e�B
fB
f�B
f�B
f�B
ffB
ffB
f�B
g8B
gRB
gmB
g8B
gB
f�B
gB
g�B
h>B
h�B
i*B
i*B
iyB
i�B
jB
jB
jeB
jeB
jB
jeB
jB
j�B
j�B
kB
kB
kB
kQB
kkB
k�B
k�B
k�B
k�B
k�B
l=B
l�B
l�B
l�B
lqB
lqB
lqB
l�B
l�B
m)B
m�B
mwB
m�B
n}B
n}B
oB
o5B
oOB
o�B
oiB
o�B
o�B
p!B
pUB
poB
p�B
qB
qAB
qvB
q�B
r-B
raB
r|B
r|B
raB
raB
r|B
r�B
r�B
r�B
r�B
sB
s3B
sB
shB
s�B
t9B
tTB
t�B
t�B
tnB
t�B
uB
u%B
uB
u?B
uZB
u�B
v+B
vB
vB
vFB
vFB
v`B
v�B
v�B
wLB
wfB
wLB
wLB
wLB
w�B
w�B
xB
w�B
x�B
xlB
x�B
x�B
y	B
y>B
yrB
yrB
yrB
yrB
y�B
z*B
zDB
zDB
z^B
z�B
z�B
{B
{0B
{JB
{B
{�B
{�B
|6B
|B
|PB
|jB
|�B
|�B
}B
}<B
}VB
}qB
}�B
}�B
}�B
}�B
~B
~(B
~B
~BB
~�B
~�B
~�B
~�B
B
B
HB
�B
�B
�B
�B
�4B
�OB
�OB
�iB
��B
��B
��B
�B
� B
� B
�;B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�B
�-B
�aB
�{B
��B
��B
��B
��B
�B
�3B
�MB
��B
��B
��B
��B
��B
�B
�9B
�B
�9B
��B
��B
��B
�%B
�?B
��B
��B
�B
�+B
�zB
�zB
��B
��B
��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230310034434  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230310034448  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230310034448  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230310034449                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230310034449  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230310034449  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230310040100                      G�O�G�O�G�O�                