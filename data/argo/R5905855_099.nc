CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:28:17Z creation;2022-06-04T19:28:18Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604192817  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               cA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ٞE~{�v1   @ٞF��˪@,�33333�d(1&�x�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A>ffA`  A���A�  A�  A�  A�  A���A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B���B���B���C   C  C�fC  C  C
  C  C33C�fC�fC  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<L�C=�fC?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj33Ck�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}y�D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @\)@u@��H@��HAp�A;�
A]p�A
>A��RA��RA��RA��RAυA߅A�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��B��GB��B��B��B��B��B��B�G�B�z�B��B��B��B��BîB�zBˮBϮBӮB׮BۮB߮B�B��GB��GB��GB�z�B�z�B�z�B��C�
C�pC�
C�
C	�
C�
C
=C�pC�pC�
C�
C�
C�
C�pC�
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
C<#�C=�pC?�pCA�
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
Cg�Cj
=Ck�pCm�
Co�
Cq�
Cs�
Cu�
Cw�
Cy�
C{�
C}�
C�
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��D|)D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%�)D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL�)DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}o]D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D��D�:�D�z�D���D���D�:�D�w�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�>D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�~D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�d{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�VA�VA��A��A�	lA�_A��A�A��A���A�� A�ԕA���Aد�Aج�Aث�Aة�AءbA؝�AزaA�ĜAش9A؇�A�t�A�bNA�;�A��A��BA׻0A׫�AףnAך�AחYAהFAא�A׋xA׆�A�~�A�y�A�q�A�EmAֻ�A���A�oiA��A͍A�m)A���A�ޞA�F?Aȼ�A�IRAį�A!A���A�-wA��aA���A��aA�kQA���A���A��jA�c�A���A�Z�A�iA�qvA���A��A��A��LA�}�A��A��|A�	A�|�A��A�f�A��A��A�Q�A�w�A���A��-A�m]A�'�A�L�A���A�/�A�OvA�;�A�OA��aA��YA��)A�GA��8A�ߤA���A��A�~]A��A}�Az7LAxe,AsAo�AhߤAezA`�eAZ�{AT~AP��AMX�AJ!�AE.�AB��A@PHA>��A>A=��A<��A9��A8��A8W?A8 iA7�A6��A3VmA0�PA/��A.��A.eA+��A)��A&8�A$6�A#��A#�-A$�A%8�A%�9A%�6A%�	A&A%�A%JA%C-A#��A#JA"��A"�A#%�A"�]A"�A#uA#a|A"�A"�A"�A!r�A!4A �fA t�Ao�A��AĜASAdZA�MA�A�xA�]A��A0UAp;A�AAG�A��A��AW�A�[AqvA�A��A�Ag8A�A��A�CA�A��A�qA��AMjA��A{JAOA�.A-wAc�A�9A?AzxAFA�AA?A
��A
y>A
�A
W?A	�A	��A	GEA�AE�A�{A�oAɆA#:Au�A�$Ax�A�A�uA��AjA!�A�A��A33A ��A L�@��@�0U@��@@���@�s@�d�@�A @��~@���@�\)@�33@��@��K@���@�8�@���@�C@��@�.I@��o@�@�Vm@��@�@�$@�j@�8@���@��.@� \@�ff@�+@�3�@���@�k�@��@�A @��c@�7�@��@�V@�ƨ@�u�@�;@�ϫ@��@�h
@�@@��@ථ@���@��'@�V�@ݜ@��@ܮ}@�"h@�
=@���@�q@���@�-�@�[W@��@֢4@ָR@֘_@��/@�K^@��;@���@��@Ԓ�@��@��@ӨX@��m@�,=@��a@фM@��@�L0@��@ϟV@�8�@ή}@���@�Mj@�S&@Λ�@ͥ@�N<@�G�@��@́o@��@˭C@�b�@ʣ�@�M@ɜ@��]@�`�@Ǚ�@�E9@ƻ�@��6@Ŋ	@�k�@�)_@���@�d�@���@ð�@�rG@�C@¬�@�J�@�~@��@�N<@���@�Z@��T@���@�L�@�+�@��s@�Q@���@�L�@���@��@�'�@���@���@���@�H�@��@�qv@��!@�u@��h@���@�|�@�G@���@�<6@���@�e�@�1'@� �@�G@���@�e,@�"�@��@�Ĝ@��@�0U@��@���@�|�@��@�g�@��@���@��m@�M�@��@��@��}@�2�@��@�/�@�ѷ@��6@�m�@���@�l�@��@�xl@�a|@�~@��N@���@�8�@��F@�?@��@�ݘ@�C�@�͟@�e@��9@���@��s@�I�@�خ@�{J@�!�@���@���@�w�@�.�@���@��t@���@�Dg@��@���@���@���@���@�i�@�*�@���@�!�@���@�u%@�:*@��@���@��@��@��4@�Ft@��]@���@���@�s@�?}@��@��L@�n�@�4n@��@��a@��=@�_p@�C�@�#�@���@���@�oi@�I�@�{@��@��@���@���@���@�zx@�a@�J#@�)_@��@���@�K^@��[@��@�<6@���@�ی@�r�@�?@��@��@���@�F@��@��2@�ی@��h@��@���@��~@�zx@�]�@�Dg@�!�@��P@���@���@�?�@��m@�f�@�5�@��@��E@��!@��u@�r�@�Q�@��#@���@�8@�m�@�,=@��@���@��[@�e,@�^�@�Y�@�F�@���@�q@�7�@�{@�_@��r@�ݘ@��z@���@�dZ@��@���@���@�R�@��@���@�`B@�6z@��@��`@��o@�@���@�E9@��@��m@��D@�Xy@�5?@�+k@�ݘ@���@�L�@��@���@�W�@�4@�q@�@~p;@}ϫ@}Y�@}�@|w�@{�&@{]�@z6�@y=�@xĜ@x��@x4n@w�$@w i@v��@v0U@u�d@uDg@t�?@t2�@s��@sY@r�F@r.�@qm]@p�	@p��@o�&@n��@n�1@n?@m�S@ma�@l��@l?�@k�&@kn/@j�@jp;@i��@i��@i�@i��@i��@iu�@i�@h��@h9X@g��@g_p@fں@fz@f;�@fe@e�@eA @d�f@d��@d|�@dZ@c��@c��@c/�@b�@b�s@b�<@bl�@bL0@b1�@a��@`��@`|�@`S�@`!@_خ@_�w@_��@_��@_�@@_��@_O@^�@^z@^u@]@]�7@]N<@]�@\�@\�o@\M@\�@[��@[X�@[,�@Z��@ZJ@Y��@Y�S@X�O@Xg8@XU2@XQ�@X6@W��@Wn/@V�X@V-@U�9@U��@Uc@T��@S��@S��@R��@R}V@RW�@Q��@Q�@Q�C@QIR@P��@Pz�@Pj@P<�@O��@O�k@O'�@N��@NGE@M��@MA @K�A@J��@JV@J=q@J!�@Ju@I��@IJ�@H�?@H�Y@H1@G�*@G~�@Gj�@G.I@F�c@E�j@E\�@E+@D��@D2�@C�;@C��@C��@C�k@C]�@C&@B��@B#:@A�^@A�@@z�@@/�@?��@?4�@>�]@>kQ@>�@=��@=A @<ی@<�@<	�@;�F@;�@;g�@;>�@:�@:�b@:��@:�1@:��@:v�@:=q@9�>@9�^@9��@9Q�@8�@8��@8r�@8H@8M@7��@7��@7S�@7)_@7@6ȴ@6��@6e@5�^@5��@5@4��@4�4@4��@4q@41'@4�@3�+@3�*@3{J@3
=@2�\@2xl@2n�@23�@1�@1��@1e,@1%F@0�[@0�@0M@0>B@0�@/�r@/�@/�
@/��@/~�@/b�@/$t@.ں@.��@.��@.n�@-��@-�3@-��@-��@-Dg@-�@,��@,��@,|�@,/�@,M@,�@+�@+�@+��@+�@*��@*GE@)��@)Dg@(�	@(�p@(w�@(Q�@(*�@'�r@'�@'ݘ@'��@'��@'��@'X�@'H�@';d@'Y@'�@&��@&�@%��@%0�@%@@$�I@$bN@$I�@$2�@$2�@$4n@$6@$7�@$9X@$6@$  @#��@#�*@#�V@#v`@#1�@"ȴ@"3�@!��@!�@!F@ ��@ �@ �E@ �$@ ��@ 7�@˒@RT@�@�@8�@�@��@f�@�?@]d@7�@<�@<�@2�@�@�;@s@,�@�@��@�@��@xl@8�@�9@�X@�@|@f�@Dg@%@�I@?�@�@�a@��@��@�:@t�@C�@33@)_@o@�M@�B@��@M�@$�@#:@!�@J@��@��@�#@��@�@m]@IR@�f@�e@h�@A�@~@��@~�@�@�r@YK@($@	@J@��@f�@�@�@��@�U@tT@<�@@�@� @�@��@�0@��@��@�4@(@�@��@��@�A@Ov@;�@($@�@�.@�j@�j@�T@�@u�@X@S&@*0@��@��@֡@�z@tT@tT@>B@�Q@��@j�@C�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�VA�VA��A��A�	lA�_A��A�A��A���A�� A�ԕA���Aد�Aج�Aث�Aة�AءbA؝�AزaA�ĜAش9A؇�A�t�A�bNA�;�A��A��BA׻0A׫�AףnAך�AחYAהFAא�A׋xA׆�A�~�A�y�A�q�A�EmAֻ�A���A�oiA��A͍A�m)A���A�ޞA�F?Aȼ�A�IRAį�A!A���A�-wA��aA���A��aA�kQA���A���A��jA�c�A���A�Z�A�iA�qvA���A��A��A��LA�}�A��A��|A�	A�|�A��A�f�A��A��A�Q�A�w�A���A��-A�m]A�'�A�L�A���A�/�A�OvA�;�A�OA��aA��YA��)A�GA��8A�ߤA���A��A�~]A��A}�Az7LAxe,AsAo�AhߤAezA`�eAZ�{AT~AP��AMX�AJ!�AE.�AB��A@PHA>��A>A=��A<��A9��A8��A8W?A8 iA7�A6��A3VmA0�PA/��A.��A.eA+��A)��A&8�A$6�A#��A#�-A$�A%8�A%�9A%�6A%�	A&A%�A%JA%C-A#��A#JA"��A"�A#%�A"�]A"�A#uA#a|A"�A"�A"�A!r�A!4A �fA t�Ao�A��AĜASAdZA�MA�A�xA�]A��A0UAp;A�AAG�A��A��AW�A�[AqvA�A��A�Ag8A�A��A�CA�A��A�qA��AMjA��A{JAOA�.A-wAc�A�9A?AzxAFA�AA?A
��A
y>A
�A
W?A	�A	��A	GEA�AE�A�{A�oAɆA#:Au�A�$Ax�A�A�uA��AjA!�A�A��A33A ��A L�@��@�0U@��@@���@�s@�d�@�A @��~@���@�\)@�33@��@��K@���@�8�@���@�C@��@�.I@��o@�@�Vm@��@�@�$@�j@�8@���@��.@� \@�ff@�+@�3�@���@�k�@��@�A @��c@�7�@��@�V@�ƨ@�u�@�;@�ϫ@��@�h
@�@@��@ථ@���@��'@�V�@ݜ@��@ܮ}@�"h@�
=@���@�q@���@�-�@�[W@��@֢4@ָR@֘_@��/@�K^@��;@���@��@Ԓ�@��@��@ӨX@��m@�,=@��a@фM@��@�L0@��@ϟV@�8�@ή}@���@�Mj@�S&@Λ�@ͥ@�N<@�G�@��@́o@��@˭C@�b�@ʣ�@�M@ɜ@��]@�`�@Ǚ�@�E9@ƻ�@��6@Ŋ	@�k�@�)_@���@�d�@���@ð�@�rG@�C@¬�@�J�@�~@��@�N<@���@�Z@��T@���@�L�@�+�@��s@�Q@���@�L�@���@��@�'�@���@���@���@�H�@��@�qv@��!@�u@��h@���@�|�@�G@���@�<6@���@�e�@�1'@� �@�G@���@�e,@�"�@��@�Ĝ@��@�0U@��@���@�|�@��@�g�@��@���@��m@�M�@��@��@��}@�2�@��@�/�@�ѷ@��6@�m�@���@�l�@��@�xl@�a|@�~@��N@���@�8�@��F@�?@��@�ݘ@�C�@�͟@�e@��9@���@��s@�I�@�خ@�{J@�!�@���@���@�w�@�.�@���@��t@���@�Dg@��@���@���@���@���@�i�@�*�@���@�!�@���@�u%@�:*@��@���@��@��@��4@�Ft@��]@���@���@�s@�?}@��@��L@�n�@�4n@��@��a@��=@�_p@�C�@�#�@���@���@�oi@�I�@�{@��@��@���@���@���@�zx@�a@�J#@�)_@��@���@�K^@��[@��@�<6@���@�ی@�r�@�?@��@��@���@�F@��@��2@�ی@��h@��@���@��~@�zx@�]�@�Dg@�!�@��P@���@���@�?�@��m@�f�@�5�@��@��E@��!@��u@�r�@�Q�@��#@���@�8@�m�@�,=@��@���@��[@�e,@�^�@�Y�@�F�@���@�q@�7�@�{@�_@��r@�ݘ@��z@���@�dZ@��@���@���@�R�@��@���@�`B@�6z@��@��`@��o@�@���@�E9@��@��m@��D@�Xy@�5?@�+k@�ݘ@���@�L�@��@���@�W�@�4@�q@�@~p;@}ϫ@}Y�@}�@|w�@{�&@{]�@z6�@y=�@xĜ@x��@x4n@w�$@w i@v��@v0U@u�d@uDg@t�?@t2�@s��@sY@r�F@r.�@qm]@p�	@p��@o�&@n��@n�1@n?@m�S@ma�@l��@l?�@k�&@kn/@j�@jp;@i��@i��@i�@i��@i��@iu�@i�@h��@h9X@g��@g_p@fں@fz@f;�@fe@e�@eA @d�f@d��@d|�@dZ@c��@c��@c/�@b�@b�s@b�<@bl�@bL0@b1�@a��@`��@`|�@`S�@`!@_خ@_�w@_��@_��@_�@@_��@_O@^�@^z@^u@]@]�7@]N<@]�@\�@\�o@\M@\�@[��@[X�@[,�@Z��@ZJ@Y��@Y�S@X�O@Xg8@XU2@XQ�@X6@W��@Wn/@V�X@V-@U�9@U��@Uc@T��@S��@S��@R��@R}V@RW�@Q��@Q�@Q�C@QIR@P��@Pz�@Pj@P<�@O��@O�k@O'�@N��@NGE@M��@MA @K�A@J��@JV@J=q@J!�@Ju@I��@IJ�@H�?@H�Y@H1@G�*@G~�@Gj�@G.I@F�c@E�j@E\�@E+@D��@D2�@C�;@C��@C��@C�k@C]�@C&@B��@B#:@A�^@A�@@z�@@/�@?��@?4�@>�]@>kQ@>�@=��@=A @<ی@<�@<	�@;�F@;�@;g�@;>�@:�@:�b@:��@:�1@:��@:v�@:=q@9�>@9�^@9��@9Q�@8�@8��@8r�@8H@8M@7��@7��@7S�@7)_@7@6ȴ@6��@6e@5�^@5��@5@4��@4�4@4��@4q@41'@4�@3�+@3�*@3{J@3
=@2�\@2xl@2n�@23�@1�@1��@1e,@1%F@0�[@0�@0M@0>B@0�@/�r@/�@/�
@/��@/~�@/b�@/$t@.ں@.��@.��@.n�@-��@-�3@-��@-��@-Dg@-�@,��@,��@,|�@,/�@,M@,�@+�@+�@+��@+�@*��@*GE@)��@)Dg@(�	@(�p@(w�@(Q�@(*�@'�r@'�@'ݘ@'��@'��@'��@'X�@'H�@';d@'Y@'�@&��@&�@%��@%0�@%@@$�I@$bN@$I�@$2�@$2�@$4n@$6@$7�@$9X@$6@$  @#��@#�*@#�V@#v`@#1�@"ȴ@"3�@!��@!�@!F@ ��@ �@ �E@ �$@ ��@ 7�@˒@RT@�@�@8�@�@��@f�@�?@]d@7�@<�@<�@2�@�@�;@s@,�@�@��@�@��@xl@8�@�9@�X@�@|@f�@Dg@%@�I@?�@�@�a@��@��@�:@t�@C�@33@)_@o@�M@�B@��@M�@$�@#:@!�@J@��@��@�#@��@�@m]@IR@�f@�e@h�@A�@~@��@~�@�@�r@YK@($@	@J@��@f�@�@�@��@�U@tT@<�@@�@� @�@��@�0@��@��@�4@(@�@��@��@�A@Ov@;�@($@�@�.@�j@�j@�T@�@u�@X@S&@*0@��@��@֡@�z@tT@tT@>B@�Q@��@j�@C�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	:*B	:xB	:^B	:DB	:�B	:xB	:�B	:�B	:�B	;�B	<�B	=qB	?.B	CGB	E9B	HfB	LJB	QNB	Z�B	�@B	֡B	�B
$@B
AUB
V�B
��B
�kB
�)B
�B
��B
��B
�B
�B
�B
�UB
�B
�!B
�B
�UB
�OB
��B
��B
�aB
�]BgB
�B
�RB
�RB�B�B�BH�BpB�B��B�B�BЗBڠB�BӏBݲB�B�B�^B�BB��B��B �B�*B��B�'B�fB��B�B��B�B��B�aB��B�LB��B�zB�aB�0B��B�7B~�Bn}B[#B1'B�B�B
�tB
��B
�wB
�eB
�B
�SB
tB
WsB
G�B
D�B
4B
�B
�B	�NB	�B	��B	�_B	k6B	N�B	6zB	%�B	+B	�B	B	�B	�B	aB	%B	�B	�B	B	VB	B	�B	$B	�B	�B	pB		�B	�B	 �B�B�B�=B� B��B��B��B	SB	C�B	abB	k�B	p�B	wB	{�B	��B	�B	��B	��B	��B	�qB	οB	ևB	��B	��B	��B	��B	��B
�B
B
1B
	lB	�}B	��B	�xB	��B	��B	�nB	�B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�qB	�B	�B	��B	�B	�B	�qB	�B	��B	�B	�2B
B
bB
�B
WB

B
�B
�B
uB
7B
�B
_B
�B
B
1B
�B
B
�B
�B
dB
5B
)�B
+QB
%�B
&�B
(�B
'�B
$@B
"�B
 \B
$tB
 �B
B
B
�B
�B
B
�B
�B
�B
jB
B
QB
�B
uB
HB
�B
�B
B
dB
�B
9B
�B
�B
�B
]B
]B
xB
)B
�B
�B
�B
�B
�B
�B
hB
�B
�B
�B
�B
�B
�B
�B
<B
)B
�B
9B
{B
B
�B
;B
 �B
�B
�B
�B
tB
3B
�B
�B
1B
�B
�B
GB
-B
 OB
{B
AB
;B	��B	�cB	��B	�.B	��B	��B	��B	�$B	�	B	�B	��B	�XB	��B	�BB
�B
�B
uB
aB
{B
{B
�B
[B
�B	��B	�qB	�jB	�6B	��B	�B	��B	�XB	��B	�B	�JB	�B
 OB
 �B
 �B	��B	�HB	�B	��B	�<B	��B	��B	�XB	��B	��B	��B	�PB	�B	��B	��B	��B	��B	��B	�qB	��B	�.B	�B	��B	�cB	��B	��B	��B	��B
 B
 B
 4B
  B
 iB
�B
uB
�B
uB
�B
 �B
 OB
 OB
UB
�B
�B
�B
�B
[B
�B
'B
�B
�B
SB
�B
B
�B
B
�B
�B
%B
%B
%B
%B
YB
�B
�B
+B
+B
zB
�B
_B
_B
�B
B
+B
EB
B
�B
zB
�B
�B
�B
	�B
	lB
	B
�B
	B
	B
	�B
	�B

#B

�B

rB

�B

�B

�B
xB
dB
~B
�B
�B
�B
"B
B
B
BB
}B
NB
�B
TB
�B
[B
�B
uB
B
�B
�B
�B
�B
B
B
B
SB
mB
9B
�B
YB
�B
_B
_B
�B
�B
yB
�B
�B
B
�B
�B
�B
�B
B
1B
KB
�B
�B
B
B
kB
�B
	B
	B
#B
qB
�B
�B
)B
xB
xB
�B
�B
�B
�B
B
/B
/B
dB
�B
dB
B
�B
�B
!B
VB
;B
 B
�B
 'B
 B
�B
 �B
 �B
 �B
 �B
 �B
!�B
"NB
"hB
"NB
"hB
"�B
"�B
#:B
#�B
#�B
$B
$&B
$�B
$�B
%FB
%zB
%�B
%�B
%�B
%�B
&�B
&2B
'B
($B
'�B
(sB
(�B
)*B
*B
*KB
*0B
*0B
+B
+B
+B
+6B
+�B
+�B
,B
,B
,"B
,�B
-B
-]B
-�B
.�B
/�B
/�B
/�B
/�B
0;B
0UB
1�B
3B
2�B
2aB
2�B
3B
3B
33B
3�B
5B
6B
5�B
5�B
5�B
6zB
6�B
7�B
88B
8�B
8�B
9XB
9�B
9�B
:B
:DB
:*B
;dB
;�B
;�B
;�B
<6B
<�B
<�B
<�B
=VB
=�B
=�B
>BB
>�B
>�B
?B
?HB
?}B
?�B
@B
@B
@�B
AB
A;B
A;B
A�B
A�B
A�B
BAB
BuB
B�B
B�B
CGB
C�B
C�B
C�B
C�B
DB
D3B
DMB
D�B
D�B
EB
E�B
E�B
E�B
F%B
F�B
GzB
G�B
G�B
HB
H1B
H1B
HfB
HfB
H�B
H�B
H�B
H�B
IRB
IlB
IRB
I�B
J�B
J�B
J�B
K)B
K^B
K^B
K^B
K^B
KDB
K)B
K)B
K)B
KDB
K�B
K�B
K�B
L0B
LdB
LdB
LdB
LdB
L�B
L�B
L�B
L�B
MB
MB
L�B
MB
MB
L�B
L�B
L�B
L�B
L�B
MPB
MPB
MjB
M�B
M�B
M�B
N<B
N"B
M�B
N�B
OBB
O�B
P}B
PHB
PHB
P�B
QhB
Q�B
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
R B
Q�B
RTB
R�B
R�B
R�B
R�B
S@B
SuB
SuB
T,B
TFB
TFB
UB
UMB
UMB
U�B
VmB
W$B
W$B
W?B
WYB
W�B
W�B
X+B
XEB
XyB
X�B
X�B
X_B
W�B
W�B
W�B
XB
W�B
X_B
XyB
X�B
YB
YeB
Y�B
ZB
ZB
ZB
Z�B
[#B
[#B
[WB
[qB
[�B
\)B
]~B
]�B
_;B
_�B
`\B
`BB
`�B
aB
a�B
a�B
a�B
a�B
a|B
a�B
a�B
a�B
bB
b�B
b�B
b�B
b�B
cnB
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
eB
eB
e`B
e�B
e�B
e�B
e�B
f2B
fLB
ffB
f�B
f�B
f�B
f�B
f�B
gB
gB
gB
gB
g8B
gRB
gRB
g�B
h
B
h>B
hXB
h�B
i*B
i*B
iB
h�B
iyB
i�B
i�B
i�B
jB
jeB
jB
jeB
jeB
jeB
j�B
j�B
k6B
k6B
lWB
lqB
l�B
l�B
m)B
m)B
mCB
mwB
mwB
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n/B
nB
nIB
oB
oOB
oB
o�B
o�B
pB
pB
pB
pB
pB
o�B
o�B
o�B
p;B
poB
poB
pUB
poB
poB
p!B
o�B
oB
o B
oiB
o�B
o�B
o�B
o�B
o�B
p!B
p!B
p!B
o�B
q'B
p�B
p�B
p�B
p�B
qAB
q�B
q�B
q�B
q�B
q�B
q�B
rB
r|B
r�B
r�B
r�B
sB
r�B
sB
shB
s�B
tB
t9B
tTB
tnB
t�B
t�B
utB
u�B
v+B
vzB
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wB
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xlB
x�B
x�B
x�B
y$B
y�B
z^B
z�B
z�B
z�B
z�B
{dB
{�B
|B
|B
|6B
|jB
|�B
}B
}"B
}"B
}<B
}VB
}<B
}<B
}<B
}"B
}VB
}�B
}�B
~B
~(B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
}B
�B
�B
�B
� B
� B
�B
�OB
�iB
�OB
��B
�B
�B
�UB
�U1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	:*B	:xB	:^B	:DB	:�B	:xB	:�B	:�B	:�B	;�B	<�B	=qB	?.B	CGB	E9B	HfB	LJB	QNB	Z�B	�@B	֡B	�B
$@B
AUB
V�B
��B
�kB
�)B
�B
��B
��B
�B
�B
�B
�UB
�B
�!B
�B
�UB
�OB
��B
��B
�aB
�]BgB
�B
�RB
�RB�B�B�BH�BpB�B��B�B�BЗBڠB�BӏBݲB�B�B�^B�BB��B��B �B�*B��B�'B�fB��B�B��B�B��B�aB��B�LB��B�zB�aB�0B��B�7B~�Bn}B[#B1'B�B�B
�tB
��B
�wB
�eB
�B
�SB
tB
WsB
G�B
D�B
4B
�B
�B	�NB	�B	��B	�_B	k6B	N�B	6zB	%�B	+B	�B	B	�B	�B	aB	%B	�B	�B	B	VB	B	�B	$B	�B	�B	pB		�B	�B	 �B�B�B�=B� B��B��B��B	SB	C�B	abB	k�B	p�B	wB	{�B	��B	�B	��B	��B	��B	�qB	οB	ևB	��B	��B	��B	��B	��B
�B
B
1B
	lB	�}B	��B	�xB	��B	��B	�nB	�B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�qB	�B	�B	��B	�B	�B	�qB	�B	��B	�B	�2B
B
bB
�B
WB

B
�B
�B
uB
7B
�B
_B
�B
B
1B
�B
B
�B
�B
dB
5B
)�B
+QB
%�B
&�B
(�B
'�B
$@B
"�B
 \B
$tB
 �B
B
B
�B
�B
B
�B
�B
�B
jB
B
QB
�B
uB
HB
�B
�B
B
dB
�B
9B
�B
�B
�B
]B
]B
xB
)B
�B
�B
�B
�B
�B
�B
hB
�B
�B
�B
�B
�B
�B
�B
<B
)B
�B
9B
{B
B
�B
;B
 �B
�B
�B
�B
tB
3B
�B
�B
1B
�B
�B
GB
-B
 OB
{B
AB
;B	��B	�cB	��B	�.B	��B	��B	��B	�$B	�	B	�B	��B	�XB	��B	�BB
�B
�B
uB
aB
{B
{B
�B
[B
�B	��B	�qB	�jB	�6B	��B	�B	��B	�XB	��B	�B	�JB	�B
 OB
 �B
 �B	��B	�HB	�B	��B	�<B	��B	��B	�XB	��B	��B	��B	�PB	�B	��B	��B	��B	��B	��B	�qB	��B	�.B	�B	��B	�cB	��B	��B	��B	��B
 B
 B
 4B
  B
 iB
�B
uB
�B
uB
�B
 �B
 OB
 OB
UB
�B
�B
�B
�B
[B
�B
'B
�B
�B
SB
�B
B
�B
B
�B
�B
%B
%B
%B
%B
YB
�B
�B
+B
+B
zB
�B
_B
_B
�B
B
+B
EB
B
�B
zB
�B
�B
�B
	�B
	lB
	B
�B
	B
	B
	�B
	�B

#B

�B

rB

�B

�B

�B
xB
dB
~B
�B
�B
�B
"B
B
B
BB
}B
NB
�B
TB
�B
[B
�B
uB
B
�B
�B
�B
�B
B
B
B
SB
mB
9B
�B
YB
�B
_B
_B
�B
�B
yB
�B
�B
B
�B
�B
�B
�B
B
1B
KB
�B
�B
B
B
kB
�B
	B
	B
#B
qB
�B
�B
)B
xB
xB
�B
�B
�B
�B
B
/B
/B
dB
�B
dB
B
�B
�B
!B
VB
;B
 B
�B
 'B
 B
�B
 �B
 �B
 �B
 �B
 �B
!�B
"NB
"hB
"NB
"hB
"�B
"�B
#:B
#�B
#�B
$B
$&B
$�B
$�B
%FB
%zB
%�B
%�B
%�B
%�B
&�B
&2B
'B
($B
'�B
(sB
(�B
)*B
*B
*KB
*0B
*0B
+B
+B
+B
+6B
+�B
+�B
,B
,B
,"B
,�B
-B
-]B
-�B
.�B
/�B
/�B
/�B
/�B
0;B
0UB
1�B
3B
2�B
2aB
2�B
3B
3B
33B
3�B
5B
6B
5�B
5�B
5�B
6zB
6�B
7�B
88B
8�B
8�B
9XB
9�B
9�B
:B
:DB
:*B
;dB
;�B
;�B
;�B
<6B
<�B
<�B
<�B
=VB
=�B
=�B
>BB
>�B
>�B
?B
?HB
?}B
?�B
@B
@B
@�B
AB
A;B
A;B
A�B
A�B
A�B
BAB
BuB
B�B
B�B
CGB
C�B
C�B
C�B
C�B
DB
D3B
DMB
D�B
D�B
EB
E�B
E�B
E�B
F%B
F�B
GzB
G�B
G�B
HB
H1B
H1B
HfB
HfB
H�B
H�B
H�B
H�B
IRB
IlB
IRB
I�B
J�B
J�B
J�B
K)B
K^B
K^B
K^B
K^B
KDB
K)B
K)B
K)B
KDB
K�B
K�B
K�B
L0B
LdB
LdB
LdB
LdB
L�B
L�B
L�B
L�B
MB
MB
L�B
MB
MB
L�B
L�B
L�B
L�B
L�B
MPB
MPB
MjB
M�B
M�B
M�B
N<B
N"B
M�B
N�B
OBB
O�B
P}B
PHB
PHB
P�B
QhB
Q�B
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
R B
Q�B
RTB
R�B
R�B
R�B
R�B
S@B
SuB
SuB
T,B
TFB
TFB
UB
UMB
UMB
U�B
VmB
W$B
W$B
W?B
WYB
W�B
W�B
X+B
XEB
XyB
X�B
X�B
X_B
W�B
W�B
W�B
XB
W�B
X_B
XyB
X�B
YB
YeB
Y�B
ZB
ZB
ZB
Z�B
[#B
[#B
[WB
[qB
[�B
\)B
]~B
]�B
_;B
_�B
`\B
`BB
`�B
aB
a�B
a�B
a�B
a�B
a|B
a�B
a�B
a�B
bB
b�B
b�B
b�B
b�B
cnB
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
eB
eB
e`B
e�B
e�B
e�B
e�B
f2B
fLB
ffB
f�B
f�B
f�B
f�B
f�B
gB
gB
gB
gB
g8B
gRB
gRB
g�B
h
B
h>B
hXB
h�B
i*B
i*B
iB
h�B
iyB
i�B
i�B
i�B
jB
jeB
jB
jeB
jeB
jeB
j�B
j�B
k6B
k6B
lWB
lqB
l�B
l�B
m)B
m)B
mCB
mwB
mwB
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n/B
nB
nIB
oB
oOB
oB
o�B
o�B
pB
pB
pB
pB
pB
o�B
o�B
o�B
p;B
poB
poB
pUB
poB
poB
p!B
o�B
oB
o B
oiB
o�B
o�B
o�B
o�B
o�B
p!B
p!B
p!B
o�B
q'B
p�B
p�B
p�B
p�B
qAB
q�B
q�B
q�B
q�B
q�B
q�B
rB
r|B
r�B
r�B
r�B
sB
r�B
sB
shB
s�B
tB
t9B
tTB
tnB
t�B
t�B
utB
u�B
v+B
vzB
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wB
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xlB
x�B
x�B
x�B
y$B
y�B
z^B
z�B
z�B
z�B
z�B
{dB
{�B
|B
|B
|6B
|jB
|�B
}B
}"B
}"B
}<B
}VB
}<B
}<B
}<B
}"B
}VB
}�B
}�B
~B
~(B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
}B
�B
�B
�B
� B
� B
�B
�OB
�iB
�OB
��B
�B
�B
�UB
�U1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105248  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192817  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192817  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192818                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042825  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042825  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                