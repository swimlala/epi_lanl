CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-05-09T09:42:36Z creation;2023-05-09T09:42:37Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230509094236  20230509095754  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�)Еm�1   @�)���@/�\(��b�vȴ91   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @&ff@�  @�  @���A   A>ffA^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8ffB@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�33B���B�  B�ffB���B���B�  B�ffB���B�  B�  B�  B�ffB�  B���B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C33C  C�fC  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8�fD9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA�fDB  DB� DC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�c31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @(�@u@��H@��Ap�A;�
A[�
A}p�A��RA��RA��RA��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/B7B?\)BG\)BN��BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��B��B��B��B��B��B�z�B��GB�G�B��B�zB�G�B�z�B��B�zB�z�BˮBϮBӮB�zBۮB�z�B�z�B�B�B�B�B��B��B��C�
C�
C�
C�
C	�
C�
C
=C�
C�pC�
C�
C�
C�pC�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D|)D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8|)D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DA|)DA��DBu�DB��DCu�DC��DDu�DD��DE|)DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]|)D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D�D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AƸRAƳ3AƬAƶAƻdAƾAƿA��9A��3A�A��3A��tA�ŢA���A�ΥA���A��6A�ΥA���A�ѷA�ϫA��A�бA��}A��jA��A��,A��2A���A��9A���A���A�خA��QA��WA���A��HAƂA�<jA�%AŞ�A�>A�{A�.A�*�A��mA��A��wA��RA�h�A���A�=qA��YA���A�[WA�� A�a�A�	A��\A�͟A��-A�@�A��A���A�=�A��A|l�Ax��Au��Ap�Am�+Ak�Ah�Ae��Ac��A`��A^oiAZ33AU�AS'�AQ��AP��AMO�AH �AE�AB_A=��A9)_A6kQA4��A3�A1�A/�A-aA+�1A*��A*]�A)��A(�A(m]A'�PA'P�A':�A'!�A&�#A&TaA%�A%�A$A"�A!��A!�A ��A S�A�BAA�A��AzxA�zA�.A�XAjA��A��A]�A��AAZ�A�!A�A�FA��Af�A+�A�A(�A�rA�nAbNAB�A:�A4�A�hA�yA}�AzA��A6A�A��AA A
1�A	iDA�`A�XA��AL�A��Aa�A�$A˒A�0AOA.IA?A0�A��A�A\�A�A�FA>�A�.A]dA�ZA�	A�UA �^@��3@�=@��'@��m@�zx@�	�@�Q@�o@���@�~�@��K@�Vm@�@��o@�$@�G�@��@���@�c@� i@���@@�A�@��3@�,�@�c�@�ԕ@�v`@�@O@��M@��@ꍹ@��D@��@���@�F@�Ft@��@��@思@�~(@旍@思@�Q�@�F@�'R@�!@��@�v�@�u@�iD@���@�{�@�ԕ@�/@�rG@��3@�A�@���@�w2@�Ɇ@�?�@�O@��@�A�@�j@ٵt@�&�@��p@�Z@�6@�@�%@��@Պ�@�zx@��@֦L@ׅ@�xl@�Q�@��&@�_�@�e�@�p�@��
@�/@���@�xl@ъ�@�1�@���@��@�d�@Ϭq@�֡@Ϊe@�GE@�dZ@�'�@�A�@�-�@�w2@�Ɇ@�`�@ɱ[@�;d@���@�`�@��@ǭC@�~�@�/@���@�[�@�خ@�-w@���@ć�@�	@Þ�@��@�@�Ov@�>B@��@�S&@��@��m@�ff@��@��@���@�j�@��h@�  @���@���@�V�@�&�@��@���@���@�@���@�S�@�x@��@�Ĝ@��@�Ĝ@��@�2�@��@�1�@��]@��A@�S�@��@���@��@��,@�9�@��P@�z@�#:@�!�@��@�8@��c@���@�g8@�E�@��T@�e�@�_p@�)_@���@�E�@�%�@�@�&�@��@���@�Vm@�Dg@�͟@��Y@�m�@���@��@���@��>@��F@�$t@�&@�	l@��c@�u�@��N@���@�j�@��@��@�U2@��@��@�GE@��@��Q@�t�@���@�@���@��E@�͟@���@�?@��@���@���@�'R@��X@�Y�@��@�@�_p@�b�@�B�@��@��@���@�K^@�	@�ݘ@���@��@��k@�|�@�L�@��@�֡@���@�Z�@���@�Y�@�,�@��B@��@���@�v�@�L0@�'R@��A@�Vm@��E@�i�@�&�@��.@���@���@�)_@���@�7@���@�Z�@�33@�(@��6@�=q@��@��@���@�_�@�  @�[W@�!-@��y@���@�c @�'R@��@�rG@��c@�|�@��]@���@��@��M@���@�_@�!@��@���@��@��@�o @��@�g8@�#:@��@�ݘ@�ԕ@��X@��{@�/�@��@���@�?@��@���@�A�@�V@��@��M@�ߤ@���@��'@��}@��1@�0U@��[@�|�@�f�@�A�@�:�@�!�@��@��K@�($@���@�o�@�e,@�!-@��5@��@�@�d�@�-�@��&@���@�6z@��@���@�?�@�7�@�1'@�	@��D@�@�7L@��@���@��<@��h@�l"@�b@��@��X@��=@���@�a@���@�h
@�@�$@;d@�@~�@~xl@~-@~�@}��@|q@{�A@{��@{�	@z�'@zv�@y��@yk�@yB�@x�	@x�/@x�p@x�@x_@w�@w]�@w�@v�]@v�m@v��@v��@vR�@vu@u}�@u�@t��@t~(@t-�@s�;@s�P@s�@r��@r{�@rB[@q�Z@q��@p��@oj�@nL0@n@n�@mԕ@mB�@m�@m�=@lZ@lZ@k˒@j�,@j.�@i�@i�@ix�@i?}@h�@h>B@g��@g��@g�@f�}@f�A@fYK@fe@eJ�@d��@d��@dj@dl"@d1@c@b)�@a��@a^�@a*0@a�@`��@`��@`u�@`�@_�@_�@^��@^ȴ@^�X@^}V@^W�@^Q@^�@]�#@]��@]o @]O�@]	l@]�@\��@\��@\x@[�P@[U�@[�@Z��@Z��@Z6�@Y��@YN<@Y#�@X�v@Xz�@W�r@W�@W��@Wl�@WC@V�h@V6�@U�@Uk�@U@T��@T`�@T�@S�[@SU�@R�@Rc @R#:@Q�N@Q�7@Q\�@Q�@Poi@O�Q@O~�@OW?@OY@Ni�@Nu@M�@MO�@L�K@LS�@K�[@Kb�@J�@J�+@J1�@I�Z@I�z@Io @I�@H�[@H�I@Hw�@H?�@H�@G��@Gl�@F��@F�r@F@E@E��@E:�@D��@D��@D�u@De�@D"h@D7@D�@C� @Co�@C@O@B�8@B��@B�1@Bp;@A�H@As�@@֡@@]d@@M@?�m@?��@?�:@?O@>�X@>u%@>@=��@=�-@=@<w�@<A�@<2�@</�@;�+@;��@;iD@;Y@:͟@:��@:z@9��@9��@9k�@92a@9+@9�@8��@8Z@8@7x@71�@7�@6��@6d�@5�H@5�7@5G�@4m�@3��@3s@3RT@36z@3�@2�A@2_@1�=@1��@1��@1p�@1%F@0�?@0�o@/��@/�
@/��@.�6@.��@.�+@.��@.��@.��@.Ta@-��@-��@-8�@,�j@,~(@,I�@+��@+��@+��@+|�@+l�@+
=@*��@*^5@*�@)�"@)L�@)q@(�@(�.@(I�@'�@'��@'��@'j�@'_p@'H�@&�]@&8�@%�)@%��@%u�@%�@$��@$��@$�@$,=@$!@#��@#A�@"͟@"�r@"1�@"
�@!�T@!��@!G�@!�@ �@ b@�[@t�@H�@+@��@\�@	@�>@��@^�@%F@�f@�/@�p@�@��@�@c�@1'@��@�@�@S�@�@��@v�@ff@��@�z@@�^@��@/@ \@%@ی@�@��@bN@,=@��@��@a@@�@�@�\@^5@H�@@��@��@��@f�@J�@-w@+�@%F@��@�@N�@@��@�*@�k@a@o@
=@�@S@��@�H@��@&�@�@��@�)@��@��@�M@O�@0�@��@��@��@�.@D�@9X@�@  @��@�m@˒@��@��@n/@4�@�@�6@�1@�r@{�@kQ@�@�@�t@�X@��@c�@ \@�@%@�5@��@?�@*�@$@"h@�@ݘ@�@�k@��@S�@>�@�@
�@
��@
�h@
��@
��@
��@
ff@
B[@
O@
�@	�@	�@	��@	��@	zx@	N<@	%F@		l@�|@��@��@oi@U2@?�@ �@��@ݘ@�6@��@�:@dZ@&@�@Y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AƸRAƳ3AƬAƶAƻdAƾAƿA��9A��3A�A��3A��tA�ŢA���A�ΥA���A��6A�ΥA���A�ѷA�ϫA��A�бA��}A��jA��A��,A��2A���A��9A���A���A�خA��QA��WA���A��HAƂA�<jA�%AŞ�A�>A�{A�.A�*�A��mA��A��wA��RA�h�A���A�=qA��YA���A�[WA�� A�a�A�	A��\A�͟A��-A�@�A��A���A�=�A��A|l�Ax��Au��Ap�Am�+Ak�Ah�Ae��Ac��A`��A^oiAZ33AU�AS'�AQ��AP��AMO�AH �AE�AB_A=��A9)_A6kQA4��A3�A1�A/�A-aA+�1A*��A*]�A)��A(�A(m]A'�PA'P�A':�A'!�A&�#A&TaA%�A%�A$A"�A!��A!�A ��A S�A�BAA�A��AzxA�zA�.A�XAjA��A��A]�A��AAZ�A�!A�A�FA��Af�A+�A�A(�A�rA�nAbNAB�A:�A4�A�hA�yA}�AzA��A6A�A��AA A
1�A	iDA�`A�XA��AL�A��Aa�A�$A˒A�0AOA.IA?A0�A��A�A\�A�A�FA>�A�.A]dA�ZA�	A�UA �^@��3@�=@��'@��m@�zx@�	�@�Q@�o@���@�~�@��K@�Vm@�@��o@�$@�G�@��@���@�c@� i@���@@�A�@��3@�,�@�c�@�ԕ@�v`@�@O@��M@��@ꍹ@��D@��@���@�F@�Ft@��@��@思@�~(@旍@思@�Q�@�F@�'R@�!@��@�v�@�u@�iD@���@�{�@�ԕ@�/@�rG@��3@�A�@���@�w2@�Ɇ@�?�@�O@��@�A�@�j@ٵt@�&�@��p@�Z@�6@�@�%@��@Պ�@�zx@��@֦L@ׅ@�xl@�Q�@��&@�_�@�e�@�p�@��
@�/@���@�xl@ъ�@�1�@���@��@�d�@Ϭq@�֡@Ϊe@�GE@�dZ@�'�@�A�@�-�@�w2@�Ɇ@�`�@ɱ[@�;d@���@�`�@��@ǭC@�~�@�/@���@�[�@�خ@�-w@���@ć�@�	@Þ�@��@�@�Ov@�>B@��@�S&@��@��m@�ff@��@��@���@�j�@��h@�  @���@���@�V�@�&�@��@���@���@�@���@�S�@�x@��@�Ĝ@��@�Ĝ@��@�2�@��@�1�@��]@��A@�S�@��@���@��@��,@�9�@��P@�z@�#:@�!�@��@�8@��c@���@�g8@�E�@��T@�e�@�_p@�)_@���@�E�@�%�@�@�&�@��@���@�Vm@�Dg@�͟@��Y@�m�@���@��@���@��>@��F@�$t@�&@�	l@��c@�u�@��N@���@�j�@��@��@�U2@��@��@�GE@��@��Q@�t�@���@�@���@��E@�͟@���@�?@��@���@���@�'R@��X@�Y�@��@�@�_p@�b�@�B�@��@��@���@�K^@�	@�ݘ@���@��@��k@�|�@�L�@��@�֡@���@�Z�@���@�Y�@�,�@��B@��@���@�v�@�L0@�'R@��A@�Vm@��E@�i�@�&�@��.@���@���@�)_@���@�7@���@�Z�@�33@�(@��6@�=q@��@��@���@�_�@�  @�[W@�!-@��y@���@�c @�'R@��@�rG@��c@�|�@��]@���@��@��M@���@�_@�!@��@���@��@��@�o @��@�g8@�#:@��@�ݘ@�ԕ@��X@��{@�/�@��@���@�?@��@���@�A�@�V@��@��M@�ߤ@���@��'@��}@��1@�0U@��[@�|�@�f�@�A�@�:�@�!�@��@��K@�($@���@�o�@�e,@�!-@��5@��@�@�d�@�-�@��&@���@�6z@��@���@�?�@�7�@�1'@�	@��D@�@�7L@��@���@��<@��h@�l"@�b@��@��X@��=@���@�a@���@�h
@�@�$@;d@�@~�@~xl@~-@~�@}��@|q@{�A@{��@{�	@z�'@zv�@y��@yk�@yB�@x�	@x�/@x�p@x�@x_@w�@w]�@w�@v�]@v�m@v��@v��@vR�@vu@u}�@u�@t��@t~(@t-�@s�;@s�P@s�@r��@r{�@rB[@q�Z@q��@p��@oj�@nL0@n@n�@mԕ@mB�@m�@m�=@lZ@lZ@k˒@j�,@j.�@i�@i�@ix�@i?}@h�@h>B@g��@g��@g�@f�}@f�A@fYK@fe@eJ�@d��@d��@dj@dl"@d1@c@b)�@a��@a^�@a*0@a�@`��@`��@`u�@`�@_�@_�@^��@^ȴ@^�X@^}V@^W�@^Q@^�@]�#@]��@]o @]O�@]	l@]�@\��@\��@\x@[�P@[U�@[�@Z��@Z��@Z6�@Y��@YN<@Y#�@X�v@Xz�@W�r@W�@W��@Wl�@WC@V�h@V6�@U�@Uk�@U@T��@T`�@T�@S�[@SU�@R�@Rc @R#:@Q�N@Q�7@Q\�@Q�@Poi@O�Q@O~�@OW?@OY@Ni�@Nu@M�@MO�@L�K@LS�@K�[@Kb�@J�@J�+@J1�@I�Z@I�z@Io @I�@H�[@H�I@Hw�@H?�@H�@G��@Gl�@F��@F�r@F@E@E��@E:�@D��@D��@D�u@De�@D"h@D7@D�@C� @Co�@C@O@B�8@B��@B�1@Bp;@A�H@As�@@֡@@]d@@M@?�m@?��@?�:@?O@>�X@>u%@>@=��@=�-@=@<w�@<A�@<2�@</�@;�+@;��@;iD@;Y@:͟@:��@:z@9��@9��@9k�@92a@9+@9�@8��@8Z@8@7x@71�@7�@6��@6d�@5�H@5�7@5G�@4m�@3��@3s@3RT@36z@3�@2�A@2_@1�=@1��@1��@1p�@1%F@0�?@0�o@/��@/�
@/��@.�6@.��@.�+@.��@.��@.��@.Ta@-��@-��@-8�@,�j@,~(@,I�@+��@+��@+��@+|�@+l�@+
=@*��@*^5@*�@)�"@)L�@)q@(�@(�.@(I�@'�@'��@'��@'j�@'_p@'H�@&�]@&8�@%�)@%��@%u�@%�@$��@$��@$�@$,=@$!@#��@#A�@"͟@"�r@"1�@"
�@!�T@!��@!G�@!�@ �@ b@�[@t�@H�@+@��@\�@	@�>@��@^�@%F@�f@�/@�p@�@��@�@c�@1'@��@�@�@S�@�@��@v�@ff@��@�z@@�^@��@/@ \@%@ی@�@��@bN@,=@��@��@a@@�@�@�\@^5@H�@@��@��@��@f�@J�@-w@+�@%F@��@�@N�@@��@�*@�k@a@o@
=@�@S@��@�H@��@&�@�@��@�)@��@��@�M@O�@0�@��@��@��@�.@D�@9X@�@  @��@�m@˒@��@��@n/@4�@�@�6@�1@�r@{�@kQ@�@�@�t@�X@��@c�@ \@�@%@�5@��@?�@*�@$@"h@�@ݘ@�@�k@��@S�@>�@�@
�@
��@
�h@
��@
��@
��@
ff@
B[@
O@
�@	�@	�@	��@	��@	zx@	N<@	%F@		l@�|@��@��@oi@U2@?�@ �@��@ݘ@�6@��@�:@dZ@&@�@Y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
+B
+6B
+kB
+6B
+B
+6B
+B
+B
+6B
+6B
+6B
+6B
+QB
+B
*�B
+B
+B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
+B
*�B
+B
+B
+B
+B
+B
+B
+B
+B
+6B
+6B
*�B
$@B
 \B
�B
�B	��B	��B	��B	�B
#�B
�B	��B
B	��B
B	�B	�B	�B	�/B	�dB	�B	��B	�B	��B	�SB	�#B	B	�B	��B	�hB	��B	�B	�B	{B	p!B	i*B	c�B	`vB	[�B	P.B	BAB	&�B	fB�FB�B�KB�/B�9B��B�9B��BHBy�Bu�Bo�BoiBq�Bo�Bq�Bs�BtBs�Bv`B~(B�RB�eB��B��B�AB�	B�^BʌB�1B�B��B�lB� B�B��B�B��B�B�IB��B��B��B��B�MB�B�'B�cB�B��B�B�B	�B		�B	BB	{B	�B	%FB	,=B	3�B	:�B	JrB	UMB	ZQB	X_B	Z�B	X�B	S�B	S�B	T,B	R:B	O�B	NB	O\B	RB	R�B	QB	O(B	LB	I�B	GEB	L�B	M�B	UMB	\xB	a-B	h$B	h�B	poB	raB	r-B	raB	p!B	n�B	m�B	k�B	iyB	e�B	`�B	\]B	Z�B	V�B	R�B	[WB	Z�B	X�B	Z�B	YB	VB	Q�B	PbB	OvB	RB	V�B	XyB	W�B	Y�B	[qB	]IB	^5B	_;B	c:B	eFB	i�B	nB	oOB	r|B	xB	yXB	{�B	}B	}�B	�B	�AB	��B	�B	�B	�[B	�{B	��B	��B	�B	��B	��B	�[B	��B	� B	��B	�mB	��B	��B	��B	�kB	�VB	��B	�B	��B	�LB	�2B	��B	��B	��B	�`B	��B	�ZB	��B	�5B	�!B	�;B	�|B	�9B	��B	�nB	��B	�>B	��B	�AB	�+B	��B	�,B	�&B	��B	͹B	�(B	ՁB	�B	�&B	�}B	�BB	бB	՛B	�7B	��B	��B	��B	��B	��B	�)B	�!B	�jB	��B	�|B	�B	�B	��B	�tB	�B	�B	�tB	�zB	��B	�@B	�B	�B	��B	��B	��B	��B	�B	�yB	�B	�B	��B	��B	�B	��B	��B	�XB	��B	�mB	�8B	��B	�LB	�B	�B	�B	��B	�B	��B	�B	�B	�)B	��B	�B	�qB	��B	�=B	�B	�!B	�B	�B	�B	�hB	�3B	��B	�aB	�|B	��B	��B	�B	�B	��B	��B	��B	�>B	�B	�^B	��B	�	B	�*B	�dB	�6B	�B	��B	�xB	��B	�xB	��B	�B	�B	�VB	�wB	�.B	��B	��B
B
 �B
B
GB
{B
3B
�B
�B
�B
B
�B
�B
tB
�B
�B
�B
�B
�B
9B
mB
�B
�B
oB
 B
[B
�B
aB
�B
�B
?B
�B
?B
EB
�B
�B
+B
tB
YB
�B
�B
DB
dB
B
PB
6B
�B
�B
�B
�B
jB
�B
B
B
�B
BB
 B
B
hB
 B
TB
oB
�B
B
�B
B
@B
B
&B
FB
�B
MB
MB
gB
B
�B
2B
�B
�B
�B
?B
?B
?B
�B
B
B
EB
_B
1B
�B
�B
WB
�B
CB
�B
/B
/B
OB
jB
�B
�B
jB
5B
�B
�B
 �B
!�B
"B
"4B
"�B
"�B
"�B
#TB
#�B
$@B
$�B
$�B
%,B
%�B
%�B
&B
&2B
%�B
&�B
&�B
&�B
&�B
'B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
($B
'�B
(>B
(>B
(XB
(>B
'�B
'8B
&�B
&LB
&�B
'8B
'RB
'RB
(�B
)�B
(�B
)B
)DB
)�B
)�B
)�B
*KB
)�B
)�B
)�B
*B
+kB
,B
*�B
*0B
*B
*0B
)�B
*KB
*B
*�B
*�B
+kB
+�B
+�B
+�B
,�B
,�B
,�B
-B
,�B
-)B
-]B
-wB
-�B
-�B
/ B
/ B
/5B
/B
0UB
0UB
1AB
1[B
1vB
1�B
1�B
1�B
2B
2aB
2�B
3MB
3�B
3�B
3�B
3�B
3�B
4B
4TB
5B
5tB
5�B
5�B
6B
6FB
6�B
6�B
6�B
7�B
7�B
7�B
8B
8�B
:*B
:�B
:�B
;JB
<PB
=qB
?HB
@iB
?cB
?�B
@�B
@�B
A B
AB
AB
A B
AUB
A B
@�B
@�B
AUB
A;B
@�B
AB
@�B
@�B
A�B
A�B
BAB
C�B
C�B
D�B
ESB
D�B
EB
E�B
E�B
E�B
E�B
FB
F�B
FtB
F%B
E�B
E�B
E�B
E�B
F�B
G+B
GEB
G_B
GzB
G�B
H�B
H�B
H�B
H�B
IB
IB
IB
IB
IRB
I�B
J	B
J#B
J�B
KB
K�B
K�B
K�B
K�B
L0B
L0B
L�B
L�B
M6B
L�B
M�B
NVB
N�B
N"B
N"B
N"B
N�B
OB
OBB
O�B
O�B
O�B
O�B
O�B
O�B
PbB
Q B
QhB
Q4B
Q4B
Q4B
QNB
RTB
R�B
S@B
S@B
S�B
TB
T,B
T�B
T�B
UgB
U�B
U�B
V9B
V�B
V�B
W
B
W
B
W?B
WsB
W�B
XB
XEB
X_B
YB
Y�B
ZB
ZQB
Z�B
Z�B
[	B
[	B
[qB
[WB
[WB
[=B
[�B
[�B
\B
\B
\)B
\CB
\�B
\�B
]/B
]�B
]�B
]�B
^B
^5B
^jB
^�B
_!B
_VB
_�B
_�B
`vB
`�B
aB
a-B
`�B
abB
abB
a�B
a�B
bNB
bB
bhB
b�B
c B
c:B
c�B
c�B
c�B
dB
d&B
dZB
d�B
eB
e,B
e`B
ezB
fB
fB
f2B
g8B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
iDB
iDB
iDB
iDB
iDB
i�B
i�B
i�B
i�B
jB
j�B
kB
kB
j�B
j�B
j�B
kB
k�B
lB
l"B
l�B
l�B
l�B
mCB
mwB
m�B
mwB
mwB
m�B
n/B
nB
n/B
n�B
n�B
n�B
o5B
o5B
o�B
oOB
o5B
oOB
oOB
oiB
o5B
o�B
pUB
p�B
p�B
p�B
qAB
q[B
q[B
q�B
q�B
q�B
r|B
r�B
sB
s3B
s�B
s�B
s�B
s�B
tB
t9B
tnB
u?B
u�B
u�B
u�B
u�B
vFB
v�B
v�B
v�B
w2B
wfB
w�B
w�B
w�B
w�B
xB
xB
x8B
xRB
xlB
x�B
x�B
x�B
yXB
yXB
y�B
y�B
zB
z�B
z�B
z�B
z�B
z�B
{dB
{B
{JB
{dB
{B
{dB
|B
|B
|PB
|�B
|�B
}"B
}VB
}VB
}�B
}�B
}�B
}�B
~B
~BB
~BB
~�B
~�B
~�B
~�B
~�B
HB
}B
�B
� B
�4B
�iB
�OB
��B
��B
� B
�B
�;B
�;B
�;B
��B
��B
�'B
�AB
�[B
�AB
�AB
��B
��B
��B
�GB
�aB
��B
�{B
��B
��B
�B
�B
��B
�B
�MB
�gB
��B
��B
��B
�B
�SB
�SB
�mB
�mB
�mB
��B
�B
�?B
�%B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�1B
�1B
�fB
�fB
��B
��B
��B
��B
��B
��B
��B
�RB
�7B
��B
��B
��B
��B
��B
��B
�#B
�XB
�rB
��B
��B
��B
��B
�DB
�)B
�^B
�xB
��B
��B
��B
��B
��B
�JB
�~B
�~B
�J1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
+B
+6B
+kB
+6B
+B
+6B
+B
+B
+6B
+6B
+6B
+6B
+QB
+B
*�B
+B
+B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
+B
*�B
+B
+B
+B
+B
+B
+B
+B
+B
+6B
+6B
*�B
$@B
 \B
�B
�B	��B	��B	��B	�B
#�B
�B	��B
B	��B
B	�B	�B	�B	�/B	�dB	�B	��B	�B	��B	�SB	�#B	B	�B	��B	�hB	��B	�B	�B	{B	p!B	i*B	c�B	`vB	[�B	P.B	BAB	&�B	fB�FB�B�KB�/B�9B��B�9B��BHBy�Bu�Bo�BoiBq�Bo�Bq�Bs�BtBs�Bv`B~(B�RB�eB��B��B�AB�	B�^BʌB�1B�B��B�lB� B�B��B�B��B�B�IB��B��B��B��B�MB�B�'B�cB�B��B�B�B	�B		�B	BB	{B	�B	%FB	,=B	3�B	:�B	JrB	UMB	ZQB	X_B	Z�B	X�B	S�B	S�B	T,B	R:B	O�B	NB	O\B	RB	R�B	QB	O(B	LB	I�B	GEB	L�B	M�B	UMB	\xB	a-B	h$B	h�B	poB	raB	r-B	raB	p!B	n�B	m�B	k�B	iyB	e�B	`�B	\]B	Z�B	V�B	R�B	[WB	Z�B	X�B	Z�B	YB	VB	Q�B	PbB	OvB	RB	V�B	XyB	W�B	Y�B	[qB	]IB	^5B	_;B	c:B	eFB	i�B	nB	oOB	r|B	xB	yXB	{�B	}B	}�B	�B	�AB	��B	�B	�B	�[B	�{B	��B	��B	�B	��B	��B	�[B	��B	� B	��B	�mB	��B	��B	��B	�kB	�VB	��B	�B	��B	�LB	�2B	��B	��B	��B	�`B	��B	�ZB	��B	�5B	�!B	�;B	�|B	�9B	��B	�nB	��B	�>B	��B	�AB	�+B	��B	�,B	�&B	��B	͹B	�(B	ՁB	�B	�&B	�}B	�BB	бB	՛B	�7B	��B	��B	��B	��B	��B	�)B	�!B	�jB	��B	�|B	�B	�B	��B	�tB	�B	�B	�tB	�zB	��B	�@B	�B	�B	��B	��B	��B	��B	�B	�yB	�B	�B	��B	��B	�B	��B	��B	�XB	��B	�mB	�8B	��B	�LB	�B	�B	�B	��B	�B	��B	�B	�B	�)B	��B	�B	�qB	��B	�=B	�B	�!B	�B	�B	�B	�hB	�3B	��B	�aB	�|B	��B	��B	�B	�B	��B	��B	��B	�>B	�B	�^B	��B	�	B	�*B	�dB	�6B	�B	��B	�xB	��B	�xB	��B	�B	�B	�VB	�wB	�.B	��B	��B
B
 �B
B
GB
{B
3B
�B
�B
�B
B
�B
�B
tB
�B
�B
�B
�B
�B
9B
mB
�B
�B
oB
 B
[B
�B
aB
�B
�B
?B
�B
?B
EB
�B
�B
+B
tB
YB
�B
�B
DB
dB
B
PB
6B
�B
�B
�B
�B
jB
�B
B
B
�B
BB
 B
B
hB
 B
TB
oB
�B
B
�B
B
@B
B
&B
FB
�B
MB
MB
gB
B
�B
2B
�B
�B
�B
?B
?B
?B
�B
B
B
EB
_B
1B
�B
�B
WB
�B
CB
�B
/B
/B
OB
jB
�B
�B
jB
5B
�B
�B
 �B
!�B
"B
"4B
"�B
"�B
"�B
#TB
#�B
$@B
$�B
$�B
%,B
%�B
%�B
&B
&2B
%�B
&�B
&�B
&�B
&�B
'B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
($B
'�B
(>B
(>B
(XB
(>B
'�B
'8B
&�B
&LB
&�B
'8B
'RB
'RB
(�B
)�B
(�B
)B
)DB
)�B
)�B
)�B
*KB
)�B
)�B
)�B
*B
+kB
,B
*�B
*0B
*B
*0B
)�B
*KB
*B
*�B
*�B
+kB
+�B
+�B
+�B
,�B
,�B
,�B
-B
,�B
-)B
-]B
-wB
-�B
-�B
/ B
/ B
/5B
/B
0UB
0UB
1AB
1[B
1vB
1�B
1�B
1�B
2B
2aB
2�B
3MB
3�B
3�B
3�B
3�B
3�B
4B
4TB
5B
5tB
5�B
5�B
6B
6FB
6�B
6�B
6�B
7�B
7�B
7�B
8B
8�B
:*B
:�B
:�B
;JB
<PB
=qB
?HB
@iB
?cB
?�B
@�B
@�B
A B
AB
AB
A B
AUB
A B
@�B
@�B
AUB
A;B
@�B
AB
@�B
@�B
A�B
A�B
BAB
C�B
C�B
D�B
ESB
D�B
EB
E�B
E�B
E�B
E�B
FB
F�B
FtB
F%B
E�B
E�B
E�B
E�B
F�B
G+B
GEB
G_B
GzB
G�B
H�B
H�B
H�B
H�B
IB
IB
IB
IB
IRB
I�B
J	B
J#B
J�B
KB
K�B
K�B
K�B
K�B
L0B
L0B
L�B
L�B
M6B
L�B
M�B
NVB
N�B
N"B
N"B
N"B
N�B
OB
OBB
O�B
O�B
O�B
O�B
O�B
O�B
PbB
Q B
QhB
Q4B
Q4B
Q4B
QNB
RTB
R�B
S@B
S@B
S�B
TB
T,B
T�B
T�B
UgB
U�B
U�B
V9B
V�B
V�B
W
B
W
B
W?B
WsB
W�B
XB
XEB
X_B
YB
Y�B
ZB
ZQB
Z�B
Z�B
[	B
[	B
[qB
[WB
[WB
[=B
[�B
[�B
\B
\B
\)B
\CB
\�B
\�B
]/B
]�B
]�B
]�B
^B
^5B
^jB
^�B
_!B
_VB
_�B
_�B
`vB
`�B
aB
a-B
`�B
abB
abB
a�B
a�B
bNB
bB
bhB
b�B
c B
c:B
c�B
c�B
c�B
dB
d&B
dZB
d�B
eB
e,B
e`B
ezB
fB
fB
f2B
g8B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
iDB
iDB
iDB
iDB
iDB
i�B
i�B
i�B
i�B
jB
j�B
kB
kB
j�B
j�B
j�B
kB
k�B
lB
l"B
l�B
l�B
l�B
mCB
mwB
m�B
mwB
mwB
m�B
n/B
nB
n/B
n�B
n�B
n�B
o5B
o5B
o�B
oOB
o5B
oOB
oOB
oiB
o5B
o�B
pUB
p�B
p�B
p�B
qAB
q[B
q[B
q�B
q�B
q�B
r|B
r�B
sB
s3B
s�B
s�B
s�B
s�B
tB
t9B
tnB
u?B
u�B
u�B
u�B
u�B
vFB
v�B
v�B
v�B
w2B
wfB
w�B
w�B
w�B
w�B
xB
xB
x8B
xRB
xlB
x�B
x�B
x�B
yXB
yXB
y�B
y�B
zB
z�B
z�B
z�B
z�B
z�B
{dB
{B
{JB
{dB
{B
{dB
|B
|B
|PB
|�B
|�B
}"B
}VB
}VB
}�B
}�B
}�B
}�B
~B
~BB
~BB
~�B
~�B
~�B
~�B
~�B
HB
}B
�B
� B
�4B
�iB
�OB
��B
��B
� B
�B
�;B
�;B
�;B
��B
��B
�'B
�AB
�[B
�AB
�AB
��B
��B
��B
�GB
�aB
��B
�{B
��B
��B
�B
�B
��B
�B
�MB
�gB
��B
��B
��B
�B
�SB
�SB
�mB
�mB
�mB
��B
�B
�?B
�%B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�1B
�1B
�fB
�fB
��B
��B
��B
��B
��B
��B
��B
�RB
�7B
��B
��B
��B
��B
��B
��B
�#B
�XB
�rB
��B
��B
��B
��B
�DB
�)B
�^B
�xB
��B
��B
��B
��B
��B
�JB
�~B
�~B
�J1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230509094235  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230509094236  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230509094236  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230509094237                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230509094237  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230509094237  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230509095754                      G�O�G�O�G�O�                