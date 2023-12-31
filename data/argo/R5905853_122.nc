CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-13T18:51:38Z creation;2022-06-13T18:51:39Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220613185138  20220613185959  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               zA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @����T21   @���oP@/1&�x��c\(�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C�fC  C  C  C  C  C�C �C"�C$  C%�fC'�fC*  C,  C.  C0  C2  C4  C6  C8�C:�C<�C>  C@  CB�CD�CE�fCG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�3D�@ D� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @\)@u@��H@��HAp�A=p�A]p�A}p�A��RA��RA��RA��RAυA߅A�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��GB��B��B��B�z�B�z�B��B��B��B��B��B��B��B��GB��GB��GB��GB�z�BӮB׮BۮB߮B�B�B�B�B�B��B��B��C�
C�
C�
C�
C	�
C�
C�C�
C�pC�
C�
C�
C�
C�
C�C�C!�C#�
C%�pC'�pC)�
C+�
C-�
C/�
C1�
C3�
C5�
C7�C9�C;�C=�
C?�
CA�CC�CE�pCG�pCI�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C�޸C��C��C��C��C��C��RC��RC��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�޸C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DX|)DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D�D��D�:�D�z�D���D���D�:�D�z�D���D���D�7�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��0A���A�̘A��6A��BA���A���A��vA�ΥA��pA�ΥA���Aͺ^A̭�Aʟ�A�z�A�6FAɒ�A��AȅA�_pA�@�A�6A��8A�u�Aƣ:AŔA�C�A��2A���Aĳ�A�h�A�]�A�Y�A�uA�A�ȴA�oA¬A��`A��A�49A�ÖA�[�A���A�1[A��A���A�/�A��aA���A���A�MA�u%A�TaA��A���A�W?A�xA���A�_;A�A���A�bNA�0UA�r|A�}�A�e,A���A��EA�PA��BA�YA��jA�m]A�ݘA��UA��uA�c�A�0!A���A�&�A��A���A�A�-�A�I�A�~A�y�A���A��A��'A���A{��Ax�{Avy�Ar��Ap@Aj�AgԕAc?}A_��A\|AX��ATߤAR�yAP�AORTAM($AJL0AH`�AFk�AC`BAA�}A?�A<�cA;kQA:u%A6��A4C-A31A2�:A2#�A1ߤA1�LA1c�A0خA0`BA.�aA.)_A-xA,��A+�A*��A*�;A)��A*�+A*oA&�HA&'RA%($A$U2A#p;A"i�A"��A"p�A"Q�A"oiA!��A ��A�A_pAe�A�zAGEA	lA~(A��A��A=qA�AqvA��A�9A�hAB[A�A8�A�wAIRA5?A�RAYAںA��A��Av�A]�A�=AԕA��A�1A�DAA�A�ZA��A��A�7AD�A	A�aAd�A�A҉A�7ATaA��A�CAU�A҉A�'Ap;AY�Ak�AZ�A
�A
n�A	��A�+A�-Ap;A-ASAƨA�A?}A�.A2aA	�A7�A��A��AtTA��A�:A��A�A�A ��A YKA 8�A $t@���@�*0@���@���@�Y@��/@�M@���@���@�)�@��@�ں@��!@���@�5?@�w2@�m�@���@��@�}V@�2�@��@��@��@���@��@�@�@�~�@�F�@�@�l�@��A@@��@�$t@�ں@�{@��@�O@�\)@�~(@�{@��&@��@��@�z�@�@��	@仙@�\�@�F@�9�@��@��@�h@���@�}@�-@��@ް�@ޣ�@���@ݟV@��y@�u�@�e,@���@�j@��@�F@ֵ@�<�@��@��U@�;�@�C�@��3@҃@ѯ�@о@�	@ϑh@�!-@��?@΄�@�W�@�!�@���@�a@�8@�!-@�҉@�u%@�N<@��A@ɱ[@�J�@�S�@��@ǧ�@�\)@��@ƃ@�D�@��@Ŵ�@Š'@�<6@ľ@Ē�@��@ãn@�U�@�q@��@�@�B[@�	�@��X@�U�@��?@�6�@��@@�=@�(�@��B@�6@��r@�U�@�V@��9@��L@�`�@��@���@�!-@���@�W�@�GE@��[@�4�@��p@��@�&�@���@��*@��@���@�@�@��.@��@���@���@�2�@���@�	l@�y>@�	�@��Q@�|@���@�L0@�@��@��F@�W?@�:�@��8@�Xy@��&@��@���@���@�v`@�33@��@�u�@���@��	@���@���@��_@��r@�0U@�@���@�33@�Ft@��z@���@�L�@��@��@��'@��_@�.�@��d@���@�F�@��B@���@�:�@��@���@�T�@��@��@�YK@���@��@�C�@��@�u�@���@��@�Ta@�	@�*0@���@��K@�Z�@�|�@�w�@��Z@��X@��@���@���@�zx@�s@�f�@�a�@�S&@�7L@��@��?@�S�@�,=@���@��n@�iD@�+@���@�v�@�@�@���@��)@���@�P�@��@��[@�u�@��@��K@���@���@�o @�RT@�%F@�S@��@���@��r@�E�@���@�T�@�K�@�E9@�8�@��f@���@�:*@��@��+@��z@�l�@���@�M�@��@���@�~�@�F@�o@���@��<@��@��L@�n�@�#:@�}�@�\)@�Y@��@�ȴ@��j@���@��@�R�@���@��>@���@�b�@�&@��|@��E@���@��4@�l"@�J�@�)�@�)�@��@���@�Z�@�#�@��]@���@��F@�l"@�M@��@��h@��f@�j@�5�@��)@�a|@�>B@�
�@���@�˒@��$@�;d@���@�҉@�w�@�-�@��r@���@�O@�-w@��y@�_�@��r@��T@���@���@���@��{@�\�@�:�@�4�@� \@��@��@�_@�2�@��@��N@���@�rG@��@�4n@�@~��@~z@~e@}�^@}w2@}Y�@}+@|��@|��@|�@|��@{@O@zc @zO@y�@y�"@y|@y!�@x��@xr�@x6@w��@w��@w'�@vz@vB[@v#:@u&�@t�o@t  @s�0@sRT@s�@r�]@rTa@q7L@p��@o��@ol�@o,�@n�1@m�@m�@l��@lu�@k��@j��@j.�@j	@i��@i��@i�@h��@h-�@hG@g�@gY@f��@f�@fkQ@fR�@fM�@f)�@e�3@e�@dz�@d,=@cƨ@c|�@cP�@c1�@b�@bq�@a�@a&�@`��@`��@`@_� @_��@_�k@_�P@_�*@_x@_(@^�@^s�@^H�@]�D@]�@\��@\�e@[��@[�@Z($@Y�H@Ye,@Y2a@Y�@X��@X'R@W�a@W��@W,�@V��@VW�@U��@U#�@T��@Te�@Sخ@S��@S�@S��@Sƨ@S��@S��@S�P@S��@Rߤ@RJ@Q�@Q*0@Q�@Q�@P�	@P�`@P�$@P��@P7@O�+@O�w@OC�@N͟@N��@N�r@N5?@M��@M4@L�$@L!@K�0@Kqv@KC@J��@J��@Js�@JJ�@I�j@I�@I��@I��@Ip�@I�@H��@H4n@GW?@Fȴ@F��@F�r@FL0@FO@F4@E�@E��@E�@D��@D�@D��@DN�@C��@C˒@C��@C�@C@B�@B��@Bff@B.�@Bu@A��@Aԕ@Ac@AL�@A;@@�v@@�$@@H@?�@?b�@>�@>��@>3�@=^�@=V@<��@<Ɇ@<��@<'R@;�*@;S�@:�@:{@9��@9:�@9#�@8�@7��@7
=@6�"@6ߤ@6�6@6=q@5��@5�~@4�@4��@4l"@4C-@4~@4x@3�]@3�k@2�@2C�@1�@1�~@1�@0�e@0>B@0�@0�@/�@//�@.��@.�@-�#@-��@-=�@-+@,�@,�@,�I@,�@,�@,2�@+ݘ@+��@+��@+�*@+�*@+��@+/�@*��@*u@)��@)�z@)�C@)��@)��@)x�@)J�@(��@(��@(��@(�I@(K^@(:�@(%�@'��@'�}@'�q@'l�@'C�@&��@&8�@&_@%�#@%��@%�7@%7L@%�@$�K@$�@$��@$�@$bN@$9X@$  @#�6@#��@#��@#RT@"�@"��@"YK@!�D@!��@!�@!�@!�T@!�@!L�@!	l@ �5@ ֡@ �z@ D�@�@�{@n/@e�@U�@;d@��@��@3�@�>@��@J�@0�@�@�9@��@��@�e@��@��@Q�@	�@��@t�@��@L0@�@�@@�H@��@?}@�@��@:�@G@�;@�F@�:@y�@qv@�@�m@�R@�6@��@p;@Ta@H�@�.@�~@2a@@@�@�p@u�@6@�W@�}@�4@�@_�@^5@W�@M�@B[@)�@
�@��@�@�N@��@j@<6@�@�?@�I@��@|�@j@N�@~@M@�@��@$t@�2@��@s�@a|@GE@�@�@��@s�@^�@8�@#�@�@�5@��@�O11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��0A���A�̘A��6A��BA���A���A��vA�ΥA��pA�ΥA���Aͺ^A̭�Aʟ�A�z�A�6FAɒ�A��AȅA�_pA�@�A�6A��8A�u�Aƣ:AŔA�C�A��2A���Aĳ�A�h�A�]�A�Y�A�uA�A�ȴA�oA¬A��`A��A�49A�ÖA�[�A���A�1[A��A���A�/�A��aA���A���A�MA�u%A�TaA��A���A�W?A�xA���A�_;A�A���A�bNA�0UA�r|A�}�A�e,A���A��EA�PA��BA�YA��jA�m]A�ݘA��UA��uA�c�A�0!A���A�&�A��A���A�A�-�A�I�A�~A�y�A���A��A��'A���A{��Ax�{Avy�Ar��Ap@Aj�AgԕAc?}A_��A\|AX��ATߤAR�yAP�AORTAM($AJL0AH`�AFk�AC`BAA�}A?�A<�cA;kQA:u%A6��A4C-A31A2�:A2#�A1ߤA1�LA1c�A0خA0`BA.�aA.)_A-xA,��A+�A*��A*�;A)��A*�+A*oA&�HA&'RA%($A$U2A#p;A"i�A"��A"p�A"Q�A"oiA!��A ��A�A_pAe�A�zAGEA	lA~(A��A��A=qA�AqvA��A�9A�hAB[A�A8�A�wAIRA5?A�RAYAںA��A��Av�A]�A�=AԕA��A�1A�DAA�A�ZA��A��A�7AD�A	A�aAd�A�A҉A�7ATaA��A�CAU�A҉A�'Ap;AY�Ak�AZ�A
�A
n�A	��A�+A�-Ap;A-ASAƨA�A?}A�.A2aA	�A7�A��A��AtTA��A�:A��A�A�A ��A YKA 8�A $t@���@�*0@���@���@�Y@��/@�M@���@���@�)�@��@�ں@��!@���@�5?@�w2@�m�@���@��@�}V@�2�@��@��@��@���@��@�@�@�~�@�F�@�@�l�@��A@@��@�$t@�ں@�{@��@�O@�\)@�~(@�{@��&@��@��@�z�@�@��	@仙@�\�@�F@�9�@��@��@�h@���@�}@�-@��@ް�@ޣ�@���@ݟV@��y@�u�@�e,@���@�j@��@�F@ֵ@�<�@��@��U@�;�@�C�@��3@҃@ѯ�@о@�	@ϑh@�!-@��?@΄�@�W�@�!�@���@�a@�8@�!-@�҉@�u%@�N<@��A@ɱ[@�J�@�S�@��@ǧ�@�\)@��@ƃ@�D�@��@Ŵ�@Š'@�<6@ľ@Ē�@��@ãn@�U�@�q@��@�@�B[@�	�@��X@�U�@��?@�6�@��@@�=@�(�@��B@�6@��r@�U�@�V@��9@��L@�`�@��@���@�!-@���@�W�@�GE@��[@�4�@��p@��@�&�@���@��*@��@���@�@�@��.@��@���@���@�2�@���@�	l@�y>@�	�@��Q@�|@���@�L0@�@��@��F@�W?@�:�@��8@�Xy@��&@��@���@���@�v`@�33@��@�u�@���@��	@���@���@��_@��r@�0U@�@���@�33@�Ft@��z@���@�L�@��@��@��'@��_@�.�@��d@���@�F�@��B@���@�:�@��@���@�T�@��@��@�YK@���@��@�C�@��@�u�@���@��@�Ta@�	@�*0@���@��K@�Z�@�|�@�w�@��Z@��X@��@���@���@�zx@�s@�f�@�a�@�S&@�7L@��@��?@�S�@�,=@���@��n@�iD@�+@���@�v�@�@�@���@��)@���@�P�@��@��[@�u�@��@��K@���@���@�o @�RT@�%F@�S@��@���@��r@�E�@���@�T�@�K�@�E9@�8�@��f@���@�:*@��@��+@��z@�l�@���@�M�@��@���@�~�@�F@�o@���@��<@��@��L@�n�@�#:@�}�@�\)@�Y@��@�ȴ@��j@���@��@�R�@���@��>@���@�b�@�&@��|@��E@���@��4@�l"@�J�@�)�@�)�@��@���@�Z�@�#�@��]@���@��F@�l"@�M@��@��h@��f@�j@�5�@��)@�a|@�>B@�
�@���@�˒@��$@�;d@���@�҉@�w�@�-�@��r@���@�O@�-w@��y@�_�@��r@��T@���@���@���@��{@�\�@�:�@�4�@� \@��@��@�_@�2�@��@��N@���@�rG@��@�4n@�@~��@~z@~e@}�^@}w2@}Y�@}+@|��@|��@|�@|��@{@O@zc @zO@y�@y�"@y|@y!�@x��@xr�@x6@w��@w��@w'�@vz@vB[@v#:@u&�@t�o@t  @s�0@sRT@s�@r�]@rTa@q7L@p��@o��@ol�@o,�@n�1@m�@m�@l��@lu�@k��@j��@j.�@j	@i��@i��@i�@h��@h-�@hG@g�@gY@f��@f�@fkQ@fR�@fM�@f)�@e�3@e�@dz�@d,=@cƨ@c|�@cP�@c1�@b�@bq�@a�@a&�@`��@`��@`@_� @_��@_�k@_�P@_�*@_x@_(@^�@^s�@^H�@]�D@]�@\��@\�e@[��@[�@Z($@Y�H@Ye,@Y2a@Y�@X��@X'R@W�a@W��@W,�@V��@VW�@U��@U#�@T��@Te�@Sخ@S��@S�@S��@Sƨ@S��@S��@S�P@S��@Rߤ@RJ@Q�@Q*0@Q�@Q�@P�	@P�`@P�$@P��@P7@O�+@O�w@OC�@N͟@N��@N�r@N5?@M��@M4@L�$@L!@K�0@Kqv@KC@J��@J��@Js�@JJ�@I�j@I�@I��@I��@Ip�@I�@H��@H4n@GW?@Fȴ@F��@F�r@FL0@FO@F4@E�@E��@E�@D��@D�@D��@DN�@C��@C˒@C��@C�@C@B�@B��@Bff@B.�@Bu@A��@Aԕ@Ac@AL�@A;@@�v@@�$@@H@?�@?b�@>�@>��@>3�@=^�@=V@<��@<Ɇ@<��@<'R@;�*@;S�@:�@:{@9��@9:�@9#�@8�@7��@7
=@6�"@6ߤ@6�6@6=q@5��@5�~@4�@4��@4l"@4C-@4~@4x@3�]@3�k@2�@2C�@1�@1�~@1�@0�e@0>B@0�@0�@/�@//�@.��@.�@-�#@-��@-=�@-+@,�@,�@,�I@,�@,�@,2�@+ݘ@+��@+��@+�*@+�*@+��@+/�@*��@*u@)��@)�z@)�C@)��@)��@)x�@)J�@(��@(��@(��@(�I@(K^@(:�@(%�@'��@'�}@'�q@'l�@'C�@&��@&8�@&_@%�#@%��@%�7@%7L@%�@$�K@$�@$��@$�@$bN@$9X@$  @#�6@#��@#��@#RT@"�@"��@"YK@!�D@!��@!�@!�@!�T@!�@!L�@!	l@ �5@ ֡@ �z@ D�@�@�{@n/@e�@U�@;d@��@��@3�@�>@��@J�@0�@�@�9@��@��@�e@��@��@Q�@	�@��@t�@��@L0@�@�@@�H@��@?}@�@��@:�@G@�;@�F@�:@y�@qv@�@�m@�R@�6@��@p;@Ta@H�@�.@�~@2a@@@�@�p@u�@6@�W@�}@�4@�@_�@^5@W�@M�@B[@)�@
�@��@�@�N@��@j@<6@�@�?@�I@��@|�@j@N�@~@M@�@��@$t@�2@��@s�@a|@GE@�@�@��@s�@^�@8�@#�@�@�5@��@�O11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
^B
�B
*�B
d�B
g�B
m�B
xB
��B
�FB
��B
�!B
�B�BNB�B�BMB�B�B�B�B
�BVB9�BB�B]�B�tB��B�B1ABR�B\BmBx8B�B�_B�AB~wB|B��B��B�B�NB��BgmBOBGEBBB@�B<B9XB5�B49B2|B+�B�B�B�+B�B�tB��B��B�B�B��B��B�_Bu%Br�BrGBR�BpB
�3B
��B
r�B
S�B
BuB
7�B
%zB
�B
$B
�B	�6B	�9B	��B	��B	�_B	�B	uB	`B	N�B	@�B	1B	$�B	qB	oB	jB	1B	GB	�B		�B	�B	�B	�B	�B	B	_B	�B		B	
XB	
�B	<B	�B	?B	B	)B	�B	"NB	'RB	.cB	2B	6zB	5�B	:^B	G�B	u�B	z�B	h�B	i_B	p�B	y	B	x�B	v�B	��B	��B	�,B	��B	�dB	�<B	��B	āB	��B	��B	��B	�KB	�B	��B	��B	B	�9B	ªB	��B	�B	B	��B	�6B	��B	�RB	�B	�#B	��B	͟B	͹B	��B	�KB	�=B	�;B	ܒB	�MB	��B	��B	�YB	�IB	�B	�B	��B	�B	��B	�QB	�B	��B	��B	�B	�B	��B	�B	� B	��B	�B	�*B	�$B	�B	�B	�B	�6B	��B	�B	�_B	�B	��B	��B	�;B	��B	�'B	��B	�UB	�B	�yB	��B	�nB	�B	��B	�hB	�B	�mB	��B	�B	�DB	�B	�sB	�B	�B	��B	�B	�B	�B	�
B	�XB	�B	�XB	�B	�DB	��B	��B	��B	�*B	�B	��B	�B	�*B	�XB	�$B	��B	�B	��B	�kB	�B	� B	�B	�B	�aB	�B	��B	�B	�B	�B	�/B	�/B	�IB	�CB	��B	�B	�B	�6B	�B	��B	��B	�B	�B	�XB	��B	��B	�2B	�`B	��B	�:B	�B	�B	�B	�sB	�8B	��B	�B	�B	�"B	�WB	�B	�B	�B	�B	�B	�2B	�B	�B	��B	�,B	��B	�B	�$B	��B	��B	��B	��B	�B	�B	��B	�sB	�$B	�B	��B	�B	�B	��B	�CB	��B	��B	�*B	�B	��B	�wB	�)B	�B	�qB	�WB	��B	�B	�;B	�oB	�B	�B	�GB	�aB	�B	�B	��B	��B	��B	��B	�nB	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�XB	�*B	�^B	��B	�xB	��B	�xB	�0B	��B	�B	�B	��B	�PB	��B	��B	��B	�"B	�B	�"B	��B	��B	��B	�(B	�BB	��B	��B	��B	��B	��B	�B	��B	��B	��B	�.B	��B	��B	��B	��B	�.B	��B	�.B
 4B
 �B
 iB
 OB
 OB
 B
 4B
  B
 4B	��B
 �B
 iB
 �B
 �B
UB
 �B
 �B
 4B
 OB
;B
oB
�B
�B
�B
�B
'B
�B
�B
B
�B
aB
�B
-B
GB
�B
�B
�B
GB
-B
'B
 �B
B
MB
�B
SB
�B
 OB
gB
9B
�B
[B
�B
B
?B
_B
�B
�B
�B
	�B

=B
DB
~B
�B
�B
PB
jB
�B
�B
�B
�B
�B
NB
�B
 B
�B
�B
B
�B
{B
B
�B
�B
9B
�B
�B

B
�B
�B
B
�B
B
B
_B
�B
�B
�B
�B
�B
�B
�B
�B
#B
WB
�B
WB
�B
B
�B
jB
�B
�B
B
pB
�B
�B
 B
 BB
 'B
 'B
 \B
 �B
!�B
!�B
"4B
"NB
"hB
"�B
"hB
"4B
"�B
"�B
"�B
#TB
#nB
#�B
#�B
#�B
#�B
$B
$&B
$&B
$ZB
$@B
$@B
$�B
$�B
%`B
%�B
%�B
%�B
%�B
&2B
&fB
&fB
&fB
&�B
'B
'B
'B
'�B
'�B
'�B
(>B
(�B
(
B
(
B
'�B
'�B
'�B
'�B
(>B
(>B
(sB
)�B
+�B
,B
,B
,"B
+�B
+�B
,=B
,�B
-wB
.cB
/ B
/�B
/�B
0B
0;B
0!B
0UB
0!B
/�B
/�B
0UB
0B
0�B
1B
1vB
1�B
33B
3�B
4�B
4�B
4�B
4�B
4nB
4nB
2�B
2�B
33B
33B
33B
3�B
3�B
4B
4B
4�B
5�B
6zB
7B
7B
6�B
7�B
8B
8lB
8�B
9	B
9$B
9	B
8lB
88B
8�B
9	B
9>B
9	B
8�B
8�B
9	B
9XB
9>B
9�B
:*B
;B
;0B
;JB
;B
;�B
;�B
<B
;�B
<B
="B
=VB
=VB
=VB
=<B
="B
=B
=qB
=�B
=�B
?cB
@4B
@iB
@iB
@iB
@�B
@�B
A;B
B[B
B�B
BAB
BB
A�B
A�B
A�B
BB
DMB
D�B
EmB
E9B
EB
EB
EB
DgB
D�B
E�B
E�B
EB
EmB
EmB
E�B
FB
FB
E�B
E�B
EmB
EmB
FYB
F�B
GEB
G�B
G_B
GEB
G�B
HKB
H1B
H�B
IB
J�B
KDB
K^B
KDB
K^B
J�B
JrB
K�B
L�B
MB
M�B
M�B
M�B
N"B
NVB
N�B
N�B
N�B
OBB
OvB
OvB
O�B
PB
O�B
O�B
PB
O�B
O�B
O�B
PB
PHB
PHB
PHB
P.B
PHB
P}B
P�B
P�B
P�B
QNB
Q�B
R B
SB
S�B
S�B
S�B
TB
T,B
TFB
T{B
T�B
U�B
U�B
UgB
U�B
U�B
UgB
U2B
U�B
U�B
U�B
U�B
VB
V�B
V�B
W$B
W�B
XEB
X�B
YeB
ZQB
ZQB
ZkB
Z�B
Z�B
[�B
\B
\CB
\]B
]/B
]dB
]dB
]~B
]�B
]�B
^5B
^OB
_B
_VB
_�B
_�B
_�B
_�B
a�B
a�B
a�B
a�B
a�B
bB
b4B
b�B
c B
cTB
cnB
c�B
c�B
c�B
c�B
dB
d�B
eB
e,B
e�B
e�B
fLB
f�B
f�B
f�B
gB
g8B
g�B
h>B
hXB
h�B
h�B
h�B
iB
i_B
i_B
iDB
iDB
i�B
i�B
jB
jB
jB
i�B
jB
jKB
j�B
kkB
k�B
kkB
k�B
k�B
k�B
k�B
k�B
lWB
lWB
l=B
lWB
l�B
l�B
l�B
l�B
l�B
mB
m)B
m)B
m�B
n/B
n/B
nIB
n}B
n�B
n�B
n�B
oB
oB
oB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
pB
poB
p�B
p�B
qB
q'B
qB
qB
p�B
q'B
q�B
q�B
q�B
q�B
q�B
rGB
r|B
r�B
r�B
r�B
s3B
sB
shB
s�B
t9B
tnB
t�B
t�B
t�B
u?B
uZB
u?B
uZB
uZB
uZB
uZB
u�B
u�B
vB
v+B
wB
w�B
w�B
w�B
w�B
w�B
w�B
x8B
xlB
x�B
y	B
y$B
yXB
yrB
y�B
y�B
y�B
z*B
zDB
zDB
zDB
zxB
z�B
z�B
zxB
z�B
{0B
{dB
{�B
{B
{�B
|6B
|�B
|�B
|�B
|�B
}�B
~B
}�B
}�B
}�B
}�B
~B
~BB
~BB
~(B
~BB
~�B
~�B
~�B
.B
HB
}B
}B
}B
}B
�B
�B
�B
�B
�iB
��B
��B
�UB
�oB
�oB
�UB
��B
��B
�'B
�AB
�[B
�uB
�uB
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
^B
�B
*�B
d�B
g�B
m�B
xB
��B
�FB
��B
�!B
�B�BNB�B�BMB�B�B�B�B
�BVB9�BB�B]�B�tB��B�B1ABR�B\BmBx8B�B�_B�AB~wB|B��B��B�B�NB��BgmBOBGEBBB@�B<B9XB5�B49B2|B+�B�B�B�+B�B�tB��B��B�B�B��B��B�_Bu%Br�BrGBR�BpB
�3B
��B
r�B
S�B
BuB
7�B
%zB
�B
$B
�B	�6B	�9B	��B	��B	�_B	�B	uB	`B	N�B	@�B	1B	$�B	qB	oB	jB	1B	GB	�B		�B	�B	�B	�B	�B	B	_B	�B		B	
XB	
�B	<B	�B	?B	B	)B	�B	"NB	'RB	.cB	2B	6zB	5�B	:^B	G�B	u�B	z�B	h�B	i_B	p�B	y	B	x�B	v�B	��B	��B	�,B	��B	�dB	�<B	��B	āB	��B	��B	��B	�KB	�B	��B	��B	B	�9B	ªB	��B	�B	B	��B	�6B	��B	�RB	�B	�#B	��B	͟B	͹B	��B	�KB	�=B	�;B	ܒB	�MB	��B	��B	�YB	�IB	�B	�B	��B	�B	��B	�QB	�B	��B	��B	�B	�B	��B	�B	� B	��B	�B	�*B	�$B	�B	�B	�B	�6B	��B	�B	�_B	�B	��B	��B	�;B	��B	�'B	��B	�UB	�B	�yB	��B	�nB	�B	��B	�hB	�B	�mB	��B	�B	�DB	�B	�sB	�B	�B	��B	�B	�B	�B	�
B	�XB	�B	�XB	�B	�DB	��B	��B	��B	�*B	�B	��B	�B	�*B	�XB	�$B	��B	�B	��B	�kB	�B	� B	�B	�B	�aB	�B	��B	�B	�B	�B	�/B	�/B	�IB	�CB	��B	�B	�B	�6B	�B	��B	��B	�B	�B	�XB	��B	��B	�2B	�`B	��B	�:B	�B	�B	�B	�sB	�8B	��B	�B	�B	�"B	�WB	�B	�B	�B	�B	�B	�2B	�B	�B	��B	�,B	��B	�B	�$B	��B	��B	��B	��B	�B	�B	��B	�sB	�$B	�B	��B	�B	�B	��B	�CB	��B	��B	�*B	�B	��B	�wB	�)B	�B	�qB	�WB	��B	�B	�;B	�oB	�B	�B	�GB	�aB	�B	�B	��B	��B	��B	��B	�nB	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�XB	�*B	�^B	��B	�xB	��B	�xB	�0B	��B	�B	�B	��B	�PB	��B	��B	��B	�"B	�B	�"B	��B	��B	��B	�(B	�BB	��B	��B	��B	��B	��B	�B	��B	��B	��B	�.B	��B	��B	��B	��B	�.B	��B	�.B
 4B
 �B
 iB
 OB
 OB
 B
 4B
  B
 4B	��B
 �B
 iB
 �B
 �B
UB
 �B
 �B
 4B
 OB
;B
oB
�B
�B
�B
�B
'B
�B
�B
B
�B
aB
�B
-B
GB
�B
�B
�B
GB
-B
'B
 �B
B
MB
�B
SB
�B
 OB
gB
9B
�B
[B
�B
B
?B
_B
�B
�B
�B
	�B

=B
DB
~B
�B
�B
PB
jB
�B
�B
�B
�B
�B
NB
�B
 B
�B
�B
B
�B
{B
B
�B
�B
9B
�B
�B

B
�B
�B
B
�B
B
B
_B
�B
�B
�B
�B
�B
�B
�B
�B
#B
WB
�B
WB
�B
B
�B
jB
�B
�B
B
pB
�B
�B
 B
 BB
 'B
 'B
 \B
 �B
!�B
!�B
"4B
"NB
"hB
"�B
"hB
"4B
"�B
"�B
"�B
#TB
#nB
#�B
#�B
#�B
#�B
$B
$&B
$&B
$ZB
$@B
$@B
$�B
$�B
%`B
%�B
%�B
%�B
%�B
&2B
&fB
&fB
&fB
&�B
'B
'B
'B
'�B
'�B
'�B
(>B
(�B
(
B
(
B
'�B
'�B
'�B
'�B
(>B
(>B
(sB
)�B
+�B
,B
,B
,"B
+�B
+�B
,=B
,�B
-wB
.cB
/ B
/�B
/�B
0B
0;B
0!B
0UB
0!B
/�B
/�B
0UB
0B
0�B
1B
1vB
1�B
33B
3�B
4�B
4�B
4�B
4�B
4nB
4nB
2�B
2�B
33B
33B
33B
3�B
3�B
4B
4B
4�B
5�B
6zB
7B
7B
6�B
7�B
8B
8lB
8�B
9	B
9$B
9	B
8lB
88B
8�B
9	B
9>B
9	B
8�B
8�B
9	B
9XB
9>B
9�B
:*B
;B
;0B
;JB
;B
;�B
;�B
<B
;�B
<B
="B
=VB
=VB
=VB
=<B
="B
=B
=qB
=�B
=�B
?cB
@4B
@iB
@iB
@iB
@�B
@�B
A;B
B[B
B�B
BAB
BB
A�B
A�B
A�B
BB
DMB
D�B
EmB
E9B
EB
EB
EB
DgB
D�B
E�B
E�B
EB
EmB
EmB
E�B
FB
FB
E�B
E�B
EmB
EmB
FYB
F�B
GEB
G�B
G_B
GEB
G�B
HKB
H1B
H�B
IB
J�B
KDB
K^B
KDB
K^B
J�B
JrB
K�B
L�B
MB
M�B
M�B
M�B
N"B
NVB
N�B
N�B
N�B
OBB
OvB
OvB
O�B
PB
O�B
O�B
PB
O�B
O�B
O�B
PB
PHB
PHB
PHB
P.B
PHB
P}B
P�B
P�B
P�B
QNB
Q�B
R B
SB
S�B
S�B
S�B
TB
T,B
TFB
T{B
T�B
U�B
U�B
UgB
U�B
U�B
UgB
U2B
U�B
U�B
U�B
U�B
VB
V�B
V�B
W$B
W�B
XEB
X�B
YeB
ZQB
ZQB
ZkB
Z�B
Z�B
[�B
\B
\CB
\]B
]/B
]dB
]dB
]~B
]�B
]�B
^5B
^OB
_B
_VB
_�B
_�B
_�B
_�B
a�B
a�B
a�B
a�B
a�B
bB
b4B
b�B
c B
cTB
cnB
c�B
c�B
c�B
c�B
dB
d�B
eB
e,B
e�B
e�B
fLB
f�B
f�B
f�B
gB
g8B
g�B
h>B
hXB
h�B
h�B
h�B
iB
i_B
i_B
iDB
iDB
i�B
i�B
jB
jB
jB
i�B
jB
jKB
j�B
kkB
k�B
kkB
k�B
k�B
k�B
k�B
k�B
lWB
lWB
l=B
lWB
l�B
l�B
l�B
l�B
l�B
mB
m)B
m)B
m�B
n/B
n/B
nIB
n}B
n�B
n�B
n�B
oB
oB
oB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
pB
poB
p�B
p�B
qB
q'B
qB
qB
p�B
q'B
q�B
q�B
q�B
q�B
q�B
rGB
r|B
r�B
r�B
r�B
s3B
sB
shB
s�B
t9B
tnB
t�B
t�B
t�B
u?B
uZB
u?B
uZB
uZB
uZB
uZB
u�B
u�B
vB
v+B
wB
w�B
w�B
w�B
w�B
w�B
w�B
x8B
xlB
x�B
y	B
y$B
yXB
yrB
y�B
y�B
y�B
z*B
zDB
zDB
zDB
zxB
z�B
z�B
zxB
z�B
{0B
{dB
{�B
{B
{�B
|6B
|�B
|�B
|�B
|�B
}�B
~B
}�B
}�B
}�B
}�B
~B
~BB
~BB
~(B
~BB
~�B
~�B
~�B
.B
HB
}B
}B
}B
}B
�B
�B
�B
�B
�iB
��B
��B
�UB
�oB
�oB
�UB
��B
��B
�'B
�AB
�[B
�uB
�uB
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220613184824  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220613185138  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220613185139  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220613185139                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220614035144  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220614035144  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220613185959                      G�O�G�O�G�O�                