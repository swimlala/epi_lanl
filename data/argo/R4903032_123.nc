CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-11-05T09:01:16Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �4   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �4   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �4   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �4   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20211105090116  20211105090116  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               {A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @٠S�G��1   @٠Tww��@;�V�u�c�     1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         {A   A   F   @�ff@�  A   A   A@  A`  A�  A�  A�33A�33A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�C3DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�G�@��H@��HAp�A=p�A]p�A}p�A��RA��A��A��AθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��GB�G�B��B��B��B��B��B��B��B��B��B��B��B��BîBǮBˮBϮBӮB׮BۮB߮B�B�B�B�B�B��B��B��C�
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
C/�C1�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,�)D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNo]DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�>D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�{A�(�A��A��A��A�"�A�&�A�(�A�+A�+A�+A�-A�+A�+A�-A�1'A�1'A�+A�1'A�7LA�9XA�9XA�5?A�&�A�5?A� �A� �A��A��A�A��A��
A�ƨAƕ�A�=qA�33A�dZA�1A���A���A�\)A��A���A�K�A�-A�5?A��A�;dA���A��A�\)A��/A�1'A���A��A�jA��A�l�A�1'A���A���A�1A�33A��yA���A�{A�bNA��A�VA�?}A�C�A���A��-A���A�+A� �A��`A��\A��PA�E�A��A�JA�VA�`BA�(�A���A�O�A~��A|��A{|�AxJAvffAr�9Ap~�ApE�Am`BAlE�Ak�Ajn�Ah5?AeVAbz�A_�A^bA]G�A]VA\�!A[l�AZbNAY�TAY��AX��AV��AU�AS�-AS&�AR��AQ�;AQXAPffAO��AN��AMC�AL$�AJ�9AI�hAH�AG��AF�AF-AE��AD�\ABr�A@�A?�^A?+A>�jA>JA=/A<(�A;%A9�;A8~�A7�TA7/A6M�A5C�A3|�A3x�A3G�A21'A1��A1�A05?A/\)A.v�A-G�A,�9A,jA+��A*�yA*r�A)��A(��A'�A&�/A&�DA&�+A&z�A&v�A&ZA%�PA#�A#��A#��A#��A#|�A"��A"A!�hA ~�A (�A  �A�PA�A{A�;A�A?}A%A=qA��AO�A�/A�A�A��A;dA��AI�AjAXAI�A�hAAr�AƨA�A�\AƨAA~�AM�A�AS�A�`AjAp�A
�HA
�A
 �A	A�A\)A�yA�7AVAA��A33An�A�wA �A ^5A b@��F@��@�C�@�  @�j@�z�@���@��#@��@�l�@�^@�D@�  @�;d@�ȴ@�@�z�@��y@�=q@�$�@��#@��@�Z@ߥ�@�=q@���@�9X@ۍP@�C�@�~�@�@ش9@�z�@�(�@Դ9@�^5@�Ĝ@ϥ�@Ώ\@�hs@�A�@��#@�A�@�t�@Ɵ�@��@�x�@î@��@�9X@���@�ƨ@���@���@�t�@�@�Ĝ@��@�;d@��!@�$�@��-@��-@��`@���@�=q@�&�@��@�9X@���@�33@�O�@�Q�@��F@�@��\@�J@��^@�7L@�I�@��w@��P@�;d@��\@�X@�%@���@��9@��9@���@��@�j@�  @��@�@��\@�n�@�-@�X@��@�;d@�
=@���@��H@���@�ȴ@��!@��\@�~�@�n�@�M�@��-@�(�@��@���@�J@�/@�%@��`@��D@�1'@�t�@�33@�@��!@�E�@���@��-@�x�@�X@��@��@��@�l�@�K�@��y@��@���@�p�@��/@�v�@���@���@�@���@��7@��@�p�@�X@�/@�A�@�t�@�C�@��@��@�^5@�5?@���@���@��@�Ĝ@�z�@�j@�I�@�1'@� �@��@�b@�b@�  @���@���@�\)@��@��!@�ff@�=q@��T@��7@�p�@�`B@�7L@���@���@��D@��@�Z@�A�@�(�@���@��;@���@�;d@��H@���@��+@�M�@�5?@�$�@���@��#@��^@�x�@�/@�V@��`@��9@��u@�Q�@�b@��m@���@�t�@�ȴ@��@���@��@�?}@�%@��9@�Z@��@�@�w@��@;d@~�y@~5?@}@|��@|1@{��@|1@|1@{t�@z�!@z~�@z^5@z=q@zJ@yG�@x��@x�9@xbN@x  @w�@w�@v��@vV@v5?@v@u�@tI�@s�m@s��@so@r�\@rJ@q��@q��@q��@q��@q�7@qhs@q&�@p�@o�@o;d@n��@n�y@n�R@nv�@n5?@n@m@m��@mO�@l��@l��@l��@lz�@l9X@l1@kƨ@kt�@kS�@k"�@j�@j^5@i��@h��@h�@hr�@h �@g��@gK�@f�@f��@e�@e��@eO�@d�@dz�@c�m@c33@b�!@bn�@b=q@b�@a�#@ax�@a7L@`��@`�u@`1'@_�@_�@_��@_\)@_+@^�@^E�@]�T@]�-@]O�@\�@\�/@\��@\z�@\�@[��@["�@Z�H@Z�!@Z~�@Z�@Y�@YX@Y&�@X�`@Xr�@W�@W�w@W|�@W;d@W
=@V�@Vȴ@V��@VE�@U��@T�@T�/@T��@T(�@S�
@S��@So@R�!@R�\@Rn�@R^5@R^5@R-@RJ@Q��@Q��@Qx�@Qhs@Q%@P�`@P��@P�9@PQ�@O�;@OK�@N�y@N��@N�+@N�+@Nv�@Nff@NV@NE�@N5?@N$�@N@M�T@M��@M?}@L�/@L9X@K��@KS�@Ko@J�@J�@J��@JM�@J�@I��@I�^@I�7@IX@I7L@HĜ@HA�@H �@G�;@G��@G|�@G;d@G+@F�y@F�+@F5?@E��@E/@D��@D�j@D��@DZ@D9X@Cƨ@Cƨ@Ct�@C33@C"�@C@B��@BM�@B-@A�7@A7L@A&�@@��@@��@@�9@@�u@@r�@@ �@?�@?�;@?|�@?
=@>ȴ@>��@>�+@>V@>5?@>@=��@=p�@=O�@<��@<�D@<I�@<9X@;�m@;��@;o@:�!@:~�@:�@9�^@9��@9�7@9X@9G�@9&�@8Ĝ@8��@8Q�@8 �@7�@7�w@7�@7K�@6�y@6ȴ@6��@6��@6�+@6V@6{@6{@5�T@5p�@4��@4�@4Z@49X@4�@3�
@3��@3dZ@3C�@3"�@2n�@1��@1�@1��@1�^@1��@1��@1�@0��@0�u@0r�@0 �@0  @/�;@/�@/l�@/K�@/;d@.�@.v�@-�@-��@-�-@-�h@-/@,�/@,�@,��@,�D@,Z@,(�@+�
@+t�@+C�@+C�@+"�@*�H@*��@*n�@*�@)��@)x�@)X@)�@(��@(�9@(�@( �@'�;@'�@'K�@'
=@&�@&�+@&E�@&$�@&{@%�@%��@%�-@%��@%�@%`B@%O�@$��@$��@$��@$j@$Z@$I�@$(�@#��@#�m@#ƨ@#�F@#��@#�@#C�@"�H@"n�@"J@!�^@!��@!�7@!hs@!7L@!�@!%@ �`@ ��@ r�@ A�@   @�@�;@�;@��@�w@;d@
=@��@�y@��@�@�R@��@��@V@@�T@�-@�@p�@`B@O�@O�@/@�@j@I�@1@�
@��@�@S�@C�@o@�H@�!@~�@M�@�@�#@�^@��@G�@��@��@Ĝ@��@�@1'@��@�P@;d@�y@ȴ@��@v�@V@@��@��@p�@O�@?}@V@�@�@��@�@z�@Z@I�@I�@(�@1@�m@��@��@��@��@��@�@S�@C�@33@"�@o@�@��@��@�!@~�@M�@=q@J@�#@��@�@�`@��@bN@1'@ �@�@��@�@�P@l�@K�@�y@�@�R@�R@�+@V@@�T@@�h@O�@V@�@�@�D@j@9X@(�@�@�@��@�
@�
@ƨ@�F@��@��@��@�@t�@t�@C�@C�@o@
^5@	�#@	��@	�7@	hs@	X@	X@	X@	X@	G�@	&�@	�@	%@Ĝ@r�@Q�@A�@1'@  @��@��@l�@K�@+@
=@�y@�@�@�@ȴ@�R@v�@E�@$�@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�{A�(�A��A��A��A�"�A�&�A�(�A�+A�+A�+A�-A�+A�+A�-A�1'A�1'A�+A�1'A�7LA�9XA�9XA�5?A�&�A�5?A� �A� �A��A��A�A��A��
A�ƨAƕ�A�=qA�33A�dZA�1A���A���A�\)A��A���A�K�A�-A�5?A��A�;dA���A��A�\)A��/A�1'A���A��A�jA��A�l�A�1'A���A���A�1A�33A��yA���A�{A�bNA��A�VA�?}A�C�A���A��-A���A�+A� �A��`A��\A��PA�E�A��A�JA�VA�`BA�(�A���A�O�A~��A|��A{|�AxJAvffAr�9Ap~�ApE�Am`BAlE�Ak�Ajn�Ah5?AeVAbz�A_�A^bA]G�A]VA\�!A[l�AZbNAY�TAY��AX��AV��AU�AS�-AS&�AR��AQ�;AQXAPffAO��AN��AMC�AL$�AJ�9AI�hAH�AG��AF�AF-AE��AD�\ABr�A@�A?�^A?+A>�jA>JA=/A<(�A;%A9�;A8~�A7�TA7/A6M�A5C�A3|�A3x�A3G�A21'A1��A1�A05?A/\)A.v�A-G�A,�9A,jA+��A*�yA*r�A)��A(��A'�A&�/A&�DA&�+A&z�A&v�A&ZA%�PA#�A#��A#��A#��A#|�A"��A"A!�hA ~�A (�A  �A�PA�A{A�;A�A?}A%A=qA��AO�A�/A�A�A��A;dA��AI�AjAXAI�A�hAAr�AƨA�A�\AƨAA~�AM�A�AS�A�`AjAp�A
�HA
�A
 �A	A�A\)A�yA�7AVAA��A33An�A�wA �A ^5A b@��F@��@�C�@�  @�j@�z�@���@��#@��@�l�@�^@�D@�  @�;d@�ȴ@�@�z�@��y@�=q@�$�@��#@��@�Z@ߥ�@�=q@���@�9X@ۍP@�C�@�~�@�@ش9@�z�@�(�@Դ9@�^5@�Ĝ@ϥ�@Ώ\@�hs@�A�@��#@�A�@�t�@Ɵ�@��@�x�@î@��@�9X@���@�ƨ@���@���@�t�@�@�Ĝ@��@�;d@��!@�$�@��-@��-@��`@���@�=q@�&�@��@�9X@���@�33@�O�@�Q�@��F@�@��\@�J@��^@�7L@�I�@��w@��P@�;d@��\@�X@�%@���@��9@��9@���@��@�j@�  @��@�@��\@�n�@�-@�X@��@�;d@�
=@���@��H@���@�ȴ@��!@��\@�~�@�n�@�M�@��-@�(�@��@���@�J@�/@�%@��`@��D@�1'@�t�@�33@�@��!@�E�@���@��-@�x�@�X@��@��@��@�l�@�K�@��y@��@���@�p�@��/@�v�@���@���@�@���@��7@��@�p�@�X@�/@�A�@�t�@�C�@��@��@�^5@�5?@���@���@��@�Ĝ@�z�@�j@�I�@�1'@� �@��@�b@�b@�  @���@���@�\)@��@��!@�ff@�=q@��T@��7@�p�@�`B@�7L@���@���@��D@��@�Z@�A�@�(�@���@��;@���@�;d@��H@���@��+@�M�@�5?@�$�@���@��#@��^@�x�@�/@�V@��`@��9@��u@�Q�@�b@��m@���@�t�@�ȴ@��@���@��@�?}@�%@��9@�Z@��@�@�w@��@;d@~�y@~5?@}@|��@|1@{��@|1@|1@{t�@z�!@z~�@z^5@z=q@zJ@yG�@x��@x�9@xbN@x  @w�@w�@v��@vV@v5?@v@u�@tI�@s�m@s��@so@r�\@rJ@q��@q��@q��@q��@q�7@qhs@q&�@p�@o�@o;d@n��@n�y@n�R@nv�@n5?@n@m@m��@mO�@l��@l��@l��@lz�@l9X@l1@kƨ@kt�@kS�@k"�@j�@j^5@i��@h��@h�@hr�@h �@g��@gK�@f�@f��@e�@e��@eO�@d�@dz�@c�m@c33@b�!@bn�@b=q@b�@a�#@ax�@a7L@`��@`�u@`1'@_�@_�@_��@_\)@_+@^�@^E�@]�T@]�-@]O�@\�@\�/@\��@\z�@\�@[��@["�@Z�H@Z�!@Z~�@Z�@Y�@YX@Y&�@X�`@Xr�@W�@W�w@W|�@W;d@W
=@V�@Vȴ@V��@VE�@U��@T�@T�/@T��@T(�@S�
@S��@So@R�!@R�\@Rn�@R^5@R^5@R-@RJ@Q��@Q��@Qx�@Qhs@Q%@P�`@P��@P�9@PQ�@O�;@OK�@N�y@N��@N�+@N�+@Nv�@Nff@NV@NE�@N5?@N$�@N@M�T@M��@M?}@L�/@L9X@K��@KS�@Ko@J�@J�@J��@JM�@J�@I��@I�^@I�7@IX@I7L@HĜ@HA�@H �@G�;@G��@G|�@G;d@G+@F�y@F�+@F5?@E��@E/@D��@D�j@D��@DZ@D9X@Cƨ@Cƨ@Ct�@C33@C"�@C@B��@BM�@B-@A�7@A7L@A&�@@��@@��@@�9@@�u@@r�@@ �@?�@?�;@?|�@?
=@>ȴ@>��@>�+@>V@>5?@>@=��@=p�@=O�@<��@<�D@<I�@<9X@;�m@;��@;o@:�!@:~�@:�@9�^@9��@9�7@9X@9G�@9&�@8Ĝ@8��@8Q�@8 �@7�@7�w@7�@7K�@6�y@6ȴ@6��@6��@6�+@6V@6{@6{@5�T@5p�@4��@4�@4Z@49X@4�@3�
@3��@3dZ@3C�@3"�@2n�@1��@1�@1��@1�^@1��@1��@1�@0��@0�u@0r�@0 �@0  @/�;@/�@/l�@/K�@/;d@.�@.v�@-�@-��@-�-@-�h@-/@,�/@,�@,��@,�D@,Z@,(�@+�
@+t�@+C�@+C�@+"�@*�H@*��@*n�@*�@)��@)x�@)X@)�@(��@(�9@(�@( �@'�;@'�@'K�@'
=@&�@&�+@&E�@&$�@&{@%�@%��@%�-@%��@%�@%`B@%O�@$��@$��@$��@$j@$Z@$I�@$(�@#��@#�m@#ƨ@#�F@#��@#�@#C�@"�H@"n�@"J@!�^@!��@!�7@!hs@!7L@!�@!%@ �`@ ��@ r�@ A�@   @�@�;@�;@��@�w@;d@
=@��@�y@��@�@�R@��@��@V@@�T@�-@�@p�@`B@O�@O�@/@�@j@I�@1@�
@��@�@S�@C�@o@�H@�!@~�@M�@�@�#@�^@��@G�@��@��@Ĝ@��@�@1'@��@�P@;d@�y@ȴ@��@v�@V@@��@��@p�@O�@?}@V@�@�@��@�@z�@Z@I�@I�@(�@1@�m@��@��@��@��@��@�@S�@C�@33@"�@o@�@��@��@�!@~�@M�@=q@J@�#@��@�@�`@��@bN@1'@ �@�@��@�@�P@l�@K�@�y@�@�R@�R@�+@V@@�T@@�h@O�@V@�@�@�D@j@9X@(�@�@�@��@�
@�
@ƨ@�F@��@��@��@�@t�@t�@C�@C�@o@
^5@	�#@	��@	�7@	hs@	X@	X@	X@	X@	G�@	&�@	�@	%@Ĝ@r�@Q�@A�@1'@  @��@��@l�@K�@+@
=@�y@�@�@�@ȴ@�R@v�@E�@$�@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	7B	7B+B%B%BB%BBBBBBBBBBBBBBBBB��B  B��B��B��B��B��B��B��B��B�B��BM�B�BbBB��B��B�B�B�B�sB�TB�)B�B��B�}B�LB��B��B�B}�B� B~�Bx�Bu�Bp�Bk�BcTBG�B>wB<jB49B&�B�BPB��B�mB��B�qB�B��B��B�Bk�BZBG�B(�B�B	7B��B�mB�
B�wB�'B��B��B�Bt�BaHBO�BK�B>wB5?B0!B'�B�B1B��B�B�fB�TB�NB�HB�)B�
B��B��B��BŢB�qB�RB�?B�FB�FB�3B�B�B��B��B��B��B�{B�hB�VB�DB�7B�+B�B� B{�Bw�Bu�Bt�Bs�Bo�Bm�BiyBffBbNB^5B]/BZBXBP�BO�BO�BL�BJ�BJ�BH�BF�BC�B?}B<jB:^B8RB5?B2-B/B.B(�B&�B$�B$�B$�B$�B#�B!�B�B�B�B�B�B{BuBbBbBVBPBPBDB
=B	7B1B+B%BBBBBB  B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�sB
�mB
�fB
�`B
�ZB
�TB
�NB
�HB
�;B
�;B
�/B
�)B
�B
�B
�
B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
ȴB
ǮB
ƨB
ĜB
�}B
�}B
�qB
�dB
�^B
�^B
�XB
�XB
�XB
�XB
�RB
�RB
�RB
�LB
�LB
�LB
�XB
�XB
�^B
�XB
�XB
�^B
�^B
�^B
�dB
�^B
�dB
�^B
�dB
�^B
�XB
�jB
�^B
�dB
�jB
�qB
�wB
�wB
B
B
ÖB
ÖB
ÖB
B
ŢB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�B
�B
�B
�B
�/B
�HB
�TB
�`B
�`B
�`B
�fB
�B
�B
�B
�B
��B
��B
��B
��B
��B  B  BBB	7B
=BDBDBDBJBJBJBVBbBoB{B�B�B�B"�B$�B%�B%�B&�B'�B&�B'�B(�B(�B(�B(�B-B49B49B49B?}BB�BC�BD�BF�BH�BL�BM�BN�BP�BR�BS�BT�BT�BVBXBZB^5BaHBbNBdZBhsBjBl�Bp�B� B�B�B�B�B�B�B�B�B�%B�JB�bB�oB�{B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�-B�9B�LB�XB�^B�jB�}B��B��BBĜBǮBǮBǮBɺBɺB��B��B��B��B��B��B��B��B��B�B�B�B�B�)B�5B�BB�BB�HB�NB�ZB�`B�mB�sB�B�B�B��B��B��B��B��BBBB%B	7B
=BDBJBbBoB{B�B�B�B�B�B �B!�B#�B#�B%�B)�B+B,B.B0!B2-B5?B7LB8RB9XB:^B<jBA�BA�BB�BD�BG�BI�BK�BL�BL�BL�BM�BM�BN�BP�BS�BVBZB[#B\)B^5B^5B_;B`BBaHBbNBcTBdZBe`Be`BffBgmBhsBiyBjBk�Bk�Bm�Bm�Bo�Br�Br�Br�Bt�Bv�Bw�Bx�Bz�Bz�B{�B|�B}�B� B�B�B�B�B�%B�+B�7B�=B�JB�PB�\B�hB�oB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�-B�-B�3B�9B�?B�FB�LB�RB�RB�XB�XB�XB�^B�dB�dB�jB�jB�qB�wB�wB�wB�}B��B��BBĜBĜBŢBŢBŢBŢBƨBƨBƨBƨBǮBǮBȴBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�
B�B�B�B�#B�)B�)B�)B�)B�/B�/B�5B�5B�;B�;B�;B�BB�HB�NB�TB�TB�ZB�ZB�ZB�`B�`B�fB�fB�fB�mB�sB�yB�B�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBBBBBBB%B%B+B+B1B1B1B	7B	7B
=B
=B
=B
=B
=BDBDBJBJBJBJBPBPBPBVB\B\B\B\BbBbBhBhBhBoBoBuBuB{B{B{B{B�B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B �B!�B!�B!�B"�B"�B"�B"�B"�B"�B#�B#�B#�B$�B$�B$�B%�B%�B%�B%�B&�B&�B&�B'�B'�B'�B'�B'�B(�B(�B)�B)�B)�B)�B+B+B+B,B,B,B-B-B-B.B.B.B/B/B/B/B/B/B/B0!B0!B0!B0!B0!B1'B1'B1'B1'B2-B2-B2-B1'B2-B2-B2-B2-B2-B33B33B33B33B33B33B49B49B49B5?B5?B6FB6FB7LB8RB8RB8RB8RB9XB9XB9XB9XB9XB:^B:^B:^B:^B;dB;dB<jB<jB<jB<jB<jB=qB=qB=qB=qB<jB<jB=qB=qB=qB=qB>wB>wB>wB>wB?}B?}B?}B?}B?}B?}B>wB>wB?}B@�B@�BA�BA�BA�BA�BA�BA�BB�BC�BC�BC�BC�BD�BD�BD�BE�BE�BE�BE�BF�BF�BF�BF�BG�BG�BG�BG�BG�BG�BG�BH�BH�BI�BI�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 B	7B	7B+B%B%BB%BBBBBBBBBBBBBBBBB��B  B��B��B��B��B��B��B��B��B�B��BM�B�BbBB��B��B�B�B�B�sB�TB�)B�B��B�}B�LB��B��B�B}�B� B~�Bx�Bu�Bp�Bk�BcTBG�B>wB<jB49B&�B�BPB��B�mB��B�qB�B��B��B�Bk�BZBG�B(�B�B	7B��B�mB�
B�wB�'B��B��B�Bt�BaHBO�BK�B>wB5?B0!B'�B�B1B��B�B�fB�TB�NB�HB�)B�
B��B��B��BŢB�qB�RB�?B�FB�FB�3B�B�B��B��B��B��B�{B�hB�VB�DB�7B�+B�B� B{�Bw�Bu�Bt�Bs�Bo�Bm�BiyBffBbNB^5B]/BZBXBP�BO�BO�BL�BJ�BJ�BH�BF�BC�B?}B<jB:^B8RB5?B2-B/B.B(�B&�B$�B$�B$�B$�B#�B!�B�B�B�B�B�B{BuBbBbBVBPBPBDB
=B	7B1B+B%BBBBBB  B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�sB
�mB
�fB
�`B
�ZB
�TB
�NB
�HB
�;B
�;B
�/B
�)B
�B
�B
�
B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
ȴB
ǮB
ƨB
ĜB
�}B
�}B
�qB
�dB
�^B
�^B
�XB
�XB
�XB
�XB
�RB
�RB
�RB
�LB
�LB
�LB
�XB
�XB
�^B
�XB
�XB
�^B
�^B
�^B
�dB
�^B
�dB
�^B
�dB
�^B
�XB
�jB
�^B
�dB
�jB
�qB
�wB
�wB
B
B
ÖB
ÖB
ÖB
B
ŢB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�B
�B
�B
�B
�/B
�HB
�TB
�`B
�`B
�`B
�fB
�B
�B
�B
�B
��B
��B
��B
��B
��B  B  BBB	7B
=BDBDBDBJBJBJBVBbBoB{B�B�B�B"�B$�B%�B%�B&�B'�B&�B'�B(�B(�B(�B(�B-B49B49B49B?}BB�BC�BD�BF�BH�BL�BM�BN�BP�BR�BS�BT�BT�BVBXBZB^5BaHBbNBdZBhsBjBl�Bp�B� B�B�B�B�B�B�B�B�B�%B�JB�bB�oB�{B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�-B�9B�LB�XB�^B�jB�}B��B��BBĜBǮBǮBǮBɺBɺB��B��B��B��B��B��B��B��B��B�B�B�B�B�)B�5B�BB�BB�HB�NB�ZB�`B�mB�sB�B�B�B��B��B��B��B��BBBB%B	7B
=BDBJBbBoB{B�B�B�B�B�B �B!�B#�B#�B%�B)�B+B,B.B0!B2-B5?B7LB8RB9XB:^B<jBA�BA�BB�BD�BG�BI�BK�BL�BL�BL�BM�BM�BN�BP�BS�BVBZB[#B\)B^5B^5B_;B`BBaHBbNBcTBdZBe`Be`BffBgmBhsBiyBjBk�Bk�Bm�Bm�Bo�Br�Br�Br�Bt�Bv�Bw�Bx�Bz�Bz�B{�B|�B}�B� B�B�B�B�B�%B�+B�7B�=B�JB�PB�\B�hB�oB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�-B�-B�3B�9B�?B�FB�LB�RB�RB�XB�XB�XB�^B�dB�dB�jB�jB�qB�wB�wB�wB�}B��B��BBĜBĜBŢBŢBŢBŢBƨBƨBƨBƨBǮBǮBȴBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�
B�B�B�B�#B�)B�)B�)B�)B�/B�/B�5B�5B�;B�;B�;B�BB�HB�NB�TB�TB�ZB�ZB�ZB�`B�`B�fB�fB�fB�mB�sB�yB�B�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBBBBBBB%B%B+B+B1B1B1B	7B	7B
=B
=B
=B
=B
=BDBDBJBJBJBJBPBPBPBVB\B\B\B\BbBbBhBhBhBoBoBuBuB{B{B{B{B�B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B �B!�B!�B!�B"�B"�B"�B"�B"�B"�B#�B#�B#�B$�B$�B$�B%�B%�B%�B%�B&�B&�B&�B'�B'�B'�B'�B'�B(�B(�B)�B)�B)�B)�B+B+B+B,B,B,B-B-B-B.B.B.B/B/B/B/B/B/B/B0!B0!B0!B0!B0!B1'B1'B1'B1'B2-B2-B2-B1'B2-B2-B2-B2-B2-B33B33B33B33B33B33B49B49B49B5?B5?B6FB6FB7LB8RB8RB8RB8RB9XB9XB9XB9XB9XB:^B:^B:^B:^B;dB;dB<jB<jB<jB<jB<jB=qB=qB=qB=qB<jB<jB=qB=qB=qB=qB>wB>wB>wB>wB?}B?}B?}B?}B?}B?}B>wB>wB?}B@�B@�BA�BA�BA�BA�BA�BA�BB�BC�BC�BC�BC�BD�BD�BD�BE�BE�BE�BE�BF�BF�BF�BF�BG�BG�BG�BG�BG�BG�BG�BH�BH�BI�BI�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.16 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20211105090116                              AO  ARCAADJP                                                                    20211105090116    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20211105090116  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20211105090116  QCF$                G�O�G�O�G�O�8000            