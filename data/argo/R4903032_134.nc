CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-02-23T10:01:04Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  o`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220223100104  20220223100104  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @ٻ�骛1   @ٻ֛r@<;"��`B�c܃n��1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�3D�C3D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�<�D�|�Dܼ�D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@��H@��HAp�A=p�A]p�A}p�A��RA��RA��RA��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bn��Bw\)B\)B��B��B��B��B��B�z�B��B��B��B��B��B��B��B��B��B��BîBǮBˮBϮBӮB׮BۮB߮B�B�B�B�B�B��B��B��C�C�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%|)D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Duu�Du��Dvu�Dv��Dwu�Dw��Dxu�Dx��Dyu�Dy��Dzu�Dz��D{u�D{��D|u�D|��D}u�D}��D~u�D~��Du�D��D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D��D�>D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�7�D�w�Dܷ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D��D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�{A�v�A�\)A�I�A�7LA�&�A��A�A��mA��FA�ffA��A��A���A�?}A�VA��/A���A�n�A�5?A�ƨA�M�A�
=A�ƨA��FA���A�ffA�A�p�A�bA��A��A�+A�{A��A�-A��
A�\)A�/A�&�A�oA��9A�&�A��mA�~�A�(�A��A��FA�bNA��yA�33A��!A�VA�JA�K�A��A��A�M�A��yA�l�A��+A�ȴA�?}A��9A��A��DA�Q�A�ffA��HA�A�=qA��mA�
=A���A�ffA�r�A�|�A���A�`BA�-A���A��RA�n�A�A|9XA{7LAz��Azz�AzffAyC�Au+At{AsVArĜAr~�Ar �Aq�
Aq�-Aql�AqK�Aq�Ap�jApVAo7LAnE�Al��AiS�Af~�AeXAdE�Ab�!Aa�#A`�\A_�A^v�A\^5A[�AZ�AZ�AZ�AZA�AY�^AX�AXbNAW;dAU\)ATbNASS�AR�!AR-AP�AOXAN��AM�#AM��AM+AL�!AL�AK?}AJ �AI��AIO�AH��AH�AG&�AF�\AE�wAE�ADM�AC|�ACoAAA?�A>�9A=O�A<�yA<M�A;��A;p�A:VA9l�A9�A9\)A9�A8ZA7�TA7"�A5l�A5?}A4�/A3�A3
=A2��A1�#A133A0=qA/��A.ffA-��A,��A,  A+"�A*^5A)A)x�A)
=A(��A'�A&��A&JA%��A%p�A$��A#A#"�A"ĜA"M�A"1A!��A!oA v�A��AdZA-A��A&�A��A(�A�A�AVA��AbAXA�DA�;A��A�AS�A��A�\A(�A�hA�jAXA�A�^A�HAXA
I�A
$�A	�A	��A	�A	t�A	XA	&�A	
=A�yA�HAȴA�A��AA��A��AK�AI�A �y@�o@��u@�S�@��@�%@��@��\@��@�ƨ@�E�@�x�@��@�{@�33@���@�A�@旍@�/@�\)@�J@���@�@�O�@�
=@���@�ȴ@ѡ�@�  @Ͼw@υ@�C�@��H@�$�@Ͳ-@�x�@��`@�dZ@��#@ȣ�@Ɵ�@š�@�&�@�Ĝ@�9X@þw@��y@�v�@�E�@��-@�/@��/@�Q�@���@�O�@��9@�33@��@�&�@��@�  @���@�dZ@�ȴ@��\@�M�@��@��h@��/@�I�@�o@��h@�7L@���@�S�@���@���@�@��@���@���@�Ĝ@���@��
@��y@�J@�x�@��9@�Q�@��
@�|�@��R@�{@��@���@��@��D@�(�@��;@�dZ@�E�@�{@��@��h@�7L@�r�@�33@��+@�$�@��h@���@��P@�l�@�C�@���@�ff@�E�@�E�@�=q@�-@�@�?}@��@��u@���@�"�@���@���@�v�@�@�/@�V@��@���@�r�@�A�@��@���@�C�@���@���@�V@�{@�7L@�V@�%@���@��`@��j@���@�z�@�r�@� �@���@�\)@���@�M�@�5?@���@�&�@���@��@�A�@���@�
=@�{@��@��9@�I�@�b@�ƨ@��@���@��+@�n�@�ff@�M�@�E�@�-@�@��@�@�hs@��@��@��@�r�@�Z@�b@�ƨ@��P@�dZ@�S�@�;d@�33@��@��y@��@��+@�$�@���@��h@�G�@��@��j@�z�@�1@~��@~v�@~@}?}@|��@{�F@{t�@{33@z^5@y�7@yG�@x��@x�u@xr�@x �@w��@w�@v��@vff@vE�@v$�@v@u�@tZ@sdZ@s33@s@rn�@rM�@r�@q��@q�@p�`@p�`@p�`@p�`@p�`@p�`@p�9@p�u@pr�@o�@o|�@o�@n�@n{@m�@l��@l�/@l�/@l�j@l�j@l�@l��@lZ@ko@i�@i��@i��@iX@i7L@h��@h��@h��@h��@h�@hr�@hbN@hbN@hQ�@hA�@g�@g�w@gl�@g\)@g+@f�R@f��@fv�@f5?@f@e��@e`B@d��@d��@dZ@d(�@c��@c�@cS�@co@b�@b�@b�@b��@b�!@b�\@bM�@b�@a�#@a�7@`�9@`bN@_��@_�@^�+@^E�@^{@]�T@]�h@]O�@]/@\�D@\�@[ƨ@[dZ@[S�@[33@[@Z��@Z��@Z��@Z�\@ZJ@Y�7@YX@Y&�@X�`@XĜ@XA�@W��@WK�@V�@V��@V��@V��@Vv�@VV@V@U��@U�@U/@T�@Tz�@TI�@T(�@S��@SS�@R�@R~�@R=q@Q��@Q�^@Qx�@P��@P1'@O�w@O\)@O
=@N�@N��@NE�@M�T@L��@LZ@LZ@L9X@K��@K@J��@Jn�@I�^@Ihs@I�@H�@H1'@G�@G�w@Gl�@GK�@Fv�@E��@E��@Ep�@E/@E�@EV@D��@D�/@D�j@D�D@DZ@CdZ@C"�@B�@B��@B��@B��@B=q@A�#@A��@Ax�@A7L@@��@@��@@A�@@ �@?�@>�y@>5?@=�@=�T@=��@=@=@=�h@=`B@=O�@<�/@<j@<I�@<9X@<(�@;��@;��@;dZ@;"�@:�H@:�\@:~�@:M�@9�@9�^@9��@9��@9x�@9G�@8�`@8�@8Q�@8 �@7�@7�;@7��@7��@7K�@7�@6��@6�@6��@6v�@6E�@6@5@5?}@4�@4�D@4Z@4I�@49X@4�@3�m@3ƨ@3��@3dZ@3C�@3o@2�@2�@2�!@2=q@1��@1��@1�^@1��@17L@0��@0�`@0Ĝ@0r�@0b@/�@/|�@.�y@.�R@.��@.�+@.�+@.v�@.ff@.V@.E�@-�@-@-�-@-��@-p�@-/@,��@,�/@,��@,z�@,j@,�@+��@+ƨ@+��@+t�@+33@+"�@+o@+o@+@*�H@*^5@*-@)�@)�^@)��@)G�@(�`@(��@(�9@(��@(��@(r�@(�u@( �@'�;@'�@'��@'��@'�P@'\)@'K�@'
=@&ȴ@&�+@&V@&E�@%�@%��@%`B@%/@$��@$��@$�D@$�@#�
@#ƨ@#��@#t�@#dZ@#S�@#S�@#o@"��@"��@"�!@"��@"~�@"=q@!��@!��@!�#@!�#@!��@!�^@!�^@!��@!hs@!X@!&�@ ��@ �9@ �u@ �@ Q�@  �@ b@ b@�@��@�P@|�@
=@��@$�@{@�T@@�-@`B@�@�/@�@�D@Z@I�@I�@I�@(�@ƨ@C�@�@�!@~�@M�@-@-@�@�#@�^@�7@x�@7L@&�@��@�@bN@ �@��@�P@K�@+@�y@�R@��@�+@V@E�@5?@@@��@?}@V@�@z�@j@j@Z@Z@I�@9X@1@�F@C�@o@�@�@�H@�!@��@�\@n�@=q@-@��@��@X@G�@G�@7L@&�@��@��@�9@�@�@r�@Q�@�@�@�P@l�@K�@�@��@��@ȴ@��@v�@E�@$�@{@{@@��@�h@O�@V@�/@�/@�/@��@�j@�j@�@j@I�@I�@9X@(�@�m@��@t�@dZ@33@33@"�@@
��@
n�@
�@	��@	�#@	�^@	�7@	7L@��@�u@1'@  @�@��@�w@�@\)@+@;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�{A�v�A�\)A�I�A�7LA�&�A��A�A��mA��FA�ffA��A��A���A�?}A�VA��/A���A�n�A�5?A�ƨA�M�A�
=A�ƨA��FA���A�ffA�A�p�A�bA��A��A�+A�{A��A�-A��
A�\)A�/A�&�A�oA��9A�&�A��mA�~�A�(�A��A��FA�bNA��yA�33A��!A�VA�JA�K�A��A��A�M�A��yA�l�A��+A�ȴA�?}A��9A��A��DA�Q�A�ffA��HA�A�=qA��mA�
=A���A�ffA�r�A�|�A���A�`BA�-A���A��RA�n�A�A|9XA{7LAz��Azz�AzffAyC�Au+At{AsVArĜAr~�Ar �Aq�
Aq�-Aql�AqK�Aq�Ap�jApVAo7LAnE�Al��AiS�Af~�AeXAdE�Ab�!Aa�#A`�\A_�A^v�A\^5A[�AZ�AZ�AZ�AZA�AY�^AX�AXbNAW;dAU\)ATbNASS�AR�!AR-AP�AOXAN��AM�#AM��AM+AL�!AL�AK?}AJ �AI��AIO�AH��AH�AG&�AF�\AE�wAE�ADM�AC|�ACoAAA?�A>�9A=O�A<�yA<M�A;��A;p�A:VA9l�A9�A9\)A9�A8ZA7�TA7"�A5l�A5?}A4�/A3�A3
=A2��A1�#A133A0=qA/��A.ffA-��A,��A,  A+"�A*^5A)A)x�A)
=A(��A'�A&��A&JA%��A%p�A$��A#A#"�A"ĜA"M�A"1A!��A!oA v�A��AdZA-A��A&�A��A(�A�A�AVA��AbAXA�DA�;A��A�AS�A��A�\A(�A�hA�jAXA�A�^A�HAXA
I�A
$�A	�A	��A	�A	t�A	XA	&�A	
=A�yA�HAȴA�A��AA��A��AK�AI�A �y@�o@��u@�S�@��@�%@��@��\@��@�ƨ@�E�@�x�@��@�{@�33@���@�A�@旍@�/@�\)@�J@���@�@�O�@�
=@���@�ȴ@ѡ�@�  @Ͼw@υ@�C�@��H@�$�@Ͳ-@�x�@��`@�dZ@��#@ȣ�@Ɵ�@š�@�&�@�Ĝ@�9X@þw@��y@�v�@�E�@��-@�/@��/@�Q�@���@�O�@��9@�33@��@�&�@��@�  @���@�dZ@�ȴ@��\@�M�@��@��h@��/@�I�@�o@��h@�7L@���@�S�@���@���@�@��@���@���@�Ĝ@���@��
@��y@�J@�x�@��9@�Q�@��
@�|�@��R@�{@��@���@��@��D@�(�@��;@�dZ@�E�@�{@��@��h@�7L@�r�@�33@��+@�$�@��h@���@��P@�l�@�C�@���@�ff@�E�@�E�@�=q@�-@�@�?}@��@��u@���@�"�@���@���@�v�@�@�/@�V@��@���@�r�@�A�@��@���@�C�@���@���@�V@�{@�7L@�V@�%@���@��`@��j@���@�z�@�r�@� �@���@�\)@���@�M�@�5?@���@�&�@���@��@�A�@���@�
=@�{@��@��9@�I�@�b@�ƨ@��@���@��+@�n�@�ff@�M�@�E�@�-@�@��@�@�hs@��@��@��@�r�@�Z@�b@�ƨ@��P@�dZ@�S�@�;d@�33@��@��y@��@��+@�$�@���@��h@�G�@��@��j@�z�@�1@~��@~v�@~@}?}@|��@{�F@{t�@{33@z^5@y�7@yG�@x��@x�u@xr�@x �@w��@w�@v��@vff@vE�@v$�@v@u�@tZ@sdZ@s33@s@rn�@rM�@r�@q��@q�@p�`@p�`@p�`@p�`@p�`@p�`@p�9@p�u@pr�@o�@o|�@o�@n�@n{@m�@l��@l�/@l�/@l�j@l�j@l�@l��@lZ@ko@i�@i��@i��@iX@i7L@h��@h��@h��@h��@h�@hr�@hbN@hbN@hQ�@hA�@g�@g�w@gl�@g\)@g+@f�R@f��@fv�@f5?@f@e��@e`B@d��@d��@dZ@d(�@c��@c�@cS�@co@b�@b�@b�@b��@b�!@b�\@bM�@b�@a�#@a�7@`�9@`bN@_��@_�@^�+@^E�@^{@]�T@]�h@]O�@]/@\�D@\�@[ƨ@[dZ@[S�@[33@[@Z��@Z��@Z��@Z�\@ZJ@Y�7@YX@Y&�@X�`@XĜ@XA�@W��@WK�@V�@V��@V��@V��@Vv�@VV@V@U��@U�@U/@T�@Tz�@TI�@T(�@S��@SS�@R�@R~�@R=q@Q��@Q�^@Qx�@P��@P1'@O�w@O\)@O
=@N�@N��@NE�@M�T@L��@LZ@LZ@L9X@K��@K@J��@Jn�@I�^@Ihs@I�@H�@H1'@G�@G�w@Gl�@GK�@Fv�@E��@E��@Ep�@E/@E�@EV@D��@D�/@D�j@D�D@DZ@CdZ@C"�@B�@B��@B��@B��@B=q@A�#@A��@Ax�@A7L@@��@@��@@A�@@ �@?�@>�y@>5?@=�@=�T@=��@=@=@=�h@=`B@=O�@<�/@<j@<I�@<9X@<(�@;��@;��@;dZ@;"�@:�H@:�\@:~�@:M�@9�@9�^@9��@9��@9x�@9G�@8�`@8�@8Q�@8 �@7�@7�;@7��@7��@7K�@7�@6��@6�@6��@6v�@6E�@6@5@5?}@4�@4�D@4Z@4I�@49X@4�@3�m@3ƨ@3��@3dZ@3C�@3o@2�@2�@2�!@2=q@1��@1��@1�^@1��@17L@0��@0�`@0Ĝ@0r�@0b@/�@/|�@.�y@.�R@.��@.�+@.�+@.v�@.ff@.V@.E�@-�@-@-�-@-��@-p�@-/@,��@,�/@,��@,z�@,j@,�@+��@+ƨ@+��@+t�@+33@+"�@+o@+o@+@*�H@*^5@*-@)�@)�^@)��@)G�@(�`@(��@(�9@(��@(��@(r�@(�u@( �@'�;@'�@'��@'��@'�P@'\)@'K�@'
=@&ȴ@&�+@&V@&E�@%�@%��@%`B@%/@$��@$��@$�D@$�@#�
@#ƨ@#��@#t�@#dZ@#S�@#S�@#o@"��@"��@"�!@"��@"~�@"=q@!��@!��@!�#@!�#@!��@!�^@!�^@!��@!hs@!X@!&�@ ��@ �9@ �u@ �@ Q�@  �@ b@ b@�@��@�P@|�@
=@��@$�@{@�T@@�-@`B@�@�/@�@�D@Z@I�@I�@I�@(�@ƨ@C�@�@�!@~�@M�@-@-@�@�#@�^@�7@x�@7L@&�@��@�@bN@ �@��@�P@K�@+@�y@�R@��@�+@V@E�@5?@@@��@?}@V@�@z�@j@j@Z@Z@I�@9X@1@�F@C�@o@�@�@�H@�!@��@�\@n�@=q@-@��@��@X@G�@G�@7L@&�@��@��@�9@�@�@r�@Q�@�@�@�P@l�@K�@�@��@��@ȴ@��@v�@E�@$�@{@{@@��@�h@O�@V@�/@�/@�/@��@�j@�j@�@j@I�@I�@9X@(�@�m@��@t�@dZ@33@33@"�@@
��@
n�@
�@	��@	�#@	�^@	�7@	7L@��@�u@1'@  @�@��@�w@�@\)@+@;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B�1Bs�BcTBT�BQ�BN�BK�BG�BF�B?}B49B/B(�B(�B&�B"�B�B
=B��BBB+BDBB��B�B�TB�HB�NB�NB�/B��B��BȴBĜB��B�wB�^B�9B��B��B��B��B�bB�+B�+B~�B{�Bq�BaHBS�BJ�B@�B6FB.B+B�B{B  B�B�B�/B�^B��Bz�B[#B2-B{B��B�NB��B�dB��B�bB�7B�%B�B�B�Bm�BjBe`BdZBcTBcTBaHBbNBaHBaHBbNB`BB_;B[#BVBO�B@�B5?B/B.B)�B+B$�B'�B&�B�B�B�B�B�B�B{BoBbBJBB��B��B��B��B�B�mB�ZB�TB�NB�BB�5B�#B�B��B��B��B��B��BŢB��B�dB�FB�!B��B��B��B��B��B�VB�JB�DB�1B�%B�B�B�B�DB�DB�=B�+B�Bw�Bu�Bv�Bn�Bl�Bk�BgmBdZBaHB^5BXBT�BP�BL�BI�BF�BC�BB�B@�B=qB<jB8RB8RB7LB6FB5?B2-B0!B0!B0!B0!B.B,B(�B&�B$�B �B�B�B�B�B�BoBbBPB	7B%BBB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�sB
�sB
�ZB
�TB
�TB
�TB
�NB
�NB
�NB
�NB
�NB
�HB
�HB
�BB
�;B
�;B
�/B
�B
�B
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
ǮB
ǮB
ƨB
ŢB
ĜB
ĜB
B
��B
��B
��B
��B
��B
�}B
�}B
�wB
�qB
B
B
B
ÖB
ÖB
ÖB
ÖB
ĜB
ĜB
ŢB
ĜB
ŢB
ŢB
ǮB
ȴB
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
��B
��B
��B
��B
�B
�#B
�)B
�/B
�;B
�;B
�;B
�NB
�NB
�TB
�TB
�ZB
�mB
�mB
�B
�B
�B
�B
��B
��B
��B
��B  B  B  B  BBB+BDBJB\BbBoBuB�B�B�B�B�B!�B"�B#�B&�B,B,B-B/B1'B49B:^B;dB=qB?}BD�BK�BL�BL�BN�BS�BS�BS�BT�BT�BXB\)B]/B`BBgmBjBm�Bn�Bo�Bs�Bw�Bx�By�Bz�B|�B}�B~�B�B�B�1B�7B�=B�JB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�FB�RB�^B�}BÖB��B��B��B�B�#B�/B�TB�`B�mB�sB�sB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B��B  B  BBBBBB%B	7BJBVBbBhB{B�B�B�B!�B"�B%�B(�B-B.B/B33B6FB7LB:^B;dB;dB=qB>wBB�BD�BE�BF�BG�BH�BK�BP�BT�BVBW
BYBYBZB\)B^5B_;B_;B`BB_;B`BB`BBaHBbNBbNBe`BffBhsBiyBjBl�Bl�Bl�Bn�Bo�Bo�Bp�Bp�Bp�Bt�Bw�Bw�Bx�By�Bz�B|�B}�B� B� B� B�B�B�B�B�B�%B�+B�7B�7B�=B�JB�JB�PB�\B�\B�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�'B�-B�-B�3B�9B�?B�FB�FB�FB�FB�RB�^B�dB�dB�jB�jB�qB�}B��BBÖBBBÖBÖBĜBŢBŢBƨBǮBǮBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�#B�#B�#B�#B�/B�5B�5B�;B�HB�HB�NB�ZB�ZB�`B�`B�fB�fB�sB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBBBBBBBBBBBB%B+B+B+B1B1B1B1B	7B	7B
=B
=BDBDBDBJBJBPBVBVB\B\B\B\BbBbBbBbBhBhBoBhBoBuBuB{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B �B!�B!�B!�B"�B"�B#�B#�B#�B$�B%�B$�B%�B%�B%�B%�B%�B&�B&�B'�B'�B'�B'�B'�B(�B(�B(�B)�B)�B)�B+B+B,B+B,B,B-B.B.B.B.B/B/B/B/B0!B0!B0!B1'B1'B1'B2-B2-B1'B2-B2-B2-B33B2-B33B33B33B33B49B5?B49B49B5?B5?B5?B5?B5?B6FB6FB6FB7LB8RB8RB8RB8RB9XB9XB9XB:^B:^B;dB:^B;dB;dB;dB;dB<jB<jB=qB>wB=qB>wB>wB>wB>wB>wB?}B?}B@�B@�B@�B@�B@�BA�BA�BB�BB�BB�BC�BC�BC�BD�BD�BD�BE�BE�BE�BE�BE�BE�BF�BF�BG�BH�BH�BG�BH�BG�BH�BH�BH�BI�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BK�BL�BL�BM�BL�BM�BM�BM�BN�BN�BN�BN�BN�BN�BO�BO�BP�BP�BP�BP�BP�BP�BQ�BQ�BR�BR�BR�BR�BR�BR�BR�BS�BS�BT�BT�BT�BT�BT�BT�BT�BT�BVBVBVBVBVBW
BW
BXBXBXBXBXBYBXBYBYBZBZBZBZB[#B[#B\)B]/B]/B]/B]/B]/B]/B^5B^5B^544444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B��B��B��B��B��B��B��B��B��B��B��B�1Bs�BcTBT�BQ�BN�BK�BG�BF�B?}B49B/B(�B(�B&�B"�B�B
=B��BBB+BDBB��B�B�TB�HB�NB�NB�/B��B��BȴBĜB��B�wB�^B�9B��B��B��B��B�bB�+B�+B~�B{�Bq�BaHBS�BJ�B@�B6FB.B+B�B{B  B�B�B�/B�^B��Bz�B[#B2-B{B��B�NB��B�dB��B�bB�7B�%B�B�B�Bm�BjBe`BdZBcTBcTBaHBbNBaHBaHBbNB`BB_;B[#BVBO�B@�B5?B/B.B)�B+B$�B'�B&�B�B�B�B�B�B�B{BoBbBJBB��B��B��B��B�B�mB�ZB�TB�NB�BB�5B�#B�B��B��B��B��B��BŢB��B�dB�FB�!B��B��B��B��B��B�VB�JB�DB�1B�%B�B�B�B�DB�DB�=B�+B�Bw�Bu�Bv�Bn�Bl�Bk�BgmBdZBaHB^5BXBT�BP�BL�BI�BF�BC�BB�B@�B=qB<jB8RB8RB7LB6FB5?B2-B0!B0!B0!B0!B.B,B(�B&�B$�B �B�B�B�B�B�BoBbBPB	7B%BBB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�sB
�sB
�ZB
�TB
�TB
�TB
�NB
�NB
�NB
�NB
�NB
�HB
�HB
�BB
�;B
�;B
�/B
�B
�B
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
ǮB
ǮB
ƨB
ŢB
ĜB
ĜB
B
��B
��B
��B
��B
��B
�}B
�}B
�wB
�qB
B
B
B
ÖB
ÖB
ÖB
ÖB
ĜB
ĜB
ŢB
ĜB
ŢB
ŢB
ǮB
ȴB
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
��B
��B
��B
��B
�B
�#B
�)B
�/B
�;B
�;B
�;B
�NB
�NB
�TB
�TB
�ZB
�mB
�mB
�B
�B
�B
�B
��B
��B
��B
��B  B  B  B  BBB+BDBJB\BbBoBuB�B�B�B�B�B!�B"�B#�B&�B,B,B-B/B1'B49B:^B;dB=qB?}BD�BK�BL�BL�BN�BS�BS�BS�BT�BT�BXB\)B]/B`BBgmBjBm�Bn�Bo�Bs�Bw�Bx�By�Bz�B|�B}�B~�B�B�B�1B�7B�=B�JB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�FB�RB�^B�}BÖB��B��B��B�B�#B�/B�TB�`B�mB�sB�sB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B��B  B  BBBBBB%B	7BJBVBbBhB{B�B�B�B!�B"�B%�B(�B-B.B/B33B6FB7LB:^B;dB;dB=qB>wBB�BD�BE�BF�BG�BH�BK�BP�BT�BVBW
BYBYBZB\)B^5B_;B_;B`BB_;B`BB`BBaHBbNBbNBe`BffBhsBiyBjBl�Bl�Bl�Bn�Bo�Bo�Bp�Bp�Bp�Bt�Bw�Bw�Bx�By�Bz�B|�B}�B� B� B� B�B�B�B�B�B�%B�+B�7B�7B�=B�JB�JB�PB�\B�\B�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�'B�-B�-B�3B�9B�?B�FB�FB�FB�FB�RB�^B�dB�dB�jB�jB�qB�}B��BBÖBBBÖBÖBĜBŢBŢBƨBǮBǮBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�#B�#B�#B�#B�/B�5B�5B�;B�HB�HB�NB�ZB�ZB�`B�`B�fB�fB�sB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBBBBBBBBBBBB%B+B+B+B1B1B1B1B	7B	7B
=B
=BDBDBDBJBJBPBVBVB\B\B\B\BbBbBbBbBhBhBoBhBoBuBuB{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B �B!�B!�B!�B"�B"�B#�B#�B#�B$�B%�B$�B%�B%�B%�B%�B%�B&�B&�B'�B'�B'�B'�B'�B(�B(�B(�B)�B)�B)�B+B+B,B+B,B,B-B.B.B.B.B/B/B/B/B0!B0!B0!B1'B1'B1'B2-B2-B1'B2-B2-B2-B33B2-B33B33B33B33B49B5?B49B49B5?B5?B5?B5?B5?B6FB6FB6FB7LB8RB8RB8RB8RB9XB9XB9XB:^B:^B;dB:^B;dB;dB;dB;dB<jB<jB=qB>wB=qB>wB>wB>wB>wB>wB?}B?}B@�B@�B@�B@�B@�BA�BA�BB�BB�BB�BC�BC�BC�BD�BD�BD�BE�BE�BE�BE�BE�BE�BF�BF�BG�BH�BH�BG�BH�BG�BH�BH�BH�BI�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BK�BL�BL�BM�BL�BM�BM�BM�BN�BN�BN�BN�BN�BN�BO�BO�BP�BP�BP�BP�BP�BP�BQ�BQ�BR�BR�BR�BR�BR�BR�BR�BS�BS�BT�BT�BT�BT�BT�BT�BT�BT�BVBVBVBVBVBW
BW
BXBXBXBXBXBYBXBYBYBZBZBZBZB[#B[#B\)B]/B]/B]/B]/B]/B]/B^5B^5B^544444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.16 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220223100104                              AO  ARCAADJP                                                                    20220223100104    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220223100104  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220223100104  QCF$                G�O�G�O�G�O�8000            