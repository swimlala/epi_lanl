CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:29:48Z creation;2022-06-04T19:29:48Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192948  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               kA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ٲcg���1   @ٲc�g��@+��l�C��d#-1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B   B  B  B  B   B(  B0  B8  B@  BHffBPffBW��Bb��Bg��Bp��Bx  B��B���B�  B�  B�  B�ffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  Bי�B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C33C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~�fD  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @fg@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�A�fgA�fgA���B��B��B��B��B'��B/��B7��B?��BH33BP33BWfgBb��BgfgBp��Bw��BfgB��3B��fB��fB��fB�L�B��fB��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB׀ B۳3B��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C&fCٙC�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CB�CD�CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D"3D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~�3D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�A�D���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�!-A�"hA�!�A�$A�$A�+�A�&�A�&A�VA�"4A�-A�2�A�2�A�3�A�5tA�6�A�7LA�9$A�9XA�:^A�;dA�<�A�<�A�<A�;�A�<6A�/�A�{�A�bA��A��XA�˒A��A�'RA�zxA���A�1�A��A��TA���A��A�^�A�~�A��A�<�A�_A��A��A�רA��A�`BA� A�B'A���A�<jA��iA���A�:�A��A���A��
A��xA���A���A�#A��dA�_�A��Ax�rAu�*Ar��An�9Al�xAe$�AaCA]�AY�AU�ZAQ�AO�+AL~(AJ$tAH��AF]�AC��AB�{AA�<A?�VA=�UA;u�A9VA8c�A7��A5�AA4A2l�A1��A0ȴA/�7A-�A+�>A*ںA)��A)�HA)��A(bNA'��A'=A'F�A&�A#��A!��A u�AȴA�A�A&�A�A��A��Ak�A�{A֡AxA�APHA�Am�AH�A��A�OA��A��AU2A5�A�2Ay�A|A�5A�@A\)A�cAz�A-wA�	AXA.IAk�A�}A�HAs�A��A��A��As�A}VA�jA�A%FA�Ad�A�OA��A�FA�oA iA`BARTA)_A�=A,=A
��A
�'A
cA
kQA
\�A	�.A	d�A		A��A��AU�A(AXyA��AN<A�A�AA�A�A��A'�A]dA�A�@A5?A ��A ��A ��A +k@�@���@�O�@�K^@��*@�H�@��u@��@��`@���@��@�bN@��F@�z@�e�@��@���@�Ov@��@��@�<6@��B@��@�;@�D�@��@�d�@��@例@���@�l�@�}�@��@엍@��a@��@��c@�0U@�\)@���@�@�@���@�Z@���@��3@�0@�N<@���@�+k@ᴢ@�33@୬@���@�]d@�j@�u%@��@�@۫�@�ߤ@ڨ�@�$@ق�@ث6@�Q�@�>B@�"h@���@׿H@�n/@��@ַ�@֮}@֔F@�~�@���@Լj@�+k@��}@�$t@�ff@�#:@�'R@�;�@��@��]@��@я�@�)_@Њr@�j@�@�U�@�!�@���@ΰ�@�-@͆�@���@�	�@�n/@ʤ�@�0U@�e@��@ɱ[@Ɋ�@�F@�ȴ@ȓu@�3�@�G@���@��@�!@Ņ�@Œ:@��@���@�g�@�@��p@�l"@���@��h@��@�m�@���@�|@�<6@��@�Xy@�%�@�8@�C-@�@��T@���@�+@�Q�@�dZ@�2a@�:�@��5@�	�@���@�.I@�?�@���@���@��"@�Mj@��@�J@���@�w2@�K�@�@���@�]d@���@��6@��^@���@�E9@��@��@���@�C-@���@���@���@�@�a�@�4�@��@��R@��o@�:*@��@��@��z@��S@��"@�y>@�4n@��)@��*@��	@�o @���@�7@�x@��|@��U@�q�@�J@�ݘ@���@��@��@��@�hs@�ѷ@��b@�x@��w@�~�@��@�ߤ@���@��b@��@�Xy@��@��@�ԕ@���@�qv@��@��@�{�@�Z@� �@�ƨ@��{@�&�@��@�S�@�ݘ@�+@��m@�z@�PH@�e@��&@��k@�K�@�Ĝ@�C-@�4@���@�rG@��@��z@�Xy@��@���@��a@��[@�|@�8@��@��O@��A@��@�tT@�i�@�`�@�W�@��@�W?@��@�R�@��;@�T�@���@�w�@�W�@�7@�zx@�>�@��M@�ی@���@�M�@��o@��#@���@���@�m]@�)_@��@���@�h
@�<�@��@�Y�@�F�@�+@��@�p;@�5?@�&�@� �@��@��}@���@�dZ@���@�Ft@���@���@��=@�@O@���@��_@�^5@�!�@��9@���@��P@�J#@���@���@���@�W�@��@��@�|@�=@�%F@�C@���@��x@�_�@�@��@���@���@�l�@�+�@�҉@�w�@�N�@��@�  @��@b�@~�m@~B[@}��@}=�@|�j@|tT@|@{�[@{�@z4@y�)@y��@y�@x�?@xN�@w��@w��@w�@v�@v+k@t��@t1@s�@r�<@r��@r�\@rc @r�@q�j@q��@qc�@q@pѷ@p�@pN�@o�&@o��@o�:@oj�@oS@n�@n��@n��@ni�@n)�@m@m�C@l�@l/�@k��@kj�@j~�@jJ@i��@i-w@h��@h�Y@hC-@f�"@fV@f3�@e�D@e��@e!�@d��@d��@c�@cj�@b��@b�@bE�@aԕ@ap�@`��@`��@`�4@`�@_�:@_o@^��@]��@]k�@]F@]�@\�@\ѷ@\�j@\��@\��@\�_@\9X@[��@Z�1@Y�)@YN<@X|�@X�@W�m@Wݘ@W�P@W/�@V�<@V{@U7L@UN<@UN<@U�@T�@T'R@S��@Sb�@R�@R��@R��@Ru@Q��@Q4@P�$@PK^@O��@Oy�@Nq�@M&�@L�Y@L1'@L�@K�Q@K�w@K��@Kg�@J�s@J;�@J�@I��@IA @I�@H�v@Hq@G�+@G�A@G�Q@G�@G��@G�@G{J@Gqv@GRT@G1�@G@F�\@Fc @E�9@E*0@D֡@D`�@C�k@Bں@Bi�@B@A�z@A}�@Am]@Aj@Aj@Ak�@Aa�@@��@@e�@@1'@?��@?�m@?�;@?Z�@?@>��@>��@=�@=X@=V@<֡@<h�@<1'@;�*@;@:��@:�@9T�@8֡@8w�@8Ft@8�@7�V@7\)@76z@6��@6�B@6�A@5�o@5`B@4�U@4_@4�@3�@3+@2��@2��@2u%@1��@1=�@0��@0Ĝ@0�z@0��@0?�@0,=@/�@/��@/x@/o�@/Mj@/!-@.��@.�@.Ov@-�D@-�@-��@-B�@,��@,�@,z�@,Z@+�[@+\)@*��@*��@*��@*p;@*($@)�j@)u�@)0�@)%@(ѷ@(�e@(_@'�@'��@'S�@'�@&҉@&{�@&V@&B[@%��@%o @%F@%-w@%�@$�@$��@$�e@$��@$z�@$S�@$C-@#�@#�$@#�4@#\)@#A�@#)_@#�@#)_@#�:@#>�@"��@"��@"��@"�!@"xl@"�@!�d@!��@!�"@!4@ ѷ@ ��@ U2@�@��@��@y�@dZ@4�@�@��@�+@��@�}@~�@5?@��@��@c@T�@5�@�@�@�@��@�.@Z@S�@H@��@�}@��@Z�@
=@��@��@u%@V@=q@n�@Q@�@�3@�C@��@L�@2a@@�@��@�@6@x@�@��@@O@��@�@� @C�@�@�@��@e,@O�@�@�E@��@�U@�e@��@_@D�@7@�@�@��@��@t�@]�@;d@&@�@�@��@��@ȴ@z@�@��@�@��@O�@B�@?}@q@��@��@�I@?�@,=@9X@6@�@��@˒@s@�@�@�8@�@��@� @z@Q@ �@�j@��@��@�h@�h@a�@/@@�@��@��@�I@��@tT@Z@$@b@�@��@�V@��@�4@v`@s@RT@
��@
��@
��@
ff@
Q@
L0@
3�@
-@
.�@
@	��@	�@	�#@	�d@	�@	��@	��@	?}@	�@�@Ĝ@��@��@u�@7�@��@��@��@P�@�@��@�H@͟@��@�<@�!@�}@�F@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�!-A�"hA�!�A�$A�$A�+�A�&�A�&A�VA�"4A�-A�2�A�2�A�3�A�5tA�6�A�7LA�9$A�9XA�:^A�;dA�<�A�<�A�<A�;�A�<6A�/�A�{�A�bA��A��XA�˒A��A�'RA�zxA���A�1�A��A��TA���A��A�^�A�~�A��A�<�A�_A��A��A�רA��A�`BA� A�B'A���A�<jA��iA���A�:�A��A���A��
A��xA���A���A�#A��dA�_�A��Ax�rAu�*Ar��An�9Al�xAe$�AaCA]�AY�AU�ZAQ�AO�+AL~(AJ$tAH��AF]�AC��AB�{AA�<A?�VA=�UA;u�A9VA8c�A7��A5�AA4A2l�A1��A0ȴA/�7A-�A+�>A*ںA)��A)�HA)��A(bNA'��A'=A'F�A&�A#��A!��A u�AȴA�A�A&�A�A��A��Ak�A�{A֡AxA�APHA�Am�AH�A��A�OA��A��AU2A5�A�2Ay�A|A�5A�@A\)A�cAz�A-wA�	AXA.IAk�A�}A�HAs�A��A��A��As�A}VA�jA�A%FA�Ad�A�OA��A�FA�oA iA`BARTA)_A�=A,=A
��A
�'A
cA
kQA
\�A	�.A	d�A		A��A��AU�A(AXyA��AN<A�A�AA�A�A��A'�A]dA�A�@A5?A ��A ��A ��A +k@�@���@�O�@�K^@��*@�H�@��u@��@��`@���@��@�bN@��F@�z@�e�@��@���@�Ov@��@��@�<6@��B@��@�;@�D�@��@�d�@��@例@���@�l�@�}�@��@엍@��a@��@��c@�0U@�\)@���@�@�@���@�Z@���@��3@�0@�N<@���@�+k@ᴢ@�33@୬@���@�]d@�j@�u%@��@�@۫�@�ߤ@ڨ�@�$@ق�@ث6@�Q�@�>B@�"h@���@׿H@�n/@��@ַ�@֮}@֔F@�~�@���@Լj@�+k@��}@�$t@�ff@�#:@�'R@�;�@��@��]@��@я�@�)_@Њr@�j@�@�U�@�!�@���@ΰ�@�-@͆�@���@�	�@�n/@ʤ�@�0U@�e@��@ɱ[@Ɋ�@�F@�ȴ@ȓu@�3�@�G@���@��@�!@Ņ�@Œ:@��@���@�g�@�@��p@�l"@���@��h@��@�m�@���@�|@�<6@��@�Xy@�%�@�8@�C-@�@��T@���@�+@�Q�@�dZ@�2a@�:�@��5@�	�@���@�.I@�?�@���@���@��"@�Mj@��@�J@���@�w2@�K�@�@���@�]d@���@��6@��^@���@�E9@��@��@���@�C-@���@���@���@�@�a�@�4�@��@��R@��o@�:*@��@��@��z@��S@��"@�y>@�4n@��)@��*@��	@�o @���@�7@�x@��|@��U@�q�@�J@�ݘ@���@��@��@��@�hs@�ѷ@��b@�x@��w@�~�@��@�ߤ@���@��b@��@�Xy@��@��@�ԕ@���@�qv@��@��@�{�@�Z@� �@�ƨ@��{@�&�@��@�S�@�ݘ@�+@��m@�z@�PH@�e@��&@��k@�K�@�Ĝ@�C-@�4@���@�rG@��@��z@�Xy@��@���@��a@��[@�|@�8@��@��O@��A@��@�tT@�i�@�`�@�W�@��@�W?@��@�R�@��;@�T�@���@�w�@�W�@�7@�zx@�>�@��M@�ی@���@�M�@��o@��#@���@���@�m]@�)_@��@���@�h
@�<�@��@�Y�@�F�@�+@��@�p;@�5?@�&�@� �@��@��}@���@�dZ@���@�Ft@���@���@��=@�@O@���@��_@�^5@�!�@��9@���@��P@�J#@���@���@���@�W�@��@��@�|@�=@�%F@�C@���@��x@�_�@�@��@���@���@�l�@�+�@�҉@�w�@�N�@��@�  @��@b�@~�m@~B[@}��@}=�@|�j@|tT@|@{�[@{�@z4@y�)@y��@y�@x�?@xN�@w��@w��@w�@v�@v+k@t��@t1@s�@r�<@r��@r�\@rc @r�@q�j@q��@qc�@q@pѷ@p�@pN�@o�&@o��@o�:@oj�@oS@n�@n��@n��@ni�@n)�@m@m�C@l�@l/�@k��@kj�@j~�@jJ@i��@i-w@h��@h�Y@hC-@f�"@fV@f3�@e�D@e��@e!�@d��@d��@c�@cj�@b��@b�@bE�@aԕ@ap�@`��@`��@`�4@`�@_�:@_o@^��@]��@]k�@]F@]�@\�@\ѷ@\�j@\��@\��@\�_@\9X@[��@Z�1@Y�)@YN<@X|�@X�@W�m@Wݘ@W�P@W/�@V�<@V{@U7L@UN<@UN<@U�@T�@T'R@S��@Sb�@R�@R��@R��@Ru@Q��@Q4@P�$@PK^@O��@Oy�@Nq�@M&�@L�Y@L1'@L�@K�Q@K�w@K��@Kg�@J�s@J;�@J�@I��@IA @I�@H�v@Hq@G�+@G�A@G�Q@G�@G��@G�@G{J@Gqv@GRT@G1�@G@F�\@Fc @E�9@E*0@D֡@D`�@C�k@Bں@Bi�@B@A�z@A}�@Am]@Aj@Aj@Ak�@Aa�@@��@@e�@@1'@?��@?�m@?�;@?Z�@?@>��@>��@=�@=X@=V@<֡@<h�@<1'@;�*@;@:��@:�@9T�@8֡@8w�@8Ft@8�@7�V@7\)@76z@6��@6�B@6�A@5�o@5`B@4�U@4_@4�@3�@3+@2��@2��@2u%@1��@1=�@0��@0Ĝ@0�z@0��@0?�@0,=@/�@/��@/x@/o�@/Mj@/!-@.��@.�@.Ov@-�D@-�@-��@-B�@,��@,�@,z�@,Z@+�[@+\)@*��@*��@*��@*p;@*($@)�j@)u�@)0�@)%@(ѷ@(�e@(_@'�@'��@'S�@'�@&҉@&{�@&V@&B[@%��@%o @%F@%-w@%�@$�@$��@$�e@$��@$z�@$S�@$C-@#�@#�$@#�4@#\)@#A�@#)_@#�@#)_@#�:@#>�@"��@"��@"��@"�!@"xl@"�@!�d@!��@!�"@!4@ ѷ@ ��@ U2@�@��@��@y�@dZ@4�@�@��@�+@��@�}@~�@5?@��@��@c@T�@5�@�@�@�@��@�.@Z@S�@H@��@�}@��@Z�@
=@��@��@u%@V@=q@n�@Q@�@�3@�C@��@L�@2a@@�@��@�@6@x@�@��@@O@��@�@� @C�@�@�@��@e,@O�@�@�E@��@�U@�e@��@_@D�@7@�@�@��@��@t�@]�@;d@&@�@�@��@��@ȴ@z@�@��@�@��@O�@B�@?}@q@��@��@�I@?�@,=@9X@6@�@��@˒@s@�@�@�8@�@��@� @z@Q@ �@�j@��@��@�h@�h@a�@/@@�@��@��@�I@��@tT@Z@$@b@�@��@�V@��@�4@v`@s@RT@
��@
��@
��@
ff@
Q@
L0@
3�@
-@
.�@
@	��@	�@	�#@	�d@	�@	��@	��@	?}@	�@�@Ĝ@��@��@u�@7�@��@��@��@P�@�@��@�H@͟@��@�<@�!@�}@�F@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BKBK^BK^BK^BK�BK�BK^BKDBKBJ�BK�BMBMBMBL�BL�BL�BL�BL�BMBM6BMBL�BLdBN�BY�B��B	�=B	�KB
�%B5�B}�B��B�	B��B�CBںB�BB��B�B�BB�B\B�B��B�NB��B�]B��B�@BwfB7�B'8BE�B`�BAoB+QB�B�B�B
�	B
��B
�eB
yXB
`�B
'8B	��B	�BB	�(B	��B	��B	��B	u%B	gRB	R B	D�B	4�B	+�B	 �B	1B	@B	�B	�B	GB	�B�cB��B��B��B�DB�rB	 B	uB	�B	�B	�B	�B	�B	�B�B��B	&B	 �B	IB	_B	�B	.IB	2�B	3MB	D�B	V�B	e`B	iB	vB	}qB	~wB	��B	�B	��B	��B	��B	��B	��B	�	B	�JB	�4B	�B	�xB	�B	�B	��B	�ZB	�&B	��B	�bB	�FB	�'B	��B	��B	��B	��B	��B	�|B	��B	�[B	�'B	�OB	�WB	�B	�^B
�B

�B
	lB
 �B	��B
!HB
&�B
*�B
.cB
/iB
./B
-�B
-B
,�B
-�B
.�B
.�B
/�B
.�B
-�B
,�B
-�B
.IB
-wB
-wB
-wB
.cB
/iB
0!B
/�B
/ B
.�B
.}B
.�B
.IB
-�B
-�B
/�B
.IB
.B
./B
-CB
,�B
-wB
-]B
+�B
*�B
)�B
)�B
*0B
+B
*�B
'�B
%zB
$tB
"�B
 vB
 BB
 �B
!|B
!�B
"hB
�B
!B
 �B
 B
;B
jB
�B
 vB
B
~B
qB
�B
�B
+B
sB
?B

B
�B

B
�B
MB
�B
�B
SB
sB
B
aB
�B
 B
4B
hB
NB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&B
�B
�B
oB
B
:B
�B
�B
�B
�B
B
�B
�B
}B
HB
�B
HB
�B
�B
�B
�B
hB
BB
�B
"B
�B
�B
�B
�B
�B
�B
.B
�B
�B
�B
�B
�B
4B
NB
B
B
bB
\B
�B
�B
�B
�B
�B
�B
B
:B
B
�B
�B
B
�B
hB
4B
bB
�B
�B
xB
VB
B
B
~B
�B
�B
DB

=B

�B
B
DB

=B
	�B
	�B
�B
fB
fB
	B
	�B
	RB
	�B
	�B
�B
�B
�B

	B
B
B
�B
�B
�B
B

�B

�B
~B
�B
VB
�B
�B
^B
^B
)B
�B
�B
<B
�B
�B
(B
�B
�B
\B
\B
B
�B
�B
\B
�B
}B
}B
B
�B
B
NB
4B
4B
4B
4B
�B
�B
 B
TB
:B
�B
�B
�B
B
�B
&B
@B
uB
B
B
FB
aB
[B
aB
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B

B
�B
�B
yB
_B
�B
�B
�B
sB
sB
YB
sB
YB
sB
�B
+B
�B
B
�B
�B
�B
B
B
B
QB
�B
�B
	B
�B
�B
B
�B
IB
�B
�B
�B
�B
~B
~B
~B
5B
5B
5B
B
B
B
�B
jB
�B
B
�B
 'B
 �B
 �B
!�B
!�B
!�B
"hB
"NB
"hB
"hB
"NB
# B
#�B
#nB
#TB
#�B
$ZB
$�B
$�B
$�B
%,B
%`B
&LB
&�B
&�B
&�B
'8B
'�B
($B
(>B
(sB
(sB
(�B
(sB
)B
+kB
,�B
-CB
-]B
-)B
.cB
./B
.�B
/iB
/�B
0!B
0UB
0�B
1vB
1�B
1�B
1�B
1�B
2GB
2�B
4B
4nB
4nB
4TB
4�B
5tB
6B
6B
6B
6+B
6FB
6zB
6�B
72B
7�B
7�B
8B
8RB
8�B
8�B
9$B
9rB
9�B
:^B
:xB
:�B
:�B
:�B
;JB
;�B
;�B
;�B
;�B
;�B
<PB
<PB
<6B
<�B
<�B
=<B
<�B
<6B
<PB
<PB
<jB
<�B
<�B
="B
=�B
=�B
>(B
>B
>BB
>]B
>�B
>�B
>�B
>�B
>�B
?}B
?�B
@4B
@iB
@4B
@OB
@OB
@4B
@�B
@�B
@�B
A�B
A�B
BuB
B�B
CaB
C�B
C�B
C�B
D�B
E9B
E9B
EB
E9B
E�B
FB
F?B
GB
G_B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
H�B
G�B
G�B
G_B
G+B
G_B
HKB
H�B
IRB
I�B
J�B
KDB
J�B
I�B
IB
I�B
I�B
I�B
I�B
J	B
JrB
J�B
J�B
J�B
J#B
J�B
KB
J�B
JXB
I�B
H�B
H�B
H�B
I�B
I�B
J�B
J=B
J�B
KB
KxB
K�B
K�B
L�B
M�B
M�B
NB
N<B
NpB
NVB
NVB
N�B
O�B
O�B
P.B
PB
O�B
PB
PB
P�B
Q B
P�B
Q B
Q B
QB
QB
Q�B
QhB
Q�B
RB
R:B
R�B
R�B
S�B
TFB
T{B
T�B
U�B
U�B
U�B
U�B
U�B
V9B
V9B
VmB
VmB
V�B
VmB
V�B
V�B
WYB
W�B
W�B
W�B
W�B
XB
XEB
XEB
YeB
YB
Y�B
Y�B
ZkB
ZkB
Z�B
[=B
[�B
\B
\�B
]B
]IB
]dB
]dB
]�B
^B
^B
^5B
^5B
^OB
^�B
_!B
_�B
_�B
_�B
`'B
`�B
`�B
`�B
aB
a|B
bB
b4B
b�B
b�B
b�B
cB
cB
cTB
c�B
c�B
c�B
c�B
d&B
d�B
d�B
eB
eB
e�B
fB
f�B
gmB
gmB
gRB
gmB
h$B
hXB
h�B
iB
iB
i*B
iyB
i�B
jB
j0B
jKB
jeB
jeB
j�B
kB
k6B
k�B
kkB
k�B
l"B
l=B
lB
l�B
l�B
mB
m)B
mCB
mwB
m�B
m�B
m�B
m�B
m�B
m�B
ncB
n�B
n�B
n�B
o B
oiB
o�B
o�B
q[B
q�B
qvB
q�B
qvB
qAB
p�B
poB
p;B
pUB
poB
p�B
q'B
q[B
qvB
q�B
r-B
r-B
rGB
rGB
r|B
r�B
shB
tB
t�B
t�B
u?B
u?B
u?B
uB
u?B
u�B
vzB
v�B
v�B
v�B
v�B
wB
w2B
wB
wB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
y$B
y$B
y$B
yrB
y�B
y�B
zB
zDB
z*B
zxB
z�B
z�B
z�B
z�B
z�B
{JB
{�B
{�B
|B
|PB
|PB
|jB
|�B
}<B
}VB
}�B
}�B
~(B
~BB
~BB
~]B
~�B
~�B
~�B
~�B
~�B
~�B
.B
.B
cB
�B
�B
�B
�B
�B
�B
� B
� B
�4B
��B
�B
��B
�B
�;B
�UB
�UB
�UB
�oB
�oB
�oB
��B
��B
��B
�B
�'B
�AB
�[B
��B
�-B
�GB
�GB
�GB
��B
��B
��B
��B
�MB
��B
��B
��B
��B
��B
�B
�B
�9B
�mB
�mB
��B
��B
��B
��B
��B
�?B
�YB
�tB
��B
��B
��B
��B
��B
��B
��B
�EB
�B
��B
��B
��B
��B
�1B
�1B
�1B
�KB
��B
��B
��B
��B
��B
��B
��B
�B
�7B
��B
��B
��B
��B
�	B
�=B
��B
��B
��B
��B
�)B
�DB
�)B
�DB
�DB
�DB
�^B
�^B
�xB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BKBK^BK^BK^BK�BK�BK^BKDBKBJ�BK�BMBMBMBL�BL�BL�BL�BL�BMBM6BMBL�BLdBN�BY�B��B	�=B	�KB
�%B5�B}�B��B�	B��B�CBںB�BB��B�B�BB�B\B�B��B�NB��B�]B��B�@BwfB7�B'8BE�B`�BAoB+QB�B�B�B
�	B
��B
�eB
yXB
`�B
'8B	��B	�BB	�(B	��B	��B	��B	u%B	gRB	R B	D�B	4�B	+�B	 �B	1B	@B	�B	�B	GB	�B�cB��B��B��B�DB�rB	 B	uB	�B	�B	�B	�B	�B	�B�B��B	&B	 �B	IB	_B	�B	.IB	2�B	3MB	D�B	V�B	e`B	iB	vB	}qB	~wB	��B	�B	��B	��B	��B	��B	��B	�	B	�JB	�4B	�B	�xB	�B	�B	��B	�ZB	�&B	��B	�bB	�FB	�'B	��B	��B	��B	��B	��B	�|B	��B	�[B	�'B	�OB	�WB	�B	�^B
�B

�B
	lB
 �B	��B
!HB
&�B
*�B
.cB
/iB
./B
-�B
-B
,�B
-�B
.�B
.�B
/�B
.�B
-�B
,�B
-�B
.IB
-wB
-wB
-wB
.cB
/iB
0!B
/�B
/ B
.�B
.}B
.�B
.IB
-�B
-�B
/�B
.IB
.B
./B
-CB
,�B
-wB
-]B
+�B
*�B
)�B
)�B
*0B
+B
*�B
'�B
%zB
$tB
"�B
 vB
 BB
 �B
!|B
!�B
"hB
�B
!B
 �B
 B
;B
jB
�B
 vB
B
~B
qB
�B
�B
+B
sB
?B

B
�B

B
�B
MB
�B
�B
SB
sB
B
aB
�B
 B
4B
hB
NB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&B
�B
�B
oB
B
:B
�B
�B
�B
�B
B
�B
�B
}B
HB
�B
HB
�B
�B
�B
�B
hB
BB
�B
"B
�B
�B
�B
�B
�B
�B
.B
�B
�B
�B
�B
�B
4B
NB
B
B
bB
\B
�B
�B
�B
�B
�B
�B
B
:B
B
�B
�B
B
�B
hB
4B
bB
�B
�B
xB
VB
B
B
~B
�B
�B
DB

=B

�B
B
DB

=B
	�B
	�B
�B
fB
fB
	B
	�B
	RB
	�B
	�B
�B
�B
�B

	B
B
B
�B
�B
�B
B

�B

�B
~B
�B
VB
�B
�B
^B
^B
)B
�B
�B
<B
�B
�B
(B
�B
�B
\B
\B
B
�B
�B
\B
�B
}B
}B
B
�B
B
NB
4B
4B
4B
4B
�B
�B
 B
TB
:B
�B
�B
�B
B
�B
&B
@B
uB
B
B
FB
aB
[B
aB
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B

B
�B
�B
yB
_B
�B
�B
�B
sB
sB
YB
sB
YB
sB
�B
+B
�B
B
�B
�B
�B
B
B
B
QB
�B
�B
	B
�B
�B
B
�B
IB
�B
�B
�B
�B
~B
~B
~B
5B
5B
5B
B
B
B
�B
jB
�B
B
�B
 'B
 �B
 �B
!�B
!�B
!�B
"hB
"NB
"hB
"hB
"NB
# B
#�B
#nB
#TB
#�B
$ZB
$�B
$�B
$�B
%,B
%`B
&LB
&�B
&�B
&�B
'8B
'�B
($B
(>B
(sB
(sB
(�B
(sB
)B
+kB
,�B
-CB
-]B
-)B
.cB
./B
.�B
/iB
/�B
0!B
0UB
0�B
1vB
1�B
1�B
1�B
1�B
2GB
2�B
4B
4nB
4nB
4TB
4�B
5tB
6B
6B
6B
6+B
6FB
6zB
6�B
72B
7�B
7�B
8B
8RB
8�B
8�B
9$B
9rB
9�B
:^B
:xB
:�B
:�B
:�B
;JB
;�B
;�B
;�B
;�B
;�B
<PB
<PB
<6B
<�B
<�B
=<B
<�B
<6B
<PB
<PB
<jB
<�B
<�B
="B
=�B
=�B
>(B
>B
>BB
>]B
>�B
>�B
>�B
>�B
>�B
?}B
?�B
@4B
@iB
@4B
@OB
@OB
@4B
@�B
@�B
@�B
A�B
A�B
BuB
B�B
CaB
C�B
C�B
C�B
D�B
E9B
E9B
EB
E9B
E�B
FB
F?B
GB
G_B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
H�B
G�B
G�B
G_B
G+B
G_B
HKB
H�B
IRB
I�B
J�B
KDB
J�B
I�B
IB
I�B
I�B
I�B
I�B
J	B
JrB
J�B
J�B
J�B
J#B
J�B
KB
J�B
JXB
I�B
H�B
H�B
H�B
I�B
I�B
J�B
J=B
J�B
KB
KxB
K�B
K�B
L�B
M�B
M�B
NB
N<B
NpB
NVB
NVB
N�B
O�B
O�B
P.B
PB
O�B
PB
PB
P�B
Q B
P�B
Q B
Q B
QB
QB
Q�B
QhB
Q�B
RB
R:B
R�B
R�B
S�B
TFB
T{B
T�B
U�B
U�B
U�B
U�B
U�B
V9B
V9B
VmB
VmB
V�B
VmB
V�B
V�B
WYB
W�B
W�B
W�B
W�B
XB
XEB
XEB
YeB
YB
Y�B
Y�B
ZkB
ZkB
Z�B
[=B
[�B
\B
\�B
]B
]IB
]dB
]dB
]�B
^B
^B
^5B
^5B
^OB
^�B
_!B
_�B
_�B
_�B
`'B
`�B
`�B
`�B
aB
a|B
bB
b4B
b�B
b�B
b�B
cB
cB
cTB
c�B
c�B
c�B
c�B
d&B
d�B
d�B
eB
eB
e�B
fB
f�B
gmB
gmB
gRB
gmB
h$B
hXB
h�B
iB
iB
i*B
iyB
i�B
jB
j0B
jKB
jeB
jeB
j�B
kB
k6B
k�B
kkB
k�B
l"B
l=B
lB
l�B
l�B
mB
m)B
mCB
mwB
m�B
m�B
m�B
m�B
m�B
m�B
ncB
n�B
n�B
n�B
o B
oiB
o�B
o�B
q[B
q�B
qvB
q�B
qvB
qAB
p�B
poB
p;B
pUB
poB
p�B
q'B
q[B
qvB
q�B
r-B
r-B
rGB
rGB
r|B
r�B
shB
tB
t�B
t�B
u?B
u?B
u?B
uB
u?B
u�B
vzB
v�B
v�B
v�B
v�B
wB
w2B
wB
wB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
y$B
y$B
y$B
yrB
y�B
y�B
zB
zDB
z*B
zxB
z�B
z�B
z�B
z�B
z�B
{JB
{�B
{�B
|B
|PB
|PB
|jB
|�B
}<B
}VB
}�B
}�B
~(B
~BB
~BB
~]B
~�B
~�B
~�B
~�B
~�B
~�B
.B
.B
cB
�B
�B
�B
�B
�B
�B
� B
� B
�4B
��B
�B
��B
�B
�;B
�UB
�UB
�UB
�oB
�oB
�oB
��B
��B
��B
�B
�'B
�AB
�[B
��B
�-B
�GB
�GB
�GB
��B
��B
��B
��B
�MB
��B
��B
��B
��B
��B
�B
�B
�9B
�mB
�mB
��B
��B
��B
��B
��B
�?B
�YB
�tB
��B
��B
��B
��B
��B
��B
��B
�EB
�B
��B
��B
��B
��B
�1B
�1B
�1B
�KB
��B
��B
��B
��B
��B
��B
��B
�B
�7B
��B
��B
��B
��B
�	B
�=B
��B
��B
��B
��B
�)B
�DB
�)B
�DB
�DB
�DB
�^B
�^B
�xB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105250  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192948  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192948  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192948                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042956  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042956  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                