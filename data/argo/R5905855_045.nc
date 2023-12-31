CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:18:32Z creation;2022-06-04T19:18:32Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191832  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               -A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��=��1   @���hK�@.�hr�!�c{-1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB/��B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�B�ffB�33B���C   C  C  C  C  C
  CffC33C�fC  C  C  C  C  C  C  C�fC"  C$� C%�fC(  C*  C+�fC.  C0  C2  C3�fC5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCe�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D��3D�3D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�3D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @6fg@|��@�ff@�ffA��A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B(33B/fgB7��B?��BG��BOfgBW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB�� B�� B��fB��fB��fBó3B��fB��fBϳ3B��fB��fB��fB��fB��fB��fB��fB�� B�L�B��B��3B��fC�3C�3C�3C�3C	�3CY�C&fCٙC�3C�3C�3C�3C�3C�3C�3CٙC!�3C$s3C%ٙC'�3C)�3C+ٙC-�3C/�3C1�3C3ٙC5ٙC7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3CcٙCeٙCg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D
3D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D 3D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fD���D��D�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��D�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�A�D�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�+311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A��NA��A��A��A��BA���A�̘A��gA��?A��A��^A�˒A�ȴA��XA��zA�ƨA��[AϬ=Aϳ3AϿ�AϘ_Aώ�Aϊ�Aϋ�A�.A�xlA�zA�v+A�t�A�p�A�m�A�k�A�gA�\]A�GzAοHA�N�A��KA�f2AȪ�A�-�A��A�]dA¬=A�{A��cA��A�4A���A�ĜA�	�A�`A��A�;A���A��A�C�A��A���A���A���A�DA��A�>wA���A��A���A�=A�l�A�`vA���A��-A��xA��A���A�WsA��A���A~+�AyK^Aw��Au>BAq�&An��AjOvAgn/Ad�zAb��A_��A[&AY��AX�?AV�AR�+AMCAL2aAI@OAE~�AAh
A<u%A:GEA8��A8rGA4��A2�A0�:A.�A-��A,��A*�rA*˒A*rGA*�A)��A)�\A)*0A'��A&�gA&�rA%��A$��A$��A%OA%s�A%
�A#H�A!�?A!�HA!�yA"L�A!�A!A�-AC�An�AMA8A��A��AZ�A֡AA�0A{A\�A�}A��A2aA�A�4AA�[AG�A��A�A��A{�A�A��A��AD�A�<A�\A5?A�<A�eAJ#AA�A
�DA!-A
�PA
�A	�0A	=AJ�A.�A�Au%A��A��AOvA��AA A��A8�A�fA��A��AU�A�A �A��AS&AD�A�A��Al"A�ARTAbA l�A Y�A|�A)�A E�@��x@���@��}@��@��@��@���@�;d@��>@�@�#:@���@�Z@���@���@��@�@���@�GE@�y�@�֡@�l�@�@�>�@�.�@��@�O@�@�U2@��@�{@�g�@�+@뇔@�@��@�ff@�~@�s�@��p@�;�@��T@�k�@�.I@�7@�T�@�q@�j@�O@�rG@◍@�U2@�:*@�0U@�i�@��@ᙚ@࿱@�7@߼�@�ƨ@ߋ�@�+@�ff@�YK@�4@ݜ�@�ی@�tT@�dZ@���@ڍ�@��#@�j@أ@�b@���@��@׷@ֺ�@�Z�@��@�x�@֘_@�o�@�͟@ԩ�@Ԟ�@ԣ@�,=@�rG@�%F@Ҩ�@�l�@��@ы�@�*0@�Ɇ@�g8@�*�@��@ϼ�@�C@��p@�c @͞�@�RT@���@̬�@�+k@˓@�\)@��@�^5@���@�%@ȳh@ȹ�@ȵ@�5?@Ǌ	@��`@�z@�O@��@ů�@�8�@Ĝx@Ā�@���@�o�@�c�@�<6@F@���@�@��@�$�@��Q@��4@�X�@�E9@���@���@�(�@�ݘ@���@�o�@�'�@��@��,@���@���@�	�@��@�RT@�
=@��e@�)�@��@���@��X@�]�@��8@��[@�($@� �@��@�rG@��"@��@���@��}@�c�@�($@��@���@���@�=�@���@���@��.@�h
@�Ta@�@�@�x@��X@���@��u@�L0@�I�@�#:@���@�<6@���@��L@�n�@�v�@�_@�b@���@�E9@���@�M@�:�@��@��#@�x@�\�@�L�@�!-@�!�@���@�:�@��H@�`B@�\)@�b�@�L�@��@��@�C�@��D@��@�W?@��@��)@���@�7@��z@�|@��@���@���@�s�@��@��'@�qv@�(@���@��A@�@���@�'�@��h@���@�}V@�%�@���@�Q�@��P@�ȴ@��@�Q@�_@��3@�o�@���@��p@��@�6�@���@�zx@�5�@�0�@�-w@�%F@�@�ȴ@�w�@��@��V@�=�@�!-@��@��@�[�@��@�L�@��]@�/�@��@�zx@���@�N�@�4n@���@�@��	@�/�@���@��c@�֡@�-�@��0@�y�@�1�@��@��P@��6@�N�@� �@��@��@@�A�@��@���@��]@��@�Q�@�($@�{@�G@��>@��d@���@���@�m]@��@���@���@�|�@�:�@��.@��k@�o�@�G�@�Ĝ@��I@���@�p;@�H�@��@��;@���@�8�@��s@���@�PH@�1'@�$�@�!@�@���@��@��S@�W?@� \@���@��@��_@�W�@��@�e,@�;@��@�]d@�H@�	@��^@���@�5�@��@�҉@���@���@�q�@�K^@�=q@�!�@��6@��k@�5�@�;@���@��+@�1�@��@���@�J�@���@��x@�m�@�B[@�4n@�1�@�"h@�
�@��@�k@E9@�@~�@~҉@~�!@~C�@}�X@|��@{�@{�P@{{J@{E9@z�R@z@y�@yw2@y;@xw�@x'R@w�Q@wy�@w$t@v��@v�}@vV@v	@u�H@uj@t|�@s��@s��@s.I@r��@r3�@r �@q��@q�^@qa�@q�@p�_@p*�@o4�@n��@nu@m��@mX@m*0@l��@lPH@k�w@k�P@k\)@k=@j��@i��@iIR@h�f@h��@h��@h�@hy>@g�Q@gH�@f�2@f��@f�6@f�A@eϫ@e-w@d�)@d��@d�I@dM@c��@c6z@b�,@b\�@b)�@a�z@a��@a|@aA @a+@`�z@`!@_�+@_خ@_��@_v`@_X�@_A�@_,�@^��@^ȴ@^�A@]�@\��@\��@\2�@\$@[�6@[H�@Z��@Y��@YS&@Xѷ@X��@XS�@XA�@X/�@Xx@W�	@V��@VR�@U�o@U�@UY�@T�@T�9@TbN@TA�@S�@R��@Rd�@R)�@Q�)@Q�d@Q�-@Q�S@Q*0@P�	@P�f@Pq@Oخ@O��@Ol�@N�h@N)�@N_@M�Z@M��@MDg@L��@L��@Lg8@K��@J��@Jc @JQ@JH�@J
�@I�X@I��@Izx@Ij@I=�@H�9@H_@HA�@H*�@H	�@Gخ@G��@G�@G=@F�1@F��@F{�@FJ@E��@EA @D��@D4n@C��@C'�@B�@B��@Bz@B?@A�j@A��@A4@@��@@��@@��@@h�@?�@?n/@?;d@?�@>��@>1�@=�@=�X@=p�@=%F@<�K@<��@<`�@;�]@;�K@;��@;W?@;@O@;E9@:�@:��@:Z�@:8�@:)�@9�)@9k�@8�$@8V�@8U2@8C-@81@7��@7��@7��@7|�@7.I@6��@6YK@5��@5j@5*0@4q@4V�@4Ft@3�@3C@2��@2s�@2�@1ϫ@1��@1�h@1rG@1Dg@0�	@0�@0�@0A�@0?�@04n@/��@/y�@/iD@/"�@.�@.��@.i�@.�@-�o@-��@-��@-c@-Vm@,�@,��@,e�@,[�@,Ft@+�@+��@+1�@+�@*�8@*J�@*{@)�.@)��@)��@)hs@)Vm@) \@(��@(�Y@(>B@'�r@'� @'��@'y�@'�@&��@&�6@&�r@&Ov@&@%�j@%�C@%IR@%8�@%*0@%+@$�f@$ی@$�?@$��@$]d@$7�@#�m@#�w@#��@#l�@#1�@"��@"��@"��@"^5@";�@"&�@!�9@!�n@!w2@!?}@!@ �@ ��@ ��@ ��@ q@ K^@�@��@�*@��@�@{J@=@@�2@�6@��@s�@d�@&�@�@�@m]@N<@&�@��@�@�_@w�@C-@(�@M@�A@�;@�*@qv@E9@�@�c@�6@�+@kQ@�@�^@p�@@�@�j@]d@1'@	�@� @��@=@��@s�@��@��@@c@+@�U@��@oi@6@��@��@�0@v`@4�@��@�@L0@�@�H@c@V@�[@�u@:�@�@�a@�@iD@P�@,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A��NA��A��A��A��BA���A�̘A��gA��?A��A��^A�˒A�ȴA��XA��zA�ƨA��[AϬ=Aϳ3AϿ�AϘ_Aώ�Aϊ�Aϋ�A�.A�xlA�zA�v+A�t�A�p�A�m�A�k�A�gA�\]A�GzAοHA�N�A��KA�f2AȪ�A�-�A��A�]dA¬=A�{A��cA��A�4A���A�ĜA�	�A�`A��A�;A���A��A�C�A��A���A���A���A�DA��A�>wA���A��A���A�=A�l�A�`vA���A��-A��xA��A���A�WsA��A���A~+�AyK^Aw��Au>BAq�&An��AjOvAgn/Ad�zAb��A_��A[&AY��AX�?AV�AR�+AMCAL2aAI@OAE~�AAh
A<u%A:GEA8��A8rGA4��A2�A0�:A.�A-��A,��A*�rA*˒A*rGA*�A)��A)�\A)*0A'��A&�gA&�rA%��A$��A$��A%OA%s�A%
�A#H�A!�?A!�HA!�yA"L�A!�A!A�-AC�An�AMA8A��A��AZ�A֡AA�0A{A\�A�}A��A2aA�A�4AA�[AG�A��A�A��A{�A�A��A��AD�A�<A�\A5?A�<A�eAJ#AA�A
�DA!-A
�PA
�A	�0A	=AJ�A.�A�Au%A��A��AOvA��AA A��A8�A�fA��A��AU�A�A �A��AS&AD�A�A��Al"A�ARTAbA l�A Y�A|�A)�A E�@��x@���@��}@��@��@��@���@�;d@��>@�@�#:@���@�Z@���@���@��@�@���@�GE@�y�@�֡@�l�@�@�>�@�.�@��@�O@�@�U2@��@�{@�g�@�+@뇔@�@��@�ff@�~@�s�@��p@�;�@��T@�k�@�.I@�7@�T�@�q@�j@�O@�rG@◍@�U2@�:*@�0U@�i�@��@ᙚ@࿱@�7@߼�@�ƨ@ߋ�@�+@�ff@�YK@�4@ݜ�@�ی@�tT@�dZ@���@ڍ�@��#@�j@أ@�b@���@��@׷@ֺ�@�Z�@��@�x�@֘_@�o�@�͟@ԩ�@Ԟ�@ԣ@�,=@�rG@�%F@Ҩ�@�l�@��@ы�@�*0@�Ɇ@�g8@�*�@��@ϼ�@�C@��p@�c @͞�@�RT@���@̬�@�+k@˓@�\)@��@�^5@���@�%@ȳh@ȹ�@ȵ@�5?@Ǌ	@��`@�z@�O@��@ů�@�8�@Ĝx@Ā�@���@�o�@�c�@�<6@F@���@�@��@�$�@��Q@��4@�X�@�E9@���@���@�(�@�ݘ@���@�o�@�'�@��@��,@���@���@�	�@��@�RT@�
=@��e@�)�@��@���@��X@�]�@��8@��[@�($@� �@��@�rG@��"@��@���@��}@�c�@�($@��@���@���@�=�@���@���@��.@�h
@�Ta@�@�@�x@��X@���@��u@�L0@�I�@�#:@���@�<6@���@��L@�n�@�v�@�_@�b@���@�E9@���@�M@�:�@��@��#@�x@�\�@�L�@�!-@�!�@���@�:�@��H@�`B@�\)@�b�@�L�@��@��@�C�@��D@��@�W?@��@��)@���@�7@��z@�|@��@���@���@�s�@��@��'@�qv@�(@���@��A@�@���@�'�@��h@���@�}V@�%�@���@�Q�@��P@�ȴ@��@�Q@�_@��3@�o�@���@��p@��@�6�@���@�zx@�5�@�0�@�-w@�%F@�@�ȴ@�w�@��@��V@�=�@�!-@��@��@�[�@��@�L�@��]@�/�@��@�zx@���@�N�@�4n@���@�@��	@�/�@���@��c@�֡@�-�@��0@�y�@�1�@��@��P@��6@�N�@� �@��@��@@�A�@��@���@��]@��@�Q�@�($@�{@�G@��>@��d@���@���@�m]@��@���@���@�|�@�:�@��.@��k@�o�@�G�@�Ĝ@��I@���@�p;@�H�@��@��;@���@�8�@��s@���@�PH@�1'@�$�@�!@�@���@��@��S@�W?@� \@���@��@��_@�W�@��@�e,@�;@��@�]d@�H@�	@��^@���@�5�@��@�҉@���@���@�q�@�K^@�=q@�!�@��6@��k@�5�@�;@���@��+@�1�@��@���@�J�@���@��x@�m�@�B[@�4n@�1�@�"h@�
�@��@�k@E9@�@~�@~҉@~�!@~C�@}�X@|��@{�@{�P@{{J@{E9@z�R@z@y�@yw2@y;@xw�@x'R@w�Q@wy�@w$t@v��@v�}@vV@v	@u�H@uj@t|�@s��@s��@s.I@r��@r3�@r �@q��@q�^@qa�@q�@p�_@p*�@o4�@n��@nu@m��@mX@m*0@l��@lPH@k�w@k�P@k\)@k=@j��@i��@iIR@h�f@h��@h��@h�@hy>@g�Q@gH�@f�2@f��@f�6@f�A@eϫ@e-w@d�)@d��@d�I@dM@c��@c6z@b�,@b\�@b)�@a�z@a��@a|@aA @a+@`�z@`!@_�+@_خ@_��@_v`@_X�@_A�@_,�@^��@^ȴ@^�A@]�@\��@\��@\2�@\$@[�6@[H�@Z��@Y��@YS&@Xѷ@X��@XS�@XA�@X/�@Xx@W�	@V��@VR�@U�o@U�@UY�@T�@T�9@TbN@TA�@S�@R��@Rd�@R)�@Q�)@Q�d@Q�-@Q�S@Q*0@P�	@P�f@Pq@Oخ@O��@Ol�@N�h@N)�@N_@M�Z@M��@MDg@L��@L��@Lg8@K��@J��@Jc @JQ@JH�@J
�@I�X@I��@Izx@Ij@I=�@H�9@H_@HA�@H*�@H	�@Gخ@G��@G�@G=@F�1@F��@F{�@FJ@E��@EA @D��@D4n@C��@C'�@B�@B��@Bz@B?@A�j@A��@A4@@��@@��@@��@@h�@?�@?n/@?;d@?�@>��@>1�@=�@=�X@=p�@=%F@<�K@<��@<`�@;�]@;�K@;��@;W?@;@O@;E9@:�@:��@:Z�@:8�@:)�@9�)@9k�@8�$@8V�@8U2@8C-@81@7��@7��@7��@7|�@7.I@6��@6YK@5��@5j@5*0@4q@4V�@4Ft@3�@3C@2��@2s�@2�@1ϫ@1��@1�h@1rG@1Dg@0�	@0�@0�@0A�@0?�@04n@/��@/y�@/iD@/"�@.�@.��@.i�@.�@-�o@-��@-��@-c@-Vm@,�@,��@,e�@,[�@,Ft@+�@+��@+1�@+�@*�8@*J�@*{@)�.@)��@)��@)hs@)Vm@) \@(��@(�Y@(>B@'�r@'� @'��@'y�@'�@&��@&�6@&�r@&Ov@&@%�j@%�C@%IR@%8�@%*0@%+@$�f@$ی@$�?@$��@$]d@$7�@#�m@#�w@#��@#l�@#1�@"��@"��@"��@"^5@";�@"&�@!�9@!�n@!w2@!?}@!@ �@ ��@ ��@ ��@ q@ K^@�@��@�*@��@�@{J@=@@�2@�6@��@s�@d�@&�@�@�@m]@N<@&�@��@�@�_@w�@C-@(�@M@�A@�;@�*@qv@E9@�@�c@�6@�+@kQ@�@�^@p�@@�@�j@]d@1'@	�@� @��@=@��@s�@��@��@@c@+@�U@��@oi@6@��@��@�0@v`@4�@��@�@L0@�@�H@c@V@�[@�u@:�@�@�a@�@iD@P�@,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�{B�,B�aB�aB�aB�FB�[B��B�uB�[B��B�@B��B��B�@B�[B�[B�@B�&B��B��B��B��B��B��B�NB��B� B��B� B�4B�B��B��B�}B�.B��B�~B��B��B�B�EB�NB�!B�QB	lWB	��B	�XB
B'B
�]B
��B
��B
�B
�-BfB�BB*�BDMBezBhsBx�BgB=�B�B+B�B
��B
��B
�VB
�6B
��B
z�B
j�B
OvB
)�B
B	��B	�B	ѝB	�EB	bB	Y1B	PbB	I�B	I�B	7�B	,=B	1vB	7�B	F�B	VSB	Q�B	UgB	_B	Q�B	0�B	B'B	>�B	$�B		�B�}B��B��B��B��B��B	AB	�B	+B	�B	�B	
�B	(B	�B	 \B	.B	EmB	I�B	OvB	TFB	R B	N�B	PHB	]B	n�B	q�B	k�B	b�B	mB	y>B	�B	��B	��B	��B	�_B	��B	�B	�NB	�:B	��B	��B	�WB	�B	�/B	��B	��B	��B	��B	�6B	��B	��B	�UB	�AB	�|B	�9B	�%B	�B	�?B	�B	��B	�LB	��B	��B	��B	�|B	��B	�"B	��B	��B	��B	�}B	�uB	�-B	��B	��B	�B	�<B	��B	��B	�B	�EB	�_B	ǮB	�B	�B	��B	�tB	�B	ߊB	ܬB	��B	�uB	�VB	��B	��B	�MB	�?B	ּB	�?B	��B	��B	�9B	�B	�B	��B	�qB	�B	��B	�B	�B	�B	�B	�B	�AB	��B	�LB	��B	�|B	�LB	�B	�B	�]B	��B	�B	�$B	��B	�_B	�B	�B	�B	�B	��B	��B	�B	�8B	�B	�B	��B	�B	��B	�-B	��B	�oB	�B	�OB	�/B	�)B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�nB	�hB	�TB	�tB	�B	�>B	��B	�B	�B	�B	�B	�`B	�B	�B	��B	�qB	�B	�B	��B	�B	�DB	�B	�DB	�mB	�8B	�XB	�
B	�B	�8B	�B	�fB	�KB	��B	�B	�B	�fB	�8B	�RB	��B	��B	�B	��B	�B	�B	�B	��B	��B	�]B	�CB	�)B	�)B	�wB	�B	�B	�OB	�B	��B	�AB	�B	�AB	�B	�AB	�B	�B	�|B	�B	��B	�ZB	�B	��B	��B	��B	�-B	��B	�MB	�TB	��B	��B	�hB	�B	�B	��B	�TB	��B	��B	�!B	�B	��B	�B	�iB	�!B	�B	��B	�B	�[B	�'B	�[B	�B	��B	�aB	�GB	�GB	�3B	�B	�+B	��B	��B	�lB	��B	��B	�`B	�`B	�B	��B	��B	��B	�+B	��B	��B	�RB	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�^B	��B	��B	��B	��B	��B	��B	��B	�]B	�wB	��B	�BB	�<B	��B	�HB	��B
;B
�B
[B
uB
�B
�B
oB
�B
�B
-B
uB
�B
�B
B
�B
�B
SB
B
+B
�B
�B
fB

#B
�B
xB

�B
)B
�B
�B
�B
jB
�B
�B
jB
jB
�B
�B
B
�B
�B
�B
\B
�B
�B
(B
VB
�B
�B
6B
jB
�B
(B
�B
B
B
bB
}B
�B
�B
�B
�B
�B
TB
�B
�B
oB
TB
oB
TB
oB
:B
 B
�B
oB
:B
:B
oB
�B
�B
TB
�B
�B
�B
�B
�B
TB
&B
�B
�B
B
�B
�B
{B
�B
�B
�B
B
B
�B
�B
�B
�B
FB
�B
�B
gB
�B
�B

B
YB
�B
B
EB
_B
�B
�B
�B
KB
�B
B
eB
�B
B
�B
�B
#B
=B
�B
�B
�B
�B
�B
�B
�B
B
dB
�B
�B
OB
�B
�B
�B
 'B
 BB
 'B
 'B
 �B
 �B
!HB
!HB
!B
!B
!-B
!|B
!�B
"�B
#B
#�B
$ZB
%,B
%B
%FB
%zB
%�B
&B
&�B
&�B
&�B
&�B
'RB
'�B
'�B
'�B
($B
(sB
)*B
)DB
)�B
)�B
*�B
*�B
+QB
+�B
,=B
,�B
,�B
-CB
-CB
-CB
-wB
-�B
-�B
./B
.IB
.�B
.}B
.}B
.}B
.�B
/OB
0;B
0�B
1vB
2-B
2�B
33B
3hB
3�B
3�B
3�B
3�B
3�B
3�B
4TB
4�B
4�B
4�B
4�B
5?B
5?B
5tB
6`B
6�B
6�B
6�B
7�B
7�B
7�B
8B
8B
8B
8�B
8�B
8�B
9�B
:*B
:�B
;B
;B
;dB
;B
;B
;0B
;B
:�B
:xB
:�B
:�B
:xB
;B
:�B
;B
;dB
;JB
;0B
;dB
;0B
;JB
;dB
;�B
;�B
<B
<B
;�B
;�B
<jB
=qB
>B
>]B
>BB
>]B
>�B
>�B
?HB
?}B
?�B
@iB
@�B
@�B
@�B
@�B
A B
AB
A B
AUB
A;B
AB
A�B
B[B
A�B
B'B
B�B
CGB
C�B
D3B
EmB
F%B
F%B
F�B
F�B
F�B
GB
G+B
G+B
G�B
G�B
H1B
H�B
H�B
H�B
H�B
IB
IRB
I7B
IRB
JXB
JrB
J�B
J�B
J�B
J�B
J�B
K�B
K�B
KxB
LB
LJB
L0B
L0B
L�B
M6B
MB
MB
M6B
M�B
M�B
M�B
M�B
NVB
N�B
N�B
N�B
N�B
OB
OBB
OBB
O\B
OBB
OBB
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q B
Q�B
R B
R B
Q�B
RB
Q�B
Q�B
RTB
R�B
S@B
S�B
S�B
S�B
S�B
S�B
TaB
T�B
T�B
T�B
UB
T�B
UMB
U�B
VB
VB
VB
V�B
V�B
W?B
W?B
WsB
W�B
XB
X_B
X�B
X�B
YB
YeB
Y�B
ZB
Z�B
[	B
Z�B
ZQB
ZQB
Z�B
[�B
[�B
Z�B
[=B
\]B
\�B
]B
]dB
]dB
]�B
]�B
]�B
^5B
^jB
^�B
_B
_B
_�B
_�B
_�B
_�B
`\B
`vB
`vB
`�B
`�B
`�B
aB
aHB
abB
a�B
a�B
bB
bNB
b4B
b4B
b�B
b�B
b�B
cTB
cTB
cnB
cnB
c�B
c�B
c�B
c�B
dB
dZB
dtB
d�B
d�B
d�B
d�B
eFB
ezB
e�B
e�B
e�B
f�B
f�B
f�B
f�B
gB
f�B
gB
gB
g8B
g�B
g�B
g�B
h$B
h>B
hXB
h�B
h�B
h�B
h�B
iB
iDB
i_B
iyB
i�B
i�B
i�B
i�B
jB
j0B
j0B
jKB
jB
jB
j�B
j�B
j�B
kB
k6B
k�B
k�B
k�B
k�B
k�B
k�B
l=B
lWB
lqB
l�B
l�B
l�B
l�B
l�B
l�B
m)B
mCB
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n/B
nIB
n�B
n�B
n�B
n}B
n�B
o5B
oOB
oOB
o�B
o�B
o�B
o�B
p!B
p;B
poB
p�B
p�B
p�B
p�B
qB
q'B
qAB
q[B
q�B
q�B
q�B
rB
r|B
r�B
r�B
s3B
sB
s�B
tB
tB
tB
tTB
tnB
tnB
t�B
uB
utB
u�B
u�B
u�B
u�B
v`B
vzB
v�B
v�B
v�B
wB
wB
w�B
w�B
xB
xlB
x�B
y$B
y	B
y	B
y>B
yrB
y�B
y�B
z*B
z^B
z�B
{dB
{B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�{B�,B�aB�aB�aB�FB�[B��B�uB�[B��B�@B��B��B�@B�[B�[B�@B�&B��B��B��B��B��B��B�NB��B� B��B� B�4B�B��B��B�}B�.B��B�~B��B��B�B�EB�NB�!B�QB	lWB	��B	�XB
B'B
�]B
��B
��B
�B
�-BfB�BB*�BDMBezBhsBx�BgB=�B�B+B�B
��B
��B
�VB
�6B
��B
z�B
j�B
OvB
)�B
B	��B	�B	ѝB	�EB	bB	Y1B	PbB	I�B	I�B	7�B	,=B	1vB	7�B	F�B	VSB	Q�B	UgB	_B	Q�B	0�B	B'B	>�B	$�B		�B�}B��B��B��B��B��B	AB	�B	+B	�B	�B	
�B	(B	�B	 \B	.B	EmB	I�B	OvB	TFB	R B	N�B	PHB	]B	n�B	q�B	k�B	b�B	mB	y>B	�B	��B	��B	��B	�_B	��B	�B	�NB	�:B	��B	��B	�WB	�B	�/B	��B	��B	��B	��B	�6B	��B	��B	�UB	�AB	�|B	�9B	�%B	�B	�?B	�B	��B	�LB	��B	��B	��B	�|B	��B	�"B	��B	��B	��B	�}B	�uB	�-B	��B	��B	�B	�<B	��B	��B	�B	�EB	�_B	ǮB	�B	�B	��B	�tB	�B	ߊB	ܬB	��B	�uB	�VB	��B	��B	�MB	�?B	ּB	�?B	��B	��B	�9B	�B	�B	��B	�qB	�B	��B	�B	�B	�B	�B	�B	�AB	��B	�LB	��B	�|B	�LB	�B	�B	�]B	��B	�B	�$B	��B	�_B	�B	�B	�B	�B	��B	��B	�B	�8B	�B	�B	��B	�B	��B	�-B	��B	�oB	�B	�OB	�/B	�)B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�nB	�hB	�TB	�tB	�B	�>B	��B	�B	�B	�B	�B	�`B	�B	�B	��B	�qB	�B	�B	��B	�B	�DB	�B	�DB	�mB	�8B	�XB	�
B	�B	�8B	�B	�fB	�KB	��B	�B	�B	�fB	�8B	�RB	��B	��B	�B	��B	�B	�B	�B	��B	��B	�]B	�CB	�)B	�)B	�wB	�B	�B	�OB	�B	��B	�AB	�B	�AB	�B	�AB	�B	�B	�|B	�B	��B	�ZB	�B	��B	��B	��B	�-B	��B	�MB	�TB	��B	��B	�hB	�B	�B	��B	�TB	��B	��B	�!B	�B	��B	�B	�iB	�!B	�B	��B	�B	�[B	�'B	�[B	�B	��B	�aB	�GB	�GB	�3B	�B	�+B	��B	��B	�lB	��B	��B	�`B	�`B	�B	��B	��B	��B	�+B	��B	��B	�RB	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�^B	��B	��B	��B	��B	��B	��B	��B	�]B	�wB	��B	�BB	�<B	��B	�HB	��B
;B
�B
[B
uB
�B
�B
oB
�B
�B
-B
uB
�B
�B
B
�B
�B
SB
B
+B
�B
�B
fB

#B
�B
xB

�B
)B
�B
�B
�B
jB
�B
�B
jB
jB
�B
�B
B
�B
�B
�B
\B
�B
�B
(B
VB
�B
�B
6B
jB
�B
(B
�B
B
B
bB
}B
�B
�B
�B
�B
�B
TB
�B
�B
oB
TB
oB
TB
oB
:B
 B
�B
oB
:B
:B
oB
�B
�B
TB
�B
�B
�B
�B
�B
TB
&B
�B
�B
B
�B
�B
{B
�B
�B
�B
B
B
�B
�B
�B
�B
FB
�B
�B
gB
�B
�B

B
YB
�B
B
EB
_B
�B
�B
�B
KB
�B
B
eB
�B
B
�B
�B
#B
=B
�B
�B
�B
�B
�B
�B
�B
B
dB
�B
�B
OB
�B
�B
�B
 'B
 BB
 'B
 'B
 �B
 �B
!HB
!HB
!B
!B
!-B
!|B
!�B
"�B
#B
#�B
$ZB
%,B
%B
%FB
%zB
%�B
&B
&�B
&�B
&�B
&�B
'RB
'�B
'�B
'�B
($B
(sB
)*B
)DB
)�B
)�B
*�B
*�B
+QB
+�B
,=B
,�B
,�B
-CB
-CB
-CB
-wB
-�B
-�B
./B
.IB
.�B
.}B
.}B
.}B
.�B
/OB
0;B
0�B
1vB
2-B
2�B
33B
3hB
3�B
3�B
3�B
3�B
3�B
3�B
4TB
4�B
4�B
4�B
4�B
5?B
5?B
5tB
6`B
6�B
6�B
6�B
7�B
7�B
7�B
8B
8B
8B
8�B
8�B
8�B
9�B
:*B
:�B
;B
;B
;dB
;B
;B
;0B
;B
:�B
:xB
:�B
:�B
:xB
;B
:�B
;B
;dB
;JB
;0B
;dB
;0B
;JB
;dB
;�B
;�B
<B
<B
;�B
;�B
<jB
=qB
>B
>]B
>BB
>]B
>�B
>�B
?HB
?}B
?�B
@iB
@�B
@�B
@�B
@�B
A B
AB
A B
AUB
A;B
AB
A�B
B[B
A�B
B'B
B�B
CGB
C�B
D3B
EmB
F%B
F%B
F�B
F�B
F�B
GB
G+B
G+B
G�B
G�B
H1B
H�B
H�B
H�B
H�B
IB
IRB
I7B
IRB
JXB
JrB
J�B
J�B
J�B
J�B
J�B
K�B
K�B
KxB
LB
LJB
L0B
L0B
L�B
M6B
MB
MB
M6B
M�B
M�B
M�B
M�B
NVB
N�B
N�B
N�B
N�B
OB
OBB
OBB
O\B
OBB
OBB
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q B
Q�B
R B
R B
Q�B
RB
Q�B
Q�B
RTB
R�B
S@B
S�B
S�B
S�B
S�B
S�B
TaB
T�B
T�B
T�B
UB
T�B
UMB
U�B
VB
VB
VB
V�B
V�B
W?B
W?B
WsB
W�B
XB
X_B
X�B
X�B
YB
YeB
Y�B
ZB
Z�B
[	B
Z�B
ZQB
ZQB
Z�B
[�B
[�B
Z�B
[=B
\]B
\�B
]B
]dB
]dB
]�B
]�B
]�B
^5B
^jB
^�B
_B
_B
_�B
_�B
_�B
_�B
`\B
`vB
`vB
`�B
`�B
`�B
aB
aHB
abB
a�B
a�B
bB
bNB
b4B
b4B
b�B
b�B
b�B
cTB
cTB
cnB
cnB
c�B
c�B
c�B
c�B
dB
dZB
dtB
d�B
d�B
d�B
d�B
eFB
ezB
e�B
e�B
e�B
f�B
f�B
f�B
f�B
gB
f�B
gB
gB
g8B
g�B
g�B
g�B
h$B
h>B
hXB
h�B
h�B
h�B
h�B
iB
iDB
i_B
iyB
i�B
i�B
i�B
i�B
jB
j0B
j0B
jKB
jB
jB
j�B
j�B
j�B
kB
k6B
k�B
k�B
k�B
k�B
k�B
k�B
l=B
lWB
lqB
l�B
l�B
l�B
l�B
l�B
l�B
m)B
mCB
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n/B
nIB
n�B
n�B
n�B
n}B
n�B
o5B
oOB
oOB
o�B
o�B
o�B
o�B
p!B
p;B
poB
p�B
p�B
p�B
p�B
qB
q'B
qAB
q[B
q�B
q�B
q�B
rB
r|B
r�B
r�B
s3B
sB
s�B
tB
tB
tB
tTB
tnB
tnB
t�B
uB
utB
u�B
u�B
u�B
u�B
v`B
vzB
v�B
v�B
v�B
wB
wB
w�B
w�B
xB
xlB
x�B
y$B
y	B
y	B
y>B
yrB
y�B
y�B
z*B
z^B
z�B
{dB
{B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105236  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191832  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191832  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191832                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041840  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041840  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                