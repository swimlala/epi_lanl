CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:11:25Z creation;2022-06-04T19:11:27Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191125  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ر� �Q)1   @ر���l�@0o��-V�d1&�x�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  BA33BG��BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~33C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ�fD[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D�fD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�C3DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�#311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @  @|��@�ffA ��A33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��BA  BGfgBOfgBW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��B�L�B��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��fB�3B��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-ٙC/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CP�CR�CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C~&fC�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D'3D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ�3D[3D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D�3D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�A�D�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD���D��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD���D��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�!�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�AɎ"A�t�A�e,A�>�A�0�A�&A��A�GA���A���A��VA�A��A� �A��A��2A��A���A�бA��#A�w�AǙ1A��A��A�h>A���A�!�A�GzA���Aǐ.A�&A��pAĞA�M6A�<jA��`A���A�PHA��A���A�u�A��A���A�D�A�e�A��"A���A�kQA��$A�A���A��A�CaA��A�"4A�.A��A�p;A���A�+�A�w�A���A��A��nA��.A���A�1�A��'A�ϫA���A��<A�X�A��A��NA�xlA�GA�ȀA��A�@OA���A��A��A���A���A��IA��=A�I�A��OA��A���A�M6A�F�A�8�AɆA{��Az$Aw��At�Aq��Ap�cAo��AnQAmPHAl��Ak��Ak_pAj�YAiN�Ah��Af]dAa�_A^'�A[oiAX�AT��AP�XAN�AM��AJ�AC��A?�rA<��A;�A;7�A9�fA7�;A6|�A4l"A3:�A2=A11�A0�A-��A(��A'�A$�A$($A$'RA$��A%g8A%6zA$�A$ƨA$��A �'A��A�A1�A��ACA�A��A@�A+A��A�AL�A&�A?A�Ae�A>BA��AaAG�A�A(A�cA{�A1A��A	lA��A��A�AK^A�DA��A��As�A�HA iA�A�7A�mA��AzxA��A;dA
�A�8A?}AC-A4�A��A��Al�A��AtTAY�A"hA�0A�AA$A��A�AOA+A1'A�KA*�A˒A�HAg8A ��A �'A L�@���@��@��@��6@�'R@��D@�ƨ@�(@�8@�Q�@�K�@��@�N�@���@��v@�l�@�c @���@�E�@��@�33@�>B@�S&@���@�0@�3�@�J#@�h�@�&�@��N@�\)@ꤩ@��9@�K�@�I@�_@�H�@�?@��@��U@�:*@唯@���@�ں@߆�@ߌ~@��]@���@۷@�|@�L�@�'�@ڵ@�Ta@��@��@�F@��]@�|�@�V�@�~@׺^@�ѷ@���@Ջ�@�%@�i�@���@�F@��'@�)�@�j@В�@�,=@��@�� @�(@�c @�a�@̧�@�z@�d�@�]d@�Q�@�,=@��@�f�@��@�4@ɔ�@Ɋ	@�n/@�&@���@�\�@�ԕ@�?}@��s@��m@ư!@�u�@�b@��d@ņ�@�&@��@���@�9X@���@�-w@�a|@�-�@�-@��@��@���@�}�@��/@��@��?@���@�V�@�4@��*@�j@�+@���@�~@���@�j@��H@�N�@�W�@�B[@���@��@��1@�\�@�.�@��@�x�@�h
@��)@��@@�H�@�!-@�!�@��@�%@��@��?@��u@�0U@���@�7L@���@�l"@���@���@��@�\)@��<@���@�}V@�s�@�_�@�9X@��@���@��$@�]d@���@���@�qv@�Vm@�'�@��@�{@��@���@���@��V@���@�v`@�4@�Ɇ@��r@��@���@��[@�c@�n/@�%@��1@�}V@�$@�ϫ@��P@�Z�@�<6@��`@�_�@��N@�n/@�7L@�ȴ@�h
@�!�@���@�-w@��'@�w�@�d�@�_@��K@���@�<�@�ϫ@�_p@��R@�V�@�M@�L0@�I�@�#:@��@�T�@��|@��O@���@��z@���@���@��@���@�x@�
=@���@�L0@�خ@���@�u�@�7L@��8@���@�ff@�O@��T@���@�|�@�1�@�o@��@�@�@�@��	@��@��u@�\�@�/�@��@��@@�@�'�@��@���@��@��[@���@��A@�c @�\�@�J�@��@�iD@��@��@�O@��T@��V@�33@�ѷ@�6�@��j@��}@��w@���@���@��@��c@���@��@���@�Xy@�(�@��0@�Z�@�o@��	@���@�8�@��@��@�ԕ@��h@��-@�� @���@�33@��@��H@���@���@�*�@��@���@�#�@���@�xl@�M@��+@���@�s�@�8@��@��`@��'@��@�u�@�~@�G@���@��@@�B�@��@�	l@��@��9@���@�Q@�O@��r@��w@���@�j@�<6@���@�Ĝ@��x@��+@�z�@�i�@�Ft@��@;d@o@
=@~�@~��@~��@~�A@~h
@~E�@~�@}�@}�7@}q@|�?@|��@{�@{|�@{$t@zL0@y�@yhs@y%F@x�v@x�$@xtT@x�@w�@wO@w(@v�]@v҉@v�'@vq�@u�@u4@t��@tq@s��@sخ@s��@sJ#@so@r�!@r~�@r?@q�T@q��@qk�@q!�@q�@p��@p��@p�@o��@o�:@oy�@o@n8�@m�@mF@l�)@lQ�@k�@k�}@k�@k1�@jZ�@i�@h�/@h�)@h�9@hu�@h�@g|�@f�@f.�@e��@e�h@e@d[�@c�	@b��@b �@ak�@a/@`�e@`~@_�	@_iD@_'�@^�@^��@^{@]��@]��@]S&@\�O@\A�@[��@Zff@Ym]@X�@Xz�@Wخ@WS�@V�]@V;�@U�C@U�'@U�@TbN@S��@S6z@S9�@R�]@Ri�@Q�@Q�@Q�'@Q\�@P֡@P��@PbN@P1'@P	�@O�&@OiD@OX�@OJ#@OA�@OY@N�1@Ni�@N($@M��@Me,@M�@M�@L�)@L��@Lj@K�}@K|�@KC�@K"�@J�c@J�h@J��@JW�@J
�@I�)@I��@I��@I=�@H�|@H�E@H�O@HC-@H(�@G�[@G�@G��@G|�@GiD@GH�@F�8@F�m@F�L@Fs�@FW�@FGE@F$�@F	@E�@E��@EQ�@E@D��@DPH@C� @CMj@C&@C i@B��@B��@B��@Bi�@BGE@B.�@B4@A��@A�M@AX@@��@@�Y@@9X@?�@?��@?�@@?��@>�y@>�}@>kQ@>B[@>�@=��@=�3@=zx@<�	@<�O@<��@<!@;�@;�g@;�@;�@;��@;RT@;�@:�1@:\�@:;�@:�@9�z@9��@9+�@8�@8�@7H�@7�@6�M@6��@6s�@6M�@6Ov@63�@6&�@5��@5 \@4G@3�@3��@3b�@2��@2��@25?@2�@1��@1�@1�n@1�@0��@0�Y@0l"@0D�@0�@/ƨ@/��@/P�@.��@.�@.0U@-J�@-�@,�E@,��@,��@,m�@,x@+��@*�X@*�R@*��@*�1@)��@)x�@)=�@)*0@)q@)�@(�|@(��@(�u@(PH@(�@'�@'��@'U�@'6z@'(@'S@&�B@&q�@&u@%@%|@%[W@%4@$�)@$�Y@$m�@$%�@#�}@#�f@#(@"�@"�X@"�@"v�@"
�@!�@!:�@ �$@ ��@ 6@ �@�@��@�a@�0@�@n/@�s@��@Ov@ԕ@��@��@s�@Y�@/@�@�9@l"@9X@�@�@=@Y@�m@}V@\�@=q@�@�>@�@��@B�@�E@֡@ѷ@Ɇ@�?@�j@r�@�r@�a@�w@��@n/@@O@Y@�,@��@�\@.�@��@��@�~@zx@a�@�@��@��@Q�@9X@(�@!@�@�w@t�@U�@;d@��@��@V@e@�@��@zx@e,@^�@X@Dg@�K@�@<�@~@�@��@�a@�@8@Y@��@�+@kQ@=q@�@ �@�@��@�@�t@�M@`B@X@=�@%F@@@�@��@�e@z�@:�@ݘ@�a11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�AɎ"A�t�A�e,A�>�A�0�A�&A��A�GA���A���A��VA�A��A� �A��A��2A��A���A�бA��#A�w�AǙ1A��A��A�h>A���A�!�A�GzA���Aǐ.A�&A��pAĞA�M6A�<jA��`A���A�PHA��A���A�u�A��A���A�D�A�e�A��"A���A�kQA��$A�A���A��A�CaA��A�"4A�.A��A�p;A���A�+�A�w�A���A��A��nA��.A���A�1�A��'A�ϫA���A��<A�X�A��A��NA�xlA�GA�ȀA��A�@OA���A��A��A���A���A��IA��=A�I�A��OA��A���A�M6A�F�A�8�AɆA{��Az$Aw��At�Aq��Ap�cAo��AnQAmPHAl��Ak��Ak_pAj�YAiN�Ah��Af]dAa�_A^'�A[oiAX�AT��AP�XAN�AM��AJ�AC��A?�rA<��A;�A;7�A9�fA7�;A6|�A4l"A3:�A2=A11�A0�A-��A(��A'�A$�A$($A$'RA$��A%g8A%6zA$�A$ƨA$��A �'A��A�A1�A��ACA�A��A@�A+A��A�AL�A&�A?A�Ae�A>BA��AaAG�A�A(A�cA{�A1A��A	lA��A��A�AK^A�DA��A��As�A�HA iA�A�7A�mA��AzxA��A;dA
�A�8A?}AC-A4�A��A��Al�A��AtTAY�A"hA�0A�AA$A��A�AOA+A1'A�KA*�A˒A�HAg8A ��A �'A L�@���@��@��@��6@�'R@��D@�ƨ@�(@�8@�Q�@�K�@��@�N�@���@��v@�l�@�c @���@�E�@��@�33@�>B@�S&@���@�0@�3�@�J#@�h�@�&�@��N@�\)@ꤩ@��9@�K�@�I@�_@�H�@�?@��@��U@�:*@唯@���@�ں@߆�@ߌ~@��]@���@۷@�|@�L�@�'�@ڵ@�Ta@��@��@�F@��]@�|�@�V�@�~@׺^@�ѷ@���@Ջ�@�%@�i�@���@�F@��'@�)�@�j@В�@�,=@��@�� @�(@�c @�a�@̧�@�z@�d�@�]d@�Q�@�,=@��@�f�@��@�4@ɔ�@Ɋ	@�n/@�&@���@�\�@�ԕ@�?}@��s@��m@ư!@�u�@�b@��d@ņ�@�&@��@���@�9X@���@�-w@�a|@�-�@�-@��@��@���@�}�@��/@��@��?@���@�V�@�4@��*@�j@�+@���@�~@���@�j@��H@�N�@�W�@�B[@���@��@��1@�\�@�.�@��@�x�@�h
@��)@��@@�H�@�!-@�!�@��@�%@��@��?@��u@�0U@���@�7L@���@�l"@���@���@��@�\)@��<@���@�}V@�s�@�_�@�9X@��@���@��$@�]d@���@���@�qv@�Vm@�'�@��@�{@��@���@���@��V@���@�v`@�4@�Ɇ@��r@��@���@��[@�c@�n/@�%@��1@�}V@�$@�ϫ@��P@�Z�@�<6@��`@�_�@��N@�n/@�7L@�ȴ@�h
@�!�@���@�-w@��'@�w�@�d�@�_@��K@���@�<�@�ϫ@�_p@��R@�V�@�M@�L0@�I�@�#:@��@�T�@��|@��O@���@��z@���@���@��@���@�x@�
=@���@�L0@�خ@���@�u�@�7L@��8@���@�ff@�O@��T@���@�|�@�1�@�o@��@�@�@�@��	@��@��u@�\�@�/�@��@��@@�@�'�@��@���@��@��[@���@��A@�c @�\�@�J�@��@�iD@��@��@�O@��T@��V@�33@�ѷ@�6�@��j@��}@��w@���@���@��@��c@���@��@���@�Xy@�(�@��0@�Z�@�o@��	@���@�8�@��@��@�ԕ@��h@��-@�� @���@�33@��@��H@���@���@�*�@��@���@�#�@���@�xl@�M@��+@���@�s�@�8@��@��`@��'@��@�u�@�~@�G@���@��@@�B�@��@�	l@��@��9@���@�Q@�O@��r@��w@���@�j@�<6@���@�Ĝ@��x@��+@�z�@�i�@�Ft@��@;d@o@
=@~�@~��@~��@~�A@~h
@~E�@~�@}�@}�7@}q@|�?@|��@{�@{|�@{$t@zL0@y�@yhs@y%F@x�v@x�$@xtT@x�@w�@wO@w(@v�]@v҉@v�'@vq�@u�@u4@t��@tq@s��@sخ@s��@sJ#@so@r�!@r~�@r?@q�T@q��@qk�@q!�@q�@p��@p��@p�@o��@o�:@oy�@o@n8�@m�@mF@l�)@lQ�@k�@k�}@k�@k1�@jZ�@i�@h�/@h�)@h�9@hu�@h�@g|�@f�@f.�@e��@e�h@e@d[�@c�	@b��@b �@ak�@a/@`�e@`~@_�	@_iD@_'�@^�@^��@^{@]��@]��@]S&@\�O@\A�@[��@Zff@Ym]@X�@Xz�@Wخ@WS�@V�]@V;�@U�C@U�'@U�@TbN@S��@S6z@S9�@R�]@Ri�@Q�@Q�@Q�'@Q\�@P֡@P��@PbN@P1'@P	�@O�&@OiD@OX�@OJ#@OA�@OY@N�1@Ni�@N($@M��@Me,@M�@M�@L�)@L��@Lj@K�}@K|�@KC�@K"�@J�c@J�h@J��@JW�@J
�@I�)@I��@I��@I=�@H�|@H�E@H�O@HC-@H(�@G�[@G�@G��@G|�@GiD@GH�@F�8@F�m@F�L@Fs�@FW�@FGE@F$�@F	@E�@E��@EQ�@E@D��@DPH@C� @CMj@C&@C i@B��@B��@B��@Bi�@BGE@B.�@B4@A��@A�M@AX@@��@@�Y@@9X@?�@?��@?�@@?��@>�y@>�}@>kQ@>B[@>�@=��@=�3@=zx@<�	@<�O@<��@<!@;�@;�g@;�@;�@;��@;RT@;�@:�1@:\�@:;�@:�@9�z@9��@9+�@8�@8�@7H�@7�@6�M@6��@6s�@6M�@6Ov@63�@6&�@5��@5 \@4G@3�@3��@3b�@2��@2��@25?@2�@1��@1�@1�n@1�@0��@0�Y@0l"@0D�@0�@/ƨ@/��@/P�@.��@.�@.0U@-J�@-�@,�E@,��@,��@,m�@,x@+��@*�X@*�R@*��@*�1@)��@)x�@)=�@)*0@)q@)�@(�|@(��@(�u@(PH@(�@'�@'��@'U�@'6z@'(@'S@&�B@&q�@&u@%@%|@%[W@%4@$�)@$�Y@$m�@$%�@#�}@#�f@#(@"�@"�X@"�@"v�@"
�@!�@!:�@ �$@ ��@ 6@ �@�@��@�a@�0@�@n/@�s@��@Ov@ԕ@��@��@s�@Y�@/@�@�9@l"@9X@�@�@=@Y@�m@}V@\�@=q@�@�>@�@��@B�@�E@֡@ѷ@Ɇ@�?@�j@r�@�r@�a@�w@��@n/@@O@Y@�,@��@�\@.�@��@��@�~@zx@a�@�@��@��@Q�@9X@(�@!@�@�w@t�@U�@;d@��@��@V@e@�@��@zx@e,@^�@X@Dg@�K@�@<�@~@�@��@�a@�@8@Y@��@�+@kQ@=q@�@ �@�@��@�@�t@�M@`B@X@=�@%F@@@�@��@�e@z�@:�@ݘ@�a11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B�kB��B��B��B��B�|B��B�JB��B��B� B��BB�aB�aB�B˒B��B�7B	�B	l=B	|�B	��B	�B	�6B
*�B
S�B
��B
�jB
҉B
҉B
ΥBuB*�B0�B1�B)�B'8B,=B(
B�B �B�BܬB�B˒B�B�B��B��B�B��B �B �BAB �B�B��B�=B�vB�OB�MB�4B�PB�DBɺB��B�-B�B��B��B��B��B�(B�B� BncBL0BI7B=�B%�B
�=B
�jB
�AB
��B
�HB
��B
��B
RTB
�B
MB	�CB	��B	�xB	�-B	�LB	��B	�1B	��B	�B	�MB	HB	|B	{�B	z�B	xB	s�B	n�B	eFB	U�B	IRB	@B	+B	�B	�B	�B	@B	(�B	�B��B�B�B�B�B�mB��B��B�3B��B	�B	B		�B�'B�&B�FB��B�WB	�B	A�B	VB	W�B	XEB	T{B	C�B	.cB	)B	qB	�B	B		7B	mB	%B	~B		B	H�B	ffB	HB	m�B	c�B	j0B	~�B	�{B	�<B	��B	�zB	��B	��B	̈́B	��B	��B	�B	�B	�B	��B	ʦB	��B	ٴB	�kB	�7B	یB	��B	�B	�yB	�B	��B	�gB	�KB	�)B	�EB	��B	�HB	��B	�2B	�dB	��B	�jB	ޞB	ޞB	�jB	�5B	߾B	�B	�vB	�B	�FB	�#B	��B	�B	��B	��B	��B	�\B	�B	ߤB	�hB	�B	ߤB	�B	�;B	�WB	ؓB	ںB	�B	��B	��B	�B	�B	�B	�B	�B	�RB	��B	�B	�B	�B	�B	�B	��B	�tB	�B	�B	��B	�B	�zB	�B	�fB	��B	�B	�B	�2B	�fB	��B	�B	�zB	��B	�B	��B	��B	�B	�_B	�sB	��B	ٚB	�B	��B	յB	��B	��B	��B	چB	��B	��B	�WB	��B	یB	�=B	�	B	�=B	�	B	��B	�?B	ևB	�B	�
B	�YB	��B	�mB	�sB	ؓB	خB	�_B	�B	��B	�yB	��B	�	B	��B	ݲB	��B	�B	޸B	�B	�B	��B	ߊB	ބB	�B	�'B	�B	�tB	�&B	�@B	�,B	��B	��B	��B	��B	�mB	��B	�$B	�sB	�>B	��B	��B	�B	��B	�B	�8B	�B	�B	��B	�B	�DB	�B	��B	�`B	��B	��B	�B	�zB	��B	��B	�sB	��B	��B	�B	�B	�QB	��B	��B	��B	�qB	�B	��B	�CB	��B	�B	�B	��B	�AB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�|B	�B	�B	�B	�B	�TB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�fB	�zB	��B	�tB	�?B	��B	��B	�%B	��B	�%B	�?B	�tB	��B	�tB	�tB	��B	��B	�FB	�LB	�B	�LB	�LB	�2B	��B	�8B	�B	��B	�	B	�XB	�rB	�XB	��B	��B	�dB	��B	��B	�PB	��B	��B	��B	�(B	��B	�.B	��B	��B
�B
AB
�B
B
�B
�B
mB
mB
mB
SB
mB
�B
+B
�B
�B
�B
�B
�B
�B
�B
	RB
	RB
	�B
	�B
	�B

�B
)B

�B

�B
)B
xB
^B
B
JB
�B
jB
<B
�B
B
\B
�B
�B
HB
HB
 B
�B
,B
B
�B
hB
[B
B
B
gB
�B
B
9B
�B

B
?B
?B
�B
mB
�B
2B
{B
MB
�B
?B
B
sB

B
�B
mB
YB
�B

B
mB
SB
mB
mB
9B
�B
�B
sB
sB
+B
�B
�B
	B
	B
�B
IB
 'B
 \B
 �B
 �B
!-B
!HB
!|B
"4B
"B
!�B
!�B
"NB
"�B
"NB
"�B
#nB
#�B
$�B
$�B
%`B
&2B
%�B
&�B
'RB
'�B
'�B
($B
(�B
)DB
)_B
)yB
)�B
*KB
*�B
+B
+6B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,qB
,qB
-B
-�B
.cB
.�B
.�B
.�B
.�B
.�B
.�B
.�B
/ B
/ B
/5B
/�B
/�B
0;B
0UB
0oB
1[B
1[B
1vB
2aB
2GB
2�B
3B
3hB
3�B
3hB
3�B
4TB
4�B
5ZB
5tB
5?B
5?B
5�B
6�B
6�B
7LB
8RB
88B
8B
8�B
8�B
9	B
8�B
9	B
9rB
9�B
:B
:B
:*B
:DB
:DB
:xB
;0B
;dB
;B
;B
;B
<�B
<�B
<�B
<�B
=B
=VB
=qB
=�B
>B
>�B
?.B
?}B
?cB
?cB
?cB
?}B
?�B
@ B
@OB
@iB
@�B
AUB
AB
AUB
@�B
@�B
A;B
A;B
A�B
A�B
BAB
BAB
B�B
B�B
B�B
C-B
C-B
CGB
C{B
DgB
DgB
E9B
ESB
EmB
E�B
E�B
F?B
FYB
FtB
FtB
F%B
GB
G�B
G�B
GEB
GB
F�B
F�B
F�B
G�B
HB
HfB
H�B
IlB
J#B
J=B
I�B
I�B
J�B
JrB
J#B
J	B
I�B
I�B
I�B
I�B
J�B
J=B
JrB
JXB
JrB
KB
K)B
K)B
J�B
J�B
K)B
K)B
KxB
K�B
KxB
K�B
LB
L0B
LJB
LdB
L�B
L�B
MB
MjB
M�B
N<B
N�B
N�B
N�B
N�B
N�B
N�B
OBB
O�B
O�B
P}B
P}B
P}B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q4B
QhB
Q�B
Q�B
RB
R B
R:B
RoB
RoB
RoB
R�B
R�B
R�B
R�B
R�B
R�B
SuB
SuB
S[B
S[B
TaB
T�B
UMB
UgB
UgB
UMB
U2B
U�B
VB
U�B
U�B
VmB
V�B
V�B
V�B
V�B
V�B
WYB
W�B
W�B
W�B
W�B
W�B
XB
XB
X�B
Y1B
YeB
Y�B
Y�B
Y�B
ZB
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[�B
\)B
\)B
\CB
\xB
\�B
\�B
]B
]/B
]IB
]�B
^B
^B
^�B
^�B
^�B
^�B
_!B
_VB
_�B
_�B
_�B
`'B
`�B
`�B
aHB
abB
abB
abB
a�B
bB
b�B
b�B
b�B
b�B
cTB
c�B
c�B
c�B
c�B
c�B
dB
dB
d&B
dZB
d�B
d�B
e,B
eFB
ezB
e�B
ezB
e�B
e�B
f�B
f�B
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
h$B
h�B
h�B
h�B
h�B
hsB
h�B
h�B
i�B
i�B
i�B
j�B
kB
j�B
kB
kB
kB
kB
kQB
k�B
k�B
k�B
lWB
lqB
lWB
lqB
lqB
lqB
l�B
l�B
l�B
l�B
m)B
m]B
m�B
m�B
m�B
nIB
nIB
n}B
o B
n�B
n�B
o B
oiB
o�B
o�B
o�B
pB
o�B
o�B
p;B
p�B
qB
qB
p�B
q�B
q�B
q�B
q�B
q�B
q�B
raB
r�B
r�B
r�B
r�B
r�B
sB
sB
s�B
s�B
s�B
s�B
s�B
s�B
tB
tnB
tnB
tnB
t�B
uB
utB
u�B
v+B
v`B
v�B
v�B
v�B
v�B
v�B
wfB
w�B
xB
xB
xB
xlB
xlB
x�B
x�B
x�B
yXB
y�B
y�B
y�B
zB
zB
zB
z*B
z*B
zDB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B�kB��B��B��B��B�|B��B�JB��B��B� B��BB�aB�aB�B˒B��B�7B	�B	l=B	|�B	��B	�B	�6B
*�B
S�B
��B
�jB
҉B
҉B
ΥBuB*�B0�B1�B)�B'8B,=B(
B�B �B�BܬB�B˒B�B�B��B��B�B��B �B �BAB �B�B��B�=B�vB�OB�MB�4B�PB�DBɺB��B�-B�B��B��B��B��B�(B�B� BncBL0BI7B=�B%�B
�=B
�jB
�AB
��B
�HB
��B
��B
RTB
�B
MB	�CB	��B	�xB	�-B	�LB	��B	�1B	��B	�B	�MB	HB	|B	{�B	z�B	xB	s�B	n�B	eFB	U�B	IRB	@B	+B	�B	�B	�B	@B	(�B	�B��B�B�B�B�B�mB��B��B�3B��B	�B	B		�B�'B�&B�FB��B�WB	�B	A�B	VB	W�B	XEB	T{B	C�B	.cB	)B	qB	�B	B		7B	mB	%B	~B		B	H�B	ffB	HB	m�B	c�B	j0B	~�B	�{B	�<B	��B	�zB	��B	��B	̈́B	��B	��B	�B	�B	�B	��B	ʦB	��B	ٴB	�kB	�7B	یB	��B	�B	�yB	�B	��B	�gB	�KB	�)B	�EB	��B	�HB	��B	�2B	�dB	��B	�jB	ޞB	ޞB	�jB	�5B	߾B	�B	�vB	�B	�FB	�#B	��B	�B	��B	��B	��B	�\B	�B	ߤB	�hB	�B	ߤB	�B	�;B	�WB	ؓB	ںB	�B	��B	��B	�B	�B	�B	�B	�B	�RB	��B	�B	�B	�B	�B	�B	��B	�tB	�B	�B	��B	�B	�zB	�B	�fB	��B	�B	�B	�2B	�fB	��B	�B	�zB	��B	�B	��B	��B	�B	�_B	�sB	��B	ٚB	�B	��B	յB	��B	��B	��B	چB	��B	��B	�WB	��B	یB	�=B	�	B	�=B	�	B	��B	�?B	ևB	�B	�
B	�YB	��B	�mB	�sB	ؓB	خB	�_B	�B	��B	�yB	��B	�	B	��B	ݲB	��B	�B	޸B	�B	�B	��B	ߊB	ބB	�B	�'B	�B	�tB	�&B	�@B	�,B	��B	��B	��B	��B	�mB	��B	�$B	�sB	�>B	��B	��B	�B	��B	�B	�8B	�B	�B	��B	�B	�DB	�B	��B	�`B	��B	��B	�B	�zB	��B	��B	�sB	��B	��B	�B	�B	�QB	��B	��B	��B	�qB	�B	��B	�CB	��B	�B	�B	��B	�AB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�|B	�B	�B	�B	�B	�TB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�fB	�zB	��B	�tB	�?B	��B	��B	�%B	��B	�%B	�?B	�tB	��B	�tB	�tB	��B	��B	�FB	�LB	�B	�LB	�LB	�2B	��B	�8B	�B	��B	�	B	�XB	�rB	�XB	��B	��B	�dB	��B	��B	�PB	��B	��B	��B	�(B	��B	�.B	��B	��B
�B
AB
�B
B
�B
�B
mB
mB
mB
SB
mB
�B
+B
�B
�B
�B
�B
�B
�B
�B
	RB
	RB
	�B
	�B
	�B

�B
)B

�B

�B
)B
xB
^B
B
JB
�B
jB
<B
�B
B
\B
�B
�B
HB
HB
 B
�B
,B
B
�B
hB
[B
B
B
gB
�B
B
9B
�B

B
?B
?B
�B
mB
�B
2B
{B
MB
�B
?B
B
sB

B
�B
mB
YB
�B

B
mB
SB
mB
mB
9B
�B
�B
sB
sB
+B
�B
�B
	B
	B
�B
IB
 'B
 \B
 �B
 �B
!-B
!HB
!|B
"4B
"B
!�B
!�B
"NB
"�B
"NB
"�B
#nB
#�B
$�B
$�B
%`B
&2B
%�B
&�B
'RB
'�B
'�B
($B
(�B
)DB
)_B
)yB
)�B
*KB
*�B
+B
+6B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,qB
,qB
-B
-�B
.cB
.�B
.�B
.�B
.�B
.�B
.�B
.�B
/ B
/ B
/5B
/�B
/�B
0;B
0UB
0oB
1[B
1[B
1vB
2aB
2GB
2�B
3B
3hB
3�B
3hB
3�B
4TB
4�B
5ZB
5tB
5?B
5?B
5�B
6�B
6�B
7LB
8RB
88B
8B
8�B
8�B
9	B
8�B
9	B
9rB
9�B
:B
:B
:*B
:DB
:DB
:xB
;0B
;dB
;B
;B
;B
<�B
<�B
<�B
<�B
=B
=VB
=qB
=�B
>B
>�B
?.B
?}B
?cB
?cB
?cB
?}B
?�B
@ B
@OB
@iB
@�B
AUB
AB
AUB
@�B
@�B
A;B
A;B
A�B
A�B
BAB
BAB
B�B
B�B
B�B
C-B
C-B
CGB
C{B
DgB
DgB
E9B
ESB
EmB
E�B
E�B
F?B
FYB
FtB
FtB
F%B
GB
G�B
G�B
GEB
GB
F�B
F�B
F�B
G�B
HB
HfB
H�B
IlB
J#B
J=B
I�B
I�B
J�B
JrB
J#B
J	B
I�B
I�B
I�B
I�B
J�B
J=B
JrB
JXB
JrB
KB
K)B
K)B
J�B
J�B
K)B
K)B
KxB
K�B
KxB
K�B
LB
L0B
LJB
LdB
L�B
L�B
MB
MjB
M�B
N<B
N�B
N�B
N�B
N�B
N�B
N�B
OBB
O�B
O�B
P}B
P}B
P}B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q4B
QhB
Q�B
Q�B
RB
R B
R:B
RoB
RoB
RoB
R�B
R�B
R�B
R�B
R�B
R�B
SuB
SuB
S[B
S[B
TaB
T�B
UMB
UgB
UgB
UMB
U2B
U�B
VB
U�B
U�B
VmB
V�B
V�B
V�B
V�B
V�B
WYB
W�B
W�B
W�B
W�B
W�B
XB
XB
X�B
Y1B
YeB
Y�B
Y�B
Y�B
ZB
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[�B
\)B
\)B
\CB
\xB
\�B
\�B
]B
]/B
]IB
]�B
^B
^B
^�B
^�B
^�B
^�B
_!B
_VB
_�B
_�B
_�B
`'B
`�B
`�B
aHB
abB
abB
abB
a�B
bB
b�B
b�B
b�B
b�B
cTB
c�B
c�B
c�B
c�B
c�B
dB
dB
d&B
dZB
d�B
d�B
e,B
eFB
ezB
e�B
ezB
e�B
e�B
f�B
f�B
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
h$B
h�B
h�B
h�B
h�B
hsB
h�B
h�B
i�B
i�B
i�B
j�B
kB
j�B
kB
kB
kB
kB
kQB
k�B
k�B
k�B
lWB
lqB
lWB
lqB
lqB
lqB
l�B
l�B
l�B
l�B
m)B
m]B
m�B
m�B
m�B
nIB
nIB
n}B
o B
n�B
n�B
o B
oiB
o�B
o�B
o�B
pB
o�B
o�B
p;B
p�B
qB
qB
p�B
q�B
q�B
q�B
q�B
q�B
q�B
raB
r�B
r�B
r�B
r�B
r�B
sB
sB
s�B
s�B
s�B
s�B
s�B
s�B
tB
tnB
tnB
tnB
t�B
uB
utB
u�B
v+B
v`B
v�B
v�B
v�B
v�B
v�B
wfB
w�B
xB
xB
xB
xlB
xlB
x�B
x�B
x�B
yXB
y�B
y�B
y�B
zB
zB
zB
z*B
z*B
zDB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105226  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191125  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191127  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191127                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041135  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041135  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                