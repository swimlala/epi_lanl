CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-12-31T18:03:28Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
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
_FillValue                    ��        ��Argo profile    3.1 1.2 19500101000000  20171231180328  20181025093511  4901545 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  4741                            2C  D   NAVIS_A                         0185                            052512                          863 @�A���1   @�A�e�@;������ch ě��1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��RA��!A��-A���A���A�^5A�A���A���A�x�A�S�A�=qA�/A�{A��/A���A�n�A�^5A�VA�"�A���A��/A���A��^A���A���A���A���A���A���A��\A��A�r�A�Q�A�-A�  A��TA��PA�1'A�S�A�"�A���A��mA�G�A�S�A�t�A�ĜA���A�=qA��PA�oA�t�A��RA�z�A�5?A���A��A�A��wA��hA�dZA�(�A�VA��mA��7A�+A�bNA��A�n�A�A���A�A��\A�$�A��HA��^A�bA��9A��A�(�A���A�l�A�#A~��A{|�Ay��AvA�At5?As�
As�AsXAr�Aq�Ao�wAnr�Am�Al{Ai�#Ahv�Ah1'Ae7LAc��AcS�AbȴAb5?AbA�Aa"�A`(�A^�A^�A]�7A\5?A[|�A[�AZ^5AX�AWAV��AUƨAT�HAShsASdZAS�mAT �AS��AR�HAQl�AP��AO�PAO/AN�DAM��AM�AL��AL~�AK�AJbAI�hAH�9AF�+AE��AD{AC;dAB�AA��AA;dA@A�A?\)A>��A=�hA<  A;A:9XA9�FA9�A89XA8{A7l�A6�9A6M�A5�^A4��A3��A3�A2~�A2-A1��A0��A05?A/�
A/t�A.�A.bNA-G�A,�\A,bA+O�A*ZA)�A)\)A(�HA(ffA'�A&bNA%K�A$ZA$bA"ȴA!;dA ��A ��A �A��A��A�!AVAx�A��AbAA�AK�Av�A�HA�uA(�A?}A1'A~�A�A\)A��A�uA��A"�A��A�A5?A�uA|�AA
ĜA
�+A
E�A
JA	��A	�A�At�A��A�PA�`A=qAp�AVA~�AA+A A�@��@�^5@��j@�ȴ@�&�@�r�@���@�Ĝ@�33@��#@��@�u@�1@�@���@�l�@蛦@���@�\)@�ff@���@�1'@⟾@�X@�9X@߮@�;d@ް!@��@�?}@�1'@ڧ�@�x�@� �@��y@�J@�I�@�ȴ@�M�@��#@�x�@ЋD@�1@���@��@�Ĝ@�A�@˅@�33@��@��`@�(�@�t�@�E�@�G�@ċD@�j@Ĭ@�l�@\@���@��@���@���@�;d@���@�=q@�V@��/@�I�@�;d@��#@�t�@���@�M�@�p�@��w@��H@�`B@�z�@�\)@��@��^@�G�@���@�Q�@���@��@���@���@���@��;@�"�@���@�=q@��h@���@�j@��w@�C�@���@��!@�V@���@���@��^@��7@�7L@�Ĝ@���@�bN@�1'@���@�K�@�~�@�E�@�5?@�?}@�z�@�Z@�1@���@�C�@��\@�p�@��j@�z�@���@�S�@�ȴ@���@�M�@��#@���@�&�@�1'@���@��H@�~�@���@���@�`B@��@���@�bN@�(�@�1@��P@�33@�"�@��@�o@�@��@��@��@�ȴ@��+@��@�p�@�?}@�V@��@�Ĝ@��9@��@�j@�Q�@���@��@���@�=q@��#@��@�7L@�%@��/@��@�Z@�(�@��;@�S�@�33@��H@��R@�~�@�n�@�M�@���@��@���@�@�@��@��^@���@�?}@���@���@��D@��D@�r�@�bN@�bN@�x�@��h@��@��@��@�Z@�I�@��@�w@��@��@~ff@}`B@|��@|�D@|j@|(�@{��@{�
@{��@{S�@{o@z��@zn�@zn�@zM�@y��@y��@y�#@z�\@zn�@y�#@yhs@x�9@w�@w�;@w\)@v�y@vV@vff@u��@u`B@t�D@s��@s��@s��@sC�@so@r~�@r�\@r��@r��@r��@r��@r��@r�\@q��@qG�@p��@p�u@p�u@p�@pbN@o�;@o;d@o+@o�@n��@nv�@nV@n$�@m�T@m��@mp�@n{@r��@s"�@n{@n@m�@n�+@o�w@p�`@o�@mp�@m��@pb@pb@o�@o�@ol�@o;d@n�@o
=@nȴ@o+@n��@nV@n$�@m@m��@n{@n��@n��@nV@m�T@m�-@m�h@m�h@m�@l�@lI�@l1@l9X@lI�@l9X@k�m@k�m@k��@k"�@ko@j��@i�^@i&�@hĜ@h�u@hb@g��@g�@gK�@fV@ep�@e�@d�@d9X@c�m@c��@c33@b-@a�#@a��@ax�@aX@a�@`r�@`1'@_�@_�@_��@_��@`  @_�w@_+@^�R@^ff@]�@]��@\�@\�D@\��@\j@\(�@\�@[�F@[dZ@[C�@["�@[o@[@Z^5@Z�@Y�7@X�`@XA�@W�@V�y@V�y@Vȴ@V�+@Vff@VV@VE�@VE�@V5?@U�-@U��@U��@V@U@U/@T��@TI�@T(�@S�
@S��@SC�@So@R�@R�H@R��@R�H@R��@R�!@R��@R~�@R-@Q�#@Q��@Q��@QX@Q7L@P��@P��@P�u@PbN@O��@O|�@O+@N�R@N5?@M�@M@M�-@M�@M?}@L�@L��@L�D@L(�@L1@Kƨ@K��@KS�@K"�@K@J�H@JM�@I��@I�#@I��@Ihs@H��@HĜ@H��@H�`@HĜ@Hr�@Hb@H  @G�@G�@GK�@G�@F�y@F��@Fv�@F5?@F{@EO�@E/@D�/@D�D@Dj@C�
@C33@C33@C"�@C"�@Co@C@B�H@BM�@@Ĝ@@  @?�w@?l�@?;d@>�y@>$�@=��@=�@=O�@=?}@=�@<z�@<1@;�@:��@:-@:J@9�@9�7@9�@8�@8Q�@8Q�@8bN@8bN@81'@7�;@7;d@6ȴ@6��@6�+@6ff@6E�@6{@5@5��@5p�@5�@4�/@4j@4(�@3ƨ@3��@3dZ@3"�@3@2��@2M�@2M�@2M�@2J@1�#@1hs@1&�@0��@0r�@0bN@01'@0  @/��@/;d@.�y@.ȴ@.��@.�+@.V@.E�@.{@-�@-�T@-��@-O�@,��@,�@,�/@,�j@,�@,�@,��@,z�@,9X@,�@+�
@+t�@+@*��@*M�@)��@)��@)��@)��@)�7@)hs@(�`@(1'@(b@'�w@'l�@';d@'
=@&��@&�y@&ȴ@&��@&�+@&v�@&$�@%@%�h@%?}@$�@$�D@$j@$(�@#�
@#"�@"��@"��@"n�@"^5@"=q@"-@"J@!�#@!�^@!��@!�7@!X@!G�@ ��@ �@ A�@  �@   @�w@\)@
=@�@��@��@v�@E�@$�@$�@@��@�h@`B@V@�j@z�@I�@(�@��@�F@��@dZ@C�@"�@o@�!@~�@^5@=q@�@��@��@X@7L@��@��@�u@r�@bN@A�@ �@  @�;@��@|�@;d@+@��@�@�R@�+@ff@{@��@��@�@`B@`B@�@�@��@�@�D@I�@(�@��@ƨ@�F@��@�@dZ@S�@S�@C�@@��@��@~�@^5@M�@-@J@�^@��@x�@G�@��@Q�@Q�@1'@ �@  @�;@��@�@�@|�@+@��@�y@�R@��@�+@ff@{@@�h@�h@`B@?}@��@��@z�@z�@j@�@�m@�F@�@C�@o@
�@
�!@
��@
~�@
^5@
M�@
J@	��@	��@	�@	�^@	��@	�7@	x�@	hs@	G�@	&�@��@�`@Ĝ@�9@��@�u@�@r�@Q�@b@�@  @  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��RA��!A��-A���A���A�^5A�A���A���A�x�A�S�A�=qA�/A�{A��/A���A�n�A�^5A�VA�"�A���A��/A���A��^A���A���A���A���A���A���A��\A��A�r�A�Q�A�-A�  A��TA��PA�1'A�S�A�"�A���A��mA�G�A�S�A�t�A�ĜA���A�=qA��PA�oA�t�A��RA�z�A�5?A���A��A�A��wA��hA�dZA�(�A�VA��mA��7A�+A�bNA��A�n�A�A���A�A��\A�$�A��HA��^A�bA��9A��A�(�A���A�l�A�#A~��A{|�Ay��AvA�At5?As�
As�AsXAr�Aq�Ao�wAnr�Am�Al{Ai�#Ahv�Ah1'Ae7LAc��AcS�AbȴAb5?AbA�Aa"�A`(�A^�A^�A]�7A\5?A[|�A[�AZ^5AX�AWAV��AUƨAT�HAShsASdZAS�mAT �AS��AR�HAQl�AP��AO�PAO/AN�DAM��AM�AL��AL~�AK�AJbAI�hAH�9AF�+AE��AD{AC;dAB�AA��AA;dA@A�A?\)A>��A=�hA<  A;A:9XA9�FA9�A89XA8{A7l�A6�9A6M�A5�^A4��A3��A3�A2~�A2-A1��A0��A05?A/�
A/t�A.�A.bNA-G�A,�\A,bA+O�A*ZA)�A)\)A(�HA(ffA'�A&bNA%K�A$ZA$bA"ȴA!;dA ��A ��A �A��A��A�!AVAx�A��AbAA�AK�Av�A�HA�uA(�A?}A1'A~�A�A\)A��A�uA��A"�A��A�A5?A�uA|�AA
ĜA
�+A
E�A
JA	��A	�A�At�A��A�PA�`A=qAp�AVA~�AA+A A�@��@�^5@��j@�ȴ@�&�@�r�@���@�Ĝ@�33@��#@��@�u@�1@�@���@�l�@蛦@���@�\)@�ff@���@�1'@⟾@�X@�9X@߮@�;d@ް!@��@�?}@�1'@ڧ�@�x�@� �@��y@�J@�I�@�ȴ@�M�@��#@�x�@ЋD@�1@���@��@�Ĝ@�A�@˅@�33@��@��`@�(�@�t�@�E�@�G�@ċD@�j@Ĭ@�l�@\@���@��@���@���@�;d@���@�=q@�V@��/@�I�@�;d@��#@�t�@���@�M�@�p�@��w@��H@�`B@�z�@�\)@��@��^@�G�@���@�Q�@���@��@���@���@���@��;@�"�@���@�=q@��h@���@�j@��w@�C�@���@��!@�V@���@���@��^@��7@�7L@�Ĝ@���@�bN@�1'@���@�K�@�~�@�E�@�5?@�?}@�z�@�Z@�1@���@�C�@��\@�p�@��j@�z�@���@�S�@�ȴ@���@�M�@��#@���@�&�@�1'@���@��H@�~�@���@���@�`B@��@���@�bN@�(�@�1@��P@�33@�"�@��@�o@�@��@��@��@�ȴ@��+@��@�p�@�?}@�V@��@�Ĝ@��9@��@�j@�Q�@���@��@���@�=q@��#@��@�7L@�%@��/@��@�Z@�(�@��;@�S�@�33@��H@��R@�~�@�n�@�M�@���@��@���@�@�@��@��^@���@�?}@���@���@��D@��D@�r�@�bN@�bN@�x�@��h@��@��@��@�Z@�I�@��@�w@��@��@~ff@}`B@|��@|�D@|j@|(�@{��@{�
@{��@{S�@{o@z��@zn�@zn�@zM�@y��@y��@y�#@z�\@zn�@y�#@yhs@x�9@w�@w�;@w\)@v�y@vV@vff@u��@u`B@t�D@s��@s��@s��@sC�@so@r~�@r�\@r��@r��@r��@r��@r��@r�\@q��@qG�@p��@p�u@p�u@p�@pbN@o�;@o;d@o+@o�@n��@nv�@nV@n$�@m�T@m��@mp�@n{@r��@s"�@n{@n@m�@n�+@o�w@p�`@o�@mp�@m��@pb@pb@o�@o�@ol�@o;d@n�@o
=@nȴ@o+@n��@nV@n$�@m@m��@n{@n��@n��@nV@m�T@m�-@m�h@m�h@m�@l�@lI�@l1@l9X@lI�@l9X@k�m@k�m@k��@k"�@ko@j��@i�^@i&�@hĜ@h�u@hb@g��@g�@gK�@fV@ep�@e�@d�@d9X@c�m@c��@c33@b-@a�#@a��@ax�@aX@a�@`r�@`1'@_�@_�@_��@_��@`  @_�w@_+@^�R@^ff@]�@]��@\�@\�D@\��@\j@\(�@\�@[�F@[dZ@[C�@["�@[o@[@Z^5@Z�@Y�7@X�`@XA�@W�@V�y@V�y@Vȴ@V�+@Vff@VV@VE�@VE�@V5?@U�-@U��@U��@V@U@U/@T��@TI�@T(�@S�
@S��@SC�@So@R�@R�H@R��@R�H@R��@R�!@R��@R~�@R-@Q�#@Q��@Q��@QX@Q7L@P��@P��@P�u@PbN@O��@O|�@O+@N�R@N5?@M�@M@M�-@M�@M?}@L�@L��@L�D@L(�@L1@Kƨ@K��@KS�@K"�@K@J�H@JM�@I��@I�#@I��@Ihs@H��@HĜ@H��@H�`@HĜ@Hr�@Hb@H  @G�@G�@GK�@G�@F�y@F��@Fv�@F5?@F{@EO�@E/@D�/@D�D@Dj@C�
@C33@C33@C"�@C"�@Co@C@B�H@BM�@@Ĝ@@  @?�w@?l�@?;d@>�y@>$�@=��@=�@=O�@=?}@=�@<z�@<1@;�@:��@:-@:J@9�@9�7@9�@8�@8Q�@8Q�@8bN@8bN@81'@7�;@7;d@6ȴ@6��@6�+@6ff@6E�@6{@5@5��@5p�@5�@4�/@4j@4(�@3ƨ@3��@3dZ@3"�@3@2��@2M�@2M�@2M�@2J@1�#@1hs@1&�@0��@0r�@0bN@01'@0  @/��@/;d@.�y@.ȴ@.��@.�+@.V@.E�@.{@-�@-�T@-��@-O�@,��@,�@,�/@,�j@,�@,�@,��@,z�@,9X@,�@+�
@+t�@+@*��@*M�@)��@)��@)��@)��@)�7@)hs@(�`@(1'@(b@'�w@'l�@';d@'
=@&��@&�y@&ȴ@&��@&�+@&v�@&$�@%@%�h@%?}@$�@$�D@$j@$(�@#�
@#"�@"��@"��@"n�@"^5@"=q@"-@"J@!�#@!�^@!��@!�7@!X@!G�@ ��@ �@ A�@  �@   @�w@\)@
=@�@��@��@v�@E�@$�@$�@@��@�h@`B@V@�j@z�@I�@(�@��@�F@��@dZ@C�@"�@o@�!@~�@^5@=q@�@��@��@X@7L@��@��@�u@r�@bN@A�@ �@  @�;@��@|�@;d@+@��@�@�R@�+@ff@{@��@��@�@`B@`B@�@�@��@�@�D@I�@(�@��@ƨ@�F@��@�@dZ@S�@S�@C�@@��@��@~�@^5@M�@-@J@�^@��@x�@G�@��@Q�@Q�@1'@ �@  @�;@��@�@�@|�@+@��@�y@�R@��@�+@ff@{@@�h@�h@`B@?}@��@��@z�@z�@j@�@�m@�F@�@C�@o@
�@
�!@
��@
~�@
^5@
M�@
J@	��@	��@	�@	�^@	��@	�7@	x�@	hs@	G�@	&�@��@�`@Ĝ@�9@��@�u@�@r�@Q�@b@�@  @  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB0!B.B.B-B.B+B(�B"�B�B�B�B�B�B�B�B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BoBPB+BB��B�B�
B�bB\)B:^B,B�B��B�HB�B��B��B��B�RB�B��B��B��B�uB�Bv�BgmBZBT�BJ�BD�B>wB8RB#�B�B�B�BB
��B
�B
�B
��B
ǮB
�qB
�LB
�B
��B
�{B
�B
z�B
u�B
ZB
L�B
9XB
/B
-B
+B
)�B
&�B
�B
{B
DB
B	��B	�;B	��B	��B	�'B	��B	��B	��B	��B	��B	�\B	�=B	�B	� B	}�B	w�B	r�B	n�B	iyB	aHB	[#B	S�B	M�B	K�B	K�B	P�B	^5B	t�B	r�B	q�B	hsB	dZB	_;B	\)B	XB	S�B	Q�B	O�B	M�B	F�B	A�B	=qB	6FB	)�B	#�B	�B	�B	bB	VB	DB	B��B��B��B�B�fB�TB�BB�/B�B�B��B��B��B��BǮBÖB��B�qB�jB�^B�XB�LB�?B�9B�-B�B�B��B��B��B��B��B��B��B��B��B��B�{B�hB�bB�DB�B�B�B� B}�B{�Bz�By�Bw�Bu�Bt�Bu�Bu�Bs�Bq�Bs�Br�Bp�Bn�BjBffBffBdZBcTBbNB_;B\)BS�BO�BK�BG�BD�BB�BA�B@�B?}B>wB=qB;dB9XB8RB6FB49B49B49B2-B1'B1'B0!B0!B/B.B.B-B,B,B+B(�B'�B(�B(�B'�B'�B'�B&�B&�B%�B&�B&�B&�B%�B%�B%�B$�B%�B&�B)�B+B+B,B.B/B.B0!B2-B2-B0!B2-B6FB7LB9XB:^B;dB:^B:^B:^B9XB9XB9XB9XB9XB9XB:^B<jB>wB?}BA�BC�BM�BO�BO�BP�BS�BS�BP�BO�BN�BM�BS�BP�BO�BL�BN�BP�BN�BM�BK�BK�BK�BM�BM�BN�BQ�BR�BS�BS�BT�BVBW
BXB\)B]/B`BBcTBe`BgmBiyBjBk�Bm�Bo�Bp�Bq�Bs�Bt�Bu�Bv�Bw�By�B|�B|�B~�B� B�B�B�7B�JB�bB�hB�uB�uB�uB�{B��B��B��B��B��B�B�B�!B�'B�3B�FB�LB�XB�wB��BĜBŢBȴBɺB��B��B��B��B��B�B�)B�BB�HB�NB�NB�TB�fB�mB�yB�B�B�B�B�B��B��B��B��B��B��B��B��B	B	%B	1B	DB	PB	\B	hB	oB	{B	�B	�B	�B	#�B	%�B	)�B	+B	-B	-B	.B	0!B	2-B	5?B	7LB	8RB	:^B	:^B	:^B	<jB	>wB	@�B	A�B	A�B	B�B	C�B	E�B	O�B	P�B	Q�B	S�B	VB	W
B	YB	[#B	_;B	`BB	aHB	bNB	bNB	cTB	dZB	e`B	ffB	gmB	hsB	hsB	iyB	jB	l�B	m�B	n�B	p�B	v�B	w�B	y�B	|�B	|�B	}�B	}�B	~�B	~�B	� B	�B	�B	�B	�%B	�1B	�=B	�DB	�PB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�}B	�}B	�LB	�RB	�^B	��B	ȴB	��B	��B	ȴB	��B	��B	�
B	�
B	�
B	�B	�B	�)B	�;B	�BB	�HB	�BB	�BB	�HB	�NB	�ZB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
+B
1B

=B
DB
JB
PB
VB
VB
VB
VB
VB
\B
\B
bB
bB
\B
VB
\B
bB
hB
hB
hB
hB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
,B
,B
+B
+B
,B
,B
+B
+B
,B
,B
,B
,B
,B
+B
+B
+B
,B
,B
,B
,B
,B
,B
-B
,B
-B
-B
-B
-B
,B
,B
,B
+B
,B
.B
-B
-B
-B
/B
2-B
33B
33B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
:^B
:^B
;dB
;dB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
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
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
jB
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
l�B
m�B
m�B
m�B
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
q�B
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
r�B
s�B
s�B
s�B
s�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B/�B.0B.B,�B.�B+�B+�B&
B'BPBMBB�B)B�B�B:B�B�B�BQBFB�BBB�B�B�B�B�B�B�B�B"B$BB�B�B��B�iB�VB��BasB<KB.�BRB�B��B��B֟BҊB��B�fB�(B�QB��B�
B��B��B|GBl�B[xBX�BL�BFpB@�B@�B(BB�BqBgB
�B
��B
��B
˚B
�B
�;B
�OB
��B
��B
�B
��B
}pB
}�B
]�B
U<B
>\B
0B
-�B
+�B
+B
*B
#�B
�B
�B
B	��B	��B	��B	�:B	�hB	��B	�&B	�B	��B	��B	�+B	��B	�nB	��B	�yB	y�B	s�B	p�B	m�B	d�B	^NB	VzB	P�B	O�B	K�B	OqB	]�B	u�B	u�B	u�B	joB	f�B	`B	]�B	Z&B	T�B	R�B	P�B	P�B	H�B	B�B	?DB	:�B	+�B	'%B	�B	�B	B	�B	]B	'B	 �B�vB�|B��B�HB�B��B�BيB��B��B��B�bBΐB��B�aB�B�KB��B�B�hB�JB�OB��B��B�B��B�KB��B�NB��B��B��B��B�-B��B�TB��B�CB��B�,B��B�B�|B�B�B|�B{�B|/By�Bw�Bu�BvwBvoBvBvBt�Bs�BsJBq�Bo;BhBhBe�Bd~Bd4Ba�B`�BV7BR>BP�BKBF!BCOBBLBAQB@DB?�B?B>�B9�B:YB8�B5�B5�B6B3B2}B2�B1�B21B0�B.�B0B/[B-�B,�B-B+OB)�B*�B)�B(�B(�B(�B)#B)�B)LB'�B'�B(>B'�B&�B'�B&�B'UB'�B*�B+�B+�B-IB/�B1HB/�B2B3�B3nB2�B4_B7B7�B9�B;�B<<B<FB;�B<B:1B:pB9�B;B;=B:�B;�B>NB@B@�BA�BCZBO�BQBQ�BQ�BURBV�BR�BP�BOPBM�BU�BQ�BQ!BNtBQ�BQ{BO�BN�BM�BL�BM�BN�BOVBP�BR<BS�BT�BT�BU�BV�BW�BY�B]B^�BaJBc�Bf)BhkBjOBk]Bl�BnQBpBqBr8BtDBt�Bu�BwBxRBz�B}2B}KBTB�mB�B�UB��B��B��B��B��B�B�'B�B��B�hB��B�aB��B�B��B�]B��B��B��B�B��B�bB·B�HBƨB�B�B�-B�TB�B�RB�AB��BܽB�`B�^B�dB�oB�rB�mB�wB��B��B�pB�XB��B�	B��B�	B��B�!B�B�B��B��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	[B	$B	&MB	*8B	+LB	-+B	-AB	.�B	0:B	2"B	5<B	7SB	8oB	:�B	:�B	:�B	<�B	>�B	@�B	A�B	A�B	B�B	C�B	D(B	O�B	QB	R�B	T�B	V�B	W-B	Y�B	[2B	_8B	`xB	b>B	cB	b�B	c�B	dzB	e�B	f�B	g�B	h�B	h�B	i�B	j�B	l�B	m�B	n�B	p�B	v�B	w�B	ysB	}B	}TB	~AB	~nB	xB	B	�XB	�TB	�rB	�B	��B	�|B	��B	��B	�ZB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�`B	�B	�	B	�B	��B	��B	��B	�,B	�AB	��B	��B	��B	�DB	�B	�B	�3B	�B	�PB	��B	�lB	�EB	 B	�\B	�dB	�B	��B	�B	�sB	�aB	�zB	�iB	�B	�PB	�B	�:B	�6B	�_B	�B	�iB	�B	�B	��B	�lB	�B	�GB	�0B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	�sB	�B	�B	��B	�B	��B	�B	��B	�B	��B	�5B	�B	� B	�@B	�B	��B	�.B	��B	��B	�B	�GB	�BB	�+B	�)B	�.B	��B	�!B	�B	� B	�B	�B	�LB	�B	�B	�B	��B	��B	��B
 .B
jB
aB
MB
fB
SB
�B
WB
B
zB

qB
YB
�B
�B
sB
oB
iB
kB
�B
�B
�B
�B
�B
8B
}B
bB
�B
�B
�B
zB
xB
zB
�B
�B
�B
B
IB
�B
B
B
�B
�B
�B
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
 B
�B
 �B
 �B
 �B
!B
!B
!�B
!�B
"EB
#B
#B
#0B
#<B
$B
$B
#�B
$B
%B
%!B
&B
&B
&4B
'B
'!B
'B
('B
(B
(B
(B
(jB
(7B
(B
(B
)5B
)XB
)$B
)�B
)�B
*!B
*@B
*PB
*B
*B
*7B
+QB
+)B
,0B
,7B
,.B
,6B
,%B
,B
+B
+:B
,=B
,%B
+bB
+hB
,B
,B
,B
,B
,B
+B
+mB
,
B
,�B
,:B
,EB
,/B
,HB
,�B
-JB
,EB
-4B
- B
--B
-�B
,XB
,iB
,�B
+WB
,&B
.0B
-WB
-eB
-B
/CB
24B
3/B
3=B
4dB
4B
4�B
5�B
5hB
6\B
6eB
6`B
6pB
6�B
6dB
6qB
7�B
7�B
7�B
7�B
7�B
7jB
7�B
8�B
9|B
:�B
:�B
;iB
;kB
<�B
=�B
=�B
>�B
>�B
>�B
?�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
G B
GB
HB
G�B
G�B
H�B
H�B
H�B
I�B
I�B
JB
J.B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
NB
OB
M�B
OB
OB
OB
O�B
QB
Q)B
QhB
R2B
SB
SB
SB
SB
TB
TB
T!B
TB
TB
TB
U'B
UB
UTB
U;B
V6B
V B
V"B
V5B
VOB
WJB
W0B
W0B
X!B
X/B
X9B
X+B
XB
X0B
X;B
YNB
YCB
Y^B
YZB
ZRB
ZHB
Z=B
ZHB
[ZB
[EB
[NB
[DB
\JB
\BB
\{B
\XB
\LB
\LB
]OB
]tB
]\B
]fB
]RB
^rB
^}B
_NB
_[B
_PB
_[B
_\B
_ZB
`eB
`~B
`dB
`B
a`B
awB
akB
amB
azB
boB
b�B
b�B
b�B
bqB
bnB
c]B
c�B
c�B
cyB
cwB
cyB
c�B
d}B
d�B
d�B
dnB
e�B
exB
e�B
eqB
ekB
exB
e�B
e�B
f�B
f�B
f�B
fB
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
iB
i�B
i�B
j�B
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
l�B
m�B
m�B
m�B
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
q�B
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
r�B
s�B
s�B
s�B
s�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Fr�<#�
<#�
<#�
<#�
<#�
<%t*<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.05 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810231352182018102313521820181023135218  AO  ARCAADJP                                                                    20171231180328    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171231180328  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171231180328  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181023135218  QC  PRES            @�ffD��fG�O�                PM  ARSQCTM V1.1                                                                20181023135218  QC  PSAL            @�ffD��fG�O�                PM  ARSQOWGUV1.0CTD_2017v1 + Argo_2017v02                                       20181025093511  IP                  G�O�G�O�G�O�                