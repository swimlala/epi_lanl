CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-04-09T06:44:22Z creation;2023-04-09T06:44:23Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230409064422  20230409065707  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�"I���?1   @�"J$8!`@0#�
=p��b���"��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffBffBffB��B(  B0  B8  B@  BH  BPffBX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C�C  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC7�fC:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct�Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @fg@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B33B33B33BfgB'��B/��B7��B?��BG��BP33BW��B_fgBg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB�� B��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC �C�3C�C�3CٙC	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5ٙC7ٙC9�3C;�3C>�C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cr�Ct�Cu�3Cw�3Cy�3C{�3C}�3CٙC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��DvgD��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�{3D�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�T�A�[�A�\]A�\�A�Z�A�]dA�[�A�^A�`�A�aA�aHA�b�A�a�A�cTA�d�A�f2A�P�A�8�A�A���A��A��A�ɆA�^�A��AA�s�A��A��;A�d�A�^�A��A��TA�i�A���A�k�A�DA���A��A��?A��A�!�A��A��`A���A��A��A�  A��A��A�QA�>wA�lWA�9�A�R�A���A�($A�5tA�X�A���A��xA�GzA�"�A���A�]/A�N�A�<jA��A�\�A�X�A�� A�FtA���A�p�A�!bA�6�A��LA�_�A�7A���A���A���A��MA�HKA�ɆA�,�A��A~~Az!Asw2Ap��Ao!�AmkQAkffAjd�AiiDAe��Ab�A^~�A\.IAY��AV��AU+�AS4�AQ=AN��AMK^AL��AIJ�AG��AD)�A@zA<�A:4A7VA4�.A3?}A1�TA1��A.u�A+�nA*C�A)�A)��A(��A%xlA#u�A"d�A ��A��A
=A��AYKA�DA��A�EA��A�3A0�A��A҉AZ�AxlA�AOA�hA�FA�6A��A�A�A[�A��Ay�A��A,=AK�A�.A.IA]�A�A��A�hA�A�A˒A=qA�AR�A�A�eA�Ao�A�HA�9A�;AqA�BA�LA��A[WA?}A�]A��A�AS&A�AM�Aj�A�'A�.AVmA��AXA
w�A	�A�AY�A��A,=AxA��A��A�[AOAA�A.IA�DAA A��AYKAW�A�HA�|A��A��A�dAϫA4AJA �uA ��A iDA ��A �@��@��f@���@���@���@�v�@���@�G@�zx@�Ɇ@�*0@��E@���@���@��@��@��z@��@�u%@��@��]@���@���@�b@�@@��y@�S�@��N@�+@蛦@�RT@��@凔@� i@�`�@��@㐗@�?}@�@�o@���@�2�@��@�ѷ@���@�*0@���@���@�J�@��@۳�@��@��@�@@ڶ�@�]d@ٵt@�e,@�2a@�q@�qv@ք�@�@յt@�T�@�C@ԭ�@�3�@���@��@҄�@�1�@���@���@�c @�@�iD@��@��@�~@ͦ�@�/@͢�@�zx@͸�@͊�@ͦ�@�_p@���@̭�@˰�@�+@�� @�1�@�}V@�`�@�\�@�-�@���@ǹ�@ǀ4@ǈf@�v`@�B�@�+@�;d@�L�@Ʈ}@�9X@��Z@�n/@Ě@Ý�@��@�%F@��@�w�@��}@���@���@��W@�˒@��@���@���@�X�@���@�xl@�!@�J@��^@��@�M@�2�@�
�@�T�@���@���@��P@�n�@��@���@�,�@�ں@�q�@��@���@�x@��@��@�_�@��@���@�j@�%@��m@���@��A@�D�@��#@���@�J#@�S@��/@��O@���@�e@���@���@��4@�4@��]@��j@��@��_@�3�@���@�ѷ@�W�@��@��	@��@��o@���@��~@�Mj@�@��@�[�@���@��@��@��w@��f@�g8@��@���@�f�@�J�@�@�w�@���@���@��@@�O@��M@��!@�z�@��@��-@��	@�T�@��|@���@�O@��@�($@��.@�~�@�ȴ@��4@���@���@��8@��/@��'@���@�e�@�Q�@�@���@�Y@��@�q@�?@�1@��@�X@�+@��`@���@��@��@���@�~�@�L�@���@�;�@��;@��@�X�@�Dg@�͟@�5?@�3�@�g8@��@���@���@�A @�YK@��q@�rG@�-w@�g�@�9�@��]@�.�@�˒@���@��@��
@��@��^@�t�@�&�@���@�ѷ@��F@�-�@��&@�U�@��@��j@�l"@�^5@�M�@�5?@��]@��9@��n@�l�@�o@���@�,=@��N@���@�X�@�=�@�*0@�o@���@��@��4@�~@���@���@�E9@��[@�R�@�@��@��@���@���@���@�u�@�RT@�/�@��|@��O@�:*@��@���@��7@��@���@�`�@�Ft@�;�@�4n@�-�@��.@�x@�@��X@�0U@��#@���@�hs@���@�|�@�IR@��@�҉@�Z�@�.�@�$@�
�@�qv@���@��@~͟@~^5@~�@H�@~��@~�+@~4@}S&@}�@}+@}X@}�@|�@{RT@zkQ@y�@xɆ@x�o@x1@v1�@uc@t2�@s�0@r�"@r�'@r�A@q�)@q2a@q4@q%@pbN@o�0@o��@o��@n��@n-@n@m��@m�N@m�C@k�+@k1�@l�@lw�@l@k6z@k6z@k�P@k�@j��@jYK@i0�@h�@h(�@h@h�@h'R@h1'@h,=@h  @g�w@g��@gMj@g�@f��@e��@dz�@c|�@b�}@aA @`w�@`�@_��@^��@^@�@^�}@^�}@]c@]�@\�e@\4n@[˒@\A�@\�)@[�@Ze@Y��@Y@Ze@Z:*@Y��@Yhs@YJ�@X�I@W�@VB[@U�N@V;�@VE�@U��@U�~@Tی@T�e@T��@TXy@T"h@S��@S��@S��@S�	@S1�@R��@R�1@R	@Q�@QDg@Q#�@P��@P�@PM@P7�@O�+@O\)@N��@N&�@Mϫ@M��@MrG@L��@LPH@K��@K�{@KH�@J��@JC�@I��@Iq@H��@HtT@H�o@H��@G��@G�@G�@G�q@G��@Gy�@G6z@G'�@G�@F��@F@�@E��@E�@D�`@D�_@C�W@C�}@C��@C��@C|�@C_p@C�@B�c@B��@B��@Bn�@A�@A�o@A��@A�@A^�@AS&@A/@@��@@��@@��@@Q�@@x@?�;@?��@?@>��@>v�@>$�@>�@=m]@<��@<��@<��@<e�@;�@;�}@;�m@;��@;��@;$t@:c @:6�@:�@:�@:C�@9�"@8�5@8��@8�@8~(@8U2@8�@7�:@7=@6�@6�<@6�<@6�F@6YK@5�#@5�@5*0@4�O@4"h@3�@3��@3|�@3/�@3C@2�@2� @24@1��@1�7@1!�@0�K@0��@0�@0<�@/��@/~�@/~�@/�{@/8@.��@.V@.J@-��@-rG@-Q�@--w@,�p@,e�@+�W@+��@+�@+y�@+dZ@+RT@+iD@+4�@*�@*�s@*��@*v�@*GE@*	@)��@)�@)�d@)�@)��@)��@)��@)e,@)Vm@)J�@);@(�z@(S�@'��@'�q@'�	@'dZ@'"�@&��@&��@&��@&��@&��@&��@&�\@&\�@&?@&J@%�X@%q@$��@$[�@#��@#�W@#خ@#�}@#�6@#�6@#�w@#�0@#t�@# i@"q�@"Ov@"$�@!�3@!��@!+�@ �@ ��@ m�@ 6@ �@�@�a@�@�{@v`@_p@�@�M@�X@kQ@��@�=@j@=�@&�@�f@�@�@j@V�@K^@%�@��@�{@e�@Mj@�@��@h
@@�@)�@4@��@F@ \@��@�I@M@�@��@�@x@"�@�c@�s@�h@6�@@��@��@j@7L@��@�@Ĝ@l"@�
@��@�$@~�@a@Mj@�@��@�@h
@L0@B[@�@��@IR@;@��@��@�z@��@q@H@6@�m@˒@��@�V@/�@�s@�'@��@�@�F@L0@@�@��@��@}�@rG@J�@�@�v@�p@��@�@g8@Ft@ �@1@�Q@��@�4@F�@�@
�@
ȴ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�T�A�[�A�\]A�\�A�Z�A�]dA�[�A�^A�`�A�aA�aHA�b�A�a�A�cTA�d�A�f2A�P�A�8�A�A���A��A��A�ɆA�^�A��AA�s�A��A��;A�d�A�^�A��A��TA�i�A���A�k�A�DA���A��A��?A��A�!�A��A��`A���A��A��A�  A��A��A�QA�>wA�lWA�9�A�R�A���A�($A�5tA�X�A���A��xA�GzA�"�A���A�]/A�N�A�<jA��A�\�A�X�A�� A�FtA���A�p�A�!bA�6�A��LA�_�A�7A���A���A���A��MA�HKA�ɆA�,�A��A~~Az!Asw2Ap��Ao!�AmkQAkffAjd�AiiDAe��Ab�A^~�A\.IAY��AV��AU+�AS4�AQ=AN��AMK^AL��AIJ�AG��AD)�A@zA<�A:4A7VA4�.A3?}A1�TA1��A.u�A+�nA*C�A)�A)��A(��A%xlA#u�A"d�A ��A��A
=A��AYKA�DA��A�EA��A�3A0�A��A҉AZ�AxlA�AOA�hA�FA�6A��A�A�A[�A��Ay�A��A,=AK�A�.A.IA]�A�A��A�hA�A�A˒A=qA�AR�A�A�eA�Ao�A�HA�9A�;AqA�BA�LA��A[WA?}A�]A��A�AS&A�AM�Aj�A�'A�.AVmA��AXA
w�A	�A�AY�A��A,=AxA��A��A�[AOAA�A.IA�DAA A��AYKAW�A�HA�|A��A��A�dAϫA4AJA �uA ��A iDA ��A �@��@��f@���@���@���@�v�@���@�G@�zx@�Ɇ@�*0@��E@���@���@��@��@��z@��@�u%@��@��]@���@���@�b@�@@��y@�S�@��N@�+@蛦@�RT@��@凔@� i@�`�@��@㐗@�?}@�@�o@���@�2�@��@�ѷ@���@�*0@���@���@�J�@��@۳�@��@��@�@@ڶ�@�]d@ٵt@�e,@�2a@�q@�qv@ք�@�@յt@�T�@�C@ԭ�@�3�@���@��@҄�@�1�@���@���@�c @�@�iD@��@��@�~@ͦ�@�/@͢�@�zx@͸�@͊�@ͦ�@�_p@���@̭�@˰�@�+@�� @�1�@�}V@�`�@�\�@�-�@���@ǹ�@ǀ4@ǈf@�v`@�B�@�+@�;d@�L�@Ʈ}@�9X@��Z@�n/@Ě@Ý�@��@�%F@��@�w�@��}@���@���@��W@�˒@��@���@���@�X�@���@�xl@�!@�J@��^@��@�M@�2�@�
�@�T�@���@���@��P@�n�@��@���@�,�@�ں@�q�@��@���@�x@��@��@�_�@��@���@�j@�%@��m@���@��A@�D�@��#@���@�J#@�S@��/@��O@���@�e@���@���@��4@�4@��]@��j@��@��_@�3�@���@�ѷ@�W�@��@��	@��@��o@���@��~@�Mj@�@��@�[�@���@��@��@��w@��f@�g8@��@���@�f�@�J�@�@�w�@���@���@��@@�O@��M@��!@�z�@��@��-@��	@�T�@��|@���@�O@��@�($@��.@�~�@�ȴ@��4@���@���@��8@��/@��'@���@�e�@�Q�@�@���@�Y@��@�q@�?@�1@��@�X@�+@��`@���@��@��@���@�~�@�L�@���@�;�@��;@��@�X�@�Dg@�͟@�5?@�3�@�g8@��@���@���@�A @�YK@��q@�rG@�-w@�g�@�9�@��]@�.�@�˒@���@��@��
@��@��^@�t�@�&�@���@�ѷ@��F@�-�@��&@�U�@��@��j@�l"@�^5@�M�@�5?@��]@��9@��n@�l�@�o@���@�,=@��N@���@�X�@�=�@�*0@�o@���@��@��4@�~@���@���@�E9@��[@�R�@�@��@��@���@���@���@�u�@�RT@�/�@��|@��O@�:*@��@���@��7@��@���@�`�@�Ft@�;�@�4n@�-�@��.@�x@�@��X@�0U@��#@���@�hs@���@�|�@�IR@��@�҉@�Z�@�.�@�$@�
�@�qv@���@��@~͟@~^5@~�@H�@~��@~�+@~4@}S&@}�@}+@}X@}�@|�@{RT@zkQ@y�@xɆ@x�o@x1@v1�@uc@t2�@s�0@r�"@r�'@r�A@q�)@q2a@q4@q%@pbN@o�0@o��@o��@n��@n-@n@m��@m�N@m�C@k�+@k1�@l�@lw�@l@k6z@k6z@k�P@k�@j��@jYK@i0�@h�@h(�@h@h�@h'R@h1'@h,=@h  @g�w@g��@gMj@g�@f��@e��@dz�@c|�@b�}@aA @`w�@`�@_��@^��@^@�@^�}@^�}@]c@]�@\�e@\4n@[˒@\A�@\�)@[�@Ze@Y��@Y@Ze@Z:*@Y��@Yhs@YJ�@X�I@W�@VB[@U�N@V;�@VE�@U��@U�~@Tی@T�e@T��@TXy@T"h@S��@S��@S��@S�	@S1�@R��@R�1@R	@Q�@QDg@Q#�@P��@P�@PM@P7�@O�+@O\)@N��@N&�@Mϫ@M��@MrG@L��@LPH@K��@K�{@KH�@J��@JC�@I��@Iq@H��@HtT@H�o@H��@G��@G�@G�@G�q@G��@Gy�@G6z@G'�@G�@F��@F@�@E��@E�@D�`@D�_@C�W@C�}@C��@C��@C|�@C_p@C�@B�c@B��@B��@Bn�@A�@A�o@A��@A�@A^�@AS&@A/@@��@@��@@��@@Q�@@x@?�;@?��@?@>��@>v�@>$�@>�@=m]@<��@<��@<��@<e�@;�@;�}@;�m@;��@;��@;$t@:c @:6�@:�@:�@:C�@9�"@8�5@8��@8�@8~(@8U2@8�@7�:@7=@6�@6�<@6�<@6�F@6YK@5�#@5�@5*0@4�O@4"h@3�@3��@3|�@3/�@3C@2�@2� @24@1��@1�7@1!�@0�K@0��@0�@0<�@/��@/~�@/~�@/�{@/8@.��@.V@.J@-��@-rG@-Q�@--w@,�p@,e�@+�W@+��@+�@+y�@+dZ@+RT@+iD@+4�@*�@*�s@*��@*v�@*GE@*	@)��@)�@)�d@)�@)��@)��@)��@)e,@)Vm@)J�@);@(�z@(S�@'��@'�q@'�	@'dZ@'"�@&��@&��@&��@&��@&��@&��@&�\@&\�@&?@&J@%�X@%q@$��@$[�@#��@#�W@#خ@#�}@#�6@#�6@#�w@#�0@#t�@# i@"q�@"Ov@"$�@!�3@!��@!+�@ �@ ��@ m�@ 6@ �@�@�a@�@�{@v`@_p@�@�M@�X@kQ@��@�=@j@=�@&�@�f@�@�@j@V�@K^@%�@��@�{@e�@Mj@�@��@h
@@�@)�@4@��@F@ \@��@�I@M@�@��@�@x@"�@�c@�s@�h@6�@@��@��@j@7L@��@�@Ĝ@l"@�
@��@�$@~�@a@Mj@�@��@�@h
@L0@B[@�@��@IR@;@��@��@�z@��@q@H@6@�m@˒@��@�V@/�@�s@�'@��@�@�F@L0@@�@��@��@}�@rG@J�@�@�v@�p@��@�@g8@Ft@ �@1@�Q@��@�4@F�@�@
�@
ȴ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	ƨB	�tB	ƎB	ƎB	�?B	�YB	�%B	�YB	ƎB	ƨB	ƨB	��B	�tB	�tB	ƨB	��B	żB	żB	ŢB	��B
)�B
H1B
W
B
GzB
UgB
�+B
��B
̳B
�B!�B)�BA�BVSBd�BtB��B�PB��B�=B��B�<B��B�(BɆB��B��BoOBd@Bm�B��B��B�KB��B�VBp�B#�BN"ByXB�B�tB�B�_B�
B�NB��BżB�B��B�B{Bq[Bh>BwLBh�BSB,=B{B
�zB
�2B
ϫB
��B
��B
}B
P�B
!�B	��B	�2B	��B	u�B	bhB	YKB	O\B	D�B	?�B	;B	-)B	�B	BB	SB	MB��B�'B�&B�1BΥBżB��B�	B��B�BB�oB�B}VBx�B{B}B��B�iBz^B��B��B��B�RB�1B��B�uB��B��B�mB�gB��B��B�MB��B��B��B��B��B�DB�NB��B�B��B�BB��B�B�/B�ZB��B��B�B��B	VB	&fB	'B	.�B	?�B	D�B	@�B	AUB	<PB	="B	K�B	�7B	�;B	��B	�B	��B	�=B	�B	��B	ևB	�+B	�BB	��B	�B	ߤB	޸B	�OB	�jB	��B	ߤB	�OB	�vB	ߤB	�/B	ٚB	��B	бB	уB	ՁB	��B	ʦB	�B	�4B	�B	��B	��B	��B	��B	��B	�wB	�`B	�B	��B	�GB	��B	�cB	��B	��B	��B	��B	ĜB	̳B	ϑB	�HB	�:B	�B	�7B	��B	�B	ݘB	ߊB	��B	�B	�B	��B	�TB	�`B	�FB	�B	��B	�B	�BB	�&B	�ZB	�B	�nB	�B	��B	�"B	�B	�yB	�B	�\B	�B	�EB	�sB	�B	�eB	ּB	�YB	ևB	��B	��B	ּB	�aB	�uB	�oB	�B	�NB	�HB	��B	�[B	��B	ևB	�9B	��B	�,B	ңB	� B	ҽB	ՁB	�+B	��B	��B	�dB	��B	�B	��B	��B	�4B	�HB	�HB	�HB	��B	�xB	�xB	�CB	��B	یB	ۦB	�CB	�jB	ܒB	ޞB	߾B	�B	ߤB	�B	�B	��B	�'B	�OB	�B	� B	�B	�sB	��B	�UB	��B	�B	��B	�B	�nB	�3B	�!B	�IB	��B	�B	�B	�0B	�B	�QB	��B	��B	�aB	��B	�B	�B	��B	��B	�<B	�JB	�^B	��B	��B	�B	�cB	�kB	�WB	�wB	�IB	�}B	�B	��B	�B	��B	�	B	��B	�B	��B	�jB	��B	�B	�DB	��B	�xB	�B	�"B	��B	��B	��B
 OB
 B	�}B
 �B
 B
 �B	��B	�}B	�cB	��B	�}B	�HB	�.B	�B	�B	�}B
  B	��B	��B	��B
 4B
;B
UB
[B
[B
�B
�B
�B
gB
�B
�B
KB
�B
	B
	�B
	�B
	�B

�B

rB
�B
zB
tB
�B
�B
B
�B
 �B
 4B
 B
  B	��B
  B
 �B
�B
aB
{B
AB
B
�B
�B
;B
 �B
B
�B
�B
[B
�B
�B
B
AB
uB
AB
AB
[B
uB
aB
�B
?B
+B
	�B
	�B
	B
�B
�B

=B
�B
�B
�B
B
B
 B
�B
 B
�B
B
uB
uB
�B
�B
�B
{B
�B
�B
�B
gB
�B
MB
�B
�B
gB
�B
�B
2B
�B
�B
,B
�B
yB
�B
B
�B
B
EB
SB
�B
�B
�B
)B
�B
�B
WB
�B
xB
B
pB
 \B
 �B
!B
!-B
!bB
"4B
"NB
"NB
"B
!bB
!�B
!|B
!�B
!�B
!�B
"B
#�B
$ZB
$tB
$�B
$@B
$ZB
$�B
%`B
%�B
%zB
%�B
%�B
%�B
&LB
'B
'�B
(sB
(sB
(sB
(�B
)yB
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*B
*0B
*�B
*�B
+6B
+B
+B
+B
+B
+6B
+�B
,=B
,qB
,�B
,�B
-CB
.cB
.�B
/ B
/ B
.�B
.�B
.cB
0oB
1[B
1�B
2aB
2�B
3B
2�B
2�B
2GB
2�B
0UB
/�B
.B
-wB
0;B
2�B
2�B
2�B
33B
2�B
2B
2aB
3�B
3�B
2|B
1AB
/�B
/B
./B
-�B
-)B
+�B
)�B
*KB
*eB
+B
*�B
*�B
-�B
-�B
.�B
/iB
/OB
/�B
/�B
1B
1'B
0�B
1�B
3B
4nB
5�B
3�B
3�B
6`B
9�B
9	B
8�B
:B
<�B
=�B
=�B
=qB
<jB
<B
;�B
;�B
<jB
=�B
>�B
@OB
@�B
AB
AoB
AUB
AoB
@�B
@�B
?cB
>�B
>(B
=�B
<�B
<PB
;�B
;JB
;0B
<�B
>(B
="B
=VB
=VB
<�B
<�B
?B
A�B
@�B
?.B
?B
?�B
BB
C�B
D3B
C�B
C�B
CaB
CB
AUB
@�B
B�B
DB
DB
D�B
DB
C�B
C�B
D�B
EB
EmB
E�B
F�B
G+B
G�B
GEB
GEB
G+B
G�B
H�B
H�B
I7B
IRB
I�B
I�B
I�B
J�B
KDB
KxB
K�B
K�B
K�B
LdB
L~B
L�B
L�B
MB
L�B
L�B
MB
L�B
MPB
M�B
OvB
QB
P}B
QhB
Q�B
R�B
S&B
S�B
SuB
S@B
S@B
S�B
S�B
S�B
T�B
TaB
T�B
T�B
T�B
T�B
T�B
UMB
U�B
V�B
W
B
W$B
WYB
W�B
XB
Y1B
ZB
ZB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[=B
[�B
[�B
\CB
]B
]/B
]IB
]~B
^�B
^�B
^�B
^�B
_!B
_�B
_�B
`�B
a�B
a�B
a�B
a-B
aHB
bB
bNB
cnB
cnB
b�B
c:B
d�B
d�B
eFB
eFB
eB
dtB
dZB
e,B
e�B
e�B
f�B
f�B
f�B
f�B
f2B
e�B
e�B
fLB
f�B
g�B
h$B
h$B
h$B
i_B
iyB
i�B
i�B
i�B
i�B
jB
kB
kB
j�B
kB
k�B
k�B
k�B
k�B
k�B
kkB
kQB
kQB
kQB
k�B
k�B
l=B
l=B
l=B
lqB
lqB
l�B
m�B
nB
m�B
n/B
n/B
n}B
n�B
n�B
o B
oB
o B
o B
o B
oB
o5B
oB
o5B
oOB
oiB
oiB
oOB
oOB
oOB
o5B
oOB
o�B
o�B
o�B
pB
p�B
qB
q�B
q�B
raB
r|B
r|B
r�B
r�B
r�B
rGB
r|B
rGB
r�B
r|B
r�B
r�B
sMB
s�B
t9B
tB
tB
tB
tnB
tTB
tnB
t�B
u%B
utB
u%B
u�B
u�B
u�B
u�B
vB
v�B
v�B
v�B
v�B
v�B
v�B
wB
wLB
wLB
wfB
w�B
w�B
w�B
xB
xB
xlB
xRB
x8B
x�B
y$B
y	B
y>B
y$B
yXB
y�B
z*B
z*B
z*B
zDB
z�B
z�B
{0B
{0B
{�B
{�B
|PB
|jB
|PB
|�B
}B
}<B
}"B
}<B
}�B
}�B
~B
~(B
~�B
~�B
~�B
~�B
B
HB
�B
�4B
�OB
�OB
�iB
�iB
��B
��B
��B
�oB
�oB
�oB
��B
��B
�[B
��B
��B
��B
��B
��B
�-B
�GB
�GB
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
�9B
�mB
��B
��B
��B
�B
��B
�%B
��B
��B
��B
��B
��B
�B
�+B
�_B
�EB
��B
��B
��B
��B
�1B
�1B
�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	ƨB	�tB	ƎB	ƎB	�?B	�YB	�%B	�YB	ƎB	ƨB	ƨB	��B	�tB	�tB	ƨB	��B	żB	żB	ŢB	��B
)�B
H1B
W
B
GzB
UgB
�+B
��B
̳B
�B!�B)�BA�BVSBd�BtB��B�PB��B�=B��B�<B��B�(BɆB��B��BoOBd@Bm�B��B��B�KB��B�VBp�B#�BN"ByXB�B�tB�B�_B�
B�NB��BżB�B��B�B{Bq[Bh>BwLBh�BSB,=B{B
�zB
�2B
ϫB
��B
��B
}B
P�B
!�B	��B	�2B	��B	u�B	bhB	YKB	O\B	D�B	?�B	;B	-)B	�B	BB	SB	MB��B�'B�&B�1BΥBżB��B�	B��B�BB�oB�B}VBx�B{B}B��B�iBz^B��B��B��B�RB�1B��B�uB��B��B�mB�gB��B��B�MB��B��B��B��B��B�DB�NB��B�B��B�BB��B�B�/B�ZB��B��B�B��B	VB	&fB	'B	.�B	?�B	D�B	@�B	AUB	<PB	="B	K�B	�7B	�;B	��B	�B	��B	�=B	�B	��B	ևB	�+B	�BB	��B	�B	ߤB	޸B	�OB	�jB	��B	ߤB	�OB	�vB	ߤB	�/B	ٚB	��B	бB	уB	ՁB	��B	ʦB	�B	�4B	�B	��B	��B	��B	��B	��B	�wB	�`B	�B	��B	�GB	��B	�cB	��B	��B	��B	��B	ĜB	̳B	ϑB	�HB	�:B	�B	�7B	��B	�B	ݘB	ߊB	��B	�B	�B	��B	�TB	�`B	�FB	�B	��B	�B	�BB	�&B	�ZB	�B	�nB	�B	��B	�"B	�B	�yB	�B	�\B	�B	�EB	�sB	�B	�eB	ּB	�YB	ևB	��B	��B	ּB	�aB	�uB	�oB	�B	�NB	�HB	��B	�[B	��B	ևB	�9B	��B	�,B	ңB	� B	ҽB	ՁB	�+B	��B	��B	�dB	��B	�B	��B	��B	�4B	�HB	�HB	�HB	��B	�xB	�xB	�CB	��B	یB	ۦB	�CB	�jB	ܒB	ޞB	߾B	�B	ߤB	�B	�B	��B	�'B	�OB	�B	� B	�B	�sB	��B	�UB	��B	�B	��B	�B	�nB	�3B	�!B	�IB	��B	�B	�B	�0B	�B	�QB	��B	��B	�aB	��B	�B	�B	��B	��B	�<B	�JB	�^B	��B	��B	�B	�cB	�kB	�WB	�wB	�IB	�}B	�B	��B	�B	��B	�	B	��B	�B	��B	�jB	��B	�B	�DB	��B	�xB	�B	�"B	��B	��B	��B
 OB
 B	�}B
 �B
 B
 �B	��B	�}B	�cB	��B	�}B	�HB	�.B	�B	�B	�}B
  B	��B	��B	��B
 4B
;B
UB
[B
[B
�B
�B
�B
gB
�B
�B
KB
�B
	B
	�B
	�B
	�B

�B

rB
�B
zB
tB
�B
�B
B
�B
 �B
 4B
 B
  B	��B
  B
 �B
�B
aB
{B
AB
B
�B
�B
;B
 �B
B
�B
�B
[B
�B
�B
B
AB
uB
AB
AB
[B
uB
aB
�B
?B
+B
	�B
	�B
	B
�B
�B

=B
�B
�B
�B
B
B
 B
�B
 B
�B
B
uB
uB
�B
�B
�B
{B
�B
�B
�B
gB
�B
MB
�B
�B
gB
�B
�B
2B
�B
�B
,B
�B
yB
�B
B
�B
B
EB
SB
�B
�B
�B
)B
�B
�B
WB
�B
xB
B
pB
 \B
 �B
!B
!-B
!bB
"4B
"NB
"NB
"B
!bB
!�B
!|B
!�B
!�B
!�B
"B
#�B
$ZB
$tB
$�B
$@B
$ZB
$�B
%`B
%�B
%zB
%�B
%�B
%�B
&LB
'B
'�B
(sB
(sB
(sB
(�B
)yB
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*B
*0B
*�B
*�B
+6B
+B
+B
+B
+B
+6B
+�B
,=B
,qB
,�B
,�B
-CB
.cB
.�B
/ B
/ B
.�B
.�B
.cB
0oB
1[B
1�B
2aB
2�B
3B
2�B
2�B
2GB
2�B
0UB
/�B
.B
-wB
0;B
2�B
2�B
2�B
33B
2�B
2B
2aB
3�B
3�B
2|B
1AB
/�B
/B
./B
-�B
-)B
+�B
)�B
*KB
*eB
+B
*�B
*�B
-�B
-�B
.�B
/iB
/OB
/�B
/�B
1B
1'B
0�B
1�B
3B
4nB
5�B
3�B
3�B
6`B
9�B
9	B
8�B
:B
<�B
=�B
=�B
=qB
<jB
<B
;�B
;�B
<jB
=�B
>�B
@OB
@�B
AB
AoB
AUB
AoB
@�B
@�B
?cB
>�B
>(B
=�B
<�B
<PB
;�B
;JB
;0B
<�B
>(B
="B
=VB
=VB
<�B
<�B
?B
A�B
@�B
?.B
?B
?�B
BB
C�B
D3B
C�B
C�B
CaB
CB
AUB
@�B
B�B
DB
DB
D�B
DB
C�B
C�B
D�B
EB
EmB
E�B
F�B
G+B
G�B
GEB
GEB
G+B
G�B
H�B
H�B
I7B
IRB
I�B
I�B
I�B
J�B
KDB
KxB
K�B
K�B
K�B
LdB
L~B
L�B
L�B
MB
L�B
L�B
MB
L�B
MPB
M�B
OvB
QB
P}B
QhB
Q�B
R�B
S&B
S�B
SuB
S@B
S@B
S�B
S�B
S�B
T�B
TaB
T�B
T�B
T�B
T�B
T�B
UMB
U�B
V�B
W
B
W$B
WYB
W�B
XB
Y1B
ZB
ZB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[=B
[�B
[�B
\CB
]B
]/B
]IB
]~B
^�B
^�B
^�B
^�B
_!B
_�B
_�B
`�B
a�B
a�B
a�B
a-B
aHB
bB
bNB
cnB
cnB
b�B
c:B
d�B
d�B
eFB
eFB
eB
dtB
dZB
e,B
e�B
e�B
f�B
f�B
f�B
f�B
f2B
e�B
e�B
fLB
f�B
g�B
h$B
h$B
h$B
i_B
iyB
i�B
i�B
i�B
i�B
jB
kB
kB
j�B
kB
k�B
k�B
k�B
k�B
k�B
kkB
kQB
kQB
kQB
k�B
k�B
l=B
l=B
l=B
lqB
lqB
l�B
m�B
nB
m�B
n/B
n/B
n}B
n�B
n�B
o B
oB
o B
o B
o B
oB
o5B
oB
o5B
oOB
oiB
oiB
oOB
oOB
oOB
o5B
oOB
o�B
o�B
o�B
pB
p�B
qB
q�B
q�B
raB
r|B
r|B
r�B
r�B
r�B
rGB
r|B
rGB
r�B
r|B
r�B
r�B
sMB
s�B
t9B
tB
tB
tB
tnB
tTB
tnB
t�B
u%B
utB
u%B
u�B
u�B
u�B
u�B
vB
v�B
v�B
v�B
v�B
v�B
v�B
wB
wLB
wLB
wfB
w�B
w�B
w�B
xB
xB
xlB
xRB
x8B
x�B
y$B
y	B
y>B
y$B
yXB
y�B
z*B
z*B
z*B
zDB
z�B
z�B
{0B
{0B
{�B
{�B
|PB
|jB
|PB
|�B
}B
}<B
}"B
}<B
}�B
}�B
~B
~(B
~�B
~�B
~�B
~�B
B
HB
�B
�4B
�OB
�OB
�iB
�iB
��B
��B
��B
�oB
�oB
�oB
��B
��B
�[B
��B
��B
��B
��B
��B
�-B
�GB
�GB
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
�9B
�mB
��B
��B
��B
�B
��B
�%B
��B
��B
��B
��B
��B
�B
�+B
�_B
�EB
��B
��B
��B
��B
�1B
�1B
�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230409064413  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230409064422  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230409064422  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230409064423                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230409064423  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230409064423  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230409065707                      G�O�G�O�G�O�                