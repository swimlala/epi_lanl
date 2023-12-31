CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:43:13Z creation;2022-06-04T17:43:13Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174313  20220610141505  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               rA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��ȶ�j1   @���""""@/G�z�H�c`Q��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   AA��A^ffA�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBi33Bo��BxffB~  B���B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�33B�ffB�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C�C��C	�fC  C  C  C  C  C  C  C  C  C  C   C"33C$�C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ�CL�CN  CP  CQ�fCS�fCU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DRfDR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D���D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܃3D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @fg@|��@�ff@�ffA33A@��A]��A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B`33Bi  BofgBx33B}��B��3B��fB��fB��fB��B��B��fB��fB��fB��fB��fB��fB��B�L�B��fB��fB��fBǳ3B˳3Bϳ3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�C�C� C	ٙC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C"&fC$�C%ٙC'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CF�CG�3CJ�CL�CM�3CO�3CQٙCSٙCUٙCW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3CiٙCk�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DR3DR�3DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��3D�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD܁�DܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��3D�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�#nA�)�A�7�A�9�A�:*A�:�A�8�A�8RA�:�A�@�A�@OA�>A�:^A�<A�7�A�/OA�.IA�1[A�*�A�!�A��A��A��vA���A�A˰�A��A��A�e�A�5A��A�<�A�� Aƨ�A�RTA���AĒ�A�E�A���A�9XA��7A�zA���A���A���A���A�%A�?}A��QA��A�+A��HA�@A�ޞA���A�QNA�{A�уA�s�A�F�A��&A��	A�b�A��#A���A��vA�.A�A�A�t�A���A�SA�uA��*A�r�A��A���A�LdA���A�MjA~;A{�Ax�Au�Amf�Af@�A_5�AY�
AU�jAS�*AP��AL��AI�hAH��AG�AEJ#AD8�AA�,A@�YA>�LA=�A;�A8N<A50�A3�A2��A0�DA/VA.33A-�hA-4A,�A+��A+&A*�xA*.�A)��A)�PA)z�A)tTA)D�A(��A(@�A(0UA(%A'��A'v�A'A&��A%��A%�A$�	A%�A%$�A$�QA$�.A$�A#�A!�}A!&�A!?}A!1�A! �A!{A �A �kA q�A�)A!�A|�A�ZA҉AcA��A�A~(AخA^�A�PA�FAC-A]dAA A�"A�fAJA�4A[WAOA��A��AMjAp�A��A�	An�A�6A+ArGA�jA��A��Am�A$tAƨA��A5?A
U2A	��A	��A	}VA�A{JA�?A��A�HA� A7�A��A$�A�!A�AsAa�A�^AS�A8�AߤAS&A/A"hAqA �:A ,�@��@�rG@��@�x@� �@�Z@���@��j@���@�@���@�Y�@�j@��@�S�@�4@��@��@�d�@�$@�t�@�#:@�m]@�!@�-�@��3@�~�@�2a@�+@��P@�r@��@���@�@�@�5?@�M@�@���@�"@��@�kQ@�  @��@�@�6@�M@�Y@��@��@�;�@�X@���@�v�@��@���@ऩ@�ߤ@�	�@߼�@�zx@�4@��@ޠ�@��@�c�@�%�@�:�@�_@لM@�"�@�Y�@�`B@�~�@֘_@�V�@�H@�{�@��@�qv@Ԉ�@�Ta@��@�o�@ҵ�@�S�@ѐ�@� i@�u�@��@��@�Z�@��@��)@�_@�#�@��@ʉ�@�}�@��@�e�@���@�X@��]@�q�@�
�@ŶF@ħ�@�{�@�,=@þw@Ð�@Ç�@�H�@�ߤ@£�@��@���@�|@���@��@���@�@O@�.I@�(@�z�@�A�@��@�u�@�S@��@��y@��K@��D@�U2@��@��r@���@��"@���@�R�@�'R@���@��@���@�zx@�Dg@���@��6@��.@�u%@�6@��H@�C�@�4@�/@�V@�D�@���@�7L@��D@�@���@��@��]@�ȴ@��@��+@�z@�i�@�G@�<6@���@�~(@��A@��:@�8@���@��@�Q�@�>B@�@�@��@�E9@��	@���@�5?@�e@��@���@�B�@���@�1'@��@���@��@�c @��D@��X@�/@�M�@��@�[W@�*0@���@��@�r�@� �@�ԕ@�8@��@�	@���@�P�@��@��9@�z�@�PH@��@���@�s�@�(@�u%@�J@�ƨ@� \@�Ĝ@�g8@���@�O�@��@�v�@�W�@��$@�~�@�-w@�֡@��e@�Q�@�(�@���@�'�@�tT@�z@�E�@�}V@���@�($@��z@��"@�g�@��]@�M�@�ϫ@�u�@�/@�(@���@�Ɇ@���@�Ft@��}@�/�@���@���@���@�)�@���@�RT@�C@���@�i�@�.�@��@��}@�c�@�J#@�$t@��h@�|�@�;�@�J@��o@�ϫ@��'@��4@�w2@�>�@��X@��x@�p;@�8�@��@���@��@��C@�A�@�%@���@��R@��@�Ft@�.�@�3�@�.�@��@�@�4@��0@��P@�^�@�.I@��@�֡@��<@���@�V@�7�@�~@�@��D@��&@���@�J#@�:�@���@���@���@�c�@�&�@�G@��@��K@���@�<6@��@��@��?@���@�xl@�a|@�H�@�6@��W@���@�o @�F@�
=@��X@�@j�@S�@Y@~�R@}�9@|�`@|V�@{��@{@O@{S@{ i@z��@zn�@yx�@x~(@w~�@wdZ@wRT@w@O@vkQ@u�"@t��@t	�@s�a@s�4@s9�@r�]@rM�@r	@q�@q��@q=�@q�@p�z@pS�@o�@o�q@ol�@o�@o@n�M@n�@nH�@m�@m��@m:�@l�o@k��@k��@k|�@j�"@j� @jn�@j �@i�t@i��@i��@iS&@i�@h��@h[�@h�@g�@g�
@gƨ@g��@g>�@f�}@f� @fGE@f �@e�@eq@d��@dѷ@d��@du�@dD�@d@c��@c|�@c!-@bu%@b\�@a��@a�7@a|@aA @a�@`�@`�.@`tT@`q@`m�@`9X@`  @_s@^��@]��@][W@]<6@]�@\�@\�I@\9X@\@[��@[�q@[��@['�@Z�@Z�h@Z��@Z��@Z?@Y��@Yu�@YIR@X�f@X�I@XS�@W��@W��@W��@W"�@V��@V}V@VYK@U��@Uc�@U*0@T�j@TQ�@S�;@Se�@S�@Rں@RV@Q`B@Q4@P��@P�.@Pj@P:�@O�
@OO@N�1@Nu@M`B@L��@LV�@LM@Kݘ@K��@KA�@K@J҉@J^5@J+k@I��@I�z@I��@H�K@H?�@Hb@G�Q@GdZ@F�c@F�X@F�!@F��@F($@EIR@D�4@D��@DbN@D!@C��@C�0@C�*@Cv`@B�@B��@Bn�@B=q@B�@B�@A��@A��@A�@A�-@A�7@As�@AVm@A%F@A@@�@@�[@@��@@?�@@�@@�@@  @?�@?��@?�@?'�@>��@>Z�@>E�@>.�@>	@=^�@=q@<��@<D�@;� @;=@;�@:�@:�B@:�}@:��@:W�@9�o@9��@94@9�@8�j@8'R@7�:@7�@6�@6�@6�+@6$�@5�@5�@5Q�@4��@4S�@4/�@4�@3�@3��@3�@2��@2a|@2Q@2C�@2�@1��@1��@1|@1Vm@1@@0�K@0U2@07@0�@0G@/�K@/��@/t�@.�,@.3�@-��@-��@-S&@,�@,c�@,A�@+�m@+v`@+/�@*�@*s�@*�@)��@)��@)��@)�S@)f�@)8�@)@@)�@(�	@(�j@(l"@(M@(<�@(�@'�&@'��@'X�@'C�@&�@&�@&��@&Z�@&O@%�t@%��@%S&@%%F@$�v@$��@$��@$2�@#�m@#�*@#�	@#s@#F�@#'�@#�@"�@"�B@"��@"��@"�\@"C�@"-@!��@!c@!:�@ �/@ |�@ �@��@�P@�4@x@j�@dZ@_p@]�@X�@Z�@W?@P�@O@U�@;d@҉@4@�d@�-@�@�S@T�@�@�/@�.@>B@�]@�g@��@;d@�@��@�@�y@ں@��@M�@@�@�n@w2@k�@:�@q@�@��@�@Q�@��@ݘ@�:@C�@S@��@�@h
@0U@�@�@�7@hs@#�@�p@��@g8@K^@D�@/�@��@�V@�@e�@>�@.I@�@�c@��@�]@�!@��@h
@�@�T@�z@��@w2@!�@%@�|@�O@�Y@H@�A@�0@��@iD@4�@�y@�F@W�@u@��@��@O�@7L@0�@!�@��@��@c�@��@�K@��@��@v`@=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�#nA�)�A�7�A�9�A�:*A�:�A�8�A�8RA�:�A�@�A�@OA�>A�:^A�<A�7�A�/OA�.IA�1[A�*�A�!�A��A��A��vA���A�A˰�A��A��A�e�A�5A��A�<�A�� Aƨ�A�RTA���AĒ�A�E�A���A�9XA��7A�zA���A���A���A���A�%A�?}A��QA��A�+A��HA�@A�ޞA���A�QNA�{A�уA�s�A�F�A��&A��	A�b�A��#A���A��vA�.A�A�A�t�A���A�SA�uA��*A�r�A��A���A�LdA���A�MjA~;A{�Ax�Au�Amf�Af@�A_5�AY�
AU�jAS�*AP��AL��AI�hAH��AG�AEJ#AD8�AA�,A@�YA>�LA=�A;�A8N<A50�A3�A2��A0�DA/VA.33A-�hA-4A,�A+��A+&A*�xA*.�A)��A)�PA)z�A)tTA)D�A(��A(@�A(0UA(%A'��A'v�A'A&��A%��A%�A$�	A%�A%$�A$�QA$�.A$�A#�A!�}A!&�A!?}A!1�A! �A!{A �A �kA q�A�)A!�A|�A�ZA҉AcA��A�A~(AخA^�A�PA�FAC-A]dAA A�"A�fAJA�4A[WAOA��A��AMjAp�A��A�	An�A�6A+ArGA�jA��A��Am�A$tAƨA��A5?A
U2A	��A	��A	}VA�A{JA�?A��A�HA� A7�A��A$�A�!A�AsAa�A�^AS�A8�AߤAS&A/A"hAqA �:A ,�@��@�rG@��@�x@� �@�Z@���@��j@���@�@���@�Y�@�j@��@�S�@�4@��@��@�d�@�$@�t�@�#:@�m]@�!@�-�@��3@�~�@�2a@�+@��P@�r@��@���@�@�@�5?@�M@�@���@�"@��@�kQ@�  @��@�@�6@�M@�Y@��@��@�;�@�X@���@�v�@��@���@ऩ@�ߤ@�	�@߼�@�zx@�4@��@ޠ�@��@�c�@�%�@�:�@�_@لM@�"�@�Y�@�`B@�~�@֘_@�V�@�H@�{�@��@�qv@Ԉ�@�Ta@��@�o�@ҵ�@�S�@ѐ�@� i@�u�@��@��@�Z�@��@��)@�_@�#�@��@ʉ�@�}�@��@�e�@���@�X@��]@�q�@�
�@ŶF@ħ�@�{�@�,=@þw@Ð�@Ç�@�H�@�ߤ@£�@��@���@�|@���@��@���@�@O@�.I@�(@�z�@�A�@��@�u�@�S@��@��y@��K@��D@�U2@��@��r@���@��"@���@�R�@�'R@���@��@���@�zx@�Dg@���@��6@��.@�u%@�6@��H@�C�@�4@�/@�V@�D�@���@�7L@��D@�@���@��@��]@�ȴ@��@��+@�z@�i�@�G@�<6@���@�~(@��A@��:@�8@���@��@�Q�@�>B@�@�@��@�E9@��	@���@�5?@�e@��@���@�B�@���@�1'@��@���@��@�c @��D@��X@�/@�M�@��@�[W@�*0@���@��@�r�@� �@�ԕ@�8@��@�	@���@�P�@��@��9@�z�@�PH@��@���@�s�@�(@�u%@�J@�ƨ@� \@�Ĝ@�g8@���@�O�@��@�v�@�W�@��$@�~�@�-w@�֡@��e@�Q�@�(�@���@�'�@�tT@�z@�E�@�}V@���@�($@��z@��"@�g�@��]@�M�@�ϫ@�u�@�/@�(@���@�Ɇ@���@�Ft@��}@�/�@���@���@���@�)�@���@�RT@�C@���@�i�@�.�@��@��}@�c�@�J#@�$t@��h@�|�@�;�@�J@��o@�ϫ@��'@��4@�w2@�>�@��X@��x@�p;@�8�@��@���@��@��C@�A�@�%@���@��R@��@�Ft@�.�@�3�@�.�@��@�@�4@��0@��P@�^�@�.I@��@�֡@��<@���@�V@�7�@�~@�@��D@��&@���@�J#@�:�@���@���@���@�c�@�&�@�G@��@��K@���@�<6@��@��@��?@���@�xl@�a|@�H�@�6@��W@���@�o @�F@�
=@��X@�@j�@S�@Y@~�R@}�9@|�`@|V�@{��@{@O@{S@{ i@z��@zn�@yx�@x~(@w~�@wdZ@wRT@w@O@vkQ@u�"@t��@t	�@s�a@s�4@s9�@r�]@rM�@r	@q�@q��@q=�@q�@p�z@pS�@o�@o�q@ol�@o�@o@n�M@n�@nH�@m�@m��@m:�@l�o@k��@k��@k|�@j�"@j� @jn�@j �@i�t@i��@i��@iS&@i�@h��@h[�@h�@g�@g�
@gƨ@g��@g>�@f�}@f� @fGE@f �@e�@eq@d��@dѷ@d��@du�@dD�@d@c��@c|�@c!-@bu%@b\�@a��@a�7@a|@aA @a�@`�@`�.@`tT@`q@`m�@`9X@`  @_s@^��@]��@][W@]<6@]�@\�@\�I@\9X@\@[��@[�q@[��@['�@Z�@Z�h@Z��@Z��@Z?@Y��@Yu�@YIR@X�f@X�I@XS�@W��@W��@W��@W"�@V��@V}V@VYK@U��@Uc�@U*0@T�j@TQ�@S�;@Se�@S�@Rں@RV@Q`B@Q4@P��@P�.@Pj@P:�@O�
@OO@N�1@Nu@M`B@L��@LV�@LM@Kݘ@K��@KA�@K@J҉@J^5@J+k@I��@I�z@I��@H�K@H?�@Hb@G�Q@GdZ@F�c@F�X@F�!@F��@F($@EIR@D�4@D��@DbN@D!@C��@C�0@C�*@Cv`@B�@B��@Bn�@B=q@B�@B�@A��@A��@A�@A�-@A�7@As�@AVm@A%F@A@@�@@�[@@��@@?�@@�@@�@@  @?�@?��@?�@?'�@>��@>Z�@>E�@>.�@>	@=^�@=q@<��@<D�@;� @;=@;�@:�@:�B@:�}@:��@:W�@9�o@9��@94@9�@8�j@8'R@7�:@7�@6�@6�@6�+@6$�@5�@5�@5Q�@4��@4S�@4/�@4�@3�@3��@3�@2��@2a|@2Q@2C�@2�@1��@1��@1|@1Vm@1@@0�K@0U2@07@0�@0G@/�K@/��@/t�@.�,@.3�@-��@-��@-S&@,�@,c�@,A�@+�m@+v`@+/�@*�@*s�@*�@)��@)��@)��@)�S@)f�@)8�@)@@)�@(�	@(�j@(l"@(M@(<�@(�@'�&@'��@'X�@'C�@&�@&�@&��@&Z�@&O@%�t@%��@%S&@%%F@$�v@$��@$��@$2�@#�m@#�*@#�	@#s@#F�@#'�@#�@"�@"�B@"��@"��@"�\@"C�@"-@!��@!c@!:�@ �/@ |�@ �@��@�P@�4@x@j�@dZ@_p@]�@X�@Z�@W?@P�@O@U�@;d@҉@4@�d@�-@�@�S@T�@�@�/@�.@>B@�]@�g@��@;d@�@��@�@�y@ں@��@M�@@�@�n@w2@k�@:�@q@�@��@�@Q�@��@ݘ@�:@C�@S@��@�@h
@0U@�@�@�7@hs@#�@�p@��@g8@K^@D�@/�@��@�V@�@e�@>�@.I@�@�c@��@�]@�!@��@h
@�@�T@�z@��@w2@!�@%@�|@�O@�Y@H@�A@�0@��@iD@4�@�y@�F@W�@u@��@��@O�@7L@0�@!�@��@��@c�@��@�K@��@��@v`@=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�6B��B�JB�JB�JB�JB�dB�B�dB�0B�JB��B�"B�B�(B��B� B��B��B��B��BðB�fB�{B�%B	#B	F�B	s�B	� B	��B	��B	�kB	��B
�B
,�B
e,B
� B
��B
��BX+B��B֡B4B�B�B�JB��B�B�DB��B��B�BÖB��B��B�EB��B�aBu�Bb4B^OB[qBW�BQBJrB=�B,�B
��B
͟B
��B
�~B
�zB
sB
[�B
<6B
,=B
"�B
{B
�B	��B	��B	ʌB	��B	�DB	kB	MB	7�B	)DB	 �B	�B	B	 4B��B��B��B�+B��B�UB�OB��B�B��B�'B�B�tB�B�)B�HB	�B	4B	�B	$B	*eB	0�B	7�B	H�B	N�B	p�B	� B	��B	�B	�-B	�B	�B	�4B	�mB	�)B	ϫB	��B	��B	��B	�FB	ڠB	��B	ݲB	�xB	��B	֡B	�B	�B	��B	�8B	�RB	�B	�B	�B	�B	��B	�	B	��B	��B	��B	�JB	�PB	��B	�2B	�9B	��B	�B	��B	�_B	�B	�HB	�5B	�;B	��B	�!B	�B	��B	��B	��B	�cB	�B	�sB	�B	�B	�ZB	�B	��B	�B	�B	�"B	��B	�B	�UB	��B	� B	�B	��B	�)B	�B	�B	�)B	�WB	�)B	��B	�B	��B	��B	�]B	�B	�
B	�_B	��B	�B	�B	��B	�B	��B	�B	�B	�B	��B	�_B	�_B	�$B	�B	��B	�/B	��B	��B	�$B	��B	��B	�.B	ΥB	�B	��B	��B	��B	�3B	��B	�B	�B	�MB	�B	�B	�-B	��B	�vB	��B	�3B	�TB	��B	��B	��B	�DB	�B	�dB	��B	��B	��B	�FB	�B	��B	�TB	��B	�hB	�B	�B	�B	�oB	�B	�B	�iB	�UB	�B	�;B	�?B	�rB	�B	�B	�PB	�jB	�B	��B	�B	��B	�LB	��B	�aB	�GB	�B	�hB	�TB	� B	�B	��B	� B	�9B	�tB	��B	�?B	�FB	�8B	�lB	�LB	��B	�fB	�`B	��B	�hB	�B	�B	�B	�B	�0B	�B	�_B	�B	�B	��B	�zB	��B	��B	�B	��B	�WB	�CB	�eB	��B	��B	��B	�eB	��B	�B	�[B	�B	��B	��B	�B	��B	�oB	�;B	�;B	�B	�AB	�B	�[B	�GB	�9B	�9B	��B	��B	�B	��B	��B	�+B	�+B	�+B	��B	��B	�zB	��B	�B	��B	��B	��B	��B	��B	�XB	�DB	�^B	��B	��B	�B	�0B	�B	�JB	��B	�jB	��B	��B	�<B	��B	��B	��B	��B	��B	��B	��B	��B	�cB
  B	�}B	��B	�cB	��B	�BB	�(B	�B	�(B	��B
B
�B
�B
AB
�B
aB
B
�B
�B
�B
�B
uB
�B
�B
�B
B
�B
�B
uB
�B
-B
�B
�B
�B
�B
gB
�B
�B
�B
�B
�B
EB
�B
KB
B
B
KB
KB
�B
	B
�B
�B
	�B

�B
^B
�B
�B
�B
B
B
pB
bB
�B
�B
bB
B
�B
�B
BB
BB
pB
VB
(B
�B
�B
B
�B
�B
$B
�B
yB
�B
_B
�B
7B
�B
WB
�B
�B
IB
IB
�B
B
jB
jB
!B
�B
 �B
 �B
!|B
"4B
"4B
"hB
"�B
# B
#B
#:B
#�B
$B
$tB
$�B
%,B
%�B
%�B
%�B
%�B
&B
&�B
'B
'mB
(
B
'�B
(
B
(
B
(
B
($B
'�B
'�B
'�B
(�B
)_B
)_B
)�B
)�B
)yB
)_B
)�B
*B
)�B
)DB
)yB
)�B
)�B
)�B
*KB
*B
*�B
*�B
*�B
*�B
*�B
+6B
+QB
+�B
+�B
,WB
,�B
-CB
.�B
.�B
/ B
/5B
/iB
/�B
0!B
0UB
0oB
0�B
0�B
0�B
0�B
0�B
1AB
1�B
1[B
1�B
1�B
1�B
2�B
2�B
2�B
3hB
3MB
3B
2aB
2-B
2|B
2�B
3B
33B
4B
5�B
5%B
5%B
4�B
5?B
5tB
6B
5�B
4�B
5tB
5�B
5�B
5�B
5tB
5�B
5�B
5�B
6FB
6zB
6zB
6�B
7B
72B
7�B
7�B
8B
8lB
8lB
8lB
9$B
9�B
9�B
:�B
;0B
;JB
<6B
<�B
=�B
=�B
>B
>B
>�B
>�B
>�B
>�B
?}B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
AoB
A�B
A�B
A�B
A�B
B[B
B�B
B�B
CGB
CGB
C�B
EB
E9B
E9B
E9B
ESB
EB
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F%B
F�B
G+B
G�B
GzB
GzB
G�B
G�B
G�B
HB
HfB
H�B
H�B
IlB
IlB
I�B
I�B
I�B
J#B
JrB
J�B
J�B
J�B
J�B
KB
K)B
K�B
K�B
LB
K�B
K�B
K�B
LB
LdB
L~B
L�B
M�B
N"B
N�B
N�B
N�B
O\B
P.B
PHB
P�B
P�B
P�B
P�B
QB
QhB
RB
RTB
R�B
S[B
S@B
SuB
S�B
S�B
S�B
S�B
TB
T,B
TFB
TaB
T{B
TFB
U2B
UgB
UgB
U�B
U�B
V9B
V9B
VB
VmB
V�B
WYB
W�B
W�B
W�B
XB
XEB
X_B
XyB
XyB
X�B
Y1B
YKB
YeB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Y�B
ZB
ZQB
Z7B
ZQB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[#B
[=B
[�B
[�B
\)B
\)B
\CB
\CB
\�B
\�B
]B
]�B
^B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_VB
_�B
_�B
_�B
`B
`vB
`�B
abB
a|B
a|B
a�B
a�B
b4B
b4B
bhB
cB
c B
c:B
cnB
c�B
c�B
d&B
d�B
d�B
d�B
d�B
d�B
d�B
e,B
eFB
e`B
ezB
e�B
fLB
ffB
fLB
ffB
f�B
f�B
f�B
gRB
g�B
g�B
h$B
h$B
h�B
h�B
h�B
h�B
h�B
iDB
iyB
i�B
i�B
i�B
i�B
jB
jB
jKB
jB
jB
jB
jB
j�B
j�B
kB
kB
k6B
kQB
k�B
k�B
k�B
l=B
lWB
lqB
lqB
l�B
mCB
m]B
mwB
mwB
m�B
m�B
m�B
nB
nIB
n}B
n}B
n}B
n�B
n�B
n�B
n�B
o B
o B
n�B
oB
oB
oB
oiB
o�B
o�B
o�B
o�B
p!B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qAB
q�B
r-B
rGB
rGB
rGB
r�B
r�B
sB
sMB
s�B
s�B
tB
t9B
t�B
t�B
t�B
uB
uB
t�B
u?B
utB
u�B
u�B
v`B
v`B
vzB
v�B
v�B
v�B
wB
wB
wLB
w�B
w�B
w�B
w�B
w�B
xB
xB
xRB
x8B
xlB
x�B
x�B
x�B
x�B
y>B
yrB
y�B
y�B
y�B
y�B
z*B
zxB
z�B
z�B
z�B
z�B
{dB
{�B
{B
{B
{�B
{�B
{�B
|B
|6B
|PB
|jB
|�B
|�B
}B
}B
}VB
}�B
}�B
~B
~wB
~�B
~�B
B
.B
�B
�B
�OB
��B
�iB
��B
��B
��B
��B
�;B
�oB
��B
�'B
�'B
�AB
�[B
�[B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�6B��B�JB�JB�JB�JB�dB�B�dB�0B�JB��B�"B�B�(B��B� B��B��B��B��BðB�fB�{B�%B	#B	F�B	s�B	� B	��B	��B	�kB	��B
�B
,�B
e,B
� B
��B
��BX+B��B֡B4B�B�B�JB��B�B�DB��B��B�BÖB��B��B�EB��B�aBu�Bb4B^OB[qBW�BQBJrB=�B,�B
��B
͟B
��B
�~B
�zB
sB
[�B
<6B
,=B
"�B
{B
�B	��B	��B	ʌB	��B	�DB	kB	MB	7�B	)DB	 �B	�B	B	 4B��B��B��B�+B��B�UB�OB��B�B��B�'B�B�tB�B�)B�HB	�B	4B	�B	$B	*eB	0�B	7�B	H�B	N�B	p�B	� B	��B	�B	�-B	�B	�B	�4B	�mB	�)B	ϫB	��B	��B	��B	�FB	ڠB	��B	ݲB	�xB	��B	֡B	�B	�B	��B	�8B	�RB	�B	�B	�B	�B	��B	�	B	��B	��B	��B	�JB	�PB	��B	�2B	�9B	��B	�B	��B	�_B	�B	�HB	�5B	�;B	��B	�!B	�B	��B	��B	��B	�cB	�B	�sB	�B	�B	�ZB	�B	��B	�B	�B	�"B	��B	�B	�UB	��B	� B	�B	��B	�)B	�B	�B	�)B	�WB	�)B	��B	�B	��B	��B	�]B	�B	�
B	�_B	��B	�B	�B	��B	�B	��B	�B	�B	�B	��B	�_B	�_B	�$B	�B	��B	�/B	��B	��B	�$B	��B	��B	�.B	ΥB	�B	��B	��B	��B	�3B	��B	�B	�B	�MB	�B	�B	�-B	��B	�vB	��B	�3B	�TB	��B	��B	��B	�DB	�B	�dB	��B	��B	��B	�FB	�B	��B	�TB	��B	�hB	�B	�B	�B	�oB	�B	�B	�iB	�UB	�B	�;B	�?B	�rB	�B	�B	�PB	�jB	�B	��B	�B	��B	�LB	��B	�aB	�GB	�B	�hB	�TB	� B	�B	��B	� B	�9B	�tB	��B	�?B	�FB	�8B	�lB	�LB	��B	�fB	�`B	��B	�hB	�B	�B	�B	�B	�0B	�B	�_B	�B	�B	��B	�zB	��B	��B	�B	��B	�WB	�CB	�eB	��B	��B	��B	�eB	��B	�B	�[B	�B	��B	��B	�B	��B	�oB	�;B	�;B	�B	�AB	�B	�[B	�GB	�9B	�9B	��B	��B	�B	��B	��B	�+B	�+B	�+B	��B	��B	�zB	��B	�B	��B	��B	��B	��B	��B	�XB	�DB	�^B	��B	��B	�B	�0B	�B	�JB	��B	�jB	��B	��B	�<B	��B	��B	��B	��B	��B	��B	��B	��B	�cB
  B	�}B	��B	�cB	��B	�BB	�(B	�B	�(B	��B
B
�B
�B
AB
�B
aB
B
�B
�B
�B
�B
uB
�B
�B
�B
B
�B
�B
uB
�B
-B
�B
�B
�B
�B
gB
�B
�B
�B
�B
�B
EB
�B
KB
B
B
KB
KB
�B
	B
�B
�B
	�B

�B
^B
�B
�B
�B
B
B
pB
bB
�B
�B
bB
B
�B
�B
BB
BB
pB
VB
(B
�B
�B
B
�B
�B
$B
�B
yB
�B
_B
�B
7B
�B
WB
�B
�B
IB
IB
�B
B
jB
jB
!B
�B
 �B
 �B
!|B
"4B
"4B
"hB
"�B
# B
#B
#:B
#�B
$B
$tB
$�B
%,B
%�B
%�B
%�B
%�B
&B
&�B
'B
'mB
(
B
'�B
(
B
(
B
(
B
($B
'�B
'�B
'�B
(�B
)_B
)_B
)�B
)�B
)yB
)_B
)�B
*B
)�B
)DB
)yB
)�B
)�B
)�B
*KB
*B
*�B
*�B
*�B
*�B
*�B
+6B
+QB
+�B
+�B
,WB
,�B
-CB
.�B
.�B
/ B
/5B
/iB
/�B
0!B
0UB
0oB
0�B
0�B
0�B
0�B
0�B
1AB
1�B
1[B
1�B
1�B
1�B
2�B
2�B
2�B
3hB
3MB
3B
2aB
2-B
2|B
2�B
3B
33B
4B
5�B
5%B
5%B
4�B
5?B
5tB
6B
5�B
4�B
5tB
5�B
5�B
5�B
5tB
5�B
5�B
5�B
6FB
6zB
6zB
6�B
7B
72B
7�B
7�B
8B
8lB
8lB
8lB
9$B
9�B
9�B
:�B
;0B
;JB
<6B
<�B
=�B
=�B
>B
>B
>�B
>�B
>�B
>�B
?}B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
AoB
A�B
A�B
A�B
A�B
B[B
B�B
B�B
CGB
CGB
C�B
EB
E9B
E9B
E9B
ESB
EB
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F%B
F�B
G+B
G�B
GzB
GzB
G�B
G�B
G�B
HB
HfB
H�B
H�B
IlB
IlB
I�B
I�B
I�B
J#B
JrB
J�B
J�B
J�B
J�B
KB
K)B
K�B
K�B
LB
K�B
K�B
K�B
LB
LdB
L~B
L�B
M�B
N"B
N�B
N�B
N�B
O\B
P.B
PHB
P�B
P�B
P�B
P�B
QB
QhB
RB
RTB
R�B
S[B
S@B
SuB
S�B
S�B
S�B
S�B
TB
T,B
TFB
TaB
T{B
TFB
U2B
UgB
UgB
U�B
U�B
V9B
V9B
VB
VmB
V�B
WYB
W�B
W�B
W�B
XB
XEB
X_B
XyB
XyB
X�B
Y1B
YKB
YeB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Y�B
ZB
ZQB
Z7B
ZQB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[#B
[=B
[�B
[�B
\)B
\)B
\CB
\CB
\�B
\�B
]B
]�B
^B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_VB
_�B
_�B
_�B
`B
`vB
`�B
abB
a|B
a|B
a�B
a�B
b4B
b4B
bhB
cB
c B
c:B
cnB
c�B
c�B
d&B
d�B
d�B
d�B
d�B
d�B
d�B
e,B
eFB
e`B
ezB
e�B
fLB
ffB
fLB
ffB
f�B
f�B
f�B
gRB
g�B
g�B
h$B
h$B
h�B
h�B
h�B
h�B
h�B
iDB
iyB
i�B
i�B
i�B
i�B
jB
jB
jKB
jB
jB
jB
jB
j�B
j�B
kB
kB
k6B
kQB
k�B
k�B
k�B
l=B
lWB
lqB
lqB
l�B
mCB
m]B
mwB
mwB
m�B
m�B
m�B
nB
nIB
n}B
n}B
n}B
n�B
n�B
n�B
n�B
o B
o B
n�B
oB
oB
oB
oiB
o�B
o�B
o�B
o�B
p!B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qAB
q�B
r-B
rGB
rGB
rGB
r�B
r�B
sB
sMB
s�B
s�B
tB
t9B
t�B
t�B
t�B
uB
uB
t�B
u?B
utB
u�B
u�B
v`B
v`B
vzB
v�B
v�B
v�B
wB
wB
wLB
w�B
w�B
w�B
w�B
w�B
xB
xB
xRB
x8B
xlB
x�B
x�B
x�B
x�B
y>B
yrB
y�B
y�B
y�B
y�B
z*B
zxB
z�B
z�B
z�B
z�B
{dB
{�B
{B
{B
{�B
{�B
{�B
|B
|6B
|PB
|jB
|�B
|�B
}B
}B
}VB
}�B
}�B
~B
~wB
~�B
~�B
B
.B
�B
�B
�OB
��B
�iB
��B
��B
��B
��B
�;B
�oB
��B
�'B
�'B
�AB
�[B
�[B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104931  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174313  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174313  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174313                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024320  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024320  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                