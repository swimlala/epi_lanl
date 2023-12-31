CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:29:15Z creation;2022-06-04T19:29:15Z conversion to V3.1      
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192915  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               hA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @٪���1   @٪�x	+@+�7Kƨ�d%����1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  BbffBf  Bp  Bx  B���B�ffB�  B�  B�  B�33B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C��C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ D�|�D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dփ3D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@fg@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��Bb33Be��Bo��Bw��B�� B�L�B��fB��fB��fB��B��3B��3B��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��B׳3B۳3B��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�C� C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CD�CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�{3DƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fDց�D־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD遙D�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�3D��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AѾwAѽ<Aѽ<AѾ�Aѽ�AѾwA�ÖA���A�ʌA��}A���A���A�ҽA��sA�خA�ٴA���A�ںA���A���A��A���A��/A��A���A��KA��
A�0!A��gA�4A���A�MAƑ�A��lA�u%A���A��A���A��XA��(A���A��BA�;0A��A�X�A�ݘA���A��!A��@A�	�A���A��WA��A��oA�1�A�?A�E9A�e`A��QA�=qA���A���A�xA�!�A�~�A��?A��A��A�$�A�רA���Aw��Aq�Aoe,Al��Ak�AfیAa}�AZ��AW\)AT:�AQ��AOAK�3AJU2AHL�AD0UAAN�A?�+A=ߤA=A<�A<c A;A A8]dA6
�A5/�A4#:A2h�A0��A/��A.�ZA-W?A,:�A+c�A+o A*�TA)�nA'4A%/A$�+A#҉A! \AS�AqA��A�oA"�A#�A(A��A�,A2aAr�A�A�A�A��AoiA�A��A{JAA A��A��A҉A=qA�A`BA�`A��AT�A��A	A��AxA/�A��AOA_pA�WA�Aa�AqAX�A�VA6zA
�eA	�&A	��A	:�AP�A�DA��A��A=A�kA(�AL0A��A,=A[�A�kA'�A�A �)A 1'@��M@� \@��D@��@��@���@���@���@���@�@@�&�@�H�@�;d@�j�@�qv@��@�k�@��P@�m�@��K@�@��z@�Z�@�*�@�	@��@��o@�g8@�Ft@�1�@�:*@�e@���@�C@�4@�PH@��X@���@��@�)_@�@���@�33@�
�@�[@��@�[�@�+k@�)�@�4@��@�c @��j@�n@��.@��>@�c@�+@㧇@��f@�L@�o@�=q@��@�s�@�=q@���@���@�+k@��@�!�@��r@���@��@��@ِ�@م�@َ"@��f@�J@���@�@�p�@�ߤ@��@ռ�@�ƨ@��@���@�S�@��3@�w�@��W@я�@��@К@ϟV@���@�^5@�~@ͩ*@� \@̰!@��@�� @�l�@�@�u�@�1@ɫ�@ɖS@�w2@�IR@�@O@���@���@Ȟ@�~@��@�iD@�&�@�%@��@��m@ƚ�@ƆY@��Q@ł�@�k�@�S@Ğ�@�M@�8@�@��@�C-@��@��F@���@���@�!-@�|�@�9X@��r@�˒@��M@�!�@���@�خ@�X�@��I@��o@�"�@�҉@��Y@�V@�Ft@��@��@@�S@�@��@���@�>�@��O@�U2@��@�{J@��@��@�e@��V@�W?@�=@��@��@���@�*�@�c@�=@��@��f@���@�Z@��[@�Vm@�ں@�Q@��@��@���@��4@��@��4@�5?@���@�[W@�q@��@��2@��,@���@�Q�@�}�@��@�y>@�h
@�H�@�_@�/�@���@���@�~(@�.�@���@�0�@�@�҉@�kQ@�M@��Z@��@���@�v`@�J�@�;@��`@�e�@��K@�}�@�@��@��N@��@��@�3�@���@��@�	l@���@��o@��@��D@���@�~�@���@�j@�1�@��@���@�s�@�A @�ȴ@�a|@��@�[W@��y@���@�Xy@��@��-@�o @�J#@��@��h@�:�@���@�e,@�/@��@��/@�Ta@�1�@���@�iD@��@���@�PH@�~@��m@��q@�U�@��@��@�4n@���@���@��@@��~@�RT@��2@���@�V@�u@��@���@�\)@�'�@��?@�r�@��Z@��-@���@���@�{J@�|@�s�@�N<@��@���@�~(@�Z�@�N�@�>B@��@��M@�Y�@�J�@�.I@��H@���@��1@��r@�{�@�u%@�u%@�ff@� �@��H@���@�S�@� \@��@��[@�r�@��@���@�\)@�U�@�N<@�J�@�;d@�+@��@���@���@��[@��*@�F@��@��@��	@���@���@�_�@�'R@�Q@��@�P@~��@~.�@}�@}��@}F@}+@|g8@{��@{j�@{S@zq�@yϫ@yY�@y2a@x�@x��@xQ�@w��@w��@w]�@w�@v�1@ve@u��@t��@s�@s>�@r�s@r_�@qzx@q@@p~(@pM@oy�@o
=@nJ�@ne@n@m�@m��@mQ�@m2a@m@@l�`@l��@lq@lFt@l1@k��@k�@k@O@j��@j�m@j�L@jM�@j�@i�@i�@if�@h�9@hu�@h2�@g�]@gخ@g�4@f�@fH�@e��@ef�@eA @d�@d��@d:�@cO@c)_@b��@bL0@a��@ax�@a�@`q@`2�@_�@_�@_\)@^��@^��@^YK@]�@\�f@\�I@\,=@\b@[�@[�W@[_p@Zz@Z1�@Z
�@Y�@Y��@YG�@X�[@X�@XFt@X�@W�6@W��@W�q@W�{@WE9@V҉@V	@U��@U��@Uk�@UDg@U�@T��@T|�@S�&@S��@S�@Sa@S)_@R�c@R�@R.�@Q�@Q��@Qc@QA @P�@P��@P/�@O�@O�q@OU�@O&@N��@N�@N
�@M��@M/@L��@L�z@L1@Kg�@J��@Ji�@J8�@I�j@I^�@H�K@H�U@Hl"@Hc�@HQ�@H �@G��@G{J@GF�@F��@F$�@E��@E;@Dw�@D/�@D�@D�@C�f@B�2@B�+@BOv@A��@AN<@@�5@@�e@@c�@@>B@@ �@?�]@?��@?F�@>�c@>�@>s�@>$�@=�@=��@=|@=c�@<�@<Q�@;ݘ@;_p@:�M@:�L@:C�@9�"@9	l@8g8@8~@8~@87@7�@6�H@6=q@6 �@5�@5�3@5�"@5|@5N<@4u�@3�
@3�@3Z�@3A�@3�@2��@2�\@2�@1j@10�@0��@04n@/�]@/�@/ƨ@/��@/8@.͟@.q�@.H�@.{@-�)@-��@-��@-^�@-@@,��@,q@+�]@+�6@+�P@+b�@+4�@*�@*�\@*H�@*8�@*u@)�C@)|@)5�@)@(��@(j@(M@(9X@("h@'�&@'�:@''�@&�y@&��@&B[@&@�@% \@$�[@$$@#�6@#��@#�$@#��@#��@#,�@"�X@"+k@!��@!��@!�@!rG@!��@!�"@!Y�@!�@ ��@ �@ *�@�0@��@t�@{J@iD@J#@�@�@��@O@_@�M@+�@��@�O@e�@M@��@�0@{J@;d@��@�'@�6@�@��@W�@
�@��@�)@�@�T@�N@�"@c�@/@��@�U@��@��@��@�@M@�@��@�}@˒@��@�@K�@�@҉@�m@�@��@�A@xl@ff@)�@�.@��@�=@f�@<6@	l@�K@֡@��@bN@:�@�
@;d@33@8@1�@+@+@�@ȴ@@��@�#@�@c@G�@�@�5@��@��@�@��@�_@e�@Q�@S�@S�@S�@!@�@�@�0@��@33@�@ȴ@��@��@\�@6�@+k@&�@ �@��@�@�@��@hs@7L@�v@��@�.@[�@I�@%�@�@��@�&@�;@خ@��@l�@;d@
��@
��@
��@
{�@
c @
B[@	�@	�@	��@	k�@	S&@	 \@�P@�@�)@��@��@r�@[�@6@	�@��@�@�P@P�@�@@�@��@}V@kQ@L0@��@�@��@��@}�@Vm@=�@!�@�P@�@��@]d@C-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AѾwAѽ<Aѽ<AѾ�Aѽ�AѾwA�ÖA���A�ʌA��}A���A���A�ҽA��sA�خA�ٴA���A�ںA���A���A��A���A��/A��A���A��KA��
A�0!A��gA�4A���A�MAƑ�A��lA�u%A���A��A���A��XA��(A���A��BA�;0A��A�X�A�ݘA���A��!A��@A�	�A���A��WA��A��oA�1�A�?A�E9A�e`A��QA�=qA���A���A�xA�!�A�~�A��?A��A��A�$�A�רA���Aw��Aq�Aoe,Al��Ak�AfیAa}�AZ��AW\)AT:�AQ��AOAK�3AJU2AHL�AD0UAAN�A?�+A=ߤA=A<�A<c A;A A8]dA6
�A5/�A4#:A2h�A0��A/��A.�ZA-W?A,:�A+c�A+o A*�TA)�nA'4A%/A$�+A#҉A! \AS�AqA��A�oA"�A#�A(A��A�,A2aAr�A�A�A�A��AoiA�A��A{JAA A��A��A҉A=qA�A`BA�`A��AT�A��A	A��AxA/�A��AOA_pA�WA�Aa�AqAX�A�VA6zA
�eA	�&A	��A	:�AP�A�DA��A��A=A�kA(�AL0A��A,=A[�A�kA'�A�A �)A 1'@��M@� \@��D@��@��@���@���@���@���@�@@�&�@�H�@�;d@�j�@�qv@��@�k�@��P@�m�@��K@�@��z@�Z�@�*�@�	@��@��o@�g8@�Ft@�1�@�:*@�e@���@�C@�4@�PH@��X@���@��@�)_@�@���@�33@�
�@�[@��@�[�@�+k@�)�@�4@��@�c @��j@�n@��.@��>@�c@�+@㧇@��f@�L@�o@�=q@��@�s�@�=q@���@���@�+k@��@�!�@��r@���@��@��@ِ�@م�@َ"@��f@�J@���@�@�p�@�ߤ@��@ռ�@�ƨ@��@���@�S�@��3@�w�@��W@я�@��@К@ϟV@���@�^5@�~@ͩ*@� \@̰!@��@�� @�l�@�@�u�@�1@ɫ�@ɖS@�w2@�IR@�@O@���@���@Ȟ@�~@��@�iD@�&�@�%@��@��m@ƚ�@ƆY@��Q@ł�@�k�@�S@Ğ�@�M@�8@�@��@�C-@��@��F@���@���@�!-@�|�@�9X@��r@�˒@��M@�!�@���@�خ@�X�@��I@��o@�"�@�҉@��Y@�V@�Ft@��@��@@�S@�@��@���@�>�@��O@�U2@��@�{J@��@��@�e@��V@�W?@�=@��@��@���@�*�@�c@�=@��@��f@���@�Z@��[@�Vm@�ں@�Q@��@��@���@��4@��@��4@�5?@���@�[W@�q@��@��2@��,@���@�Q�@�}�@��@�y>@�h
@�H�@�_@�/�@���@���@�~(@�.�@���@�0�@�@�҉@�kQ@�M@��Z@��@���@�v`@�J�@�;@��`@�e�@��K@�}�@�@��@��N@��@��@�3�@���@��@�	l@���@��o@��@��D@���@�~�@���@�j@�1�@��@���@�s�@�A @�ȴ@�a|@��@�[W@��y@���@�Xy@��@��-@�o @�J#@��@��h@�:�@���@�e,@�/@��@��/@�Ta@�1�@���@�iD@��@���@�PH@�~@��m@��q@�U�@��@��@�4n@���@���@��@@��~@�RT@��2@���@�V@�u@��@���@�\)@�'�@��?@�r�@��Z@��-@���@���@�{J@�|@�s�@�N<@��@���@�~(@�Z�@�N�@�>B@��@��M@�Y�@�J�@�.I@��H@���@��1@��r@�{�@�u%@�u%@�ff@� �@��H@���@�S�@� \@��@��[@�r�@��@���@�\)@�U�@�N<@�J�@�;d@�+@��@���@���@��[@��*@�F@��@��@��	@���@���@�_�@�'R@�Q@��@�P@~��@~.�@}�@}��@}F@}+@|g8@{��@{j�@{S@zq�@yϫ@yY�@y2a@x�@x��@xQ�@w��@w��@w]�@w�@v�1@ve@u��@t��@s�@s>�@r�s@r_�@qzx@q@@p~(@pM@oy�@o
=@nJ�@ne@n@m�@m��@mQ�@m2a@m@@l�`@l��@lq@lFt@l1@k��@k�@k@O@j��@j�m@j�L@jM�@j�@i�@i�@if�@h�9@hu�@h2�@g�]@gخ@g�4@f�@fH�@e��@ef�@eA @d�@d��@d:�@cO@c)_@b��@bL0@a��@ax�@a�@`q@`2�@_�@_�@_\)@^��@^��@^YK@]�@\�f@\�I@\,=@\b@[�@[�W@[_p@Zz@Z1�@Z
�@Y�@Y��@YG�@X�[@X�@XFt@X�@W�6@W��@W�q@W�{@WE9@V҉@V	@U��@U��@Uk�@UDg@U�@T��@T|�@S�&@S��@S�@Sa@S)_@R�c@R�@R.�@Q�@Q��@Qc@QA @P�@P��@P/�@O�@O�q@OU�@O&@N��@N�@N
�@M��@M/@L��@L�z@L1@Kg�@J��@Ji�@J8�@I�j@I^�@H�K@H�U@Hl"@Hc�@HQ�@H �@G��@G{J@GF�@F��@F$�@E��@E;@Dw�@D/�@D�@D�@C�f@B�2@B�+@BOv@A��@AN<@@�5@@�e@@c�@@>B@@ �@?�]@?��@?F�@>�c@>�@>s�@>$�@=�@=��@=|@=c�@<�@<Q�@;ݘ@;_p@:�M@:�L@:C�@9�"@9	l@8g8@8~@8~@87@7�@6�H@6=q@6 �@5�@5�3@5�"@5|@5N<@4u�@3�
@3�@3Z�@3A�@3�@2��@2�\@2�@1j@10�@0��@04n@/�]@/�@/ƨ@/��@/8@.͟@.q�@.H�@.{@-�)@-��@-��@-^�@-@@,��@,q@+�]@+�6@+�P@+b�@+4�@*�@*�\@*H�@*8�@*u@)�C@)|@)5�@)@(��@(j@(M@(9X@("h@'�&@'�:@''�@&�y@&��@&B[@&@�@% \@$�[@$$@#�6@#��@#�$@#��@#��@#,�@"�X@"+k@!��@!��@!�@!rG@!��@!�"@!Y�@!�@ ��@ �@ *�@�0@��@t�@{J@iD@J#@�@�@��@O@_@�M@+�@��@�O@e�@M@��@�0@{J@;d@��@�'@�6@�@��@W�@
�@��@�)@�@�T@�N@�"@c�@/@��@�U@��@��@��@�@M@�@��@�}@˒@��@�@K�@�@҉@�m@�@��@�A@xl@ff@)�@�.@��@�=@f�@<6@	l@�K@֡@��@bN@:�@�
@;d@33@8@1�@+@+@�@ȴ@@��@�#@�@c@G�@�@�5@��@��@�@��@�_@e�@Q�@S�@S�@S�@!@�@�@�0@��@33@�@ȴ@��@��@\�@6�@+k@&�@ �@��@�@�@��@hs@7L@�v@��@�.@[�@I�@%�@�@��@�&@�;@خ@��@l�@;d@
��@
��@
��@
{�@
c @
B[@	�@	�@	��@	k�@	S&@	 \@�P@�@�)@��@��@r�@[�@6@	�@��@�@�P@P�@�@@�@��@}V@kQ@L0@��@�@��@��@}�@Vm@=�@!�@�P@�@��@]d@C-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	KxB	J�B	JXB	JXB	J=B	J	B	J�B	OBB	N<B	P.B	R�B	L�B	P�B	X�B	[=B	\xB	^B	^�B	^�B	`�B	h�B	kQB	m)B	m�B	n�B	pB	qAB	��B	�%B
�mBo BW�B\BIlBHB<PBF�BqvB�nB��B��B�;B� B��B�B�B�|BۦB�MB�zB��B��B|�Bh�B?.B �BgB�B	7B�B
��B
�B
�uB
��B
�fB
�$B
�`B
�_B
SuB
7�B
B	��B	ǮB	��B	�B	�RB	�B	x�B	W?B	F?B	9�B	-�B	# B	7B	�B	0B��B�B�B�B�B�qB�QB�fB�\B�B׍B��B�*B�0B�aB��B		RB	�B	6B	�B	CB	B	�B	�B	�B	
XB��B��B	 �B	�B	"B	0;B	4�B	8B	A B	H�B	I�B	I7B	VSB	g�B	y	B	.B	��B	��B	�!B	��B	�hB	��B	��B	�B	�tB	��B	�&B	��B	��B	��B	��B	�?B	��B	�tB	��B	�zB	�`B	��B	�$B	�qB	�]B	�fB	��B	��B	��B	��B	�JB	��B	�B	�}B	�aB	��B	��B	уB	҉B	ԕB	�oB	��B	�B	ɠB	ɺB	��B	̘B	�DB	��B	ˬB	ˬB	˒B	�~B	�+B	��B	�B	�B	�B	��B	�B	�B	�B	�BB	�bB	��B	�B	�'B	�VB	�hB	��B	�0B	�WB	�lB	�B	��B	�DB
 �B
-B
3B
�B
B
mB
	RB
_B
�B
 B	��B	��B	�(B	�VB	�B	��B	�DB	��B
UB
�B
�B
�B
�B
�B
_B
�B
�B
�B
�B
�B
�B

�B

�B
DB
^B
xB

XB
B
 OB	�B	�"B	��B	�"B	�dB	�dB	��B	�qB	��B	��B	�HB	��B
 OB
 OB
B
 B
�B
�B
�B
�B
�B
+B
YB
�B
B
gB
�B
aB
B
�B
�B
B
 �B
 �B
 �B
 �B	��B	�B	�HB	�}B	�HB	��B	��B
 OB
 4B	��B	��B
;B
�B
�B
uB
AB
[B
�B
uB
�B
�B
�B
B
GB
�B
3B
�B
�B
�B
B
SB
�B
�B
�B
�B
B
�B
�B
�B
�B
YB
tB
tB
�B
�B
B
+B
�B
�B
B
�B
+B
fB
KB
�B
	RB
	lB
	lB
�B
)B

�B

	B
	B
�B
�B
�B
�B
B
B
KB
1B
�B
�B
�B
+B
1B
�B
	RB
	�B
	�B
	�B
	RB

=B

XB
^B
�B
�B
B
JB
�B
0B
�B
B
jB
�B
�B
�B
�B
�B
VB
"B
B
�B
vB
vB
\B
\B
 B
�B
�B
�B
 B
NB
�B
�B
�B
B
TB
�B
&B
[B
�B
,B
,B
B
gB
�B
MB
B
SB
B
B
�B
�B
B
�B
�B
sB
�B
�B
�B
B
B
�B
B
B
eB
�B
�B
�B
�B
�B
QB
	B
qB
)B
xB
�B
�B
/B
B
/B
�B
�B
�B
�B
�B
�B
�B
�B
pB
�B
 vB
 �B
!�B
!�B
!�B
!�B
"4B
"hB
"�B
#�B
#�B
#�B
$ZB
$@B
$ZB
$�B
%B
%FB
%�B
&�B
&fB
&�B
'8B
'RB
'RB
'�B
(sB
(�B
(�B
)B
(�B
(�B
(�B
)_B
)�B
)DB
)�B
*�B
+�B
+�B
,"B
,�B
,�B
-B
-�B
.�B
/ B
/OB
/OB
/OB
/OB
/5B
/5B
/iB
/�B
/�B
0UB
0UB
0oB
0�B
0�B
1'B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
3�B
3hB
2�B
3�B
5�B
5�B
5�B
5�B
6`B
6`B
6`B
7B
7�B
8B
7�B
8�B
9>B
9$B
9rB
9XB
8�B
8lB
8�B
:B
:�B
:�B
;B
;dB
;dB
;�B
;�B
;�B
;�B
<B
<B
<PB
<�B
<�B
=<B
=qB
>�B
>�B
>�B
>�B
?�B
?�B
@B
@4B
@�B
@�B
AoB
A�B
A�B
A�B
A�B
A�B
A�B
B'B
B'B
B[B
B�B
B�B
B�B
B�B
CB
CGB
C{B
C�B
C�B
C�B
DMB
DgB
D�B
E9B
E�B
FB
F?B
FYB
F?B
FtB
G_B
GzB
G�B
HKB
HKB
H�B
HfB
H�B
I�B
IRB
IlB
I�B
I�B
J	B
J#B
I�B
JXB
JXB
JrB
J�B
J�B
J�B
J�B
K^B
KDB
KxB
K�B
K�B
K�B
K�B
LdB
L�B
L�B
L�B
L�B
L�B
MjB
M�B
M�B
NB
N"B
N<B
N<B
N"B
N<B
N�B
N�B
O�B
O�B
O�B
O�B
PB
P.B
P}B
P�B
QhB
QhB
QhB
QhB
Q�B
Q�B
Q�B
R:B
R�B
R�B
R�B
R�B
R�B
SB
S@B
SuB
SuB
S�B
S�B
S�B
TB
T{B
TaB
T�B
UB
UB
UgB
U�B
V9B
VmB
VmB
V�B
V�B
W?B
W?B
W�B
WsB
WsB
W�B
W�B
W�B
W�B
X_B
XEB
XEB
X_B
X�B
YKB
YB
Y�B
YB
ZB
Y�B
Y�B
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[WB
[qB
[qB
[�B
[�B
[WB
[WB
[WB
[�B
[�B
[�B
[�B
[�B
\)B
\�B
\�B
]�B
]~B
]�B
]IB
]/B
^OB
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_�B
_�B
`'B
`BB
`BB
`vB
`�B
`vB
`�B
a-B
aB
a|B
a�B
a�B
b4B
b�B
bhB
b�B
cB
cnB
cnB
c�B
c�B
c�B
c�B
c�B
d@B
d@B
d�B
e,B
eFB
e`B
e�B
ezB
e�B
f2B
fLB
f2B
ffB
f�B
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
h
B
h$B
h�B
h�B
i�B
j0B
kB
j�B
kQB
l�B
l�B
l�B
l�B
mCB
m�B
m�B
m]B
ncB
n�B
o B
oiB
o B
p!B
o�B
o�B
o�B
o�B
o�B
p�B
q'B
q�B
q�B
q�B
r-B
r�B
r�B
raB
r�B
r�B
r|B
r�B
r�B
sB
s�B
t9B
tTB
uB
u%B
u?B
u?B
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v`B
vzB
vFB
v`B
v�B
v�B
v�B
v�B
v�B
wB
wB
wLB
wfB
wfB
wLB
w�B
w�B
xB
x8B
x8B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y$B
yXB
yXB
yXB
yXB
y�B
y�B
yrB
y�B
z�B
z�B
z�B
{B
{0B
{JB
{�B
|B
|B
|B
|B
|PB
|PB
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}VB
}VB
}VB
}VB
}�B
}�B
}�B
~(B
~wB
~�B
~�B
~�B
~�B
~�B
B
B
HB
�B
�B
� B
�B
�B
�4B
�iB
��B
��B
��B
��B
��B
��B
�;B
�UB
��B
��B
�B
�'B
�AB
�[B
�[B
��B
��B
�B
�aB
�aB
��B
��B
��B
��B
�3B
�MB
�MB
�gB
��B
��B
��B
��B
�B
�9B
��B
��B
��B
��B
�%B
�YB
�tB
��B
��B
��B
�B
�+B
�_B
�zB
��B
��B
�B
�1B
�fB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	KxB	J�B	JXB	JXB	J=B	J	B	J�B	OBB	N<B	P.B	R�B	L�B	P�B	X�B	[=B	\xB	^B	^�B	^�B	`�B	h�B	kQB	m)B	m�B	n�B	pB	qAB	��B	�%B
�mBo BW�B\BIlBHB<PBF�BqvB�nB��B��B�;B� B��B�B�B�|BۦB�MB�zB��B��B|�Bh�B?.B �BgB�B	7B�B
��B
�B
�uB
��B
�fB
�$B
�`B
�_B
SuB
7�B
B	��B	ǮB	��B	�B	�RB	�B	x�B	W?B	F?B	9�B	-�B	# B	7B	�B	0B��B�B�B�B�B�qB�QB�fB�\B�B׍B��B�*B�0B�aB��B		RB	�B	6B	�B	CB	B	�B	�B	�B	
XB��B��B	 �B	�B	"B	0;B	4�B	8B	A B	H�B	I�B	I7B	VSB	g�B	y	B	.B	��B	��B	�!B	��B	�hB	��B	��B	�B	�tB	��B	�&B	��B	��B	��B	��B	�?B	��B	�tB	��B	�zB	�`B	��B	�$B	�qB	�]B	�fB	��B	��B	��B	��B	�JB	��B	�B	�}B	�aB	��B	��B	уB	҉B	ԕB	�oB	��B	�B	ɠB	ɺB	��B	̘B	�DB	��B	ˬB	ˬB	˒B	�~B	�+B	��B	�B	�B	�B	��B	�B	�B	�B	�BB	�bB	��B	�B	�'B	�VB	�hB	��B	�0B	�WB	�lB	�B	��B	�DB
 �B
-B
3B
�B
B
mB
	RB
_B
�B
 B	��B	��B	�(B	�VB	�B	��B	�DB	��B
UB
�B
�B
�B
�B
�B
_B
�B
�B
�B
�B
�B
�B

�B

�B
DB
^B
xB

XB
B
 OB	�B	�"B	��B	�"B	�dB	�dB	��B	�qB	��B	��B	�HB	��B
 OB
 OB
B
 B
�B
�B
�B
�B
�B
+B
YB
�B
B
gB
�B
aB
B
�B
�B
B
 �B
 �B
 �B
 �B	��B	�B	�HB	�}B	�HB	��B	��B
 OB
 4B	��B	��B
;B
�B
�B
uB
AB
[B
�B
uB
�B
�B
�B
B
GB
�B
3B
�B
�B
�B
B
SB
�B
�B
�B
�B
B
�B
�B
�B
�B
YB
tB
tB
�B
�B
B
+B
�B
�B
B
�B
+B
fB
KB
�B
	RB
	lB
	lB
�B
)B

�B

	B
	B
�B
�B
�B
�B
B
B
KB
1B
�B
�B
�B
+B
1B
�B
	RB
	�B
	�B
	�B
	RB

=B

XB
^B
�B
�B
B
JB
�B
0B
�B
B
jB
�B
�B
�B
�B
�B
VB
"B
B
�B
vB
vB
\B
\B
 B
�B
�B
�B
 B
NB
�B
�B
�B
B
TB
�B
&B
[B
�B
,B
,B
B
gB
�B
MB
B
SB
B
B
�B
�B
B
�B
�B
sB
�B
�B
�B
B
B
�B
B
B
eB
�B
�B
�B
�B
�B
QB
	B
qB
)B
xB
�B
�B
/B
B
/B
�B
�B
�B
�B
�B
�B
�B
�B
pB
�B
 vB
 �B
!�B
!�B
!�B
!�B
"4B
"hB
"�B
#�B
#�B
#�B
$ZB
$@B
$ZB
$�B
%B
%FB
%�B
&�B
&fB
&�B
'8B
'RB
'RB
'�B
(sB
(�B
(�B
)B
(�B
(�B
(�B
)_B
)�B
)DB
)�B
*�B
+�B
+�B
,"B
,�B
,�B
-B
-�B
.�B
/ B
/OB
/OB
/OB
/OB
/5B
/5B
/iB
/�B
/�B
0UB
0UB
0oB
0�B
0�B
1'B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
3�B
3hB
2�B
3�B
5�B
5�B
5�B
5�B
6`B
6`B
6`B
7B
7�B
8B
7�B
8�B
9>B
9$B
9rB
9XB
8�B
8lB
8�B
:B
:�B
:�B
;B
;dB
;dB
;�B
;�B
;�B
;�B
<B
<B
<PB
<�B
<�B
=<B
=qB
>�B
>�B
>�B
>�B
?�B
?�B
@B
@4B
@�B
@�B
AoB
A�B
A�B
A�B
A�B
A�B
A�B
B'B
B'B
B[B
B�B
B�B
B�B
B�B
CB
CGB
C{B
C�B
C�B
C�B
DMB
DgB
D�B
E9B
E�B
FB
F?B
FYB
F?B
FtB
G_B
GzB
G�B
HKB
HKB
H�B
HfB
H�B
I�B
IRB
IlB
I�B
I�B
J	B
J#B
I�B
JXB
JXB
JrB
J�B
J�B
J�B
J�B
K^B
KDB
KxB
K�B
K�B
K�B
K�B
LdB
L�B
L�B
L�B
L�B
L�B
MjB
M�B
M�B
NB
N"B
N<B
N<B
N"B
N<B
N�B
N�B
O�B
O�B
O�B
O�B
PB
P.B
P}B
P�B
QhB
QhB
QhB
QhB
Q�B
Q�B
Q�B
R:B
R�B
R�B
R�B
R�B
R�B
SB
S@B
SuB
SuB
S�B
S�B
S�B
TB
T{B
TaB
T�B
UB
UB
UgB
U�B
V9B
VmB
VmB
V�B
V�B
W?B
W?B
W�B
WsB
WsB
W�B
W�B
W�B
W�B
X_B
XEB
XEB
X_B
X�B
YKB
YB
Y�B
YB
ZB
Y�B
Y�B
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[WB
[qB
[qB
[�B
[�B
[WB
[WB
[WB
[�B
[�B
[�B
[�B
[�B
\)B
\�B
\�B
]�B
]~B
]�B
]IB
]/B
^OB
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_�B
_�B
`'B
`BB
`BB
`vB
`�B
`vB
`�B
a-B
aB
a|B
a�B
a�B
b4B
b�B
bhB
b�B
cB
cnB
cnB
c�B
c�B
c�B
c�B
c�B
d@B
d@B
d�B
e,B
eFB
e`B
e�B
ezB
e�B
f2B
fLB
f2B
ffB
f�B
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
h
B
h$B
h�B
h�B
i�B
j0B
kB
j�B
kQB
l�B
l�B
l�B
l�B
mCB
m�B
m�B
m]B
ncB
n�B
o B
oiB
o B
p!B
o�B
o�B
o�B
o�B
o�B
p�B
q'B
q�B
q�B
q�B
r-B
r�B
r�B
raB
r�B
r�B
r|B
r�B
r�B
sB
s�B
t9B
tTB
uB
u%B
u?B
u?B
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v`B
vzB
vFB
v`B
v�B
v�B
v�B
v�B
v�B
wB
wB
wLB
wfB
wfB
wLB
w�B
w�B
xB
x8B
x8B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y$B
yXB
yXB
yXB
yXB
y�B
y�B
yrB
y�B
z�B
z�B
z�B
{B
{0B
{JB
{�B
|B
|B
|B
|B
|PB
|PB
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}VB
}VB
}VB
}VB
}�B
}�B
}�B
~(B
~wB
~�B
~�B
~�B
~�B
~�B
B
B
HB
�B
�B
� B
�B
�B
�4B
�iB
��B
��B
��B
��B
��B
��B
�;B
�UB
��B
��B
�B
�'B
�AB
�[B
�[B
��B
��B
�B
�aB
�aB
��B
��B
��B
��B
�3B
�MB
�MB
�gB
��B
��B
��B
��B
�B
�9B
��B
��B
��B
��B
�%B
�YB
�tB
��B
��B
��B
�B
�+B
�_B
�zB
��B
��B
�B
�1B
�fB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105249  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192915  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192915  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192915                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042923  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042923  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                