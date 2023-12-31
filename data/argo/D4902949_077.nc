CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  [   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-11-06T10:00:38Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  F�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  J8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  W�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  [    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  hl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  u�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  y4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    Ɣ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ɔ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ̔   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ϔ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@        �@Argo profile    3.1 1.2 19500101000000  20191106100038  20200924132255  4902949 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               MA   AO  6976                            2C  D   NAVIS_A                         0823                            170210                          863 @��׵�U31   @����s��@3��"��`�c��
=p�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      MA   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D�|�D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9ٙC;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Ch�Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D 3D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�;3D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�{3D̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aݛ�Aݛ�A���A�t�A�E�A�9XA��A�VA��A��
A���A���A�ƨAۣ�A�/AڮAڏ\Aڝ�Aڡ�A�l�A�G�A�&�A�JA��A��
A٩�Aٕ�A�|�A�VA�9XA�oA��AؾwA�`BA��;A�?}A�A־wA�33A�E�A�?}A�-A�ȴA�"�A��Aʰ!A�G�A�$�A�ffA��/A©�A�|�A�+A��wA�oA�t�A���A�-A�\)A�M�A�A���A�JA���A��hA�?}A��`A��A��!A�C�A��
A��HA�n�A�/A���A��A�z�A�|�A�5?A�n�A��/A���A���A���A�C�A�%A�7LA�1A�%A���A�A�A���A���A��DA��A�O�A���A�ffAhsA}S�A{�
Aw�PAqVAm"�Akx�Ai�TAf��Af�AdI�Ac\)AcG�Ab��A_;dA\�DAZ�HAY33AV��AQoAO7LAL�jAJffAI�-AIdZAH�AH9XAF{AA�A>�A=��A<�A:�uA9�hA8�9A7��A6�A5�A4��A1��A.=qA-�A,1A+�PA+�A*��A*bNA(Q�A&�!A%��A#S�A"��A"n�A!x�A ��A��A��A�9A"�Az�A�
AA�Al�A�AȴAbNA=qA+A�DA�A;dA$�AbA��A
�9A	�AJAx�A?}A"�A��A�!A��A�DA�AA�A�TA�PA �HA ��A v�@�+@��@�1@���@�ff@�@��-@�-@�@�1@�ȴ@�/@�o@�-@�p�@���@�j@�9X@���@�w@�@�\)@�1@��D@��D@�Q�@�Z@�b@��y@�~�@�+@�n�@�M�@�{@���@�9@�9X@��@�\)@��@���@�v�@�x�@��@�V@���@��`@�j@�D@�A�@�A�@�ƨ@�+@��@���@���@���@߾w@�ff@݉7@�X@݉7@ݑh@ݩ�@��`@�Q�@� �@�9X@�Q�@�j@�z�@�z�@��@�p�@�7L@�7L@�7L@�G�@���@�p�@�v�@�V@��@Ӯ@�t�@�\)@�+@�$�@��m@�~�@ͺ^@���@�;d@�"�@�C�@��@ʟ�@ʏ\@ʗ�@�@��
@��m@�\)@ʰ!@�-@ɡ�@��`@�bN@ȓu@���@�7L@���@Ȭ@ȃ@�bN@��@�+@�v�@���@�bN@°!@�-@���@��`@Ĵ9@ě�@��@Õ�@�@��/@��@���@���@��@�-@���@�V@�A�@���@�p�@��@��w@��P@�33@�ff@��T@�{@�$�@��@�J@���@���@�|�@���@�^5@�ff@���@��D@��@���@��h@���@���@���@��`@��@��@�l�@�\)@�C�@���@��@�J@���@��@���@��-@��-@��h@�p�@�%@��9@���@��
@�l�@�33@�;d@�"�@��R@�~�@�V@�=q@��@��^@�O�@�7L@�V@���@�(�@��@�  @��;@���@�t�@�dZ@�33@�^5@��T@���@�7L@�%@��@���@��j@���@�1@�|�@�33@�+@�@���@���@���@���@���@���@��\@�v�@�M�@�$�@��@���@��7@�?}@�V@���@��u@�r�@�9X@�1'@�1'@�9X@�1'@� �@�b@�b@�b@��w@�C�@��P@�S�@�ȴ@���@�^5@��h@���@��u@�r�@�Z@�1@��;@��@�+@��R@�~�@�^5@�V@�M�@��@��T@��^@��h@��7@��7@�x�@�V@��D@�Z@�9X@�  @���@���@�t�@�S�@�33@�
=@���@��y@�ȴ@��\@�^5@�=q@�J@��#@���@���@�@��@��/@���@�z�@�j@�(�@���@���@�|�@�o@��R@�ff@�=q@��@�@���@��@��@��#@�@��@�?}@��@�(�@���@��@�-@��T@���@�`B@�7L@�V@���@�Ĝ@��u@�(�@��;@�33@���@��\@�n�@�J@�@��h@�G�@��`@��j@���@�Z@��@���@�\)@�"�@��y@��\@�v�@�^5@�=q@�$�@�J@��@��^@���@���@��h@��h@�`B@�V@��/@���@�bN@�I�@�(�@� �@��@�b@�b@�b@�1@�1@�  @+@~�+@~ff@~v�@~v�@~ff@~E�@}`B@|��@|j@|I�@|9X@|(�@|�@|�@{��@{�m@{�F@{��@{�@{t�@{C�@{"�@{"�@z�@z��@z-@z�@zJ@y�#@y��@yx�@yG�@y�@x�`@x��@x�9@x�u@x�@xr�@x �@w��@wl�@w;d@w+@w�@w
=@v��@vȴ@vv�@u�-@u?}@u�@t�@tZ@t1@s�F@s33@r��@r-@q��@q�@q�7@p�`@p�9@p�u@p�u@p�u@p�u@p�u@p�@pb@o��@n�@mO�@mV@mV@l��@l�@l�@l�/@l��@lj@k��@kC�@k"�@j�H@jM�@j�@i��@i�#@i��@i��@ix�@hQ�@g�P@g;d@f�y@f�@f�R@f�R@f��@fff@eV@dz�@d9X@cƨ@c�F@ct�@co@b��@b-@a��@aG�@`�`@`�9@`�@`A�@_��@_K�@^�@^��@^V@]�@]�@\��@\�/@\��@\1@[��@[C�@["�@[o@Z�@Z��@Z�H@[@[o@Z�@Y�@Y�7@Y�7@YG�@X�`@X�9@XbN@X1'@W�;@W�P@W;d@V�y@Vff@VE�@V{@UV@T�@T��@Tz�@TI�@T�@S�m@SdZ@SS�@SC�@R��@R��@RJ@Q��@QX@P�`@P�u@Pr�@PA�@O��@O;d@O�@O
=@O
=@N��@N�y@Nv�@N{@M�@M@M�@MO�@L��@K�m@K��@Kt�@K@J^5@I��@Hr�@HA�@HA�@H1'@Hb@G��@G�@Gl�@Gl�@Gl�@GK�@G;d@G�@F�R@F��@FV@E�@D�@DZ@B�@A�@@Ĝ@?�;@?K�@>�y@>v�@<�j@;�m@;t�@;S�@;o@:�\@9�#@9&�@8Ĝ@8Q�@8b@8 �@8 �@8 �@7�;@8 �@8  @8  @8  @8 �@7+@6��@6ȴ@6��@6v�@6ff@6E�@6v�@6ff@6v�@6E�@65?@65?@6$�@6$�@5�T@5��@5�@5?}@5�@4��@4�/@4��@4�@4j@3�
@2��@2M�@1�7@/�@-��@-p�@-/@-V@,�@,��@,�j@,�D@,1@+��@*��@*=q@)��@)7L@(Ĝ@(��@(A�@(1'@(1'@(A�@(A�@'�w1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aݛ�Aݛ�A���A�t�A�E�A�9XA��A�VA��A��
A���A���A�ƨAۣ�A�/AڮAڏ\Aڝ�Aڡ�A�l�A�G�A�&�A�JA��A��
A٩�Aٕ�A�|�A�VA�9XA�oA��AؾwA�`BA��;A�?}A�A־wA�33A�E�A�?}A�-A�ȴA�"�A��Aʰ!A�G�A�$�A�ffA��/A©�A�|�A�+A��wA�oA�t�A���A�-A�\)A�M�A�A���A�JA���A��hA�?}A��`A��A��!A�C�A��
A��HA�n�A�/A���A��A�z�A�|�A�5?A�n�A��/A���A���A���A�C�A�%A�7LA�1A�%A���A�A�A���A���A��DA��A�O�A���A�ffAhsA}S�A{�
Aw�PAqVAm"�Akx�Ai�TAf��Af�AdI�Ac\)AcG�Ab��A_;dA\�DAZ�HAY33AV��AQoAO7LAL�jAJffAI�-AIdZAH�AH9XAF{AA�A>�A=��A<�A:�uA9�hA8�9A7��A6�A5�A4��A1��A.=qA-�A,1A+�PA+�A*��A*bNA(Q�A&�!A%��A#S�A"��A"n�A!x�A ��A��A��A�9A"�Az�A�
AA�Al�A�AȴAbNA=qA+A�DA�A;dA$�AbA��A
�9A	�AJAx�A?}A"�A��A�!A��A�DA�AA�A�TA�PA �HA ��A v�@�+@��@�1@���@�ff@�@��-@�-@�@�1@�ȴ@�/@�o@�-@�p�@���@�j@�9X@���@�w@�@�\)@�1@��D@��D@�Q�@�Z@�b@��y@�~�@�+@�n�@�M�@�{@���@�9@�9X@��@�\)@��@���@�v�@�x�@��@�V@���@��`@�j@�D@�A�@�A�@�ƨ@�+@��@���@���@���@߾w@�ff@݉7@�X@݉7@ݑh@ݩ�@��`@�Q�@� �@�9X@�Q�@�j@�z�@�z�@��@�p�@�7L@�7L@�7L@�G�@���@�p�@�v�@�V@��@Ӯ@�t�@�\)@�+@�$�@��m@�~�@ͺ^@���@�;d@�"�@�C�@��@ʟ�@ʏ\@ʗ�@�@��
@��m@�\)@ʰ!@�-@ɡ�@��`@�bN@ȓu@���@�7L@���@Ȭ@ȃ@�bN@��@�+@�v�@���@�bN@°!@�-@���@��`@Ĵ9@ě�@��@Õ�@�@��/@��@���@���@��@�-@���@�V@�A�@���@�p�@��@��w@��P@�33@�ff@��T@�{@�$�@��@�J@���@���@�|�@���@�^5@�ff@���@��D@��@���@��h@���@���@���@��`@��@��@�l�@�\)@�C�@���@��@�J@���@��@���@��-@��-@��h@�p�@�%@��9@���@��
@�l�@�33@�;d@�"�@��R@�~�@�V@�=q@��@��^@�O�@�7L@�V@���@�(�@��@�  @��;@���@�t�@�dZ@�33@�^5@��T@���@�7L@�%@��@���@��j@���@�1@�|�@�33@�+@�@���@���@���@���@���@���@��\@�v�@�M�@�$�@��@���@��7@�?}@�V@���@��u@�r�@�9X@�1'@�1'@�9X@�1'@� �@�b@�b@�b@��w@�C�@��P@�S�@�ȴ@���@�^5@��h@���@��u@�r�@�Z@�1@��;@��@�+@��R@�~�@�^5@�V@�M�@��@��T@��^@��h@��7@��7@�x�@�V@��D@�Z@�9X@�  @���@���@�t�@�S�@�33@�
=@���@��y@�ȴ@��\@�^5@�=q@�J@��#@���@���@�@��@��/@���@�z�@�j@�(�@���@���@�|�@�o@��R@�ff@�=q@��@�@���@��@��@��#@�@��@�?}@��@�(�@���@��@�-@��T@���@�`B@�7L@�V@���@�Ĝ@��u@�(�@��;@�33@���@��\@�n�@�J@�@��h@�G�@��`@��j@���@�Z@��@���@�\)@�"�@��y@��\@�v�@�^5@�=q@�$�@�J@��@��^@���@���@��h@��h@�`B@�V@��/@���@�bN@�I�@�(�@� �@��@�b@�b@�b@�1@�1@�  @+@~�+@~ff@~v�@~v�@~ff@~E�@}`B@|��@|j@|I�@|9X@|(�@|�@|�@{��@{�m@{�F@{��@{�@{t�@{C�@{"�@{"�@z�@z��@z-@z�@zJ@y�#@y��@yx�@yG�@y�@x�`@x��@x�9@x�u@x�@xr�@x �@w��@wl�@w;d@w+@w�@w
=@v��@vȴ@vv�@u�-@u?}@u�@t�@tZ@t1@s�F@s33@r��@r-@q��@q�@q�7@p�`@p�9@p�u@p�u@p�u@p�u@p�u@p�@pb@o��@n�@mO�@mV@mV@l��@l�@l�@l�/@l��@lj@k��@kC�@k"�@j�H@jM�@j�@i��@i�#@i��@i��@ix�@hQ�@g�P@g;d@f�y@f�@f�R@f�R@f��@fff@eV@dz�@d9X@cƨ@c�F@ct�@co@b��@b-@a��@aG�@`�`@`�9@`�@`A�@_��@_K�@^�@^��@^V@]�@]�@\��@\�/@\��@\1@[��@[C�@["�@[o@Z�@Z��@Z�H@[@[o@Z�@Y�@Y�7@Y�7@YG�@X�`@X�9@XbN@X1'@W�;@W�P@W;d@V�y@Vff@VE�@V{@UV@T�@T��@Tz�@TI�@T�@S�m@SdZ@SS�@SC�@R��@R��@RJ@Q��@QX@P�`@P�u@Pr�@PA�@O��@O;d@O�@O
=@O
=@N��@N�y@Nv�@N{@M�@M@M�@MO�@L��@K�m@K��@Kt�@K@J^5@I��@Hr�@HA�@HA�@H1'@Hb@G��@G�@Gl�@Gl�@Gl�@GK�@G;d@G�@F�R@F��@FV@E�@D�@DZ@B�@A�@@Ĝ@?�;@?K�@>�y@>v�@<�j@;�m@;t�@;S�@;o@:�\@9�#@9&�@8Ĝ@8Q�@8b@8 �@8 �@8 �@7�;@8 �@8  @8  @8  @8 �@7+@6��@6ȴ@6��@6v�@6ff@6E�@6v�@6ff@6v�@6E�@65?@65?@6$�@6$�@5�T@5��@5�@5?}@5�@4��@4�/@4��@4�@4j@3�
@2��@2M�@1�7@/�@-��@-p�@-/@-V@,�@,��@,�j@,�D@,1@+��@*��@*=q@)��@)7L@(Ĝ@(��@(A�@(1'@(1'@(A�@(A�@'�w1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
Q�B
P�B
XB
W
B
T�B
S�B
S�B
S�B
R�B
Q�B
R�B
S�B
T�B
YB
XB
P�B
P�B
W
B
aHB
`BB
`BB
`BB
aHB
`BB
_;B
^5B
^5B
]/B
VB
XB
^5B
jB
s�B
m�B
m�B
t�B
v�B
w�B
u�B
u�B
|�B
�+B
�+B
�=B
�\B
�RB\Bx�B�B��B��B��B��B��B�TB�B1BJB\B�B#�B+B8RB9XB;dB<jB<jBA�B@�BB�BM�BO�BP�BR�BR�BR�BR�BN�B>wB�B��B�)B��B�uB�\B�DB~�By�Bx�Bu�BiyBO�B
�;B
�3B
�B
v�B
m�B
:^B
-B
 �B
uB	��B	��B	�FB	��B	��B	�VB	�%B	�B	x�B	w�B	v�B	l�B	T�B	H�B	D�B	9XB	!�B	{B	PB	B��B��B��B�B��B�TB�)B�
B�B��B��BȴBÖB��B�wB�XB�dB�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�JB�VB�%B�B�B�B�B�B�B� B~�B}�B�B�B�B�B�B�B�B�%B�%B�7B�1B�1B�1B�=B�VB�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�}B��B��B�/B�5B�HB�B�B��B��B��B	JB	�B	oB	�B	�B	�B	�B	�B	 �B	#�B	"�B	"�B	#�B	#�B	$�B	%�B	&�B	.B	33B	6FB	49B	33B	2-B	1'B	/B	49B	7LB	7LB	9XB	:^B	<jB	B�B	E�B	E�B	H�B	L�B	N�B	P�B	VB	R�B	M�B	O�B	VB	ZB	[#B	e`B	jB	k�B	iyB	iyB	hsB	hsB	hsB	hsB	l�B	n�B	l�B	l�B	o�B	o�B	q�B	u�B	{�B	}�B	~�B	� B	�B	�%B	�PB	�PB	�DB	�7B	�=B	�=B	�7B	�PB	�hB	�{B	��B	��B	��B	�{B	�uB	�oB	�bB	�oB	�uB	�oB	�hB	��B	��B	�B	�B	�'B	�-B	�FB	�FB	�FB	�FB	�?B	�?B	�?B	�?B	�3B	�-B	�!B	�B	�B	��B	��B	�B	��B	�B	�'B	�LB	�XB	�dB	�jB	�dB	�LB	�!B	�9B	�RB	�^B	�^B	ŢB	��B	��B	ɺB	ÖB	ÖB	ÖB	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�
B	�
B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�
B	�B	�B	�B	�B	�
B	�B	�B	�B	�)B	�/B	�/B	�5B	�5B	�5B	�;B	�;B	�;B	�;B	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�NB	�TB	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
	7B
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
PB
\B
bB
bB
bB
bB
bB
bB
hB
oB
oB
hB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
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
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
%�B
&�B
&�B
'�B
)�B
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
+B
+B
,B
,B
,B
,B
,B
,B
,B
.B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
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
E�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
L�B
M�B
M�B
N�B
N�B
N�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
T�B
T�B
T�B
S�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
W
B
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
YB
YB
YB
ZB
]/B
^5B
^5B
_;B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
RB
S>B
Z9B
W�B
U8B
T�B
T&B
T�B
SRB
RB
R�B
T(B
U�B
[@B
Z]B
QsB
P�B
W	B
bCB
`�B
`�B
`�B
a�B
`�B
_�B
^�B
^�B
]�B
VoB
X�B
^�B
k!B
t�B
o"B
o�B
u�B
wzB
x�B
w�B
w�B
�eB
�nB
��B
��B
�rB
��BB{�B�jB��B��BՆB��B�B�mB��BDB�BgB�B'B-�B9�B9�B<�B=�BA�BD�BE�BK�BUPBQBQ�BTBSYBS�BU�B[1BK�B�B�B�VB��B��B�JB�EB�By�By2Bx�Bx�Bn5B
�zB
��B
�AB
}B
|:B
<�B
0�B
#ZB
�B
�B	�XB	��B	�=B	��B	�LB	�-B	�%B	yB	y�B	~�B	sTB	X�B	L�B	K*B	F6B	&>B	B	�B	�B��B�B�OB� B	hB�BޯB��B�#BԖB�B˭B�$BêB��B�LBĕB�5B�B�_B�1B�B��B�WB�-B�~B�vB��B��B�B�~B��B��B��B��B��B�B��B� B��B��B��B��BgB��B��B�6B�,B��B�cB��B�1B�B��B��B��B��B�B��B��B�FB�B��B��B��B��B�rB�VB�bB�2B��B��B�OB��B��B�B��B��B��B�PB��B��B�0B��B�`B�B��B�B�gB�:B�'B��B��B�UB�0B޶B��B�(B�B��B�#B��B	aB	�B	.B	kB	B	B	�B	�B	"IB	$dB	"�B	"�B	$B	$B	%1B	&UB	&�B	.�B	4%B	6�B	5�B	44B	2�B	2'B	0�B	57B	7�B	7B	9RB	:NB	=RB	CDB	E�B	E�B	H�B	L�B	N�B	P�B	XB	T�B	N!B	O�B	VB	ZB	ZEB	fDB	n�B	m�B	j�B	jB	h�B	h�B	h�B	jB	pB	p�B	m�B	m�B	q�B	o�B	q�B	vCB	|dB	~B	~�B	IB	��B	�B	�4B	�gB	�B	�B	�jB	�B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	�~B	��B	��B	�'B	�]B	��B	�HB	�LB	�B	��B	��B	�tB	�5B	�vB	��B	�B	�+B	��B	��B	�#B	��B	��B	�B	��B	�8B	�lB	�B	��B	��B	�B	��B	�;B	��B	��B	�-B	�`B	�tB	�?B	�WB	��B	�B	�B	ϞB	ӸB	�B	��B	ěB	��B	�AB	��B	��B	��B	ǟB	�wB	��B	��B	��B	��B	��B	��B	�B	�B	ԥB	ԂB	�B	�>B	֫B	�eB	��B	�=B	קB	�eB	�KB	�1B	�AB	֣B	װB	�/B	�PB	�iB	�B	� B	�)B	�5B	�TB	�>B	�1B	�YB	�B	��B	ݑB	ݯB	�rB	�ZB	�_B	�VB	�oB	�B	��B	�B	�VB	�~B	�B	�UB	�WB	�lB	�~B	�^B	�jB	�|B	�B	�B	�qB	�B	�B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�eB	�UB	�6B	��B	�B	�.B	�B	��B	�]B	�B	�B	�YB	�B	�`B	�_B	�}B	�#B	��B	��B	��B	�QB	��B	�B	�B	��B	��B	� B	��B	��B	�/B	�B	�6B	��B	�!B	�kB	�B	�B	�#B	��B	�B	�B	�AB	�2B	�B	�3B	�6B	�B	��B	�B	��B	�iB	�^B	�+B	�B	�dB
 �B
VB
=B
�B
�B
�B
RB
GB
BB
,B
)B
.B
9B
EB
�B
�B
�B
�B
 B
bB

6B
�B
�B
�B
�B
�B
wB
�B
�B
�B
�B
SB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
EB
,B
�B
�B
�B
�B
�B
mB
B
-B
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
B
$B
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
!B
!FB
 �B
 �B
!�B
!�B
!�B
!�B
"B
"$B
"xB
#<B
"�B
#:B
$*B
$#B
$0B
$QB
%PB
%_B
&B
%�B
&CB
&iB
'B
'
B
&�B
&�B
&�B
&�B
'B
&LB
'3B
'�B
))B
*7B
*B
*B
*B
*B
*B
*B
*YB
*�B
+LB
+%B
+AB
+~B
,2B
,)B
,(B
,B
,'B
,>B
,�B
.�B
.OB
.KB
.$B
/1B
/"B
/.B
/SB
/�B
0�B
1WB
1uB
1:B
1YB
1nB
2~B
2|B
3xB
3�B
3yB
4^B
4`B
4lB
4�B
4�B
5�B
5tB
5vB
5�B
6�B
6�B
7eB
7_B
7�B
7�B
8�B
8pB
8gB
8sB
8nB
8NB
8CB
8OB
8|B
9B
9�B
9bB
9�B
:�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
;�B
;�B
;�B
<4B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
BB
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
D2B
D)B
D�B
D�B
EB
F5B
FHB
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
IB
I�B
I�B
J�B
KB
KB
K�B
L�B
M�B
N|B
NBB
O&B
O5B
P'B
Q�B
R@B
RB
R"B
RNB
StB
SrB
TFB
TPB
U5B
T�B
UB
UB
U5B
T�B
TB
UB
UB
T�B
T�B
V5B
V7B
V3B
V%B
VB
V"B
U�B
VB
VB
V5B
W"B
VB
VB
VB
VBB
W#B
WYB
WIB
W.B
W0B
W+B
X&B
X2B
XVB
X�B
Z
B
Y�B
Y�B
[�B
^�B
^�B
^vB
_^B
^ZB
_`B
_QB
_lB
_�B
_�B
aB
a�B
a�B
b�B
b�B
brB
c�B
chB
cZB
cOB
c_B
c�B
dg1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<=7"<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%Ǚ<#�
<#�
<#�
<#�
<#�
<#�
<#�
<U��<`ԟ<:�<1��<�a�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�#<��<���<ii<#�
<#�
<xt�<#�
<#�
<#�
<#�
<B�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<]�(<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<K��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%<(V�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<-Ӎ<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.05 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  202009211514302020092115143020200921151430  AO  ARCAADJP                                                                    20191106100038    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20191106100038  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20191106100038  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20200921151430  QC  PRES            @�  D��fG�O�                PM  ARSQCTM V1.1                                                                20200921151430  QC  PSAL            @�  D��fG�O�                PM  ARSQCOWGV1.1CTD_2018v2 + Argo_2018v01                                       20200924132255  IP                  G�O�G�O�G�O�                