CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-12-11T11:00:26Z creation      
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
resolution        =���   axis      Z        L  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  n�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��        ��Argo profile    3.1 1.2 19500101000000  20191211110026  20230721230924  4901659 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5382                            2C  D   NAVIS_A                         0371                            082713                          863 @�����`1   @���@;�-V�c���Q�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� Dz��D{y�D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�<�D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B33B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3CuٙCw�3Cy�3C{�3C~�C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%�3D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz�gD{vgD{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�;3D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�;3D�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�A�D�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD���D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�/A�1'A�/A�/A�1'A�33A�/A�/A�/A�$�A�"�A�{A��A��hA�M�A�&�A��#A�|�A�bNA�-A�A��TA��jA��-A�t�A�C�A��A�
=A�A���A���A��A�ȴA��DA�XA�^5A��^A�=qA�1A�ffA�JA�%A�z�A���A���A���A�%A�^5A�  A���A�~�A�JA���A�Q�A��A�bA�Q�A�p�A��A��wA��A��A��-A��jA��FA�/A��A���A���A��A�dZA��A��#A�ƨA��9A��A��A�G�A��A� �A�z�A�$�A�ffA���A��
A�&�A��^A�oA�p�A��FA�Q�A�%A���A��#A�"�A��A�Q�A~bNA|�AyAx��Avv�As+Aq|�Ap(�AnE�Ak��Ai33Ag`BAf1AeXAd��Ad�Abn�Aa"�A`n�A_�#A^��A]�-A]?}A\�9A[l�AX�HAV�AV-AT��ASS�AR$�AP=qAO/AN�RAN�DAN^5AM�AMAJ�HAI��AI;dAHv�AHJAG�#AG��AGVAE��AE�AD�AC�#AA�PA@bA?p�A>�A>�DA>M�A=�;A=�7A=7LA;�;A:�yA:�\A:I�A9��A8ȴA7t�A6��A65?A5|�A3��A3�A1�-A1`BA1"�A/l�A.(�A-��A-+A,�A+��A*jA+VA*jA*bNA*5?A(�A({A'O�A&��A%�A$�A${A#;dA"�!A!��A!XA ĜA jA��AbNAAdZA�yAJA�!A(�A�mAO�A��A^5A��A �A��AS�A��An�A�hAC�A��A~�Az�A�A�A5?A�PA��A�FA�A�HA�#A33A
��A
1'A
1A	��AbNA%A9XA�
A�PA�AȴAv�A�^A�A�PA ��@��;@���@��R@��@��@�t�@�~�@�/@�r�@�F@��@�7L@�\@��#@�G�@��
@�"�@�x�@�dZ@�E�@�(�@ݲ-@�Q�@ۮ@�|�@�=q@��@�;d@պ^@�O�@�&�@��@���@�1'@�\)@ҧ�@�bN@��H@�~�@�E�@��T@���@�Z@�dZ@�hs@�j@ǥ�@�=q@�  @�
=@�J@�K�@��@�1'@��@�v�@�?}@���@��9@���@�9X@���@�l�@���@��@�A�@��@��@��T@��7@��@�bN@�|�@��\@���@��D@���@��@��@�=q@��@��`@�+@��@�5?@��`@��@���@�ȴ@���@�x�@�V@�b@�|�@�33@��@���@�@�V@�Q�@��@��@�+@��+@���@��9@�9X@�b@�  @�dZ@�+@���@���@�^5@��@���@��h@�V@�I�@���@�"�@��H@���@�n�@�{@��@��@��#@���@�x�@�X@�G�@���@�1@�|�@�;d@�o@��@��\@�5?@�@�p�@�`B@�G�@��@��j@�r�@�Q�@��@��@�K�@�C�@�33@�
=@��@���@�~�@��@�@��h@�X@��@��`@��u@�Z@�I�@���@�ƨ@���@�S�@�"�@��H@���@��R@��\@�E�@�@��@�`B@�&�@�Ĝ@��u@�bN@�b@��;@��w@���@��P@��@�\)@�K�@�+@�ȴ@�v�@�E�@�J@���@�X@��9@���@�Q�@� �@�@�;@�@\)@;d@�@~��@~ȴ@~�+@}@}?}@|��@|�D@|z�@|Z@|9X@|�@{ƨ@z=q@y�@y��@y�^@y��@yhs@y7L@y%@x�9@xr�@xb@w�;@w�@w|�@v��@v{@u�h@u`B@u`B@u/@t�/@t��@tz�@tz�@tz�@tj@tj@tj@t9X@s��@sdZ@sC�@so@r��@r�\@r^5@q�@qhs@qG�@q7L@q&�@p�9@p�9@pb@o+@n�@n�R@nv�@nV@n$�@n{@m@m�h@m?}@m�@l��@l�j@lz�@l(�@l�@l�@l1@k��@k�
@ko@j��@j�\@j=q@jJ@i��@i�@i�^@iX@i%@hĜ@hr�@g�@g+@f�R@f��@fff@f@e@e��@e�@ep�@eO�@e/@e�@d�/@d�D@d�@c�
@c�F@ct�@c@b~�@b=q@b-@bJ@a��@a�^@a��@ahs@a&�@a�@`��@`�9@`r�@`1'@`b@_��@_��@_+@^�R@^v�@]@]�h@]V@\j@[�
@[C�@Z�@Z�\@Z=q@Y�@YG�@Y%@X��@XĜ@X�9@X��@X�u@Xr�@XQ�@XA�@XA�@X1'@X1'@X �@X  @W�@W�;@W�;@W�w@W��@WK�@V��@Vȴ@V�R@V�+@V5?@U`B@U/@T�@T��@T��@T�/@T��@Tz�@T(�@T1@SdZ@R�H@R�\@RM�@Q��@QX@QG�@Q7L@Q&�@Q%@PĜ@P�u@Pr�@P �@O�@O|�@O
=@N�R@N$�@M@M��@M?}@MV@L��@L�@L��@LI�@K�F@Ko@JM�@JJ@I�#@I�7@Ihs@I&�@H��@HĜ@HĜ@HĜ@H��@H�u@Hr�@HbN@H1'@G��@G+@F�@Fff@F{@E@EO�@E�@D��@D�@DI�@C�@CdZ@CdZ@C33@B�H@B��@B�!@B�\@B^5@BJ@AG�@@Ĝ@@�@@A�@?�@?l�@?+@?
=@>��@>�R@>v�@>V@>@=@=�@=?}@=V@<�@<j@<j@<9X@;�m@;��@;S�@;@:��@:��@:~�@:^5@:-@9��@9�7@9&�@9%@8�9@8 �@7�@7�P@7l�@7+@7
=@6��@6V@5�T@5�-@5�h@4�/@4z�@49X@41@3�
@3�
@3ƨ@3��@3��@3S�@3@2n�@2=q@2�@1��@1�^@1G�@0�`@0r�@0Q�@0Q�@01'@0  @/�w@/��@/�P@.�R@-�h@-`B@-�@-V@,�/@,��@,��@,��@,��@,�D@,�D@,�D@,j@,9X@,�@+�
@+��@+�@+"�@*�!@*�!@*^5@)��@)�^@)hs@)G�@(Ĝ@(�@(r�@(r�@(bN@(bN@(bN@(bN@(bN@(A�@(  @'�P@'
=@&ȴ@&�+@&$�@%�-@%�@$�j@$��@$��@$��@$�D@$j@$I�@$9X@$9X@$(�@$1@#��@#ƨ@#C�@#o@#@"�@"��@"��@"�!@"��@"�\@"~�@"�@!��@!��@!�7@!X@!�@ �u@ A�@ 1'@ b@�@K�@�@��@��@��@5?@�h@�@�@�/@�j@�@��@�D@j@I�@ƨ@dZ@dZ@dZ@C�@^5@�@J@��@�@��@��@&�@�9@��@�u@r�@r�@bN@A�@ �@b@b@b@  @�@�P@
=@�@{@�T@�-@`B@�@��@�j@z�@(�@�
@��@S�@33@33@"�@@��@��@�\@~�@^5@=q@-@��@�^@��@x�@hs@hs@7L@%@�`@��@�9@�9@�u@�@r�@bN@Q�@  @l�@K�@+@��@E�@�-@p�@`B@O�@?}@/@�@V@��@�@��@�j@�j@�D@j@(�@1@ƨ@�F@��@��@��@�@S�@o@
�@
��@
�!@
�\@
M�@
J@	�@	�#@	�7@	hs@	G�@	%@Ĝ@��@r�@ �@�@��@��@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�/A�1'A�/A�/A�1'A�33A�/A�/A�/A�$�A�"�A�{A��A��hA�M�A�&�A��#A�|�A�bNA�-A�A��TA��jA��-A�t�A�C�A��A�
=A�A���A���A��A�ȴA��DA�XA�^5A��^A�=qA�1A�ffA�JA�%A�z�A���A���A���A�%A�^5A�  A���A�~�A�JA���A�Q�A��A�bA�Q�A�p�A��A��wA��A��A��-A��jA��FA�/A��A���A���A��A�dZA��A��#A�ƨA��9A��A��A�G�A��A� �A�z�A�$�A�ffA���A��
A�&�A��^A�oA�p�A��FA�Q�A�%A���A��#A�"�A��A�Q�A~bNA|�AyAx��Avv�As+Aq|�Ap(�AnE�Ak��Ai33Ag`BAf1AeXAd��Ad�Abn�Aa"�A`n�A_�#A^��A]�-A]?}A\�9A[l�AX�HAV�AV-AT��ASS�AR$�AP=qAO/AN�RAN�DAN^5AM�AMAJ�HAI��AI;dAHv�AHJAG�#AG��AGVAE��AE�AD�AC�#AA�PA@bA?p�A>�A>�DA>M�A=�;A=�7A=7LA;�;A:�yA:�\A:I�A9��A8ȴA7t�A6��A65?A5|�A3��A3�A1�-A1`BA1"�A/l�A.(�A-��A-+A,�A+��A*jA+VA*jA*bNA*5?A(�A({A'O�A&��A%�A$�A${A#;dA"�!A!��A!XA ĜA jA��AbNAAdZA�yAJA�!A(�A�mAO�A��A^5A��A �A��AS�A��An�A�hAC�A��A~�Az�A�A�A5?A�PA��A�FA�A�HA�#A33A
��A
1'A
1A	��AbNA%A9XA�
A�PA�AȴAv�A�^A�A�PA ��@��;@���@��R@��@��@�t�@�~�@�/@�r�@�F@��@�7L@�\@��#@�G�@��
@�"�@�x�@�dZ@�E�@�(�@ݲ-@�Q�@ۮ@�|�@�=q@��@�;d@պ^@�O�@�&�@��@���@�1'@�\)@ҧ�@�bN@��H@�~�@�E�@��T@���@�Z@�dZ@�hs@�j@ǥ�@�=q@�  @�
=@�J@�K�@��@�1'@��@�v�@�?}@���@��9@���@�9X@���@�l�@���@��@�A�@��@��@��T@��7@��@�bN@�|�@��\@���@��D@���@��@��@�=q@��@��`@�+@��@�5?@��`@��@���@�ȴ@���@�x�@�V@�b@�|�@�33@��@���@�@�V@�Q�@��@��@�+@��+@���@��9@�9X@�b@�  @�dZ@�+@���@���@�^5@��@���@��h@�V@�I�@���@�"�@��H@���@�n�@�{@��@��@��#@���@�x�@�X@�G�@���@�1@�|�@�;d@�o@��@��\@�5?@�@�p�@�`B@�G�@��@��j@�r�@�Q�@��@��@�K�@�C�@�33@�
=@��@���@�~�@��@�@��h@�X@��@��`@��u@�Z@�I�@���@�ƨ@���@�S�@�"�@��H@���@��R@��\@�E�@�@��@�`B@�&�@�Ĝ@��u@�bN@�b@��;@��w@���@��P@��@�\)@�K�@�+@�ȴ@�v�@�E�@�J@���@�X@��9@���@�Q�@� �@�@�;@�@\)@;d@�@~��@~ȴ@~�+@}@}?}@|��@|�D@|z�@|Z@|9X@|�@{ƨ@z=q@y�@y��@y�^@y��@yhs@y7L@y%@x�9@xr�@xb@w�;@w�@w|�@v��@v{@u�h@u`B@u`B@u/@t�/@t��@tz�@tz�@tz�@tj@tj@tj@t9X@s��@sdZ@sC�@so@r��@r�\@r^5@q�@qhs@qG�@q7L@q&�@p�9@p�9@pb@o+@n�@n�R@nv�@nV@n$�@n{@m@m�h@m?}@m�@l��@l�j@lz�@l(�@l�@l�@l1@k��@k�
@ko@j��@j�\@j=q@jJ@i��@i�@i�^@iX@i%@hĜ@hr�@g�@g+@f�R@f��@fff@f@e@e��@e�@ep�@eO�@e/@e�@d�/@d�D@d�@c�
@c�F@ct�@c@b~�@b=q@b-@bJ@a��@a�^@a��@ahs@a&�@a�@`��@`�9@`r�@`1'@`b@_��@_��@_+@^�R@^v�@]@]�h@]V@\j@[�
@[C�@Z�@Z�\@Z=q@Y�@YG�@Y%@X��@XĜ@X�9@X��@X�u@Xr�@XQ�@XA�@XA�@X1'@X1'@X �@X  @W�@W�;@W�;@W�w@W��@WK�@V��@Vȴ@V�R@V�+@V5?@U`B@U/@T�@T��@T��@T�/@T��@Tz�@T(�@T1@SdZ@R�H@R�\@RM�@Q��@QX@QG�@Q7L@Q&�@Q%@PĜ@P�u@Pr�@P �@O�@O|�@O
=@N�R@N$�@M@M��@M?}@MV@L��@L�@L��@LI�@K�F@Ko@JM�@JJ@I�#@I�7@Ihs@I&�@H��@HĜ@HĜ@HĜ@H��@H�u@Hr�@HbN@H1'@G��@G+@F�@Fff@F{@E@EO�@E�@D��@D�@DI�@C�@CdZ@CdZ@C33@B�H@B��@B�!@B�\@B^5@BJ@AG�@@Ĝ@@�@@A�@?�@?l�@?+@?
=@>��@>�R@>v�@>V@>@=@=�@=?}@=V@<�@<j@<j@<9X@;�m@;��@;S�@;@:��@:��@:~�@:^5@:-@9��@9�7@9&�@9%@8�9@8 �@7�@7�P@7l�@7+@7
=@6��@6V@5�T@5�-@5�h@4�/@4z�@49X@41@3�
@3�
@3ƨ@3��@3��@3S�@3@2n�@2=q@2�@1��@1�^@1G�@0�`@0r�@0Q�@0Q�@01'@0  @/�w@/��@/�P@.�R@-�h@-`B@-�@-V@,�/@,��@,��@,��@,��@,�D@,�D@,�D@,j@,9X@,�@+�
@+��@+�@+"�@*�!@*�!@*^5@)��@)�^@)hs@)G�@(Ĝ@(�@(r�@(r�@(bN@(bN@(bN@(bN@(bN@(A�@(  @'�P@'
=@&ȴ@&�+@&$�@%�-@%�@$�j@$��@$��@$��@$�D@$j@$I�@$9X@$9X@$(�@$1@#��@#ƨ@#C�@#o@#@"�@"��@"��@"�!@"��@"�\@"~�@"�@!��@!��@!�7@!X@!�@ �u@ A�@ 1'@ b@�@K�@�@��@��@��@5?@�h@�@�@�/@�j@�@��@�D@j@I�@ƨ@dZ@dZ@dZ@C�@^5@�@J@��@�@��@��@&�@�9@��@�u@r�@r�@bN@A�@ �@b@b@b@  @�@�P@
=@�@{@�T@�-@`B@�@��@�j@z�@(�@�
@��@S�@33@33@"�@@��@��@�\@~�@^5@=q@-@��@�^@��@x�@hs@hs@7L@%@�`@��@�9@�9@�u@�@r�@bN@Q�@  @l�@K�@+@��@E�@�-@p�@`B@O�@?}@/@�@V@��@�@��@�j@�j@�D@j@(�@1@ƨ@�F@��@��@��@�@S�@o@
�@
��@
�!@
�\@
M�@
J@	�@	�#@	�7@	hs@	G�@	%@Ĝ@��@r�@ �@�@��@��@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�?B�^B��B��BĜBȴB��B��B��B��B��B��B��B��B��B��B��B��BȴBŢB�XB�'B��B�oBXB6FB+B �B�B33B5?B/B)�B,B-B+B&�B#�B �B�B�BVBB��B�fB�#BÖB�9B��B��B�uB�7B~�Bs�BgmB]/BW
BT�BS�BQ�BI�B@�B<jB33B)�B�B�BVB1BB
��B
�B
�yB
�5B
��B
��B
ȴB
B
�LB
�B
��B
�JB
w�B
bNB
L�B
F�B
5?B
�B
bB
+B	��B	�B	�#B	��B	ƨB	��B	�dB	�RB	�-B	��B	��B	��B	��B	�oB	�VB	�=B	�B	t�B	e`B	aHB	]/B	T�B	N�B	F�B	B�B	A�B	@�B	>wB	:^B	49B	'�B	"�B	�B	�B	�B	�B	�B	�B	�B	�B	oB	hB	\B	DB		7B	+B	B	B	B	  B��B��B�B�B�B�B�mB�HB�#B��B��B�jB�3B��B��B��B��B�{B�oB�VB�DB�+B�B��B��B��B��B��B��B�hB�VB�B�B}�B{�B{�Bv�Bv�Br�Bq�BjB`BB^5B\)B\)BZBXBXBXBYBXBVBT�BR�BQ�BO�BN�BL�BK�BK�BH�BF�BF�BF�BC�BC�B@�B>wB<jB;dB:^B:^B8RB8RB9XB8RB5?B33B2-B1'B1'B0!B1'B5?B49B2-B0!B.B,B)�B'�B#�B!�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBoBhBbBbBbBbB\B\BVBVB\B\B\B\BVBVB\BVB\B\B\B\BVB\B\BVBbBbBbBhBuBuBuB�B�B�B�B�B!�B!�B"�B"�B"�B"�B"�B&�B'�B(�B(�B+B.B/B/B1'B33B5?B8RB:^B<jB<jB>wB@�B?}BB�BG�BH�BK�BP�BT�BVBZB^5B`BBbNBgmBjBjBl�Bm�Br�Bv�Bz�B|�B}�B� B�B�1B�PB�bB�hB�hB��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�3B�?B�FB�FB�FB�RB�RB�XB�XB�^B��BĜBŢBƨBȴB��B��B��B��B��B�B�
B�#B�)B�/B�;B�NB�`B�`B�fB�mB�yB�B�B�B�B�B��B��B��B��B	B	B	B	%B	1B	DB	JB	\B	bB	hB	oB	�B	�B	�B	�B	�B	"�B	#�B	%�B	)�B	,B	.B	/B	0!B	0!B	2-B	2-B	33B	6FB	9XB	:^B	<jB	@�B	B�B	H�B	I�B	K�B	M�B	O�B	O�B	P�B	R�B	R�B	S�B	S�B	T�B	W
B	[#B	^5B	`BB	aHB	bNB	cTB	cTB	dZB	e`B	n�B	o�B	p�B	p�B	p�B	r�B	s�B	s�B	u�B	v�B	x�B	y�B	z�B	{�B	~�B	�B	�%B	�+B	�+B	�7B	�=B	�JB	�VB	�\B	�\B	�bB	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�-B	�?B	�FB	�LB	�RB	�XB	�XB	�XB	�^B	�^B	�^B	�dB	�jB	�qB	��B	B	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�5B	�5B	�BB	�HB	�NB	�TB	�TB	�ZB	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
DB
PB
VB
\B
\B
bB
hB
oB
oB
oB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
&�B
&�B
'�B
(�B
(�B
)�B
(�B
)�B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
/B
0!B
0!B
1'B
2-B
2-B
33B
33B
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
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
<jB
=qB
=qB
>wB
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
F�B
F�B
F�B
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
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
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
XB
YB
ZB
ZB
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
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
`BB
`BB
aHB
aHB
aHB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
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
jB
jB
k�B
l�B
l�B
l�B
m�B
m�B
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
o�B
o�B
o�B
o�B
o�B
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
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�?B�^B�jB�wBBĜBƨBɺBɺBȴBǮBǮBǮBǮBǮBǮBƨBÖB��B�?B�B��B��B_;B8RB#�B�B�B49B.B+B$�B&�B'�B%�B!�B�B�B�BhBDBB�B�NB�B��B�!B��B��B�\B�Bz�Bo�BcTBXBP�BO�BN�BM�BD�B;dB8RB/B%�B�B{B	7BBB
��B
�B
�`B
�B
��B
ȴB
ĜB
�wB
�3B
��B
��B
�7B
t�B
`BB
F�B
D�B
33B
�B
JB
B	��B	�yB	�
B	��B	��B	�jB	�FB	�?B	�B	��B	��B	��B	�{B	�PB	�7B	�+B	� B	p�B	`BB	]/B	ZB	P�B	K�B	A�B	=qB	;dB	;dB	9XB	6FB	1'B	"�B	�B	�B	�B	�B	�B	�B	�B	{B	bB	VB	VB	DB	%B	B	B	  B��B��B��B��B�B�B�B�B�sB�TB�)B�B��BȴB�LB�B��B��B��B��B�VB�PB�7B�+B�B{�B�uB�uB��B��B��B�hB�JB�=B� B}�Bx�Bv�Bw�Bq�Bq�Bm�Bm�BffB[#BYBW
BXBVBR�BR�BR�BS�BR�BQ�BO�BM�BL�BJ�BI�BH�BF�BF�BC�BA�BB�BB�B>wB>wB<jB:^B7LB6FB6FB5?B33B33B49B49B1'B/B-B,B,B+B,B0!B0!B.B,B(�B'�B%�B$�B�B�B�B�B�B�B�B�B�B{BhBhBhBbBbB\BPBPBJBDBDBDB
=B
=B
=B	7B
=B
=B
=B
=B	7B	7B
=B
=B
=B
=B
=B
=B	7B
=B
=B
=BDBDBJBPBVBVBbB{B�B�B�B�B�B�B�B�B�B�B�B �B"�B#�B#�B%�B(�B)�B)�B,B.B0!B33B5?B7LB7LB9XB;dB;dB>wBB�BC�BF�BK�BO�BP�BT�BYB[#B]/BbNBe`Be`BgmBhsBm�Bq�Bu�Bw�Bx�Bz�B}�B�B�1B�=B�JB�JB�bB�hB�uB��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�'B�'B�3B�3B�9B�9B�?B�dB�wB��B��BÖBŢBȴB��B��B��B��B��B�B�
B�B�B�/B�;B�;B�HB�NB�ZB�`B�mB�B�B�B�B�B��B��B��B��B	  B	B	B	%B	+B		7B	DB	JB	PB	bB	�B	�B	�B	�B	�B	�B	 �B	$�B	%�B	'�B	(�B	)�B	+B	,B	-B	.B	1'B	49B	5?B	7LB	;dB	=qB	B�B	D�B	F�B	G�B	I�B	I�B	J�B	L�B	L�B	M�B	N�B	O�B	Q�B	VB	YB	ZB	[#B	\)B	]/B	]/B	_;B	`BB	hsB	iyB	jB	jB	jB	l�B	m�B	n�B	o�B	q�B	r�B	s�B	u�B	v�B	y�B	~�B	� B	�B	�B	�B	�B	�%B	�1B	�7B	�7B	�=B	�=B	�DB	�JB	�VB	�\B	�bB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�3B	�3B	�9B	�9B	�9B	�?B	�LB	�RB	�dB	�jB	�qB	�wB	�}B	�}B	��B	B	ÖB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�#B	�#B	�)B	�5B	�5B	�;B	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
B
%B
1B
1B
	7B

=B
DB
DB
JB
JB
JB
JB
JB
PB
VB
PB
\B
bB
hB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
(�B
(�B
)�B
)�B
)�B
,B
,B
,B
-B
-B
-B
-B
.B
/B
.B
/B
/B
0!B
0!B
1'B
1'B
1'B
2-B
1'B
2-B
2-B
33B
33B
33B
33B
33B
49B
5?B
5?B
5?B
6FB
7LB
6FB
7LB
7LB
8RB
8RB
9XB
8RB
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
>wB
=qB
>wB
>wB
?}B
?}B
?}B
@�B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
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
B�B
C�B
D�B
D�B
E�B
F�B
E�B
F�B
F�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
L�B
M�B
M�B
M�B
M�B
O�B
N�B
O�B
O�B
P�B
O�B
P�B
P�B
P�B
Q�B
Q�B
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
VB
T�B
VB
VB
VB
W
B
XB
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
ZB
[#B
\)B
[#B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
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
dZB
dZB
dZB
dZB
e`B
ffB
ffB
ffB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
p�B
o�B
o�B
p�B
p�B
p�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0.05 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9999 (+/-0), vertically averaged dS = -0.006 (+/-0.001)                                                                                                                        Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      202307212300262023072123002620230721230026  AO  ARCAADJP                                                                    20191211110026    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20191211110026  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20191211110026  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20230712105749  QC  PRES            @���D��G�O�                PM  ARSQCTM V1.1                                                                20230712105749  QC  PSAL            @���D��G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20230721230924  IP                  G�O�G�O�G�O�                