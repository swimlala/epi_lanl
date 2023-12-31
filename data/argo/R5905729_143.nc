CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-03-27T10:02:48Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݌   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20220327100248  20220331211527  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @���z�<1   @���I���@&�I�^5?�d?n��P1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   B   F   @���@�  @���A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   BffBffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B�33B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڃ3D��3D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D��3D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@�ff@�33A33A?33A_33A�fgA���A���A���A���Aϙ�Aߙ�AA���B33B33B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��B��fB��B��B��fB��fB��fB��B��3B��fB��fB��fB��fB��fB��fB׳3B۳3B��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C:�C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D03D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��3D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fDځ�D���D��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD���D��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ffA�ffA�dZA�hsA�n�A�p�A�r�A�r�A�t�A�t�A�r�A�p�A�n�A�jA�hsA�ffA�ffA�hsA�jA�jA�ffA�bNA�ZA�S�A�I�A�A�A�5?A�&�A�bA��A���Aׇ+A�-AֶFA��A�S�A��A�VA�=qA˴9A�bNA��A�K�A��yA�~�A��^A��mA�G�A��A�
=A�5?A�ƨAx��Ar�jAn�HAk�-Ah��Af�Ad�RAc+Aa��A`M�A^z�A\A�AY�AV�ATjAR{APANVAL��AJ��AI;dAG�#AF�!AE��AD��AC��AC
=ABZAA�A@��A@5?A?;dA>5?A=\)A<�A;�-A:��A9�A9�A89XA7&�A6A4��A4  A3/A2z�A1�wA133A0��A0ZA/�A/��A/O�A/�A.ĜA.VA-�^A-�A,��A,-A+��A+t�A+�A*�A*9XA)ƨA)l�A)"�A(�yA(�A(bNA(bA'��A'33A&��A&�+A&E�A%��A%�FA%x�A%33A$�A$��A$VA$  A#��A#S�A"��A"�\A"�A!�-A!K�A �yA �DA I�A �A�A�-AhsA�A��A�\AI�A{A�mA�-At�A/A�/A~�A�A��A��A�AdZA7LA%AȴA�DAI�A��A��AG�A�A��AVAbA��Ax�AoA��AM�A1A��A��AhsA/A�A�!AffA�A�mA�wA�PA`BA&�A�`A��AbNA �A�A�wA�PAK�AA�!AVA�A�A%A��AVA�A�TA�At�A/A��An�AbA��A�7AS�A/A%A
��A
��A
^5A
�A	��A	t�A	�A��A�uAQ�AA��AK�A��A��AQ�A��A�-Ap�A"�AȴAjA1A��A7LA��AE�A�A��AXA�A �A �+A (�@��P@��@�-@�@�`B@���@��u@��@���@�o@�~�@��^@��@�1'@�|�@��@��+@�J@��7@��@�1'@�P@���@�n�@��#@�O�@�j@�1'@@�+@�R@�5?@홚@��/@�1'@�|�@���@�-@��@�?}@��/@�r�@�b@�F@�dZ@��@���@�+@�$�@�-@�/@�j@�A�@�ƨ@�S�@���@��@�M�@��T@�x�@�%@��D@���@�dZ@��@�^5@���@�G�@ܼj@�1'@ۮ@�"�@ڗ�@�@�hs@���@�I�@�ƨ@�K�@���@�5?@թ�@��@ԣ�@�(�@Ӯ@�33@җ�@��T@�&�@�r�@϶F@�
=@�ff@ͺ^@��@̣�@�A�@��;@�|�@�o@ʧ�@�-@ɡ�@���@�9X@�\)@ư!@�5?@Ų-@�?}@���@�9X@å�@��@+@�@��h@�%@�r�@���@�o@�M�@��7@�Ĝ@��;@�
=@�^5@���@��@�A�@��w@�K�@��@�ff@��#@�G�@���@�Z@��@���@�C�@��@���@�5?@��^@�7L@��@�1'@��F@�33@��\@��@�hs@���@�z�@���@�t�@��y@�n�@���@��h@�/@���@�bN@���@�|�@���@�v�@��@��#@��h@�7L@�Ĝ@�Q�@��@��P@�"�@�ȴ@�v�@�{@��^@�hs@��@���@��u@�I�@�  @���@�;d@���@�n�@�{@���@��7@�?}@��`@��u@�Q�@�b@��w@�l�@��@���@�^5@�@���@�?}@��`@��u@�Z@�(�@���@�ƨ@���@�\)@�"�@��@�ȴ@���@�n�@�5?@���@�@��7@�O�@��@���@�z�@�1'@��@��@�\)@�o@���@�ff@�J@��-@�O�@��@��@�b@��w@�l�@��@���@�V@��@��@�&�@���@�r�@�1@���@�+@�ȴ@�^5@�@�@�x�@�/@��@��@�bN@�1@���@�"�@���@�n�@�$�@��#@��h@�7L@��/@��u@�9X@��m@���@�K�@�
=@���@��\@�E�@�@���@���@�x�@�O�@�&�@���@���@��u@�Z@��@�w@;d@~�R@~$�@}�@|�/@|Z@{�
@{S�@z��@z=q@y�#@y�7@y&�@xĜ@xbN@w�@wl�@v�y@vv�@v$�@u��@u�h@u?}@t�@t��@t9X@s�m@s�@so@r��@r�\@r^5@r-@q��@q�#@q��@qx�@q7L@q%@p��@p�u@pQ�@p  @o�w@ol�@o+@n�@n��@nE�@m�T@m�@m�@l�@l9X@kƨ@kdZ@j�@j~�@i��@ix�@i%@h�u@h1'@g��@gl�@g
=@f�R@fV@f@e�h@e/@d�/@d��@dZ@d�@c�
@ct�@c"�@b�H@b��@b^5@b�@a��@ax�@a&�@`Ĝ@`r�@` �@_�w@_|�@_+@^ȴ@^E�@]�-@]/@\�@\(�@[��@["�@Z��@ZM�@Y��@Y��@YX@Y�@X��@X�u@XbN@X �@W�;@W��@W\)@W�@V�@V�+@V$�@U@U`B@UV@T�j@Tj@T�@Sƨ@St�@S"�@R�H@R��@R^5@R�@Q��@Qx�@Q&�@PĜ@Pr�@Pb@O�w@Ol�@O+@N��@Nȴ@N��@NV@N{@M��@M�h@M`B@M�@L�/@L��@L9X@K�
@Kt�@J�@J~�@J�@I�^@IX@I%@H��@H��@Hr�@H1'@G�@G�w@G|�@GK�@G
=@Fȴ@F�+@F5?@E�T@E�@E/@D��@Dj@C��@C��@CC�@B�H@B~�@B�@A��@Ax�@A&�@@��@@r�@@b@?�w@?|�@?+@>�@>v�@>$�@=�T@=�h@=?}@<��@<��@<�@<z�@<Z@<9X@<9X@<9X@<(�@;��@;��@;33@:��@:n�@:�@9��@9�^@9��@9X@97L@9&�@9�@8��@8r�@8A�@7�w@7l�@6��@65?@6$�@5�@5�h@5�@5?}@4�@4��@4j@3��@3�@3C�@2�H@2n�@2�@1��@1��@1%@0�`@0�u@0b@/��@/��@/|�@/K�@/�@.�R@.E�@.{@-@-?}@,��@,(�@+��@+ƨ@+�F@+�@+dZ@+33@+"�@*�@*�!@*�\@*^5@*J@)��@)�7@)�7@)x�@)7L@(��@(�9@(bN@'�;@'\)@'�@&�y@&�y@&�@&�@&�R@&�+@&V@&5?@&{@%�T@%�-@%�h@%�@%�@%p�@%p�@%p�@%p�@%O�@%/@$��@$�D@#��@#ƨ@#�F@#��@#t�@#S�@"�@"n�@"J@!��@!x�@!&�@!%@!%@!%@!%@!%@!%@!%@!%@ �9@ bN@ Q�@�;@�@�@|�@\)@;d@�@��@E�@@@�@�@�@�j@��@��@Z@��@�m@�m@�F@��@33@^5@=q@=q@-@��@��@�7@X@�@%@��@�9@�u@�@�@��@�w@��@l�@\)@ȴ@V@ff@ff@ff@ff@ff@E�@�T@��@�-@�h@O�@/@�@�/@��@Z@9X@9X@�@1@ƨ@�@33@�@��@��@M�@�@��@��@�@��@��@��@�7@hs@G�@��@�u@�@r�@A�@  @�;@�@�@�@�P@\)@+@��@�@�@��@��@��@��@�+@E�@{@�T@�@p�@`B@?}@/@/@/@�@�@��@�111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ffA�ffA�dZA�hsA�n�A�p�A�r�A�r�A�t�A�t�A�r�A�p�A�n�A�jA�hsA�ffA�ffA�hsA�jA�jA�ffA�bNA�ZA�S�A�I�A�A�A�5?A�&�A�bA��A���Aׇ+A�-AֶFA��A�S�A��A�VA�=qA˴9A�bNA��A�K�A��yA�~�A��^A��mA�G�A��A�
=A�5?A�ƨAx��Ar�jAn�HAk�-Ah��Af�Ad�RAc+Aa��A`M�A^z�A\A�AY�AV�ATjAR{APANVAL��AJ��AI;dAG�#AF�!AE��AD��AC��AC
=ABZAA�A@��A@5?A?;dA>5?A=\)A<�A;�-A:��A9�A9�A89XA7&�A6A4��A4  A3/A2z�A1�wA133A0��A0ZA/�A/��A/O�A/�A.ĜA.VA-�^A-�A,��A,-A+��A+t�A+�A*�A*9XA)ƨA)l�A)"�A(�yA(�A(bNA(bA'��A'33A&��A&�+A&E�A%��A%�FA%x�A%33A$�A$��A$VA$  A#��A#S�A"��A"�\A"�A!�-A!K�A �yA �DA I�A �A�A�-AhsA�A��A�\AI�A{A�mA�-At�A/A�/A~�A�A��A��A�AdZA7LA%AȴA�DAI�A��A��AG�A�A��AVAbA��Ax�AoA��AM�A1A��A��AhsA/A�A�!AffA�A�mA�wA�PA`BA&�A�`A��AbNA �A�A�wA�PAK�AA�!AVA�A�A%A��AVA�A�TA�At�A/A��An�AbA��A�7AS�A/A%A
��A
��A
^5A
�A	��A	t�A	�A��A�uAQ�AA��AK�A��A��AQ�A��A�-Ap�A"�AȴAjA1A��A7LA��AE�A�A��AXA�A �A �+A (�@��P@��@�-@�@�`B@���@��u@��@���@�o@�~�@��^@��@�1'@�|�@��@��+@�J@��7@��@�1'@�P@���@�n�@��#@�O�@�j@�1'@@�+@�R@�5?@홚@��/@�1'@�|�@���@�-@��@�?}@��/@�r�@�b@�F@�dZ@��@���@�+@�$�@�-@�/@�j@�A�@�ƨ@�S�@���@��@�M�@��T@�x�@�%@��D@���@�dZ@��@�^5@���@�G�@ܼj@�1'@ۮ@�"�@ڗ�@�@�hs@���@�I�@�ƨ@�K�@���@�5?@թ�@��@ԣ�@�(�@Ӯ@�33@җ�@��T@�&�@�r�@϶F@�
=@�ff@ͺ^@��@̣�@�A�@��;@�|�@�o@ʧ�@�-@ɡ�@���@�9X@�\)@ư!@�5?@Ų-@�?}@���@�9X@å�@��@+@�@��h@�%@�r�@���@�o@�M�@��7@�Ĝ@��;@�
=@�^5@���@��@�A�@��w@�K�@��@�ff@��#@�G�@���@�Z@��@���@�C�@��@���@�5?@��^@�7L@��@�1'@��F@�33@��\@��@�hs@���@�z�@���@�t�@��y@�n�@���@��h@�/@���@�bN@���@�|�@���@�v�@��@��#@��h@�7L@�Ĝ@�Q�@��@��P@�"�@�ȴ@�v�@�{@��^@�hs@��@���@��u@�I�@�  @���@�;d@���@�n�@�{@���@��7@�?}@��`@��u@�Q�@�b@��w@�l�@��@���@�^5@�@���@�?}@��`@��u@�Z@�(�@���@�ƨ@���@�\)@�"�@��@�ȴ@���@�n�@�5?@���@�@��7@�O�@��@���@�z�@�1'@��@��@�\)@�o@���@�ff@�J@��-@�O�@��@��@�b@��w@�l�@��@���@�V@��@��@�&�@���@�r�@�1@���@�+@�ȴ@�^5@�@�@�x�@�/@��@��@�bN@�1@���@�"�@���@�n�@�$�@��#@��h@�7L@��/@��u@�9X@��m@���@�K�@�
=@���@��\@�E�@�@���@���@�x�@�O�@�&�@���@���@��u@�Z@��@�w@;d@~�R@~$�@}�@|�/@|Z@{�
@{S�@z��@z=q@y�#@y�7@y&�@xĜ@xbN@w�@wl�@v�y@vv�@v$�@u��@u�h@u?}@t�@t��@t9X@s�m@s�@so@r��@r�\@r^5@r-@q��@q�#@q��@qx�@q7L@q%@p��@p�u@pQ�@p  @o�w@ol�@o+@n�@n��@nE�@m�T@m�@m�@l�@l9X@kƨ@kdZ@j�@j~�@i��@ix�@i%@h�u@h1'@g��@gl�@g
=@f�R@fV@f@e�h@e/@d�/@d��@dZ@d�@c�
@ct�@c"�@b�H@b��@b^5@b�@a��@ax�@a&�@`Ĝ@`r�@` �@_�w@_|�@_+@^ȴ@^E�@]�-@]/@\�@\(�@[��@["�@Z��@ZM�@Y��@Y��@YX@Y�@X��@X�u@XbN@X �@W�;@W��@W\)@W�@V�@V�+@V$�@U@U`B@UV@T�j@Tj@T�@Sƨ@St�@S"�@R�H@R��@R^5@R�@Q��@Qx�@Q&�@PĜ@Pr�@Pb@O�w@Ol�@O+@N��@Nȴ@N��@NV@N{@M��@M�h@M`B@M�@L�/@L��@L9X@K�
@Kt�@J�@J~�@J�@I�^@IX@I%@H��@H��@Hr�@H1'@G�@G�w@G|�@GK�@G
=@Fȴ@F�+@F5?@E�T@E�@E/@D��@Dj@C��@C��@CC�@B�H@B~�@B�@A��@Ax�@A&�@@��@@r�@@b@?�w@?|�@?+@>�@>v�@>$�@=�T@=�h@=?}@<��@<��@<�@<z�@<Z@<9X@<9X@<9X@<(�@;��@;��@;33@:��@:n�@:�@9��@9�^@9��@9X@97L@9&�@9�@8��@8r�@8A�@7�w@7l�@6��@65?@6$�@5�@5�h@5�@5?}@4�@4��@4j@3��@3�@3C�@2�H@2n�@2�@1��@1��@1%@0�`@0�u@0b@/��@/��@/|�@/K�@/�@.�R@.E�@.{@-@-?}@,��@,(�@+��@+ƨ@+�F@+�@+dZ@+33@+"�@*�@*�!@*�\@*^5@*J@)��@)�7@)�7@)x�@)7L@(��@(�9@(bN@'�;@'\)@'�@&�y@&�y@&�@&�@&�R@&�+@&V@&5?@&{@%�T@%�-@%�h@%�@%�@%p�@%p�@%p�@%p�@%O�@%/@$��@$�D@#��@#ƨ@#�F@#��@#t�@#S�@"�@"n�@"J@!��@!x�@!&�@!%@!%@!%@!%@!%@!%@!%@!%@ �9@ bN@ Q�@�;@�@�@|�@\)@;d@�@��@E�@@@�@�@�@�j@��@��@Z@��@�m@�m@�F@��@33@^5@=q@=q@-@��@��@�7@X@�@%@��@�9@�u@�@�@��@�w@��@l�@\)@ȴ@V@ff@ff@ff@ff@ff@E�@�T@��@�-@�h@O�@/@�@�/@��@Z@9X@9X@�@1@ƨ@�@33@�@��@��@M�@�@��@��@�@��@��@��@�7@hs@G�@��@�u@�@r�@A�@  @�;@�@�@�@�P@\)@+@��@�@�@��@��@��@��@�+@E�@{@�T@�@p�@`B@?}@/@/@/@�@�@��@�111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�!B
�B
��B
r�B
m�B
k�B
iyB
hsB
ffB
e`B
cTB
cTB
bNB
aHB
aHB
`BB
^5B
[#B
W
B
R�B
O�B
K�B
H�B
D�B
@�B
:^B
1'B
#�B
{B
B	��B	�mB	�#B	��B	��B	ƨB	�
B
�B
%�B��B	Bk�B�BVB�ZBdZB�B]/BW
B�B�TB�B�B��B�;BB-B9XBE�B>wB)�BuB1BJB+BK�BjB�bB�!B��B��B��B	oB	+B	;dB	K�B	XB	^5B	cTB	cTB	bNB	_;B	`BB	m�B	}�B	�B	�7B	�PB	�{B	��B	��B	��B	�LB	��B	�/B	�B	��B
%B
�B
�B
�B
�B
%�B
&�B
"�B
�B
oB
DB
VB
�B
�B
 �B
"�B
�B
�B
�B
#�B
)�B
-B
-B
'�B
"�B
�B
�B
�B
#�B
)�B
(�B
'�B
&�B
%�B
"�B
 �B
�B
�B
�B
�B
�B
{B
oB
uB
�B
�B
�B
$�B
,B
.B
+B
&�B
#�B
$�B
&�B
'�B
+B
-B
,B
'�B
$�B
!�B
�B
 �B
$�B
-B
33B
1'B
,B
&�B
!�B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
$�B
&�B
'�B
%�B
#�B
!�B
 �B
!�B
#�B
(�B
(�B
%�B
!�B
�B
�B
�B
�B
�B
�B
{B
\B
	7B
B
B
B
B
B

=B
{B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
%�B
'�B
+B
+B
'�B
"�B
�B
�B
�B
�B
�B
�B
 �B
"�B
 �B
�B
�B
"�B
'�B
,B
0!B
1'B
49B
33B
.B
)�B
'�B
%�B
#�B
#�B
$�B
(�B
1'B
5?B
5?B
5?B
33B
0!B
/B
1'B
5?B
:^B
>wB
>wB
:^B
6FB
2-B
/B
+B
&�B
"�B
"�B
&�B
+B
0!B
0!B
-B
)�B
'�B
'�B
+B
.B
/B
/B
/B
.B
-B
-B
.B
+B
&�B
"�B
"�B
%�B
'�B
,B
1'B
5?B
8RB
8RB
8RB
9XB
9XB
9XB
7LB
49B
1'B
-B
)�B
)�B
)�B
)�B
)�B
+B
,B
)�B
&�B
#�B
!�B
�B
�B
�B
�B
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
"�B
 �B
 �B
!�B
!�B
"�B
"�B
 �B
�B
�B
�B
�B
�B
"�B
'�B
,B
/B
49B
9XB
;dB
;dB
:^B
9XB
8RB
8RB
7LB
5?B
49B
6FB
;dB
?}B
?}B
?}B
=qB
:^B
9XB
;dB
;dB
<jB
;dB
8RB
5?B
1'B
/B
.B
/B
0!B
/B
2-B
49B
5?B
7LB
<jB
B�B
E�B
E�B
D�B
C�B
D�B
H�B
J�B
L�B
N�B
O�B
P�B
O�B
N�B
L�B
L�B
K�B
K�B
J�B
G�B
F�B
I�B
L�B
L�B
J�B
H�B
G�B
G�B
J�B
L�B
N�B
O�B
P�B
O�B
M�B
K�B
K�B
P�B
T�B
VB
S�B
P�B
N�B
N�B
P�B
P�B
P�B
R�B
S�B
R�B
S�B
S�B
S�B
S�B
S�B
R�B
P�B
N�B
N�B
O�B
R�B
T�B
XB
W
B
VB
T�B
VB
W
B
VB
T�B
S�B
S�B
R�B
R�B
R�B
R�B
T�B
W
B
ZB
]/B
^5B
_;B
_;B
^5B
^5B
^5B
_;B
_;B
^5B
]/B
\)B
[#B
[#B
[#B
[#B
YB
XB
XB
YB
YB
YB
XB
W
B
W
B
VB
VB
T�B
S�B
R�B
Q�B
Q�B
T�B
T�B
S�B
Q�B
O�B
O�B
P�B
Q�B
P�B
O�B
O�B
O�B
P�B
Q�B
R�B
VB
XB
XB
XB
W
B
W
B
T�B
R�B
Q�B
Q�B
S�B
VB
W
B
W
B
VB
VB
W
B
YB
ZB
\)B
_;B
aHB
cTB
dZB
dZB
cTB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
dZB
cTB
bNB
bNB
bNB
aHB
`BB
_;B
_;B
`BB
bNB
bNB
bNB
bNB
bNB
dZB
dZB
cTB
cTB
bNB
aHB
`BB
aHB
cTB
e`B
ffB
ffB
e`B
e`B
e`B
e`B
e`B
dZB
e`B
ffB
gmB
gmB
gmB
gmB
gmB
ffB
e`B
e`B
e`B
dZB
cTB
cTB
bNB
aHB
aHB
`BB
`BB
_;B
_;B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
^5B
^5B
]/B
^5B
_;B
`BB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
cTB
cTB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
gmB
hsB
iyB
k�B
k�B
jB
iyB
hsB
iyB
iyB
hsB
hsB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
jB
iyB
iyB
hsB
hsB
hsB
gmB
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
jB
jB
iyB
iyB
hsB
hsB
hsB
gmB
ffB
ffB
gmB
hsB
hsB
hsB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
e`B
e`B
dZB
dZB
e`B
gmB
gmB
hsB
hsB
iyB
iyB
hsB
hsB
hsB
hsB
gmB
gmB
gmB
ffB
e`B
e`B
e`B
dZB
dZB
dZB
dZB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
cTB
cTB
bNB
cTB
cTB
dZB
dZB
cTB
dZB
e`B
ffB
ffB
gmB
hsB
jB
k�B
k�B
l�B
m�B
n�B
o�B
q�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
v�B
u�B
v�B
v�B
u�B
v�B
v�B
v�B
y�B
x�B
x�B
y�B
x�B
x�B
x�B
w�B
w�B
z�B
z�B
y�B
y�B
z�B
z�B
y�B
y�B
{�B
z�B
z�B
{�B
|�B
|�B
|�B
|�B
{�B
{�B
|�B
{�B
{�B
{�B
{�B
~�B
~�B
� B
~�B
~�B
~�B
� B
~�B
~�B
~�B
~�B
~�B
� B
� B
�B
�B
� B
� B
� B
~�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�+B
�%B
�B
�B
�+B
�1B
�1B
�7B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�=B
�7B
�7B
�=B
�7B
�=B
�DB
�=B
�=B
�=B
�=B
�7B
�7B
�DB
�DB
�DB
�JB
�JB
�PB
�PB
�PB
�JB
�JB
�VB
�PB
�JB
�JB
�DB
�=B
�VB
�\B
�VB
�PB
�PB
�VB
�VB
�VB
�\B
�VB
�VB
�VB
�\B
�\B
�JB
�bB
�bB
�\B
�\B
�VB
�VB
�oB
�oB
�oB
�oB
�hB
�hB
�bB
�oB
�oB
�hB
�hB
�oB
�oB
�hB
�hB
�oB
�uB
�uB
�uB
�oB
�oB
�oB
�oB
�uB
�{B
�uB
�uB
�{B
��B
��B
��B
�{B
��B
��B
��B
�{B
�{B
�uB
�{B
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
��B
�V333333333333333333333333333333333333334434333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 B
�!B
�B
��B
r�B
m�B
k�B
iyB
hsB
ffB
e`B
cTB
cTB
bNB
aHB
aHB
`BB
^5B
[#B
W
B
R�B
O�B
K�B
H�B
D�B
@�B
:^B
1'B
#�B
{B
B	��B	�mB	�#B	��B	��B	ƨB	�
B
�B
%�B��B	Bk�B�BVB�ZBdZB�B]/BW
B�B�TB�B�B��B�;BB-B9XBE�B>wB)�BuB1BJB+BK�BjB�bB�!B��B��B��B	oB	+B	;dB	K�B	XB	^5B	cTB	cTB	bNB	_;B	`BB	m�B	}�B	�B	�7B	�PB	�{B	��B	��B	��B	�LB	��B	�/B	�B	��B
%B
�B
�B
�B
�B
%�B
&�B
"�B
�B
oB
DB
VB
�B
�B
 �B
"�B
�B
�B
�B
#�B
)�B
-B
-B
'�B
"�B
�B
�B
�B
#�B
)�B
(�B
'�B
&�B
%�B
"�B
 �B
�B
�B
�B
�B
�B
{B
oB
uB
�B
�B
�B
$�B
,B
.B
+B
&�B
#�B
$�B
&�B
'�B
+B
-B
,B
'�B
$�B
!�B
�B
 �B
$�B
-B
33B
1'B
,B
&�B
!�B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
$�B
&�B
'�B
%�B
#�B
!�B
 �B
!�B
#�B
(�B
(�B
%�B
!�B
�B
�B
�B
�B
�B
�B
{B
\B
	7B
B
B
B
B
B

=B
{B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
%�B
'�B
+B
+B
'�B
"�B
�B
�B
�B
�B
�B
�B
 �B
"�B
 �B
�B
�B
"�B
'�B
,B
0!B
1'B
49B
33B
.B
)�B
'�B
%�B
#�B
#�B
$�B
(�B
1'B
5?B
5?B
5?B
33B
0!B
/B
1'B
5?B
:^B
>wB
>wB
:^B
6FB
2-B
/B
+B
&�B
"�B
"�B
&�B
+B
0!B
0!B
-B
)�B
'�B
'�B
+B
.B
/B
/B
/B
.B
-B
-B
.B
+B
&�B
"�B
"�B
%�B
'�B
,B
1'B
5?B
8RB
8RB
8RB
9XB
9XB
9XB
7LB
49B
1'B
-B
)�B
)�B
)�B
)�B
)�B
+B
,B
)�B
&�B
#�B
!�B
�B
�B
�B
�B
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
"�B
 �B
 �B
!�B
!�B
"�B
"�B
 �B
�B
�B
�B
�B
�B
"�B
'�B
,B
/B
49B
9XB
;dB
;dB
:^B
9XB
8RB
8RB
7LB
5?B
49B
6FB
;dB
?}B
?}B
?}B
=qB
:^B
9XB
;dB
;dB
<jB
;dB
8RB
5?B
1'B
/B
.B
/B
0!B
/B
2-B
49B
5?B
7LB
<jB
B�B
E�B
E�B
D�B
C�B
D�B
H�B
J�B
L�B
N�B
O�B
P�B
O�B
N�B
L�B
L�B
K�B
K�B
J�B
G�B
F�B
I�B
L�B
L�B
J�B
H�B
G�B
G�B
J�B
L�B
N�B
O�B
P�B
O�B
M�B
K�B
K�B
P�B
T�B
VB
S�B
P�B
N�B
N�B
P�B
P�B
P�B
R�B
S�B
R�B
S�B
S�B
S�B
S�B
S�B
R�B
P�B
N�B
N�B
O�B
R�B
T�B
XB
W
B
VB
T�B
VB
W
B
VB
T�B
S�B
S�B
R�B
R�B
R�B
R�B
T�B
W
B
ZB
]/B
^5B
_;B
_;B
^5B
^5B
^5B
_;B
_;B
^5B
]/B
\)B
[#B
[#B
[#B
[#B
YB
XB
XB
YB
YB
YB
XB
W
B
W
B
VB
VB
T�B
S�B
R�B
Q�B
Q�B
T�B
T�B
S�B
Q�B
O�B
O�B
P�B
Q�B
P�B
O�B
O�B
O�B
P�B
Q�B
R�B
VB
XB
XB
XB
W
B
W
B
T�B
R�B
Q�B
Q�B
S�B
VB
W
B
W
B
VB
VB
W
B
YB
ZB
\)B
_;B
aHB
cTB
dZB
dZB
cTB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
dZB
cTB
bNB
bNB
bNB
aHB
`BB
_;B
_;B
`BB
bNB
bNB
bNB
bNB
bNB
dZB
dZB
cTB
cTB
bNB
aHB
`BB
aHB
cTB
e`B
ffB
ffB
e`B
e`B
e`B
e`B
e`B
dZB
e`B
ffB
gmB
gmB
gmB
gmB
gmB
ffB
e`B
e`B
e`B
dZB
cTB
cTB
bNB
aHB
aHB
`BB
`BB
_;B
_;B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
^5B
^5B
]/B
^5B
_;B
`BB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
cTB
cTB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
gmB
hsB
iyB
k�B
k�B
jB
iyB
hsB
iyB
iyB
hsB
hsB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
jB
iyB
iyB
hsB
hsB
hsB
gmB
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
jB
jB
iyB
iyB
hsB
hsB
hsB
gmB
ffB
ffB
gmB
hsB
hsB
hsB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
e`B
e`B
dZB
dZB
e`B
gmB
gmB
hsB
hsB
iyB
iyB
hsB
hsB
hsB
hsB
gmB
gmB
gmB
ffB
e`B
e`B
e`B
dZB
dZB
dZB
dZB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
cTB
cTB
bNB
cTB
cTB
dZB
dZB
cTB
dZB
e`B
ffB
ffB
gmB
hsB
jB
k�B
k�B
l�B
m�B
n�B
o�B
q�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
v�B
u�B
v�B
v�B
u�B
v�B
v�B
v�B
y�B
x�B
x�B
y�B
x�B
x�B
x�B
w�B
w�B
z�B
z�B
y�B
y�B
z�B
z�B
y�B
y�B
{�B
z�B
z�B
{�B
|�B
|�B
|�B
|�B
{�B
{�B
|�B
{�B
{�B
{�B
{�B
~�B
~�B
� B
~�B
~�B
~�B
� B
~�B
~�B
~�B
~�B
~�B
� B
� B
�B
�B
� B
� B
� B
~�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�+B
�%B
�B
�B
�+B
�1B
�1B
�7B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�=B
�7B
�7B
�=B
�7B
�=B
�DB
�=B
�=B
�=B
�=B
�7B
�7B
�DB
�DB
�DB
�JB
�JB
�PB
�PB
�PB
�JB
�JB
�VB
�PB
�JB
�JB
�DB
�=B
�VB
�\B
�VB
�PB
�PB
�VB
�VB
�VB
�\B
�VB
�VB
�VB
�\B
�\B
�JB
�bB
�bB
�\B
�\B
�VB
�VB
�oB
�oB
�oB
�oB
�hB
�hB
�bB
�oB
�oB
�hB
�hB
�oB
�oB
�hB
�hB
�oB
�uB
�uB
�uB
�oB
�oB
�oB
�oB
�uB
�{B
�uB
�uB
�{B
��B
��B
��B
�{B
��B
��B
��B
�{B
�{B
�uB
�{B
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
��B
�V333333333333333333333333333333333333334434333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220327100248                              AO  ARCAADJP                                                                    20220327100248    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220327100248  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220327100248  QCF$                G�O�G�O�G�O�4000            