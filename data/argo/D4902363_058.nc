CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-11-17T00:35:12Z creation;2016-11-17T00:35:15Z conversion to V3.1;2019-12-19T08:25:18Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        t  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܀   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20161117003512  20200115111517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               :A   JA  I2_0576_058                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�ڵ�$� 1   @�ڶ�� @:̿�[W?�d��,<��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�<�D΀ D�� D�  D�C3Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�ff@�ffA33A@��A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB߳3B��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}ٚC�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�;3D�~fDξfD��fD�A�D�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�A�D�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A��A��A��A��A��A��A��A��A��A��mA��`A��;A��;A��/A��#A��/A��HA�Aϧ�Aϝ�AύPA�z�A�v�A�v�A�dZA�^5A�XA�Q�A�M�A�G�A�?}A�5?A�(�A�JA���A�?}A˙�A���A�K�AŇ+A���A��yA��A�
=A��A�33A�hsA��-A�x�A���A��#A�|�A���A���A��A��hA��A��!A�G�A�S�A��A���A��A��A�JA�%A��A�I�A�l�A���A�9XA�Q�A���A�33A��/A��A�%A�1A�5?A���A��^A�1A���A��yA���A���A�p�A���A��\A�bNA�=qA���A���A�O�A�A��A�S�A�ƨA���A�z�A�\)A�C�A��A��A��wA���A���A��;A�-A��A�^5A�7LA�1'A��A�+A�`BA���A�(�A��A��A}��A|�/A{?}AxbAv��AvM�At��As�Ar1'Aq+Ap1Ao&�An~�Am�PAl�HAk|�Ai�-Ag�7Ae�#Ad�`Ad�+AdJAc
=Aa�7A_K�A^VA\��A]33A^ĜA^�RA]
=A[l�AX��AU?}AS|�AR�\AQ��APȴAP�AO/ANJAKhsAI�wAG��AF�/AF  AE�-AE
=ADJAC��AC\)AC�AB��AB=qA@ �A?�-A>��A<��A<^5A< �A;ƨA;�PA:��A9�A8�yA8=qA7�PA6�A6(�A4��A3��A3A3|�A2�A2�uA1�^A0�`A0M�A/C�A,1A*�A*n�A)��A(��A(r�A'?}A%�PA$�9A#�FA#
=A"�A!��A ��A�FA\)A��A�A��A&�A�/A��A�AA�A��AZA�HA��A �AC�Az�AƨA�yAA�A�TA?}A1'A`BA�/A��AI�A"�A
�uA	��A	oA��A9XA��A�A5?A�wAO�A�AffAG�AbA+A ��A �@��@��m@�p�@���@�J@���@���@��@�"�@���@��@�bN@�\@��^@�%@���@�R@�z�@�\@�@�  @�o@⟾@�/@ߝ�@��y@ݑh@���@��
@�;d@���@�b@��T@ա�@ա�@���@���@���@���@��@�1@�=q@˝�@���@ʇ+@�@ȼj@�  @�\)@ư!@ŉ7@��`@�I�@î@�=q@�1'@�-@�&�@��9@��@�l�@�
=@���@��@��@�V@�bN@�;d@�V@�r�@�(�@��;@�5?@��@��@�l�@�C�@��!@�9X@�o@���@�n�@��@���@��T@���@���@�33@��@�Ĝ@���@�ƨ@�C�@�@�@��@�I�@�bN@�1'@���@�z�@��@��@��
@��w@��@��D@�Q�@�+@��!@�-@���@���@�|�@�C�@�v�@�5?@�x�@��D@���@�t�@�K�@�@���@�{@��^@�`B@�/@�&�@�Ĝ@��u@�I�@��@���@�+@�l�@�S�@��y@�v�@��@��^@�O�@�Q�@�ƨ@���@�$�@���@�x�@�X@��@�9X@���@�|�@��@���@��@���@��@�7L@���@�Ĝ@��@�%@�%@��/@��j@��@���@��u@�z�@�Z@�(�@�ƨ@�t�@�;d@���@��@���@�^5@�J@���@���@��7@�`B@�G�@�&�@�%@��/@���@��u@�Z@�A�@� �@�@K�@�@~��@~�y@~ȴ@~�R@~��@~��@~�+@~ff@~$�@}�h@}p�@}`B@}/@|��@|z�@|Z@|9X@|(�@{ƨ@{��@{C�@{@z�\@z~�@z~�@z�!@zn�@y��@y��@yG�@yG�@xĜ@w�@t��@tj@t�j@t1@tz�@t��@t��@t�@tz�@tZ@t�@s"�@r�!@r��@r�\@r^5@r��@r�!@rn�@rM�@r�@qG�@p�9@p��@pbN@pb@o�@o��@o��@o|�@o+@nȴ@nv�@m�T@m��@m�h@m`B@m/@l��@l�@lz�@l�@k��@l�@l�@k�@k33@k@j��@k"�@k@j��@j~�@j^5@j�@i��@ix�@i�@h��@h��@h�9@hbN@hA�@h �@g�@g�P@f�y@f��@f$�@e�@eV@d�@d��@c��@c@b�H@c@a��@a7L@a&�@`�9@`r�@`A�@`  @_�;@_�w@_l�@_\)@_
=@^v�@]�@]`B@]�@\��@\�@[�m@["�@Z^5@Y�@ZJ@Z=q@ZM�@Z-@Y�@Y��@Y�^@Y��@Y��@X�`@X�@XA�@Xb@W�;@W;d@V5?@T�@T�@Tz�@Tz�@Tj@Tj@Tz�@T�D@S�F@SC�@SC�@S33@SC�@So@R��@RM�@Q��@Q��@QG�@P�`@P�u@P �@O�@N��@Nv�@N{@M��@M�@MO�@L��@L�@Lj@K��@KC�@K@K@J�!@J^5@JM�@J=q@J-@I�@I7L@I%@H��@H�`@H�u@H�u@Hb@G��@G�w@G��@G|�@G�@Fȴ@F��@F��@F�+@F$�@E�@Dj@C�m@Cƨ@C�F@C��@C��@C��@C��@CdZ@Co@B��@B��@BM�@A�#@A�#@A��@Ahs@AG�@@��@@�u@@�@@r�@@ �@@b@?�;@?K�@>��@>ȴ@>�R@>�R@>��@>�+@>v�@>V@=�@<��@<�@<�D@<j@<I�@<I�@;�
@;dZ@;C�@;"�@:��@:~�@:^5@:=q@:�@9�@9�#@9��@9��@9x�@9hs@9hs@97L@9%@8��@8A�@7�@7\)@7;d@6��@6��@6v�@6ff@6E�@6{@5��@5�@5/@4�j@49X@4(�@41@3��@3ƨ@3S�@2�\@2M�@1�#@1��@1��@1G�@0�@/�@/��@/;d@.�y@.��@.ff@.V@.V@.$�@-�@-��@-`B@,��@,�j@,�@,�@,j@+�m@+��@+�@*�@*��@*=q@*�@*�@*J@)��@)G�@(A�@'��@';d@&��@&ȴ@&�R@&��@&��@&��@&�+@&E�@&@%�T@%��@%O�@$��@$�@$z�@$Z@$1@#ƨ@#��@#o@"�@"��@"^5@"=q@!�^@!hs@!X@!7L@ �`@ �9@ A�@   @�@�;@�;@��@�w@�@�y@�@ȴ@��@��@v�@5?@$�@{@�-@p�@�@�/@�j@�D@I�@��@ƨ@t�@33@�@�H@��@~�@n�@^5@M�@M�@-@�@�@�^@7L@bN@b@�;@��@�@��@��@�P@|�@l�@K�@K�@;d@;d@�@�@
=@�y@ȴ@ȴ@��@�+@V@$�@$�@$�@�T@/@��@��@z�@I�@�@ƨ@�F@��@��@��@�@@��@~�@~�@^5@-@J@�@�#@��@hs@&�@%@%@��@�`@��@�9@��@��@bN@A�@�;@��@�P@|�@;d@�R@ff@V@$�@�@�-@�@�h@�h@p�@?}@?}@?}@�@�D@I�@�@�F@S�@o@
��@
��@
��@
��@
�!@
�!@
��@
~�@
^5@
^5@
^5@
^5@
n�@
^5@
^5@
-@
J@	��@	��@	�^@	�^@	hs@	�@��@�u@bN@bN@Q�@Q�@A�@1'@ �@b@  @��@�w@��@;d@�@
=@�y@�R@��@v�@ff@V@5?@5?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A��A��A��A��A��A��A��A��A��A��mA��`A��;A��;A��/A��#A��/A��HA�Aϧ�Aϝ�AύPA�z�A�v�A�v�A�dZA�^5A�XA�Q�A�M�A�G�A�?}A�5?A�(�A�JA���A�?}A˙�A���A�K�AŇ+A���A��yA��A�
=A��A�33A�hsA��-A�x�A���A��#A�|�A���A���A��A��hA��A��!A�G�A�S�A��A���A��A��A�JA�%A��A�I�A�l�A���A�9XA�Q�A���A�33A��/A��A�%A�1A�5?A���A��^A�1A���A��yA���A���A�p�A���A��\A�bNA�=qA���A���A�O�A�A��A�S�A�ƨA���A�z�A�\)A�C�A��A��A��wA���A���A��;A�-A��A�^5A�7LA�1'A��A�+A�`BA���A�(�A��A��A}��A|�/A{?}AxbAv��AvM�At��As�Ar1'Aq+Ap1Ao&�An~�Am�PAl�HAk|�Ai�-Ag�7Ae�#Ad�`Ad�+AdJAc
=Aa�7A_K�A^VA\��A]33A^ĜA^�RA]
=A[l�AX��AU?}AS|�AR�\AQ��APȴAP�AO/ANJAKhsAI�wAG��AF�/AF  AE�-AE
=ADJAC��AC\)AC�AB��AB=qA@ �A?�-A>��A<��A<^5A< �A;ƨA;�PA:��A9�A8�yA8=qA7�PA6�A6(�A4��A3��A3A3|�A2�A2�uA1�^A0�`A0M�A/C�A,1A*�A*n�A)��A(��A(r�A'?}A%�PA$�9A#�FA#
=A"�A!��A ��A�FA\)A��A�A��A&�A�/A��A�AA�A��AZA�HA��A �AC�Az�AƨA�yAA�A�TA?}A1'A`BA�/A��AI�A"�A
�uA	��A	oA��A9XA��A�A5?A�wAO�A�AffAG�AbA+A ��A �@��@��m@�p�@���@�J@���@���@��@�"�@���@��@�bN@�\@��^@�%@���@�R@�z�@�\@�@�  @�o@⟾@�/@ߝ�@��y@ݑh@���@��
@�;d@���@�b@��T@ա�@ա�@���@���@���@���@��@�1@�=q@˝�@���@ʇ+@�@ȼj@�  @�\)@ư!@ŉ7@��`@�I�@î@�=q@�1'@�-@�&�@��9@��@�l�@�
=@���@��@��@�V@�bN@�;d@�V@�r�@�(�@��;@�5?@��@��@�l�@�C�@��!@�9X@�o@���@�n�@��@���@��T@���@���@�33@��@�Ĝ@���@�ƨ@�C�@�@�@��@�I�@�bN@�1'@���@�z�@��@��@��
@��w@��@��D@�Q�@�+@��!@�-@���@���@�|�@�C�@�v�@�5?@�x�@��D@���@�t�@�K�@�@���@�{@��^@�`B@�/@�&�@�Ĝ@��u@�I�@��@���@�+@�l�@�S�@��y@�v�@��@��^@�O�@�Q�@�ƨ@���@�$�@���@�x�@�X@��@�9X@���@�|�@��@���@��@���@��@�7L@���@�Ĝ@��@�%@�%@��/@��j@��@���@��u@�z�@�Z@�(�@�ƨ@�t�@�;d@���@��@���@�^5@�J@���@���@��7@�`B@�G�@�&�@�%@��/@���@��u@�Z@�A�@� �@�@K�@�@~��@~�y@~ȴ@~�R@~��@~��@~�+@~ff@~$�@}�h@}p�@}`B@}/@|��@|z�@|Z@|9X@|(�@{ƨ@{��@{C�@{@z�\@z~�@z~�@z�!@zn�@y��@y��@yG�@yG�@xĜ@w�@t��@tj@t�j@t1@tz�@t��@t��@t�@tz�@tZ@t�@s"�@r�!@r��@r�\@r^5@r��@r�!@rn�@rM�@r�@qG�@p�9@p��@pbN@pb@o�@o��@o��@o|�@o+@nȴ@nv�@m�T@m��@m�h@m`B@m/@l��@l�@lz�@l�@k��@l�@l�@k�@k33@k@j��@k"�@k@j��@j~�@j^5@j�@i��@ix�@i�@h��@h��@h�9@hbN@hA�@h �@g�@g�P@f�y@f��@f$�@e�@eV@d�@d��@c��@c@b�H@c@a��@a7L@a&�@`�9@`r�@`A�@`  @_�;@_�w@_l�@_\)@_
=@^v�@]�@]`B@]�@\��@\�@[�m@["�@Z^5@Y�@ZJ@Z=q@ZM�@Z-@Y�@Y��@Y�^@Y��@Y��@X�`@X�@XA�@Xb@W�;@W;d@V5?@T�@T�@Tz�@Tz�@Tj@Tj@Tz�@T�D@S�F@SC�@SC�@S33@SC�@So@R��@RM�@Q��@Q��@QG�@P�`@P�u@P �@O�@N��@Nv�@N{@M��@M�@MO�@L��@L�@Lj@K��@KC�@K@K@J�!@J^5@JM�@J=q@J-@I�@I7L@I%@H��@H�`@H�u@H�u@Hb@G��@G�w@G��@G|�@G�@Fȴ@F��@F��@F�+@F$�@E�@Dj@C�m@Cƨ@C�F@C��@C��@C��@C��@CdZ@Co@B��@B��@BM�@A�#@A�#@A��@Ahs@AG�@@��@@�u@@�@@r�@@ �@@b@?�;@?K�@>��@>ȴ@>�R@>�R@>��@>�+@>v�@>V@=�@<��@<�@<�D@<j@<I�@<I�@;�
@;dZ@;C�@;"�@:��@:~�@:^5@:=q@:�@9�@9�#@9��@9��@9x�@9hs@9hs@97L@9%@8��@8A�@7�@7\)@7;d@6��@6��@6v�@6ff@6E�@6{@5��@5�@5/@4�j@49X@4(�@41@3��@3ƨ@3S�@2�\@2M�@1�#@1��@1��@1G�@0�@/�@/��@/;d@.�y@.��@.ff@.V@.V@.$�@-�@-��@-`B@,��@,�j@,�@,�@,j@+�m@+��@+�@*�@*��@*=q@*�@*�@*J@)��@)G�@(A�@'��@';d@&��@&ȴ@&�R@&��@&��@&��@&�+@&E�@&@%�T@%��@%O�@$��@$�@$z�@$Z@$1@#ƨ@#��@#o@"�@"��@"^5@"=q@!�^@!hs@!X@!7L@ �`@ �9@ A�@   @�@�;@�;@��@�w@�@�y@�@ȴ@��@��@v�@5?@$�@{@�-@p�@�@�/@�j@�D@I�@��@ƨ@t�@33@�@�H@��@~�@n�@^5@M�@M�@-@�@�@�^@7L@bN@b@�;@��@�@��@��@�P@|�@l�@K�@K�@;d@;d@�@�@
=@�y@ȴ@ȴ@��@�+@V@$�@$�@$�@�T@/@��@��@z�@I�@�@ƨ@�F@��@��@��@�@@��@~�@~�@^5@-@J@�@�#@��@hs@&�@%@%@��@�`@��@�9@��@��@bN@A�@�;@��@�P@|�@;d@�R@ff@V@$�@�@�-@�@�h@�h@p�@?}@?}@?}@�@�D@I�@�@�F@S�@o@
��@
��@
��@
��@
�!@
�!@
��@
~�@
^5@
^5@
^5@
^5@
n�@
^5@
^5@
-@
J@	��@	��@	�^@	�^@	hs@	�@��@�u@bN@bN@Q�@Q�@A�@1'@ �@b@  @��@�w@��@;d@�@
=@�y@�R@��@v�@ff@V@5?@5?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�LB�FB�3B�3B�3B�9B�3B�3B�?B�?B�9B�3B�-B�-B�!B�B�B�B��B�-B��B�yB�sB�yB�B�B�B�B�B�B�B��B�B�B�B�mB�;B�
B��BB�XB�FB�9B�B��B��B��B��B��B�hB�oB�VB�=B�7B�7B�B|�Bw�Bs�Bo�BiyB^5BW
BT�BQ�BC�B�B��B�B�B�yB�TB�BB�)B�B��BÖB�jB�-B��B�JB� B|�By�Bv�Br�Bp�B^5BQ�BD�B&�B�BVBBB
�B
��B
�}B
�'B
��B
��B
��B
��B
�hB
}�B
o�B
aHB
E�B
7LB
2-B
,B
�B
�B
hB
	7B
B	��B	��B	�B	�B	�/B	��B	�^B	�!B	��B	��B	��B	�bB	t�B	p�B	`BB	gmB	�JB	��B	��B	�+B	w�B	W
B	?}B	6FB	.B	$�B	�B	�B	PB	B��B�B�B�B�B�mB�fB�ZB�TB�NB�HB�HB��B��B��BǮBƨBƨBŢBĜBŢBĜBB�}B��B��BBB�}B�wB�qB�jB�jB�jB�RB�?B�B��B�PB��B��B��B�hB�JB�B}�B|�Bx�Bw�Bv�Bs�Bo�Bp�Br�Bm�Bq�Bw�By�B�B�B�B}�B|�B|�Bu�Bt�Bn�Bl�Bk�BiyBhsBffBdZBbNB]/B[#BVBS�BM�BJ�BG�BF�BD�BB�BA�B@�B>wB>wB>wB=qB;dB:^B7LB49B2-B0!B/B/B+B(�B%�B#�B#�B!�B!�B!�B!�B!�B$�B%�B#�B#�B!�B �B�B�B�B �B�B�B�B�B�B�B�B�B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B"�B!�B!�B!�B �B�B �B!�B!�B"�B"�B#�B#�B#�B#�B#�B$�B#�B%�B$�B$�B'�B)�B,B+B,B,B33B49B6FB8RB9XB9XB9XB>wBA�BC�BH�BI�BI�BK�BL�BL�BN�BQ�BS�BT�BVBYB[#B\)B]/B_;BaHBdZBjBs�Bw�Bx�Bz�Bz�Bx�By�B�B�B�B�B�B�7B�=B�DB�JB�\B�uB��B��B��B��B��B��B��B��B��B��B�B�3B�RB�dB�wB��BBÖBĜBȴB��B��B��B��B�
B�;B�NB�NB�`B�sB�B��B��B��B��B	  B	B		7B		7B	DB	JB	PB	PB	VB	\B	bB	hB	�B	�B	�B	�B	�B	 �B	#�B	%�B	'�B	)�B	+B	-B	.B	/B	1'B	2-B	49B	49B	7LB	8RB	9XB	;dB	?}B	@�B	A�B	A�B	B�B	B�B	B�B	C�B	C�B	D�B	F�B	N�B	O�B	P�B	S�B	VB	XB	YB	ZB	ZB	]/B	]/B	^5B	_;B	aHB	bNB	dZB	gmB	iyB	k�B	m�B	p�B	q�B	t�B	v�B	v�B	w�B	{�B	|�B	�B	�+B	�1B	�7B	�7B	�7B	�7B	�=B	�=B	�=B	�DB	�DB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�?B	�FB	�FB	�FB	�FB	�XB	�^B	�dB	�jB	�jB	�qB	�}B	��B	B	ĜB	ŢB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�/B	�5B	�BB	�BB	�HB	�HB	�NB	�TB	�TB	�ZB	�ZB	�`B	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B

=B
DB
DB
JB
JB
PB
VB
\B
hB
hB
hB
oB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
+B
+B
+B
,B
,B
,B
-B
.B
/B
/B
/B
/B
/B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
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
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
A�B
A�B
B�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
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
T�B
T�B
T�B
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
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
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
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
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
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
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
e`B
e`B
ffB
gmB
gmB
gmB
hsB
hsB
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
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�/B�fB�zB�MB�MB�MB�9B�3B�MB�?B�?B�TB�3B�GB�GB�;B�OB��B�B�B�RBӏB�B�0B�6B�B�B��B��B�B�B�9B�fB�9B��B�|B�B��B�QB��B��B�DB��B�B� B��B��B�,B�#B�B�{B�2B�.B��B��B��B�MB~BBx�Bu%BsBk�B_�BXBV�BVmBIRB_BoB�B� B�B��B��B��B��BӏB�B�B��B��B��B��B}<Bz*BwLBs�Br-B`\BT�BH�B(�BEB�BtBB
��B
ՁB
��B
�B
��B
�B
�B
�2B
�[B
�B
rB
dZB
G+B
8lB
4B
-�B
 B
�B
�B

XB
B
 4B	�B	��B	��B	ߤB	ѝB	�B	��B	�B	�sB	��B	��B	v+B	q�B	_�B	f�B	�B	��B	�B	��B	{B	YB	@�B	7fB	/OB	%�B	 BB	�B	bB	3B��B�B�B�/B�B�B�B��B�B�B�B�TB�BңB��B�KB�+B�EB�?B�B��BżB�{B��B��B��B�3B�{B��B��B�BB�"B��B��B��B�fB��B�QB�B�xB�!B��B�&B�VB�SBHB}�By�By>BxRBt�BpUBq[Bs�BncBrGBx8Bz*B��B��B�-B~�B~(B~�Bw�Bv�Bo�Bm�Bl�Bj�BiyBgBezBc�B^�B\)BW�BVBO\BK�BH�BG�BESBCaBBuBA�B?�B?.B?.B>]B<6B<B8�B5tB2�B0�B0�B1[B,�B*eB'B$�B$�B"4B"�B"�B"�B"�B&B&�B$�B$�B"�B"NB�B vB�B!|BVB�B�BQB_B$B9BB�B�B�B�B�BQB]BQB�BkBB#B5B)B=B=B�BdBjB vB!�B#nB#�B"�B# B#TB"B �B!HB"hB"NB#TB#TB$ZB$@B$ZB$�B$�B%�B%B&fB%zB&2B)*B*eB,=B+kB,�B-�B3�B4�B6�B8�B9�B9�B:DB?BB[BD�BI7BJ	BJXBL0BM6BM�BO�BR:BTFBU�BW?BZQB[qB\]B]IB_VBa-Bd@BkBt�BxRByrB{�B{�By$BzDB��B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�4B�B�B��B�CB��B��B��B��B�B�GB�B�SB�RB�B�B�B�oB��B߾B�B�B��B�B��B�B�B�$B��B	  B	B		RB		lB	^B	dB	jB	jB	�B	�B	�B	�B	�B	�B	�B	�B	�B	!B	$&B	&B	($B	*0B	+6B	-)B	./B	/OB	1vB	2|B	4nB	4�B	7fB	8�B	9�B	;�B	?�B	@�B	A�B	A�B	B�B	B�B	B�B	C�B	C�B	D�B	F�B	N�B	PB	QB	TFB	VB	XEB	Y1B	ZQB	ZQB	]dB	]~B	^jB	_pB	abB	bhB	dZB	g�B	i�B	k�B	m�B	p�B	rB	utB	wfB	v�B	w�B	|B	|�B	��B	�EB	�fB	�RB	�RB	�lB	��B	�rB	�=B	�XB	�^B	�DB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�*B	�0B	�B	�B	�=B	�)B	�)B	�/B	�/B	�5B	�'B	�aB	�tB	�`B	�zB	�zB	�FB	��B	�xB	�B	��B	��B	��B	��B	��B	��B	ĶB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�.B	�B	��B	�B	�NB	�B	�B	�B	ևB	�$B	�+B	�KB	�7B	�=B	�IB	�jB	�\B	�\B	�bB	�B	�B	�B	�B	�tB	�tB	�zB	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�0B	�JB	�JB	�JB	��B	��B	��B	��B	��B	��B	�B	�VB	�B	�B	�B
  B
 B
'B
aB
aB
-B
GB
MB
SB
tB
_B
zB
fB
	�B

�B
^B
^B
dB
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
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
 �B
!�B
#B
"�B
#�B
#�B
$B
#�B
#�B
$B
%,B
&LB
&B
'B
'B
'B
'B
(>B
(>B
)B
)*B
)*B
*0B
+B
+B
+6B
,"B
,B
,"B
-)B
./B
/5B
/B
/OB
/OB
/iB
0oB
0oB
1AB
2GB
2GB
2GB
2aB
2aB
2GB
3MB
3MB
3hB
3hB
3�B
4nB
4TB
4TB
5ZB
5ZB
5�B
5�B
6`B
6zB
7�B
7�B
7�B
7�B
8�B
9rB
:�B
:�B
;�B
;B
;dB
;B
;B
;B
;B
<�B
<�B
=�B
>�B
>�B
>�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
A�B
A�B
B�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J	B
J�B
J�B
K�B
K�B
K�B
MB
L�B
MB
L�B
M�B
NB
M�B
N�B
N�B
N�B
N�B
OB
OB
O�B
O�B
O�B
O�B
O�B
O�B
PB
Q B
Q B
QB
Q B
RB
R B
RB
SB
SB
SB
S&B
TB
T,B
TB
T�B
U2B
UB
T�B
T�B
UB
T�B
V9B
VB
VB
VB
V9B
WYB
X+B
X+B
X+B
Y1B
YB
YB
YB
YB
YB
Y1B
YB
YB
Y1B
Y1B
Y1B
Y1B
Z7B
Z7B
ZB
Z7B
ZQB
ZQB
[WB
[=B
[=B
[=B
[WB
\]B
\]B
\]B
]IB
]IB
]IB
]/B
^OB
^5B
^5B
^jB
^jB
_pB
_VB
_;B
_VB
_VB
`\B
`\B
`vB
`\B
`\B
`\B
`\B
`BB
`BB
aHB
aHB
abB
abB
aHB
abB
abB
abB
bhB
bNB
b�B
bhB
c�B
dtB
d�B
dtB
dtB
ezB
e�B
e`B
e`B
ezB
ezB
e`B
e`B
ezB
e�B
f�B
g�B
g�B
g�B
h�B
h�B
i�B
iyB
iyB
iyB
i�B
i�B
i�B
jB
j�B
j�B
jB
jB
j�B
jB
k�B
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
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201611210036052016112100360520161121003605201806221216582018062212165820180622121658201804050409552018040504095520180405040955  JA  ARFMdecpA19c                                                                20161117093504  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161117003512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161117003512  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161117003513  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161117003514  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161117003514  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161117003514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161117003514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161117003514  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161117003515                      G�O�G�O�G�O�                JA  ARUP                                                                        20161117012911                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161117153321  CV  JULD            G�O�G�O�F�կ                JM  ARCAJMQC2.0                                                                 20161120153605  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161120153605  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190955  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031658  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111517                      G�O�G�O�G�O�                