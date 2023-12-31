CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-08T21:35:11Z creation;2018-08-08T21:35:16Z conversion to V3.1;2019-12-19T07:31:38Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180808213511  20200116231515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_269                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�x3!�1   @�x4""" @4 U2a|�d`xF�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A��AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B ffB(ffB/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ D�|�D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @�ffA ��A��A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B33B��B 33B(33B/ffB7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'ٚC)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CJ�CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[ٚC]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DXvfDX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�{3DоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A݅AݑhAݕ�Aݕ�Aݛ�Aݛ�Aݝ�Aݟ�Aݟ�Aݡ�Aݣ�Aݥ�Aݣ�Aݧ�AݬAݬAݮAݮAݰ!A݋DA۶FA��/A�S�A�z�A�1A�A�jA��TA�1'A���A�A��A˟�A��mA�-A��yA�(�Aǩ�A�M�A�(�A�;dA���A��HA�9XAě�A�A�~�A�M�A�z�A���A���A�hsA��A�Q�A��\A�x�A�oA�5?A��PA��A�ȴA���A�hsA�A��FA�C�A��A�5?A�33A��\A�ȴA�+A�(�A�"�A�M�A���A�^5A�1'A���A���A� �A�p�A��TA�/A���A��
A�t�A�^5A�v�A���A��A���A�S�A�VA�`BA�z�A��A�bNA��-A�JA�+A��A���A���A�
=A���A� �A�ƨA�dZA�I�A�t�A��!A�^5A���A��A}�hAz��Ax��Av��At�!As`BAqhsAol�Am�^Ak�PAg�^Af�AeoAc�Ab��AaK�A`�A^�RA]�FA\1'AY�PAXjAW+AU�
AU7LAT�AS��AQ��AQ|�AP��AP(�ANAK�AI�-AG�;AF��AC��AA`BA>��A<�9A9oA7+A61'A5�FA5�7A5�A4�A4��A4�A2�!A1O�A/hsA/
=A.~�A-�A,(�A)��A(��A({A'
=A%�A$(�A#�A"{A ȴA�A��A�PAz�A��A�RAA�A|�A^5A�hA�A�Ap�AQ�A�^A�AE�A��A�`A+A�AbA"�Ar�AƨA��A�A
�yA	l�A��AM�A1A��AA�A�^A\)AM�AdZA �!A @���@��@�I�@���@�~�@�V@��j@�l�@�$�@��-@��7@�O�@���@�9X@�@���@�x�@�@�@�u@땁@�@���@�J@��@�j@�o@���@�ƨ@�/@ް!@�G�@�z�@�+@ڇ+@���@�J@�X@�l�@Гu@υ@��y@�=q@͡�@�Ĝ@�;d@�G�@ț�@�(�@�C�@Ə\@�p�@�Ĝ@î@��H@�M�@�-@��^@�%@���@�(�@��m@���@�~�@��h@���@�bN@� �@��
@�
=@�@�@�hs@���@�1@�S�@�5?@��-@���@�Z@��P@�dZ@�S�@�+@���@�M�@��7@�/@��@��`@��@�l�@�
=@�v�@�5?@��@���@�Ĝ@��j@��D@�(�@��
@��P@�
=@�n�@�@�@��h@�V@���@�Ĝ@�z�@�Q�@�9X@���@��@��R@�^5@�n�@�ff@�M�@�=q@��@��#@�?}@���@�z�@�z�@��@�Q�@�l�@�C�@��@��\@�M�@�@�hs@�/@�%@�Ĝ@�Q�@�  @���@���@�l�@�
=@���@�ȴ@��R@���@���@�~�@�V@��@��@��^@���@�O�@��@�j@��m@���@�;d@�o@���@��@�~�@�{@��-@��-@��7@�X@���@��j@���@�bN@���@���@��@��@���@��!@�^5@�=q@�E�@��@���@��#@�x�@�7L@���@��D@��@�j@�Z@�I�@�9X@�(�@��@��
@��@�33@��!@��+@�ff@�M�@���@��h@�p�@�`B@���@��u@�Z@�(�@� �@���@�l�@���@��R@�^5@��@���@��T@��-@��7@�O�@�/@��`@��9@��@�ƨ@���@�t�@�33@�
=@���@�v�@��@�hs@�&�@�V@���@��/@��D@�j@�I�@��@�ƨ@�|�@�l�@�dZ@�S�@�;d@�@�ȴ@��+@�E�@�$�@�J@���@��-@���@���@��7@��7@��h@��h@�`B@�%@�z�@�j@��D@��D@�Z@�I�@�I�@�A�@��@�dZ@�+@�C�@�;d@��@��\@�~�@�~�@�n�@�^5@�E�@�-@�$�@�{@�@��^@��h@�`B@�&�@���@��/@���@���@��@�I�@�P@~@}O�@|��@|Z@{ƨ@{o@zM�@y�@y��@yx�@x��@xQ�@w�w@w�P@w\)@w�P@v�R@v��@vȴ@v��@vȴ@vv�@v@u`B@uO�@u��@t�/@t�@r�!@q%@p��@pQ�@pA�@pr�@p�9@p�@pĜ@q%@q��@q��@q&�@p�`@p�9@pĜ@p��@p1'@o��@o��@o��@o\)@n��@n��@nv�@n@m`B@m�@l�/@l�j@l�j@l�@lZ@lI�@kƨ@kdZ@k"�@j��@j�!@j^5@i�#@ihs@ihs@i&�@hĜ@h�@hb@g�@gl�@g\)@g\)@gK�@g
=@fȴ@f�R@f�+@f{@e@d�@dI�@d�@c�m@c�
@cƨ@c��@c��@cS�@c@b�H@bM�@b-@a�^@aG�@a%@`��@`�9@`�@` �@_�w@_��@_;d@_�@^��@^�R@^ff@]�T@]�h@]�@]`B@]V@\��@\�@\�/@\��@[dZ@[@Zn�@Y�#@Yhs@Yhs@X�`@X��@Y�@X��@W�@Vȴ@Vff@VE�@V{@U�T@U@U�@U?}@UV@T�j@Tj@T�@S�F@St�@SS�@SS�@R�@R~�@R^5@R�@Q��@Q7L@P��@P��@Pr�@PA�@O��@O�P@O
=@Nff@M@Mp�@M/@L��@LZ@L1@K�m@K�@KdZ@KC�@KC�@J��@J^5@JM�@J�@I�#@IX@H��@HA�@G��@G�@G|�@G;d@G
=@F�@F�+@FV@F5?@E`B@D�@Dj@D�@D1@C�m@Cƨ@C��@CC�@C"�@Co@B��@B��@B^5@BM�@B-@BJ@A��@Ax�@@�`@@��@@bN@@Q�@@A�@@  @?�w@?�P@?K�@?;d@?
=@>�R@>�+@>ff@>V@>5?@>@=��@=�h@=V@<�/@<�j@<�j@<�@<��@<z�@<�@;�F@;dZ@:�@:�\@:^5@:M�@9�@9��@9x�@97L@8��@8Ĝ@81'@7�w@7|�@7\)@7;d@6ȴ@6�+@6E�@6$�@5�@5�h@5�@4��@4�/@4��@4�@4Z@3��@3ƨ@3��@3t�@3t�@3dZ@333@2�@2��@2M�@1x�@1�@0��@0�@0bN@0A�@0 �@0  @/�;@/��@/�P@/;d@.��@.�y@.��@.5?@-��@-@-�h@-p�@,�@,�j@,�D@,I�@+�m@+��@+��@+33@*�@*^5@*J@)��@)�7@)X@)�@(�`@(Ĝ@(bN@(  @'��@'�P@'�@&�+@&v�@&@%�-@%�-@%O�@$��@$�@$�j@$j@$9X@$9X@$(�@$�@$1@#ƨ@#��@#@"�@"�@"�H@"�\@"^5@"�@!�#@!��@!��@!��@!��@!�7@!x�@!G�@!%@ ��@ Ĝ@ �@ b@�;@��@;d@�@��@v�@E�@$�@{@@�@�T@��@�-@�h@O�@/@��@��@�@�/@��@z�@j@j@I�@�
@��@��@��@t�@C�@33@"�@o@�@�!@�\@M�@J@�^@��@x�@hs@G�@7L@&�@�@�`@��@�u@bN@A�@��@|�@�y@ȴ@�+@E�@{@�@��@�-@�h@�h@�@/@�@��@��@�D@z�@I�@1@�m@�F@�@dZ@33@@�@��@�\@~�@=q@=q@��@�^@��@x�@X@&�@&�@�@��@Ĝ@�u@bN@A�@1'@1'@1'@ �@ �@ �@  @�w@�P@l�@\)@K�@�@��@ȴ@��@��@ff@5?@{@�T@@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A݅AݑhAݕ�Aݕ�Aݛ�Aݛ�Aݝ�Aݟ�Aݟ�Aݡ�Aݣ�Aݥ�Aݣ�Aݧ�AݬAݬAݮAݮAݰ!A݋DA۶FA��/A�S�A�z�A�1A�A�jA��TA�1'A���A�A��A˟�A��mA�-A��yA�(�Aǩ�A�M�A�(�A�;dA���A��HA�9XAě�A�A�~�A�M�A�z�A���A���A�hsA��A�Q�A��\A�x�A�oA�5?A��PA��A�ȴA���A�hsA�A��FA�C�A��A�5?A�33A��\A�ȴA�+A�(�A�"�A�M�A���A�^5A�1'A���A���A� �A�p�A��TA�/A���A��
A�t�A�^5A�v�A���A��A���A�S�A�VA�`BA�z�A��A�bNA��-A�JA�+A��A���A���A�
=A���A� �A�ƨA�dZA�I�A�t�A��!A�^5A���A��A}�hAz��Ax��Av��At�!As`BAqhsAol�Am�^Ak�PAg�^Af�AeoAc�Ab��AaK�A`�A^�RA]�FA\1'AY�PAXjAW+AU�
AU7LAT�AS��AQ��AQ|�AP��AP(�ANAK�AI�-AG�;AF��AC��AA`BA>��A<�9A9oA7+A61'A5�FA5�7A5�A4�A4��A4�A2�!A1O�A/hsA/
=A.~�A-�A,(�A)��A(��A({A'
=A%�A$(�A#�A"{A ȴA�A��A�PAz�A��A�RAA�A|�A^5A�hA�A�Ap�AQ�A�^A�AE�A��A�`A+A�AbA"�Ar�AƨA��A�A
�yA	l�A��AM�A1A��AA�A�^A\)AM�AdZA �!A @���@��@�I�@���@�~�@�V@��j@�l�@�$�@��-@��7@�O�@���@�9X@�@���@�x�@�@�@�u@땁@�@���@�J@��@�j@�o@���@�ƨ@�/@ް!@�G�@�z�@�+@ڇ+@���@�J@�X@�l�@Гu@υ@��y@�=q@͡�@�Ĝ@�;d@�G�@ț�@�(�@�C�@Ə\@�p�@�Ĝ@î@��H@�M�@�-@��^@�%@���@�(�@��m@���@�~�@��h@���@�bN@� �@��
@�
=@�@�@�hs@���@�1@�S�@�5?@��-@���@�Z@��P@�dZ@�S�@�+@���@�M�@��7@�/@��@��`@��@�l�@�
=@�v�@�5?@��@���@�Ĝ@��j@��D@�(�@��
@��P@�
=@�n�@�@�@��h@�V@���@�Ĝ@�z�@�Q�@�9X@���@��@��R@�^5@�n�@�ff@�M�@�=q@��@��#@�?}@���@�z�@�z�@��@�Q�@�l�@�C�@��@��\@�M�@�@�hs@�/@�%@�Ĝ@�Q�@�  @���@���@�l�@�
=@���@�ȴ@��R@���@���@�~�@�V@��@��@��^@���@�O�@��@�j@��m@���@�;d@�o@���@��@�~�@�{@��-@��-@��7@�X@���@��j@���@�bN@���@���@��@��@���@��!@�^5@�=q@�E�@��@���@��#@�x�@�7L@���@��D@��@�j@�Z@�I�@�9X@�(�@��@��
@��@�33@��!@��+@�ff@�M�@���@��h@�p�@�`B@���@��u@�Z@�(�@� �@���@�l�@���@��R@�^5@��@���@��T@��-@��7@�O�@�/@��`@��9@��@�ƨ@���@�t�@�33@�
=@���@�v�@��@�hs@�&�@�V@���@��/@��D@�j@�I�@��@�ƨ@�|�@�l�@�dZ@�S�@�;d@�@�ȴ@��+@�E�@�$�@�J@���@��-@���@���@��7@��7@��h@��h@�`B@�%@�z�@�j@��D@��D@�Z@�I�@�I�@�A�@��@�dZ@�+@�C�@�;d@��@��\@�~�@�~�@�n�@�^5@�E�@�-@�$�@�{@�@��^@��h@�`B@�&�@���@��/@���@���@��@�I�@�P@~@}O�@|��@|Z@{ƨ@{o@zM�@y�@y��@yx�@x��@xQ�@w�w@w�P@w\)@w�P@v�R@v��@vȴ@v��@vȴ@vv�@v@u`B@uO�@u��@t�/@t�@r�!@q%@p��@pQ�@pA�@pr�@p�9@p�@pĜ@q%@q��@q��@q&�@p�`@p�9@pĜ@p��@p1'@o��@o��@o��@o\)@n��@n��@nv�@n@m`B@m�@l�/@l�j@l�j@l�@lZ@lI�@kƨ@kdZ@k"�@j��@j�!@j^5@i�#@ihs@ihs@i&�@hĜ@h�@hb@g�@gl�@g\)@g\)@gK�@g
=@fȴ@f�R@f�+@f{@e@d�@dI�@d�@c�m@c�
@cƨ@c��@c��@cS�@c@b�H@bM�@b-@a�^@aG�@a%@`��@`�9@`�@` �@_�w@_��@_;d@_�@^��@^�R@^ff@]�T@]�h@]�@]`B@]V@\��@\�@\�/@\��@[dZ@[@Zn�@Y�#@Yhs@Yhs@X�`@X��@Y�@X��@W�@Vȴ@Vff@VE�@V{@U�T@U@U�@U?}@UV@T�j@Tj@T�@S�F@St�@SS�@SS�@R�@R~�@R^5@R�@Q��@Q7L@P��@P��@Pr�@PA�@O��@O�P@O
=@Nff@M@Mp�@M/@L��@LZ@L1@K�m@K�@KdZ@KC�@KC�@J��@J^5@JM�@J�@I�#@IX@H��@HA�@G��@G�@G|�@G;d@G
=@F�@F�+@FV@F5?@E`B@D�@Dj@D�@D1@C�m@Cƨ@C��@CC�@C"�@Co@B��@B��@B^5@BM�@B-@BJ@A��@Ax�@@�`@@��@@bN@@Q�@@A�@@  @?�w@?�P@?K�@?;d@?
=@>�R@>�+@>ff@>V@>5?@>@=��@=�h@=V@<�/@<�j@<�j@<�@<��@<z�@<�@;�F@;dZ@:�@:�\@:^5@:M�@9�@9��@9x�@97L@8��@8Ĝ@81'@7�w@7|�@7\)@7;d@6ȴ@6�+@6E�@6$�@5�@5�h@5�@4��@4�/@4��@4�@4Z@3��@3ƨ@3��@3t�@3t�@3dZ@333@2�@2��@2M�@1x�@1�@0��@0�@0bN@0A�@0 �@0  @/�;@/��@/�P@/;d@.��@.�y@.��@.5?@-��@-@-�h@-p�@,�@,�j@,�D@,I�@+�m@+��@+��@+33@*�@*^5@*J@)��@)�7@)X@)�@(�`@(Ĝ@(bN@(  @'��@'�P@'�@&�+@&v�@&@%�-@%�-@%O�@$��@$�@$�j@$j@$9X@$9X@$(�@$�@$1@#ƨ@#��@#@"�@"�@"�H@"�\@"^5@"�@!�#@!��@!��@!��@!��@!�7@!x�@!G�@!%@ ��@ Ĝ@ �@ b@�;@��@;d@�@��@v�@E�@$�@{@@�@�T@��@�-@�h@O�@/@��@��@�@�/@��@z�@j@j@I�@�
@��@��@��@t�@C�@33@"�@o@�@�!@�\@M�@J@�^@��@x�@hs@G�@7L@&�@�@�`@��@�u@bN@A�@��@|�@�y@ȴ@�+@E�@{@�@��@�-@�h@�h@�@/@�@��@��@�D@z�@I�@1@�m@�F@�@dZ@33@@�@��@�\@~�@=q@=q@��@�^@��@x�@X@&�@&�@�@��@Ĝ@�u@bN@A�@1'@1'@1'@ �@ �@ �@  @�w@�P@l�@\)@K�@�@��@ȴ@��@��@ff@5?@{@�T@@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
(�B
+B
+B
,B
,B
,B
-B
-B
-B
-B
-B
-B
.B
-B
-B
-B
,B
)�B
"�B
+B	�B	��B
H�B
e`B
e`B
�B
��B
��B
�RB
�'B
ǮB
�B\B�B#�BW
Bs�B��B��B��B��B�B��B��B��B�jB�dB��BBPB1'B.B#�B-B+B;dB.B)�B �B�B33BG�BS�BXBS�BP�BH�B>wBK�BI�BZBW
Bm�B{�B�VB�Bt�BYBC�BJ�B$�B%�B{B�TB�sB�B�B��B��B��Bm�BR�BH�BA�B9XB9XB'�BhB
�HB
�B
�/B
�yB
�sB
�B
�^B
�'B
��B
� B
� B
q�B
O�B
?}B
�B
�B
�B	��B	�B	�B	�NB	�ZB	ɺB	��B	�B	��B	~�B	�JB	�PB	~�B	{�B	q�B	gmB	bNB	T�B	F�B	&�B	9XB	33B	.B	1'B	.B	 �B	bB	�B	{B	B�B�#B�B��B��B�B�'B��B��B�PB��B�B�B�3B�B�B�B��B�oB�hB� B��B�oB�Bx�Be`B}�Bu�Bp�BdZBaHBhsB]/BZBXB^5B\)BYB`BB\)BbNB^5BVBYBZB[#B_;BW
BXBXB]/BYBVBG�B[#BZBVB[#B]/Be`BaHBVBC�BD�BD�B]/BP�BVB\)BYBP�BP�BW
BT�BR�B]/B_;B`BB]/B^5BgmBaHBbNBl�Bn�Bl�BjBffBdZBbNB]/BZB[#BffBjBn�Bp�Bm�Bk�Bl�Be`BdZBdZB`BBcTBm�Bp�Bo�Bp�BgmBl�Bw�Bp�Bk�B� B�%B�+B�1B�1B�1B�B�hB�{B�hB��B�oB��B��B��B��B�B��B��B�B�B�B�B��B�B�FB�RB�wB�wB�jB�qBȴBǮBŢBɺB��B��B�B�B�/B�NB�B�B�B�B�B�B��B��B��B��B��B	B	%B	JB	JB	JB	VB	�B	�B	�B	�B	 �B	 �B	%�B	+B	/B	1'B	1'B	6FB	=qB	E�B	J�B	M�B	N�B	P�B	O�B	T�B	_;B	`BB	aHB	bNB	aHB	`BB	_;B	e`B	m�B	o�B	p�B	m�B	hsB	q�B	s�B	w�B	y�B	z�B	z�B	�B	�B	�B	�B	�1B	�=B	�PB	�PB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�B	�'B	�3B	�LB	�LB	�RB	�RB	�^B	�dB	�dB	�^B	�wB	��B	��B	�}B	ÖB	ƨB	ƨB	ɺB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�
B	�B	��B	��B	�B	�
B	�B	�
B	��B	�B	�B	�B	�B	�/B	�/B	�)B	�/B	�)B	�/B	�)B	�#B	�#B	�5B	�HB	�NB	�BB	�HB	�;B	�HB	�5B	�HB	�`B	�sB	�sB	�mB	�mB	�sB	�yB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B	��B
  B	��B
  B
B
B
1B
%B

=B
DB
PB
PB
JB
PB
JB
bB
hB
\B
JB

=B

=B
bB
oB
{B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
#�B
#�B
"�B
#�B
"�B
#�B
$�B
$�B
%�B
$�B
#�B
$�B
&�B
%�B
$�B
%�B
%�B
&�B
(�B
)�B
(�B
(�B
'�B
'�B
(�B
'�B
%�B
%�B
$�B
&�B
+B
+B
,B
,B
,B
,B
,B
+B
,B
)�B
,B
+B
,B
-B
/B
-B
-B
-B
-B
.B
.B
/B
0!B
/B
/B
/B
0!B
2-B
2-B
1'B
2-B
2-B
1'B
/B
,B
/B
0!B
0!B
1'B
33B
2-B
33B
49B
1'B
.B
0!B
33B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
6FB
6FB
7LB
7LB
7LB
8RB
9XB
7LB
7LB
8RB
8RB
7LB
9XB
9XB
;dB
;dB
:^B
9XB
:^B
9XB
9XB
9XB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
>wB
>wB
@�B
?}B
>wB
=qB
>wB
>wB
?}B
B�B
B�B
A�B
B�B
B�B
B�B
B�B
B�B
@�B
A�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
F�B
G�B
G�B
H�B
H�B
G�B
G�B
F�B
E�B
H�B
H�B
I�B
I�B
I�B
H�B
I�B
I�B
J�B
J�B
I�B
J�B
K�B
K�B
K�B
J�B
J�B
J�B
J�B
L�B
L�B
M�B
L�B
L�B
L�B
J�B
K�B
K�B
K�B
L�B
N�B
N�B
M�B
M�B
N�B
N�B
N�B
N�B
M�B
N�B
O�B
P�B
P�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
P�B
R�B
S�B
S�B
R�B
Q�B
Q�B
S�B
T�B
T�B
T�B
T�B
S�B
S�B
S�B
Q�B
Q�B
T�B
VB
W
B
XB
XB
XB
XB
XB
W
B
YB
XB
XB
YB
XB
XB
XB
ZB
YB
YB
XB
ZB
ZB
YB
ZB
ZB
[#B
ZB
[#B
ZB
[#B
]/B
]/B
]/B
]/B
]/B
]/B
\)B
]/B
\)B
^5B
\)B
\)B
_;B
^5B
^5B
`BB
_;B
`BB
aHB
aHB
`BB
bNB
bNB
bNB
bNB
aHB
aHB
aHB
_;B
bNB
cTB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
e`B
dZB
dZB
cTB
cTB
dZB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
hsB
iyB
iyB
iyB
gmB
iyB
jB
jB
jB
jB
jB
k�B
jB
jB
iyB
jB
jB
jB
jB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
k�B
l�B
jB
k�B
jB
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
o�B
n�B
o�B
p�B
p�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
p�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
w�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
(�B
+B
+B
,B
,B
,B
-B
-B
-B
-B
-B
-B
.B
-B
-B
-)B
,=B
*�B
$�B
~B	��B
aB
J=B
ffB
g�B
�MB
��B
�4B
�	B
��B
�=B
��B�BdB&�BXyBt�B�B�fB��B�HB�yB�jB�bB�B�B� B�8B�B(B1vB/ B%�B.�B-CB<PB0B+�B"�B�B4�BH�BT�BX�BT�BQ�BJrB@�BM6BK�B[�BYKBo�B}�B�(B�Bw�B]�BF�BLJB($B'�B�B�
B�B�B��B��B��B�OBrBVmBKBC�B;JB:xB)�B�B
�fB
��B
��B
��B
�yB
��B
�<B
�hB
��B
�GB
��B
tB
T,B
C-B
�B
!|B
+B
;B	�;B	��B	�B	��B	�JB	��B	��B	��B	�GB	�VB	��B	��B	}"B	shB	i*B	dB	V�B	H�B	)�B	:�B	4�B	/�B	1�B	.�B	"hB	�B	)B	�B	�B�}B�5BڠB�VB��B�'B�B�-B��B��B��B�=B��B��B��B��B��B�2B��B�&B�AB�+B�[B��Bz�Bh$B~�Bw2Br-BffBcBi�B_;B\BY�B_�B]�BZ�BabB]~Bc:B_pBW�BZQB[WB\]B`'BXyBY1BYKB^BZ7BWYBI�B[�B[#BWYB\B^Be�Ba�BWYBE�BF�BF�B]�BRoBW
B\�BZBRTBR:BX+BVBTFB]�B_�B`�B^5B_!Bg�BbNBc:Bl�Bn�Bl�Bj�BgBeBcB^OB[�B\xBg8Bk6Bo Bp�Bn/BlWBm)Bf�Be�Be�BbBd�Bn}Bq[Bp�BqvBiDBm�Bx�Br-Bm�B��B��B��B��B�B�RB�SB��B��B� B�B�[B�1B�qB�\B�LB�)B�B�yB�wB��B�iB�}B��B��B��B��B��B��B�"B�(B��B�1B�?B�=B�~B˒BևBخBݲB��B��B�B��B��B�B�B��B�B�*B�fB�qB	gB	�B	~B	�B	�B	�B	�B	�B	�B	B	!B	!bB	&LB	+QB	/OB	1[B	1�B	6zB	=�B	E�B	J�B	NB	O(B	QhB	P�B	U2B	_;B	`\B	abB	b�B	a�B	`�B	_�B	e�B	m�B	o�B	p�B	m�B	iB	q�B	tB	xB	zB	{0B	{JB	�AB	�aB	�{B	�aB	��B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�:B	�>B	�KB	�OB	�;B	�UB	��B	��B	��B	�LB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	�	B	��B	�0B	�B	��B	� B	� B	�B	�B	�B	�B	�B	�4B	�\B	�<B	�B	�9B	�9B	�MB	�gB	�$B	�1B	ՁB	�2B	�9B	�$B	�+B	�?B	�gB	�mB	�eB	�yB	�kB	�IB	�IB	�]B	�IB	�]B	�IB	�]B	�qB	یB	ބB	�bB	�hB	�vB	�|B	ߊB	�B	ޞB	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�+B	��B	��B	�(B	�JB	�"B	�B
  B
 B
 4B
AB
-B
-B
GB
'B
;B
 B
;B
;B
 B
;B
UB
B
 B	�]B	�dB	��B	�<B	�<B	�B	�BB	�<B	�.B
 B
AB	�.B
 4B	�.B
 4B
-B
9B
KB
tB

=B
DB
PB
�B
~B
�B
~B
bB
hB
�B
�B

�B

�B
}B
�B
{B
�B
�B
�B
�B
�B
�B
�B
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
B
�B
�B
�B
B
!�B
"�B
$B
#�B
#�B
"�B
#�B
#B
$B
$�B
%B
%�B
%,B
$&B
%B
'B
%�B
%B
%�B
&B
'B
)*B
)�B
)B
)B
(
B
(
B
(�B
(
B
&B
&B
%,B
'B
+B
+B
,"B
,"B
,"B
,"B
,"B
+B
,"B
*0B
,=B
+6B
,=B
-)B
/B
-)B
-)B
-CB
-)B
./B
.IB
/OB
0UB
/5B
/OB
/iB
0;B
2GB
2aB
1AB
2-B
2GB
1[B
/OB
,qB
/iB
0oB
0oB
1[B
3MB
2|B
33B
4TB
1vB
.}B
0oB
3hB
5ZB
6`B
6`B
6`B
6zB
6`B
7�B
6`B
6zB
7fB
7�B
7fB
8�B
9XB
7�B
7�B
8lB
8lB
7�B
9�B
9rB
;�B
;�B
:xB
9�B
:xB
9�B
9�B
9�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
?�B
?�B
?�B
>�B
>�B
@�B
?�B
>�B
=�B
>�B
>�B
?�B
B�B
B�B
A�B
B�B
B�B
B�B
B�B
B�B
@�B
A�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
F�B
G�B
G�B
H�B
H�B
G�B
G�B
F�B
E�B
H�B
H�B
I�B
I�B
I�B
H�B
I�B
I�B
J�B
J�B
I�B
J�B
K�B
K�B
K�B
J�B
J�B
J�B
KB
L�B
L�B
M�B
L�B
L�B
L�B
KB
LB
K�B
LB
L�B
N�B
N�B
NB
M�B
OB
N�B
OB
N�B
N"B
OB
O�B
Q B
QB
PB
Q B
RB
RB
R B
R:B
QB
S&B
T,B
TB
S&B
RB
R:B
T,B
UB
U2B
UB
T�B
TB
TB
TB
R:B
RTB
UB
VB
W$B
X+B
X+B
XEB
X+B
X+B
W$B
Y1B
X+B
X+B
Y1B
XEB
XEB
XEB
ZB
Y1B
Y1B
X_B
Z7B
Z7B
Y1B
Z7B
Z7B
[WB
ZQB
[=B
ZkB
[WB
]IB
]dB
]dB
]IB
]IB
]IB
\xB
]dB
\]B
^5B
\]B
\]B
_;B
^�B
^jB
`vB
_�B
`\B
aHB
a|B
`\B
bNB
bNB
bNB
bNB
abB
a|B
abB
_pB
bhB
cTB
bhB
b�B
bhB
bhB
cnB
dZB
dtB
dZB
ezB
d�B
dZB
cnB
cnB
dtB
cnB
cnB
c�B
dtB
dtB
d�B
dtB
f�B
f�B
g�B
g�B
hsB
h�B
hsB
h�B
hsB
g�B
g�B
g�B
h�B
h�B
iyB
iyB
iyB
i�B
h�B
i�B
iyB
i�B
g�B
i�B
jB
jB
j�B
j�B
j�B
k�B
j�B
j�B
i�B
j�B
j�B
j�B
j�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
k�B
l�B
j�B
k�B
j�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
o�B
n�B
o�B
p�B
p�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
p�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
w�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<7�4<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808130035572018081300355720180813003557201808130200242018081302002420180813020024201808140028372018081400283720180814002837  JA  ARFMdecpA19c                                                                20180809063510  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180808213511  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180808213514  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180808213514  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180808213515  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180808213515  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180808213515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180808213515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180808213515  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180808213516                      G�O�G�O�G�O�                JA  ARUP                                                                        20180808215546                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180809153514  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20180812153557  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180812153557  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180812170024  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180813152837  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231515                      G�O�G�O�G�O�                