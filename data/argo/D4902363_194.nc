CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-12-30T00:35:17Z creation;2017-12-30T00:35:21Z conversion to V3.1;2019-12-19T07:53:02Z update;     
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
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �pArgo profile    3.1 1.2 19500101000000  20171230003517  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_194                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�@�܊ 1   @�@���>�@;g��rG�dV�7��41   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ D�|�Dü�D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݃3D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)ٚC+�3C-�3C0�C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��DvfD��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DCvfDC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�{3Dû3D��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD݁�DݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�E�A�C�A�E�A�C�A�?}A�;dA�9XA�7LA�7LA�9XA�A�A�?}A�/A�-A�+A�"�A�&�A�-A��yA�ĜA���A��A�t�A�hsA�dZA�^5A�\)A�ZA�XA�XA�ZA�XA�Q�A�Q�A�O�A�G�A�?}A�-A�%A��A�ȴA���A�n�A�E�A���A���A�ĜA��yA�x�A��FA�dZA��-A��-A�^5A�ȴA�`BA�-A��9A�^5A�~�A��mA�ĜA�A�%A��\A� �A��A�9XA��wA�$�A�z�A�I�A�A�A�33A��A���A�~�A&�A~M�A}�7A|��A|ffA{�^AzVAx1'Av�DAu��Au�-Au��At�RAs%Ar��ArbNAqt�Ap��ApA�ApJAn�Anr�Al�Aj1Ai\)Ah�`AhQ�Ah1'AhJAg�^Agt�Ag&�Afz�Ae��AdA�AaS�A_�-A]VA[A[��A[��A[p�AZ��AY��AWK�AV�ATz�AS��ASAR~�AR5?AQ�7APffAO�ANjAM;dAL9XAJE�AI�AIt�AI&�AH�\AG��AGdZAF��AF �AE?}ADr�ADA�AC��AC7LAB�AB�AB�AB��AB�DABffAB-AAƨA@��A?x�A>��A=�A=�hA=7LA<�A<bNA<=qA9�TA7�mA7%A5A5"�A49XA37LA2��A1l�A0A/x�A/?}A.��A-��A,�!A+�mA+�A*ȴA*�9A*�uA*r�A*  A)A&�9A$��A#�A#O�A#;dA"��A!�A!hsA ��A ~�A E�A �A�mA��Ax�AoA�\AbNAE�A�#A�A�A��A33A�A�!AE�A�A�A�DAA�A�9AbNA�AƨA&�An�A��A�wA�AhsA?}A��A1'A
�/A	�TA��AQ�A�AAv�A��A(�A�-A =q@�dZ@�@��+@��@�p�@�%@��@�Z@��@��;@�\)@��H@�@��@�r�@��F@�o@�=q@��`@�t�@�X@��;@�7@�"�@�I�@�\@�t�@�
=@���@� �@���@�V@ݲ-@���@��;@ۅ@�\)@ڰ!@ٲ-@ج@�I�@׮@��@�n�@պ^@ԛ�@� �@�K�@ѡ�@���@��@͙�@�Ĝ@ʸR@��@���@�X@�z�@�1'@�(�@� �@ǶF@��y@�n�@�=q@��@�&�@Ĭ@� �@�dZ@�"�@�ȴ@��@�/@���@�1'@�E�@�Z@�ȴ@�J@�`B@�  @���@���@��y@�v�@��@��@���@���@��^@���@��F@���@��@���@�z�@��
@���@�V@��@���@�p�@�Q�@���@��@��+@�^5@���@�x�@��@��@��u@�I�@���@�dZ@���@��+@�V@�@���@�I�@�(�@�b@���@��P@�C�@��@�M�@�@��7@��@�Ĝ@��D@�I�@��m@�t�@�
=@���@�~�@�v�@�n�@�ff@�$�@���@���@�p�@�?}@���@��@�$�@�X@���@�b@��F@�ȴ@�V@��h@��/@���@�z�@�bN@�Q�@�1@�K�@�n�@�{@���@�x�@�G�@�%@��9@�j@���@�ƨ@��w@�t�@�C�@���@��\@��@��h@�?}@��@���@�r�@�1'@���@��w@���@��P@�l�@�"�@�n�@���@��@���@��@��@�r�@�bN@�A�@� �@�;@K�@~�+@~E�@~$�@}�-@}�@}p�@}O�@}�@}V@|��@|Z@{�m@{t�@z��@y�@y7L@xbN@x�u@x��@xA�@x �@w�;@v�R@u�@u��@t�@tj@t(�@s��@s�F@s��@s33@r�H@qhs@p��@p�`@p�u@pb@o�;@o|�@n��@m�-@mV@l�j@l�D@lj@l�D@l�D@lj@lI�@k�
@kƨ@kƨ@kƨ@kƨ@k��@kt�@kS�@kS�@kC�@ko@ko@k33@k33@kC�@k33@k"�@k"�@k"�@k"�@ko@j�@k@k@k@ko@k@k@j��@jn�@j=q@hĜ@h �@g\)@f�R@f�y@f5?@e@e�T@e`B@d�@d��@d�@c�F@c�@co@b=q@a�@a�7@aG�@a�@a%@`�9@`A�@` �@` �@`b@`  @_�@_��@_
=@^�+@]��@]O�@]V@\�@\j@\1@[�
@[�F@[��@[��@[�@[33@Z��@Z=q@Yhs@YG�@Y&�@X�`@XQ�@W+@V�R@V�+@Vff@VV@VE�@V@U��@T�@T��@T�@TZ@S�m@S��@S�@SdZ@SC�@R�H@R��@R�\@R^5@R-@RJ@Q��@Q�7@QG�@Q%@PĜ@Pr�@O�;@O�@N��@NE�@M@M`B@M/@L9X@K�m@Kƨ@Kƨ@KC�@J��@Jn�@JM�@J-@J�@I��@I�^@Ihs@IG�@I7L@I7L@I%@H�9@Hr�@H  @Gl�@G;d@G�@F��@F�R@F$�@E�@E�@D�/@D��@DZ@D1@Cƨ@C��@Ct�@Co@B�@B~�@B-@BJ@BJ@A��@A�#@A�#@A��@AG�@A%@@�u@@�@@r�@?�;@?�w@?|�@>ȴ@=@=O�@=/@=/@=�@<��@<�j@<j@<9X@<1@;�m@;�m@;ƨ@;��@;�@;t�@;dZ@;dZ@;dZ@;dZ@;C�@:��@9��@9�#@9��@9x�@9�@8��@8Ĝ@8�9@8�u@8r�@8r�@8bN@8  @7��@7l�@7;d@7+@6�+@6$�@6@6@6@6@5�@5@5�h@5�@5�@5�@5�@5�@5`B@5O�@5/@5V@4�/@4�j@4�D@4�@3��@3"�@3@3@2�H@2��@2��@2��@2�\@2�@1%@0r�@0b@/�;@/�@/�P@/�@.�R@.E�@.{@-��@-�-@-�-@-p�@-O�@-/@,��@,�/@,��@+�
@+"�@*��@*~�@*n�@*=q@*J@)�@)�^@)x�@)X@)G�@)%@(�@( �@'�;@'�@'|�@'\)@'
=@&v�@&v�@&ff@&V@&5?@%��@%�@$��@$�@$��@$j@$(�@$1@#�F@#t�@#dZ@#S�@#"�@"�H@"��@"�\@"�@!��@!�@!�#@!��@!��@!7L@ ��@ �9@ �@  �@�P@�@ȴ@�+@ff@V@E�@$�@p�@��@z�@(�@(�@�@1@��@��@�@��@�\@n�@J@�#@��@�^@��@�7@hs@7L@&�@��@�u@ �@�;@�@�P@l�@+@��@�+@V@�T@p�@/@�@�/@��@�j@�j@�j@�j@�@��@�D@�D@z�@Z@9X@9X@9X@(�@1@1@1@ƨ@ƨ@��@��@C�@�@��@n�@=q@�@�@��@��@x�@X@�`@�u@r�@r�@Q�@ �@  @��@\)@+@�y@ȴ@�+@V@5?@@@��@�@�@O�@/@�@��@�@�j@�@��@�D@Z@9X@1@��@�@t�@dZ@S�@33@@
�@
��@
~�@
n�@
�@	��@	�@	��@	x�@	hs@	hs@	hs@	hs@	hs@	X@	7L@	7L@	7L@	&�@	%@��@��@��@��@bN@A�@ �@  @  @�P@+@�@�@�R@��@E�@�@��@�@?}@�@�/@�j@z�@9X@(�@1@��@�F@S�@�@�\@n�@n�@^5@M�@M�@-@��@�#@�^@�7@hs@G�@ �`@ Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�E�A�C�A�E�A�C�A�?}A�;dA�9XA�7LA�7LA�9XA�A�A�?}A�/A�-A�+A�"�A�&�A�-A��yA�ĜA���A��A�t�A�hsA�dZA�^5A�\)A�ZA�XA�XA�ZA�XA�Q�A�Q�A�O�A�G�A�?}A�-A�%A��A�ȴA���A�n�A�E�A���A���A�ĜA��yA�x�A��FA�dZA��-A��-A�^5A�ȴA�`BA�-A��9A�^5A�~�A��mA�ĜA�A�%A��\A� �A��A�9XA��wA�$�A�z�A�I�A�A�A�33A��A���A�~�A&�A~M�A}�7A|��A|ffA{�^AzVAx1'Av�DAu��Au�-Au��At�RAs%Ar��ArbNAqt�Ap��ApA�ApJAn�Anr�Al�Aj1Ai\)Ah�`AhQ�Ah1'AhJAg�^Agt�Ag&�Afz�Ae��AdA�AaS�A_�-A]VA[A[��A[��A[p�AZ��AY��AWK�AV�ATz�AS��ASAR~�AR5?AQ�7APffAO�ANjAM;dAL9XAJE�AI�AIt�AI&�AH�\AG��AGdZAF��AF �AE?}ADr�ADA�AC��AC7LAB�AB�AB�AB��AB�DABffAB-AAƨA@��A?x�A>��A=�A=�hA=7LA<�A<bNA<=qA9�TA7�mA7%A5A5"�A49XA37LA2��A1l�A0A/x�A/?}A.��A-��A,�!A+�mA+�A*ȴA*�9A*�uA*r�A*  A)A&�9A$��A#�A#O�A#;dA"��A!�A!hsA ��A ~�A E�A �A�mA��Ax�AoA�\AbNAE�A�#A�A�A��A33A�A�!AE�A�A�A�DAA�A�9AbNA�AƨA&�An�A��A�wA�AhsA?}A��A1'A
�/A	�TA��AQ�A�AAv�A��A(�A�-A =q@�dZ@�@��+@��@�p�@�%@��@�Z@��@��;@�\)@��H@�@��@�r�@��F@�o@�=q@��`@�t�@�X@��;@�7@�"�@�I�@�\@�t�@�
=@���@� �@���@�V@ݲ-@���@��;@ۅ@�\)@ڰ!@ٲ-@ج@�I�@׮@��@�n�@պ^@ԛ�@� �@�K�@ѡ�@���@��@͙�@�Ĝ@ʸR@��@���@�X@�z�@�1'@�(�@� �@ǶF@��y@�n�@�=q@��@�&�@Ĭ@� �@�dZ@�"�@�ȴ@��@�/@���@�1'@�E�@�Z@�ȴ@�J@�`B@�  @���@���@��y@�v�@��@��@���@���@��^@���@��F@���@��@���@�z�@��
@���@�V@��@���@�p�@�Q�@���@��@��+@�^5@���@�x�@��@��@��u@�I�@���@�dZ@���@��+@�V@�@���@�I�@�(�@�b@���@��P@�C�@��@�M�@�@��7@��@�Ĝ@��D@�I�@��m@�t�@�
=@���@�~�@�v�@�n�@�ff@�$�@���@���@�p�@�?}@���@��@�$�@�X@���@�b@��F@�ȴ@�V@��h@��/@���@�z�@�bN@�Q�@�1@�K�@�n�@�{@���@�x�@�G�@�%@��9@�j@���@�ƨ@��w@�t�@�C�@���@��\@��@��h@�?}@��@���@�r�@�1'@���@��w@���@��P@�l�@�"�@�n�@���@��@���@��@��@�r�@�bN@�A�@� �@�;@K�@~�+@~E�@~$�@}�-@}�@}p�@}O�@}�@}V@|��@|Z@{�m@{t�@z��@y�@y7L@xbN@x�u@x��@xA�@x �@w�;@v�R@u�@u��@t�@tj@t(�@s��@s�F@s��@s33@r�H@qhs@p��@p�`@p�u@pb@o�;@o|�@n��@m�-@mV@l�j@l�D@lj@l�D@l�D@lj@lI�@k�
@kƨ@kƨ@kƨ@kƨ@k��@kt�@kS�@kS�@kC�@ko@ko@k33@k33@kC�@k33@k"�@k"�@k"�@k"�@ko@j�@k@k@k@ko@k@k@j��@jn�@j=q@hĜ@h �@g\)@f�R@f�y@f5?@e@e�T@e`B@d�@d��@d�@c�F@c�@co@b=q@a�@a�7@aG�@a�@a%@`�9@`A�@` �@` �@`b@`  @_�@_��@_
=@^�+@]��@]O�@]V@\�@\j@\1@[�
@[�F@[��@[��@[�@[33@Z��@Z=q@Yhs@YG�@Y&�@X�`@XQ�@W+@V�R@V�+@Vff@VV@VE�@V@U��@T�@T��@T�@TZ@S�m@S��@S�@SdZ@SC�@R�H@R��@R�\@R^5@R-@RJ@Q��@Q�7@QG�@Q%@PĜ@Pr�@O�;@O�@N��@NE�@M@M`B@M/@L9X@K�m@Kƨ@Kƨ@KC�@J��@Jn�@JM�@J-@J�@I��@I�^@Ihs@IG�@I7L@I7L@I%@H�9@Hr�@H  @Gl�@G;d@G�@F��@F�R@F$�@E�@E�@D�/@D��@DZ@D1@Cƨ@C��@Ct�@Co@B�@B~�@B-@BJ@BJ@A��@A�#@A�#@A��@AG�@A%@@�u@@�@@r�@?�;@?�w@?|�@>ȴ@=@=O�@=/@=/@=�@<��@<�j@<j@<9X@<1@;�m@;�m@;ƨ@;��@;�@;t�@;dZ@;dZ@;dZ@;dZ@;C�@:��@9��@9�#@9��@9x�@9�@8��@8Ĝ@8�9@8�u@8r�@8r�@8bN@8  @7��@7l�@7;d@7+@6�+@6$�@6@6@6@6@5�@5@5�h@5�@5�@5�@5�@5�@5`B@5O�@5/@5V@4�/@4�j@4�D@4�@3��@3"�@3@3@2�H@2��@2��@2��@2�\@2�@1%@0r�@0b@/�;@/�@/�P@/�@.�R@.E�@.{@-��@-�-@-�-@-p�@-O�@-/@,��@,�/@,��@+�
@+"�@*��@*~�@*n�@*=q@*J@)�@)�^@)x�@)X@)G�@)%@(�@( �@'�;@'�@'|�@'\)@'
=@&v�@&v�@&ff@&V@&5?@%��@%�@$��@$�@$��@$j@$(�@$1@#�F@#t�@#dZ@#S�@#"�@"�H@"��@"�\@"�@!��@!�@!�#@!��@!��@!7L@ ��@ �9@ �@  �@�P@�@ȴ@�+@ff@V@E�@$�@p�@��@z�@(�@(�@�@1@��@��@�@��@�\@n�@J@�#@��@�^@��@�7@hs@7L@&�@��@�u@ �@�;@�@�P@l�@+@��@�+@V@�T@p�@/@�@�/@��@�j@�j@�j@�j@�@��@�D@�D@z�@Z@9X@9X@9X@(�@1@1@1@ƨ@ƨ@��@��@C�@�@��@n�@=q@�@�@��@��@x�@X@�`@�u@r�@r�@Q�@ �@  @��@\)@+@�y@ȴ@�+@V@5?@@@��@�@�@O�@/@�@��@�@�j@�@��@�D@Z@9X@1@��@�@t�@dZ@S�@33@@
�@
��@
~�@
n�@
�@	��@	�@	��@	x�@	hs@	hs@	hs@	hs@	hs@	X@	7L@	7L@	7L@	&�@	%@��@��@��@��@bN@A�@ �@  @  @�P@+@�@�@�R@��@E�@�@��@�@?}@�@�/@�j@z�@9X@(�@1@��@�F@S�@�@�\@n�@n�@^5@M�@M�@-@��@�#@�^@�7@hs@G�@ �`@ Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BuBuBuBoBoBoBoBoBoBoBoBhBhBbBbB\B\BDB+B%B%B%B%B%B%BBBBBBBBBBBB��B��B��B��B�B�B�ZB�BɺB�Bp�BE�B�B�oBl�B<jB=qB<jB:^B6FB0!B(�B�B
��B
�B
�fB
�BB
�B
��B
ĜB
�}B
�dB
�LB
�B
�B
�B
�B
��B
��B
�%B
}�B
}�B
w�B
u�B
n�B
gmB
ZB
K�B
F�B
H�B
J�B
G�B
@�B
5?B
9XB
8RB
2-B
-B
,B
)�B
 �B
�B
JB	��B
B
B	��B
B	��B	��B	��B	��B	�B	�mB	�B	ŢB	�^B	�3B	�B	�?B	�?B	�-B	�B	��B	�hB	�JB	�%B	�B	�B	~�B	|�B	x�B	n�B	jB	dZB	[#B	W
B	J�B	K�B	M�B	K�B	F�B	B�B	@�B	=qB	8RB	33B	/B	/B	,B	(�B	(�B	+B	,B	,B	)�B	(�B	%�B	 �B	�B	\B	hB	PB	PB	DB	+B	B	  B�B�NB�mB�BB�HB�#B�B�
B��BɺB��B��BƨB�}B�jB�dB�dB�jB�wB�jB�XB�3B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�bB�hB�bB�JB�1B�=B�+B}�B�B�B�B� By�Bx�Bw�BjB_;Bm�Bl�BffBaHB]/BVBS�B\)BaHB]/BXBP�BQ�BM�BS�BR�BM�BL�BI�B=qB5?B=qBG�BK�BJ�BI�BI�BI�BJ�BI�BI�BH�BG�BE�BB�BB�BD�BC�BA�B>wB;dB8RB2-B49B,B+B&�B,B'�B49B2-B-B.B1'B0!B0!B0!B49B5?B1'B/B/B2-B2-B0!B5?B5?B33B5?B1'B,B/B7LB;dB7LB2-B:^B<jB<jB;dB?}BA�B@�B>wB=qBA�BB�BA�B?}BB�BC�BB�BD�BD�BA�BC�BC�BB�B<jB>wBF�BL�BL�BH�BG�BF�BP�BT�BS�BP�BM�BO�BP�BQ�BS�BYB]/B^5B[#BaHBaHBffBhsBhsBiyBgmBm�Bp�Bt�Bv�Bt�By�B{�By�By�B}�B~�B~�B�B�B�%B�B�B�1B�bB�bB�hB�\B�hB�oB�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�3B�9B�9B�3B�-B�RB�wB��BÖBŢBĜBɺB��B��B�B�B�B�#B�B�B�;B�B�B�B�B�B��B��B��B	  B	B	B	B	B	PB	\B	{B	�B	�B	�B	 �B	#�B	%�B	'�B	)�B	+B	+B	)�B	&�B	&�B	)�B	/B	1'B	2-B	49B	49B	49B	5?B	6FB	7LB	9XB	=qB	?}B	?}B	B�B	C�B	D�B	D�B	F�B	E�B	E�B	G�B	G�B	H�B	H�B	L�B	Q�B	T�B	XB	YB	[#B	\)B	ZB	\)B	`BB	`BB	gmB	iyB	l�B	o�B	r�B	r�B	q�B	q�B	t�B	w�B	w�B	v�B	w�B	v�B	u�B	u�B	x�B	|�B	� B	�B	�%B	�7B	�JB	�PB	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�-B	�-B	�-B	�-B	�3B	�3B	�9B	�9B	�9B	�FB	�RB	�RB	�RB	�XB	�XB	�XB	�XB	�dB	�qB	�}B	��B	��B	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�)B	�/B	�;B	�;B	�;B	�;B	�;B	�;B	�/B	�BB	�HB	�TB	�ZB	�TB	�ZB	�ZB	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
+B
%B
%B
	7B
DB
DB
JB
JB
JB
PB
VB
VB
VB
VB
PB
VB
VB
\B
bB
bB
bB
bB
\B
hB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
!�B
 �B
!�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
%�B
&�B
(�B
(�B
(�B
(�B
+B
,B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
.B
.B
-B
-B
.B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
/B
/B
2-B
33B
49B
5?B
49B
49B
49B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
5?B
6FB
8RB
9XB
:^B
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
@�B
?}B
?}B
B�B
B�B
B�B
A�B
@�B
@�B
B�B
B�B
C�B
B�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
G�B
H�B
G�B
G�B
G�B
G�B
F�B
G�B
H�B
I�B
J�B
K�B
K�B
K�B
J�B
I�B
J�B
L�B
L�B
N�B
M�B
M�B
M�B
L�B
L�B
N�B
O�B
O�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
O�B
O�B
P�B
Q�B
R�B
R�B
Q�B
R�B
Q�B
R�B
R�B
R�B
T�B
T�B
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
XB
YB
YB
YB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
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
`BB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
bNB
bNB
cTB
bNB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
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
hsB
iyB
hsB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
l�B
m�B
m�B
m�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BuBuB�B�BoBoBoBoBoBoB�B�BhBbB}B\BvB�B�B�BYB?B?B%B?BBBBBBBBBBAB B�.B�PB�8B�B�B��B��B�#B�xB��Bu�BL~B��B�QBq�BC�B@B=�B;JB6�B1[B*eBxB
�<B
�5B
�B
��B
�QB
�uB
�+B
� B
��B
��B
�CB
��B
�]B
�QB
�zB
�#B
��B
� B
~�B
x�B
vzB
o�B
h�B
\B
N"B
HfB
IlB
K)B
H1B
A�B
7B
9�B
8�B
3hB
.B
,�B
*�B
"4B
�B
�B
;B
�B
�B	��B
;B	�HB	�VB	�^B	�`B	�B	��B	�QB	�B	��B	�B	�CB	�tB	�tB	��B	�CB	��B	�FB	��B	��B	�B	��B	�B	}�B	y�B	pB	k�B	e�B	\�B	X�B	L�B	L�B	NB	LJB	GzB	C{B	A;B	>]B	9XB	4TB	0B	/�B	,�B	)�B	)_B	+6B	,=B	,=B	*eB	)DB	&�B	!�B	�B	NB	oB	VB	�B	�B	�B	�B	 B�B��B�B��B�NB�]B�YB��B��B�DB�xB�^BǔB� B��B��B�jB��B��B��B��B�TB��B��B��B�B�&B�@B��B��B�QB�1B�1B��B��B��B��B�B�B� B��B��B�B�B��B��B�B��B��B��B��B{By�Bx�Bl�Ba�Bm�Bm)Bg�Bb�B^�BXyBVB\�Ba�B^BYKBR�BS[BO\BT�BS�BOBBM�BK)B@ B8RB?.BHfBL0BK)BJ#BJXBJ=BK)BJ	BJ	BIBH1BF%BC{BCGBEBDMBB'B?HB<�B9�B3�B5tB-�B,�B(�B-wB)�B4�B33B.IB/ B1�B0�B0�B0�B4�B5�B1�B/�B/�B2�B2�B0�B5�B5�B4B5�B2B-wB0�B7�B;�B88B3�B:�B<�B<�B<B?�BA�B@�B>�B>BA�BB�BA�B@BB�BDBC-BD�BEBB[BD3BDBCGB=�B?�BG�BM�BM�BI�BH�BHKBQ�BUgBT{BQ�BN�BP�BQ�BR�BT�BY�B]�B^�B\Ba�Ba�Bf�Bh�Bh�Bi�BhXBn/BqABuBv�Bu?BzB|BzDBzDB~]BHB}B��B�{B�YB��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B�B�!B�'B�B�B�DB�6B�"B�CB�]B��B�hB��B��B��B�MB�$B�B� B��B�B�SB�#B�^B�bB�+B�EB�7B�qBچBںB��B��B��B��B��B�B�+B�$B�0B	 4B	-B	uB	mB	�B	�B	�B	�B	�B	�B	 B	!-B	$B	&B	($B	*B	+B	+6B	*eB	'�B	'�B	*eB	/OB	1vB	2|B	4TB	4TB	4nB	5tB	6�B	7�B	9�B	=�B	?�B	?�B	B�B	C�B	D�B	D�B	F�B	E�B	E�B	G�B	G�B	IB	IB	MB	R:B	T�B	X+B	YKB	[=B	\]B	Z�B	\xB	`vB	`�B	g�B	i�B	l�B	o�B	r�B	r�B	q�B	r-B	t�B	xB	xB	wB	w�B	v�B	vB	v+B	y	B	}"B	�4B	�-B	�%B	�lB	�~B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�GB	�GB	�-B	�aB	�GB	�3B	�3B	�9B	�nB	�TB	�FB	�RB	�lB	�RB	�rB	��B	�rB	��B	��B	��B	��B	��B	��B	ŢB	��B	��B	��B	��B	�B	� B	� B	�@B	�B	�,B	�MB	�+B	�KB	�=B	�]B	�IB	�]B	�dB	�VB	�;B	�VB	�VB	�pB	�VB	ݘB	��B	�B	�B	�tB	�B	�tB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	� B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�	B	�	B	�	B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�"B	�B	�"B	�6B	�<B	�.B
'B
[B
AB
3B
uB
9B
?B
_B
tB
YB
	RB
DB
^B
dB
dB
dB
jB
pB
VB
VB
pB
jB
pB
�B
�B
}B
}B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
!�B
 �B
!�B
$B
$B
#�B
$B
$�B
%�B
%�B
&B
'B
'B
'B
&B
'B
)*B
)*B
)*B
)*B
+6B
,"B
,"B
,B
,B
,=B
,"B
,"B
-)B
-B
-)B
-B
-B
-CB
-)B
-)B
-CB
-CB
./B
.IB
-CB
-CB
.IB
0UB
1AB
1AB
1AB
1[B
1'B
1[B
/�B
/�B
2aB
3MB
4TB
5ZB
4TB
4nB
4nB
5tB
6`B
6zB
6`B
6`B
6`B
6`B
6`B
6`B
6`B
6�B
5�B
6�B
8�B
9�B
:�B
;B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
?�B
?�B
?�B
@�B
?�B
?�B
B�B
B�B
B�B
A�B
@�B
@�B
B�B
B�B
C�B
B�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
G�B
H�B
G�B
G�B
G�B
G�B
F�B
G�B
IB
I�B
J�B
K�B
K�B
K�B
J�B
J	B
KB
L�B
MB
N�B
NB
M�B
NB
MB
MB
N�B
O�B
PB
OB
O�B
P�B
P�B
P�B
Q B
QB
Q B
Q B
Q B
P.B
P.B
Q B
RB
S&B
SB
RB
SB
R B
S&B
S@B
S&B
UB
U2B
VB
VB
VB
VB
VB
VB
VB
W$B
VB
W$B
W
B
W$B
W$B
W$B
W
B
W
B
W$B
W
B
W
B
W?B
XB
W$B
W$B
W$B
W$B
W$B
X+B
X+B
X+B
X+B
Y1B
XEB
Y1B
YKB
YKB
ZQB
[#B
[#B
[=B
[=B
[=B
[WB
[qB
\CB
\CB
]dB
]IB
]dB
^jB
^OB
^OB
^jB
_VB
_;B
_VB
_VB
_VB
`\B
`\B
`\B
`BB
`BB
`vB
`\B
`\B
`vB
`vB
bNB
bhB
bNB
bNB
bhB
b�B
cnB
bhB
b�B
cTB
b�B
cnB
cnB
cnB
dtB
dZB
e`B
e`B
ezB
ezB
dtB
e`B
e`B
e`B
e`B
ezB
ezB
ffB
ffB
ezB
ezB
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
h�B
i�B
h�B
i�B
i�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
m�B
m�B
m�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801030035522018010300355220180103003552201806221235352018062212353520180622123535201804050431562018040504315620180405043156  JA  ARFMdecpA19c                                                                20171230093516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171230003517  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171230003520  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171230003520  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171230003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171230003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171230003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171230003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171230003521  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171230003521                      G�O�G�O�G�O�                JA  ARUP                                                                        20171230005529                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171230153506  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180102153552  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180102153552  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193156  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033535  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                