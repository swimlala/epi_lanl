CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-03T00:35:14Z creation;2018-08-03T00:35:21Z conversion to V3.1;2019-12-19T07:36:01Z update;     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180803003514  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              
A   JA  I2_0576_266                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�v�����1   @�v�[��@9�Vl�!�d[\����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  AᙚA�33B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @<��@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�A�33A���A���B��BffB��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3CuٚCw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fDׁ�D׾fD��fD�A�D�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��D�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�A�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A� �A��A��A��A��A��A��A��A��A��A�oA͸RA�oA�^5A�  A�jA�  A���A���A���A��A�|�A��7A��yA���A��!A�v�A�Q�A�G�A�7LA�+A��A�1A�ffA�l�A�^5A�VA�^5A�1'A��
A�t�A��^A���A��!A�hsA�{A��/A�ƨA��DA��A�
=A�ĜA�ffA��A�I�A�{A�l�A��!A��A�t�A�C�A�ƨA�^5A���A�Q�A��FA�bNA��A��FA�l�A���A�JA� �A��uA�A��PA���A��RA���A�jA�=qA}ƨA|�Az�Ax�Av�At1'As�FAr�/Aq��Ap�uAl��Ai�Ag��Af��Af�9AfQ�AeO�AcS�AbȴA`�!A[�A[/A[K�A[K�A[7LA[/A["�A["�A[�AZĜAY��AXJAV�+AUC�AT�RASx�AS
=AR�AQAP�uAPffAPE�AP �AO|�AM�
AMhsAL~�AK��AK&�AJI�AI��AHbNAG33AF��AF1'AE`BAD�`ADv�AD-AC7LAB  AA�A@A?�PA?VA>�+A>JA=��A=S�A<��A;��A:n�A9t�A8�uA7�TA7G�A6��A6$�A5
=A3��A2�A1t�A0�A.-A-��A-��A-�hA-K�A,�`A,ȴA,z�A+��A*��A*��A*��A*I�A*1A(�A(r�A&�/A%��A%O�A%7LA%;dA%/A$��A$�A$VA#A#C�A"��A!�TA ZA�-A��A�;A�A9XA�wA�PAK�A�HA��AJA�AĜA �Ap�A^5A�#AK�AA��A�#A�AbNAƨAVA%A��AXA"�A
��A
-A	��A	��A	p�A	;dA��A1'A{AXAȴA��A��A~�A�A�`A��AVA �A 5?@�hs@�|�@�@�(�@�%@��y@���@���@�$�@���@��^@�%@��
@��@�R@�`B@�7L@��@�\@��T@�K�@ޗ�@�%@�v�@���@١�@�hs@��/@�  @֧�@��T@�hs@�&�@�1'@��@���@���@�o@Ώ\@�~�@�`B@�A�@�C�@ʇ+@�-@ə�@�&�@�Q�@�\)@Ƈ+@�p�@Ĭ@ēu@�j@þw@�33@��y@§�@�^5@�%@��u@�  @��@��h@���@�Z@�1'@���@���@�@�@�p�@���@�;d@��@�1@��w@�C�@��@��R@�V@�hs@�j@� �@�t�@�+@��@��!@���@��m@���@�E�@��#@���@��j@�Q�@��w@�o@���@�v�@�p�@�j@�9X@�(�@� �@�  @��y@��#@��@��D@��F@�S�@��\@�=q@���@�?}@��j@��@�@���@�=q@�x�@���@�j@�l�@���@��\@�M�@���@�G�@��@��9@�Z@���@��!@�5?@��@�?}@��D@���@��\@�E�@���@�`B@�G�@��@��D@�9X@� �@��@�b@�  @�ƨ@�t�@�K�@�33@��@���@���@�~�@��^@�7L@���@�bN@��m@��P@�l�@�33@���@�^5@�J@��@���@�G�@�&�@��@���@���@�A�@���@���@�ƨ@��F@��@�S�@�
=@��@���@�~�@�ff@�{@��@�G�@�V@���@��9@�Z@�(�@��@l�@~�y@~ff@~5?@~@}@}�@}O�@}/@}�@|��@|Z@{ƨ@{33@{@z�H@z�!@z~�@zn�@zJ@y��@yx�@yhs@yX@yG�@x�@xA�@x �@w�@v�R@vff@v{@u�T@u�-@up�@u?}@uV@t�/@t�j@t�D@s��@s�@st�@sdZ@s33@s@r�!@r-@q�^@qG�@pĜ@pQ�@pb@o�;@ol�@o�@n�@n��@n$�@m�h@m/@mV@l�j@lz�@lI�@l9X@l(�@lI�@lI�@lI�@lI�@l1@k�
@k�
@kƨ@k�F@k�F@k��@kS�@j�@j��@j�!@jn�@i�@i�7@ihs@h�`@h�u@h�@g�@g�@g��@gl�@g+@fȴ@fV@e@e��@e`B@e?}@d�@d�j@d�D@dj@dZ@d(�@c��@c��@cS�@co@b�@b�H@b��@b~�@bJ@a�7@aX@a�@`��@`bN@`1'@`  @_�w@_l�@^�@^v�@^E�@^{@]�@]��@]�@\�/@\��@\Z@[ƨ@["�@Z�!@Z^5@Z�@YX@X�9@XA�@W�;@W��@Wl�@W;d@VV@V@U�T@UO�@U�@T��@T��@T�j@T�@T��@Tz�@TI�@S��@So@So@So@So@S@S@R�!@RJ@Q��@Q��@Qhs@QG�@P��@PA�@P �@O�;@O\)@O;d@O�@N�y@Nȴ@N��@Nv�@N$�@M��@Mp�@MV@L��@L�@L��@LZ@L�@Kƨ@K"�@J�@J�H@J�H@J��@JM�@I��@I�^@Ihs@H�`@H�9@HQ�@G�P@G\)@G
=@Fȴ@F��@Fff@E�T@E`B@D�@D�@C��@CC�@Bn�@B�@A��@Ax�@@��@@Q�@?�@?�@?�P@?
=@>��@>E�@>@=�-@=`B@=/@<��@<�/@<��@<�@<�@;��@;��@;t�@;S�@;@:M�@9��@9x�@9%@8A�@8  @7l�@7+@6�@6�+@6v�@6ff@6{@5��@5�h@5?}@4��@4�@49X@4�@3�F@333@2�H@2�!@2��@2~�@1�@1��@1X@0��@0��@0�@0r�@0bN@0A�@/�;@/��@.�@.��@.�+@.v�@.ff@.V@.5?@-�T@-�-@-�h@-O�@,��@,�/@,��@,�j@,�j@,��@,9X@+��@+�F@+��@+�@+"�@*�@*��@*�!@*�@)�@)��@)G�@)%@(��@(��@(��@(bN@(1'@'��@'+@'�@&��@&ȴ@&ff@%@%�@%/@$��@$�D@$9X@$1@#�
@#�F@#��@#C�@"�H@"�!@"�\@"M�@!�#@!hs@!G�@!%@ ��@ �u@ r�@ 1'@�w@�@�P@\)@�@�y@�@ȴ@��@E�@{@�@V@�@Z@1@��@dZ@C�@o@��@~�@��@��@x�@x�@x�@x�@X@7L@��@bN@b@�w@��@|�@;d@+@��@��@�+@�+@�+@ff@E�@$�@�T@��@p�@/@��@��@z�@Z@I�@1@�m@��@��@�@dZ@S�@S�@33@�H@��@=q@��@�^@�7@hs@X@&�@�`@�9@bN@A�@A�@ �@  @�@|�@|�@l�@;d@��@�R@�+@V@@�T@��@@��@�h@�h@�h@�@�@O�@?}@/@�@V@V@��@��@�@�j@z�@9X@(�@(�@�@1@�m@ƨ@�@dZ@C�@o@
�H@
�H@
��@
�\@
=q@	�#@	��@	hs@	G�@	&�@	�@��@��@��@�@r�@Q�@b@��@K�@;d@;d@�@
=@��@�y@ȴ@�+@V@{@�@@��@��@�h@�@O�@?}@/@�@V@�/@�@z�@9X@(�@��@�
@��@�@S�@C�@S�@"�@o@o@@�H@�!@~�@^5@^5@=q@�@J@��@�#@��@��@��@x�@X@&�@�@ Ĝ@ �@ A�@ 1'@  �@  �@ b?��;?���?�\)?��?���?��?��?���?��R?�V?�5??�5?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A� �A��A��A��A��A��A��A��A��A��A�oA͸RA�oA�^5A�  A�jA�  A���A���A���A��A�|�A��7A��yA���A��!A�v�A�Q�A�G�A�7LA�+A��A�1A�ffA�l�A�^5A�VA�^5A�1'A��
A�t�A��^A���A��!A�hsA�{A��/A�ƨA��DA��A�
=A�ĜA�ffA��A�I�A�{A�l�A��!A��A�t�A�C�A�ƨA�^5A���A�Q�A��FA�bNA��A��FA�l�A���A�JA� �A��uA�A��PA���A��RA���A�jA�=qA}ƨA|�Az�Ax�Av�At1'As�FAr�/Aq��Ap�uAl��Ai�Ag��Af��Af�9AfQ�AeO�AcS�AbȴA`�!A[�A[/A[K�A[K�A[7LA[/A["�A["�A[�AZĜAY��AXJAV�+AUC�AT�RASx�AS
=AR�AQAP�uAPffAPE�AP �AO|�AM�
AMhsAL~�AK��AK&�AJI�AI��AHbNAG33AF��AF1'AE`BAD�`ADv�AD-AC7LAB  AA�A@A?�PA?VA>�+A>JA=��A=S�A<��A;��A:n�A9t�A8�uA7�TA7G�A6��A6$�A5
=A3��A2�A1t�A0�A.-A-��A-��A-�hA-K�A,�`A,ȴA,z�A+��A*��A*��A*��A*I�A*1A(�A(r�A&�/A%��A%O�A%7LA%;dA%/A$��A$�A$VA#A#C�A"��A!�TA ZA�-A��A�;A�A9XA�wA�PAK�A�HA��AJA�AĜA �Ap�A^5A�#AK�AA��A�#A�AbNAƨAVA%A��AXA"�A
��A
-A	��A	��A	p�A	;dA��A1'A{AXAȴA��A��A~�A�A�`A��AVA �A 5?@�hs@�|�@�@�(�@�%@��y@���@���@�$�@���@��^@�%@��
@��@�R@�`B@�7L@��@�\@��T@�K�@ޗ�@�%@�v�@���@١�@�hs@��/@�  @֧�@��T@�hs@�&�@�1'@��@���@���@�o@Ώ\@�~�@�`B@�A�@�C�@ʇ+@�-@ə�@�&�@�Q�@�\)@Ƈ+@�p�@Ĭ@ēu@�j@þw@�33@��y@§�@�^5@�%@��u@�  @��@��h@���@�Z@�1'@���@���@�@�@�p�@���@�;d@��@�1@��w@�C�@��@��R@�V@�hs@�j@� �@�t�@�+@��@��!@���@��m@���@�E�@��#@���@��j@�Q�@��w@�o@���@�v�@�p�@�j@�9X@�(�@� �@�  @��y@��#@��@��D@��F@�S�@��\@�=q@���@�?}@��j@��@�@���@�=q@�x�@���@�j@�l�@���@��\@�M�@���@�G�@��@��9@�Z@���@��!@�5?@��@�?}@��D@���@��\@�E�@���@�`B@�G�@��@��D@�9X@� �@��@�b@�  @�ƨ@�t�@�K�@�33@��@���@���@�~�@��^@�7L@���@�bN@��m@��P@�l�@�33@���@�^5@�J@��@���@�G�@�&�@��@���@���@�A�@���@���@�ƨ@��F@��@�S�@�
=@��@���@�~�@�ff@�{@��@�G�@�V@���@��9@�Z@�(�@��@l�@~�y@~ff@~5?@~@}@}�@}O�@}/@}�@|��@|Z@{ƨ@{33@{@z�H@z�!@z~�@zn�@zJ@y��@yx�@yhs@yX@yG�@x�@xA�@x �@w�@v�R@vff@v{@u�T@u�-@up�@u?}@uV@t�/@t�j@t�D@s��@s�@st�@sdZ@s33@s@r�!@r-@q�^@qG�@pĜ@pQ�@pb@o�;@ol�@o�@n�@n��@n$�@m�h@m/@mV@l�j@lz�@lI�@l9X@l(�@lI�@lI�@lI�@lI�@l1@k�
@k�
@kƨ@k�F@k�F@k��@kS�@j�@j��@j�!@jn�@i�@i�7@ihs@h�`@h�u@h�@g�@g�@g��@gl�@g+@fȴ@fV@e@e��@e`B@e?}@d�@d�j@d�D@dj@dZ@d(�@c��@c��@cS�@co@b�@b�H@b��@b~�@bJ@a�7@aX@a�@`��@`bN@`1'@`  @_�w@_l�@^�@^v�@^E�@^{@]�@]��@]�@\�/@\��@\Z@[ƨ@["�@Z�!@Z^5@Z�@YX@X�9@XA�@W�;@W��@Wl�@W;d@VV@V@U�T@UO�@U�@T��@T��@T�j@T�@T��@Tz�@TI�@S��@So@So@So@So@S@S@R�!@RJ@Q��@Q��@Qhs@QG�@P��@PA�@P �@O�;@O\)@O;d@O�@N�y@Nȴ@N��@Nv�@N$�@M��@Mp�@MV@L��@L�@L��@LZ@L�@Kƨ@K"�@J�@J�H@J�H@J��@JM�@I��@I�^@Ihs@H�`@H�9@HQ�@G�P@G\)@G
=@Fȴ@F��@Fff@E�T@E`B@D�@D�@C��@CC�@Bn�@B�@A��@Ax�@@��@@Q�@?�@?�@?�P@?
=@>��@>E�@>@=�-@=`B@=/@<��@<�/@<��@<�@<�@;��@;��@;t�@;S�@;@:M�@9��@9x�@9%@8A�@8  @7l�@7+@6�@6�+@6v�@6ff@6{@5��@5�h@5?}@4��@4�@49X@4�@3�F@333@2�H@2�!@2��@2~�@1�@1��@1X@0��@0��@0�@0r�@0bN@0A�@/�;@/��@.�@.��@.�+@.v�@.ff@.V@.5?@-�T@-�-@-�h@-O�@,��@,�/@,��@,�j@,�j@,��@,9X@+��@+�F@+��@+�@+"�@*�@*��@*�!@*�@)�@)��@)G�@)%@(��@(��@(��@(bN@(1'@'��@'+@'�@&��@&ȴ@&ff@%@%�@%/@$��@$�D@$9X@$1@#�
@#�F@#��@#C�@"�H@"�!@"�\@"M�@!�#@!hs@!G�@!%@ ��@ �u@ r�@ 1'@�w@�@�P@\)@�@�y@�@ȴ@��@E�@{@�@V@�@Z@1@��@dZ@C�@o@��@~�@��@��@x�@x�@x�@x�@X@7L@��@bN@b@�w@��@|�@;d@+@��@��@�+@�+@�+@ff@E�@$�@�T@��@p�@/@��@��@z�@Z@I�@1@�m@��@��@�@dZ@S�@S�@33@�H@��@=q@��@�^@�7@hs@X@&�@�`@�9@bN@A�@A�@ �@  @�@|�@|�@l�@;d@��@�R@�+@V@@�T@��@@��@�h@�h@�h@�@�@O�@?}@/@�@V@V@��@��@�@�j@z�@9X@(�@(�@�@1@�m@ƨ@�@dZ@C�@o@
�H@
�H@
��@
�\@
=q@	�#@	��@	hs@	G�@	&�@	�@��@��@��@�@r�@Q�@b@��@K�@;d@;d@�@
=@��@�y@ȴ@�+@V@{@�@@��@��@�h@�@O�@?}@/@�@V@�/@�@z�@9X@(�@��@�
@��@�@S�@C�@S�@"�@o@o@@�H@�!@~�@^5@^5@=q@�@J@��@�#@��@��@��@x�@X@&�@�@ Ĝ@ �@ A�@ 1'@  �@  �@ b?��;?���?�\)?��?���?��?��?���?��R?�V?�5??�5?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BL�BI�BJ�BJ�BK�BK�BJ�BK�BJ�BJ�BH�BB�B9XB.BB0!B �B��B�uB�bB�%BYBJ�B�PB��B��B��B��B��B��B��B�{B�PBz�Be`BXBP�BJ�BE�BO�BH�B+B2-B?}B?}B<jB:^B9XB33B&�B�B�B�BbBB�B�sB�NB��B��BɺB�}B�?B�B��B�uB�Bl�B{�BcTBYB?}B,B�BhB	7B
�fB
ǮB
�!B
�=B
s�B
L�B
W
B
H�B
<jB
"�B
�B
�B
�B
B	��B	�
B	�9B	ĜB	ƨB	ĜB	�qB	�!B	��B	��B	�B	`BB	�B	�PB	�oB	�uB	��B	��B	��B	�uB	�PB	�B	r�B	cTB	hsB	ffB	ZB	cTB	XB	R�B	ZB	]/B	\)B	XB	M�B	@�B	D�B	>wB	49B	2-B	+B	(�B	�B	�B	�B	�B	{B	�B	oB	PB	B��B��B��B��B��B��B�B�B�B�sB�#B�B�B��B��B��B��BɺB��B�jB�B��B��B��B�B�!B�B�B��B��B��B��B��B��B��B��B��B�JB�\B�B�B�bB��B��B�{B�bB�VB�%B�B�B{�Bt�BiyBp�Bp�BdZBk�BhsBn�Bq�Bn�BiyBgmB^5BP�BD�BM�BS�BJ�BS�BP�BR�BN�BG�BD�B@�B@�B:^B0!B7LB9XBB�B?}B<jB>wBB�BA�B?}B9XB;dB?}B8RB9XB=qB2-B/B2-B33B6FB6FB/B �B�B&�B$�B�B�B�B(�B&�B �B�B�B�B�BuBuB�B �B�BuB�BoB�B�BuB"�B'�B&�B#�B �B�B!�B#�B$�B�B�B�B�B�B"�B$�B�B�B�B"�B%�B$�B$�B#�B"�B'�B'�B,B1'B1'B.B-B0!B0!B/B)�B2-B33B49B33B<jB>wB?}B?}B;dB<jBA�B@�B<jB:^B6FBI�BM�BL�BM�BO�BM�BK�BN�BVBR�BVBVBS�BP�BP�BYB[#B_;B]/BcTBdZBdZBe`BhsBhsBffBk�Bt�Bu�Bv�Bu�Bq�Bs�Bx�B{�B|�B�B�B�=B�7B�DB�PB�VB�\B�uB�{B�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�?B�RB�LB�XB�qBB��B��B��B�
B�B�B�BB�ZB�`B�`B�ZB�TB�ZB�mB�sB�yB�yB�sB�mB�mB�B��B��B��B��B	B	B	B	B	1B	DB	DB	JB	bB	oB	oB	oB	hB	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	%�B	&�B	&�B	'�B	/B	33B	5?B	9XB	:^B	>wB	?}B	B�B	C�B	D�B	F�B	H�B	I�B	J�B	L�B	N�B	O�B	N�B	P�B	R�B	W
B	[#B	]/B	^5B	^5B	`BB	`BB	bNB	e`B	ffB	gmB	ffB	e`B	jB	k�B	k�B	l�B	t�B	u�B	w�B	y�B	z�B	|�B	}�B	}�B	� B	� B	� B	�B	�+B	�1B	�1B	�7B	�7B	�=B	�PB	�\B	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�9B	�FB	�FB	�RB	�^B	�XB	�qB	�wB	�wB	�}B	�}B	��B	ÖB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�5B	�;B	�;B	�;B	�;B	�HB	�NB	�NB	�HB	�TB	�`B	�fB	�mB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
B
1B
1B
	7B
1B
1B
	7B
	7B
	7B
	7B
DB
DB

=B
VB
PB
VB
VB
\B
VB
VB
\B
bB
oB
uB
uB
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
!�B
!�B
#�B
#�B
$�B
$�B
&�B
&�B
'�B
(�B
(�B
+B
+B
)�B
)�B
+B
+B
+B
,B
,B
-B
-B
-B
.B
/B
0!B
/B
.B
0!B
0!B
0!B
2-B
2-B
33B
33B
2-B
1'B
2-B
1'B
5?B
5?B
5?B
6FB
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
7LB
8RB
8RB
9XB
9XB
8RB
9XB
:^B
:^B
9XB
:^B
;dB
:^B
<jB
=qB
=qB
<jB
<jB
=qB
=qB
<jB
?}B
?}B
?}B
>wB
>wB
@�B
@�B
A�B
@�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
J�B
I�B
J�B
J�B
K�B
L�B
L�B
M�B
M�B
O�B
O�B
O�B
N�B
O�B
O�B
P�B
R�B
R�B
R�B
R�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
T�B
T�B
T�B
VB
T�B
T�B
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
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
ZB
YB
ZB
ZB
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
^5B
^5B
^5B
_;B
`BB
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
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
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
dZB
ffB
e`B
e`B
e`B
ffB
ffB
e`B
e`B
e`B
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
hsB
hsB
gmB
hsB
jB
jB
jB
jB
jB
jB
jB
iyB
jB
jB
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
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
p�B
p�B
q�B
q�B
q�B
p�B
p�B
p�B
q�B
r�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BL�BI�BJ�BJ�BK�BK�BJ�BK�BJ�BJ�BIRBD�B>�B8�B�B8�B*�B �B��B��B��B_!BP�B�"B�B��B�#B��B��B��B��B��B��B|�Bg�BZ�BS@BMBG�BP�BJ#B.�B3�B@B@B="B:�B9�B4B(sB vBjB�B�B�B�?B�0B�&B�}BðBʌB��B�`B�WB�@B�gB�Bo�B|�Bf2B[=BC-B.�B �BB
�B
�B
�B
��B
�VB
x�B
RB
X�B
J�B
>�B
&B
�B
 �B
�B
�B	��B	�WB	�RB	�B	ǔB	�B	�]B	��B	�B	��B	�KB	d�B	��B	�PB	�oB	��B	��B	��B	��B	��B	�B	��B	uB	e`B	i�B	gRB	[�B	c�B	YeB	TFB	Z�B	]dB	\�B	X�B	N�B	BuB	ESB	?�B	5ZB	2�B	,"B	)�B	xB	B	]B	xB	�B	9B	B	"B	mB�xB�	B�+B�lB��B��B�hB�AB�CB�yB��BڠB�eB�B��B��B͹BʦB�B��B��B��B��B��B�5B�oB�wB��B��B�KB��B��B��B�&B�-B�IB�eB��B�HB�B��B��B��B��B��B��B��B�B��B��B|�Bv+Bk�Bq�Bq�Be�Bl�Bi�BoOBr-BoBj0Bh$B_pBR�BGEBN�BT�BL0BT�BQ�BSuBO�BH�BE�BA�BA�B<jB2B8�B:DBCB@B=VB?BB�BBB@B:^B<B@B9rB:DB=�B3�B0�B3MB4TB6�B6�B0!B#:B�B(XB&B BBqB�B)*B'8B!�B$B�BpB�BgB�BxB!-B�B�BkBBqB�BB#TB(>B'8B$�B!�B �B"hB$ZB%FB�B�B�BpB�B#TB%,B�B�BpB#TB&LB%`B%zB$�B#�B(�B(�B,�B1[B1vB.�B-wB0oB0oB/�B+B2�B3�B5B49B<�B>�B?�B?�B<6B=BA�BAB=VB;�B7�BJ	BN<BM6BN"BPBNpBL�BO�BVSBSuBVmBVSBT{BQ�BR BY�B[�B_�B^Bc�Bd�Bd�Be�Bh�Bh�Bg8Bl"Bt�Bu�Bv�Bv+Br�Bt�ByrB|jB}�B�uB��B��B��B��B��B��B��B��B��B�B�9B�B�QB�CB�B� B�NB�$B�0B�B�kB��B��B��B��B��B�B�(B�GB�"B�BB�NB�?B�EB�B�vB�tB�B�B�B�B��B�B�B�B��B�B��B�
B�B�%B�+B�8B�<B	 B	UB	[B	�B	fB	^B	�B	�B	}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	# B	$B	&2B	'B	'8B	(XB	/iB	3hB	5tB	9�B	:�B	>�B	?�B	B�B	C�B	D�B	F�B	H�B	I�B	J�B	L�B	OB	O�B	OB	QB	S[B	W?B	[=B	]IB	^jB	^OB	`\B	`�B	b�B	e�B	f�B	g�B	f�B	e�B	j�B	k�B	k�B	l�B	t�B	u�B	w�B	y�B	{B	}B	~(B	~(B	�4B	�4B	�OB	�MB	�EB	�KB	�KB	�lB	�lB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�
B	�B	�B	�"B	�B	�)B	�"B	�=B	�]B	�UB	�;B	�UB	�[B	�TB	�`B	�zB	�lB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	��B	� B	�B	�:B	�&B	�B	�B	�SB	�+B	�1B	�QB	�1B	�QB	�kB	�]B	�OB	�pB	�pB	ߊB	�pB	�bB	�B	�hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�B	�"B	�B	�(B	�(B
 B
 B
 B
 B
 B
 B
;B
;B
-B
-B
3B
9B
?B
SB
9B
SB
mB
KB
1B
	7B
KB
fB
	lB
	RB
	lB
	lB
^B
xB

�B
�B
�B
pB
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
!�B
"B
$B
$&B
%B
%,B
'B
'B
(
B
)*B
)B
+B
+B
*B
*B
+6B
+B
+B
,"B
,WB
-)B
-CB
-]B
./B
/5B
0;B
/OB
.IB
0;B
0UB
0UB
2GB
2GB
33B
3MB
2GB
1[B
2GB
1vB
5ZB
5tB
5ZB
6FB
5tB
5ZB
5ZB
6zB
6`B
6zB
6`B
7fB
8RB
8RB
8RB
8lB
7�B
8lB
8lB
9XB
9�B
8�B
9rB
:xB
:xB
9�B
:�B
;B
:�B
<�B
=qB
=�B
<�B
<�B
=�B
=�B
<�B
?}B
?�B
?�B
>�B
>�B
@�B
@�B
A�B
@�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
J�B
I�B
J�B
KB
K�B
L�B
MB
NB
NB
PB
PB
PB
OB
O�B
P.B
Q B
SB
R�B
SB
SB
RB
R B
R B
R B
S&B
TB
UB
UB
UB
VB
U2B
U2B
W$B
W$B
W$B
W$B
W?B
W$B
VB
W?B
W$B
W$B
XEB
XEB
XEB
Y1B
Y1B
YKB
Y1B
YKB
ZB
Z7B
Z7B
ZB
[=B
ZQB
YKB
Z7B
Z7B
[=B
\CB
\CB
]dB
]IB
]dB
]dB
]IB
]dB
^OB
_;B
^OB
^jB
^OB
_VB
`BB
_VB
_VB
_VB
_VB
`vB
`vB
`vB
a|B
bNB
bNB
bhB
bNB
cnB
cTB
cTB
cTB
c�B
cnB
cTB
dtB
dZB
dtB
dtB
e`B
ezB
d�B
d�B
dtB
ezB
e`B
e`B
ezB
ezB
e�B
dtB
ffB
ezB
ezB
ezB
ffB
f�B
ezB
ezB
e�B
f�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
i�B
iyB
h�B
h�B
g�B
h�B
j�B
jB
j�B
jB
jB
j�B
j�B
i�B
j�B
j�B
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
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
p�B
p�B
q�B
q�B
q�B
p�B
p�B
p�B
q�B
r�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<*d�<g�<#�
<#�
<AV�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808070036332018080700363320180807003633201808070200162018080702001620180807020016201808080020522018080800205220180808002052  JA  ARFMdecpA19c                                                                20180803093510  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180803003514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180803003517  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180803003518  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180803003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180803003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180803003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180803003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180803003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180803003521                      G�O�G�O�G�O�                JA  ARUP                                                                        20180803005723                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180803153254  CV  JULD            G�O�G�O�Fõ�                JM  ARGQJMQC2.0                                                                 20180803153254  CV  JULD_LOCATION   G�O�G�O�Fõ�                JM  ARGQJMQC2.0                                                                 20180803153254  CV  LATITUDE        G�O�G�O�A�n�                JM  ARGQJMQC2.0                                                                 20180803153254  CV  LONGITUDE       G�O�G�O��"ۦ                JM  ARCAJMQC2.0                                                                 20180806153633  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180806153633  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180806170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180807152052  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                