CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-25T00:35:23Z creation;2016-08-25T00:35:25Z conversion to V3.1;2019-12-19T08:32:09Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160825003523  20200115101518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_030                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�ų�$� 1   @�Ŵ��-�@;m��U�=�dg�$�/1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D3��D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;fD;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ Dʼ�D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BOffBW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CYٚC[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3�fD4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D;3D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl�fDm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��3D�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʻ3D��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�;3D�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��3D�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A·+A΃A�|�A�r�A�p�A�l�A�hsA�bNA�\)A�ZA��A�M�A�hsA˝�A�\)A�"�A�A�$�A�ZA�x�A�C�Aĩ�A�n�A�I�A�n�A��`A���A�`BA�VA�A��A���A���A�A�A���A�33A�7LA��A��yA��A�x�A�A��9A�K�A���A���A���A�v�A�{A�$�A��RA�r�A�O�A�(�A��A��/A���A�{A�33A��wA���A�%A�  A���A�ffA���A�
=A�
=A���A���A��A�v�A�ffA�A�A��A��A��9A�1A���A�33A�1'A���A�l�A���A��A~ZA}��A| �Az(�Ay7LAxv�Aw�wAw|�Avz�Au��AuAs��As+Ar��ArAp{AoK�An^5Am��AmoAmAlJAh��Ag\)Af�Af�\AeAe��Ae?}Ac�-AbVAa�Aa\)A`��A_�A_hsA_VA^�A^bA\��AZZAX�/AXJAU�#AU%AU&�AUVAS��AR�AQ�wAP-ANĜANz�AN^5AN9XAM��AM33AM�AMVAL�ALn�AJ�/AI+AHbAF��AE"�AE
=AD�AD1'AB~�AA��A@��A?33A>��A>n�A<�HA:n�A9VA8VA7�
A7VA6r�A6bA5��A4��A3�7A2^5A1�;A0��A/;dA.��A-�A,�A*��A*$�A)hsA(��A'��A'"�A%�^A%33A$E�A#?}A"�DA"VA"(�A!��A ĜA Q�A A�A�A�/A��A��A5?A�mA�A7LAȴA�hA�A�DA�AjA�FA�+A�A��AAS�A%A��A=qAG�AM�A�-A �A�yA1'A��A
��A	�^A��A|�A�#A�DA��AC�A��A�yA�jA��A5?A �`A   @��H@��@��7@�Ĝ@�9X@���@�o@��7@�V@��@�j@���@�+@��/@�hs@웦@�Q�@�9X@띲@�-@�%@��
@�&�@�u@�Q�@�ƨ@�E�@�7@��@ߍP@܃@�5?@�z�@���@ӕ�@љ�@�9X@�^5@͡�@��@̴9@�r�@� �@ˍP@���@�J@�z�@ǝ�@ƸR@��@�  @�\)@��@���@���@��@��@�+@�7L@��@�I�@��
@�C�@��#@��
@�\)@�S�@�S�@�S�@�o@���@���@�J@�p�@�V@���@�1'@��
@��P@�@���@�%@�z�@�1@��P@�S�@�o@���@�5?@��@��-@�`B@�V@���@���@�@�Ĝ@��@�+@�"�@�o@�@��@��!@�-@��@��@�I�@��@��F@�K�@�33@��@���@��y@��H@��!@���@�^5@��^@���@�/@���@��u@�bN@��@�K�@��y@�n�@�@�O�@��@�V@�r�@�I�@�I�@���@�33@�5?@��7@�V@�9X@�  @���@��
@���@�"�@�S�@�dZ@�;d@���@��@�v�@�5?@�=q@�-@��@�G�@��D@�z�@�A�@��F@�S�@���@��@���@��m@�ȴ@�\)@�C�@�-@�@�`B@��@���@��@�1'@���@�\)@�"�@��H@�$�@�`B@��j@���@���@�K�@�"�@���@���@�n�@�^5@�M�@�=q@�{@�J@�{@�{@���@���@���@��7@��@��7@�x�@��7@��h@�&�@���@��j@���@��@�bN@�9X@�1@�"�@�E�@��@���@�p�@�X@�X@�V@��/@���@��@���@���@��u@��@�9X@�;@|�@;d@+@�@~�y@~ȴ@~�R@~ff@}`B@|(�@{S�@z�@y�7@y7L@y%@y%@y%@x��@x�`@xĜ@x��@x��@x�u@xr�@xr�@xr�@xbN@xbN@x1'@w��@w
=@vff@vȴ@v�+@vff@u�@uV@sC�@qG�@p�@p�@pA�@o�P@o;d@n�y@n5?@m�h@l��@mV@l�/@m�@mO�@m��@nv�@n��@n{@m��@m�h@m�-@mp�@m?}@mV@l�@kC�@j�@j�@j�!@j��@j=q@i�7@i�@h��@h�9@hQ�@h1'@g�@g�P@fv�@e�@eV@d��@d�@d�/@d�/@d�j@dj@c��@c�
@c��@cS�@co@b�@b�\@a�^@aX@a�@`�`@`�9@`r�@`1'@`b@`  @_��@_|�@_
=@^�@^V@^5?@]�@]��@\��@\�@[�F@[dZ@Z^5@Y��@YG�@XĜ@W�@W�P@W;d@V�@V{@U�-@U��@U�h@Up�@UO�@U�@TI�@S�@SC�@R��@R-@Q��@Q��@P�`@P1'@O|�@O
=@N��@N�@N�R@Nv�@NE�@N$�@N@M�T@M@M�h@M�@L��@L1@K��@KdZ@K"�@J�@J�\@J-@JJ@I�^@I��@IG�@I�@H��@HQ�@H �@G�@G�w@G�w@G�P@F��@FE�@E�@E@E�@E?}@E/@E�@D��@D��@Dz�@Dz�@DI�@C��@C��@Ct�@CdZ@CS�@CC�@C"�@B�H@B�!@B��@B^5@A�@A�^@Ax�@AG�@A�@@��@@�`@@�@?�@?��@?K�@>�@>V@=@=?}@<��@<��@<�D@<I�@<9X@<(�@;��@;t�@;33@;@:�H@:��@:�!@:��@:�\@:�\@:^5@:�@9�^@9hs@9�@8A�@7�P@7+@6�@6ȴ@6�R@6��@6ff@6@5`B@5�@4�j@3�m@3t�@3C�@3S�@2�@2M�@2J@1�@1��@1��@1�7@1x�@1X@1X@1G�@1&�@0��@0�`@0�u@0Q�@/�w@/|�@/|�@/l�@/\)@/;d@.ȴ@.�R@.��@.�+@.V@.{@-@-O�@-/@,�@,��@,j@,Z@,Z@,I�@,I�@,9X@,�@,1@+��@+�
@+t�@*�@*�\@*~�@*~�@*J@)��@)��@)�7@)x�@)X@)�@)%@)%@(��@(��@(�`@(r�@(A�@(1'@( �@'�;@'�@'��@'��@'��@'|�@'+@&��@&�y@&ȴ@&�R@&�+@&v�@&V@&$�@%�h@$�j@$z�@#��@#ƨ@#ƨ@#�F@#�F@#�@#t�@#dZ@#33@"�H@"�!@"�!@"��@"��@"�\@"�\@"=q@!��@!��@!x�@!G�@ ��@ �`@ Ĝ@ �u@ A�@   @�w@l�@+@
=@�y@�+@E�@�@��@`B@/@��@�j@�D@I�@(�@�m@�@dZ@S�@"�@��@n�@=q@=q@-@��@�@�#@��@�^@��@��@X@&�@��@Ĝ@�@A�@��@|�@;d@��@��@v�@E�@$�@@�T@��@�-@��@�@O�@�@O�@/@V@�@�/@��@��@z�@9X@��@t�@C�@�@�\@J@�@��@x�@hs@&�@Ĝ@��@r�@bN@1'@ �@�w@�@��@�P@|�@K�@
=@�y@�R@��@ff@�@p�@`B@`B@`B@��@�/@�j@��@�D@�D@j@Z@(�@(�@1@ƨ@��@��@�@S�@S�@S�@C�@"�@
��@
�!@
��@
��@
��@
�\@
^5@
�@	��@	��@	�@	��@	�7@	hs@	7L@�`@��@�@A�@�@��@��@�w@�w@�@��@�P@l�@+@
=@�y@��@v�@ff@V@5?@�T@�T@��@��@@��@�h@p�@O�@�@�/@��@��@�j@�j@�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A·+A΃A�|�A�r�A�p�A�l�A�hsA�bNA�\)A�ZA��A�M�A�hsA˝�A�\)A�"�A�A�$�A�ZA�x�A�C�Aĩ�A�n�A�I�A�n�A��`A���A�`BA�VA�A��A���A���A�A�A���A�33A�7LA��A��yA��A�x�A�A��9A�K�A���A���A���A�v�A�{A�$�A��RA�r�A�O�A�(�A��A��/A���A�{A�33A��wA���A�%A�  A���A�ffA���A�
=A�
=A���A���A��A�v�A�ffA�A�A��A��A��9A�1A���A�33A�1'A���A�l�A���A��A~ZA}��A| �Az(�Ay7LAxv�Aw�wAw|�Avz�Au��AuAs��As+Ar��ArAp{AoK�An^5Am��AmoAmAlJAh��Ag\)Af�Af�\AeAe��Ae?}Ac�-AbVAa�Aa\)A`��A_�A_hsA_VA^�A^bA\��AZZAX�/AXJAU�#AU%AU&�AUVAS��AR�AQ�wAP-ANĜANz�AN^5AN9XAM��AM33AM�AMVAL�ALn�AJ�/AI+AHbAF��AE"�AE
=AD�AD1'AB~�AA��A@��A?33A>��A>n�A<�HA:n�A9VA8VA7�
A7VA6r�A6bA5��A4��A3�7A2^5A1�;A0��A/;dA.��A-�A,�A*��A*$�A)hsA(��A'��A'"�A%�^A%33A$E�A#?}A"�DA"VA"(�A!��A ĜA Q�A A�A�A�/A��A��A5?A�mA�A7LAȴA�hA�A�DA�AjA�FA�+A�A��AAS�A%A��A=qAG�AM�A�-A �A�yA1'A��A
��A	�^A��A|�A�#A�DA��AC�A��A�yA�jA��A5?A �`A   @��H@��@��7@�Ĝ@�9X@���@�o@��7@�V@��@�j@���@�+@��/@�hs@웦@�Q�@�9X@띲@�-@�%@��
@�&�@�u@�Q�@�ƨ@�E�@�7@��@ߍP@܃@�5?@�z�@���@ӕ�@љ�@�9X@�^5@͡�@��@̴9@�r�@� �@ˍP@���@�J@�z�@ǝ�@ƸR@��@�  @�\)@��@���@���@��@��@�+@�7L@��@�I�@��
@�C�@��#@��
@�\)@�S�@�S�@�S�@�o@���@���@�J@�p�@�V@���@�1'@��
@��P@�@���@�%@�z�@�1@��P@�S�@�o@���@�5?@��@��-@�`B@�V@���@���@�@�Ĝ@��@�+@�"�@�o@�@��@��!@�-@��@��@�I�@��@��F@�K�@�33@��@���@��y@��H@��!@���@�^5@��^@���@�/@���@��u@�bN@��@�K�@��y@�n�@�@�O�@��@�V@�r�@�I�@�I�@���@�33@�5?@��7@�V@�9X@�  @���@��
@���@�"�@�S�@�dZ@�;d@���@��@�v�@�5?@�=q@�-@��@�G�@��D@�z�@�A�@��F@�S�@���@��@���@��m@�ȴ@�\)@�C�@�-@�@�`B@��@���@��@�1'@���@�\)@�"�@��H@�$�@�`B@��j@���@���@�K�@�"�@���@���@�n�@�^5@�M�@�=q@�{@�J@�{@�{@���@���@���@��7@��@��7@�x�@��7@��h@�&�@���@��j@���@��@�bN@�9X@�1@�"�@�E�@��@���@�p�@�X@�X@�V@��/@���@��@���@���@��u@��@�9X@�;@|�@;d@+@�@~�y@~ȴ@~�R@~ff@}`B@|(�@{S�@z�@y�7@y7L@y%@y%@y%@x��@x�`@xĜ@x��@x��@x�u@xr�@xr�@xr�@xbN@xbN@x1'@w��@w
=@vff@vȴ@v�+@vff@u�@uV@sC�@qG�@p�@p�@pA�@o�P@o;d@n�y@n5?@m�h@l��@mV@l�/@m�@mO�@m��@nv�@n��@n{@m��@m�h@m�-@mp�@m?}@mV@l�@kC�@j�@j�@j�!@j��@j=q@i�7@i�@h��@h�9@hQ�@h1'@g�@g�P@fv�@e�@eV@d��@d�@d�/@d�/@d�j@dj@c��@c�
@c��@cS�@co@b�@b�\@a�^@aX@a�@`�`@`�9@`r�@`1'@`b@`  @_��@_|�@_
=@^�@^V@^5?@]�@]��@\��@\�@[�F@[dZ@Z^5@Y��@YG�@XĜ@W�@W�P@W;d@V�@V{@U�-@U��@U�h@Up�@UO�@U�@TI�@S�@SC�@R��@R-@Q��@Q��@P�`@P1'@O|�@O
=@N��@N�@N�R@Nv�@NE�@N$�@N@M�T@M@M�h@M�@L��@L1@K��@KdZ@K"�@J�@J�\@J-@JJ@I�^@I��@IG�@I�@H��@HQ�@H �@G�@G�w@G�w@G�P@F��@FE�@E�@E@E�@E?}@E/@E�@D��@D��@Dz�@Dz�@DI�@C��@C��@Ct�@CdZ@CS�@CC�@C"�@B�H@B�!@B��@B^5@A�@A�^@Ax�@AG�@A�@@��@@�`@@�@?�@?��@?K�@>�@>V@=@=?}@<��@<��@<�D@<I�@<9X@<(�@;��@;t�@;33@;@:�H@:��@:�!@:��@:�\@:�\@:^5@:�@9�^@9hs@9�@8A�@7�P@7+@6�@6ȴ@6�R@6��@6ff@6@5`B@5�@4�j@3�m@3t�@3C�@3S�@2�@2M�@2J@1�@1��@1��@1�7@1x�@1X@1X@1G�@1&�@0��@0�`@0�u@0Q�@/�w@/|�@/|�@/l�@/\)@/;d@.ȴ@.�R@.��@.�+@.V@.{@-@-O�@-/@,�@,��@,j@,Z@,Z@,I�@,I�@,9X@,�@,1@+��@+�
@+t�@*�@*�\@*~�@*~�@*J@)��@)��@)�7@)x�@)X@)�@)%@)%@(��@(��@(�`@(r�@(A�@(1'@( �@'�;@'�@'��@'��@'��@'|�@'+@&��@&�y@&ȴ@&�R@&�+@&v�@&V@&$�@%�h@$�j@$z�@#��@#ƨ@#ƨ@#�F@#�F@#�@#t�@#dZ@#33@"�H@"�!@"�!@"��@"��@"�\@"�\@"=q@!��@!��@!x�@!G�@ ��@ �`@ Ĝ@ �u@ A�@   @�w@l�@+@
=@�y@�+@E�@�@��@`B@/@��@�j@�D@I�@(�@�m@�@dZ@S�@"�@��@n�@=q@=q@-@��@�@�#@��@�^@��@��@X@&�@��@Ĝ@�@A�@��@|�@;d@��@��@v�@E�@$�@@�T@��@�-@��@�@O�@�@O�@/@V@�@�/@��@��@z�@9X@��@t�@C�@�@�\@J@�@��@x�@hs@&�@Ĝ@��@r�@bN@1'@ �@�w@�@��@�P@|�@K�@
=@�y@�R@��@ff@�@p�@`B@`B@`B@��@�/@�j@��@�D@�D@j@Z@(�@(�@1@ƨ@��@��@�@S�@S�@S�@C�@"�@
��@
�!@
��@
��@
��@
�\@
^5@
�@	��@	��@	�@	��@	�7@	hs@	7L@�`@��@�@A�@�@��@��@�w@�w@�@��@�P@l�@+@
=@�y@��@v�@ff@V@5?@�T@�T@��@��@@��@�h@p�@O�@�@�/@��@��@�j@�j@�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B~�B~�B~�B}�B}�B}�B}�B~�B~�B� B�+B��B�-B�dB�jB�qB�dB�?B�!B��B��B�+Bz�BjBP�B;dB#�BoBDB��B�B�ZB�BƨB�LB��B��Bx�BjB`BBVBD�B=qB/B�B{B+B��B�B�`B�#B�B��B��B��B��B�B��B�{B�1B~�B|�Bu�BdZB`BBYBN�BE�B5?B.B�B�BPBJB
=B%BB  B
��B
�B
�sB
�B
��B
ɺB
ŢB
�FB
��B
��B
��B
�7B
|�B
u�B
o�B
m�B
ffB
aHB
]/B
T�B
O�B
L�B
G�B
9XB
33B
-B
&�B
!�B
�B
�B	��B	�sB	�ZB	�NB	�)B	�yB	�ZB	��B	ƨB	�qB	�wB	ĜB	�}B	ĜB	ŢB	B	�jB	�3B	��B	�+B	� B	m�B	n�B	p�B	w�B	n�B	ffB	`BB	\)B	VB	S�B	T�B	W
B	T�B	Q�B	P�B	O�B	N�B	L�B	D�B	<jB	49B	-B	%�B	#�B	"�B	!�B	�B	�B	uB	DB	1B	DB	
=B��B�B�B�sB�TB�5B�;B�;B�5B�B�B��B��B��B��BɺBĜB�jB�RB�FB�9B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�oB�bB�\B�PB�DB�=B�B�B~�B|�Bv�Bt�Bo�Bm�Bk�BgmBdZBdZBcTBaHB^5B[#BZBYBS�BR�BP�BN�BM�BJ�BH�BE�BC�BB�B@�B?}B?}B?}B?}B>wB:^B6FB5?B49B33B33B33B2-B2-B2-B2-B1'B1'B1'B/B/B-B+B+B+B+B(�B(�B&�B'�B%�B%�B$�B%�B$�B$�B#�B%�B%�B$�B%�B'�B'�B'�B(�B)�B)�B)�B(�B(�B)�B)�B+B,B-B-B-B33B2-B33B33B33B49B5?B8RB;dB<jB=qB=qB>wBB�BE�BF�BG�BF�BF�BH�BI�BI�BK�BL�BL�BL�BK�BK�BK�BM�BN�BO�BO�BP�BQ�BQ�BQ�BQ�BR�BQ�BS�BS�BT�BZB]/B_;BdZBk�Bn�Bn�Bo�Bo�Bo�Bq�Bs�Bs�Bt�Bv�Bw�Bz�B|�B|�B�B�1B�=B�PB�\B�bB��B��B��B��B��B��B��B��B�B�B�B�B�'B�-B�3B�?B�FB�FB�RB�jB�wB�}B�}B��B��B��BĜBŢBǮB��B��B��B��B��B��B��B�B�B�B�#B�5B�5B�BB�NB�ZB�sB�yB�`B�`B�fB�B�B��B��B��B��B��B��B��B��B��B	  B	B	B		7B	JB	\B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	%�B	&�B	&�B	&�B	'�B	+B	/B	1'B	33B	33B	5?B	5?B	5?B	5?B	5?B	6FB	7LB	;dB	E�B	L�B	N�B	R�B	T�B	VB	YB	^5B	aHB	cTB	dZB	e`B	e`B	e`B	ffB	l�B	o�B	t�B	w�B	w�B	w�B	x�B	x�B	x�B	x�B	{�B	~�B	�B	�%B	�1B	�1B	�7B	�7B	�7B	�7B	�7B	�=B	�=B	�=B	�DB	�DB	�DB	�DB	�DB	�DB	�JB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�9B	�?B	�FB	�XB	�qB	�wB	��B	B	ÖB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�;B	�NB	�TB	�ZB	�ZB	�`B	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
1B
	7B

=B

=B

=B
DB
DB
DB
JB
JB
JB
JB
PB
VB
\B
bB
hB
hB
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
-B
-B
.B
.B
.B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
33B
49B
49B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
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
<jB
=qB
=qB
>wB
>wB
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
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
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
F�B
F�B
F�B
F�B
F�B
G�B
F�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
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
N�B
N�B
N�B
N�B
N�B
N�B
N�B
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
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
XB
ZB
ZB
YB
YB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
]/B
^5B
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
aHB
`BB
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
bNB
bNB
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
dZB
e`B
e`B
e`B
e`B
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
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BBBB}�B~B~B~BBHB�B�B��B��B��B�"B�wB�B�fB��B�kB�KB�xB�[Br�BX_BBB(sB�B�B�B�!B�B��B��B��B�QB�#B{�Bl�Bb�BX�BFtB@iB2B�B�B	�B�<B�B�RB�)B֡B�hB��B�\B�{B��B�BB�9B�7B�B~�Bw�Be,Ba|BZ�BP�BG�B6�B0!B!HB9B�B�B
�B�BBoB
��B
��B
�B
�WB
��B
�DB
�B
�B
�QB
��B
��B
�XB
}�B
v�B
pUB
n�B
gmB
bhB
^�B
U�B
P}B
N<B
I�B
:�B
4TB
-�B
'�B
"�B
!�B
�B	�qB	�*B	��B	� B	ܒB	�B	�B	�uB	ǔB	��B	�cB	��B	��B	�B	�?B	��B	�wB	��B	�xB	��B	�'B	n}B	n�B	qvB	yXB	o�B	g�B	bB	]�B	V�B	TFB	UgB	W�B	UgB	R B	QB	PbB	O�B	N�B	F�B	>B	6+B	.�B	&2B	$tB	$B	#�B	�B	�B	2B	0B		B	�B	B��B�B�WB�B�&B�B�B��B��BڠB�BּBּB�BϫB�)B�B��B�rB�LB��B�aB��B�B�QB�0B��B�NB�HB��B��B�CB�B�EB�?B��B��B��B�B��B��B��B�0B��B��B��B�4B~�BxBv+Bp�Bo5Bl�Bh
Bd�Bd�BdZBb�B_�B\xB\)BZ�BU2BS�BR:BP}BO\BL�BJ�BGEBD�BCGB@�B?�B?�B@B@�B@B;�B7fB7B5�B3�B3�B3�B2�B3B2�B2�B1�B1�B2GB0�B1B-�B+QB+QB+�B,B)�B*0B(sB(sB&fB&�B%�B&�B%�B&B%�B'�B'RB&�B(
B)DB)B)B)�B*eB*KB*eB)yB)�B*�B*�B,B,�B-�B-�B.cB3�B2�B3�B3�B3�B5?B6�B9�B;�B<�B=�B>(B?�BC�BE�BF�BG�BF�BF�BIBJ	BJXBLJBM6BM6BM6BL0BL0BL~BN�BOvBPbBPHBQNBR:BR:BRTBR:BS@BRTBT{BT�BVBZ�B]�B`BBe,Bk�Bn�Bn�Bo�Bo�Bo�BrBtBtTBuZBwBxB{0B}"B}<B�GB�1B�rB��B��B��B��B��B�5B�4B��B�2B�RB�yB��B��B��B�iB�vB�aB��B��B��B��B�$B�<B��B��B� B��B��B��B��B��BǮB��B�B�4B� B�aB�MB�2B�SB�mBևBۦB�OBބB�B�B�B�DB�KB��B��B�LB��B�aB�B�B�B�8B�B�>B�^B�"B�.B	 OB	�B	�B		�B	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	%�B	'B	'B	'B	(
B	+B	/5B	1'B	3MB	3�B	5�B	5ZB	5ZB	5ZB	5tB	6zB	7�B	<B	F%B	MB	O(B	SB	UB	VB	YKB	^OB	abB	c�B	d�B	ezB	e�B	ezB	f�B	l�B	o�B	t�B	xB	w�B	w�B	y	B	y	B	y	B	y>B	|jB	cB	��B	�tB	�fB	�KB	�RB	�7B	�RB	�lB	�RB	�rB	�XB	�XB	�^B	�DB	�DB	�^B	�^B	��B	�~B	��B	��B	��B	��B	��B	�B	�!B	�pB	�\B	�B	��B	��B	�B	��B	��B	� B	� B	�B	��B	�B	��B	��B	�B	�B	�9B	�tB	�zB	�rB	�qB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�&B	�,B	�{B	�aB	�B	�B	�B	�
B	�
B	�$B	�EB	�EB	�7B	�=B	�]B	�dB	�OB	�jB	ߊB	�hB	�B	�tB	�tB	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	� B	��B	��B	��B	�B	�B	��B	�	B	�>B	�B	�B	�0B	�PB	�B	�B	�B	�B	�.B	�HB
 OB
oB
-B
aB
MB
SB
SB
�B
zB
fB
	lB

XB

XB

XB
^B
xB
xB
dB
dB
~B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
!�B
!�B
"B
"�B
$B
$&B
$&B
%B
&B
'B
'B
(
B
($B
(
B
(
B
)B
)*B
*B
*B
*B
+B
+B
+B
+B
,B
,=B
-)B
-CB
./B
.IB
.}B
/OB
0UB
0;B
1'B
1[B
1AB
1AB
1[B
2aB
3MB
4nB
4�B
6zB
6zB
6`B
6zB
6�B
7fB
7fB
7fB
7�B
7�B
8lB
7fB
8RB
8lB
8lB
8�B
8lB
8lB
9rB
9�B
:xB
:xB
:^B
:�B
:xB
;B
;B
;�B
;�B
;B
<�B
=�B
=�B
>�B
>�B
?�B
?�B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
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
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
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
F�B
F�B
F�B
F�B
F�B
G�B
F�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
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
NB
N�B
N�B
N�B
N�B
N�B
OB
N�B
OB
OB
N�B
OB
N�B
N�B
O�B
O�B
O�B
O�B
P�B
Q B
P�B
QB
Q�B
R B
SB
S&B
S&B
S&B
S&B
SB
R B
T,B
UB
U2B
UB
U2B
U2B
V9B
VB
VB
W
B
W$B
XB
Z7B
Z7B
YKB
Y1B
[=B
[=B
[=B
[=B
[=B
\]B
\]B
\CB
\]B
\]B
]IB
]IB
]dB
]dB
^OB
^OB
]IB
^OB
^OB
^OB
^OB
^jB
^jB
^OB
^OB
_pB
`\B
`BB
`BB
`\B
`\B
`\B
`vB
`\B
aHB
`vB
a|B
a|B
abB
aHB
abB
abB
bhB
bhB
b�B
bNB
bhB
bhB
cnB
cnB
cTB
cnB
cnB
dtB
dZB
dZB
d�B
dtB
dtB
dtB
dtB
dtB
ezB
e`B
ezB
ezB
ezB
ezB
ezB
f�B
ffB
f�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
iyB
iyB
j�B
j�B
j�B
j�B
j�B
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
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608290037212016082900372120160829003721201806221212592018062212125920180622121259201804050405312018040504053120180405040531  JA  ARFMdecpA19c                                                                20160825093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160825003523  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160825003524  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160825003524  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160825003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160825003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160825003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160825003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160825003525  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160825003525                      G�O�G�O�G�O�                JA  ARUP                                                                        20160825012151                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160825153221  CV  JULD            G�O�G�O�F�-�                JM  ARCAJMQC2.0                                                                 20160828153721  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160828153721  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190531  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031259  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101518                      G�O�G�O�G�O�                