CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-07T00:35:12Z creation;2016-08-07T00:35:14Z conversion to V3.1;2019-12-19T08:33:40Z update;     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160807003512  20200115101518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_024                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��3��k 1   @��5'�} @;����m�dk��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@���A��A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB��B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  D   D � D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[y�D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx�fDy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�<�Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@�33A ��A33A@��A_33A33A���A���A���A���Aϙ�Aߙ�AB 33BffBffB��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CF�CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D3D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D�fD|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[vfD[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx�3Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�;3D�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�A�D�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�"�A� �A��A��A��A��A��A��A�{A�{A�oA��A��A��A���A�;dA�|�A���A��yA���A��\A��
A�~�A���A���A���A�ZA�v�A�I�A�A��A�=qA�&�A�l�A�C�A��A��A�bA�p�A��hA��-A�ȴA�=qA�r�A��A���A�x�A�dZA�VA��TA�jA�ȴA�$�A�1'A�?}A�dZA��DA��
A�`BA�^5A��TA��^A��7A�t�A�bNA�bNA�ZA�E�A��#A��A�7LA��;A�v�A�x�A��A���A�5?A�VA��A�A���A�\)A�%A�hA\)A~�A~�A|jAz1Aw�Avv�Au�Au�PAtE�Ar�Ar$�Ap��AodZAn5?Al��AkoAj��Ai�Ag�Ac�FAb9XAa��Aa&�A^��A\1A[AZAYt�AXȴAXE�AW�AW\)AWoAV�`AV�HAV��AV�RAV(�AUO�ASARQ�AP��AOXANjALQ�AK
=AJ�AJQ�AJ1'AI��AI��AI�FAH-AG��AGx�AF�`AF��AF�AC/A@ffA?&�A=��A;��A;K�A:(�A8I�A7��A7��A6v�A4�HA4ZA4ZA4bA3�hA2�jA1�mA0ĜA0  A/`BA//A.��A-��A,bA*��A*�A)?}A(~�A&�9A$��A$��A$bNA#�hA"�A Q�AE�A�At�AO�A"�A�A��AĜA�DA(�A�#AK�AĜAXAA�+AA�7A��AbNA=qA{AA��A�FA�PAA�A��A�A�;A��AƨAA%A	��A��A1A�-Al�A%A�A\)A��A�DA-AƨAr�AhsA �yA ȴ@�"�@��7@�I�@�K�@���@�r�@�"�@�$�@��@��m@�
=@�ff@�?}@��@�P@�V@홚@��@�l�@��@�
=@�@�@�j@�\)@�5?@�X@���@�A�@�ƨ@ޏ\@ݙ�@�`B@ܴ9@�t�@�v�@��@�j@�|�@֧�@ՙ�@�%@�Ĝ@ԃ@� �@�l�@�?}@�33@�?}@̣�@�J@ɩ�@ɑh@ȓu@��@ǥ�@�o@��@�=q@�r�@�~�@���@��@�t�@�~�@���@��`@��9@�z�@���@���@��-@��`@�1@��@���@�?}@�9X@���@�C�@�~�@�E�@��@���@���@�j@��F@�+@�@�ȴ@���@��D@��H@��@�z�@���@�o@�5?@�V@���@��u@�\)@�^5@�J@�`B@�&�@���@�bN@��@�|�@��@��@��y@�ȴ@���@���@�$�@�7L@�V@��@��/@���@��/@�Ĝ@�j@�b@��@�33@��y@���@�5?@��+@��\@�n�@�$�@��@�j@���@���@�X@�I�@��;@�\)@�K�@�C�@�+@��R@�ff@��T@���@�x�@�V@�I�@��w@�K�@�;d@��H@�=q@�5?@��@���@��@��D@�  @��;@�ƨ@�t�@�+@��y@��H@���@���@��\@�v�@�ff@�M�@�$�@��@�x�@�V@���@���@���@���@���@��u@���@��u@�I�@���@�o@�+@�33@�o@�33@�\)@��@�V@�5?@�5?@�-@���@��-@��^@���@��@�Z@�b@�9X@�Q�@�ƨ@�K�@��\@��@��^@�/@��/@��j@�Z@�bN@�r�@�j@�1'@�(�@�1'@�I�@���@�(�@�  @��@��@�  @�9X@�P@~��@~$�@}O�@z�\@zM�@{�@}�@}�@}?}@|�/@|�@{��@{ƨ@{33@z��@zM�@x��@y�@z=q@y��@yG�@x�9@x��@xr�@wK�@v@u�h@v{@v��@v�@w�@wl�@w�@w�;@xb@x  @u�-@t9X@r��@r=q@r=q@rM�@qG�@qX@q&�@q��@q�^@q�#@r-@r^5@r�\@r^5@q��@qx�@pA�@o��@o�@o|�@oK�@nff@n@m`B@mV@l�/@l�@lZ@l1@kƨ@kC�@j=q@i�^@i�7@iG�@iG�@i��@hĜ@h �@hr�@hr�@hQ�@h �@g��@gK�@fv�@f$�@e@e��@e�h@e�@ep�@e�@c�m@c�@cC�@co@b�@b��@b�!@bn�@b=q@a�7@`A�@_�w@_��@_�w@_�P@_\)@^�@^�+@^V@^@]�@]@]`B@]�@\�/@\Z@[�F@[dZ@["�@[o@Z�@Z�!@Zn�@ZM�@Z�@Yx�@Yhs@Y&�@XĜ@X�9@X�u@XbN@X1'@W�;@W+@V�R@Vv�@V@U�T@U��@U��@U�@U�@Up�@U`B@UO�@T�j@T�@Tj@TI�@T�@S�m@S�
@Sƨ@SdZ@S@R�H@R��@RJ@Q��@Qhs@QG�@Q&�@P�`@PĜ@PbN@O�;@O�P@OK�@Nff@N@M�@M�@M��@M�-@M�-@M��@M�h@M`B@M�@L��@Lz�@K��@K�F@K33@J�@J�H@J��@J~�@J-@J�@I�@Ihs@I�@H�`@H�@HA�@G�;@G�P@GK�@G�@F��@F@E@E`B@D�/@Dj@D1@C��@C33@Co@B��@B~�@B-@A�7@A%@@�`@@��@@Ĝ@@��@@�@@ �@?�w@?l�@?�@>�@>v�@=�@=@=��@=`B@=/@<�@<�@;��@;ƨ@;dZ@;"�@:�H@:��@:-@9�#@9�^@9x�@9%@8Ĝ@8�@81'@8b@7�@7+@7
=@6�@6��@6$�@5�T@5�h@5O�@5O�@5?}@5/@5/@5�@5V@4��@4�/@4��@4�j@4�@4�D@4j@49X@41@3�
@3t�@2��@2-@2J@1�^@1��@1��@1&�@0�`@0��@0Ĝ@0r�@01'@0 �@/��@/��@/l�@/K�@/�@.�@.v�@.V@.@-��@-�-@-��@-�h@-p�@-`B@-O�@-�@-V@,�@,��@,�j@,�@,��@,z�@,(�@,1@+ƨ@+�F@+��@+�@+dZ@+o@*�H@*^5@*=q@*=q@*=q@*-@*-@*-@*-@*�@*J@)��@)�7@)&�@(��@(r�@(1'@( �@(  @'�@'�;@'l�@'K�@'+@'
=@&�y@&��@&E�@&{@&@%�@%`B@%�@$��@$�D@$j@#�
@#�@#"�@"�@"�@"��@"��@"��@"n�@!��@ �`@ Q�@   @�w@�P@l�@K�@K�@K�@K�@K�@K�@;d@�@�@�@
=@ȴ@ff@5?@�@��@�h@V@j@�@��@S�@"�@�@�!@��@�\@~�@=q@J@�^@X@G�@7L@�u@r�@r�@�@�@r�@Q�@�;@��@�w@��@l�@;d@�R@E�@$�@@�-@`B@`B@O�@/@��@z�@1@�
@��@S�@"�@�@��@�\@n�@-@��@�#@��@��@��@7L@&�@%@��@Ĝ@�9@��@��@bN@bN@Q�@1'@�@�w@�@��@|�@\)@;d@+@�y@@��@�-@��@��@�h@p�@`B@`B@`B@O�@/@V@��@�/@��@�@�@�@��@z�@z�@Z@(�@�@�@�m@�
@�F@��@��@dZ@33@"�@@
��@
��@
~�@
J@	��@	hs@��@��@Ĝ@�9@��@��@A�@1'@1'@ �@b@�;@K�@��@�R@��@�+@v�@V@E�@5?@5?@5?@5?@$�@{@{@{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�"�A� �A��A��A��A��A��A��A�{A�{A�oA��A��A��A���A�;dA�|�A���A��yA���A��\A��
A�~�A���A���A���A�ZA�v�A�I�A�A��A�=qA�&�A�l�A�C�A��A��A�bA�p�A��hA��-A�ȴA�=qA�r�A��A���A�x�A�dZA�VA��TA�jA�ȴA�$�A�1'A�?}A�dZA��DA��
A�`BA�^5A��TA��^A��7A�t�A�bNA�bNA�ZA�E�A��#A��A�7LA��;A�v�A�x�A��A���A�5?A�VA��A�A���A�\)A�%A�hA\)A~�A~�A|jAz1Aw�Avv�Au�Au�PAtE�Ar�Ar$�Ap��AodZAn5?Al��AkoAj��Ai�Ag�Ac�FAb9XAa��Aa&�A^��A\1A[AZAYt�AXȴAXE�AW�AW\)AWoAV�`AV�HAV��AV�RAV(�AUO�ASARQ�AP��AOXANjALQ�AK
=AJ�AJQ�AJ1'AI��AI��AI�FAH-AG��AGx�AF�`AF��AF�AC/A@ffA?&�A=��A;��A;K�A:(�A8I�A7��A7��A6v�A4�HA4ZA4ZA4bA3�hA2�jA1�mA0ĜA0  A/`BA//A.��A-��A,bA*��A*�A)?}A(~�A&�9A$��A$��A$bNA#�hA"�A Q�AE�A�At�AO�A"�A�A��AĜA�DA(�A�#AK�AĜAXAA�+AA�7A��AbNA=qA{AA��A�FA�PAA�A��A�A�;A��AƨAA%A	��A��A1A�-Al�A%A�A\)A��A�DA-AƨAr�AhsA �yA ȴ@�"�@��7@�I�@�K�@���@�r�@�"�@�$�@��@��m@�
=@�ff@�?}@��@�P@�V@홚@��@�l�@��@�
=@�@�@�j@�\)@�5?@�X@���@�A�@�ƨ@ޏ\@ݙ�@�`B@ܴ9@�t�@�v�@��@�j@�|�@֧�@ՙ�@�%@�Ĝ@ԃ@� �@�l�@�?}@�33@�?}@̣�@�J@ɩ�@ɑh@ȓu@��@ǥ�@�o@��@�=q@�r�@�~�@���@��@�t�@�~�@���@��`@��9@�z�@���@���@��-@��`@�1@��@���@�?}@�9X@���@�C�@�~�@�E�@��@���@���@�j@��F@�+@�@�ȴ@���@��D@��H@��@�z�@���@�o@�5?@�V@���@��u@�\)@�^5@�J@�`B@�&�@���@�bN@��@�|�@��@��@��y@�ȴ@���@���@�$�@�7L@�V@��@��/@���@��/@�Ĝ@�j@�b@��@�33@��y@���@�5?@��+@��\@�n�@�$�@��@�j@���@���@�X@�I�@��;@�\)@�K�@�C�@�+@��R@�ff@��T@���@�x�@�V@�I�@��w@�K�@�;d@��H@�=q@�5?@��@���@��@��D@�  @��;@�ƨ@�t�@�+@��y@��H@���@���@��\@�v�@�ff@�M�@�$�@��@�x�@�V@���@���@���@���@���@��u@���@��u@�I�@���@�o@�+@�33@�o@�33@�\)@��@�V@�5?@�5?@�-@���@��-@��^@���@��@�Z@�b@�9X@�Q�@�ƨ@�K�@��\@��@��^@�/@��/@��j@�Z@�bN@�r�@�j@�1'@�(�@�1'@�I�@���@�(�@�  @��@��@�  @�9X@�P@~��@~$�@}O�@z�\@zM�@{�@}�@}�@}?}@|�/@|�@{��@{ƨ@{33@z��@zM�@x��@y�@z=q@y��@yG�@x�9@x��@xr�@wK�@v@u�h@v{@v��@v�@w�@wl�@w�@w�;@xb@x  @u�-@t9X@r��@r=q@r=q@rM�@qG�@qX@q&�@q��@q�^@q�#@r-@r^5@r�\@r^5@q��@qx�@pA�@o��@o�@o|�@oK�@nff@n@m`B@mV@l�/@l�@lZ@l1@kƨ@kC�@j=q@i�^@i�7@iG�@iG�@i��@hĜ@h �@hr�@hr�@hQ�@h �@g��@gK�@fv�@f$�@e@e��@e�h@e�@ep�@e�@c�m@c�@cC�@co@b�@b��@b�!@bn�@b=q@a�7@`A�@_�w@_��@_�w@_�P@_\)@^�@^�+@^V@^@]�@]@]`B@]�@\�/@\Z@[�F@[dZ@["�@[o@Z�@Z�!@Zn�@ZM�@Z�@Yx�@Yhs@Y&�@XĜ@X�9@X�u@XbN@X1'@W�;@W+@V�R@Vv�@V@U�T@U��@U��@U�@U�@Up�@U`B@UO�@T�j@T�@Tj@TI�@T�@S�m@S�
@Sƨ@SdZ@S@R�H@R��@RJ@Q��@Qhs@QG�@Q&�@P�`@PĜ@PbN@O�;@O�P@OK�@Nff@N@M�@M�@M��@M�-@M�-@M��@M�h@M`B@M�@L��@Lz�@K��@K�F@K33@J�@J�H@J��@J~�@J-@J�@I�@Ihs@I�@H�`@H�@HA�@G�;@G�P@GK�@G�@F��@F@E@E`B@D�/@Dj@D1@C��@C33@Co@B��@B~�@B-@A�7@A%@@�`@@��@@Ĝ@@��@@�@@ �@?�w@?l�@?�@>�@>v�@=�@=@=��@=`B@=/@<�@<�@;��@;ƨ@;dZ@;"�@:�H@:��@:-@9�#@9�^@9x�@9%@8Ĝ@8�@81'@8b@7�@7+@7
=@6�@6��@6$�@5�T@5�h@5O�@5O�@5?}@5/@5/@5�@5V@4��@4�/@4��@4�j@4�@4�D@4j@49X@41@3�
@3t�@2��@2-@2J@1�^@1��@1��@1&�@0�`@0��@0Ĝ@0r�@01'@0 �@/��@/��@/l�@/K�@/�@.�@.v�@.V@.@-��@-�-@-��@-�h@-p�@-`B@-O�@-�@-V@,�@,��@,�j@,�@,��@,z�@,(�@,1@+ƨ@+�F@+��@+�@+dZ@+o@*�H@*^5@*=q@*=q@*=q@*-@*-@*-@*-@*�@*J@)��@)�7@)&�@(��@(r�@(1'@( �@(  @'�@'�;@'l�@'K�@'+@'
=@&�y@&��@&E�@&{@&@%�@%`B@%�@$��@$�D@$j@#�
@#�@#"�@"�@"�@"��@"��@"��@"n�@!��@ �`@ Q�@   @�w@�P@l�@K�@K�@K�@K�@K�@K�@;d@�@�@�@
=@ȴ@ff@5?@�@��@�h@V@j@�@��@S�@"�@�@�!@��@�\@~�@=q@J@�^@X@G�@7L@�u@r�@r�@�@�@r�@Q�@�;@��@�w@��@l�@;d@�R@E�@$�@@�-@`B@`B@O�@/@��@z�@1@�
@��@S�@"�@�@��@�\@n�@-@��@�#@��@��@��@7L@&�@%@��@Ĝ@�9@��@��@bN@bN@Q�@1'@�@�w@�@��@|�@\)@;d@+@�y@@��@�-@��@��@�h@p�@`B@`B@`B@O�@/@V@��@�/@��@�@�@�@��@z�@z�@Z@(�@�@�@�m@�
@�F@��@��@dZ@33@"�@@
��@
��@
~�@
J@	��@	hs@��@��@Ĝ@�9@��@��@A�@1'@1'@ �@b@�;@K�@��@�R@��@�+@v�@V@E�@5?@5?@5?@5?@$�@{@{@{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B[#B[#B[#B[#B[#B[#B[#B[#B[#B[#B[#B[#B[#B[#B[#BXBO�BP�BW
B\)B@�B.B"�B\B�B�B�sB�HB�B��B�LB�qB�}B�9B�'B�B��B��B~�Bp�BYBK�B;dB0!B'�B#�B�BhBB��B�B�B�BB��B�9B��B�{B�=Bw�Be`B^5B[#BXBW
BVBVBT�BR�BM�B5?B#�B�B{B
��B
�;B
ȴB
�}B
�qB
�^B
�RB
�?B
�-B
�B
��B
��B
��B
��B
�VB
}�B
hsB
]/B
YB
T�B
L�B
A�B
;dB
49B
)�B
�B
{B
1B
B	��B	�B	��B	��B	ƨB	B	�?B	��B	��B	��B	�{B	�bB	�PB	�=B	�7B	�+B	�%B	�B	�B	�B	�B	{�B	r�B	k�B	cTB	\)B	VB	O�B	G�B	D�B	C�B	B�B	A�B	@�B	>wB	;dB	5?B	49B	2-B	/B	,B	"�B	\B	+B	%B��B��B�B�mB�ZB�ZB�BB�)B�B�B�B�B��B��B��B��BȴBȴBƨBÖB�dB�9B�9B�3B�!B�B��B��B��B��B��B�uB�=B�+B�%B�B�B�B�B�B� B}�B|�Bz�Bt�Bm�Bk�Bk�BffBgmBgmBffBffBffBe`Be`BcTBaHBbNB^5B]/BZBVBS�BM�BI�BG�BD�BB�BA�BA�B@�B>wB;dB;dB:^B:^B:^B9XB7LB6FB5?B49B1'B2-B2-B33B0!B/B.B-B-B,B,B-B,B-B,B,B,B+B/B/B-B,B+B,B+B)�B)�B(�B(�B(�B'�B'�B'�B'�B&�B'�B(�B'�B(�B(�B(�B'�B'�B'�B'�B&�B&�B'�B&�B,B+B+B+B+B+B+B)�B+B.B0!B1'B33B6FB7LB8RB8RB8RB8RB9XB:^B;dB<jB>wB?}B@�BA�BD�BE�BF�BH�BH�BH�BI�BJ�BJ�BM�BN�BN�BN�BR�BS�BZB]/B_;BaHBcTBe`BjBjBjBp�Bz�B}�B�%B�DB�VB�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�9B�?B�^B�wB��B��B��B�}B�wB�jB�XB�FB�FB�XB�dB�dB�dB�jB�jB�}B��B��BBĜBȴB��B��B��B��B��B��B�
B�B�/B�ZB�`B�fB�yB�B�B�B�B�B�B�B�B�B��B��B��B	B	B	+B		7B	
=B	
=B	
=B	DB	bB	{B	�B	�B	 �B	$�B	%�B	&�B	+B	0!B	49B	6FB	6FB	7LB	;dB	<jB	@�B	B�B	H�B	H�B	G�B	I�B	J�B	J�B	I�B	I�B	H�B	I�B	K�B	L�B	N�B	Q�B	R�B	S�B	T�B	VB	XB	YB	ZB	^5B	`BB	_;B	_;B	`BB	aHB	aHB	e`B	cTB	cTB	e`B	bNB	dZB	iyB	o�B	r�B	s�B	s�B	v�B	w�B	z�B	z�B	z�B	z�B	y�B	|�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�JB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	�!B	�3B	�3B	�?B	�FB	�RB	�RB	�XB	�dB	�jB	�qB	�wB	�}B	��B	B	B	ÖB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�NB	�`B	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
1B
	7B
	7B

=B
JB
PB
PB
PB
PB
VB
VB
VB
VB
VB
\B
\B
bB
hB
hB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
#�B
$�B
$�B
%�B
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
+B
+B
,B
,B
,B
-B
-B
.B
.B
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
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
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
I�B
I�B
I�B
I�B
J�B
K�B
K�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
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
VB
VB
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
ZB
ZB
ZB
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
]/B
]/B
]/B
]/B
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
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
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
ffB
gmB
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
hsB
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
k�B
k�B
k�B
k�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B[#B[#B[#B[#B[#B[=B[#B[#B[#B[#B[#B[=B[�B\CB^�B^�BW�BX�BbBeFBD�B2-B(�B�B�hB�B�B��B��B��B��B��B��B��B�GB� B��B�:B�aBtnB[�BN�B=B1B(�B%B!-B�BAB�*B�TB�B�B�_B�zB�B��B�6By�BffB^�B[�BXEBW$BV9BVSBU�BT�BQ4B72B%FB�B�B
�VB
��B
ɺB
�B
��B
��B
��B
��B
��B
��B
�sB
��B
��B
��B
�4B
��B
i�B
^B
Y�B
V�B
NpB
B�B
=B
6B
+�B
�B
9B
	7B
�B
;B	�ZB	ּB	ˬB	�1B	ŢB	��B	�2B	��B	�_B	�MB	�4B	�B	��B	��B	�_B	�?B	�SB	��B	�B	��B	~]B	tB	m�B	d�B	]�B	XyB	Q4B	HfB	D�B	C�B	B�B	A�B	A B	@B	<B	5�B	5%B	2�B	0�B	/�B	%�B	NB		7B	�B��B�`B�B�$B�B��B��B��B�EBخB��B�?B�FB�HB��B�~B�7BɆB�1B��B�"B�ZB��B��B��B� B�@B�tB�&B�|B��B��B�)B��B�tB�mB�gB�aB�AB��B��B~�B~]B}�Bv`BncBlWBlqBgBhXBg�Bf�Bf�Bf�Be�Be�BdBb�BcTB_pB^�B[�BW�BVBO�BK�BH�BE�BC-BB'BBuBA�B?�B<PB;�B;B;dB<B:�B8B6�B6�B5ZB2-B3B3MB4TB1'B/�B.�B-�B-�B,�B,�B-�B,�B-�B,�B,�B-CB,�B0oB0B-�B,qB+�B,�B+�B*�B*eB)�B)�B)�B(XB(�B(�B(�B'�B(�B)�B(�B)�B)yB)_B(sB(sB(�B)yB(sB($B(�B(sB,qB+QB+�B+kB+kB+�B+kB*�B,WB/OB0�B2-B4B6�B7�B8�B8�B8�B9	B:B;B<6B=<B?HB@OBA BB[BEBFBG+BH�BIBI7BJXBKDBKxBN<BOBBO\BO�BS�BU2B[=B]�B_�Ba�BdBf2Bj�Bj�BkkBqvB{JB~wB�YB��B��B��B��B��B��B��B��B��B�B�;B�nB�
B�*B�B�*B�*B�*B�DB�KB��B�iB��B��B�nB�ZB�xB��B��B� B�UB�B�HB��B�*B��B��B��B�B��B��B��B��B��B��B�B�-B�B�B��B�B�6B� B�:B�[B�sB�BݘB�tB�B�B�B��B��B��B�B�B�B��B��B��B�B�2B�BB	GB	SB	_B		RB	
XB	
XB	
XB	xB	�B	B	�B	�B	 �B	$�B	%�B	'B	+kB	0�B	4nB	6`B	6zB	7�B	;B	<�B	@�B	B�B	IB	IB	G�B	I�B	KDB	K)B	J=B	J=B	IB	J=B	K�B	MB	O(B	Q�B	R�B	T,B	U2B	VB	XB	YB	ZB	^�B	`\B	_VB	_;B	`BB	aHB	a�B	e�B	c�B	c�B	f2B	bNB	c�B	iB	o�B	r�B	s�B	tB	v�B	w�B	{B	{B	{0B	{0B	y�B	|�B	�;B	�[B	�MB	�SB	�SB	��B	��B	�KB	�B	�0B	�BB	�bB	�TB	�{B	��B	��B	��B	�qB	�OB	�OB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�6B	�=B	�QB	�kB	�KB	�=B	�!B	�hB	��B	�tB	�zB	�lB	��B	�rB	�B	��B	��B	��B	��B	��B	ªB	ªB	ÖB	ŢB	��B	�	B	��B	��B	��B	��B	�B	�&B	�MB	�9B	�?B	�EB	�EB	�KB	�1B	�KB	چB	�IB	�IB	�IB	�OB	�jB	�jB	�OB	ބB	ޞB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�	B	�	B	��B	�B	�DB	�B	�B	�<B	�B	�B	�B	�B	�B	�B	�B	�.B	�.B
 B
 4B
 B
 B
 B
'B
 B
AB
AB
GB
GB
MB
mB
?B
?B
?B
?B
EB
�B
fB
	RB
	lB

�B
dB
PB
PB
jB
jB
VB
pB
pB
pB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
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
"�B
#B
$B
$�B
$�B
%�B
&2B
'8B
'B
(
B
(
B
(
B
(
B
)B
)*B
*0B
+B
+6B
,"B
,=B
,=B
-)B
-CB
./B
.IB
/5B
/OB
/5B
0;B
0oB
1vB
2GB
2GB
2GB
2aB
3hB
4TB
4TB
49B
49B
49B
4TB
5?B
5ZB
5?B
5ZB
5ZB
5?B
5ZB
5ZB
5ZB
5ZB
6`B
6`B
6zB
7�B
8lB
8lB
9rB
9rB
9rB
9�B
:xB
:xB
:xB
:�B
;B
;�B
;B
;B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
>wB
>wB
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?}B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
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
I�B
I�B
J	B
J	B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
NB
M�B
M�B
NB
M�B
M�B
M�B
M�B
NB
M�B
N"B
OB
O�B
PB
O�B
O�B
O�B
QB
P�B
P�B
Q B
QB
QB
R B
RB
RB
R B
R B
R�B
R�B
R�B
R�B
SB
S&B
S&B
TB
T�B
U2B
UB
UB
U2B
VB
VB
VB
V9B
VB
VB
V9B
VB
W$B
W?B
WYB
X+B
X+B
X+B
YKB
YKB
YKB
ZB
ZQB
Z7B
[=B
[=B
[WB
\CB
\CB
\CB
]/B
]dB
]IB
]/B
]IB
]/B
]IB
]dB
]/B
]IB
]IB
]IB
^jB
^OB
^5B
^OB
^OB
_VB
_pB
_pB
`�B
a|B
a|B
aHB
aHB
aHB
abB
aHB
aHB
aHB
abB
abB
a|B
aHB
a|B
aHB
a|B
bhB
aHB
bhB
bhB
bhB
bhB
bhB
cnB
cTB
cnB
cTB
dtB
dZB
d�B
dtB
dtB
dtB
dtB
d�B
ezB
ezB
e�B
e�B
e�B
f�B
gmB
gmB
gmB
g�B
gmB
g�B
hsB
hsB
hsB
h�B
h�B
h�B
i�B
j�B
j�B
jB
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<0�|<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608110036072016081100360720160811003607201806221212082018062212120820180622121208201804050404352018040504043520180405040435  JA  ARFMdecpA19c                                                                20160807093504  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160807003512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160807003512  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160807003512  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160807003513  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160807003513  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160807003513  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160807003513  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160807003513  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160807003514                      G�O�G�O�G�O�                JA  ARUP                                                                        20160807012037                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160807153529  CV  JULD            G�O�G�O�F�	�                JM  ARCAJMQC2.0                                                                 20160810153607  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160810153607  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190435  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031208  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101518                      G�O�G�O�G�O�                