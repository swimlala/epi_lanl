CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-11-14T00:35:31Z creation;2016-11-14T00:35:33Z conversion to V3.1;2019-12-19T08:21:49Z update;     
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
resolution        =���   axis      Z        h  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  sh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20161114003531  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               :A   JA  I2_0577_058                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @����� 1   @����r @3/4֡a��d�=�b�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DE��DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۃ3D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�C3D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��3B��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3CuٚCw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�fC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�fC���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE�fDF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fDہ�D۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�A�D�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��D�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aם�Aח�Aי�AדuAבhAבhA׏\AׅAׁA�ZA�bA�O�A���A���A���A�ȴAհ!AՓuA�?}A��A�{A�oA�
=A�1A�
=A�
=A�VA�oA�{A�ȴA��mA�z�A�33A���A�7LA��A�jA��A�-AþwA�ffA�  A+A�dZA���A�+A�%A��uA��jA��/A��hA�JA��jA�  A�^5A��A���A��^A�"�A�1'A�+A�p�A��/A�5?A�"�A��A��FA�A���A�\)A��A�%A�v�A���A�"�A�ffA���A�VA��A��A��A��yA�l�A��
A�K�A�ĜA�G�A��9A�ĜA�1'A�^5A�l�A�/A�7LA��mA�E�A��+A��A��A���A��A���A�C�A���A���A��AG�A|��Az-Ax��Ax1'Av �At5?Ap�uAn9XAm7LAkG�Ai�AhAe��AcAcdZAb9XA`�/A_��A]��A\��A[p�AZ�HAZz�AX�jAWx�AVJAT�ASS�AQ��APz�AP$�AOx�ANjAL��AJ�DAH(�AF~�AD-ABbNA@�yA@{A>A�A<jA9�mA9VA7K�A5�7A4 �A3
=A21A0��A/dZA.M�A-dZA+�A)O�A(��A'�FA&��A%33A$�!A#��A"�jA!�A!K�A ĜA��A�A��Ap�A��A�A��At�A��A�DA��A�A�^A�PA\)A�A��AjA�A�Al�A�A|�AO�A/A
��A
JA	\)Az�A�hA
=Av�A�;A��A I�@��7@�j@�ȴ@�E�@��@�t�@�V@��@�M�@�h@��@��m@���@��@�7@��@��@�\@�=q@�z�@ᙚ@�1@�ƨ@߅@�ƨ@��@�hs@�;d@�5?@�@�O�@�1'@�^5@�O�@�&�@��@ӕ�@ҏ\@�n�@�%@�dZ@�v�@���@�x�@�1'@��m@�"�@ʸR@�M�@���@ɡ�@�/@��
@Ɨ�@���@�Z@�S�@§�@�ff@�=q@��#@�&�@��j@��
@�V@�@��-@���@�I�@��m@�+@���@�^5@��T@�`B@�1'@�o@��H@�ȴ@���@�V@���@�hs@�X@��`@� �@�t�@���@���@�l�@���@�5?@�hs@��j@�Z@�1@���@��R@�-@��@���@�p�@�p�@��T@�Ĝ@��m@�C�@�E�@���@���@�G�@�%@���@���@���@��
@��w@��@��
@���@��^@��@�=q@�{@��@��7@�p�@�G�@�V@�7L@�7L@��@��/@��@��@�`B@���@�o@�l�@��;@�b@�ƨ@���@���@��@�C�@�o@��T@�&�@�Z@�1@�ƨ@�t�@�\)@�+@���@��@��@���@��\@�n�@�@��@��^@��h@��@���@��9@���@�r�@�I�@��
@��@��P@��;@���@��w@�ƨ@��@�t�@�K�@��F@��w@��@��P@�|�@�dZ@�C�@�
=@���@�~�@�V@�{@�@�X@��@���@���@���@�33@�
=@��!@�v�@�=q@�E�@�-@��@���@�p�@�O�@�/@�7L@��@���@��/@���@��j@�j@� �@��@�b@���@��H@���@�$�@��@��@��^@�x�@�O�@�?}@�V@��/@���@��@�Q�@��;@��@�S�@�
=@��@��@��y@���@���@�v�@�@���@��@��@���@�%@��`@��j@��j@��j@��j@���@�Z@�1'@��@��;@�|�@�
=@�~�@��@�@���@�/@���@���@��@���@��D@�j@�1'@�1@���@��F@�t�@�dZ@�dZ@�dZ@�K�@��@�o@��@��H@���@�ff@�@�p�@�/@��@�V@���@�Z@�1@��m@��@�\)@�
=@��y@��@���@�v�@�E�@�{@��7@�X@��@���@��@���@���@��D@�(�@�1@��@��w@���@���@�S�@�o@��H@���@��\@�~�@�n�@�5?@�$�@��@��@�@���@��7@�`B@��@�r�@�9X@� �@��@~�y@~5?@}/@|��@|��@|Z@|(�@{��@{�F@{dZ@{@z~�@z-@z�@y�^@yx�@xĜ@xr�@x�@x�@xr�@xA�@w�;@w\)@w�@v��@v$�@v$�@v@u�T@u�T@u@u�@u�@t�@t�/@t�j@t(�@s�m@st�@r��@r~�@r-@q��@q�^@pA�@o�@o|�@n�R@n��@nv�@nV@nE�@n$�@n{@n@m�h@mO�@mV@l�@l�@lj@k��@kS�@j�H@j�@i��@i��@i�7@iG�@hA�@g�P@g+@f�+@f$�@e�-@ep�@e?}@d��@dj@d(�@c��@cC�@c@b-@`��@`r�@_��@_l�@^ȴ@^5?@]�@]@]O�@\�@\1@[�F@[��@[�@[t�@[S�@[33@Z��@Zn�@Y��@Y�^@Y��@Y�7@Y�7@Yx�@Y�@XA�@W�@W\)@V�R@V5?@UV@T�j@TI�@S��@S�F@R�H@R-@Q��@Qhs@P��@P�u@P �@O�@O�;@O��@O��@O��@Ol�@O;d@N�R@NE�@N@M�-@M��@M�h@L�@L(�@K��@K"�@J��@J�!@J��@J��@Jn�@I��@H�`@G�@G�@G��@GK�@G+@F�y@F�R@F�+@Fv�@FE�@E�@E��@E�-@E�h@D��@Co@B�!@BM�@B-@BJ@A��@A��@A�@A��@A�@@�9@@�u@@�@@A�@@b@?��@?K�@?
=@>��@>@=@=/@<�@<�D@<I�@<(�@;��@:�@:M�@:�@9��@9�^@9�7@9x�@9x�@97L@8b@7�;@7��@7|�@7;d@7
=@6��@6�R@6ff@5@5/@4�@4��@4�j@4��@4(�@3��@3�
@3�F@3��@3dZ@3C�@3C�@3o@2�!@2~�@2M�@2J@1��@1G�@17L@0��@0�u@0b@/�@.�R@.5?@.{@-�@-�T@-@-�@-�@-p�@-p�@-p�@-O�@,Z@,1@+��@+�m@+�
@+ƨ@+ƨ@+��@*�H@*M�@)�#@)��@)G�@(��@(�u@(bN@(A�@(b@'�;@'l�@&ȴ@&E�@&5?@&{@&@&@%�T@%��@%@%@%�-@%��@%��@%p�@%`B@%O�@$��@$�j@$�D@$I�@$1@#ƨ@#��@#�@#S�@"�!@"=q@!�@!x�@!G�@!&�@!�@!%@ ��@ �@ Q�@ 1'@ b@�;@��@�@;d@�R@E�@$�@{@�@�-@�@`B@/@�@�@��@�/@��@��@��@�j@z�@j@9X@�
@��@C�@o@�!@n�@-@J@�^@�^@��@x�@X@��@��@bN@b@��@�@l�@
=@�@ȴ@�R@��@v�@5?@5?@@��@�-@?}@�@z�@9X@9X@�m@�F@��@�@t�@dZ@C�@"�@@@�H@�!@�\@M�@-@�@J@�#@��@x�@X@7L@�@�9@1'@  @��@�w@l�@;d@�@ȴ@v�@$�@@@��@�h@p�@O�@/@V@�@�j@��@j@Z@I�@�@��@�@C�@33@o@@o@o@@
�H@
�!@
^5@
-@	�@	��@	�^@	�^@	�^@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aם�Aח�Aי�AדuAבhAבhA׏\AׅAׁA�ZA�bA�O�A���A���A���A�ȴAհ!AՓuA�?}A��A�{A�oA�
=A�1A�
=A�
=A�VA�oA�{A�ȴA��mA�z�A�33A���A�7LA��A�jA��A�-AþwA�ffA�  A+A�dZA���A�+A�%A��uA��jA��/A��hA�JA��jA�  A�^5A��A���A��^A�"�A�1'A�+A�p�A��/A�5?A�"�A��A��FA�A���A�\)A��A�%A�v�A���A�"�A�ffA���A�VA��A��A��A��yA�l�A��
A�K�A�ĜA�G�A��9A�ĜA�1'A�^5A�l�A�/A�7LA��mA�E�A��+A��A��A���A��A���A�C�A���A���A��AG�A|��Az-Ax��Ax1'Av �At5?Ap�uAn9XAm7LAkG�Ai�AhAe��AcAcdZAb9XA`�/A_��A]��A\��A[p�AZ�HAZz�AX�jAWx�AVJAT�ASS�AQ��APz�AP$�AOx�ANjAL��AJ�DAH(�AF~�AD-ABbNA@�yA@{A>A�A<jA9�mA9VA7K�A5�7A4 �A3
=A21A0��A/dZA.M�A-dZA+�A)O�A(��A'�FA&��A%33A$�!A#��A"�jA!�A!K�A ĜA��A�A��Ap�A��A�A��At�A��A�DA��A�A�^A�PA\)A�A��AjA�A�Al�A�A|�AO�A/A
��A
JA	\)Az�A�hA
=Av�A�;A��A I�@��7@�j@�ȴ@�E�@��@�t�@�V@��@�M�@�h@��@��m@���@��@�7@��@��@�\@�=q@�z�@ᙚ@�1@�ƨ@߅@�ƨ@��@�hs@�;d@�5?@�@�O�@�1'@�^5@�O�@�&�@��@ӕ�@ҏ\@�n�@�%@�dZ@�v�@���@�x�@�1'@��m@�"�@ʸR@�M�@���@ɡ�@�/@��
@Ɨ�@���@�Z@�S�@§�@�ff@�=q@��#@�&�@��j@��
@�V@�@��-@���@�I�@��m@�+@���@�^5@��T@�`B@�1'@�o@��H@�ȴ@���@�V@���@�hs@�X@��`@� �@�t�@���@���@�l�@���@�5?@�hs@��j@�Z@�1@���@��R@�-@��@���@�p�@�p�@��T@�Ĝ@��m@�C�@�E�@���@���@�G�@�%@���@���@���@��
@��w@��@��
@���@��^@��@�=q@�{@��@��7@�p�@�G�@�V@�7L@�7L@��@��/@��@��@�`B@���@�o@�l�@��;@�b@�ƨ@���@���@��@�C�@�o@��T@�&�@�Z@�1@�ƨ@�t�@�\)@�+@���@��@��@���@��\@�n�@�@��@��^@��h@��@���@��9@���@�r�@�I�@��
@��@��P@��;@���@��w@�ƨ@��@�t�@�K�@��F@��w@��@��P@�|�@�dZ@�C�@�
=@���@�~�@�V@�{@�@�X@��@���@���@���@�33@�
=@��!@�v�@�=q@�E�@�-@��@���@�p�@�O�@�/@�7L@��@���@��/@���@��j@�j@� �@��@�b@���@��H@���@�$�@��@��@��^@�x�@�O�@�?}@�V@��/@���@��@�Q�@��;@��@�S�@�
=@��@��@��y@���@���@�v�@�@���@��@��@���@�%@��`@��j@��j@��j@��j@���@�Z@�1'@��@��;@�|�@�
=@�~�@��@�@���@�/@���@���@��@���@��D@�j@�1'@�1@���@��F@�t�@�dZ@�dZ@�dZ@�K�@��@�o@��@��H@���@�ff@�@�p�@�/@��@�V@���@�Z@�1@��m@��@�\)@�
=@��y@��@���@�v�@�E�@�{@��7@�X@��@���@��@���@���@��D@�(�@�1@��@��w@���@���@�S�@�o@��H@���@��\@�~�@�n�@�5?@�$�@��@��@�@���@��7@�`B@��@�r�@�9X@� �@��@~�y@~5?@}/@|��@|��@|Z@|(�@{��@{�F@{dZ@{@z~�@z-@z�@y�^@yx�@xĜ@xr�@x�@x�@xr�@xA�@w�;@w\)@w�@v��@v$�@v$�@v@u�T@u�T@u@u�@u�@t�@t�/@t�j@t(�@s�m@st�@r��@r~�@r-@q��@q�^@pA�@o�@o|�@n�R@n��@nv�@nV@nE�@n$�@n{@n@m�h@mO�@mV@l�@l�@lj@k��@kS�@j�H@j�@i��@i��@i�7@iG�@hA�@g�P@g+@f�+@f$�@e�-@ep�@e?}@d��@dj@d(�@c��@cC�@c@b-@`��@`r�@_��@_l�@^ȴ@^5?@]�@]@]O�@\�@\1@[�F@[��@[�@[t�@[S�@[33@Z��@Zn�@Y��@Y�^@Y��@Y�7@Y�7@Yx�@Y�@XA�@W�@W\)@V�R@V5?@UV@T�j@TI�@S��@S�F@R�H@R-@Q��@Qhs@P��@P�u@P �@O�@O�;@O��@O��@O��@Ol�@O;d@N�R@NE�@N@M�-@M��@M�h@L�@L(�@K��@K"�@J��@J�!@J��@J��@Jn�@I��@H�`@G�@G�@G��@GK�@G+@F�y@F�R@F�+@Fv�@FE�@E�@E��@E�-@E�h@D��@Co@B�!@BM�@B-@BJ@A��@A��@A�@A��@A�@@�9@@�u@@�@@A�@@b@?��@?K�@?
=@>��@>@=@=/@<�@<�D@<I�@<(�@;��@:�@:M�@:�@9��@9�^@9�7@9x�@9x�@97L@8b@7�;@7��@7|�@7;d@7
=@6��@6�R@6ff@5@5/@4�@4��@4�j@4��@4(�@3��@3�
@3�F@3��@3dZ@3C�@3C�@3o@2�!@2~�@2M�@2J@1��@1G�@17L@0��@0�u@0b@/�@.�R@.5?@.{@-�@-�T@-@-�@-�@-p�@-p�@-p�@-O�@,Z@,1@+��@+�m@+�
@+ƨ@+ƨ@+��@*�H@*M�@)�#@)��@)G�@(��@(�u@(bN@(A�@(b@'�;@'l�@&ȴ@&E�@&5?@&{@&@&@%�T@%��@%@%@%�-@%��@%��@%p�@%`B@%O�@$��@$�j@$�D@$I�@$1@#ƨ@#��@#�@#S�@"�!@"=q@!�@!x�@!G�@!&�@!�@!%@ ��@ �@ Q�@ 1'@ b@�;@��@�@;d@�R@E�@$�@{@�@�-@�@`B@/@�@�@��@�/@��@��@��@�j@z�@j@9X@�
@��@C�@o@�!@n�@-@J@�^@�^@��@x�@X@��@��@bN@b@��@�@l�@
=@�@ȴ@�R@��@v�@5?@5?@@��@�-@?}@�@z�@9X@9X@�m@�F@��@�@t�@dZ@C�@"�@@@�H@�!@�\@M�@-@�@J@�#@��@x�@X@7L@�@�9@1'@  @��@�w@l�@;d@�@ȴ@v�@$�@@@��@�h@p�@O�@/@V@�@�j@��@j@Z@I�@�@��@�@C�@33@o@@o@o@@
�H@
�!@
^5@
-@	�@	��@	�^@	�^@	�^@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B�dB�)BVB�B,B-B0!B8RB>wBD�BD�BE�BF�BG�BH�BH�BH�BI�BI�BJ�BjBn�BcTBYBM�B@�B?}B8RB7LB49B6FB:^BC�BI�BO�B_;Bo�B��B��B�B�9B�dB�wBÖBƨBB�^B�^B�FB�!B��B��B��B�uB�JB�DB�=B� Bt�Bt�Bs�Bq�Bl�BXBK�BD�B=qB49B �BbB��B�B�B�yB�B�mB�B��BĜB�RB��B��By�BQ�B@�B;dB/B�BJB+B%B
�B
�sB
�BB
��B
ǮB
�9B
�hB
}�B
r�B
e`B
dZB
P�B
@�B
%�B
uB

=B	��B	�B	�`B	�B	��B	��B	��B	ĜB	�RB	�B	��B	��B	��B	�{B	�bB	�7B	|�B	r�B	jB	dZB	ZB	W
B	T�B	N�B	G�B	A�B	49B	#�B	�B	JB	B��B�B�`B�B�
B��BBŢB�wB�^B�9B�-B�B��B��B��B��B��B��B�uB�hB�bB�PB�=B�1B�%B�B�B}�B|�B{�By�Bw�Bv�Bt�Br�Bo�Bn�Bm�Bm�Bm�Bl�Bl�Bl�Bk�BjBhsBl�BffBe`Be`BffBhsBhsBhsBgmBhsBgmBdZBbNBR�BL�BR�BP�BO�BM�BK�BK�BL�BM�BL�BL�BM�BW
B[#B_;BdZBhsBiyBiyBk�Be`BbNBgmBjBq�Bu�Br�Bo�Bo�Bq�Br�Bw�B}�B�B�1B�DB�PB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�9B�LB�RB�XB�XB�^B�dB�qB��BŢBƨBǮB��B��B��B��B��B�
B�B�#B�HB�`B�fB�mB�mB�sB�B�B�B�B�B�B�B��B��B	B	B		7B	VB	\B	\B	bB	bB	bB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	%�B	'�B	)�B	,B	.B	1'B	2-B	8RB	E�B	@�B	@�B	B�B	G�B	N�B	N�B	N�B	P�B	\)B	_;B	`BB	aHB	e`B	gmB	iyB	n�B	y�B	�B	�B	�+B	�JB	�PB	�PB	�PB	�PB	�PB	�PB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�-B	�3B	�3B	�?B	�XB	�qB	�}B	��B	��B	B	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�
B	�
B	�B	�B	�#B	�)B	�)B	�;B	�HB	�NB	�NB	�HB	�HB	�HB	�NB	�TB	�ZB	�TB	�TB	�ZB	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
+B
1B
	7B

=B

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
JB
PB
PB
PB
VB
VB
VB
VB
VB
VB
\B
\B
\B
\B
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
uB
uB
uB
uB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
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
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
)�B
,B
,B
+B
+B
-B
-B
-B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
33B
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
6FB
6FB
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
7LB
7LB
7LB
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
<jB
<jB
<jB
=qB
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
>wB
?}B
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
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
D�B
E�B
G�B
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
I�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
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
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
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
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
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
_;B
`BB
aHB
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
ffB
ffB
gmB
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
jB
jB
jB
jB
jB
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
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
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
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B�PB�~B�B�B,B-)B0oB8�B>�BD�BD�BE�BF�BG�BH�BH�BH�BI�BJ	BLBl�Bq�Bf�B^jBS[BE�BD�B<PB9	B5?B7B;JBD�BJXBQhBa�Bs�B�QB��B��B�LB��B��B�B�~B�KB�B�PB��B�aB�DB��B��B��B��B��B��B�{BwBu�BuBtTBo�BY�BMPBF�B?�B7LB$@B�B�fB�B�B��B��B�BڠBϑB�EB��B�DB��B~BBTFBA�B=B1AB"hBPB�B	7B
��B
�B
��B
�9B
�^B
��B
��B
��B
tTB
f�B
f�B
S�B
DgB
(sB
2B
�B
;B	��B	�>B	�#B	��B	�gB	ϫB	�YB	�^B	��B	�@B	�xB	��B	��B	� B	�B	~�B	tTB	l=B	e�B	Z�B	X+B	V�B	QNB	JrB	DgB	6�B	&�B	�B	"B	�B�]B�3B�$BںB�KB�B�gB�B� B�6B��B��B��B��B�B��B�B�#B�KB�{B��B�B�pB�^B��B��B��B�'B~�B~B}�Bz�By�Bx�Bv�Bt�Bp�Bo Bm�BnBnBm)Bm)BmCBl=BkQBj�BoBf�Be�BfBg�Bi�Bi�Bi�BhsBi�BiBg�Bf2BT�BM�BTBRBRoBO�BMPBL�BM�BNpBM�BM�BN�BW�B[�B_�Be`Bi*BjBj�BmCBfLBb�Bg�Bj�Br|Bv�BtBpUBp;BraBs�By	B~�B�mB��B��B�B��B��B��B�OB�4B�hB��B�8B�mB�DB�XB�eB�KB��B�B��B��B�AB�B��B��B��B��B��B�B�BB��B��B�B�KB�DB�VB�pB�TB�FB׍BٚB�B��B�B�B�B��B��B�B��B��B�!B��B�B��B�^B�}B	{B	�B		�B	�B	�B	�B	B	�B	�B	�B	�B	�B	�B	qB	WB	7B	7B	�B	�B	B	!�B	#�B	&B	(XB	*eB	,"B	.B	1B	1�B	8lB	F�B	AB	@�B	B�B	HB	OB	O(B	O(B	Q B	\)B	_�B	`\B	aHB	eFB	gRB	iB	nB	y�B	��B	�B	�_B	�dB	�jB	��B	��B	��B	�"B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�*B	�*B	�KB	�=B	�)B	�IB	�OB	�UB	��B	�GB	�3B	�B	�?B	��B	��B	��B	��B	��B	�[B	ƨB	��B	��B	��B	��B	��B	�.B	�4B	�B	�,B	�,B	�MB	�YB	�SB	�SB	�YB	׍B	�mB	�EB	�WB	�]B	�CB	�VB	�|B	�B	�B	�|B	�bB	�bB	�NB	�nB	�B	�nB	�nB	�tB	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�<B	�BB	�.B	�.B
 B
  B
 4B
 B
 B
 OB
UB
[B
AB
GB
-B
3B
3B
SB
%B
%B
YB
?B
YB
EB
EB
_B
�B
�B
	�B

�B

�B
xB
�B
~B
dB
dB
dB
dB
~B
~B
dB
jB
�B
�B
pB
VB
pB
pB
pB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
!B
�B
B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
"B
!�B
!�B
#B
"�B
"�B
#B
"�B
#B
# B
#�B
$B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&B
&B
%�B
&B
'B
'B
'B
'B
'B
'mB
(>B
($B
(>B
)*B
)B
)B
)B
)*B
)B
)B
)DB
)B
)B
)B
)B
*0B
*KB
*0B
*KB
+QB
*KB
,B
,"B
+6B
+kB
-CB
-]B
-CB
.IB
.cB
/5B
/5B
/5B
/OB
/OB
0UB
0UB
0UB
0�B
1�B
3hB
3hB
3�B
3hB
4�B
4TB
4TB
4nB
4�B
5tB
5tB
6`B
6FB
6`B
6`B
6`B
6zB
6`B
7�B
7fB
7fB
7LB
7fB
7fB
7�B
7�B
8�B
8�B
8�B
9�B
9�B
:xB
:�B
:xB
:�B
:�B
;�B
;�B
;�B
<�B
<�B
<�B
=�B
=�B
=qB
=qB
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
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
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
EB
F?B
G�B
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
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
MB
L�B
M�B
NB
M�B
N"B
O(B
OB
O�B
O�B
PB
O�B
O�B
O�B
P.B
P.B
Q B
QB
Q B
RB
RB
RB
R B
R B
R B
S&B
TB
TB
TB
T,B
T,B
UB
UB
UB
UB
U2B
VB
VB
VB
VB
V9B
V9B
W$B
W$B
W$B
W?B
W$B
X_B
XEB
X_B
YB
ZkB
Z7B
ZQB
Z7B
Z7B
ZQB
[#B
[#B
[#B
[=B
[=B
[qB
\CB
\)B
\)B
\)B
\)B
\CB
\CB
\xB
]dB
]dB
^OB
^OB
^OB
^OB
_VB
_VB
_VB
_VB
_pB
_pB
`vB
abB
`vB
abB
aHB
abB
aHB
abB
aHB
aHB
abB
aHB
abB
aHB
abB
abB
bhB
bhB
bhB
b�B
cnB
cnB
cnB
c�B
c�B
d�B
d�B
d�B
ezB
ezB
e`B
e�B
ezB
ezB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
iyB
iyB
i�B
i�B
iyB
iyB
iyB
i�B
i�B
j�B
j�B
j�B
j�B
j�B
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
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
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
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
xB
x�B
x�B
y	B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
zB
y�B
zB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
z�B
|B
|B
{�B
{�B
{�B
{�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201611180034492016111800344920161118003449201806221304582018062213045820180622130458201804050704572018040507045720180405070457  JA  ARFMdecpA19c                                                                20161114093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161114003531  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161114003531  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161114003532  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161114003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161114003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161114003532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161114003532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161114003533  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161114003533                      G�O�G�O�G�O�                JA  ARUP                                                                        20161114012916                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161114153529  CV  JULD            G�O�G�O�F�ϱ                JM  ARCAJMQC2.0                                                                 20161117153449  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161117153449  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220457  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040458  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                