CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-10T00:35:31Z creation;2016-08-10T00:35:33Z conversion to V3.1;2019-12-19T08:33:27Z update;     
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
resolution        =���   axis      Z        l  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  st   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  �L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ې   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20160810003531  20200115101518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_025                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @����9 1   @����β @;�O�M�di���'R1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D�|�D�� D���D�<�D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @<��@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AB 33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�fC���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�;3D�{3D׾fD��3D�;3D�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD���D��D�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�K�A�K�A�I�A�I�A�K�A�I�A�I�A�I�A�K�A�K�A�M�A�M�A�M�A�M�A��Aʩ�A��A�p�A¡�A�&�A�oA���A��A���A�ƨA�/A�oA�O�A�n�A���A���A��^A���A���A�ZA�$�A���A��;A�{A��^A�M�A�ZA��A�ƨA��!A�v�A�I�A�/A�A���A�oA�M�A��-A��#A�?}A�bNA��7A���A�x�A��A��A�JA��A�ȴA�&�A��RA�bA��#A���A�~�A�ȴA�XA�-A��9A�l�A�&�A���A���A�M�A���A�/A�
=A�&�A�hA~9XA|��A|5?Az��Ay�-Ax�9Av�jAvE�At�HAs&�Ar�Aq�-Aq�Aqt�Ap��AoO�Am�hAk�#Aj��Ajn�AjAi`BAh��Af�`Ad�uAcC�Aa+A_%A^r�A^9XA]�A]��A]XA]"�A\��A[�AY��AX��AWdZAU�mAUO�AT�/AT��AT(�ASt�AS
=AR�+AQ7LANJAMK�AL�yAL^5AL  AK��AKC�AJ��AJ  AI/AH�AHffAGXAF=qAE+AC��AC;dAB��ABbAAx�A@�HA?l�A>ĜA>n�A<ĜA:��A:z�A:5?A8z�A7�-A7x�A7l�A7dZA7XA7G�A7;dA7"�A6�!A6ZA5��A4M�A3�A2��A1�;A1|�A1�A0��A/��A-��A,��A,JA+�A+�A+%A)�A(��A'��A&�\A%+A$^5A$9XA$ �A$A#�
A#��A#�7A"�A"(�A"A!��A!AA�A?}A��A�TA�A�hAĜA-A�RAv�AI�A{A�-A��A^5A�^A�A��A�yA�jAI�A�
A�uA��A�
A
�A
E�A
A	�mA	��A��A`BAoA��AbNA�^A
=A�uA��AO�A/A��A ��@�|�@�b@���@���@�~�@�`B@�bN@�ƨ@�~�@�@�bN@@�r�@�P@���@��#@�S�@�O�@�J@އ+@�/@�9X@�dZ@���@�E�@�V@�1@�@�{@�p�@�I�@�+@�O�@�A�@Ͼw@υ@�~�@��@̴9@�(�@ʏ\@��@�hs@�1'@�+@�v�@���@őh@�x�@��/@å�@�"�@+@�X@��w@�V@��@��@�E�@���@�bN@�  @�ƨ@�l�@��@�@��7@��@�o@��@���@���@�`B@��@��/@�j@�1@���@��@���@���@�n�@�^5@��^@���@��@�1'@��w@��@��@��F@�@��+@��@��#@���@�z�@�Q�@�(�@���@�o@�ff@��7@���@�z�@�  @��F@�\)@��@���@�^5@�-@�hs@��@��j@�  @�l�@��H@��T@��7@�Ĝ@�bN@��m@��F@�K�@���@�$�@�@���@�x�@�G�@�%@�z�@�bN@�bN@���@��R@��#@�/@��j@�I�@� �@��
@�33@���@�^5@��@��^@�hs@��@���@�r�@�1'@���@�S�@�33@��@��@���@���@�E�@���@���@��7@��@�p�@�hs@�O�@��`@�z�@�A�@�b@�ƨ@�\)@�+@�o@�"�@�;d@�"�@�\)@�33@��H@�~�@�n�@�n�@�-@��T@��^@��7@�?}@�hs@�p�@���@��@�hs@�?}@�/@�&�@�&�@�/@�V@��@���@� �@��@��@��!@�%@�9X@��@�@~�y@~�@~ȴ@~��@~��@~�R@~ff@}�h@|�@|1@|j@yx�@yx�@x�9@x�@x �@x1'@xbN@x�`@y�@y7L@y��@y��@x��@w\)@vv�@v��@vE�@v$�@v$�@v{@vV@vv�@vv�@u@s��@so@s�
@s��@tI�@t�/@t9X@s�F@s33@r��@q�@qX@q%@p��@p�9@p�9@p��@p��@p�`@p�`@p�`@p��@p�9@p�u@p�@pQ�@pA�@p  @o��@o��@n5?@n@m�@m@mp�@m`B@m`B@m/@l��@lz�@lI�@l9X@k�m@k�
@k��@ko@j�!@j=q@i�@ix�@iX@hĜ@h�@hA�@g�@g�w@gl�@g�@f�+@fff@f$�@eO�@d��@d�@d�@d��@dI�@d1@cƨ@c�@c33@c33@co@b��@b�!@bn�@a��@a&�@`�`@`bN@` �@_�w@_|�@_\)@_K�@_;d@_+@_+@^�y@^�+@^{@]�@\�@\j@[��@[��@[�@[dZ@[@Z=q@Y��@Y�@Y�@Y�#@Y�7@YX@X�`@XbN@XA�@W��@W\)@V�@V��@V�+@VV@V$�@V@U@U�h@UO�@T�@T9X@S��@St�@S"�@R��@R=q@Q��@Q�#@Q�^@Q��@Q��@Q�7@Q�7@Qx�@Qhs@QG�@P�`@P�9@P�u@P�`@P��@PQ�@O�P@N��@N�+@M�T@M/@M�@M�@L�@LZ@K�F@K�@KdZ@KdZ@KS�@K33@Ko@J��@J~�@I��@Ix�@HĜ@G�@G�@F�R@F��@F�+@F�+@Fv�@FV@E�@E��@EO�@E�@D�/@D�j@D�D@DZ@D�@D1@C�m@C��@CS�@C"�@B�\@B-@A��@A��@Ahs@A7L@A%@@��@@A�@?�;@?�@?l�@?;d@>�R@>$�@=@=`B@<�@<j@;ƨ@;dZ@;o@:�@:�\@:^5@:�@9��@97L@9%@8��@8�u@8bN@8bN@8bN@8Q�@8Q�@81'@7�@7�@7|�@7l�@7+@6ȴ@6��@6V@65?@6{@5�@5@5�@5`B@5?}@5/@4�@4�j@4�D@49X@3��@3dZ@3C�@3"�@2��@1�#@1��@1hs@1&�@1�@0�`@0Ĝ@0��@0Q�@0b@/�;@/�w@/�@/|�@.�y@.��@.v�@.@-�T@-`B@-/@,�/@,�@,z�@,Z@,�@,�@+��@+�m@+ƨ@+�F@+��@+dZ@+dZ@+dZ@+C�@*�H@*�!@*��@*��@*�\@*~�@*=q@*J@)�@)��@)hs@)&�@(��@(Ĝ@(�9@(�u@(r�@(bN@(bN@(Q�@(A�@(b@'�@'�P@'K�@&��@&ȴ@&ff@&$�@&{@&@&@%�@%�-@%�@%V@$�D@$Z@$9X@$�@#��@#��@#��@#�m@#�m@#�
@#t�@"�@"n�@!��@!x�@!�@ ��@ ��@ Ĝ@ �u@ b@�;@;d@�R@ff@V@E�@{@�@��@�-@��@�h@�@�@p�@p�@`B@`B@`B@/@�@��@�/@�@�D@z�@j@Z@�@��@��@��@�m@�
@ƨ@��@��@dZ@33@o@o@�@~�@hs@%@��@Ĝ@r�@\)@+@
=@��@��@
=@��@��@��@�y@ȴ@�+@ff@E�@{@@�@�T@��@�@?}@V@�@�/@��@�@�@��@C�@S�@S�@"�@33@�@��@�\@~�@~�@n�@n�@n�@n�@M�@=q@-@��@��@�7@x�@G�@7L@7L@&�@��@�u@r�@Q�@b@�w@l�@K�@K�@;d@
=@ȴ@�+@5?@�@��@@��@`B@/@V@��@�/@�/@��@�j@�j@�j@�D@9X@(�@�@ƨ@S�@33@@
��@
�!@
��@
n�@
�@	x�@	&�@	�@�`@Ĝ@�@b@  @��@�@�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�K�A�K�A�I�A�I�A�K�A�I�A�I�A�I�A�K�A�K�A�M�A�M�A�M�A�M�A��Aʩ�A��A�p�A¡�A�&�A�oA���A��A���A�ƨA�/A�oA�O�A�n�A���A���A��^A���A���A�ZA�$�A���A��;A�{A��^A�M�A�ZA��A�ƨA��!A�v�A�I�A�/A�A���A�oA�M�A��-A��#A�?}A�bNA��7A���A�x�A��A��A�JA��A�ȴA�&�A��RA�bA��#A���A�~�A�ȴA�XA�-A��9A�l�A�&�A���A���A�M�A���A�/A�
=A�&�A�hA~9XA|��A|5?Az��Ay�-Ax�9Av�jAvE�At�HAs&�Ar�Aq�-Aq�Aqt�Ap��AoO�Am�hAk�#Aj��Ajn�AjAi`BAh��Af�`Ad�uAcC�Aa+A_%A^r�A^9XA]�A]��A]XA]"�A\��A[�AY��AX��AWdZAU�mAUO�AT�/AT��AT(�ASt�AS
=AR�+AQ7LANJAMK�AL�yAL^5AL  AK��AKC�AJ��AJ  AI/AH�AHffAGXAF=qAE+AC��AC;dAB��ABbAAx�A@�HA?l�A>ĜA>n�A<ĜA:��A:z�A:5?A8z�A7�-A7x�A7l�A7dZA7XA7G�A7;dA7"�A6�!A6ZA5��A4M�A3�A2��A1�;A1|�A1�A0��A/��A-��A,��A,JA+�A+�A+%A)�A(��A'��A&�\A%+A$^5A$9XA$ �A$A#�
A#��A#�7A"�A"(�A"A!��A!AA�A?}A��A�TA�A�hAĜA-A�RAv�AI�A{A�-A��A^5A�^A�A��A�yA�jAI�A�
A�uA��A�
A
�A
E�A
A	�mA	��A��A`BAoA��AbNA�^A
=A�uA��AO�A/A��A ��@�|�@�b@���@���@�~�@�`B@�bN@�ƨ@�~�@�@�bN@@�r�@�P@���@��#@�S�@�O�@�J@އ+@�/@�9X@�dZ@���@�E�@�V@�1@�@�{@�p�@�I�@�+@�O�@�A�@Ͼw@υ@�~�@��@̴9@�(�@ʏ\@��@�hs@�1'@�+@�v�@���@őh@�x�@��/@å�@�"�@+@�X@��w@�V@��@��@�E�@���@�bN@�  @�ƨ@�l�@��@�@��7@��@�o@��@���@���@�`B@��@��/@�j@�1@���@��@���@���@�n�@�^5@��^@���@��@�1'@��w@��@��@��F@�@��+@��@��#@���@�z�@�Q�@�(�@���@�o@�ff@��7@���@�z�@�  @��F@�\)@��@���@�^5@�-@�hs@��@��j@�  @�l�@��H@��T@��7@�Ĝ@�bN@��m@��F@�K�@���@�$�@�@���@�x�@�G�@�%@�z�@�bN@�bN@���@��R@��#@�/@��j@�I�@� �@��
@�33@���@�^5@��@��^@�hs@��@���@�r�@�1'@���@�S�@�33@��@��@���@���@�E�@���@���@��7@��@�p�@�hs@�O�@��`@�z�@�A�@�b@�ƨ@�\)@�+@�o@�"�@�;d@�"�@�\)@�33@��H@�~�@�n�@�n�@�-@��T@��^@��7@�?}@�hs@�p�@���@��@�hs@�?}@�/@�&�@�&�@�/@�V@��@���@� �@��@��@��!@�%@�9X@��@�@~�y@~�@~ȴ@~��@~��@~�R@~ff@}�h@|�@|1@|j@yx�@yx�@x�9@x�@x �@x1'@xbN@x�`@y�@y7L@y��@y��@x��@w\)@vv�@v��@vE�@v$�@v$�@v{@vV@vv�@vv�@u@s��@so@s�
@s��@tI�@t�/@t9X@s�F@s33@r��@q�@qX@q%@p��@p�9@p�9@p��@p��@p�`@p�`@p�`@p��@p�9@p�u@p�@pQ�@pA�@p  @o��@o��@n5?@n@m�@m@mp�@m`B@m`B@m/@l��@lz�@lI�@l9X@k�m@k�
@k��@ko@j�!@j=q@i�@ix�@iX@hĜ@h�@hA�@g�@g�w@gl�@g�@f�+@fff@f$�@eO�@d��@d�@d�@d��@dI�@d1@cƨ@c�@c33@c33@co@b��@b�!@bn�@a��@a&�@`�`@`bN@` �@_�w@_|�@_\)@_K�@_;d@_+@_+@^�y@^�+@^{@]�@\�@\j@[��@[��@[�@[dZ@[@Z=q@Y��@Y�@Y�@Y�#@Y�7@YX@X�`@XbN@XA�@W��@W\)@V�@V��@V�+@VV@V$�@V@U@U�h@UO�@T�@T9X@S��@St�@S"�@R��@R=q@Q��@Q�#@Q�^@Q��@Q��@Q�7@Q�7@Qx�@Qhs@QG�@P�`@P�9@P�u@P�`@P��@PQ�@O�P@N��@N�+@M�T@M/@M�@M�@L�@LZ@K�F@K�@KdZ@KdZ@KS�@K33@Ko@J��@J~�@I��@Ix�@HĜ@G�@G�@F�R@F��@F�+@F�+@Fv�@FV@E�@E��@EO�@E�@D�/@D�j@D�D@DZ@D�@D1@C�m@C��@CS�@C"�@B�\@B-@A��@A��@Ahs@A7L@A%@@��@@A�@?�;@?�@?l�@?;d@>�R@>$�@=@=`B@<�@<j@;ƨ@;dZ@;o@:�@:�\@:^5@:�@9��@97L@9%@8��@8�u@8bN@8bN@8bN@8Q�@8Q�@81'@7�@7�@7|�@7l�@7+@6ȴ@6��@6V@65?@6{@5�@5@5�@5`B@5?}@5/@4�@4�j@4�D@49X@3��@3dZ@3C�@3"�@2��@1�#@1��@1hs@1&�@1�@0�`@0Ĝ@0��@0Q�@0b@/�;@/�w@/�@/|�@.�y@.��@.v�@.@-�T@-`B@-/@,�/@,�@,z�@,Z@,�@,�@+��@+�m@+ƨ@+�F@+��@+dZ@+dZ@+dZ@+C�@*�H@*�!@*��@*��@*�\@*~�@*=q@*J@)�@)��@)hs@)&�@(��@(Ĝ@(�9@(�u@(r�@(bN@(bN@(Q�@(A�@(b@'�@'�P@'K�@&��@&ȴ@&ff@&$�@&{@&@&@%�@%�-@%�@%V@$�D@$Z@$9X@$�@#��@#��@#��@#�m@#�m@#�
@#t�@"�@"n�@!��@!x�@!�@ ��@ ��@ Ĝ@ �u@ b@�;@;d@�R@ff@V@E�@{@�@��@�-@��@�h@�@�@p�@p�@`B@`B@`B@/@�@��@�/@�@�D@z�@j@Z@�@��@��@��@�m@�
@ƨ@��@��@dZ@33@o@o@�@~�@hs@%@��@Ĝ@r�@\)@+@
=@��@��@
=@��@��@��@�y@ȴ@�+@ff@E�@{@@�@�T@��@�@?}@V@�@�/@��@�@�@��@C�@S�@S�@"�@33@�@��@�\@~�@~�@n�@n�@n�@n�@M�@=q@-@��@��@�7@x�@G�@7L@7L@&�@��@�u@r�@Q�@b@�w@l�@K�@K�@;d@
=@ȴ@�+@5?@�@��@@��@`B@/@V@��@�/@�/@��@�j@�j@�j@�D@9X@(�@�@ƨ@S�@33@@
��@
�!@
��@
n�@
�@	x�@	&�@	�@�`@Ĝ@�@b@  @��@�@�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BbNBaHBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBbNBaHB_;BQ�BZBffBp�Bm�BhsBcTB\)BVBF�B7LB&�B�B�B��B�B�#BɺB��B��B��B�+Bk�BbNB^5BZBP�BG�B;dB%�B!�B�B�B�BhB1B�BɺB�jB�9B��B�JB�B{�Bx�Bx�Bo�B[#BI�B>wB8RB,B+B+B'�B�BhB  B
��B
��B
�B
�B
�B
�HB
�B
��B
�jB
�B
��B
��B
�\B
�7B
� B
w�B
p�B
cTB
^5B
XB
H�B
D�B
=qB
;dB
:^B
7LB
,B
�B
oB
1B
%B
B	��B	��B	�B	�)B	��B	ŢB	�RB	�9B	�-B	�!B	�B	�B	��B	��B	��B	��B	�VB	�B	y�B	t�B	r�B	p�B	n�B	iyB	ffB	cTB	^5B	I�B	C�B	@�B	=qB	;dB	9XB	7LB	49B	/B	(�B	&�B	/B	)�B	!�B	�B	oB	PB	VB	DB	
=B	+B	B��B��B��B�B�B�B�mB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�TB�HB�HB�;B�#B�
B��B��B��B��B��BƨBB��B��B��B��B�dB�LB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�oB�JB�7B�%B�B� B|�Bw�Bv�Bq�Bo�Bo�Bm�Bl�BiyBe`BbNB`BB_;B_;B_;B]/B[#BYBT�BS�BR�BP�BP�BO�BN�BN�BK�BJ�BJ�BI�BH�BF�BD�BC�BA�B@�B?}B:^B8RB5?B1'B/B-B-B,B,B,B+B+B)�B(�B'�B'�B&�B&�B'�B&�B#�B!�B!�B"�B!�B!�B!�B!�B"�B#�B#�B$�B%�B%�B&�B&�B&�B(�B)�B(�B)�B,B,B-B/B0!B0!B1'B1'B0!B2-B2-B2-B2-B33B5?B5?B7LB7LB;dB<jB=qB>wB>wB?}B?}BB�BA�BD�BG�BF�BH�BJ�BJ�BK�BK�BL�BL�BM�BN�BN�BO�BP�BP�BR�BT�BT�BT�BVB[#B_;BbNBe`BffBhsBhsBk�Bl�Bm�Bm�Bn�Bo�Bq�Bu�Bw�By�B{�B|�B~�B�B�B�1B�VB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�9B�?B�?B�?B�?B�LB�qB�}BBĜBŢBƨBȴBɺB��B��B��B��B�B�B�#B�)B�5B�HB�NB�fB�yB�B�B�B�B�B�B��B��B��B	  B	B	B	B	DB	PB	VB	bB	oB	{B	�B	�B	#�B	'�B	)�B	.B	1'B	2-B	49B	49B	49B	5?B	7LB	8RB	9XB	;dB	>wB	?}B	B�B	D�B	D�B	D�B	D�B	D�B	F�B	I�B	N�B	P�B	R�B	R�B	S�B	S�B	P�B	M�B	L�B	O�B	T�B	VB	W
B	XB	[#B	]/B	^5B	`BB	`BB	`BB	dZB	iyB	iyB	l�B	q�B	t�B	v�B	x�B	y�B	{�B	}�B	�B	�B	�B	�B	�B	�B	�B	�1B	�7B	�PB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�9B	�FB	�LB	�RB	�LB	�RB	�LB	�XB	�^B	�dB	�jB	�jB	�jB	�qB	��B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ĜB	ĜB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�
B	�B	�B	�)B	�)B	�/B	�/B	�/B	�/B	�)B	�/B	�/B	�/B	�5B	�BB	�HB	�NB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
	7B
DB
DB
JB
PB
VB
VB
\B
\B
VB
VB
\B
\B
\B
oB
oB
uB
uB
uB
uB
uB
�B
{B
�B
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
!�B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
&�B
&�B
'�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
-B
-B
-B
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
0!B
1'B
1'B
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
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
9XB
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
<jB
<jB
=qB
=qB
=qB
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
C�B
C�B
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
E�B
E�B
F�B
F�B
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
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
L�B
M�B
L�B
M�B
M�B
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
N�B
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
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
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
XB
YB
YB
YB
YB
YB
ZB
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
[#B
[#B
[#B
\)B
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
cTB
cTB
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
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hs111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BbNBaHBbNBbNBbNBbNBbNBbNBbNBbNBbhBbhBb�Bb�BbhBU�Ba�BmCBs�Bo�Bj0Bf�B_�B[WBM�B;dB)*B �B�B�B��B�nB�B��B�yB�)B�0Bm)BcTB_�B\�BS�BK�B>�B&�B"4B;BdB�B�B�B��B�B�BB��B��B��B�GB|�Bz*Bz�Br-B]�BKDB?�B9�B,�B+kB,B)�B#B�B B
��B
�`B
�-B
�OB
�B
�B
�CB
�NB
�]B
�"B
�tB
�B
��B
��B
��B
yXB
r�B
dZB
_�B
Y�B
I�B
EmB
=�B
;�B
;dB
9XB
.B
�B
�B
	B
�B
B	�cB	�$B	�B	�5B	�oB	��B	�$B	��B	��B	��B	�}B	��B	��B	��B	��B	�
B	��B	��B	z�B	uZB	s3B	qvB	o�B	jKB	g�B	ezB	aHB	J�B	D3B	A;B	=�B	<B	:B	88B	5%B	0;B	)�B	'�B	0oB	+�B	#:B	B	[B	B	vB	0B	^B	�B	�B�B�B��B�CB�kB�WB�XB�B�tB�B�B�B�B�B�B�B�B��B�\B�]B�+BөBѷB��B�pB��BǮB�{B��B�;B��B�[B��B��B��B��B��B�>B�B�2B�2B�`B�ZB��B��B�B��B��B�yB�{B�PB�XB�_B�gB��B~Bx�BxRBr-BpBp;Bn�BoBkBfLBcB`�B_�B_�B`B^5B\�B[#BV�BUMBS�BQNBQNBP�BP.BPbBLJBKDBK�BJ�BI�BGzBE�BD3BBuBB[BAB<B:�B7LB2GB0B-�B-�B,�B,�B,�B,"B,qB+QB)�B(�B(�B(�B(�B*0B(�B$�B"�B"hB#TB"�B"�B"�B"�B#�B$tB$�B%�B'B&�B'mB'RB'�B)�B*�B)�B+B,�B,�B-�B/�B0�B0�B1vB1vB0�B2�B2�B2�B33B4�B6FB6FB8RB8�B<PB<�B=�B>�B>�B?�B@iBCBB�BEmBG�BGBIRBKBKBLBL0BM6BM6BN<BOBO(BPHBQBQhBSuBUgBUgBU�BW?B\B_�Bb�Be�Bf�Bh�Bi*Bk�Bl�Bm�BnBoBpUBraBv+BxRBzDB|6B}<B}B�oB�gB��B��B��B�B�7B�B�B�QB�B�OB� B�fB�>B�eB�kB��B�vB�hB��B�tB��B��B�tB��B�B��B�GB�B�%B�B�B�#B�DB�PB�.B�:B�SB�kB�qBܒBބB�B�B�B�B�B��B��B��B��B�3B�B�B�B	 4B	'B	GB	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	($B	)�B	.IB	1vB	2|B	4TB	4TB	4�B	5tB	7�B	8�B	9�B	;dB	>wB	?}B	B�B	D�B	D�B	D�B	D�B	D�B	F�B	J	B	OB	Q4B	S@B	S@B	T�B	T�B	Q�B	NpB	MB	PB	UB	VB	W$B	XEB	[#B	]IB	^jB	`�B	`�B	`�B	d�B	jKB	i�B	l�B	q�B	t�B	v�B	x�B	y�B	|B	~B	�B	�3B	�mB	��B	�AB	�B	�SB	�KB	�RB	�PB	�bB	�hB	��B	��B	�$B	��B	�yB	��B	��B	��B	��B	� B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�/B	�/B	�5B	�UB	�;B	�TB	�`B	�fB	�lB	��B	��B	�fB	�rB	�xB	�B	��B	��B	��B	��B	��B	ªB	ðB	��B	żB	żB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�.B	�B	�2B	�B	�9B	�YB	�EB	�?B	�EB	�EB	�CB	�]B	�IB	�dB	�IB	�/B	�]B	�IB	�IB	�dB	ޞB	��B	�bB	�B	�B	�zB	�B	�B	�sB	�B	�yB	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	��B	�*B	�B	�B	�"B	�B	�"B	�"B	�B	�B	�.B	�HB	�.B
 4B
 B
UB
AB
GB
3B
3B
3B
3B
B
MB
9B
B
SB
9B
9B
9B
?B
%B
%B
	RB
xB
�B
~B
�B
�B
�B
vB
\B
pB
�B
�B
vB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
�B
�B
 �B
 �B
 �B
!�B
"B
!�B
#B
$B
$B
#�B
$B
%B
&B
'B
'B
($B
)*B
*B
*0B
*B
+B
+B
+B
,WB
-)B
-)B
-)B
-CB
./B
.B
.B
./B
/B
/OB
/OB
/5B
/5B
0;B
0;B
1[B
1AB
1AB
2GB
2GB
2aB
2GB
3hB
4TB
4TB
4TB
4nB
5ZB
5ZB
5tB
6zB
6zB
7fB
7fB
7�B
8�B
9�B
9rB
9rB
:�B
:xB
:xB
:xB
:xB
;�B
;B
;B
;B
<�B
<�B
=�B
=�B
=�B
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
C�B
C�B
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
E�B
E�B
F�B
F�B
G�B
G�B
G�B
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
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
LB
MB
MB
MB
L�B
L�B
L�B
NB
MB
M�B
MB
NB
M�B
MB
M�B
M�B
M�B
NB
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
PB
O�B
O�B
PB
PB
Q B
P�B
P�B
Q B
Q B
R�B
R�B
S�B
TB
S�B
TB
TB
S�B
SB
SB
SB
S&B
TB
T,B
TFB
T,B
SB
SB
T,B
TaB
U2B
U2B
T�B
T�B
T�B
T�B
T�B
T�B
UB
UB
U2B
VB
VB
V9B
VB
VB
W$B
V9B
VB
W$B
W$B
W$B
XB
X+B
X+B
X_B
YKB
YKB
YB
YB
Y1B
ZB
Y1B
ZQB
ZB
Z7B
ZB
ZB
ZB
Z7B
Z7B
ZQB
[=B
[=B
[=B
\]B
\)B
\CB
\CB
]/B
]IB
]dB
]dB
]IB
]IB
]dB
]IB
^jB
^OB
^jB
_VB
_pB
_VB
`\B
`\B
`vB
`\B
abB
abB
a|B
abB
b�B
b�B
bNB
bhB
bNB
bNB
cTB
cTB
cTB
cnB
cnB
cTB
cnB
c�B
dtB
dtB
dtB
dtB
e`B
ezB
ezB
e�B
e�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
hsB
hs111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608140035172016081400351720160814003517201806221212162018062212121620180622121216201804050404442018040504044420180405040444  JA  ARFMdecpA19c                                                                20160810093504  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160810003531  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160810003531  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160810003532  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160810003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160810003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160810003533  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160810003533  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160810003533  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160810003533                      G�O�G�O�G�O�                JA  ARUP                                                                        20160810011739                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160810153535  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20160813153517  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160813153517  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190444  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031216  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101518                      G�O�G�O�G�O�                