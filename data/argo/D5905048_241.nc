CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-17T00:35:35Z creation;2018-05-17T00:35:41Z conversion to V3.1;2019-12-19T07:38:07Z update;     
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
resolution        =���   axis      Z        L  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  r�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180517003535  20200116231516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_241                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�c6�΀1   @�c6�-� @4wKƧ��dL��E�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dփ3D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��fD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@���@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��BwffB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CB�CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�fC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�fC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�fC���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�;3D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fDց�D־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD���D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aʥ�Aʥ�Aʣ�Aʣ�Aʧ�Aʥ�Aʧ�AʬAʧ�Aʧ�Aʩ�Aʩ�Aʩ�Aʩ�AʬAʩ�Aʩ�Aʩ�Aʩ�Aʥ�Aʧ�AʑhAʅA�bNA�1'A��Aɺ^A�;dA�  A���A�x�A���AǙ�A�Aİ!Aę�A�t�A�ƨA�C�A�x�A�O�A��TA��`A��A�-A��A�A���A�I�A�r�A��HA�oA�E�A��A���A�/A���A�/A�A�^5A���A��A���A�n�A��FA��RA�I�A�t�A�bNA��A��A��FA�r�A���A��7A�=qA���A��9A��A��A�A�9XA��A�A�$�A�7LA��DA�E�A�ƨA��A��jA�S�A���A�"�A��A���A�+A��!A��+A���A��A�1'A�M�A�r�A��A���A���A�K�A��A���A�\)A�~�A��^A�
=A�XA�A�A��wA�+A�O�A33A}�A|{Ay��Ax�yAw�mAw�Av=qAt��Ar�HAq�-AqXAp{Ao�Am�PAk�Ai�
Ah��Ah�Ag�Af1Ad(�Ac�hAbjA`��A_�PA]�A\A�AY�7AVz�AU��AS�APr�AN-AL^5AK��AJ�AIoAFjAE�
AEt�AC�hA@=qA?33A=�mA<~�A:�9A8��A8z�A6��A3ƨA2�+A2ffA2 �A0�DA.��A-�wA,(�A*��A'K�A$�A#�A!
=A�;A��AC�AA�A1A��AVAl�A$�AK�A��A�hAĜA5?Ax�A�9A�7A�RA�mAXA
ZA�A�PA�Ar�A��AVAQ�A��A+A jA -A 1@��@�@��-@��w@�33@�=q@��@���@�33@�$�@�Z@�+@�=q@�@�A�@�C�@�O�@�l�@�$�@��`@畁@�M�@��`@�9X@�l�@�ff@�@�p�@���@��@ߝ�@�\)@�
=@��@ް!@�=q@��/@���@ڧ�@���@؋D@�o@�E�@��#@��@�I�@�+@�5?@��T@��@Л�@�bN@� �@ϝ�@�+@�v�@�O�@�Z@��@��@��@��;@�|�@ʏ\@ɉ7@ǶF@ř�@�r�@��@�@�^5@�$�@�{@�{@�{@��@�=q@�&�@�t�@���@��@��D@���@�t�@���@�o@�-@�1'@�|�@�+@��@��H@���@�v�@���@�G�@�j@� �@���@��;@�dZ@�v�@��@���@�O�@���@��u@�bN@�1'@��F@�S�@�@���@��\@�$�@��-@��7@�hs@��@�j@��P@�o@���@�n�@�J@��7@�`B@��/@��@��@�  @��F@���@��P@��H@��+@�E�@��#@��h@�G�@��`@�Q�@��@��;@�ƨ@���@��@�\)@�@��!@�v�@�=q@���@��h@�Ĝ@�1'@���@�K�@��H@���@�v�@�5?@�@�hs@�?}@��@��`@��u@�Z@�A�@��@��@�dZ@���@��y@��y@��y@��H@��!@��@���@��-@��@��@��9@�z�@�Q�@�ƨ@�l�@�+@���@���@��!@�ff@�J@�@���@�x�@��@�Ĝ@��@��u@�9X@�1'@�(�@�(�@��@�1@���@���@���@�dZ@�C�@��@��@���@��!@��\@�n�@�V@�M�@�5?@�-@�@�X@�&�@�&�@�`B@�X@�7L@��@�/@�?}@��@��`@���@�bN@��@��@� �@�1'@��F@�dZ@�dZ@�dZ@�"�@�
=@��@���@���@�v�@��T@��h@�p�@���@���@��u@�Z@���@���@�S�@�+@�@��@��R@��\@�~�@�ff@�=q@��#@�@��^@���@�X@�?}@��@��`@��9@�bN@�1'@�  @���@�dZ@�33@��@��R@�E�@�-@�{@�{@���@�?}@��@�V@��`@��j@��@�z�@�j@�Z@�9X@�1'@���@���@�S�@��@�ȴ@�=q@��@��h@�&�@���@���@���@�Z@�A�@�1'@�;@;d@~�R@~ff@~{@}p�@}O�@}�@|�@|�j@|�D@|1@{ƨ@{ƨ@{�F@{��@|j@{�F@{33@z�!@z-@y��@yx�@y&�@x��@x�`@xĜ@xQ�@w�w@w\)@v��@vV@u�T@uV@tj@t9X@t�@sƨ@s�F@sdZ@r��@r�\@rn�@rn�@r-@qhs@q%@pQ�@oK�@n�+@m�T@m�h@m/@l�@l�@kC�@j�!@jJ@i��@i��@iG�@h��@h1'@h  @gl�@g
=@f�@f��@f@e��@eO�@d�/@d9X@d(�@d(�@d�@c��@c33@b�H@b��@bJ@a�7@aG�@a&�@`��@`�9@`bN@`b@_|�@_+@_�@^�y@^��@^ff@^$�@]@]?}@\�/@\�j@\�@\�@\��@\�D@\(�@[ƨ@[��@[C�@Z�H@Z�H@Zn�@Z^5@Z=q@Y��@Yhs@X��@X�@X �@Wl�@V�y@V�R@V�+@Vff@V{@U�-@U�@U?}@T�@T�@T(�@S�
@S�@SS�@SC�@S33@R�H@R-@Q��@Q�7@Qhs@Q�@PĜ@PĜ@P��@PbN@P  @O�@N��@N�+@NV@N@M@M��@M�h@M�@M/@L�j@Lj@K��@KdZ@J�H@J�\@J^5@J-@J�@I�#@I�7@I�7@Ihs@I7L@H�`@HĜ@H��@H�9@H�@H1'@G�@G;d@Fȴ@F�+@FV@F5?@E�@E�-@E�-@E��@E�h@Ep�@EO�@E/@E/@E�@D��@D��@Dz�@DZ@D(�@C�
@C�@CS�@B�\@A�#@A��@@��@@r�@@Q�@@A�@?��@?|�@?l�@?;d@>�@=�T@=�T@=�-@=�@=p�@<��@<��@<�j@<Z@;ƨ@;t�@;"�@:��@:^5@:^5@:M�@:-@:�@:J@9�@9��@9��@9X@8��@8�`@8��@8�@8bN@8Q�@8A�@8 �@8  @7�@7�@7K�@7+@6�@6�+@6$�@5�@5@5O�@4��@4��@4j@49X@3�m@3��@3"�@2~�@2�@1�^@1x�@1G�@17L@1�@1%@0�`@0��@0�u@0r�@0bN@01'@/�w@/l�@/K�@/�@.��@.�R@.��@.V@.@-�T@-��@-�h@-�@,��@,z�@,j@,Z@,(�@+�
@+��@+�@+t�@+C�@*��@*�!@*^5@*-@)��@)x�@)X@(Ĝ@(�@( �@'�;@'��@'\)@'
=@&�@&��@&v�@&$�@%��@%�@%p�@%p�@%`B@%O�@%?}@%?}@%O�@%?}@%?}@%?}@%/@%/@%V@$�@$��@$�D@$Z@$9X@#��@#ƨ@#�@#C�@"�@"��@"��@"^5@"M�@"�@!�@!��@!&�@ �`@ �9@ Q�@ 1'@ b@��@�@�@�P@K�@��@��@V@$�@@p�@O�@/@�@��@�j@�D@Z@�@��@dZ@33@o@��@��@��@�\@^5@=q@=q@�@J@�#@x�@7L@��@��@r�@1'@�;@��@l�@l�@\)@�@
=@
=@�y@�R@v�@V@E�@$�@@�T@@@�-@�@?}@V@�@�j@�@j@(�@(�@�m@ƨ@��@��@dZ@C�@"�@�H@��@��@M�@J@�#@��@x�@7L@��@Ĝ@�u@bN@b@��@�@�P@|�@\)@\)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aʥ�Aʥ�Aʣ�Aʣ�Aʧ�Aʥ�Aʧ�AʬAʧ�Aʧ�Aʩ�Aʩ�Aʩ�Aʩ�AʬAʩ�Aʩ�Aʩ�Aʩ�Aʥ�Aʧ�AʑhAʅA�bNA�1'A��Aɺ^A�;dA�  A���A�x�A���AǙ�A�Aİ!Aę�A�t�A�ƨA�C�A�x�A�O�A��TA��`A��A�-A��A�A���A�I�A�r�A��HA�oA�E�A��A���A�/A���A�/A�A�^5A���A��A���A�n�A��FA��RA�I�A�t�A�bNA��A��A��FA�r�A���A��7A�=qA���A��9A��A��A�A�9XA��A�A�$�A�7LA��DA�E�A�ƨA��A��jA�S�A���A�"�A��A���A�+A��!A��+A���A��A�1'A�M�A�r�A��A���A���A�K�A��A���A�\)A�~�A��^A�
=A�XA�A�A��wA�+A�O�A33A}�A|{Ay��Ax�yAw�mAw�Av=qAt��Ar�HAq�-AqXAp{Ao�Am�PAk�Ai�
Ah��Ah�Ag�Af1Ad(�Ac�hAbjA`��A_�PA]�A\A�AY�7AVz�AU��AS�APr�AN-AL^5AK��AJ�AIoAFjAE�
AEt�AC�hA@=qA?33A=�mA<~�A:�9A8��A8z�A6��A3ƨA2�+A2ffA2 �A0�DA.��A-�wA,(�A*��A'K�A$�A#�A!
=A�;A��AC�AA�A1A��AVAl�A$�AK�A��A�hAĜA5?Ax�A�9A�7A�RA�mAXA
ZA�A�PA�Ar�A��AVAQ�A��A+A jA -A 1@��@�@��-@��w@�33@�=q@��@���@�33@�$�@�Z@�+@�=q@�@�A�@�C�@�O�@�l�@�$�@��`@畁@�M�@��`@�9X@�l�@�ff@�@�p�@���@��@ߝ�@�\)@�
=@��@ް!@�=q@��/@���@ڧ�@���@؋D@�o@�E�@��#@��@�I�@�+@�5?@��T@��@Л�@�bN@� �@ϝ�@�+@�v�@�O�@�Z@��@��@��@��;@�|�@ʏ\@ɉ7@ǶF@ř�@�r�@��@�@�^5@�$�@�{@�{@�{@��@�=q@�&�@�t�@���@��@��D@���@�t�@���@�o@�-@�1'@�|�@�+@��@��H@���@�v�@���@�G�@�j@� �@���@��;@�dZ@�v�@��@���@�O�@���@��u@�bN@�1'@��F@�S�@�@���@��\@�$�@��-@��7@�hs@��@�j@��P@�o@���@�n�@�J@��7@�`B@��/@��@��@�  @��F@���@��P@��H@��+@�E�@��#@��h@�G�@��`@�Q�@��@��;@�ƨ@���@��@�\)@�@��!@�v�@�=q@���@��h@�Ĝ@�1'@���@�K�@��H@���@�v�@�5?@�@�hs@�?}@��@��`@��u@�Z@�A�@��@��@�dZ@���@��y@��y@��y@��H@��!@��@���@��-@��@��@��9@�z�@�Q�@�ƨ@�l�@�+@���@���@��!@�ff@�J@�@���@�x�@��@�Ĝ@��@��u@�9X@�1'@�(�@�(�@��@�1@���@���@���@�dZ@�C�@��@��@���@��!@��\@�n�@�V@�M�@�5?@�-@�@�X@�&�@�&�@�`B@�X@�7L@��@�/@�?}@��@��`@���@�bN@��@��@� �@�1'@��F@�dZ@�dZ@�dZ@�"�@�
=@��@���@���@�v�@��T@��h@�p�@���@���@��u@�Z@���@���@�S�@�+@�@��@��R@��\@�~�@�ff@�=q@��#@�@��^@���@�X@�?}@��@��`@��9@�bN@�1'@�  @���@�dZ@�33@��@��R@�E�@�-@�{@�{@���@�?}@��@�V@��`@��j@��@�z�@�j@�Z@�9X@�1'@���@���@�S�@��@�ȴ@�=q@��@��h@�&�@���@���@���@�Z@�A�@�1'@�;@;d@~�R@~ff@~{@}p�@}O�@}�@|�@|�j@|�D@|1@{ƨ@{ƨ@{�F@{��@|j@{�F@{33@z�!@z-@y��@yx�@y&�@x��@x�`@xĜ@xQ�@w�w@w\)@v��@vV@u�T@uV@tj@t9X@t�@sƨ@s�F@sdZ@r��@r�\@rn�@rn�@r-@qhs@q%@pQ�@oK�@n�+@m�T@m�h@m/@l�@l�@kC�@j�!@jJ@i��@i��@iG�@h��@h1'@h  @gl�@g
=@f�@f��@f@e��@eO�@d�/@d9X@d(�@d(�@d�@c��@c33@b�H@b��@bJ@a�7@aG�@a&�@`��@`�9@`bN@`b@_|�@_+@_�@^�y@^��@^ff@^$�@]@]?}@\�/@\�j@\�@\�@\��@\�D@\(�@[ƨ@[��@[C�@Z�H@Z�H@Zn�@Z^5@Z=q@Y��@Yhs@X��@X�@X �@Wl�@V�y@V�R@V�+@Vff@V{@U�-@U�@U?}@T�@T�@T(�@S�
@S�@SS�@SC�@S33@R�H@R-@Q��@Q�7@Qhs@Q�@PĜ@PĜ@P��@PbN@P  @O�@N��@N�+@NV@N@M@M��@M�h@M�@M/@L�j@Lj@K��@KdZ@J�H@J�\@J^5@J-@J�@I�#@I�7@I�7@Ihs@I7L@H�`@HĜ@H��@H�9@H�@H1'@G�@G;d@Fȴ@F�+@FV@F5?@E�@E�-@E�-@E��@E�h@Ep�@EO�@E/@E/@E�@D��@D��@Dz�@DZ@D(�@C�
@C�@CS�@B�\@A�#@A��@@��@@r�@@Q�@@A�@?��@?|�@?l�@?;d@>�@=�T@=�T@=�-@=�@=p�@<��@<��@<�j@<Z@;ƨ@;t�@;"�@:��@:^5@:^5@:M�@:-@:�@:J@9�@9��@9��@9X@8��@8�`@8��@8�@8bN@8Q�@8A�@8 �@8  @7�@7�@7K�@7+@6�@6�+@6$�@5�@5@5O�@4��@4��@4j@49X@3�m@3��@3"�@2~�@2�@1�^@1x�@1G�@17L@1�@1%@0�`@0��@0�u@0r�@0bN@01'@/�w@/l�@/K�@/�@.��@.�R@.��@.V@.@-�T@-��@-�h@-�@,��@,z�@,j@,Z@,(�@+�
@+��@+�@+t�@+C�@*��@*�!@*^5@*-@)��@)x�@)X@(Ĝ@(�@( �@'�;@'��@'\)@'
=@&�@&��@&v�@&$�@%��@%�@%p�@%p�@%`B@%O�@%?}@%?}@%O�@%?}@%?}@%?}@%/@%/@%V@$�@$��@$�D@$Z@$9X@#��@#ƨ@#�@#C�@"�@"��@"��@"^5@"M�@"�@!�@!��@!&�@ �`@ �9@ Q�@ 1'@ b@��@�@�@�P@K�@��@��@V@$�@@p�@O�@/@�@��@�j@�D@Z@�@��@dZ@33@o@��@��@��@�\@^5@=q@=q@�@J@�#@x�@7L@��@��@r�@1'@�;@��@l�@l�@\)@�@
=@
=@�y@�R@v�@V@E�@$�@@�T@@@�-@�@?}@V@�@�j@�@j@(�@(�@�m@ƨ@��@��@dZ@C�@"�@�H@��@��@M�@J@�#@��@x�@7L@��@Ĝ@�u@bN@b@��@�@�P@|�@\)@\)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
VB
(�B
ffB
��B
�B
��B
��B%B<jBM�B\)BgmB��B��B��B�B��B"�B"�B!�B'�B/B7LB8RB9XB:^B0!B0!B0!B;dBD�BiyBp�Br�Bv�By�Bt�Bx�B�7B�VB�DB�7Bv�BffBffB�\B��B��B��B��B�%BdZBR�B7LB#�B�B\BVB��B�BŢB�LB��B��B��B��B��B��B��B��B�%Bp�BiyBW
B>wB.B
��B
�B
�mB
�B
�}B
�'B
�B
�3B
�B
��B
�bB
�B
z�B
q�B
k�B
dZB
cTB
]/B
R�B
H�B
@�B
9XB
,B
�B
�B
hB
DB
B	��B	�B	�mB	�mB	�;B	�B	��B	�wB	�LB	�9B	�B	��B	��B	�oB	�hB	�+B	{�B	t�B	gmB	\)B	N�B	8RB	8RB	+B	�B	oB	JB	JB	%B��B�B�B�B�NB��B�B��BɺBÖB�dB�qB�3B��B��B�B�B��B��B��B�hB�1B{�Bt�Bz�Br�Bv�Br�Bs�Bq�Bx�Bt�Bq�BhsBiyBiyBdZBZBZBcTBbNB_;B]/B\)B]/B]/BYBR�BT�BXB\)BXBXBR�BK�BH�BVB\)B]/B[#BZBW
BS�B\)B[#BZB]/BW
BYBW
BYB\)B[#BXBXBXBYB[#B^5B_;BcTBiyBm�Bk�Bm�Bq�Bt�Bs�Bs�Bv�Bx�Bx�Bx�Bx�Bv�Bs�Bw�Bv�By�By�Bx�B}�B� B}�B�B�B�B�1B�1B�PB�\B�\B�VB�PB�VB�PB�oB��B��B��B��B��B�oB�oB�bB�uB��B��B��B�B�3B�?B�LB�LB�LB�FB�3B�-B�RB�XB��BB��B�B�B�B��B�BB�NB�NB�ZB�TB�NB�TB�B�B�B�B�B�B�B��B��B��B	B	+B	1B	1B	1B	JB	\B	hB	uB	uB	�B	�B	�B	�B	�B	�B	#�B	'�B	+B	-B	/B	49B	7LB	;dB	@�B	B�B	G�B	K�B	J�B	I�B	P�B	S�B	T�B	XB	[#B	\)B	]/B	dZB	hsB	jB	k�B	m�B	m�B	n�B	r�B	t�B	t�B	u�B	y�B	{�B	{�B	~�B	� B	�B	�B	�+B	�1B	�1B	�JB	�bB	�hB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�9B	�FB	�RB	�XB	�XB	�XB	�^B	�jB	�}B	�}B	�wB	ĜB	ŢB	ŢB	ŢB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�
B	�
B	�B	�#B	�5B	�;B	�;B	�NB	�TB	�ZB	�ZB	�ZB	�fB	�`B	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
B
B
B
B
B
B
+B
%B
+B
1B
1B
	7B

=B

=B

=B

=B
	7B
1B
+B
	7B
1B

=B

=B

=B
JB
PB
PB
PB
VB
\B
PB
PB
\B
bB
bB
bB
oB
oB
oB
oB
uB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
!�B
!�B
 �B
"�B
!�B
 �B
"�B
"�B
"�B
!�B
�B
!�B
 �B
 �B
"�B
$�B
&�B
&�B
&�B
%�B
&�B
'�B
(�B
+B
+B
)�B
)�B
+B
,B
+B
,B
-B
,B
+B
,B
,B
,B
,B
/B
.B
.B
.B
,B
-B
.B
-B
.B
/B
0!B
/B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
49B
5?B
5?B
49B
49B
33B
33B
49B
49B
33B
5?B
49B
6FB
5?B
49B
49B
49B
49B
5?B
49B
6FB
7LB
8RB
8RB
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
9XB
9XB
;dB
;dB
:^B
:^B
<jB
;dB
;dB
:^B
:^B
:^B
;dB
<jB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
=qB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
@�B
@�B
A�B
B�B
A�B
A�B
@�B
@�B
A�B
A�B
B�B
B�B
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
D�B
D�B
E�B
E�B
D�B
D�B
D�B
C�B
C�B
D�B
D�B
D�B
F�B
F�B
E�B
F�B
G�B
F�B
F�B
E�B
I�B
H�B
H�B
I�B
H�B
I�B
I�B
H�B
G�B
I�B
I�B
J�B
K�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
O�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
VB
VB
W
B
VB
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
W
B
VB
W
B
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
XB
ZB
ZB
[#B
[#B
[#B
ZB
[#B
\)B
\)B
[#B
ZB
\)B
[#B
\)B
[#B
]/B
]/B
\)B
]/B
]/B
^5B
^5B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
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
`BB
aHB
aHB
aHB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
dZB
dZB
dZB
e`B
ffB
e`B
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
hsB
iyB
iyB
iyB
iyB
jB
jB
iyB
jB
jB
jB
jB
jB
iyB
jB
jB
k�B
jB
k�B
k�B
l�B
l�B
m�B
m�B
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
n�B
o�B
o�B
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
q�B
q�B
q�B
q�B
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
u�B
u�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
B
B
B
B
B
B
B
'B
B
B
B
B
B
B
B
B
B
 B
B
3B
GB
MB
�B
)�B
f�B
�B
� B
�[B
��B�B=�BOvB_;BjB�B� BՁB�B��B#nB$&B$B)�B0�B8lB9�B:DB;dB1�B1�B1�B<�BEmBjeBq�Bs�Bw�Bz�BvFBz�B�#B�(B�JB��Bx�Bg8Bf2B��B�8B�TB��B�
B��Bi_BVB:�B%�B�BoBoB �B��B�B�XB��B�IB�B��B�hB��B�B�_B��Br�BkQBZ�BB�B33B �B
�B
�yB
��B
�{B
��B
�5B
��B
��B
��B
�MB
��B
|�B
s�B
m)B
e�B
c�B
^�B
T�B
J�B
B[B
;B
.cB
 B
�B
�B
dB
gB	��B	��B	��B	�>B	��B	׍B	�B	� B	��B	�ZB	�;B	�0B	��B	��B	�oB	��B	~B	vzB	i�B	^OB	R:B	;�B	9�B	-�B	�B	B	VB	�B	�B�PB�UB�B�B�FB҉B׍BѷB��B��B�qB��B��B�QB�kB�}B�B��B��B�KB��B��B�BxB}"ButBxlBtnButBr�ByXBu�Br�Bj�BkQBkBf�B]/B]/Bd@BcnB`vB^�B]dB^OB^5BZ�BT�BV�BYB\�BYKBYBTaBNBJ�BV�B\xB]�B[�BZ�BX+BUgB\�B[�BZ�B]�BXEBZBXEBZB\�B[�BYBYBYBZkB\)B_;B`BBdZBjBnIBl=BnIBr-BuBt9BtTBwLBy	By$By	By$BwLBt�Bx�Bw�Bz�Bz�By�B~�B�iB~�B��B��B��B��B��B��B��B��B��B��B��B�"B�&B��B��B��B��B�
B�@B�[B��B��B�vB�LB��B�qB�MB�ZB��B�fB�fB�zB�9B�hB�$B�xB�B�GB�"B�+BںB��B�SB�B�B�B�tB�B�B��B�B�B��B��B��B�!B�vB�*B�PB�HB	uB	_B	fB	fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	#B	dB	$&B	(>B	+6B	-wB	/�B	4�B	7�B	;�B	@�B	CB	G�B	K�B	J�B	J=B	QNB	TFB	UgB	XyB	[qB	\xB	]�B	d�B	h�B	j�B	k�B	m�B	m�B	n�B	r�B	t�B	u%B	vFB	z*B	|jB	|PB	HB	��B	�aB	�SB	�_B	�fB	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	��B	��B	��B	��B	�B	�:B	�B	�B	�0B	�eB	�]B	�OB	�UB	��B	�nB	�zB	��B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�?B	�+B	�KB	�YB	�YB	�1B	�#B	�B	�VB	�pB	�hB	�TB	�ZB	�tB	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�	B	��B	�B	��B	�B	�*B	�B	�*B	�B	�B	�6B	�"B	�.B
 4B	�HB
 OB
-B
-B
3B
[B
oB
9B
EB
YB
EB
KB
KB
	lB

XB

XB

XB

�B
	�B
�B
zB
	�B
�B

rB

�B

�B
�B
�B
�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
!B
�B
!�B
!�B
 �B
"�B
!�B
 �B
"�B
"�B
"�B
!�B
 B
"B
!-B
!-B
# B
%B
'B
'8B
'B
&LB
'8B
($B
)DB
+6B
+6B
*0B
*KB
+6B
,"B
+6B
,"B
-)B
,"B
+QB
,=B
,WB
,=B
,WB
/B
.B
.IB
./B
,WB
-)B
./B
-]B
.IB
/OB
0;B
/OB
0;B
0;B
0oB
0UB
1AB
2GB
2GB
2GB
2GB
2GB
2aB
2aB
3MB
4TB
5?B
5?B
4TB
4nB
3hB
3hB
4TB
4TB
3MB
5ZB
4�B
6FB
5ZB
4nB
4nB
4nB
4nB
5tB
4�B
6�B
7fB
8lB
8lB
8lB
7�B
8lB
8lB
8lB
8�B
8�B
8�B
9rB
9rB
:^B
:xB
:�B
9�B
9�B
;dB
;�B
:xB
:xB
<jB
;B
;B
:�B
:�B
:�B
;�B
<�B
;B
<�B
<�B
=qB
=�B
=�B
=�B
>�B
>�B
=�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
@�B
@�B
A�B
B�B
A�B
A�B
@�B
@�B
A�B
A�B
B�B
B�B
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
D�B
D�B
E�B
E�B
D�B
D�B
D�B
C�B
C�B
D�B
D�B
D�B
F�B
F�B
E�B
F�B
G�B
F�B
F�B
FB
I�B
H�B
H�B
I�B
H�B
I�B
I�B
H�B
G�B
I�B
I�B
J�B
K�B
M�B
M�B
M�B
M�B
M�B
M�B
NB
M�B
M�B
L�B
M�B
NB
M�B
N�B
O�B
O�B
O�B
PB
PB
OB
N�B
O�B
PB
O�B
Q B
QB
Q B
PB
Q B
RB
RB
SB
SB
S&B
S&B
S&B
T,B
U2B
V9B
VB
W
B
VB
W
B
W$B
VB
W
B
W$B
W$B
W?B
V9B
W$B
X+B
X+B
X+B
XEB
X+B
XEB
XEB
YKB
Y1B
YKB
X_B
Z7B
ZQB
[#B
[WB
[=B
Z7B
[WB
\CB
\]B
[=B
ZkB
\CB
[=B
\CB
[WB
]IB
]dB
\]B
]dB
]dB
^OB
^OB
]dB
^jB
_VB
_VB
_VB
_VB
_pB
_VB
aHB
abB
abB
aHB
abB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
a|B
abB
`vB
aHB
abB
abB
`\B
a|B
a|B
abB
abB
bhB
b�B
bhB
cTB
bhB
bhB
bhB
b�B
c�B
cnB
cnB
dtB
dtB
dtB
e�B
e`B
ezB
dtB
dtB
d�B
e�B
f�B
e�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
f�B
h�B
i�B
i�B
i�B
i�B
j�B
jB
i�B
j�B
jB
j�B
j�B
j�B
i�B
j�B
j�B
k�B
j�B
k�B
k�B
l�B
l�B
m�B
m�B
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
n�B
o�B
o�B
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
q�B
q�B
q�B
q�B
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
u�B
u�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805210039572018052100395720180521003957201806221330382018062213303820180622133038201806042133052018060421330520180604213305  JA  ARFMdecpA19c                                                                20180517093518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180517003535  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180517003538  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180517003539  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180517003540  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180517003540  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180517003540  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180517003540  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180517003540  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180517003541                      G�O�G�O�G�O�                JA  ARUP                                                                        20180517005721                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180517153555  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180520153957  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180520153957  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604123305  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622043038  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231516                      G�O�G�O�G�O�                