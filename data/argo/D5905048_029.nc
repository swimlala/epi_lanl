CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-19T00:35:29Z creation;2016-08-19T00:35:31Z conversion to V3.1;2019-12-19T08:28:56Z update;     
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
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20160819003529  20200116201515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_029                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��5��Ԁ1   @��6j1N @45��ݗ��d��Q�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ D�|�D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D���D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D���D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Ct�Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D�3D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D�fD|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�{3DԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��3D�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��3D�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�+31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��TA��HA��HA��HA��TA��HA��;A��TA��yA��TA��TA��`A��TA��/A��
A��A��A��
A��
A��
A��
A���A���A�A���A��A�O�Aߩ�A�A�A��yA��A�S�Aܙ�A�  A�33Aם�A�G�Aҏ\A���A���A�E�Aѕ�A�;dA�ƨA�A�A�;dA�r�A̕�A��A��AʸRA�ffAɃA���A�bNAǟ�AƝ�A���AĲ-A�S�AA�ƨA���A��^A�x�A���A��HA�r�A���A�\)A���A�dZA���A�p�A���A��A��A� �A� �A�l�A�ƨA�5?A�E�A�`BA�E�A�JA���A�?}A���A��PA��-A��A�;dA�ZA���A�1'A���A�(�A��A�"�A�C�A�p�A���A� �A���A�bA��DA�G�A�/A���A�&�A��!A�-A���A�p�A�ffA��mA�x�A~��AzM�Au��At�\At=qAs��Ar5?Aq&�Ap��AoS�Ai�-Ae��Ac��Ac33Ab  A`A_%A^1'A\1AZE�AX�`AW�TAV=qAT  AP��AO%AN�\ANM�AM��ALAJ~�AIƨAG?}AE|�AD�AD�jADI�AA&�A?33A=�#A;"�A9�FA8n�A5�A4$�A3+A2M�A0bNA/�A.��A-7LA+�PA*r�A)�;A)\)A(��A(�RA(�DA(jA(I�A't�A%�FA$�RA#�wA"�!A �`AAVAffAS�A9XA&�A�9A��A�RA�mA�AA�7A�\A�#A�`AƨAK�AhsAp�A��A�-AM�A�!A��A�
AG�AVA
1'A	C�A	"�AĜAZA{A��AAbNA�!AȴA�-A+A Z@���@�V@���@�33@��@��u@��@�1'@��@�{@�1'@�K�@�1'@�C�@ꟾ@�h@�x�@�7L@�ƨ@�ff@�h@㕁@�z�@ޗ�@�v�@�E�@��@ݲ-@�
=@���@�M�@�-@��#@�  @�E�@�-@թ�@���@��
@�33@җ�@ҸR@�5?@��#@�p�@��`@�  @�@�5?@���@̬@˥�@�n�@�&�@���@�9X@�A�@�b@ǅ@��H@Ə\@�{@���@��@���@���@öF@Õ�@�dZ@�"�@�"�@��@�V@�/@��`@��@��F@��@���@�Z@��R@��\@��@���@�
=@�~�@���@��j@��u@��j@���@�?}@�@��@�1'@�  @��;@�t�@��@��h@��-@�/@�bN@�l�@�%@�1'@���@��@���@���@���@�/@� �@���@�V@�-@�{@�=q@��!@��H@���@���@���@�@��@��@��+@�^5@���@���@���@�Z@�Q�@�(�@���@�dZ@��P@��@��@�  @�1@�1@���@���@�K�@�+@��H@�E�@�&�@� �@��@��@��j@��/@��j@��u@��@��;@��@� �@�I�@�1'@�C�@�n�@�$�@��-@��`@���@�Ĝ@��D@�A�@�9X@�ƨ@��w@�t�@�K�@�K�@�33@��!@���@���@��!@�5?@���@��@���@��;@�|�@�C�@�;d@�;d@�33@�"�@�@���@�V@��T@���@�@��7@���@���@��D@�Q�@�A�@�(�@�1@���@��;@��w@�l�@�
=@���@���@�V@�J@���@���@��7@�X@�&�@�%@��/@��9@��D@�Q�@�  @��@�S�@�"�@��@��!@�n�@�M�@�$�@���@��@�&�@��@��j@�9X@�t�@�"�@�
=@���@���@���@�~�@�5?@��@�{@��@�@��-@���@�hs@�V@��u@�r�@�(�@�ƨ@�|�@�S�@�K�@�;d@�ȴ@�v�@�ff@�^5@�5?@��T@���@�x�@�O�@��@���@���@��9@�Z@�b@��m@��@�t�@�K�@�33@���@�=q@�V@���@�z�@�j@�j@�I�@�  @��@���@���@�K�@��H@�ȴ@��!@�M�@�E�@�5?@��@���@�@�O�@���@���@�I�@� �@���@�t�@�K�@���@��\@�M�@�=q@�$�@�$�@�@��@�@���@��@�`B@��@��9@�j@�(�@�b@��@\)@~�y@}��@}�@}p�@}/@|��@|9X@{��@{@z~�@z=q@y�@y�^@yX@xĜ@xA�@w|�@w+@v�@vff@u?}@t�D@tz�@tI�@t�@s��@s��@r�@r~�@rJ@q��@qX@q�@p�@o�w@oK�@o
=@n�y@nȴ@nȴ@n��@n��@n{@mO�@lZ@k�
@k��@k�@k@j��@j��@jJ@ix�@i&�@i�@h��@h��@hr�@gK�@f�y@f��@fff@e@e?}@d�/@d��@d9X@c��@co@b�\@b-@a��@a�7@ax�@ax�@ahs@aX@aG�@`��@`A�@`1'@`1'@`b@_��@^��@^�R@^��@^��@^��@^v�@^V@^{@]�T@]�-@]p�@]O�@]V@\�/@\z�@\9X@[�
@[dZ@[o@Z��@ZM�@Y��@Y�@Y��@Y�^@Y�^@Y��@Yx�@Yhs@Y7L@Y�@X�`@X�u@XQ�@W�@W�w@W|�@V�y@V��@V$�@U@U�h@U`B@U�@T�@T�/@T�@Tz�@TZ@T9X@S�
@St�@S33@R�\@Q��@P��@P�@P �@O��@O|�@O\)@O;d@O
=@N�R@N5?@M�h@M`B@Mp�@M?}@L�@L�j@Lz�@Lj@Lj@LI�@Kt�@Ko@J~�@J^5@J=q@I�@I�7@IG�@I&�@H�`@H�`@HĜ@Hr�@H1'@G�@Gl�@G
=@FV@E�-@D��@D9X@C�m@CdZ@B~�@A��@A�^@A�7@AG�@@��@@Q�@?�;@?�P@?K�@?+@>�y@>��@=�T@=?}@=V@<��@<I�@<1@;�F@;t�@:�@:~�@:J@9��@9hs@9�@8Ĝ@8�u@8 �@7�;@7�@7|�@7l�@7K�@7+@6��@5�@5�h@5�@5?}@4�@4�j@4�D@4I�@49X@4(�@3�m@3�@3�@3�@3�@3�@3S�@2��@2n�@2n�@2^5@2�@1�^@1x�@1hs@1X@1X@17L@1&�@1�@0��@0Ĝ@0bN@0 �@0  @/�@/l�@/;d@/�@.�@.v�@.ff@.V@.$�@.@-�@-�T@-��@-?}@,�/@,�@,z�@,�@,1@+�m@+�
@+�
@+ƨ@+��@+t�@+dZ@+dZ@+S�@+33@*�@*�!@*~�@*-@*J@)�@)�#@)�#@)�#@)�#@)�#@)��@)��@)x�@)hs@)X@)%@(�u@(A�@'��@'�P@'
=@&��@&E�@&@%�T@%�h@%`B@%V@$��@$j@$9X@$(�@#�m@#��@#t�@#dZ@#"�@"�@"��@"M�@!�@!�#@!��@!�7@!7L@!&�@ �`@ Ĝ@ �u@ r�@ 1'@ b@�;@��@�@�P@\)@
=@�@��@��@�+@v�@$�@��@p�@`B@O�@V@�@�D@Z@�m@ƨ@�@"�@@n�@�@��@��@��@��@X@�`@Ĝ@�u@bN@bN@1'@�;@��@��@l�@
=@�@ȴ@��@ff@V@E�@5?@$�@@��@�-@��@p�@?}@�@�@�/@��@��@�D@j@1@��@C�@@��@n�@^5@M�@�@��@x�@X@G�@G�@&�@%@��@�9@��@bN@�w@l�@K�@;d@+@�@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��TA��HA��HA��HA��TA��HA��;A��TA��yA��TA��TA��`A��TA��/A��
A��A��A��
A��
A��
A��
A���A���A�A���A��A�O�Aߩ�A�A�A��yA��A�S�Aܙ�A�  A�33Aם�A�G�Aҏ\A���A���A�E�Aѕ�A�;dA�ƨA�A�A�;dA�r�A̕�A��A��AʸRA�ffAɃA���A�bNAǟ�AƝ�A���AĲ-A�S�AA�ƨA���A��^A�x�A���A��HA�r�A���A�\)A���A�dZA���A�p�A���A��A��A� �A� �A�l�A�ƨA�5?A�E�A�`BA�E�A�JA���A�?}A���A��PA��-A��A�;dA�ZA���A�1'A���A�(�A��A�"�A�C�A�p�A���A� �A���A�bA��DA�G�A�/A���A�&�A��!A�-A���A�p�A�ffA��mA�x�A~��AzM�Au��At�\At=qAs��Ar5?Aq&�Ap��AoS�Ai�-Ae��Ac��Ac33Ab  A`A_%A^1'A\1AZE�AX�`AW�TAV=qAT  AP��AO%AN�\ANM�AM��ALAJ~�AIƨAG?}AE|�AD�AD�jADI�AA&�A?33A=�#A;"�A9�FA8n�A5�A4$�A3+A2M�A0bNA/�A.��A-7LA+�PA*r�A)�;A)\)A(��A(�RA(�DA(jA(I�A't�A%�FA$�RA#�wA"�!A �`AAVAffAS�A9XA&�A�9A��A�RA�mA�AA�7A�\A�#A�`AƨAK�AhsAp�A��A�-AM�A�!A��A�
AG�AVA
1'A	C�A	"�AĜAZA{A��AAbNA�!AȴA�-A+A Z@���@�V@���@�33@��@��u@��@�1'@��@�{@�1'@�K�@�1'@�C�@ꟾ@�h@�x�@�7L@�ƨ@�ff@�h@㕁@�z�@ޗ�@�v�@�E�@��@ݲ-@�
=@���@�M�@�-@��#@�  @�E�@�-@թ�@���@��
@�33@җ�@ҸR@�5?@��#@�p�@��`@�  @�@�5?@���@̬@˥�@�n�@�&�@���@�9X@�A�@�b@ǅ@��H@Ə\@�{@���@��@���@���@öF@Õ�@�dZ@�"�@�"�@��@�V@�/@��`@��@��F@��@���@�Z@��R@��\@��@���@�
=@�~�@���@��j@��u@��j@���@�?}@�@��@�1'@�  @��;@�t�@��@��h@��-@�/@�bN@�l�@�%@�1'@���@��@���@���@���@�/@� �@���@�V@�-@�{@�=q@��!@��H@���@���@���@�@��@��@��+@�^5@���@���@���@�Z@�Q�@�(�@���@�dZ@��P@��@��@�  @�1@�1@���@���@�K�@�+@��H@�E�@�&�@� �@��@��@��j@��/@��j@��u@��@��;@��@� �@�I�@�1'@�C�@�n�@�$�@��-@��`@���@�Ĝ@��D@�A�@�9X@�ƨ@��w@�t�@�K�@�K�@�33@��!@���@���@��!@�5?@���@��@���@��;@�|�@�C�@�;d@�;d@�33@�"�@�@���@�V@��T@���@�@��7@���@���@��D@�Q�@�A�@�(�@�1@���@��;@��w@�l�@�
=@���@���@�V@�J@���@���@��7@�X@�&�@�%@��/@��9@��D@�Q�@�  @��@�S�@�"�@��@��!@�n�@�M�@�$�@���@��@�&�@��@��j@�9X@�t�@�"�@�
=@���@���@���@�~�@�5?@��@�{@��@�@��-@���@�hs@�V@��u@�r�@�(�@�ƨ@�|�@�S�@�K�@�;d@�ȴ@�v�@�ff@�^5@�5?@��T@���@�x�@�O�@��@���@���@��9@�Z@�b@��m@��@�t�@�K�@�33@���@�=q@�V@���@�z�@�j@�j@�I�@�  @��@���@���@�K�@��H@�ȴ@��!@�M�@�E�@�5?@��@���@�@�O�@���@���@�I�@� �@���@�t�@�K�@���@��\@�M�@�=q@�$�@�$�@�@��@�@���@��@�`B@��@��9@�j@�(�@�b@��@\)@~�y@}��@}�@}p�@}/@|��@|9X@{��@{@z~�@z=q@y�@y�^@yX@xĜ@xA�@w|�@w+@v�@vff@u?}@t�D@tz�@tI�@t�@s��@s��@r�@r~�@rJ@q��@qX@q�@p�@o�w@oK�@o
=@n�y@nȴ@nȴ@n��@n��@n{@mO�@lZ@k�
@k��@k�@k@j��@j��@jJ@ix�@i&�@i�@h��@h��@hr�@gK�@f�y@f��@fff@e@e?}@d�/@d��@d9X@c��@co@b�\@b-@a��@a�7@ax�@ax�@ahs@aX@aG�@`��@`A�@`1'@`1'@`b@_��@^��@^�R@^��@^��@^��@^v�@^V@^{@]�T@]�-@]p�@]O�@]V@\�/@\z�@\9X@[�
@[dZ@[o@Z��@ZM�@Y��@Y�@Y��@Y�^@Y�^@Y��@Yx�@Yhs@Y7L@Y�@X�`@X�u@XQ�@W�@W�w@W|�@V�y@V��@V$�@U@U�h@U`B@U�@T�@T�/@T�@Tz�@TZ@T9X@S�
@St�@S33@R�\@Q��@P��@P�@P �@O��@O|�@O\)@O;d@O
=@N�R@N5?@M�h@M`B@Mp�@M?}@L�@L�j@Lz�@Lj@Lj@LI�@Kt�@Ko@J~�@J^5@J=q@I�@I�7@IG�@I&�@H�`@H�`@HĜ@Hr�@H1'@G�@Gl�@G
=@FV@E�-@D��@D9X@C�m@CdZ@B~�@A��@A�^@A�7@AG�@@��@@Q�@?�;@?�P@?K�@?+@>�y@>��@=�T@=?}@=V@<��@<I�@<1@;�F@;t�@:�@:~�@:J@9��@9hs@9�@8Ĝ@8�u@8 �@7�;@7�@7|�@7l�@7K�@7+@6��@5�@5�h@5�@5?}@4�@4�j@4�D@4I�@49X@4(�@3�m@3�@3�@3�@3�@3�@3S�@2��@2n�@2n�@2^5@2�@1�^@1x�@1hs@1X@1X@17L@1&�@1�@0��@0Ĝ@0bN@0 �@0  @/�@/l�@/;d@/�@.�@.v�@.ff@.V@.$�@.@-�@-�T@-��@-?}@,�/@,�@,z�@,�@,1@+�m@+�
@+�
@+ƨ@+��@+t�@+dZ@+dZ@+S�@+33@*�@*�!@*~�@*-@*J@)�@)�#@)�#@)�#@)�#@)�#@)��@)��@)x�@)hs@)X@)%@(�u@(A�@'��@'�P@'
=@&��@&E�@&@%�T@%�h@%`B@%V@$��@$j@$9X@$(�@#�m@#��@#t�@#dZ@#"�@"�@"��@"M�@!�@!�#@!��@!�7@!7L@!&�@ �`@ Ĝ@ �u@ r�@ 1'@ b@�;@��@�@�P@\)@
=@�@��@��@�+@v�@$�@��@p�@`B@O�@V@�@�D@Z@�m@ƨ@�@"�@@n�@�@��@��@��@��@X@�`@Ĝ@�u@bN@bN@1'@�;@��@��@l�@
=@�@ȴ@��@ff@V@E�@5?@$�@@��@�-@��@p�@?}@�@�@�/@��@��@�D@j@1@��@C�@@��@n�@^5@M�@�@��@x�@X@G�@G�@&�@%@��@�9@��@bN@�w@l�@K�@;d@+@�@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
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
�NB
�`B
�B
��B
�BB
�ZB
��B
�B
��B
�ZB
��B
��B
�-B
��B
��B
�RB
�
B
�sB
��BPB$�BW
BcTBjB��B��BB�B,B@�BM�BffB� B��B�FB��B�`B�B��B��B�B��Bl�BaHB\)B`BBP�B9XB49B0!B-B2-B?}B<jB;dBM�BE�BB�B<jB:^B49B&�B�B1B  B�TB�9B�uB�1By�Bn�BffBaHB[#BJ�B;dB,B�BuB
��B
�NB
�B
��B
ɺB
ǮB
�}B
�^B
�3B
�B
��B
�+B
u�B
k�B
ffB
W
B
;dB
#�B
{B
hB
PB
B	��B	��B	�B	�B	�jB	�-B	�B	�B	��B	��B	��B	�DB	�B	{�B	u�B	k�B	bNB	R�B	D�B	A�B	?}B	>wB	5?B	0!B	(�B	�B	1B	PB	PB	JB��B�B�B�TB�/B�B��B��B��B��BĜB��B�}B�jB�FB�3B�'B�!B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�FB�dB�}B��B�jB�XB�XB��B��B��B�B��B�JB��B�B�-B�3B�FB�qB�XB�9B�dB�}B�qB�qBBĜB��B�RB�9B�?B�FB�FB�LB�RB�FB�FB�LB�LB�RB�^B�qB��B��B��BÖB��BĜBƨBǮBȴBȴBɺB��B��BȴB��B��B��B��B��B��B��B�/B�NB�NB�HB�B�)B�BB�HB�`B�mB�yB�B�B�B�B�B��B��B��B��B	B	B	B	B	+B	+B		7B	JB	VB	bB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	&�B	&�B	&�B	&�B	-B	0!B	-B	)�B	+B	.B	9XB	?}B	A�B	D�B	C�B	B�B	C�B	I�B	L�B	S�B	dZB	k�B	hsB	k�B	k�B	k�B	m�B	q�B	y�B	|�B	z�B	v�B	o�B	p�B	x�B	�B	�B	�+B	�VB	��B	��B	�hB	�oB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�3B	�3B	�3B	�?B	�?B	�FB	�LB	�LB	�RB	�^B	�jB	�jB	�qB	�qB	�wB	��B	��B	�}B	ÖB	B	��B	�qB	��B	B	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�;B	�ZB	�fB	�fB	�fB	�mB	�yB	�yB	�yB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
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
+B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B
1B
1B
	7B

=B

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
VB
\B
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
bB
bB
VB
JB

=B

=B

=B
DB
JB
JB
JB
JB
JB
JB
VB
VB
\B
bB
bB
hB
oB
oB
oB
uB
uB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
#�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
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
)�B
+B
+B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
.B
.B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
49B
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
6FB
6FB
6FB
6FB
6FB
7LB
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
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
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
?}B
@�B
?}B
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
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
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
O�B
O�B
O�B
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
Q�B
Q�B
R�B
S�B
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
]/B
^5B
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
hsB
hsB
iyB
iyB
iyB
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
l�B
l�B
l�B
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
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
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
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
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
�B
��B
��B
��B
��B
��B
��B
��B
�B
� B
҉B
�gB
�B
�fB
�GB
��B
�HB
�B
�DB
��B
��B
�$B
��B
҉B
��B
��B
�B
�lB
��B
�QBoBVB%�BXyBeFBmCB�OB�PB�B!bB-�BA�BO�Bh�B��B�_B��BյB�B�;B��B��B��B��Bo�Bc�B_�BeBS[B:�B5ZB1�B0�B6+BB�B?�BABQhBH�BD�B>�B=B72B*B�B
�BmB�B��B��B�	B{0BpBg�Bc:B^BN�B=�B./B"NB�B�B
��B
�1B
�vB
�XB
�B
��B
��B
�?B
�iB
�]B
�lB
w2B
mwB
i�B
[�B
?�B
%`B
2B
�B
�B
�B
 iB	��B	��B	�CB	�]B	�hB	��B	�)B	�@B	�)B	�B	�jB	��B	}�B	xB	n�B	e�B	T�B	ESB	BAB	@�B	@�B	72B	1�B	+�B	kB		B	B	�B	�B��B��B��B�`BߊB�]B��B�BB�jB�B�%B�uB��B�]B��B�B��B��B��B�iB�}B��B�wB�B�kB�eB��B��B�'B��B��B�B�B��B��B��B�HB��B�`B��B�iB��B��B�JB�jB��B�,B��B�IB�bB��B��B�WB�B�B��B��B�^B��B�B�B�(B�]BÖB��B��B��B��B�+B�fB�B��B��B��B�LB�lB��B�B�B��B��BªB�GB�MB�'B�9B��B�1BɺBɺBʦB̈́B��B��B��B�B�PB�vB�[B̈́B��B�dB��B�B�4B�kBܬB��B��B��B��B�B��B�B�B�;B��B�tB�`B�>B��B	�B	�B	�B	mB	zB	EB		�B	�B	�B	�B	 B	MB	B	�B	�B	�B	�B	�B	�B	�B	�B	!|B	'�B	'RB	'mB	'�B	-�B	0�B	-�B	+B	+B	./B	9�B	?}B	BB	E9B	D3B	B�B	C�B	I�B	L�B	TB	d�B	lWB	h�B	k�B	lB	lqB	m�B	q�B	zDB	}�B	{�B	xB	p;B	poB	x�B	�;B	�3B	�+B	��B	�EB	�?B	��B	��B	��B	��B	�~B	��B	��B	�B	�B	�"B	�)B	�IB	�}B	�OB	��B	��B	��B	�hB	�ZB	�tB	��B	�fB	�2B	�8B	�DB	�jB	�jB	��B	��B	��B	��B	��B	��B	�3B	�GB	�B	�VB	��B	ªB	ÖB	ĶB	��B	�B	ŢB	ƨB	ǔB	ȴB	�#B	�^B	�PB	�B	�<B	�BB	��B	� B	�,B	�2B	�B	�SB	�B	�QB	�=B	�pB	�tB	�B	�fB	�B	�B	��B	��B	��B	��B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�-B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�	B	�	B	��B	�B	��B	�B	�B	�B	�B	�"B	�(B	�(B	�BB	�HB
 OB
 4B
;B
;B
;B
'B
AB
[B
uB
aB
GB
aB
�B
�B
SB
?B
_B
_B
_B
_B
fB
fB
fB
KB
KB
KB
	lB
	�B
�B
�B
	�B

rB

�B

rB

XB
xB
^B
�B
~B
dB
dB
~B
�B
�B
pB
vB
�B
vB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
(B
�B

XB

rB

XB
xB
~B
�B
~B
dB
�B
�B
pB
pB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
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
B
�B
�B
 �B
 �B
!�B
!�B
!�B
# B
# B
# B
#�B
$B
$&B
#TB
$B
$�B
%B
%B
%B
&B
'8B
'B
($B
($B
(
B
($B
($B
(>B
)*B
)*B
)B
)*B
(�B
)B
)*B
)*B
*KB
+QB
+6B
,=B
,=B
,=B
-CB
-CB
-CB
-CB
./B
./B
.IB
.IB
.IB
.}B
0;B
0UB
0UB
0UB
1vB
1[B
1AB
2|B
2aB
2aB
3�B
3hB
3MB
4TB
49B
4TB
49B
4nB
4TB
4�B
5tB
5?B
5ZB
5ZB
5�B
5�B
6`B
6FB
6FB
6FB
6zB
7fB
6`B
7fB
7fB
7fB
7fB
7�B
7fB
7fB
7fB
8�B
8�B
8lB
8�B
9�B
9rB
9XB
9rB
9rB
9XB
9�B
9rB
9rB
9�B
9�B
9rB
:�B
:xB
:xB
:�B
:�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
@�B
?�B
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
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
HB
G�B
H�B
H�B
H�B
IB
I�B
J�B
J�B
J�B
J�B
KB
J�B
K�B
K�B
K�B
K�B
MB
MB
MB
M�B
M�B
NB
OB
OB
N�B
OB
P.B
P.B
P.B
QB
QB
Q B
QB
Q4B
RB
RB
RB
RB
RB
RB
R B
S@B
T,B
TB
T,B
TB
TB
TB
U2B
T�B
UB
UB
UB
VB
VB
VB
UB
U2B
V9B
VB
VB
VB
V9B
V9B
W?B
W
B
W
B
W
B
W$B
W
B
W$B
W$B
W?B
W?B
X+B
XEB
X+B
X+B
X+B
X+B
XEB
Y1B
YB
Y1B
Y1B
YKB
YB
Y1B
Y1B
Z7B
ZQB
Z7B
Z7B
Z7B
ZB
Z7B
[#B
[=B
[#B
[=B
\CB
\)B
\CB
\CB
\CB
\CB
\CB
\]B
]dB
]IB
]IB
]/B
]/B
]/B
]/B
]IB
]dB
]IB
]IB
]IB
]IB
]dB
]dB
^OB
^�B
^jB
^jB
^jB
^jB
_VB
_VB
_pB
_VB
_pB
`vB
`vB
`vB
`vB
`\B
abB
abB
a|B
abB
abB
abB
b�B
bhB
bNB
bhB
bhB
cnB
c�B
cnB
cnB
cnB
cnB
c�B
dtB
d�B
dtB
dtB
d�B
dtB
dtB
ezB
ezB
e`B
ezB
e�B
ezB
e�B
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
h�B
h�B
i�B
i�B
i�B
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
k�B
k�B
k�B
l�B
l�B
l�B
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
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
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
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608230036532016082300365320160823003653201806221300592018062213005920180622130059201804050700002018040507000020180405070000  JA  ARFMdecpA19c                                                                20160819093510  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160819003529  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160819003530  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160819003530  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160819003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160819003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160819003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160819003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160819003531  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160819003531                      G�O�G�O�G�O�                JA  ARUP                                                                        20160819012137                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160819153542  CV  JULD            G�O�G�O�F�!�                JM  ARCAJMQC2.0                                                                 20160822153653  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160822153653  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220000  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040059  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201515                      G�O�G�O�G�O�                