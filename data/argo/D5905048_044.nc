CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-10-03T00:35:23Z creation;2016-10-03T00:35:26Z conversion to V3.1;2019-12-19T08:25:09Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20161003003523  20200116211517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ,A   JA  I2_0577_044                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��v3��1   @��v��b�@3��!�.I�d��p:�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DSy�DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ D�|�D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B33B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�fC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D 3D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DSvfDS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��3D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�{3D˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��D�;31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A�A�A�%A�A�A�A�A�  A�A�  A�  A�  A�  A���A���A���A��A��TA��A��A�/AոRA��/A�9XA���A�
=A�
=A�^5A�XA�7LA̾wA���A�M�A�ĜA�A�A���A�x�Aś�A�v�A�$�A��yA�-A��A�A�=qA�`BA�t�A��/A�?}A�bA�I�A���A�-A�XA�;dA�E�A��yA�|�A�n�A���A��uA�7LA�JA��A���A���A�oA�
=A��;A���A�VA��A���A��!A�9XA�n�A�&�A��A�hsA�1'A���A�
=A�I�A��/A��PA���A�p�A�9XA���A� �A�9XA���A�ƨA��mA�A|jAy+Av��AuhsAt�RAq
=Ao�TAnr�Ak��Aj�Aj��Aj$�Ah��AhJAfȴAehsAdv�Ac|�AbAaoA`n�A`{A_`BA\��A[x�A[/AY|�AWXAU+ASx�AQ�AP-AOAOl�AN9XAL�`ALAJ�\AH�HAG��AGG�AG�AF^5AE`BAD��AB�uA@ �A>�A;��A9p�A8�uA733A5K�A3�-A2��A25?A1�-A1"�A0ZA.�A-;dA+�A)��A'��A&jA$=qA"ZA!VA bNAx�Az�A�FA^5A�/AQ�AO�A�Av�AhsA��A�A��A�7A��A��A�hAA�AO�AQ�A��A?}A
�A�9A�A�9AK�A$�AdZA7LA~�A �y@�@�`B@��/@�z�@���@���@��@��T@���@�E�@�bN@�-@�&�@��9@��m@��@�(�@�w@��T@�R@��T@�x�@��@�@��@�"�@��@�u@�ƨ@�o@�@���@��@�Ĝ@�t�@�&�@��y@�5?@ݩ�@��`@�;d@�~�@��T@�G�@�r�@�|�@�\)@�+@��H@�v�@�X@���@��@җ�@�{@� �@�"�@�E�@�j@ʗ�@�5?@�$�@��@ɺ^@ȣ�@���@�hs@�`B@�r�@�A�@�  @��@�ƨ@ǝ�@�;d@ř�@���@ēu@ċD@���@ļj@ă@�Q�@ÍP@���@�p�@���@�Z@�r�@�b@���@�K�@���@�-@�p�@�&�@���@���@�1'@��@�@��7@���@��u@���@�
=@�v�@��#@�p�@�V@�hs@��@��w@��@���@�|�@�-@��/@�hs@��j@�Z@� �@��;@��
@���@���@��+@�o@�@�v�@�M�@�M�@���@�X@� �@�9X@�(�@��@�33@�+@��y@���@���@��@��7@�7L@��@��/@��@��u@�9X@��@�(�@�I�@��w@��@�;d@�~�@��@�-@�5?@�v�@�@�X@�I�@��@��@� �@�I�@��m@��@�o@�t�@�\)@��R@�n�@�V@�5?@�M�@�&�@���@�b@�C�@�M�@�{@�{@�+@��F@��@���@�=q@�v�@�E�@��h@��/@��@�ff@�=q@���@�V@�ff@�A�@���@�J@�E�@��@���@��T@�{@�V@��@�hs@��-@�5?@�@���@�V@��@��P@�x�@��@��/@��`@��#@�\)@�;d@��@��R@��@��h@��@�x�@��@���@�I�@�b@��@��@��@��m@��
@���@��w@��F@��@��P@�K�@��@��!@�~�@�J@���@��@�p�@�O�@�/@��/@��9@���@�r�@���@�|�@�C�@��H@��\@���@���@�v�@�M�@�5?@��@�hs@���@�I�@�A�@��@��@���@��@���@�|�@�l�@�\)@�o@���@�=q@��T@���@�@�@��h@�x�@�/@�%@��/@��9@�bN@�1@��;@���@��@�+@��+@��@�@���@��7@�X@��@��@�z�@�1@�K�@�o@��y@���@�~�@���@�@���@��@��j@��@���@��u@��D@��@�A�@��@��@\)@;d@�@
=@~�+@}@}p�@}/@}V@|�j@|�D@|Z@|�@{�@{@z�!@zn�@zJ@y�#@y�^@y��@y&�@x��@xA�@x  @w�w@v�y@v�+@vv�@vff@vV@vE�@v{@u�T@u��@u/@t�@t�D@s�
@sdZ@s@r�!@q�@p��@pb@pb@o�@oK�@o+@n�@n��@n$�@m��@m�@l�@l�@kƨ@jn�@ix�@h��@g�;@g�@f�y@f�R@fv�@fE�@f{@e�T@e@e?}@dZ@d(�@c��@c��@c�
@c�@c"�@b��@b=q@a�@a�^@`��@^��@^��@^��@^��@^��@^��@^��@^��@^v�@^V@]�@\z�@[�
@[��@[33@Z�\@Zn�@Z=q@Y��@Y�7@Yx�@YX@Y�@XA�@Xb@X  @W�@W;d@V�@VV@U�T@U�h@U/@T��@Tj@T(�@S�
@S�F@SdZ@SC�@S@R�\@Q��@Qx�@Q7L@P��@P��@P��@Pr�@PA�@O�w@O�P@O�P@OK�@N�y@NE�@M��@M�-@Mp�@M/@MV@L�/@Lz�@K�m@K��@KS�@KS�@K33@K@J��@J-@I�@I�^@I�^@I��@I��@Ihs@I&�@H��@H�9@H�@Hr�@H1'@G��@G|�@G�@F��@FE�@F5?@E�@E`B@E�@D�@D�j@D�D@D�D@DZ@D�@Cƨ@CS�@C@B^5@B�@A��@A�#@Ax�@@Ĝ@@�9@@r�@@A�@@b@?�@?l�@?;d@?�@>�@>v�@>{@=@=�@=?}@<��@<��@;��@;�
@;�
@;ƨ@;��@;�@;dZ@;33@;33@;"�@;o@;o@:�!@9�#@9x�@9hs@9G�@9�@8�9@81'@7�@7;d@6�@6V@5�@5�h@5/@5V@4�/@4�j@4�@4��@4�D@4j@4�@3��@333@2~�@2=q@2-@2J@1�7@1G�@0��@0�9@0�u@0�@0bN@/�w@/|�@/l�@/
=@.�+@.{@-�-@,��@,j@,9X@,1@+��@+�m@+�m@+�F@+dZ@*�@*-@*J@)�@)��@)�7@)7L@(�`@(�9@(��@(��@(�@(Q�@(1'@(  @'�P@'+@&�y@&�R@&��@&ff@&E�@&$�@%�T@%�-@%�h@$�@$�j@$��@$z�@$Z@#��@#o@"�!@"�\@"n�@"^5@"�@!�@!�#@!�^@!��@!G�@!&�@!%@ ��@ �9@ r�@�@�@K�@
=@�R@ff@5?@$�@�@/@�@V@V@��@��@�/@�@�/@�/@��@��@��@�j@�@��@z�@z�@Z@��@ƨ@ƨ@�
@ƨ@��@t�@33@33@"�@o@�@��@M�@�^@��@G�@G�@7L@7L@%@�9@r�@1'@�;@�@��@�P@l�@K�@��@ff@$�@@@��@�@?}@�@��@�j@�@�@z�@�@��@�
@��@S�@33@33@C�@o@@�H@�!@-@X@�@�`@�`@��@�u@A�@�@;d@�@
=@��@
=@
=@
=@��@�y@�y@ȴ@�R@�R@��@v�@E�@E�@@�-@�@?}@V@�/@��@z�@j@I�@�@�m@ƨ@�F@��@t�@dZ@C�@33@"�@o@
�H@
�!@
n�@
=q@	�#@	�^@	��@	X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A�A�A�%A�A�A�A�A�  A�A�  A�  A�  A�  A���A���A���A��A��TA��A��A�/AոRA��/A�9XA���A�
=A�
=A�^5A�XA�7LA̾wA���A�M�A�ĜA�A�A���A�x�Aś�A�v�A�$�A��yA�-A��A�A�=qA�`BA�t�A��/A�?}A�bA�I�A���A�-A�XA�;dA�E�A��yA�|�A�n�A���A��uA�7LA�JA��A���A���A�oA�
=A��;A���A�VA��A���A��!A�9XA�n�A�&�A��A�hsA�1'A���A�
=A�I�A��/A��PA���A�p�A�9XA���A� �A�9XA���A�ƨA��mA�A|jAy+Av��AuhsAt�RAq
=Ao�TAnr�Ak��Aj�Aj��Aj$�Ah��AhJAfȴAehsAdv�Ac|�AbAaoA`n�A`{A_`BA\��A[x�A[/AY|�AWXAU+ASx�AQ�AP-AOAOl�AN9XAL�`ALAJ�\AH�HAG��AGG�AG�AF^5AE`BAD��AB�uA@ �A>�A;��A9p�A8�uA733A5K�A3�-A2��A25?A1�-A1"�A0ZA.�A-;dA+�A)��A'��A&jA$=qA"ZA!VA bNAx�Az�A�FA^5A�/AQ�AO�A�Av�AhsA��A�A��A�7A��A��A�hAA�AO�AQ�A��A?}A
�A�9A�A�9AK�A$�AdZA7LA~�A �y@�@�`B@��/@�z�@���@���@��@��T@���@�E�@�bN@�-@�&�@��9@��m@��@�(�@�w@��T@�R@��T@�x�@��@�@��@�"�@��@�u@�ƨ@�o@�@���@��@�Ĝ@�t�@�&�@��y@�5?@ݩ�@��`@�;d@�~�@��T@�G�@�r�@�|�@�\)@�+@��H@�v�@�X@���@��@җ�@�{@� �@�"�@�E�@�j@ʗ�@�5?@�$�@��@ɺ^@ȣ�@���@�hs@�`B@�r�@�A�@�  @��@�ƨ@ǝ�@�;d@ř�@���@ēu@ċD@���@ļj@ă@�Q�@ÍP@���@�p�@���@�Z@�r�@�b@���@�K�@���@�-@�p�@�&�@���@���@�1'@��@�@��7@���@��u@���@�
=@�v�@��#@�p�@�V@�hs@��@��w@��@���@�|�@�-@��/@�hs@��j@�Z@� �@��;@��
@���@���@��+@�o@�@�v�@�M�@�M�@���@�X@� �@�9X@�(�@��@�33@�+@��y@���@���@��@��7@�7L@��@��/@��@��u@�9X@��@�(�@�I�@��w@��@�;d@�~�@��@�-@�5?@�v�@�@�X@�I�@��@��@� �@�I�@��m@��@�o@�t�@�\)@��R@�n�@�V@�5?@�M�@�&�@���@�b@�C�@�M�@�{@�{@�+@��F@��@���@�=q@�v�@�E�@��h@��/@��@�ff@�=q@���@�V@�ff@�A�@���@�J@�E�@��@���@��T@�{@�V@��@�hs@��-@�5?@�@���@�V@��@��P@�x�@��@��/@��`@��#@�\)@�;d@��@��R@��@��h@��@�x�@��@���@�I�@�b@��@��@��@��m@��
@���@��w@��F@��@��P@�K�@��@��!@�~�@�J@���@��@�p�@�O�@�/@��/@��9@���@�r�@���@�|�@�C�@��H@��\@���@���@�v�@�M�@�5?@��@�hs@���@�I�@�A�@��@��@���@��@���@�|�@�l�@�\)@�o@���@�=q@��T@���@�@�@��h@�x�@�/@�%@��/@��9@�bN@�1@��;@���@��@�+@��+@��@�@���@��7@�X@��@��@�z�@�1@�K�@�o@��y@���@�~�@���@�@���@��@��j@��@���@��u@��D@��@�A�@��@��@\)@;d@�@
=@~�+@}@}p�@}/@}V@|�j@|�D@|Z@|�@{�@{@z�!@zn�@zJ@y�#@y�^@y��@y&�@x��@xA�@x  @w�w@v�y@v�+@vv�@vff@vV@vE�@v{@u�T@u��@u/@t�@t�D@s�
@sdZ@s@r�!@q�@p��@pb@pb@o�@oK�@o+@n�@n��@n$�@m��@m�@l�@l�@kƨ@jn�@ix�@h��@g�;@g�@f�y@f�R@fv�@fE�@f{@e�T@e@e?}@dZ@d(�@c��@c��@c�
@c�@c"�@b��@b=q@a�@a�^@`��@^��@^��@^��@^��@^��@^��@^��@^��@^v�@^V@]�@\z�@[�
@[��@[33@Z�\@Zn�@Z=q@Y��@Y�7@Yx�@YX@Y�@XA�@Xb@X  @W�@W;d@V�@VV@U�T@U�h@U/@T��@Tj@T(�@S�
@S�F@SdZ@SC�@S@R�\@Q��@Qx�@Q7L@P��@P��@P��@Pr�@PA�@O�w@O�P@O�P@OK�@N�y@NE�@M��@M�-@Mp�@M/@MV@L�/@Lz�@K�m@K��@KS�@KS�@K33@K@J��@J-@I�@I�^@I�^@I��@I��@Ihs@I&�@H��@H�9@H�@Hr�@H1'@G��@G|�@G�@F��@FE�@F5?@E�@E`B@E�@D�@D�j@D�D@D�D@DZ@D�@Cƨ@CS�@C@B^5@B�@A��@A�#@Ax�@@Ĝ@@�9@@r�@@A�@@b@?�@?l�@?;d@?�@>�@>v�@>{@=@=�@=?}@<��@<��@;��@;�
@;�
@;ƨ@;��@;�@;dZ@;33@;33@;"�@;o@;o@:�!@9�#@9x�@9hs@9G�@9�@8�9@81'@7�@7;d@6�@6V@5�@5�h@5/@5V@4�/@4�j@4�@4��@4�D@4j@4�@3��@333@2~�@2=q@2-@2J@1�7@1G�@0��@0�9@0�u@0�@0bN@/�w@/|�@/l�@/
=@.�+@.{@-�-@,��@,j@,9X@,1@+��@+�m@+�m@+�F@+dZ@*�@*-@*J@)�@)��@)�7@)7L@(�`@(�9@(��@(��@(�@(Q�@(1'@(  @'�P@'+@&�y@&�R@&��@&ff@&E�@&$�@%�T@%�-@%�h@$�@$�j@$��@$z�@$Z@#��@#o@"�!@"�\@"n�@"^5@"�@!�@!�#@!�^@!��@!G�@!&�@!%@ ��@ �9@ r�@�@�@K�@
=@�R@ff@5?@$�@�@/@�@V@V@��@��@�/@�@�/@�/@��@��@��@�j@�@��@z�@z�@Z@��@ƨ@ƨ@�
@ƨ@��@t�@33@33@"�@o@�@��@M�@�^@��@G�@G�@7L@7L@%@�9@r�@1'@�;@�@��@�P@l�@K�@��@ff@$�@@@��@�@?}@�@��@�j@�@�@z�@�@��@�
@��@S�@33@33@C�@o@@�H@�!@-@X@�@�`@�`@��@�u@A�@�@;d@�@
=@��@
=@
=@
=@��@�y@�y@ȴ@�R@�R@��@v�@E�@E�@@�-@�@?}@V@�/@��@z�@j@I�@�@�m@ƨ@�F@��@t�@dZ@C�@33@"�@o@
�H@
�!@
n�@
=q@	�#@	�^@	��@	X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B"�B"�B"�B"�B"�B"�B#�B#�B#�B#�B#�B$�B$�B$�B#�B#�B#�B$�B$�B$�B$�B$�B#�B#�B$�B$�B$�B$�B$�B$�B#�B$�B$�B$�B$�B$�B!�B\B
�B
�B
�/B
�/B
�;B
�B6FBe`B��BZBM�BhsB��B��B��B��B+B�B<jBC�BXBbNBv�B�B�hB��B�?B�?B��B��B�
B�5B�5B��BǮB�-B��B�\B~�BdZBK�BF�BE�B7LB!�B �B�BB�sB�B�B�JB��B�LB�FB�B��B��B��B�By�Bt�Bl�BhsBdZBbNB[#BH�B>wB$�B%B
�sB
ǮB
��B
�B
dZB
Q�B
A�B
5?B
0!B
�B
uB
DB	��B	��B	��B	�B	�B	�`B	�5B	�B	��B	ȴB	��B	�^B	�?B	�'B	�B	��B	�oB	�\B	�%B	w�B	k�B	`BB	R�B	K�B	H�B	F�B	B�B	;dB	6FB	0!B	%�B	�B	�B	�B	�B	{B	bB	+B��B�B�B�5B�B��B��BǮBÖBŢBŢBÖBB�^B�FB�B��B��B��B��B��B��B��B��B��B��B�VB�\B�JB�PB�oB�uB�bB�VB�VB�1B�JB��B��B��B�uB��B��B��B�\B�1B�B�B�B�B�B}�B{�Bz�Bt�Bv�Bp�Br�Bu�By�B|�B{�B� B�+B�oB�{B�oB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�FB�RB�^B�^B�jB�wB�wB��BÖBǮB��B��B��B��B��B�
B�B�)B�/B�ZB�`B�`B�`B�`B�sB�yB�B�B�B��B��B	B	B	B	B	B	B	B	B	%B		7B	DB	VB	hB	�B	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	-B	0!B	6FB	7LB	;dB	;dB	<jB	?}B	B�B	J�B	M�B	N�B	R�B	T�B	W
B	YB	\)B	^5B	aHB	e`B	jB	x�B	~�B	�B	�B	�B	�B	�B	�=B	�7B	�7B	�7B	�=B	�=B	�PB	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�B	�B	�'B	�9B	�?B	�XB	�XB	�^B	�dB	�qB	�wB	�}B	��B	��B	��B	B	ÖB	ƨB	ƨB	ƨB	ĜB	��B	B	ŢB	ȴB	ɺB	ȴB	ȴB	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�#B	�BB	�;B	�/B	�;B	�HB	�NB	�5B	�)B	�B	�
B	�
B	�
B	�B	�)B	�B	��B	��B	��B	�
B	�B	�)B	�BB	�TB	�ZB	�TB	�mB	�B	�B	�B	�B	�B	�B	�fB	�mB	�yB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
+B
+B
+B
%B
%B
%B
	7B

=B

=B

=B

=B

=B
1B
+B
1B
	7B
DB
PB
PB
PB
PB
PB
PB
PB
JB
JB
JB
PB
VB
hB
oB
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
'�B
'�B
(�B
(�B
'�B
(�B
(�B
)�B
)�B
+B
+B
+B
,B
,B
,B
,B
,B
)�B
+B
+B
+B
,B
,B
,B
,B
,B
-B
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
1'B
1'B
1'B
2-B
33B
49B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
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
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
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
>wB
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
A�B
A�B
A�B
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
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
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
M�B
M�B
M�B
M�B
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
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
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
[#B
\)B
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
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
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
jB
jB
jB
jB
jB
jB
jB
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
l�B
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
p�B
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
t�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
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
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B"�B"�B"�B"�B"�B"�B#�B#�B#�B#�B#�B$�B$�B$�B#�B#�B#�B$�B$�B$�B$�B$�B#�B#�B$�B$�B$�B$�B$�B$�B#�B$�B$�B%,B%�B'8B%�B�B
��B
�B
��B
ߤB
�|B
�FB8Bh
B�'B\CBP}Bk�B��B�MB�ZB�PB	7B �B=qBFBY�Bc�BxB�B��B��B�8B�fB��B��BؓBߊB�\B�B��B�tB�B��B��Bj0BMBG�BH�B:�B$@B#�B�B?B�B�vB�vB�6B�B�B�RB��B��B�5B��B�aB{�Bv`Bm�Bi�BffBe�B]�BKBBAB)_B
XB
�B
�JB
�B
�YB
h
B
T{B
C{B
72B
3�B
�B
�B
"B	��B	�fB	��B	�B	� B	�B	��B	�YB	�HB	�XB	��B	�0B	�B	��B	��B	�!B	��B	��B	��B	z�B	m�B	b�B	TB	L�B	I�B	HKB	D3B	<�B	8RB	1�B	'B	jB	WB	�B	�B	�B	[B	
=B��B��B�CB߾B�=B�SB͹B��B�gB�tBƨB�BĜB��B��B�IB�yB��B��B�B�7B��B��B��B��B�eB�.B�bB��B�<B�&B��B��B�bB��B��B��B��B�=B�YB�B�IB��B�_B�B�	B�tB��B��B��B�'B~�B}VB|�Bv�Bw�Bq'BsBvzBz�B}<B|B�B��B��B��B�@B�B��B��B��B�pB�-B��B�OB�B�B�@B��B��B�B��B��B��B��B�sB�>B�B�CB��B��B��B��B�B�dB�B�B��B� B�3B��B�B�(B�bB��B��BרBڠB��BޞB�,B�2B�B�B�B�B�B��B�B�}B��B�"B	�B	aB	GB	-B	GB	{B	�B	B	�B		�B	DB	<B	�B	�B	�B	QB	�B	B	B	�B	�B	#:B	%FB	-]B	0�B	6�B	7�B	;�B	;�B	<�B	@ B	C{B	KDB	N<B	O\B	S[B	U�B	W�B	Y�B	\�B	^�B	abB	d�B	i�B	xlB	B	�AB	�{B	��B	��B	� B	��B	��B	�lB	�lB	��B	��B	��B	�B	�MB	��B	�B	�B	��B	�B	��B	��B	��B	�B	�2B	�8B	�
B	�0B	�B	�]B	��B	�vB	�OB	�IB	�[B	�nB	�tB	��B	��B	�^B	��B	��B	��B	��B	�B	��B	��B	ªB	ðB	�EB	�B	�_B	��B	��B	�[B	ŢB	�B	�#B	ȴB	ȚB	�B	�HB	�4B	�B	�2B	�9B	��B	�mB	�uB	҉B	�hB	��B	��B	�oB	�	B	��B	��B	�B	�VB	�B	��B	��B	�IB	�B	�?B	�$B	�
B	چB	�~B	��B	�[B	� B	�,B	�?B	�7B	�B	�\B	�B	�B	�:B	�8B	��B	�B	�5B	�5B	�cB	��B	��B	�B	�DB	��B	��B	��B	�HB	�}B	��B	�<B	�B	�B	�]B	�HB
 4B
;B
 B
'B
B
B
-B
-B
GB
-B
MB
MB
SB
mB
tB
YB
tB
�B
KB
	RB
	lB
	lB
	�B
	RB
	RB
	lB
	�B
�B
_B
�B
YB
?B
?B
	RB

XB

XB

rB

�B

�B
fB
EB
fB
	RB
DB
jB
�B
�B
�B
jB
�B
�B
�B
~B
dB
�B
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!B
 �B
!�B
!�B
!�B
"B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
'B
'B
'8B
'8B
(>B
($B
(>B
(>B
)DB
)DB
(
B
)*B
)B
*B
*B
+B
+6B
+QB
,=B
,=B
,WB
,�B
,qB
*B
+QB
+QB
+6B
,"B
,"B
,"B
,"B
,"B
-)B
-CB
-]B
-]B
.IB
.IB
/B
/OB
/5B
/iB
/OB
0UB
1AB
1vB
1�B
2�B
3MB
49B
4TB
49B
49B
4TB
49B
4nB
4nB
4�B
5�B
6zB
6zB
6zB
6zB
7fB
7fB
7�B
7LB
7fB
7fB
7�B
7�B
8�B
8�B
8�B
8�B
8�B
9�B
9�B
9rB
9�B
:xB
:�B
:�B
:xB
:xB
:xB
:xB
:xB
:�B
;�B
;�B
<�B
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
>�B
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
A�B
A�B
A�B
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
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
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
MB
MB
NB
M�B
M�B
M�B
M�B
NB
NB
OB
N�B
N�B
N�B
N�B
OB
O(B
O�B
O�B
PB
O�B
PB
QB
QB
Q4B
Q4B
R:B
R:B
S&B
SB
SB
S&B
S&B
SB
S�B
TB
TB
T,B
T,B
T,B
UMB
U2B
VB
V9B
V9B
VB
V9B
W$B
W?B
W$B
W$B
WYB
XEB
X+B
XEB
XEB
YKB
YKB
YeB
ZQB
[=B
[=B
[#B
[=B
[#B
[=B
[WB
[WB
\]B
]IB
]IB
]IB
]dB
]IB
]IB
^OB
^5B
^OB
^OB
^jB
^jB
_VB
_pB
_VB
`\B
`vB
`vB
`\B
a|B
abB
abB
abB
abB
a|B
bhB
bhB
b�B
bhB
b�B
c�B
dtB
dtB
d�B
dtB
d�B
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
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
jB
j�B
jB
jB
j�B
jB
j�B
jB
jB
jB
jB
j�B
j�B
jB
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
l�B
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
p�B
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
uB
tB
s�B
s�B
t�B
u�B
u�B
u�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
y	B
x�B
x�B
x�B
x�B
y	B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
zB
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201610070035132016100700351320161007003513201806221303012018062213030120180622130301201804050702332018040507023320180405070233  JA  ARFMdecpA19c                                                                20161003093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161003003523  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161003003524  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161003003524  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161003003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161003003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161003003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161003003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161003003525  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161003003526                      G�O�G�O�G�O�                JA  ARUP                                                                        20161003012208                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161003153549  CV  JULD            G�O�G�O�F�{�                JM  ARCAJMQC2.0                                                                 20161006153513  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161006153513  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220233  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040301  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211517                      G�O�G�O�G�O�                