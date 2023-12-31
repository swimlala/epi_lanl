CDF   
   
      N_PROF        N_LEVELS  �   N_CALIB       STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         	DATE_TIME         N_PARAM       	N_HISTORY                title         Argo float vertical profile    institution       CSIRO      source        
Argo float     history       �2013-07-29T01:07:57Z creation;2014-08-21T06:00:20Z update;2014-10-22T13:19:09Z update;2015-02-10T05:17:04Z update;2017-04-28T07:08:23Z update;2017-04-28T07:16:16Z update      
references        (http://www.argodatamgt.org/Documentation   user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7   REFERENCE_DATE_TIME       
         	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_CREATION         
         	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7,   DATE_UPDATE       
         	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7<   PLATFORM_NUMBER                    	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7L   PROJECT_NAME                   	long_name         Name of the project    
_FillValue                  @  7T   PI_NAME                    	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS                        conventions       Argo reference table 3     	long_name         ,List of available parameters for the station   
_FillValue                  0  7�   CYCLE_NUMBER                	long_name         Float cycle number     
_FillValue         ��   conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle           8   	DIRECTION                   	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                    	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8   DC_REFERENCE                   	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8   DATA_STATE_INDICATOR                   	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    80   	DATA_MODE                   	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    84   PLATFORM_TYPE                      	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     88   FLOAT_SERIAL_NO                    	long_name         Serial number of the float     
_FillValue                     8X   FIRMWARE_VERSION                   	long_name         Instrument firmware version    
_FillValue                     8x   WMO_INST_TYPE                      	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD                standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    conventions       8Relative julian days with decimal part (as parts of day)   units         "days since 1950-01-01 00:00:00 UTC     
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                 	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                   	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   LATITUDE                	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            8�   	LONGITUDE                   	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            8�   POSITION_QC                 	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                     	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                 	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                 	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                 	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME          	         	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                   	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES                
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    axis      Z      
_FillValue        G�O�   	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  9�   PRES_QC                    	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IP   PRES_ADJUSTED                   	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    
_FillValue        G�O�   	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  M0   PRES_ADJUSTED_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP                	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `�   TEMP_QC                    	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED                   	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  s�   TEMP_ADJUSTED_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL                	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �,   PSAL_QC                    	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED                   	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_ADJUSTED_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR                    	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   units         decibar    
_FillValue        G�O�     t  ��   TEMP_ADJUSTED_ERROR                    	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   
_FillValue        G�O�     t  �H   PSAL_ADJUSTED_ERROR                    	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   
_FillValue        G�O�     t  ̼   	PARAMETER                            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION                   	         	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT                	         	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT                	         	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE                   
         	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                       	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                      	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                      	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                      	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                         	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE             
         	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                        	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                         	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                     	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                      	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                     	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                        	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1  1.219500101000000  20110624143545  20180613230846  1901149 Argo Australia                                                  Susan Wijffels                                                  PRES            TEMP            PSAL               A   CS  1901149/20                      2C  D   APEX                            5048                            22610                           846 @������1   @���7�Ԁ�C�A�7K�@D�n��O�1   GPS     A   A   A   Primary sampling: averaged []                                                                                                                                                                                                                                      @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|y�D}  D}� D}��D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�<�D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��G@�{A=qA"=qAB=qAb=qA��A��A��A��A��A��A��A��B �\B�\B�\B�\B �\B(�\B0�\B8�\B@�\BH�\BP�\BX�\B`�\Bh�\Bp�\Bx�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C #�C#�C#�C#�C#�C
#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C #�C"#�C$#�C&#�C(#�C*#�C,#�C.#�C0#�C2#�C4#�C6#�C8#�C:#�C<#�C>#�C@#�CB#�CD#�CF#�CH#�CJ#�CL#�CN#�CP#�CR#�CT#�CV#�CX#�CZ#�C\#�C^#�C`#�Cb#�Cd#�Cf#�Ch#�Cj#�Cl#�Cn#�Cp#�Cr#�Ct#�Cv#�Cx#�Cz#�C|#�C~#�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^�\D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��HD��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��HD��{D�{D�AHD��{D��{D�HD�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�D{D��{D��HD�{D�D{D��{D��{D�{D�D{D{D��{D�{D�D{DÄ{D��{D�{D�D{DĄ{D��{D�{D�D{Dń{D��{D�{D�D{DƄ{D��{D�{D�D{DǄ{D��{D�{D�D{DȄ{D��{D�{D�D{DɄ{D��{D�{D�D{Dʄ{D��{D�{D�D{D˄{D��{D�{D�D{D̄{D��{D�{D�D{D̈́{D��{D�{D�D{D΄{D��{D�{D�D{Dτ{D��{D�{D�D{DЄ{D��{D�{D�D{Dф{D��{D�{D�D{D҄{D��{D�{D�D{Dӄ{D��{D�{D�D{DԄ{D��{D�{D�D{DՄ{D��{D�{D�D{Dք{D��{D�{D�D{Dׄ{D��{D�{D�D{D؄{D��{D�{D�D{Dل{D��{D�{D�D{Dڄ{D��{D�{D�D{Dۄ{D��{D�{D�D{D܄{D��{D�{D�D{D݄{D��{D�{D�D{Dބ{D��{D�{D�D{D߄{D��{D�{D�D{D��{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D��{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D�{D��{D�{D�D{D��{D��{D�{D�D{D��{D��{D�{D�G�D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�1A�1A�JA�oA�oA�oA�oA�oA�{A�oA�bA�VA�oA��A��A��A��A��A� �A� �A��A��A� �A�"�A�$�A�$�A�$�A�$�A�$�A�&�A�(�A�(�A�(�A�&�A�&�A�&�A�&�A�&�A�&�A�(�A�(�A�+A�+A�+A�(�A�(�A�&�A�(�A�&�A�$�A�"�A�"�A�$�A�$�A�$�A�&�A�&�A�$�A� �A� �A��A� �A� �A� �A��A��A��A��A��A��A��A��A�A��`A��#A�ĜA��RA��RA��-A���A���A���A���A���A���A���A��hA��PA��A�z�A�`BA�VA�G�A��^A�(�A�
=A��mA�A�33A���A��HA��7A�\)A�7LA���A���A�bNA�M�A�/A���A���A��uA�v�A�A�A�A���A���A���A��hA��7A�XA�A�A�+A�A�TA�TAƨA�AC�A~�/A~�9A~�A}33A|(�Az��Az~�Azn�Az(�Ay�TAy\)AxA�Aw�TAw�-Awx�Aw�Av��AvZAu�mAu"�As�As|�Ar�/Arv�Ar1Aq\)Ap��Ap��Ap(�Ap1Ao��Ao�
Ao�hAo/An��An1'AmC�Al��AlI�AkAk�hAk�Akx�AkO�Ajr�Ai�#AiXAh��Ag��Ag?}Ag7LAg/Ag�Af�jAf�+AfjAfQ�Af�Ae�Ad��Ac��Ab��Ab�+AbJAa�
Aa|�A`�RA`bNA`JA_��A_��A_�wA_�^A_�A_�hA_|�A_S�A^�9A^E�A]A]x�A\��A\r�A\{A[�;A[��A[�AZĜAZE�AY�PAY�AXn�AX  AW�AW��AX  AW�AW�AW"�AVbNAVAU�-AUt�AT�`AT(�AS�AS�
AS�FAS��AS\)AS&�AS%AR�yAR�RARA�AQ�mAQAQ�-AP�yAP1'AP�AO�wAOO�AN��AN��AN~�ANv�ANn�ANM�AN(�AN  AMG�ALbNAK�TAK��AK��AK�AJ�\AJ5?AJJAI�FAIK�AH�\AHn�AH5?AHbAG��AF��AEAC�-AB�jAA��AA��AA\)AAG�AA/AA/AA/AA+A@��A@�HA@��A@1A?�PA?hsA?G�A?&�A>�A>��A>v�A>bA=��A=%A<�A;ƨA;`BA:�A:�A9�FA97LA8�RA8��A8��A8��A8�\A8�+A8~�A8r�A8E�A8-A8A7A7�A7"�A6��A6$�A5/A5%A4�`A41'A3`BA3K�A3"�A2�/A2^5A1��A0��A0�A0��A0�A//A-�#A-K�A,��A,bNA+�A+�A*��A*E�A)33A(bNA'�A&I�A%7LA$~�A$ �A$JA#�TA#��A#dZA"��A"ĜA"�DA"z�A"r�A"Q�A"JA!�A ��A �+A $�A�mA�
A��A��A�A=qA��A|�A/A%A�yA��A1'A�TA�wA`BA�A �AA�AAx�A
=A�`A��A�\AbNAG�A��A;dA�RA=qA(�A�AA��A�uA�+A^5A�A�#A��Ap�AƨA�+A�wAG�A
~�A��AC�A�RA�TA��AjAA�A��AK�AA�A��A��AK�A;dA ��A ȴA �uA v�A A�@���@��@��@��+@�M�@�-@��@�%@�"�@�D@�P@�E�@���@�C�@�?}@�Ĝ@��@�;d@�~�@�@�?}@�@��#@��@�v�@�=q@� �@���@� �@��@�&�@�G�@� �@�P@�\)@��@���@�`B@��
@�"�@�~�@���@��`@�Z@�1@��m@�|�@���@��#@Ԭ@�\)@��@�K�@͙�@̴9@ɺ^@��m@��@��m@ÍP@�;d@°!@��@�?}@��`@�bN@���@��P@�{@�7L@��@�7L@�p�@��@��@�x�@�Ĝ@�1'@�33@�=q@��@��@��F@�K�@��@�^5@���@�/@�V@��@���@�Ĝ@��@��u@�Q�@��m@�l�@���@��\@�^5@�@�`B@���@��9@�1'@�o@�-@���@��7@�X@��@�%@���@�A�@��P@��R@�%@��D@�bN@� �@�dZ@���@�v�@�-@��@���@�G�@�7L@���@��@�1@��F@�;d@��y@�ȴ@��+@���@��@�j@�  @��
@���@�K�@���@��+@�hs@��u@�b@���@���@�p�@���@��u@��@��@�+@���@�-@�@���@��T@���@�p�@�/@��u@���@��\@���@�`B@��@���@���@��@�K�@���@��\@�M�@��@�@��@��@���@��`@�A�@��
@�|�@�33@�v�@���@��7@�x�@�x�@�hs@�`B@�X@�?}@��@���@���@�+@���@�=q@���@�@���@��@�`B@�/@�V@���@��u@�z�@�9X@��@��@��@�l�@�C�@�"�@�@���@���@�V@��T@��-@�x�@�X@�?}@�7L@��@���@�bN@�Q�@�A�@�b@�;@�w@��@�P@~��@~ff@~{@}�@}?}@}?}@}�@|�@|z�@|1@{ƨ@{t�@{dZ@{@x�`@x��@xbN@w|�@v5?@up�@t�@t��@tZ@s�F@s��@sC�@r�@p��@pĜ@p�9@p�u@p�@p��@o�;@o�@o
=@n�@o;d@o
=@m`B@k��@i%@g�@f$�@f5?@f$�@d�j@b�H@bJ@a��@a��@a��@a�@bJ@c@c�@`r�@_
=@_�@a&�@a��@a�7@a��@a�@`�9@_�@]�-@\��@\��@\�D@\z�@\�D@\��@\j@\j@\j@\j@\z�@\1@[�F@[��@[�@[S�@Z�@Z�\@ZM�@ZJ@Y�^@Yx�@Yhs@Y�@XĜ@XA�@W�;@W�@R-@O�@Ol�@Ol�@O�@PQ�@QX@P�9@PbN@PQ�@Pr�@Q&�@Qx�@Q�#@R~�@R^5@RJ@Q�@Q��@Q�#@Q�^@Qx�@Q7L@Q�@Q�@P��@PĜ@P�u@Pr�@PA�@P  @O�@Ol�@O�@O
=@N��@Nv�@N5?@N@M��@M?}@M/@M�@MV@L��@L�@L�/@L��@L��@L9X@K��@K�
@K33@K@J�H@J�\@J^5@J�@I�#@I��@I�7@IX@I&�@HbN@G�@G�w@Gl�@GK�@F�R@F�+@FE�@F{@E�@E�@D�@D��@D��@D�D@Dz�@DZ@D�@C��@C��@C��@C��@C�@Ct�@CC�@CC�@CC�@Co@B��@Bn�@B�@A��@AX@AG�@A7L@A&�@A�@A%@A%@A�@A�@A�@A%@@��@@��@@�9@@��@@Q�@@  @?�w@?|�@?\)@?\)@?\)@?K�@?+@>��@>ȴ@>ȴ@>�R@>��@>��@>v�@>V@>V@>V@>5?@>$�@>@=�T@=��@=@=��@=?}@=`B@<��@<�@<�D@<z�@<j@<I�@<9X@<9X@<I�@<j@<Z@<(�@;�
@;dZ@;C�@;33@;"�@:�@:�H@:��@:��@:�\@:~�@:n�@:-@9�#@9�7@9&�@8��@8��@8b@7�P@7l�@6�y@6��@65?@5�@5��@5@5�-@5�@5?}@5/@5�@5V@4��@4�@4�/@4�j@4�j@4�@4�@4�D@49X@4(�@4�@4�@3��@3�
@3�
@3ƨ@3ƨ@3�F@3�@3t�@3t�@3t�@3t�@3dZ@3dZ@3dZ@3dZ@3S�@3C�@3C�@333@3@2�H@2��@2�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�1A�1A�JA�oA�oA�oA�oA�oA�{A�oA�bA�VA�oA��A��A��A��A��A� �A� �A��A��A� �A�"�A�$�A�$�A�$�A�$�A�$�A�&�A�(�A�(�A�(�A�&�A�&�A�&�A�&�A�&�A�&�A�(�A�(�A�+A�+A�+A�(�A�(�A�&�A�(�A�&�A�$�A�"�A�"�A�$�A�$�A�$�A�&�A�&�A�$�A� �A� �A��A� �A� �A� �A��A��A��A��A��A��A��A��A�A��`A��#A�ĜA��RA��RA��-A���A���A���A���A���A���A���A��hA��PA��A�z�A�`BA�VA�G�A��^A�(�A�
=A��mA�A�33A���A��HA��7A�\)A�7LA���A���A�bNA�M�A�/A���A���A��uA�v�A�A�A�A���A���A���A��hA��7A�XA�A�A�+A�A�TA�TAƨA�AC�A~�/A~�9A~�A}33A|(�Az��Az~�Azn�Az(�Ay�TAy\)AxA�Aw�TAw�-Awx�Aw�Av��AvZAu�mAu"�As�As|�Ar�/Arv�Ar1Aq\)Ap��Ap��Ap(�Ap1Ao��Ao�
Ao�hAo/An��An1'AmC�Al��AlI�AkAk�hAk�Akx�AkO�Ajr�Ai�#AiXAh��Ag��Ag?}Ag7LAg/Ag�Af�jAf�+AfjAfQ�Af�Ae�Ad��Ac��Ab��Ab�+AbJAa�
Aa|�A`�RA`bNA`JA_��A_��A_�wA_�^A_�A_�hA_|�A_S�A^�9A^E�A]A]x�A\��A\r�A\{A[�;A[��A[�AZĜAZE�AY�PAY�AXn�AX  AW�AW��AX  AW�AW�AW"�AVbNAVAU�-AUt�AT�`AT(�AS�AS�
AS�FAS��AS\)AS&�AS%AR�yAR�RARA�AQ�mAQAQ�-AP�yAP1'AP�AO�wAOO�AN��AN��AN~�ANv�ANn�ANM�AN(�AN  AMG�ALbNAK�TAK��AK��AK�AJ�\AJ5?AJJAI�FAIK�AH�\AHn�AH5?AHbAG��AF��AEAC�-AB�jAA��AA��AA\)AAG�AA/AA/AA/AA+A@��A@�HA@��A@1A?�PA?hsA?G�A?&�A>�A>��A>v�A>bA=��A=%A<�A;ƨA;`BA:�A:�A9�FA97LA8�RA8��A8��A8��A8�\A8�+A8~�A8r�A8E�A8-A8A7A7�A7"�A6��A6$�A5/A5%A4�`A41'A3`BA3K�A3"�A2�/A2^5A1��A0��A0�A0��A0�A//A-�#A-K�A,��A,bNA+�A+�A*��A*E�A)33A(bNA'�A&I�A%7LA$~�A$ �A$JA#�TA#��A#dZA"��A"ĜA"�DA"z�A"r�A"Q�A"JA!�A ��A �+A $�A�mA�
A��A��A�A=qA��A|�A/A%A�yA��A1'A�TA�wA`BA�A �AA�AAx�A
=A�`A��A�\AbNAG�A��A;dA�RA=qA(�A�AA��A�uA�+A^5A�A�#A��Ap�AƨA�+A�wAG�A
~�A��AC�A�RA�TA��AjAA�A��AK�AA�A��A��AK�A;dA ��A ȴA �uA v�A A�@���@��@��@��+@�M�@�-@��@�%@�"�@�D@�P@�E�@���@�C�@�?}@�Ĝ@��@�;d@�~�@�@�?}@�@��#@��@�v�@�=q@� �@���@� �@��@�&�@�G�@� �@�P@�\)@��@���@�`B@��
@�"�@�~�@���@��`@�Z@�1@��m@�|�@���@��#@Ԭ@�\)@��@�K�@͙�@̴9@ɺ^@��m@��@��m@ÍP@�;d@°!@��@�?}@��`@�bN@���@��P@�{@�7L@��@�7L@�p�@��@��@�x�@�Ĝ@�1'@�33@�=q@��@��@��F@�K�@��@�^5@���@�/@�V@��@���@�Ĝ@��@��u@�Q�@��m@�l�@���@��\@�^5@�@�`B@���@��9@�1'@�o@�-@���@��7@�X@��@�%@���@�A�@��P@��R@�%@��D@�bN@� �@�dZ@���@�v�@�-@��@���@�G�@�7L@���@��@�1@��F@�;d@��y@�ȴ@��+@���@��@�j@�  @��
@���@�K�@���@��+@�hs@��u@�b@���@���@�p�@���@��u@��@��@�+@���@�-@�@���@��T@���@�p�@�/@��u@���@��\@���@�`B@��@���@���@��@�K�@���@��\@�M�@��@�@��@��@���@��`@�A�@��
@�|�@�33@�v�@���@��7@�x�@�x�@�hs@�`B@�X@�?}@��@���@���@�+@���@�=q@���@�@���@��@�`B@�/@�V@���@��u@�z�@�9X@��@��@��@�l�@�C�@�"�@�@���@���@�V@��T@��-@�x�@�X@�?}@�7L@��@���@�bN@�Q�@�A�@�b@�;@�w@��@�P@~��@~ff@~{@}�@}?}@}?}@}�@|�@|z�@|1@{ƨ@{t�@{dZ@{@x�`@x��@xbN@w|�@v5?@up�@t�@t��@tZ@s�F@s��@sC�@r�@p��@pĜ@p�9@p�u@p�@p��@o�;@o�@o
=@n�@o;d@o
=@m`B@k��@i%@g�@f$�@f5?@f$�@d�j@b�H@bJ@a��@a��@a��@a�@bJ@c@c�@`r�@_
=@_�@a&�@a��@a�7@a��@a�@`�9@_�@]�-@\��@\��@\�D@\z�@\�D@\��@\j@\j@\j@\j@\z�@\1@[�F@[��@[�@[S�@Z�@Z�\@ZM�@ZJ@Y�^@Yx�@Yhs@Y�@XĜ@XA�@W�;@W�@R-@O�@Ol�@Ol�@O�@PQ�@QX@P�9@PbN@PQ�@Pr�@Q&�@Qx�@Q�#@R~�@R^5@RJ@Q�@Q��@Q�#@Q�^@Qx�@Q7L@Q�@Q�@P��@PĜ@P�u@Pr�@PA�@P  @O�@Ol�@O�@O
=@N��@Nv�@N5?@N@M��@M?}@M/@M�@MV@L��@L�@L�/@L��@L��@L9X@K��@K�
@K33@K@J�H@J�\@J^5@J�@I�#@I��@I�7@IX@I&�@HbN@G�@G�w@Gl�@GK�@F�R@F�+@FE�@F{@E�@E�@D�@D��@D��@D�D@Dz�@DZ@D�@C��@C��@C��@C��@C�@Ct�@CC�@CC�@CC�@Co@B��@Bn�@B�@A��@AX@AG�@A7L@A&�@A�@A%@A%@A�@A�@A�@A%@@��@@��@@�9@@��@@Q�@@  @?�w@?|�@?\)@?\)@?\)@?K�@?+@>��@>ȴ@>ȴ@>�R@>��@>��@>v�@>V@>V@>V@>5?@>$�@>@=�T@=��@=@=��@=?}@=`B@<��@<�@<�D@<z�@<j@<I�@<9X@<9X@<I�@<j@<Z@<(�@;�
@;dZ@;C�@;33@;"�@:�@:�H@:��@:��@:�\@:~�@:n�@:-@9�#@9�7@9&�@8��@8��@8b@7�P@7l�@6�y@6��@65?@5�@5��@5@5�-@5�@5?}@5/@5�@5V@4��@4�@4�/@4�j@4�j@4�@4�@4�D@49X@4(�@4�@4�@3��@3�
@3�
@3ƨ@3ƨ@3�F@3�@3t�@3t�@3t�@3t�@3dZ@3dZ@3dZ@3dZ@3S�@3C�@3C�@333@3@2�H@2��@2�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BT�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BR�BQ�BO�BN�BM�BL�BL�BL�BL�BK�BK�BK�BK�BK�BK�BK�BK�BJ�BJ�BI�BH�BG�BH�BD�BD�BC�BC�BA�BA�B=qB:^B9XB8RB7LB6FB2-B2-B1'B0!B-B,B+B(�B&�B#�B!�B �B�B �B�B�B�B�B�B�B�B�B�BuBhB\B
=BB��B��B��B��B��B�B�B�B�yB�mB�`B�TB�BB�/B�B��B��B��BȴBƨBB�}B�qB�dB�^B�XB�LB�9B�-B�B�B��B��B��B��B��B��B��B��B��B�oB�VB�=B�+B�B�B�B�B�B� B~�B}�B{�Bw�Br�Bk�BhsBffBcTBbNB_;B[#BYBW
BVBT�BT�BT�BS�BS�BR�BP�BL�BI�BE�BC�B@�B<jB:^B8RB7LB5?B33B1'B,B(�B#�B �B �B �B!�B!�B�B�B�BhB\BJB+BBBBBB  B��B��B��B��B��B�B�B�B�yB�TB�NB�;B�#B�B��B��B��B��B��B��B��BƨB�wB�XB�RB�FB�!B�B��B��B��B��B��B��B�uB�hB�PB�Bt�BiyBaHB[#BXBVBT�BT�BT�BT�BS�BR�BQ�BN�BI�BE�BD�BB�BA�B?}B=qB:^B7LB33B-B%�B!�B�B�BuB\BDB+B%B%B%BBBBBBB  B��B��B��B�B�B�`B�TB�NB�)B�B��B��B��B��BĜB�wB�qB�jB�XB�B��B��B��B��B�oB�\B�1B�By�Bq�BgmB`BBYBT�BR�BQ�BP�BN�BM�BJ�BI�BG�BF�BF�BE�BB�B?}B:^B7LB5?B33B2-B2-B1'B.B%�B!�B!�B�B�B�B�B�B�BuBbBPB1B1B+B%BBB
��B
��B
��B
��B
�B
�`B
�NB
�;B
�)B
�#B
�B
��B
��B
��B
��B
��B
��B
ȴB
��B
�LB
�B
��B
��B
��B
��B
�\B
�%B
�B
}�B
w�B
v�B
u�B
s�B
q�B
m�B
k�B
iyB
gmB
gmB
e`B
dZB
cTB
bNB
`BB
]/B
T�B
P�B
N�B
M�B
M�B
L�B
I�B
D�B
<jB
8RB
33B
0!B
(�B
!�B
�B
uB

=B
1B
%B
%B

=B
JB
PB
bB
hB
DB
	7B
PB
oB
uB
�B
oB
hB
\B
DB
B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�sB	�`B	�NB	�/B	�B	��B	��B	ǮB	��B	�^B	�3B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�!B	�-B	�-B	�-B	�9B	�FB	�?B	�9B	�3B	�-B	�'B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�!B	�'B	�-B	�3B	�3B	�9B	�9B	�?B	�FB	�FB	�FB	�LB	�LB	�RB	�RB	�RB	�RB	�RB	�XB	�XB	�^B	�^B	�dB	�jB	�wB	�wB	�}B	�}B	�}B	�}B	�}B	�}B	�}B	�}B	B	ĜB	ƨB	ǮB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�fB	�fB	�sB	�yB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
B
B
  B	��B
B
B
	7B
JB
VB
\B
\B
JB
DB

=B

=B
DB
VB
\B
{B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
"�B
$�B
%�B
'�B
(�B
)�B
+B
,B
-B
.B
.B
,B
"�B
�B
!�B
#�B
%�B
(�B
.B
.B
0!B
0!B
33B
7LB
9XB
;dB
>wB
>wB
?}B
@�B
@�B
C�B
E�B
F�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
L�B
N�B
O�B
P�B
R�B
S�B
VB
XB
YB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
_;B
`BB
`BB
bNB
cTB
e`B
ffB
ffB
gmB
hsB
hsB
hsB
hsB
hsB
k�B
l�B
l�B
m�B
n�B
o�B
p�B
q�B
q�B
r�B
s�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
|�B
}�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�1B
�=B
�DB
�JB
�PB
�PB
�PB
�PB
�VB
�\B
�hB
�oB
�uB
�uB
�uB
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
�B
�B
�B
�-B
�3B
�9B
�9B
�?B
�FB
�FB
�LB
�LB
�RB
�RB
�^B
�^B
�dB
�qB
�qB
�qB
�wB
�wB
�wB
�wB
��B
B
ÖB
ÖB
ĜB
ĜB
ŢB
ȴB
ȴB
ȴB
ɺB
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
�B
�B
�B
�B
�B
�
B
�
B
�
B
�
B
�
B
�
B
�B
�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BKBJ�BJ�BKBKBKBKBKBKBKBKBJ�BJ�BKBKBJ�BK'BJ�BKBKBKBJ�BKBKBKBKBKBKBKBKBKBKBKBJBJBJBJBJBI�BJBI�BJBJBKBJ
BJBI�BJBJBJBJBI�BJBJBI�BJBJBJBJBJBI�BJBJBJBJBJBJBJBJBJBJ%BIYBH�BF%BERBDBB�BB�BCBB�BA�BA�BA�BA�BA�BA�BA�BBB@�BAHB?�B?B@dBA`B;_B;QB:kB<PB:}B:�B51B1HB0!B/�B/B-oB(�B(�B(�B'B#�B"�B"B 9B�B�B)BBB�B<B6BxB�B�B�BGB7B�B	�B�B�B�B�B�4B�*B�B�B�(B�YB�B�B�B�hB�<BځB�{B�,B�B�CB�B��B��B�XB��B��B��B��B��B��B�B�LB��B�ZB�aB�jB��B�B�QB��B��B�:B��B�B��B�GB��B~FBzUBzPBzxBz	Bw�BvjBu\Bt�BsBp=Bk�Bc>B_�B]�BZ	BYRBW3BR$BPBM�BLBBKCBK.BKABJfBJQBI�BH�BDBAB<�B:�B7�B3�B1B/B.�B,FB*�B)B#TB �BBB�B�B+B�B�B�B�BfB(B�B�"B��B��B��B�tB��B��B�zB�jB�B�'B��B�DB�B�B�mB��B�]B�|B�0B�&B�}B�=B�=B�pB�uBƁB��B�B��B��B�B��B��B�B��B��B�B��B�+B�LB�B��B�GB�4BnJBb*BYkBRVBN�BLxBKwBK;BK9BKFBJ�BIzBH�BF�BA*B<LB;)B9"B8MB6qB4/B1�B.�B+B%�B	BB�BBB
�B�B�B��B�B�hB�zB�wB�vB�B��B��B��B��B��B�B�B�B�%B�%B��B�NB�qB̜B˭B��B�gB��B��B�$B��B�B��B��B��B�dB��B��B�pB��B�B|BrQBk=B_�BYGBQFBL;BI�BH�BG�BE�BE8BA�B@�B>.B=B=NB<�B9�B7�B1�B.�B,0B)�B(�B(}B(DB'�B�B�B�BBSBWBzB�B?B
�BBoB
��B
��B
�B
�:B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�*B
иB
�ZB
�kB
�`B
ƩB
��B
��B
�MB
��B
��B
��B
�VB
�fB
�/B
��B
�B
~'B
{�B
wB
oWB
m�B
l�B
k�B
j�B
d�B
b�B
`�B
^B
^B
\CB
[JB
ZB
YDB
W�B
V�B
M�B
HB
E�B
DlB
D�B
D~B
B�B
>UB
4EB
0oB
+dB
(�B
!�B
B
�B
sB
�B	��B	�<B	�BB
 0B
�B
B
2B

vB
8B	�GB
�B
�B
	�B
UB
	�B
%B
�B
�B	��B	�fB	�<B	�B	� B	�OB	��B	�xB	�3B	�B	��B	�1B	�jB	�~B	�fB	�JB	ɨB	B	��B	��B	��B	�"B	�B	��B	�BB	�LB	��B	��B	�(B	�B	�B	�eB	��B	��B	�`B	�LB	��B	��B	��B	��B	��B	�B	��B	� B	�SB	��B	�,B	�B	�^B	�WB	�qB	��B	��B	��B	��B	��B	��B	��B	�%B	�;B	�cB	��B	��B	�B	�gB	�B	��B	�@B	�B	��B	��B	�B	��B	��B	��B	��B	�GB	�wB	��B	��B	�'B	��B	��B	�xB	�XB	��B	��B	��B	��B	��B	��B	�B	�pB	��B	��B	�B	��B	��B	��B	�mB	�bB	�SB	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	��B	�7B	�NB	��B	�QB	��B	�/B	�FB	�B	��B	��B	��B	��B	��B	��B	�uB	��B	�B	��B	�B	�B	�B	��B	�YB	�B	�1B	�RB	�B	�
B	��B	��B	��B	�4B	��B	��B	�iB	�QB	�FB	��B	��B	�B	�B	��B	�B	�B	�B	�B	�%B	�dB	�_B	��B	��B	��B	�B	�xB	�cB	�NB	�^B	�uB	�bB	��B	��B	�dB	ÜB	ïB	ūB	ŌB	�wB	ƑB	ǊB	ȑB	ȾB	ɐB	��B	�B	��B	��B	ϼB	дB	СB	��B	�>B	��B	ӽB	ӾB	��B	��B	��B	��B	��B	�B	�B	��B	�)B	��B	��B	��B	��B	�B	�%B	�	B	�B	��B	�-B	�HB	�B	�B	ߌB	��B	�yB	�tB	��B	�bB	�B	�:B	�rB	��B	�B	�HB	�DB	�PB	�CB	�(B	�B	�`B	��B	�iB	�B	�B	�yB	�B	�B	�B	��B	�B	�9B	�B	�cB	�B	�PB	�'B	�B	�WB	�AB	��B	�7B	��B	�sB	��B	��B	�$B	��B
�B
 B
B
�B
�B
XB
 �B
 �B
�B
�B
�B
B
�B
�B
B
B
kB
fB
CB
OB
^B
�B
�B
|B
�B
�B
�B
 }B
!�B
"�B
#�B
$�B
%B
%�B
	B
jB
>B
B
�B
�B
$�B
$�B
&�B
&|B
)*B
-|B
/�B
1iB
5B
5"B
6B
7B
6�B
:B
<>B
=FB
>6B
?%B
?>B
@JB
@PB
AHB
AUB
AcB
BqB
CoB
E�B
F^B
G�B
IyB
J�B
L�B
N�B
O�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
U�B
V�B
W%B
X�B
Y�B
\B
\�B
]B
^	B
^�B
_B
_B
_B
_pB
bBB
cB
c5B
dB
ekB
f0B
g@B
h8B
h5B
ipB
j�B
lhB
m6B
mBB
mCB
mOB
miB
n�B
oCB
oCB
oNB
oQB
pSB
pjB
qMB
qPB
qqB
q�B
r�B
s�B
t�B
w�B
x�B
y�B
y�B
y�B
z�B
z�B
z{B
z�B
z�B
z�B
z�B
z�B
{�B
|�B
|�B
~�B
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
�B
��B
��B
�B
�B
�B
�#B
�B
�$B
�3B
�`B
�B
�lB
�jB
�LB
�BB
�HB
�YB
�VB
�QB
�IB
�HB
�rB
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
� B
�B
�B
��B
�B
�CB
�:B
��B
�;B
�B
�2B
�$B
�B
�B
�B
�*B
�>B
�"B
�(B
�*B
�.B
�2B
�8B
�FB
�3B
�B
�CB
ǑB
ȍB
�eB
�eB
�[B
�qB
�qB
�^B
�hB
�_B
�iB
ʂB
�rB
�jB
�lB
�lB
�wB
�oB
�qB
�rB
�}B
�}B
�sB
�~B
ΘB
ΏB
΃B
�kB
�}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - [PRES_SurfaceOffsetNotTruncated_dbar]                                                                                                                                                                                                    no change                                                                                                                                                                                                                                                       PSAL_ADJUSTED = sal(CNDC,TEMP,PRES_ADJUSTED); PSAL_ADJ corrects conductivity cell thermal mass (CTM), Johnson et al, 2007, JAOT                                                                                                                                 PRES_SurfaceOffsetNotTruncated_dbar in TECH file for N-1 profile                                                                                                                                                                                                no change                                                                                                                                                                                                                                                       same as for PRES_ADJUSTED; CTL: alpha=0.1410, tau=6.68; pcond = 0.9998                                                                                                                                                                                          Pressures adjusted using PRES_SurfaceOffsetNotTruncated_dbar; Pressure drift corrected; Manufacturers sensor accuracy                                                                                                                                           No significant temperature drift detected; Manufacturers sensor accuracy;                                                                                                                                                                                       Salinity drift/offset - correction applied using OW piecewise-fit based on deep theta levels and Argo and CTD reference datasets.                                                                                                                               201806132308462018061323084620180613230846  CS  ARFMCSQCV4.0                                                                20170428071616    IP                G�O�G�O�G�O�                CS  ARGQCSQCV4.0                                                                20170428071616    IP                G�O�G�O�G�O�                CS  ARCACSQCV4.0                                                                20170428071616    IP                G�O�G�O�G�O�                CS  ARUPCSQCV4.0                                                                20170428071616    IP                G�O�G�O�G�O�                CS  ARGQCSQCV4.0                                                                20170428071616  QCP$                G�O�G�O�G�O�D7B7E           CS  ARGQCSQCV4.0                                                                20170428071616  QCF$                G�O�G�O�G�O�0               CS  ARSQPADJV1.0                                                                20180613063859  CV  PRES            @��GD��HG�O�                CS  ARSQCTL v1.0                                                                20180613063928  QC  PSAL            @��GD��HG�O�                CS  ARSQSIQCV2.0WOD2001 & Argo                                                  20180613064402  IP                  @�ffD�|�G�O�                CS  ARSQSIQCV2.0WOD2001 & Argo                                                  20180613064820  IP                  @�ffD�|�G�O�                