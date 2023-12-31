CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-25T15:35:52Z creation;2018-06-25T15:35:54Z conversion to V3.1;2019-12-23T06:19:42Z update;     
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
_FillValue                  `  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180625153552  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               FA   JA  I2_0675_070                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�m1~K 1   @�m��J @9ݗ�+k�c(�IQ��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @���A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<fD<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D��3D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�ffA��A(��AJffAh��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB��B
33B33B33B"33B*33B233B:33BB33BJ33BR33BZ33Bb33Bj33Br33Bz33B��fB��B��B��B��B��B��B��B��B��B��B��B��B��B��fB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~�fC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�S3C�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�S3C�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�S3C�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfD #3D �3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D	#3D	�3D
#3D
�3D#3D�3D)�D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D #3D �3D!#3D!�3D"#3D"�3D##3D#�3D$#3D$�3D%#3D%�3D&#3D&�3D'#3D'�3D(#3D(�3D)#3D)�3D*#3D*�3D+#3D+�3D,#3D,�3D-#3D-�3D.#3D.�3D/#3D/�3D0#3D0�3D1#3D1�3D2#3D2�3D3#3D3�3D4#3D4�3D5#3D5�3D6#3D6�3D7#3D7�3D8#3D8�3D9#3D9�3D:#3D:�3D;#3D;�3D<)�D<�3D=#3D=�3D>#3D>�3D?#3D?�3D@#3D@�3DA#3DA�3DB#3DB�3DC#3DC�3DD#3DD�3DE#3DE�3DF#3DF�3DG#3DG�3DH#3DH�3DI#3DI�3DJ#3DJ�3DK#3DK�3DL#3DL�3DM#3DM�3DN#3DN�3DO#3DO�3DP#3DP�3DQ#3DQ�3DR�DR�3DS#3DS�3DT#3DT�3DU#3DU�3DV#3DV�3DW#3DW�3DX#3DX�3DY#3DY�3DZ#3DZ�3D[#3D[�3D\#3D\�3D]#3D]�3D^#3D^�3D_#3D_�3D`#3D`�3Da#3Da�3Db#3Db�3Dc#3Dc�3Dd#3Dd�3De#3De�3Df#3Df�3Dg#3Dg�3Dh#3Dh�3Di#3Di�3Dj#3Dj�3Dk#3Dk�3Dl#3Dl�3Dm#3Dm�3Dn#3Dn�3Do#3Do�3Dp#3Dp�3Dq#3Dq�3Dr#3Dr�3Ds#3Ds�3Dt#3Dt�3Du#3Du�3Dv#3Dv�3Dw#3Dw�3Dx#3Dx�3Dy#3Dy�3Dz#3Dz�3D{#3D{�3D|#3D|�3D}#3D}�3D~#3D~�3D#3D�3D��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�T�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D��fD�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D���D��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D�D���D��D�Q�DÑ�D�њD��D�Q�Dđ�D�њD��D�Q�Dő�D�њD��D�Q�DƑ�D�њD��D�Q�DǑ�D�њD��D�Q�Dȑ�D�њD��D�Q�Dɑ�D�њD��D�Q�Dʑ�D�њD��D�Q�Dˑ�D�њD��D�Q�D̑�D�њD��D�Q�D͑�D�њD��D�Q�DΑ�D�њD��D�Q�Dϑ�D�њD��D�Q�DБ�D�њD��D�Q�Dё�D�њD��D�Q�Dґ�D�њD��D�Q�Dӑ�D�њD��D�Q�Dԑ�D�њD��D�Q�DՑ�D�њD��D�Q�D֑�D�њD��D�Q�Dב�D�њD��D�Q�Dؑ�D�њD��D�Q�Dّ�D�њD��D�Q�Dڑ�D�њD��D�Q�Dۑ�D�њD��D�Q�Dܑ�D�њD��D�Q�Dݑ�D�њD��D�Q�Dޑ�D�њD��D�Q�Dߑ�D�њD��D�Q�D���D�њD��D�Q�DᑚD�њD��D�Q�D⑚D�њD��D�Q�D㑚D�њD��D�Q�D䑚D�њD��D�Q�D呚D�њD��D�NfD摚D�њD��D�Q�D瑚D�њD��D�Q�D葚D�њD��D�Q�D鑚D�њD��D�Q�DꑚD�њD��D�Q�D둚D�њD��D�Q�D쑚D�њD��D�Q�D푚D�њD��D�Q�DD�њD��D�Q�DD�њD��D�Q�D�D�њD��D�Q�D�D�њD��D�Q�D�D�њD��D�Q�D�D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�K31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AƓuAƓuAƝ�Aƣ�Aƥ�Aƥ�Aƣ�Aƣ�Aơ�Aơ�Aơ�AƗ�A�t�A���A�7LA�VA�dZA�~�A��A�1A���A��hA���A��^A�"�A��uA���A�JA���A� �A��+A��A���A���A�+A�&�A���A�l�A�{A���A���A��A��A��HA��+A��^A�Q�A�oA�ĜA�+A�K�A���A��uA�dZA��wA��-A��A�5?A�dZA�E�A�ffA���A� �A��uA�O�A��A�&�A�bNA�(�A��;A�33A�ZA� �A�`BA���A��HA�jA���A��DA��A��PA�+A�jA��RA���A�p�A�A�A���A��FA�%A���A��
A��A�z�A��A��A�t�A�^5A���A�9XAK�A~VA}XA|VA{t�AxbNAv�Av��AtjAr��Ap�/Am�Ak33Ah��Af�\Ae��Ad��Ad1'Acp�AbZA_ƨA^�yA\ZA[XAZ��AZ1AX��AX^5AWhsAU\)AT1'AS�AQl�AP�jAN��AMp�AKC�AHA�AD�`AC�AB��AA|�A@��A?�;A>��A=�mA<�\A:��A:�/A:��A:M�A:-A9��A8�+A7ƨA7S�A5�#A4�+A2��A1�A1��A0�!A01'A/��A.I�A-G�A,1'A+��A+|�A+C�A*��A(�A'��A'\)A%7LA#�A"�A"�\A"�A"ffA" �A!�A!dZA (�A��A�A��A�A&�A(�AhsAM�A�A7LA{A�^A�A�/Ar�A9XAbA�^A;dAĜA �A�Ap�A��At�A
=A
ffA	"�A�A��AoA��AffAdZA{AoAjA�AO�A ȴ@��@��@���@��@��F@��@��@�J@�Q�@�w@�F@�@�E�@�O�@��@�I�@@�@��y@��@�@�  @�ȴ@�?}@��/@�Z@�dZ@�~�@�@��@�\@��T@�`B@��u@�@�M�@�x�@�ƨ@�ȴ@�7L@ץ�@�E�@ԛ�@�dZ@�\)@Ұ!@ҏ\@�{@�O�@��/@�j@Ͼw@Ώ\@�bN@ɩ�@�bN@ȼj@�p�@�V@�7L@��@�;d@�~�@ř�@�O�@þw@��@�E�@��7@�X@���@���@��@�J@��-@�/@��@��@�S�@��R@�@�?}@�V@��`@�j@���@���@�V@��@�E�@��h@�Z@��H@�G�@��@�Z@�
=@��@���@�%@��@��@�t�@�V@�@��@�G�@��@��@�r�@�Q�@�(�@��@�\)@���@��R@���@��@��R@�E�@���@��@���@��@��@���@���@��\@���@��@�/@��@��@��9@��u@��w@�S�@�"�@��@��y@��@�"�@�
=@��R@�ff@�{@�@�x�@�7L@�V@���@�r�@�b@���@�+@��@���@��\@��\@��+@�n�@�V@�=q@�=q@�5?@��T@��7@�`B@�G�@�X@�O�@�/@��/@���@�Q�@�b@��@��F@�t�@�o@��!@�^5@�5?@��T@�x�@�G�@��@��`@��/@�r�@�  @��@��P@�|�@�\)@��@���@��@�p�@�/@�/@��h@�hs@�?}@�V@��/@��9@�bN@�z�@��j@��u@�I�@�K�@���@�dZ@�@���@�5?@�-@�5?@�=q@�@���@��#@���@�G�@�r�@�(�@���@�ƨ@��F@���@�|�@�t�@�l�@�dZ@�\)@�\)@�t�@���@�l�@��\@�E�@�-@��@��#@���@�x�@��@��u@�9X@��@���@���@��@�@�M�@�J@���@��T@���@�@��@���@�`B@��@�Ĝ@�(�@�b@��@��;@�\)@��@� �@��@��@���@��P@�ff@��@�E�@�E�@�^5@�5?@���@��^@�`B@�&�@�V@�%@�%@�V@��@�V@��@�Ĝ@��u@�z�@�Z@�9X@�b@��@��@��@|�@K�@�@~ȴ@~v�@~{@}�-@}O�@}O�@}`B@}?}@|��@|z�@|Z@|�@{�
@{��@{��@{�@{dZ@z�\@y��@w��@vE�@vV@vv�@v�+@v��@vE�@v@u��@u�@t��@tZ@t1@st�@r�!@q�@q�@q�@qx�@pbN@pr�@p�@o
=@n�R@o+@oK�@o�@o�P@o��@o\)@nff@mO�@m?}@mV@l�j@lI�@k"�@j�\@h�`@hA�@hA�@hA�@hA�@hb@g;d@f�y@fv�@f5?@e�T@e�h@eO�@eV@d��@dZ@c�
@c��@ct�@b�H@b��@b��@b-@a��@a��@a7L@a%@`��@`�u@`�u@`r�@_��@^�y@^��@^{@]�@]/@]V@\�@\��@\��@\(�@[�F@[dZ@["�@Z�@Z~�@Z^5@Z=q@ZJ@Y��@Y�@Y�#@Y��@Y�^@Y�7@Yhs@X��@X�u@X1'@W�;@W|�@W�@Vȴ@V��@V�+@VV@V$�@U�h@UV@T�/@T�@Tz�@S�m@S��@St�@SC�@S@Rn�@Q��@Q��@Qhs@Q%@P��@PQ�@P �@O�;@O�@O|�@Ol�@OK�@O+@N�y@N@M?}@L�@L�/@L��@L��@LZ@K��@KdZ@K33@Ko@J�@J�@J�!@J~�@JM�@J-@J-@I�#@IX@H��@HbN@HA�@Hb@G�;@Gl�@G�@F�R@Fff@F5?@E�@E@E��@E�h@E�@E�@EO�@E/@D��@D9X@Cƨ@B�@B��@Bn�@B^5@BM�@BM�@BM�@B-@A�#@AX@@��@@�u@@�@@1'@?�;@?�;@?��@?|�@>��@>�+@>5?@>{@>@=�@=��@=�-@=p�@<��@<I�@;��@;�@:�@:��@:��@:�\@:J@9��@9�#@9�7@9�@8�9@8bN@8b@7�@7�@6��@6E�@5@5�-@5�h@5p�@5�@4�@4(�@3�m@3�
@3ƨ@3�@3��@3��@3dZ@2�H@2��@2~�@2M�@2-@1�^@1x�@1&�@0��@0�u@0Q�@/�;@/��@/K�@.��@.�R@.��@.��@.�+@.�R@.��@.ȴ@.E�@.{@-�@-�@-�h@-`B@-?}@-V@,�j@,j@,�@+��@+ƨ@+�@+t�@+"�@*�H@*��@*�!@*��@*~�@*^5@*M�@*�@)�#@)��@)��@)�^@)��@)�7@)X@)%@(��@(��@(Q�@(b@(  @'��@'\)@'
=@&�R@&ff@&E�@&@%@%��@%p�@%?}@%�@$��@$�j@$�D@$z�@$Z@$�@#�m@#�m@#ƨ@#��@#�@#t�@#t�@#33@"�H@"�!@"�\@"~�@"-@"�@!�#@!�7@!G�@!&�@!%@ ��@ �`@ �9@ �u@ bN@ A�@�@�P@\)@+@��@�@�R@�+@{@��@p�@�@�@��@��@j@I�@(�@�@�
@��@o@��@��@�\@n�@��@x�@X@%@1'@�@�;@�;@��@�w@l�@\)@;d@
=@�R@�+@E�@��@��@ff@E�@@�@�@�h@?}@O�@/@�/@�D@�D@Z@�m@ƨ@dZ@33@�@�H@�H@n�@J@��@�7@G�@�@%@��@�u@Q�@1'@ �@�@�@|�@+@ȴ@��@v�@V@{@�-@�@`B@�@�/@��@�@��@��@j@9X@��@�m@�F@��@S�@C�@33@33@@
�H@
��@
��@
�\@
M�@
J@	�#@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AƓuAƓuAƝ�Aƣ�Aƥ�Aƥ�Aƣ�Aƣ�Aơ�Aơ�Aơ�AƗ�A�t�A���A�7LA�VA�dZA�~�A��A�1A���A��hA���A��^A�"�A��uA���A�JA���A� �A��+A��A���A���A�+A�&�A���A�l�A�{A���A���A��A��A��HA��+A��^A�Q�A�oA�ĜA�+A�K�A���A��uA�dZA��wA��-A��A�5?A�dZA�E�A�ffA���A� �A��uA�O�A��A�&�A�bNA�(�A��;A�33A�ZA� �A�`BA���A��HA�jA���A��DA��A��PA�+A�jA��RA���A�p�A�A�A���A��FA�%A���A��
A��A�z�A��A��A�t�A�^5A���A�9XAK�A~VA}XA|VA{t�AxbNAv�Av��AtjAr��Ap�/Am�Ak33Ah��Af�\Ae��Ad��Ad1'Acp�AbZA_ƨA^�yA\ZA[XAZ��AZ1AX��AX^5AWhsAU\)AT1'AS�AQl�AP�jAN��AMp�AKC�AHA�AD�`AC�AB��AA|�A@��A?�;A>��A=�mA<�\A:��A:�/A:��A:M�A:-A9��A8�+A7ƨA7S�A5�#A4�+A2��A1�A1��A0�!A01'A/��A.I�A-G�A,1'A+��A+|�A+C�A*��A(�A'��A'\)A%7LA#�A"�A"�\A"�A"ffA" �A!�A!dZA (�A��A�A��A�A&�A(�AhsAM�A�A7LA{A�^A�A�/Ar�A9XAbA�^A;dAĜA �A�Ap�A��At�A
=A
ffA	"�A�A��AoA��AffAdZA{AoAjA�AO�A ȴ@��@��@���@��@��F@��@��@�J@�Q�@�w@�F@�@�E�@�O�@��@�I�@@�@��y@��@�@�  @�ȴ@�?}@��/@�Z@�dZ@�~�@�@��@�\@��T@�`B@��u@�@�M�@�x�@�ƨ@�ȴ@�7L@ץ�@�E�@ԛ�@�dZ@�\)@Ұ!@ҏ\@�{@�O�@��/@�j@Ͼw@Ώ\@�bN@ɩ�@�bN@ȼj@�p�@�V@�7L@��@�;d@�~�@ř�@�O�@þw@��@�E�@��7@�X@���@���@��@�J@��-@�/@��@��@�S�@��R@�@�?}@�V@��`@�j@���@���@�V@��@�E�@��h@�Z@��H@�G�@��@�Z@�
=@��@���@�%@��@��@�t�@�V@�@��@�G�@��@��@�r�@�Q�@�(�@��@�\)@���@��R@���@��@��R@�E�@���@��@���@��@��@���@���@��\@���@��@�/@��@��@��9@��u@��w@�S�@�"�@��@��y@��@�"�@�
=@��R@�ff@�{@�@�x�@�7L@�V@���@�r�@�b@���@�+@��@���@��\@��\@��+@�n�@�V@�=q@�=q@�5?@��T@��7@�`B@�G�@�X@�O�@�/@��/@���@�Q�@�b@��@��F@�t�@�o@��!@�^5@�5?@��T@�x�@�G�@��@��`@��/@�r�@�  @��@��P@�|�@�\)@��@���@��@�p�@�/@�/@��h@�hs@�?}@�V@��/@��9@�bN@�z�@��j@��u@�I�@�K�@���@�dZ@�@���@�5?@�-@�5?@�=q@�@���@��#@���@�G�@�r�@�(�@���@�ƨ@��F@���@�|�@�t�@�l�@�dZ@�\)@�\)@�t�@���@�l�@��\@�E�@�-@��@��#@���@�x�@��@��u@�9X@��@���@���@��@�@�M�@�J@���@��T@���@�@��@���@�`B@��@�Ĝ@�(�@�b@��@��;@�\)@��@� �@��@��@���@��P@�ff@��@�E�@�E�@�^5@�5?@���@��^@�`B@�&�@�V@�%@�%@�V@��@�V@��@�Ĝ@��u@�z�@�Z@�9X@�b@��@��@��@|�@K�@�@~ȴ@~v�@~{@}�-@}O�@}O�@}`B@}?}@|��@|z�@|Z@|�@{�
@{��@{��@{�@{dZ@z�\@y��@w��@vE�@vV@vv�@v�+@v��@vE�@v@u��@u�@t��@tZ@t1@st�@r�!@q�@q�@q�@qx�@pbN@pr�@p�@o
=@n�R@o+@oK�@o�@o�P@o��@o\)@nff@mO�@m?}@mV@l�j@lI�@k"�@j�\@h�`@hA�@hA�@hA�@hA�@hb@g;d@f�y@fv�@f5?@e�T@e�h@eO�@eV@d��@dZ@c�
@c��@ct�@b�H@b��@b��@b-@a��@a��@a7L@a%@`��@`�u@`�u@`r�@_��@^�y@^��@^{@]�@]/@]V@\�@\��@\��@\(�@[�F@[dZ@["�@Z�@Z~�@Z^5@Z=q@ZJ@Y��@Y�@Y�#@Y��@Y�^@Y�7@Yhs@X��@X�u@X1'@W�;@W|�@W�@Vȴ@V��@V�+@VV@V$�@U�h@UV@T�/@T�@Tz�@S�m@S��@St�@SC�@S@Rn�@Q��@Q��@Qhs@Q%@P��@PQ�@P �@O�;@O�@O|�@Ol�@OK�@O+@N�y@N@M?}@L�@L�/@L��@L��@LZ@K��@KdZ@K33@Ko@J�@J�@J�!@J~�@JM�@J-@J-@I�#@IX@H��@HbN@HA�@Hb@G�;@Gl�@G�@F�R@Fff@F5?@E�@E@E��@E�h@E�@E�@EO�@E/@D��@D9X@Cƨ@B�@B��@Bn�@B^5@BM�@BM�@BM�@B-@A�#@AX@@��@@�u@@�@@1'@?�;@?�;@?��@?|�@>��@>�+@>5?@>{@>@=�@=��@=�-@=p�@<��@<I�@;��@;�@:�@:��@:��@:�\@:J@9��@9�#@9�7@9�@8�9@8bN@8b@7�@7�@6��@6E�@5@5�-@5�h@5p�@5�@4�@4(�@3�m@3�
@3ƨ@3�@3��@3��@3dZ@2�H@2��@2~�@2M�@2-@1�^@1x�@1&�@0��@0�u@0Q�@/�;@/��@/K�@.��@.�R@.��@.��@.�+@.�R@.��@.ȴ@.E�@.{@-�@-�@-�h@-`B@-?}@-V@,�j@,j@,�@+��@+ƨ@+�@+t�@+"�@*�H@*��@*�!@*��@*~�@*^5@*M�@*�@)�#@)��@)��@)�^@)��@)�7@)X@)%@(��@(��@(Q�@(b@(  @'��@'\)@'
=@&�R@&ff@&E�@&@%@%��@%p�@%?}@%�@$��@$�j@$�D@$z�@$Z@$�@#�m@#�m@#ƨ@#��@#�@#t�@#t�@#33@"�H@"�!@"�\@"~�@"-@"�@!�#@!�7@!G�@!&�@!%@ ��@ �`@ �9@ �u@ bN@ A�@�@�P@\)@+@��@�@�R@�+@{@��@p�@�@�@��@��@j@I�@(�@�@�
@��@o@��@��@�\@n�@��@x�@X@%@1'@�@�;@�;@��@�w@l�@\)@;d@
=@�R@�+@E�@��@��@ff@E�@@�@�@�h@?}@O�@/@�/@�D@�D@Z@�m@ƨ@dZ@33@�@�H@�H@n�@J@��@�7@G�@�@%@��@�u@Q�@1'@ �@�@�@|�@+@ȴ@��@v�@V@{@�-@�@`B@�@�/@��@�@��@��@j@9X@��@�m@�F@��@S�@C�@33@33@@
�H@
��@
��@
�\@
M�@
J@	�#@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BH�BI�BH�BH�BH�BI�BH�BH�BH�BH�BG�BF�BA�BB�B9XBL�BZB`BBq�Bv�Bz�B�B�B�B�1B�=B�PB�uB�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�9B�LB�^B��B�9B�RBƨB��BǮB��B��B�!B��B��BjBR�BK�BVBW
BS�BI�B;dB6FB1'B'�B{BVBB�B�fB��B�}B�RB�B�{B�PB{�Bn�BYBQ�BB�B&�B
��B
�yB
�)B
�B
��B
�LB
��B
��B
�B
�B
� B
w�B
aHB
YB
S�B
J�B
C�B
=qB
,B
�B
�B
PB	��B	�B	�5B	��B	�RB	��B	��B	��B	��B	�{B	�VB	�B	w�B	m�B	e`B	bNB	_;B	YB	VB	P�B	F�B	=qB	8RB	.B	(�B	�B	{B		7B��B�`B�B��B��BȴBǮBǮBB�qB�3B�-B�'B�B�B�B��B��B��B��B�bB�B|�B{�Bw�Br�Bo�Bl�BiyBiyBffBe`BdZBdZBcTBcTBn�Bp�Bk�BffBe`Be`BcTBbNB`BB]/BYBYBW
BS�BL�BE�BA�B=qB=qB:^B:^B7LB6FB6FB5?B49B49B33B33B33B33B33B1'B.B,B)�B)�B(�B(�B'�B'�B%�B%�B%�B$�B$�B#�B"�B"�B"�B �B!�B�B �B�B�B�B�B�B!�B�B�B�B �B�B�B �B �B�B�B�B �B�B"�B"�B"�B"�B"�B#�B$�B%�B%�B'�B&�B)�B0!B/B/B.B/B/B/B0!B33B1'B2-B33B7LB:^B<jB<jB=qB@�BC�BA�B;dB8RB;dBE�BF�BN�BP�BT�BVBW
BW
B\)B^5BaHBcTBdZBffBiyBjBiyBjBm�Bo�Bt�Bv�Bv�Bw�Bw�Bw�Bw�By�B{�Bz�By�B|�B~�B~�B}�B}�B~�B� B�B�B�B�%B�1B�JB�bB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�3B�FB�^B�^B�}B��B��BBĜBƨBȴB��B��B�B�B�#B�5B�;B�NB�`B�mB�B�B�B�B�B�B��B��B��B��B��B	B	B	B	DB	oB	{B	�B	�B	�B	�B	�B	�B	�B	$�B	'�B	,B	.B	1'B	49B	7LB	9XB	;dB	<jB	>wB	@�B	@�B	A�B	C�B	F�B	I�B	J�B	K�B	K�B	L�B	K�B	L�B	M�B	M�B	P�B	W
B	ZB	[#B	]/B	`BB	bNB	dZB	iyB	k�B	n�B	s�B	y�B	}�B	}�B	~�B	�B	�%B	�=B	�JB	�bB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�'B	�'B	�'B	�'B	�-B	�?B	�RB	�^B	�^B	�XB	�RB	�XB	�jB	�qB	�wB	�wB	�wB	��B	ĜB	ƨB	ƨB	ƨB	��B	ɺB	ǮB	ǮB	ɺB	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�TB	�ZB	�`B	�mB	�mB	�mB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
1B
	7B
DB
DB
JB
JB
JB
PB
PB
VB
PB
PB
JB
DB
DB

=B
JB
PB
PB
VB
VB
PB
PB
VB
\B
\B
\B
VB
\B
bB
hB
bB
bB
bB
bB
bB
hB
hB
oB
oB
uB
uB
uB
uB
uB
uB
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
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
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
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
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
1'B
1'B
2-B
2-B
33B
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
6FB
6FB
6FB
7LB
7LB
7LB
7LB
6FB
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
;dB
<jB
<jB
<jB
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
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
E�B
F�B
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
J�B
J�B
J�B
J�B
K�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
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
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
XB
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
^5B
^5B
^5B
^5B
_;B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
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
aHB
aHB
bNB
bNB
bNB
cTB
cTB
bNB
bNB
bNB
bNB
bNB
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
dZB
dZB
dZB
dZB
e`B
e`B
ffB
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
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BH�BI�BH�BH�BH�BI�BH�BH�BH�BH�BGzBFtBAUBB[B9>BL�BY�B`BqvBv�Bz�B��B��B��B��B�	B�B�[B�:B�SB�_B�~B�qB�eB�qB�~B��B��B��B��B��B��B��B��B��B��B�B�B�*B��B�B�B�tBҽB�zBѷBѷB��B��B�SBjKBR�BK�BU�BV�BS�BI�B;0B6B0�B'�BFB"B �B�|B�2B̘B�HB�B��B�FB�B{�BncBX�BQ�BB[B&�B
��B
�DB
��B
��B
ʌB
�B
��B
�_B
��B
��B
�B
w�B
aB
X�B
S�B
J�B
CaB
=<B
+�B
kB
_B
B	��B	�|B	�B	ʌB	�B	��B	��B	��B	�KB	�,B	�"B	��B	w�B	m]B	e,B	a�B	_B	X�B	U�B	P�B	FtB	=<B	8B	-�B	(�B	xB	FB		B��B�B��B��B�~B�fB�zB�zB�[B�<B��B��B��B��B��B��B��B��B��B�_B�.B��B|�B{�Bw�Br|BoiBl=Bi*Bi*Bf2BeBd&Bd&Bc Bc BncBpoBkQBf2Be,Be,Bc Ba�B_�B\�BX�BX�BV�BS�BL�BESBA;B="B="B:B:*B6�B5�B5�B4�B4B4B2�B2�B2�B2�B2�B0�B-�B+�B)�B)�B(�B(�B'�B'�B%�B%�B%�B$�B$�B#�B"�B"�B"�B vB!|B�B �B�BjBjB�B�B!|BpB�BpB vB�B�B �B �BpB�B�B vBpB"�B"�B"�B"�B"�B#�B$�B%�B%�B'�B&�B)�B/�B.�B.�B-�B.�B.�B.�B/�B2�B0�B1�B2�B6�B:*B<B<B=<B@4BCGBA;B;B8B;BEmBFYBN�BP�BT�BU�BV�BV�B[�B]�B`�BcBdBfBiDBj0Bi*BjKBmCBoOBtnBv�BvzBw�Bw�Bw�Bw�By�B{�Bz�By�B|�B~�B~�B}�B}�B~�B�B��B��B��B��B��B�B�B�:B�,B�FB�2B�?B�~B��B�vB�|B�|B��B��B��B��B��B��B��B��B��B�B�B�.B�4B�4B�[B�MB�YBȀB�~BңBյB��B��B��B��B��B�B�B�0B�6B�=B�]B�OB�aB�tB��B��B��B��B	 �B	�B	�B	B	 B	,B	MB	2B	2B	SB	EB	eB	]B	$�B	'�B	+�B	-�B	0�B	3�B	7B	9	B	;B	<6B	>BB	@OB	@4B	A;B	CGB	FYB	IlB	J�B	KxB	KxB	L~B	KxB	L~B	M�B	M�B	P�B	V�B	Y�B	Z�B	\�B	_�B	a�B	dB	i*B	k6B	ncB	shB	y�B	}�B	}�B	~�B	��B	��B	��B	�B	�.B	� B	�&B	�&B	�?B	�qB	�WB	�WB	�]B	�xB	�]B	�dB	�vB	��B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�*B	�B	�	B	�B	�	B	�6B	�"B	�(B	�(B	�(B	�;B	�MB	�tB	�YB	�YB	ʌB	ɆB	�_B	�_B	�lB	̘B	ϑB	ңB	ԯB	ּB	��B	��B	��B	��B	��B	��B	��B	��B	� B	�&B	�B	�B	�B	�B	�B	�8B	�0B	�KB	�6B	�=B	�CB	�IB	�OB	�UB	�UB	�UB	�[B	�[B	�vB	�[B	�aB	�aB	�|B	�hB	�hB	�hB	�nB	�tB	�tB	��B	�zB	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B

�B
B
�B
�B
B
B
B
B
B
B

�B

�B
	�B
B
B
B
B
B
B
B
B
B
(B
(B
B
(B
B
4B
B
B
B
B
B
B
B
:B
:B
&B
&B
&B
@B
@B
&B
:B
 B
 B
 B
&B
@B
@B
&B
&B
&B
,B
FB
2B
SB
?B
YB
EB
EB
_B
EB
KB
eB
KB
KB
kB
QB
QB
WB
WB
]B
]B
~B
jB
�B
�B
�B
jB
pB
pB
 �B
 �B
!|B
!|B
!|B
!|B
!�B
"�B
"�B
#�B
#�B
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
)�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
1�B
1�B
2�B
3�B
4B
3�B
4�B
5B
4�B
4�B
4�B
4�B
5�B
6B
5�B
5�B
5�B
5�B
5�B
5�B
6�B
6�B
7B
7B
6B
6�B
6�B
6�B
8B
8B
8B
9	B
9	B
:B
:B
:*B
;0B
<B
<B
<B
=<B
="B
="B
=<B
>(B
>(B
>BB
>(B
>(B
>(B
>(B
>(B
>(B
>(B
?.B
?.B
@4B
@4B
@4B
A;B
A;B
A;B
B[B
BAB
BAB
AUB
A;B
B[B
BAB
B[B
BAB
BAB
BAB
CaB
ESB
FtB
G_B
G_B
G_B
G_B
HfB
HfB
IlB
IlB
I�B
IlB
I�B
IlB
IlB
I�B
IlB
J�B
JrB
J�B
JrB
J�B
K�B
JrB
K�B
K�B
L~B
L�B
L~B
L~B
L~B
L�B
L�B
L~B
L~B
L~B
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
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^B
^�B
]�B
^B
]�B
^B
]�B
^B
]�B
^B
^B
]�B
]�B
]�B
^B
]�B
]�B
]�B
^�B
_�B
aB
`�B
a�B
bB
a�B
cB
cB
a�B
a�B
bB
a�B
bB
bB
bB
bB
a�B
cB
cB
c B
dB
dB
dB
dB
dB
dB
d&B
e,B
eB
fB
fB
gB
g8B
gB
g8B
gB
gB
h>B
h$B
h$B
i*B
iDB
i*B
iDB
iDB
i*B
jKB
j0B
j0B
j0B
j0B
j0B
k6B
k6B
kQB
k6B
kQB
k6B
kQB
k6B
l=B
k6B
l=B
l=B
l=B
lWB
lWB
lWB
l=B
l=B
m]1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.55(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807010041002018070100410020180701004100201807020027022018070200270220180702002702JA  ARFMdecpA19c                                                                20180626003530  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180625153552  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180625153553  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180625153553  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180625153553  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180625153553  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180625153554  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180625153554  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180625153554  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180625153554                      G�O�G�O�G�O�                JA  ARUP                                                                        20180625155447                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180625153744  CV  JULD            G�O�G�O�F�h�                JM  ARCAJMQC2.0                                                                 20180630154100  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180630154100  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180701152702  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                